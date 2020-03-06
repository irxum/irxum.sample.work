# -*- coding: utf-8 -*-
"""

@author: alex

latitudeEval.py

"""

import pvlib.solarposition as sp
import pvlib.tracking as trk
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import numpy as np
from mpl_toolkits.basemap import Basemap

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#==============================================================================
# User specific inputs
# siteLat and siteLong can be found with http://www.latlong.net with known
# site address.
#==============================================================================

startDate = dt.datetime(2017,3,20)
endDate = dt.datetime(2017,3,21)
siteLat = np.arange(0.0, 66.0, 1.0) # latitude
siteLong = -121.734319 # longitude
timeZone = -8 # relative to UTC, -8 = Pacific Time
siteElev = 0 # in meters
aveTemp = 7.8 # in degrees C
timeFreq = 'min'
gcRatio = 0.35 # see gcrEval to see that 0.25 causes most severe backtracking
rom = 45
wk_path = r'C:\Users\alexatsunfolding\Google Drive\Analysis\Latitude Eval\Equinox-03202017'


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

dateTimeSpan = pd.DatetimeIndex(start = startDate, freq = timeFreq,
                                end = endDate)
                                
# adjust for timezone and convert to UTC
dateTimeSpanUTC = dateTimeSpan + dt.timedelta(hours = -1.0 * timeZone)


# create dataframes for solar position using pvlib
solPos = []
sp_df = pd.DataFrame()
for sl in siteLat:
    sp_df = sp.get_solarposition(time = dateTimeSpanUTC,
                                 latitude = sl, longitude = siteLong, 
                                 altitude = siteElev, 
                                 temperature = aveTemp)
    # convert the solar position data index back to local timestamp
    sp_df.index = dateTimeSpan
    solPos.append(sp_df)
    
# create dataframes for tracking angle using pvlib
trackAngl = []
tr_df = pd.DataFrame()
for s in solPos:
    tr_df = trk.singleaxis(s['apparent_zenith'], s['azimuth'], 
                           axis_tilt = 0, axis_azimuth = 0, max_angle = rom,
                           backtrack = True, gcr = gcRatio)
    tr_df['tracker_theta'] = -1.0 * tr_df['tracker_theta']
    trackAngl.append(tr_df)
    


trdiff_ls = []
durmx_ls = []
colnames = ['btmdur', 'lmdur', 'trdur', 'ladur', 'btadur', 'mxdifftime', 'mxdiffdur']
si = 0
for tr in trackAngl:
    # remove NA's from trackAngl
    tr.dropna(inplace = True)
    
    # trdiff is the delta between data points
    # using periods = -1 to look at diff between next and current angle
    trdiff = tr.tracker_theta.diff(periods=-1)
    trdiff[trdiff == 0.0].index.tolist()
    trdiffabs = trdiff.abs()
    maxdiffidx = trdiffabs.idxmax()
    # append the negative to reflect real change signage
    trdiff_ls.append(-1.0 * trdiff)
    
    trdiffzero = trdiff.where(trdiff == 0.0)
    trdiffzero.dropna(inplace = True)

    ## end of backtracking in morning is first instance of zero
    idx_btmend = trdiffzero.index[0] 
    
    ## figure out start away from max angle
    # trdiffcut1 is the trdiff after morning backtracking ends
    trdiffcut1 = trdiff.ix[idx_btmend:]
    # trdiffcut2 is the trdiff after begining of normal tracking
    trdiffcut2 = trdiffcut1.where(trdiffcut1 <> 0)
    trdiffcut2.dropna(inplace = True)
    idx_trkst = trdiffcut2.index[0]

    # figure out end of transit reaching other max angle
    trdiffcut3 = trdiff.ix[idx_trkst:]
    trdiffcut4 = trdiffcut3.where(trdiffcut3 == 0)
    trdiffcut4.dropna(inplace = True)
    idx_trkend = trdiffcut4.index[0]
    
    # figure out start of backtracking in afternoon
    trdiffcut5 = trdiff.ix[idx_trkend:]
    trdiffcut6 = trdiffcut5.where(trdiffcut5 <> 0)
    trdiffcut6.dropna(inplace = True)
    idx_btast = trdiffcut6.index[0]
    
    idx_btmst = tr.index[0]
    idx_btaend = tr.index[-1]
    idxls = [idx_btmst, idx_btmend, idx_trkst, idx_trkend, idx_btast, idx_btaend]
    dur = np.diff(idxls)
    # convert durmin to minutes
    durmin = []
    for d in dur:
        durmin.append(d.seconds // 60)
    # must add one minute to backtracking because pvlib.singleaxis
    # does not provide night time idle position and above methods
    # essentially truncate backtracking durations by one minute
    durmin[0] = durmin[0] + 1
    durmin[-1] = durmin[-1] + 1
    durmin.append(maxdiffidx)
    durmin.append(trdiffabs[maxdiffidx])
    durmx_ls.append(durmin)
    si = si + 1

durmx_df = pd.DataFrame(durmx_ls[:], columns = colnames)
durmx_df['btmrate'] = (rom * 1.0) / (durmx_df['btmdur'] * 1.0) 
durmx_df['btarate'] = (rom * 1.0) / (durmx_df['btadur'] * 1.0) 
durmx_df['trrate'] = (rom * 2.0) / (durmx_df['trdur'] * 1.0) # multiply by two
durmx_df['latitude'] = list(siteLat)


# plot durations with respect to latitude
durcols = range(0,5)
cmap = plt.get_cmap('jet')
colors = [cmap(i) for i in np.linspace(0, 1, len(durcols))]

fig = plt.figure(figsize=(10,6), facecolor='w')
ax = fig.add_subplot(111)
pLabels = ['morning backtracking', 'morning at limit', 'daily transit',
           'afternoon at limit', 'afternoon backtracking']
xvals = list(durmx_df.latitude)
for ci in durcols:
    yvals = list(durmx_df.ix[:,ci].values)
    plt.scatter(x = xvals, y = yvals, label = pLabels[ci], linewidth = 0.5,
                edgecolors = colors[ci], facecolors = 'none')
ax.get_yaxis().set_tick_params(direction='in')
ax.get_xaxis().set_tick_params(direction='in')
plt.ylim(0,500)
plt.legend(loc='upper left', bbox_to_anchor=(1,1),fontsize=11)
plt.tight_layout(pad=3, rect=[0, 0, 0.75, 1.0])
 
plt.ylabel("Duration [ minutes ]")
xlbl = "Latitude [ " + "$^\circ$" + " ]"
plt.xlabel(xlbl)
plt.show()

dur_lat = durmx_df.iloc[:,0:5]
dur_fname = wk_path + r'\dur_lat_65N_eq.csv'
dur_lat.to_csv(dur_fname)


# plot rotation rate with respect to latitude
durcols = range(6,10)
cmap = plt.get_cmap('cool')
colors = [cmap(i) for i in np.linspace(0, 1, len(durcols))]

fig = plt.figure(figsize=(10,6), facecolor='w')
ax = fig.add_subplot(111)
pLabels = ['max instantaneous', 'average morning backtracking', 
           'average afternoon backtracking', 'average daily tracking']
xvals = list(durmx_df.latitude)
idx = 0
for ci in durcols:
    yvals = list(durmx_df.ix[:,ci].values)
    plt.scatter(x = xvals, y = yvals, label = pLabels[idx], linewidth = 0.5,
                edgecolors = colors[idx], facecolors = 'none')
    idx = idx + 1
ax.get_yaxis().set_tick_params(direction='in')
ax.get_xaxis().set_tick_params(direction='in')
plt.ylim(0.2,2.2)
plt.legend(loc='upper left', bbox_to_anchor=(1,1),fontsize=11)
plt.tight_layout(pad=3, rect=[0, 0, 0.7, 1.0])
ylbl = "Rotation rate [ " + "$^\circ$" + "/minute ]"
plt.ylabel(ylbl)
xlbl = "Latitude [ " + "$^\circ$" + " ]"
plt.xlabel(xlbl)
plt.show()

#rot_lat = durmx_df.iloc[:,6:10]
#rot_fname = wk_path + r'\SummerSolstice-06212017\rot_lat_ss.csv'
#rot_lat.to_csv(rot_fname)

# plot tracking profile with respect to latitude
cmap = plt.get_cmap('brg')
colors = [cmap(i) for i in np.linspace(0, 1, len(siteLat))]
pLabels = []
for s in siteLat:
    lb = "%0.0f" % s + "$^\circ$" + " Latitude"
    pLabels.append(lb)
timeFmt = mdates.DateFormatter('%H:%M')
fig = plt.figure(figsize=(12,10), facecolor='w')
ax = fig.add_subplot(111)
idx = 0
for tr in trackAngl:
    ax.plot(tr.tracker_theta, label = pLabels[idx], color = colors[idx], 
            linewidth = 0.5, alpha = 0.8)
    idx = idx + 1
ax.get_yaxis().set_tick_params(direction='in')
ax.get_xaxis().set_tick_params(direction='in')
plt.ylim(-50,50)
ax.xaxis.set_major_formatter(timeFmt)
plt.legend(loc='upper left', bbox_to_anchor=(1,1),fontsize=8)
plt.tight_layout(pad=3, rect=[0, 0, 0.9, 1.0])
ylbl = "Tracker Angle [ " + "$^\circ$" + "]"
plt.ylabel(ylbl)
plt.title('Tracking Profile for Spring Equinox 3/20/17')
plt.show()


# plot one example tracking profile
timeFmt = mdates.DateFormatter('%H:%M')
fig = plt.figure(figsize=(10,6), facecolor='w')
ax = fig.add_subplot(111)
idx = 0
ax.plot(trackAngl[40].tracker_theta, color = 'b', linewidth = 1)
ax.get_yaxis().set_tick_params(direction='in')
ax.get_xaxis().set_tick_params(direction='in')
plt.ylim(-50,50)
ax.xaxis.set_major_formatter(timeFmt)
plt.tight_layout(pad=3, rect=[0, 0, 0.9, 1.0])
ylbl = "Tracker Angle [ " + "$^\circ$" + "]"
plt.ylabel(ylbl)
plt.show()

# plot tracking differential profile with respect to latitude
cmap = plt.get_cmap('brg')
colors = [cmap(i) for i in np.linspace(0, 1, len(siteLat))]
pLabels = []
for s in siteLat:
    lb = "%0.0f" % s + "$^\circ$" + " Latitude"
    pLabels.append(lb)
timeFmt = mdates.DateFormatter('%H:%M')
fig = plt.figure(figsize=(12,10), facecolor='w')
ax = fig.add_subplot(111)
idx = 0
for td in trdiff_ls:
    ax.plot(td, label = pLabels[idx], color = colors[idx], 
            linewidth = 0.5, alpha = 0.8)
    idx = idx + 1
ax.get_yaxis().set_tick_params(direction='in')
ax.get_xaxis().set_tick_params(direction='in')
plt.ylim(-1.5,1)
ax.xaxis.set_major_formatter(timeFmt)
plt.legend(loc='upper left', bbox_to_anchor=(1,1),fontsize=8)
plt.tight_layout(pad=3, rect=[0, 0, 0.9, 1.0])
ylbl = "Change in Angle [ " + "$^\circ$" + "]"
plt.ylabel(ylbl)
plt.title('Tracking Differential for Spring Equinox 3/20/17')
plt.show()


# plot locations on map
cmap = plt.get_cmap('brg')
colors = [cmap(i) for i in np.linspace(0, 1, len(siteLat))]
lats = siteLat
lons = [siteLong] * len(siteLat)
fig = plt.figure(figsize=(8,6), facecolor='w')
m = Basemap(llcrnrlon=-125.,llcrnrlat=-5.,urcrnrlon=-90.,urcrnrlat=60.,
            projection='lcc',lat_1=20.,lat_2=40.,lon_0=-60.,
            resolution ='l',area_thresh=1000.)
m.drawmapboundary(fill_color='cyan')
m.fillcontinents(color='0.9',lake_color='cyan',zorder=0)
m.drawcoastlines(linewidth = 0.5)
m.drawstates()
m.drawcountries()
m.drawparallels(np.arange(0,80,10),labels=[1,1,0,0])
m.drawmeridians(np.arange(-150,-80,10),labels=[0,0,0,1])
x, y = m(lons,lats)
m.scatter(x, y, s = 5, marker='D', color= colors)
plt.show()







