
# Tracking & Latitude During Summer Solstice

## Purpose

Attempt to understand the relationship between latitude and tracking as during Summer Solstice 

(Python script is located [here](https://github.com/irxum/irxum.sample.work/blob/master/latitudeEval.py).  The user specific inputs may need to be adjusted in rows 20-40)

## Part 1.  What does the tracking profile look like as latitudes increase during the Summer Solstice?  

Assuming a GCR of 0.35, we can now plot the tracking profile while changing the latitude of the site location.  The tracking profiles are for 2017 Summer Solstice which is the day of year that daylight and night time are most similar.   The plot below is the tracking profile color-coded to latitude.

![p1image](https://github.com/irxum/irxum.sample.work/blob/master/latitudeTracking_gcr_p35_ss.png)

For an understanding of the location, below is the map with color-coded locations according to latitude.  The longitude was assumed to be the same.  

![p1map](https://github.com/irxum/irxum.sample.work/blob/master/latmap.png)

## Part 2.  How do the duration of the intervals compare?  

First lets define the interval names that will be used.  Below is a typical tracking profile with intervals defined.

![p2diagram](https://github.com/irxum/irxum.sample.work/blob/master/diagram.JPG)

With the intervals defined above, the plot below shows the duration of each interval with respect to latitude.  Notice how the morning and afternoon backtracking overlap each other since the behavior is symmetric about solar noon.  The same can be said about the at limit intervals.  The general trend is that as latitude increases (more North the site is), the daily transit and at limit durations decreases, whereas the backtracking durations increase. 

## Part 3.  How does the rate of rotation (movement) change with respect to latitude?

So it seems that the pertinent information would be how fast does the tracker need to move?  There are a few ways to understand this expressed in ° / minute.

* max instantaneous rotation rate = the maximum instantaneous difference during the tracking profile which is basically the maximum absolute value of the incremental tracking angle difference (see the plot titled "Tracking Differential for Summer Solstice 6/21/17).  As is evident in the plots, this happens when backtracking ends in the morning and begins in the afternoon.
* average morning backtracking = half of range of motion (flat to 45° East) divided by morning backtracking duration
* average afternoon backtracking = half of range of motion (45° West to flat) divided by afternoon backtracking duration
* average daily tracking = full range of motion (45° East to 45° West) divided by daily tracking duration.
    
![p3image](https://github.com/irxum/irxum.sample.work/blob/master/rateofchange_gcr_p35_ss.png)


## Part 4

