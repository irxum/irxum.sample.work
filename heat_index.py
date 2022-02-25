#!/usr/bin/env python3

class HeatIndex(object):
    '''
    The HeatIndex object contains info about locations and their
    heat index values derived from their temperature and humidity 
    measurements.

    Args:
        daily_temp (list): list of strings.  Each string
            should be a three letter location followed by 
            temperature values separated by commas.
        daily_humidity (list):  list of strings.  Each string
            should be a three letter locatoin followed by
            humidity values separated by commas.
        
    Attributes:
        locations_heat_index: This is where we store the 
        for each location, its associated temperature measurements,
        its associated humidity measurements, its derived
        heat index values, its minimum heat index value, and
        its maximum heat index value.
    '''    
    def __init__(self, daily_temp, daily_humidity):
        print('Created HeatIndex')
        self.locations_heat_index = {}
        
        for t_str in daily_temp:
            location, temp_dict = self.build_entry_dict(t_str, "temp")
            self.update_locations_meas(location, temp_dict)
        
        for h_str in daily_humidity:
            location, humidity_dict = self.build_entry_dict(h_str, "humidity")
            self.update_locations_meas(location, humidity_dict)  
            
        self.update_locations_heat_index()
         
    '''
    The HeatIndexMeasError is an exception raised when a location
    is missing a type of measurement (temperature or humdity)
    
    Args:
        location (str): a three letter representation of a location
        meas (str): measurement type
        
    Attributes:
        msg:  Message indicated which measurement type is missing.
    '''
    class HeatIndexMeasError(Exception):
        def __init__(self, location, meas):
            self.msg = (
                f'Location, {location},'
                f' is missing {meas} measurements.'
                '  No heat index created.'
            )
    
    '''
    The HeatIndexMismatchError is an exception raised when a location
    has unequal number of temperature and humidity measurements.
    
    Args:
        location (str): a three letter representation of a location
        tmp_ls (list): list of temperature measurements
        hmd_ls (list): list of humidity measurements
        
    Attributes:
        msg:  Message indicated location has unequal number of 
            measurements.
    '''
    class HeatIndexMismatchError(Exception):
        def __init__(self, location, tmp_ls, hmd_ls):
            n_tmp_ls = len(tmp_ls)
            n_hmd_ls = len(hmd_ls)
            self.msg = (
                f'Location, {location},'
                f' has {n_tmp_ls} temperature measurements'
                f' but {n_hmd_ls} humidity measurements.'
                '  Check for missing or extra measurements.'
                '  No heat index created.'
            )

    '''
    The HeatIndexParamError is an exception raised when a location
    does not have a parameter
    
    Args:
        loc (str): three letter representation of location
        param (str): name of parameter
        
    Attributes:
        loc:  location
        param:  name of parameter
    '''
    class HeatIndexParamError(Exception):
        def __init__(self, loc, param):
            self.loc = loc
            self.param = param
    
    '''
    Update the locations_heat_index dictionary for a specific 
    location with measurement
    
    Args:
        locxn (str): a three letter representation of a location
        meas_dict (dict): dictionary of measurement name and values
    '''
    def update_locations_meas(self, locxn, meas_dict):
        if locxn in self.locations_heat_index.keys():
            self.locations_heat_index[locxn].update(meas_dict)
        else:
            self.locations_heat_index[locxn] = meas_dict
            
    '''
    Updates the locations_heat_index dictionary for each 
    location with heat index values and parameters
    
    Raises:
        HeatIndexMeasError:  if missing temperature or humidity
            measurements.
        HeatIndexMismatchError:  if number of temperature 
            measurements is not equal to number of humidity
            measurements.
    '''
    def update_locations_heat_index(self):
        for loc, measures in self.locations_heat_index.items():
            t_ls = measures.get("temp")
            h_ls = measures.get("humidity")
            
            try:
                if t_ls is None:
                    raise(self.HeatIndexMeasError(loc, "temperature"))
                elif h_ls is None:
                    raise(self.HeatIndexMeasError(loc, "humidity"))
                elif len(h_ls) != len(t_ls):
                    raise(self.HeatIndexMismatchError(loc, t_ls, h_ls))
                else:
                    th_ls = list(zip(t_ls, h_ls))
                    hi_ls = [self.heat_index(tval, hval) for tval, hval in th_ls]

                    # sort heat index list only once to get min and max
                    hi_ls_sorted = hi_ls.copy()
                    hi_ls_sorted.sort() 

                    hi_dict = {
                        "heat_index": hi_ls,
                        "heat_index_min": hi_ls_sorted[0],
                        "heat_index_max": hi_ls_sorted[-1]
                    }
                    self.locations_heat_index[loc].update(hi_dict)
            except self.HeatIndexMeasError as MeasError:
                print(MeasError.msg)
            except self.HeatIndexMismatchError as MismatchError:
                print(MismatchError.msg)

    '''
    Creates an entry dictionary that holds the measurement name
    and its associated values in float type as well as extracting
    the location from the string
    
    Args:
        entry_str (str): string of raw data
        measure_name (str):  name of measurement (temperature or humidity)
        
    Returns:
        loc (str):  three letter representation of location
        entry_dict (dict):  holds the measurement type and values
    '''
    @staticmethod
    def build_entry_dict(entry_str, measure_name):
        entry_parts = entry_str.split(',')
        loc = entry_parts.pop(0)
        entry_dict = {measure_name: [float(e) for e in entry_parts]}
        
        return loc, entry_dict
    
    '''
    Calculates heat index
    
    Args:
        temp (float):  temperature measurement
        humidity (float): humidity measurement
        
    Returns:
        calculated heat index as a float type
    '''
    @staticmethod
    def heat_index(temp, humidity):
        return round(1.98*(temp-(.55-.0055*humidity)*(temp-58))- 56.83, 1)
    
    '''
    provides the max heat index for a location
    
    Args:
        loc (string):  three letter representation of a location
        
    Returns:
        maximum heat index as a float
    '''    
    def max_for_location(self, loc):
        result = self.check_locations_heat_index_min_max(loc, 'heat_index_max')
        return result

    '''
    provides the min heat index for a location
    
    Args:
        loc (string):  three letter representation of a location
        
    Returns:
        minimum heat index as a float
    '''  
    def min_for_location(self, loc):
        result = self.check_locations_heat_index_min_max(loc, 'heat_index_min')
        return result
    
    '''
    checks for min or max heat index for a location
    
    Args:
        loc (string):  three letter representation of a location
        param (string):  heat_index_min or heat_index_max
        
    Raises:
        HeatIndexParamError if no heat_index_max or heat_index_min
        
    Returns:
        minimum or maximum heat index as a float
    '''  
    def check_locations_heat_index_min_max(self, loc, param):
        heat_index_param = self.locations_heat_index.get(loc).get(param)
        try:
            if heat_index_param is None:
                raise(self.HeatIndexParamError(loc, param))
            else:
                return heat_index_param
        except self.HeatIndexParamError as p:
            print(f"No {p.param} or heat index at all for {p.loc}.")
            
    
temps=["SJC,81,81,82,83,84,85,83",
       "LAX,74,75,76,78,79,81,82",
       "OAK,78,77,79,81,83,83,82",
       "TPA,66,68,69,71,73,75,77"]

humidity=["LAX,26,26,26,26,27,27,27",
          "TPA,81,82,83,84,85,92,98",
          "MIA,71,73,75,78,82,82,81",
          "SJC,50,51,52,53,54,55,56"]

heat_index = HeatIndex(temps, humidity)

assert heat_index.max_for_location('SJC') == 98.2
assert heat_index.min_for_location('LAX') == 76.8
assert heat_index.min_for_location('MIA') == 76.8
