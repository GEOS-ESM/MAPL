from scipy.io import FortranFile
import numpy as np
from datetime import datetime

def get_record_info(input_file,year):

   binary_file = FortranFile(input_file,'r')
   nrec=0
   year_start = None
   year_end = None
   while True:
       try:
          hdr = binary_file.read_reals(dtype=np.float32)
       except (OSError):
          break
       bin_var = binary_file.read_reals(dtype=np.float32)
       record_size = bin_var.size
       nrec=nrec+1
       if year != None:
          int_time = hdr.astype(int)
          mid_time = compute_time_midpoint(int_time)
          current_year = mid_time.year
          if (year_start == None) and (current_year ==year):
             year_start=nrec
          if (year_start != None) and (current_year != year) and (year_end == None):
             year_end=nrec-1

   binary_file.close()
   return nrec,record_size,year_start,year_end

def compute_time_midpoint(int_time):
      time1 = datetime(int_time[0],int_time[1],int_time[2],minute=int_time[3],hour=int_time[4],second=int_time[5])
      time2 = datetime(int_time[6],int_time[7],int_time[8],minute=int_time[9],hour=int_time[10],second=int_time[11])
      mid_time = time1+(time2-time1)/2 
      return mid_time

def compute_time_variable(mid_times):
   ndates = len(mid_times)
   times = np.zeros(ndates,dtype=np.float64)
   i=0
   for time in mid_times:
       dt = time - mid_times[0]
       seconds = dt.total_seconds()
       times[i] = seconds//60//60//24
       i=i+1
   return times
