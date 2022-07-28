library(scanstatistics)
library(foreach)


# benchmark the performance

# 

bad_points = 437
# create simulated data
injection_zones = bad_points
simulated_data = outbreak_simulation(injection_zones)

# put the simulated data into test

Kull_result = foreach(outbreak_multiple = simulated_data) %do%
  {Kull_detection(outbreak_multiple)}


# merge with the injection zones to get a complete result

Kull_final_result = foreach(t = injection_zones) %do%
  {list(t,Kull_result[[t]][[1]],Kull_result[[t]][[2]],Kull_result[[t]][[3]])}


save(Kull_final_result, file="Kull_final_result.RData")


# do it again for bad points
# put the simulated data into test

Kull_bad_point_result = foreach(outbreak_multiple = simulated_data) %do%
  {Kull_detection(outbreak_multiple)}



# merge with the injection zones to get a complete result

Kull_bad_point_final_result = foreach(t = injection_zones) %do%
  {list(t,Kull_result[[t]][[1]],Kull_result[[t]][[2]],Kull_result[[t]][[3]])}


save(Kull_bad_point_final_result, file="Kull_bad_point_final_result.RData")
