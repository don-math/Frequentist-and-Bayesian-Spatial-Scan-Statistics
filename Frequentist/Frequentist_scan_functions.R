library(scanstatistics)
library(foreach)
# preliminaries
data(neastdata)
coords = neastdata[,c("easting", "northing")]
ubpop = 0.01
pop = neastdata$population
zones = smerc::scan.zones(coords, pop, ubpop)

start_time <- Sys.time()

days_num = 330
location_num = 245

# rr stands for relative risk
rr = 1/10000


# data generation function
outbreak_simulation = function(injection)
{ data = foreach(z = injection) %do%
  {
    # create cases
    cases = rpois(days_num*location_num,lambda = rep(pop*rr, each = days_num))
    
    # prepare the data set for injection
    outbreak_multiple = matrix(cases,nrow = location_num, byrow = TRUE)
    
    # inject the cases in zone #z
    # z to be determined by user, I set it to be 1000 for convenience.
    outbreak_region = zones[[z]]
    
    
    # inject the cases to the last 30 days
    # create the data set outbreak_multiple in which the outbreak start from day 301 to day 330
    for(i in 1:30) 
    {
      for (j in 1:length(outbreak_region)) {
        
        outbreak_multiple[outbreak_region[j],300+i] = outbreak_multiple[outbreak_region[j],300+i] + rpois(1,pop[outbreak_region[j]]*rr*i/10)
        
      }
      
    }
    outbreak_multiple
    
  }

return(data)
}




# set the number of replicas
n_mcsim = 999

# replicate the population for the input of scan_pb_poisson function
population = matrix(rep(pop,330), ncol = 245, byrow = TRUE)


# Kulldorff detection function
Kull_detection = function(outbreak_multiple){
  
  counts_total = t(outbreak_multiple)
  
  for (i in 1:30) {
    counts_partial = counts_total[-c((300+i+1):331), ]
    res <- scan_pb_poisson(counts = counts_partial,
                           zones = zones,
                           population = population,
                           n_mcsim = n_mcsim,
                           gumbel = TRUE,
                           max_only = TRUE)
    
    if(res[[7]]<0.033){
      return(list(res$MLC$zone_number,i,res[[7]]))
      break
      }
   
  }

 
}




