library(neastbenchmark)
library(foreach)
set.seed(1)

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

# create cases
cases = rpois(days_num*location_num,lambda = rep(pop*rr, each = days_num))

# prepare the data set for injection
outbreak_multiple = matrix(cases,nrow = location_num, byrow = TRUE)

# inject the cases in zone #z
# z to be determined by user, I set it to be 1000 for convenience.
z = 1
outbreak_region = zones[[z]]


# inject the cases to the last 30 days
# create the data set outbreak_multiple in which the outbreak start from day 301 to day 330
for(i in 1:30) 
{
  for (j in 1:length(outbreak_region)) {
    
  outbreak_multiple[outbreak_region[j],300+i] = outbreak_multiple[outbreak_region[j],300+i] + rpois(1,pop[outbreak_region[j]]*rr*i/10)
  
  }
  
  }


end_time <- Sys.time()
end_time - start_time




