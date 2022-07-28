# preparing packages
install.packages("devtools") 
install.packages("doParallel") 
install.packages("smerc") 
install.packages("Brobdingnag") 
install.packages("foreach") 
library(devtools)
install_github("jfrench/neastbenchmark")


# load libraries
# initilize the libraries
library(neastbenchmark)
library(parallel)
cl = makeCluster(detectCores()-1)

library(doParallel)
registerDoParallel(cl)

library(neastbenchmark)
library(Brobdingnag)
library(foreach)
set.seed(1)

# preliminaries
data(neastdata)
coords = neastdata[,c("easting", "northing")]
ubpop = 0.01
pop = neastdata$population
zones = smerc::scan.zones(coords, pop, ubpop)

# set the number of locations (region)
reg_num = length(zones)
days_num = 330
location_num = 245

# set the prior probabilities of the null and alternative hypothesis

alt_prob = 0.01
null_prob = 1 - alt_prob

# rr stands for relative risk
rr = 1/10000

# save the results 
Bay_final_results001 = foreach(t = mis_locations, .packages = c("foreach","neastbenchmark","parallel","doParallel","Brobdingnag")) %do% {
  
  # create cases
  cases = rpois(days_num*location_num,lambda = rep(pop*rr, each = days_num))
  
  # prepare the data set for injection
  outbreak_multiple = matrix(cases,nrow = location_num, byrow = TRUE)

# inject the cases in zone #z
# t to be determined by user, I set it to be 1000 for convenience.
outbreak_region = zones[[t]]
zone_num = length(outbreak_region)

# inject the cases to the last 30 days
# create the data set outbreak_multiple in which the outbreak start from day 301 to day 330
for(i in 1:30) 
{
  for (j in 1:zone_num) {
    
    outbreak_multiple[outbreak_region[j],300+i] = outbreak_multiple[outbreak_region[j],300+i] + rpois(1,pop[outbreak_region[j]]*rr*i/10)
    
  }
  
}

  # calculate the mean and variance inside and outside a region (location)
  e.sample.in = numeric(reg_num)
  var.sample.in = numeric(reg_num)
  e.sample.out = numeric(reg_num)
  var.sample.out = numeric(reg_num)


e.var.in.out = foreach (region.index = 1:reg_num, .combine = "rbind") %dopar%
  {
    # pick out the region (a cluster of locations) from zones list
    region = zones[[region.index]]
    
    # initiate the recording the relative risk inside the region for 300 null days
    c.in = numeric(300)
    
    for (day in 1:300)
    {
      c.in[day] = sum(outbreak_multiple[region,day])/sum(pop[region])
    }
    
    # calculate e.sample.in and var.sample.in
    e.sample.in  = mean(c.in)
    var.sample.in = var(c.in)
    
    # record the relative risk outside the region
    c.out = numeric(300)
    
    for (day in 1:300)
    {
      c.out[day] = sum(outbreak_multiple[-region,day])/sum(pop[-region])
    }
    
    # calculate e.sample.out and var.sample.out
    e.sample.out  = mean(c.out)
    var.sample.out = var(c.out)
    c(e.sample.in,var.sample.in,e.sample.out,var.sample.out)
  }

  e.sample.in = e.var.in.out[,1]
  var.sample.in = e.var.in.out[,2]
  
  e.sample.out = e.var.in.out[,3]
  var.sample.out = e.var.in.out[,4]
  
  
  # calculate alpha and betas
  alpha.in = (e.sample.in)^2/(var.sample.in)
  beta.in = (e.sample.in)/(var.sample.in)
  
  alpha.out = (e.sample.out)^2/(var.sample.out)
  beta.out = (e.sample.out)/(var.sample.out)



# calculate the mean and variance in all regions (location)
c.all = numeric(300)
for (j in 1:300) c.all[j] = sum(outbreak_multiple[,j])/sum(pop)

# calculate e.sample.out and var.sample.out
e.sample.all = mean(c.all)
var.sample.all = var(c.all)

# calculate alpha and betas
alpha.all = (e.sample.all)^2/(var.sample.all)
beta.all = (e.sample.all)/(var.sample.all)

# generate the weight for different likelihoods
weight = c(0:10)

# calculate the posterior probability for each region
post_h1 = function(i,j){
  
  # calculate the weighted log posterior prob of the i-th region and the j-th day
  log.part1u =  ((0.2*weight+1)*alpha.in[i])*log(beta.in[i])+lgamma((0.2*weight+1)*alpha.in[i]+sum(outbreak_multiple[zones[[i]],j]))
  log.part1l = ((0.2*weight+1)*alpha.in[i]+sum(outbreak_multiple[zones[[i]],j]))*log(beta.in[i]+sum(pop[zones[[i]]]))+lgamma((0.2*weight+1)*alpha.in[i])
  
  log.part2u = alpha.out[i]*log(beta.out[i])+lgamma(alpha.out[i]+sum(outbreak_multiple[-zones[[i]],j]))
  log.part2l = (alpha.out[i]+sum(outbreak_multiple[-zones[[i]],j]))*log(beta.out[i]+sum(pop[-zones[[i]]]))+lgamma(alpha.out[i])
  
  return(mean(log.part1u-log.part1l+log.part2u-log.part2l))
  
}


# under null hypothesis
# calculate the log posterior probability for the data for the j-th day

sum_outbreak = colSums(outbreak_multiple)
tpop = sum(pop)
l_beta.all_tpop  = log(beta.all+tpop)

log.part1u =  alpha.all*log(beta.all)
log.part1l = l_beta.all_tpop*(alpha.all+sum_outbreak)
log.part2u = lgamma(alpha.all+sum_outbreak)
log.part2l = lgamma(alpha.all)


log.part1_all =  log.part1u - log.part1l
log.part2_all = log.part2u - log.part2l

post_h0 = function(j){
  
  # calculate the log posterior prob of the j-th day
  # log.part1u =  alpha.all*log(beta.all)
  log.part1 =  log.part1_all[j]
  
  log.part2 = log.part2_all[j]
  # log.part2l = lgamma(alpha.all)
  
  # add a large constant to minimize the rounding error
  # of brob numbers near zero
  return(log.part1 + log.part2)
  
}


# build a list to record the posterior probability
# of alternative hypothesis, note I use a list since the
# matrix is not available in brob package
# takes 200s-ish to run

ph1_data = vector("list",length = 330)

ph1_data = foreach(j = 1:330, .packages = 'Brobdingnag') %dopar% {
  
  ph1_day = as.brob(rep(1,reg_num))
  
  for (i in 1:reg_num) {
    
    # convert the log posterior to original posterior
    # have to use brob numbers otherwise they will inflow
    ph1_day[i] = exp(as.brob(post_h1(i,j)))
  }
  ph1_day
}

# build a list to record the posterior probability
# of null hypothesis, note I use a list since
# matrix is not available in brob package

ph0_data = vector("list",length = 330)

for (j in 1:330) {
  # initial ph0_day with exp(0)
  ph0_data[[j]] = exp(as.brob(post_h0(j)))
}



fraction_lower_part  = list()


for (j in 1:330) {
  
  fraction_lower_right_part = ph0_data[[j]]*null_prob
  
  fraction_lower_left_part= sum(ph1_data[[j]][1:reg_num])*alt_prob/reg_num
  
  fraction_lower_part[[j]] = fraction_lower_left_part + fraction_lower_right_part
  
}



# calculate the posterior of i th location or j th day
post = function(i,j) {
  
  fraction_upper_part = ph1_data[[j]][i]*alt_prob/reg_num
  
  return(fraction_upper_part/fraction_lower_part[[j]])
  
}

# calculate posterior probabilities and save in a list
post_prob = vector("list",length = 330)

post_prob = foreach(j = 1:330, .packages = 'Brobdingnag') %dopar% {
  
  post_prob_day = as.brob(rep(1,reg_num))
  
  for (i in 1:reg_num) {
    
    # convert the log posterior to original posterior
    # have to use brob numbers otherwise they will inflow
    post_prob_day[i] = as.brob(post(i,j))
  }
  post_prob_day
}

# record the highest log posterior on each null day
# make record.max as brob type to save the extreme small numbers of posteriors
record.max = rep(0,330)
max.region = rep(0,330)

# cost 200s-ish to run
for(j in 1:330){
  
  post.count = post_prob[[j]]
  record.max[j]= max(log(post.count))
  max.region[j] = which.max(log(post.count))
  
}


# mark the 96.666th quantile of the posterior of the null days
# convert record.max as numeric to use quantile function
# since quantile function is not defined for brob data

threshold = quantile(record.max[-c(301:330)], probs = 0.96666)[[1]]

#detection process
#after 300 null days, scan through each location and each day
# output the location and the day index when the posterior
# is higher than the threshold

# cost 15s-ish to run


for (j in 301:330){
  for(i in 1:reg_num)
  {
    if(log(post_prob[[j]][i]) > threshold){break}
  }
  if(log(post_prob[[j]][i]) > threshold){break}
}

return(list(t,max.region[j],j,log(post_prob[[j]][max.region[j]])))


}




save(Bay_final_results001, file="Bay_final_results001.RData")



parallel::stopCluster(cl)
