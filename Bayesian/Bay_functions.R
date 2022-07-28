

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



# calculate the posterior of i th location or j th day
post = function(i,j) {
  
  fraction_upper_part = ph1_data[[j]][i]*alt_prob/reg_num
  
  return(fraction_upper_part/fraction_lower_part[[j]])
  
}
