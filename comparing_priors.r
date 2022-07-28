# load results for prior 0.10
results_0.10 = Bay_final_results_0.10

reg_num = length(results_0.10)
Bay_days_to_detection_0.10 = numeric(reg_num)
detected_cluster_0.10 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.10[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.10[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.10)
sd(Bay_days_to_detection_0.10)

# percentage of detection by day i
pct_detection_by_day_i_0.10 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.10[i] =  sum(Bay_days_to_detection_0.10 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.10,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.10 = numeric(reg_num)
Bay_sensitivity_0.10 =  numeric(reg_num)
Bay_ppv_0.10 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.10[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.10[i]]]))])
  Bay_sensitivity_0.10[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.10[i] = pop_in/sum(pop[zones[[detected_cluster_0.10[i]]]])
  Bay_accuracy_0.10[i] = (pop_in + pop_out)/sum(pop)
}


# load results for prior 0.05
results_0.05 = Bay_final_results_0.05

reg_num = length(results_0.05)
Bay_days_to_detection_0.05 = numeric(reg_num)
detected_cluster_0.05 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.05[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.05[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.05)
sd(Bay_days_to_detection_0.05)

# percentage of detection by day i
pct_detection_by_day_i_0.05 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.05[i] =  sum(Bay_days_to_detection_0.05 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.05,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.05 = numeric(reg_num)
Bay_sensitivity_0.05 =  numeric(reg_num)
Bay_ppv_0.05 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.05[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.05[i]]]))])
  Bay_sensitivity_0.05[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.05[i] = pop_in/sum(pop[zones[[detected_cluster_0.05[i]]]])
  Bay_accuracy_0.05[i] = (pop_in + pop_out)/sum(pop)
}


# load results for prior 0.01
results_0.01 = Bay_final_results_0.01

reg_num = length(results_0.01)
Bay_days_to_detection_0.01 = numeric(reg_num)
detected_cluster_0.01 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.01[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.01[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.01)
sd(Bay_days_to_detection_0.01)

# percentage of detection by day i
pct_detection_by_day_i_0.01 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.01[i] =  sum(Bay_days_to_detection_0.01 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.01,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.01 = numeric(reg_num)
Bay_sensitivity_0.01 =  numeric(reg_num)
Bay_ppv_0.01 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.01[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.01[i]]]))])
  Bay_sensitivity_0.01[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.01[i] = pop_in/sum(pop[zones[[detected_cluster_0.01[i]]]])
  Bay_accuracy_0.01[i] = (pop_in + pop_out)/sum(pop)
}

# load results for prior 0.005
results_0.005 = Bay_final_results_0.005

reg_num = length(results_0.005)
Bay_days_to_detection_0.005 = numeric(reg_num)
detected_cluster_0.005 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.005[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.005[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.005)
sd(Bay_days_to_detection_0.005)

# percentage of detection by day i
pct_detection_by_day_i_0.005 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.005[i] =  sum(Bay_days_to_detection_0.005 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.005,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.005 = numeric(reg_num)
Bay_sensitivity_0.005 =  numeric(reg_num)
Bay_ppv_0.005 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.005[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.005[i]]]))])
  Bay_sensitivity_0.005[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.005[i] = pop_in/sum(pop[zones[[detected_cluster_0.005[i]]]])
  Bay_accuracy_0.005[i] = (pop_in + pop_out)/sum(pop)
}

# # load results for prior 0.001
# results_0.001 = Bay_final_results_0.001
# 
# reg_num = length(results_0.001)
# Bay_days_to_detection_0.001 = numeric(reg_num)
# detected_cluster_0.001 = numeric(reg_num)
# 
# # input the results of detection algorithm
# for (i in 1:reg_num) {
#   
#   detected_cluster_0.001[i] = results[[i]][[2]]
#   # subtract the first 300 null days
#   Bay_days_to_detection_0.001[i] = results[[i]][[3]] 
# }
# 
# mean(Bay_days_to_detection_0.001)
# sd(Bay_days_to_detection_0.001)
# 
# # percentage of detection by day i
# pct_detection_by_day_i_0.001 = numeric(30)
# for (i in 1:30) pct_detection_by_day_i_0.001[i] =  sum(Bay_days_to_detection_0.001 <= i)/reg_num
# 
# # plot the density of days to detection
# days = 1:30
# plot(days,pct_detection_by_day_i_0.001,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")
# 
# # initiate the accuracy, sensitivity and PPV
# Bay_accuracy_0.001 = numeric(reg_num)
# Bay_sensitivity_0.001 =  numeric(reg_num)
# Bay_ppv_0.001 = numeric(reg_num)
# 
# for (i in 1:reg_num) {
#   pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.001[i]]])])
#   pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.001[i]]]))])
#   Bay_sensitivity_0.001[i] = pop_in/sum(pop[zones[[i]]])
#   Bay_ppv_0.001[i] = pop_in/sum(pop[zones[[detected_cluster_0.001[i]]]])
#   Bay_accuracy_0.001[i] = (pop_in + pop_out)/sum(pop)
# }



# load results for prior 0.0005
results_0.0005 = Bay_final_results_0.0005

reg_num = length(results_0.0005)
Bay_days_to_detection_0.0005 = numeric(reg_num)
detected_cluster_0.0005 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.0005[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.0005[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.0005)
sd(Bay_days_to_detection_0.0005)

# percentage of detection by day i
pct_detection_by_day_i_0.0005 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.0005[i] =  sum(Bay_days_to_detection_0.0005 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.0005,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.0005 = numeric(reg_num)
Bay_sensitivity_0.0005 =  numeric(reg_num)
Bay_ppv_0.0005 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.0005[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.0005[i]]]))])
  Bay_sensitivity_0.0005[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.0005[i] = pop_in/sum(pop[zones[[detected_cluster_0.0005[i]]]])
  Bay_accuracy_0.0005[i] = (pop_in + pop_out)/sum(pop)
}

# load results for prior 0.0001
results_0.0001 = Bay_final_results_0.0001

reg_num = length(results_0.0001)
Bay_days_to_detection_0.0001 = numeric(reg_num)
detected_cluster_0.0001 = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {
  
  detected_cluster_0.0001[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection_0.0001[i] = results[[i]][[3]] 
}

mean(Bay_days_to_detection_0.0001)
sd(Bay_days_to_detection_0.0001)

# percentage of detection by day i
pct_detection_by_day_i_0.0001 = numeric(30)
for (i in 1:30) pct_detection_by_day_i_0.0001[i] =  sum(Bay_days_to_detection_0.0001 <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i_0.0001,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy_0.0001 = numeric(reg_num)
Bay_sensitivity_0.0001 =  numeric(reg_num)
Bay_ppv_0.0001 = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster_0.0001[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster_0.0001[i]]]))])
  Bay_sensitivity_0.0001[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv_0.0001[i] = pop_in/sum(pop[zones[[detected_cluster_0.0001[i]]]])
  Bay_accuracy_0.0001[i] = (pop_in + pop_out)/sum(pop)
}
