# load results
results = Bay_final_results001

reg_num = length(results)
Bay_days_to_detection = numeric(reg_num)
detected_cluster = numeric(reg_num)

# input the results of detection algorithm
for (i in 1:reg_num) {

  detected_cluster[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Bay_days_to_detection[i] = results[[i]][[3]] - 300
}

mean(Bay_days_to_detection)
sd(Bay_days_to_detection)

# percentage of detection by day i
pct_detection_by_day_i = numeric(30)
for (i in 1:30) pct_detection_by_day_i[i] =  sum(Bay_days_to_detection <= i)/reg_num

# plot the density of days to detection
days = 1:30
plot(days,pct_detection_by_day_i,main = "Percentage of Outbreaks Detected by Bayesian scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Bay_accuracy = numeric(reg_num)
Bay_sensitivity =  numeric(reg_num)
Bay_ppv = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster[i]]]))])
  Bay_sensitivity[i] = pop_in/sum(pop[zones[[i]]])
  Bay_ppv[i] = pop_in/sum(pop[zones[[detected_cluster[i]]]])
  Bay_accuracy[i] = (pop_in + pop_out)/sum(pop)
}

plot(Bay_sensitivity, main = "Sensitivity plot for Bayesian Scan", ylab = "Sensitivity", xlab = "Injected Location")
plot(Bay_ppv, main ="Positive Predictive Value Plot for Bayesian scan (PPV)", xlab = "Injected Location", ylab = "PPV")
plot(Bay_accuracy, main = "Accuracy plot for Bayescian scan", ylab = "Accuracy Percentage", xlab = "Injected Location")
