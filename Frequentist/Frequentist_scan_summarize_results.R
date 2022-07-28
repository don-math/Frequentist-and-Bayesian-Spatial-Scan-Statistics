
# build vectors to record the days to detection 
# and detected clusters
# load results of Frequentist scan
# make sure you run pop, otherwise you will get many zeros 
# for sensitivity, ppv, accuracy

results = Kull_final_result

# didn't not detect the outbreak in the following zones
# replace those results with no detection
undetected_points = c(437,720,1002)

for (i in 1:length(undetected_points)) {
  Kull_final_result[[undetected_points[i]]] = list(undetected_points[i], 1, 30, 1)
  
}

reg_num = length(results)
Freq_days_to_detection = numeric(reg_num)
detected_cluster = numeric(reg_num)

sd(Freq_days_to_detection)

for (i in 1:reg_num) {

  detected_cluster[i] = results[[i]][[2]]
  # subtract the first 300 null days
  Freq_days_to_detection[i] = results[[i]][[3]]
}




# percentage of detection by day i
Freq_pct_detection_by_day_i = numeric(30)
for (i in 1:30) Freq_pct_detection_by_day_i[i] =  sum(Freq_days_to_detection <= i)/reg_num


# summarize frequentist scan detection time
summary(Freq_days_to_detection)

# plot the density of days to detection
days = 1:30
plot(days,Freq_pct_detection_by_day_i,main = "Percentage of Outbreaks Detected by Frequentist scan", ylab = "Percentage", xlab = "Days")

# initiate the accuracy, sensitivity and PPV
Freq_accuracy = numeric(reg_num)
Freq_sensitivity =  numeric(reg_num)
Freq_ppv = numeric(reg_num)

for (i in 1:reg_num) {
  pop_in = sum(pop[intersect(zones[[i]],zones[[detected_cluster[i]]])])
  pop_out = sum(pop[intersect(setdiff(c(1:245), zones[[i]]),setdiff(c(1:245), zones[[detected_cluster[i]]]))])
  Freq_sensitivity[i] = pop_in/sum(pop[zones[[i]]])
  Freq_ppv[i] = pop_in/sum(pop[zones[[detected_cluster[i]]]])
  Freq_accuracy[i] = (pop_in + pop_out)/sum(pop)
}

plot(Freq_sensitivity, main = "Sensitivity plot for Frequentist Scan", ylab = "Sensitivity", xlab = "Injected Location")
plot(Freq_ppv, main ="Positive Predictive Value Plot for Frequentist scan (PPV)", xlab = "Injected Location", ylab = "PPV Percentage")
plot(Freq_accuracy, main = "Accuracy plot for Frequentist scan", ylab = "Accuracy Percentage", xlab = "Injected Location")