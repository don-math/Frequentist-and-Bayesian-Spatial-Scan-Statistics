library(ggplot2)
library(foreach)
library(scanstatistics)
library(ggpubr)
# build a data.frame to contain results of two methods

method = as.factor(c(rep("Frequentist",reg_num), rep("Bayesian",reg_num)))

# combine the method type, days to detection, sensitivity, ppv, accuracy, injection sizes 
# for both methods
days_combined = c(Freq_days_to_detection,Bay_days_to_detection)
sensitivity_combined = c(Freq_sensitivity,Bay_sensitivity)
ppv_combined = c(Freq_ppv,Bay_ppv)
accuracy_combined = c(Freq_accuracy,Bay_accuracy)
injection_size = rep(foreach(i = 1:reg_num, .combine = "c") %do% length(zones[[i]]), 2)

# save in one data frame
visual_data = data.frame(method,days_combined,sensitivity_combined,ppv_combined,accuracy_combined,injection_size)

# create a boxplot for the detection days
b1 = ggplot(visual_data,aes(x=method, y= days_combined)) +
    geom_boxplot(fill = "cornflowerblue", color = "black", notch = FALSE) +
    geom_point(position = "jitter", color = "blue", alpha = 0.5) +
    geom_rug(sides = "1", color = "black") + ylab('days to detection')

b2 = ggplot(visual_data,aes(x=method, y= sensitivity_combined)) +
  geom_boxplot(fill = "cornflowerblue", color = "black", notch = FALSE) +
  geom_point(position = "jitter", color = "blue", alpha = 0.5) +
  geom_rug(sides = "1", color = "black")+ ylab(NULL)+xlab('sensitivity')

b3 = ggplot(visual_data,aes(x=method, y= ppv_combined)) +
  geom_boxplot(fill = "cornflowerblue", color = "black", notch = FALSE) +
  geom_point(position = "jitter", color = "blue", alpha = 0.5) +
  geom_rug(sides = "1", color = "black")+ ylab(NULL) +xlab('ppv')

b4 = ggplot(visual_data,aes(x=method, y= accuracy_combined)) +
  geom_boxplot(fill = "cornflowerblue", color = "black", notch = FALSE) +
  geom_point(position = "jitter", color = "blue", alpha = 0.5) +
  geom_rug(sides = "1", color = "black")+ ylab(NULL) +xlab('accuracy')

# b1 = boxplot(days_combined ~ method, 
#         data = visual_data, 
#         notch = TRUE, 
#         col = (c("gold","darkgreen")), 
#         main = "average days to detection", 
#         ylab = "days",
#         xlab = "method")
# 
# b2 = boxplot(sensitivity_combined ~ method, 
#         data = visual_data, 
#         notch = TRUE, 
#         col = (c("gold","darkgreen")), 
#         main = "average sensitivity", 
#         ylab = NULL,
#         xlab = "method")
# 
# b3 = boxplot(ppv_combined ~ method, 
#         data = visual_data, 
#         notch = TRUE, 
#         col = (c("gold","darkgreen")), 
#         main = "average ppv", 
#         ylab = NULL,
#         xlab = "method") 
# 
# b4 = boxplot(accuracy_combined ~ method, 
#         data = visual_data, 
#         notch = TRUE, 
#         col = (c("gold","darkgreen")), 
#         main = "average accuracy", 
#         ylab = NULL,
#         xlab = "method")
    
# change headers of the visual_data
names(visual_data) = c('method','days', 'sensitivity', 'ppv','accuracy',
                       'injection_size')

# create density plots for both methods
d1 = ggplot(data = visual_data, aes(x = days_combined, fill = method)) +
  geom_density(alpha = 0.3) +
  labs(x="days to detection") 

d2 = ggplot(data = visual_data, aes(x = sensitivity_combined, fill = method)) +
  geom_density(alpha = 0.3) +
  labs(x="sensitivity") +
  theme(legend.position="none")

d3 = ggplot(data = visual_data, aes(x = ppv_combined, fill = method)) +
  geom_density(alpha = 0.3) +
  labs(x="positive predictive value") +
  theme(legend.position="none")+ ylab(NULL)

d4 = ggplot(data = visual_data, aes(x = accuracy_combined, fill = method)) +
  geom_density(alpha = 0.3) +
  labs(x="accuracy")+ ylab(NULL)


# days to detection quantile
plot(quantile(Freq_ppv,probs = seq(0,0.5,0.05)))
plot(quantile(Bay_ppv,probs = seq(0,0.5,0.05)))

# sensitivity quantile
quantile(Freq_sensitivity,probs = seq(0,0.5,0.1))
quantile(Bay_sensitivity,probs = seq(0,0.5,0.1))

# ppv quantile
quantile(Freq_ppv,probs = seq(0,0.5,0.1))
quantile(Bay_ppv,probs = seq(0,0.5,0.1))


# accuracy quantile
quantile(Freq_accuracy,probs = seq(0,0.5,0.1))
quantile(Bay_accuracy,probs = seq(0,0.5,0.1))



ggarrange(d1, b1, nrow = 1, widths = c(1.7,1))
dtotal = ggarrange(d2, d3, d4, nrow = 1, widths = c(1,1,1.42), labels = 'auto')
btotal = ggarrange(b2, b3, b4, nrow = 1, 
                   widths = c(1,1,1), 
                   labels ='auto',
                  vjust = 1.1)

