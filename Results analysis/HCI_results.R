rm(list = ls())
setwd("~/HCI_project/Results analysis")
library(ggplot2)

results_data <- read.csv('HCI_results.csv', header=TRUE, sep=",")

#results from data frame
results_data$time_head <- gsub(",", "", results_data$time_head)
time_head <- as.numeric(results_data$time_head)
print(time_head)
error_head <- as.numeric(results_data$error_rate_head)

time_gesture <- as.numeric(results_data$time_gesture)
error_gesture <- as.numeric(results_data$error_rate_gesture)

#mean, median, standard deviation
mean_time_head <- mean(time_head)
med_time_head <- median(time_head)
sd_time_head <- sd(na.omit(time_head))

mean_error_head <- mean(error_head)
med_error_head <- median(error_head)
sd_error_head <- sd(na.omit(error_head))

mean_time_gesture <- mean(time_gesture)
med_time_gesture <- median(time_gesture)
sd_time_gesture <- sd(na.omit(time_gesture))

mean_error_gesture <- mean(error_gesture)
med_error_gesture <- median(error_gesture)
sd_error_gesture <- sd(na.omit(error_gesture))

print(mean_time_head)
print(med_time_head)
print(sd_time_head)

print(mean_error_head)
print(med_error_head)
print(sd_error_head)

print(mean_time_gesture)
print(med_time_gesture)
print(sd_time_gesture)

print(mean_error_gesture)
print(med_error_gesture)
print(sd_error_gesture)

#visualization of the data (barplots)
time_data <- data.frame(
  name = c("Head", "Gesture"),
  mean_value = c(mean_time_head, mean_time_gesture),
  sd_value = c(sd_time_head, sd_time_gesture)
)
ggplot(time_data) +
  geom_bar(aes(x = name, y = mean_value), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  labs(title = "Mean Execution Time", x = "Tracking Modality", y = "Mean Execution Time in ms") + 
  theme_minimal() +
  scale_y_continuous(expand = c(0.5, 0.2))

error_data <- data.frame(
  name = c("Head", "Gesture"),
  mean_value = c(mean_error_head, mean_error_gesture),
  sd_value = c(sd_error_head, sd_error_gesture)
)
ggplot(error_data) +
  geom_bar(aes(x = name, y = mean_value), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  labs(title = "Mean Error Rate", x = "Tracking Modality", y = "Mean Error Rate as percentage") +
  theme_minimal() +
  scale_y_continuous(expand = c(0.5, 0.2))

#check if data is normally distributed
print(shapiro.test(time_head)) #not normally distributed
print(shapiro.test(error_head)) #normally distributed
print(shapiro.test(time_gesture)) #normally distributed
print(shapiro.test(error_gesture)) #normally distributed

#statistical tests
print(wilcox.test(time_head, time_gesture, paired = TRUE)) #z=0, p=7.629e-06 --> statistically significant difference
print(t.test(error_head, error_gesture, paired = TRUE)) #t(17)=0.14722, p=0.8847 --> not statistically significant difference