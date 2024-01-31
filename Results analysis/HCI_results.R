rm(list = ls())
setwd("~/HCI_project/Results analysis")
library(ggplot2)

results_data <- read.csv('HCI_results.csv', header=TRUE, sep=",")

#results from data frame
results_data$time_head <- gsub(",", "", results_data$time_head)
time_head <- as.numeric(results_data$time_head)
error_head <- as.numeric(results_data$error_rate_head)

time_gesture <- as.numeric(results_data$time_gesture)
error_gesture <- as.numeric(results_data$error_rate_gesture)

a1_head <- results_data$a.1d._head
b1_head <- results_data$b.1d._head
a2_head <- results_data$a.2d._head
b2_head <- results_data$b.2d._head

a1_gesture <- results_data$a.1d._gesture
b1_gesture <- results_data$b.1d._gesture
a2_gesture <- results_data$a.2d._gesture
b2_gesture <- results_data$b.2d._gesture


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

mean_a1_head <- mean(a1_head)
med_a1_head <- median(a1_head)
sd_a1_head <- sd(na.omit(a1_head))

mean_b1_head <- mean(b1_head)
med_b1_head <- median(b1_head)
sd_b1_head <- sd(na.omit(b1_head))

mean_a2_head <- mean(a2_head)
med_a2_head <- median(a2_head)
sd_a2_head <- sd(na.omit(a2_head))

mean_b2_head <- mean(b2_head)
med_b2_head <- median(b2_head)
sd_b2_head <- sd(na.omit(b2_head))

mean_a1_gesture <- mean(a1_gesture)
med_a1_gesture <- median(a1_gesture)
sd_a1_gesture <- sd(na.omit(a1_gesture))

mean_b1_gesture <- mean(b1_gesture)
med_b1_gesture <- median(b1_gesture)
sd_b1_gesture <- sd(na.omit(b1_gesture))

mean_a2_gesture <- mean(a2_gesture)
med_a2_gesture <- median(a2_gesture)
sd_a2_gesture <- sd(na.omit(a2_gesture))

mean_b2_gesture <- mean(b2_gesture)
med_b2_gesture <- median(b2_gesture)
sd_b2_gesture <- sd(na.omit(b2_gesture))


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

print(mean_a1_head)
print(med_a1_head)
print(sd_a1_head)

print(mean_b1_head)
print(med_b1_head)
print(sd_b1_head)

print(mean_a2_head)
print(med_a2_head)
print(sd_a2_head)

print(mean_b2_head)
print(med_b2_head)
print(sd_b2_head)

print(mean_a1_gesture)
print(med_a1_gesture)
print(sd_a1_gesture)

print(mean_b1_gesture)
print(med_b1_gesture)
print(sd_b1_gesture)

print(mean_a2_gesture)
print(med_a2_gesture)
print(sd_a2_gesture)

print(mean_b2_gesture)
print(med_b2_gesture)
print(sd_b2_gesture)


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

a_data <- data.frame(
  name = c("Head a(1d)", "Gesture a(1d)", "Head a(2d)", "Gesture a(2d)"),
  mean_value = c(mean_a1_head, mean_a1_gesture, mean_a1_head, mean_a2_gesture),
  sd_value = c(sd_a1_head, sd_a1_gesture, sd_a2_head, sd_a2_gesture)
)
ggplot(a_data) +
  geom_bar(aes(x = name, y = mean_value), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  labs(title = "Comparison of initial displacement in univariate and bivariate models", x = "Tracking Modality", y = "Initial displacement in ms") +
  theme_minimal() +
  scale_y_continuous(expand = c(0.5, 0.2))

b_data <- data.frame(
  name = c("Head b(1d)", "Gesture b(1d)", "Head b(2d)", "Gesture b(2d)"),
  mean_value = c(mean_b1_head, mean_b1_gesture, mean_b1_head, mean_b2_gesture),
  sd_value = c(sd_b1_head, sd_b1_gesture, sd_b2_head, sd_b2_gesture)
)
ggplot(b_data) +
  geom_bar(aes(x = name, y = mean_value), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = name, ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  labs(title = "Comparison of movement coefficient in univariate and bivariate models", x = "Tracking Modality", y = "Movement coefficient in ms/bit") +
  theme_minimal() +
  scale_y_continuous(expand = c(0.5, 0.2))

#check if data is normally distributed
print(shapiro.test(time_head)) #not normally distributed
print(shapiro.test(error_head)) #normally distributed
print(shapiro.test(time_gesture)) #normally distributed
print(shapiro.test(error_gesture)) #normally distributed
print(shapiro.test(a1_head)) #not normally distributed
print(shapiro.test(b1_head)) #not normally distributed
print(shapiro.test(a2_head)) #normally distributed
print(shapiro.test(b2_head)) #not normally distributed
print(shapiro.test(a1_gesture)) #normally distributed
print(shapiro.test(b1_gesture)) #not normally distributed
print(shapiro.test(a2_gesture)) #normally distributed
print(shapiro.test(b2_gesture)) #not normally distributed

#statistical tests
print(wilcox.test(time_head, time_gesture, paired = TRUE)) #z=0, p=7.629e-06 --> statistically significant difference
print(t.test(error_head, error_gesture, paired = TRUE)) #t(17)=0.14722, p=0.8847 --> not statistically significant difference
print(wilcox.test(a1_head, a1_gesture, paired = TRUE)) #z=19, p-value=0.002335 --> statistically significant difference
print(wilcox.test(b1_head, b1_gesture, paired = TRUE)) #z=67, p-value=0.4423 --> not statistically significant difference
print(t.test(a2_head, a2_gesture, paired = TRUE)) #t(17)=-4.6325, p-value=0.000238 --> statistically significant difference
print(wilcox.test(b2_head, b2_gesture, paired = TRUE)) #z=24, p-value=0.01383 --> statistically significant difference
