rm(list = ls())
setwd("~/HCI_project/Questionnaire analysis")

questionnaire_data <- read.csv('HCI_questionnaire.csv', header=TRUE, sep=",")

#demographic data
ages <- questionnaire_data$Age
gender <- questionnaire_data$Gender

#relevant answers
head_frustration <- questionnaire_data$Head_controller_frustration
head_satisfaction <- questionnaire_data$Head_controller_satisfaction

gesture_frustration <- questionnaire_data$Gesture_controller_frustration
gesture_satisfaction <- questionnaire_data$Gesture_controller_satisfaction


#descriptive statistics (mean, median, standard deviation)
mean_age <- mean(ages)
mean_head_frustration <- mean(head_frustration)
median_head_frustration <- median(head_frustration)
sd_head_frustration <- sd(head_frustration)
mean_head_satisfaction <- mean(head_satisfaction)
median_head_satisfaction <- median(head_satisfaction)
sd_head_satisfaction <- sd(head_satisfaction)

mean_gesture_frustration <- mean(gesture_frustration)
median_gesture_frustration <- median(gesture_frustration)
sd_gesture_frustration <- sd(gesture_frustration)
mean_gesture_satisfaction <- mean(gesture_satisfaction)
median_gesture_satisfaction <- median(gesture_satisfaction)
sd_gesture_satisfaction <- sd(gesture_satisfaction)

print(mean_age)

print(mean_head_frustration)
print(median_head_frustration)
print(sd_head_frustration)

print(mean_head_satisfaction)
print(median_head_satisfaction)
print(sd_head_satisfaction)

print(mean_gesture_frustration)
print(median_gesture_frustration)
print(sd_gesture_frustration)

print(mean_gesture_satisfaction)
print(median_gesture_satisfaction)
print(sd_gesture_satisfaction)

#visualization of descriptive statistics
boxplot_satisfaction <- list(head = head_satisfaction, gesture = gesture_satisfaction)
boxplot(boxplot_satisfaction, col = c("yellow", "blue"), names = c("Head control", "Gesture control"),
        main = "Boxplots of levels of satisfaction", ylab = "")

boxplot_frustration <- list(head = head_frustration, gesture = gesture_frustration)
boxplot(boxplot_frustration, col = c("yellow", "blue"), names = c("Head control", "Gesture control"),
        main = "Boxplots of levels of frustration", ylab = "")


#check if data is normally distributed
print(shapiro.test(head_frustration)) #not normally distributed
print(shapiro.test(head_satisfaction)) #not normally distributed
print(shapiro.test(gesture_frustration)) #normally distributed
print(shapiro.test(gesture_satisfaction)) #normally distributed

#statistical tests
print(wilcox.test(head_frustration, gesture_frustration, paired = TRUE)) 
print(wilcox.test(head_satisfaction, gesture_satisfaction, paired = TRUE))