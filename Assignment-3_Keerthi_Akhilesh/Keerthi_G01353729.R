###--------------------------------------
#Student Name: Akhilesh Keerthi
#GNumber: G01353729
###--------------------------------------

library(dplyr)
library(tidyverse)
rm(list=ls())

data <- read.csv("C:\\Users\\akhilesh\\Desktop\\GitWork\\AIT580\\data\\EmployeeAttrition.csv")
# this is just for testing to use print statement
print(data[1,])

#a. Find the number of rows and columns in the dataset (5 points)
print("Number of rows are:")
print(nrow(data))
print("Number of columns are: ")
print(ncol(data))

# b. Find the maximum Age in the dataset (5 points)
max(data$Age)
print(paste0("The Maximum age in dataset: ", max(data$Age)))

# c. Find the minimum DailyRate in the dataset (5 points)
min(data$DailyRate)
print(paste0("The Minimum DailyRate in dataset: ", min(data$DailyRate)))

# d. Find the average/mean MontlyIncome in the dataset (5 points)
mean(data$MonthlyIncome)
print(paste0("The Average of MonthlyIncome in dataset: ", mean(data$MonthlyIncome)))

# e. How many employees rated WorkLifeBalance as 1 (5 points)
nrow(data[data$WorkLifeBalance==1,])
print(paste0("The number of employees with WorkLifeBalance as 1: ", nrow(data[data$WorkLifeBalance==1,])))

# f. What percent of total employees have TotalWorkingYears less than equal to 5? 
n = nrow(data)
tWY = sum(data$TotalWorkingYears<5)
print(paste0("The Percentage of total employees having TotalWorkingYears less than 5: ",round(tWY/n*100,3)))

tWY5 = sum(data$TotalWorkingYears>5)
print(paste0("Percentage of total employees have TotalWorkingYears > 5: ",round(tWY5/n*100,3)))

# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
data_1 = data %>%
  filter(data$Attrition == "Yes", RelationshipSatisfaction == 1, YearsSinceLastPromotion >3)
print.data.frame(select(data_1, EmployeeNumber, Department, MaritalStatus))

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)

dmale = data[data[,"Gender"]=="Male",]
dfemale = data[data[,"Gender"]=="Female",]

mmean = mean(dmale$EnvironmentSatisfaction)
fmean = mean(dfemale$EnvironmentSatisfaction)

mmedian = median(dmale$EnvironmentSatisfaction)
fmedian = median(dfemale$EnvironmentSatisfaction)

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mmode = mode(dmale$EnvironmentSatisfaction)
fmode = mode(dfemale$EnvironmentSatisfaction)

mstd = sd(dmale$EnvironmentSatisfaction)
fstd = sd(dfemale$EnvironmentSatisfaction)

mfred = with(dmale, table(EnvironmentSatisfaction))
ffred = with(dfemale, table(EnvironmentSatisfaction))

print(paste0("Summary of EnvironmentSatisfaction where Gender = Male:", " Mean= ", mmean,
             " Median= ", mmedian,
             " Mode= ", mmode,
             " Standard Deviation= ", mstd,
             " Frequency Distribution= ", mfred))

print(paste0("Summary of EnvironmentSatisfaction where Gender = Male:", " Mean= ", fmean,
             " Median= ", fmedian,
             " Mode= ", fmode,
             " Standard Deviation= ", fstd,
             " Frequency Distribution= ", ffred))


#### Part 2 ####

data_prt2 <- read.csv("C:\\Users\\akhilesh\\Desktop\\GitWork\\AIT580\\data\\Acme.csv")

#1. Identify data types for each attribute in the dataset
print(str(data_prt2))

#2. Produce a summary statistic for each attribute in the dataset
print(summary(data_prt2))


#3. Produce visualizations for each attribute
hist(data_prt2$Years, main = "Histogram of Years vs frequency",
     xlab="Years of Experience",
     ylab="Frequency")

hist(data_prt2$StSalary, main = "Histogram of Salary vs Frequency",
     xlab = "Salary", ylab = "Frequency") 
barplot(prop.table(table(data_prt2$Gender))) +
  title(main = "Bar plot of Gender vs frequency",
        xlab = "Gender",
        ylab = "Frequency")
barplot(prop.table(table(data_prt2$Degree))) +
  title(main="Bar plot of Degree vs frequency",
        xlab = "Degree",
        ylab = "Frequency")


#4a. Display the relationship between: Years of Experience and Starting Salary for all employees
ggplot(data_prt2, aes(x= Years, y = StSalary)) +
  geom_point()  +
  labs(title="Scatter-Plot between YOE and Salary of employees")


#4b. Display the relationship between: Years of Experience and Starting Salary for each gender
ggplot(data_prt2, aes(x= Years, y = StSalary, color = Gender)) +
  geom_point()  +
  labs(title="Scatter-Plot between YOE and Salary of each gender")


#4c. Display the relationship between: Years of Experience and Starting Salary for each degree
ggplot(data_prt2, aes(x= Years, y = StSalary, color = Degree)) +
  geom_point() +
  labs(title="Scatter Plot between YOE and Salary of each degree")

#5. Find the correlation between Starting Salary and Years of Experience?
print(cor(data_prt2$StSalary, data_prt2$Years))

#5a. Is the correlation different for each gender?
print(data_prt2 %>%
        group_by(Gender) %>%
        summarise(r = cor(StSalary, Years)))

#5b. Is the correlation different for each degree?
print(data_prt2 %>%
        group_by(Degree) %>%
        summarise(r = cor(StSalary, Years)))

#6. What can you conclude about Acme with respect to gender bias after the overall analysis?
print("Answer: The correlation coefficient of males and females is between 0.5 and 1, that is, a strong correlation.Acme does not determine starting salary based on gender")



