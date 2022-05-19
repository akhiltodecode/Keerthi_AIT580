###------------------
###Assignment 13 - Regression and Clustering in R
###------------------

### Name: Akhilesh Keerthi
### GNumber	    : G01353729

library(tidyverse)

employees_data <- read.csv('C:\\Users\\akhilesh\\Desktop\\GitWork\\AIT580\\data\\EmployeeAttrition.csv')
#class(employees_data)
#tbl_df(employees_data)

#a. Show the scatter plot with relationship curve between TotalWorkingYears and
#MonthlyIncome. Briefly explain your observation in the plot (Hint: Use scatter.smooth()
#function in R) (5 points) 
with(employees_data, scatter.smooth(TotalWorkingYears, MonthlyIncome))
#Desciption - The total number of hours worked increases, so does the monthly income  
# therefore, they seem to be positively correlated with each other.


#b. Show the scatter plot with relationship curve between Age and DistanceFromHome.
#Briefly explain your observation in the plot (Hint: Use scatter.smooth() function in R) (5points)
with(employees_data, scatter.smooth(Age, DistanceFromHome))
# Two variables do not move in the same direction 
# and it is difficult to conclude the increase/decrease value of the variables as they seem to be stable, so there is no obvious relationship between the variables. 
# The correlation value is 0.00168612


#c. Calculate	Correlation	for	(a)	and	(b) and	explain	the	values	to	support	your	answer	in	(a)	and	(b)
#1.Correlation	for	(a)
cor(x=data$TotalWorkingYears, y=data$MonthlyIncome)
#2.Correlation	for	(b)
cor(x=data$Age, y=data$DistanceFromHome)
#The correlation value of a- +0.7728932 b- -0.0016 respectively, 
#0.77value is closer to +1 which is highly corelated and -0.001 which is negative 

#d. Using Linear Regression, find details of the relationship 
#between TotalWorkingYears and MonthlyIncome. 
#Explain results in terms of p-value at 95% confidence interval and
#determine whether the relationship is significant or not (Hint: Use lm() to create linear
#regression model. Use print() to show coefficients. Use summary() to show more details)
#plot(MonthlyIncome~TotalWorkingYears, data = employees_data)

years.monthlyIncome = lm(MonthlyIncome~TotalWorkingYears, data=employees_data)
years.monthlyIncome

#abline(years.monthlyIncome, col = "blue")
#plot(years.monthlyIncome)

summary(years.monthlyIncome)

#ggplot(employees_data, aes(x = TotalWorkingYears, y= MonthlyIncome), na.rm= TRUE) +
#  geom_point() + geom +
#  labs(x = "TotalWorkingYears", y = "MonthlyIncome", title = "Scatterplot relationship of TotalWorkingYears and MonthlyIncome")


#2. Clustering
#a.Use K-means Clustering algorithm to find groups between HourlyRate and
#TotalWorkingYears. Use number of clusters as 3. Explain how each group is different
#from another in terms of employees representing those groups. (10 points)

employee_data_2 <- as.data.frame(cbind(data$HourlyRate,data$TotalWorkingYears))
K_meanscluster<-kmeans(employee_data_2,3)
K_meanscluster$cluster <- as.factor(K_meanscluster$cluster)
K_meanscluster
ggplot(employee_data,aes(HourlyRate,TotalWorkingYears, color = K_meanscluster$cluster )) + geom_point()

#there are 3 clusters where, the first cluster shows the first grouping, where the hourly rate 
#ranges between 1 to 60 working hours. 
#Cluster 2 from 60-80 and 3 ranges from 80-100.

#b. Use number of clusters as 5. What did you observe? Did you see any split of groups
#observed in (a)? Observe the splitting groups and explain in terms of employees
#representing those groups. (Hint: Use kmeans() for clustering algorithm. Install
#ggplot2 library in R and use ggplot() function to visualize the clustering results)
#(10 points)

K_meanscluster2<-kmeans(employee_data_2,5)
K_meanscluster3<-K_meanscluster2$cluster
ggplot(data,aes(HourlyRate,TotalWorkingYears, color = K_meanscluster3 )) + geom_point()

# 5 clusters here classifies all working years of employees into several groups of hourly wages 
# Within the same cluster in the class, the total number of years of work will be different 
# Clusters are as small as the similarities between classes. There are 5 clusters to which hourly rates apply 
# Grouping by 10-50, 50-70, 70-75, 7-100, and 10-30 employees.





