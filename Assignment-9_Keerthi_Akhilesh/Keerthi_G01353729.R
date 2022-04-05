###------------------
###Hypothesis Testing
###------------------

###Students Name:Akhilesh Keerthi
###GNumber:G01353729
#setwd("~/git/AIT580/")

rm(list=ls())

data <- read.csv('C:\\Users\\akhilesh\\Desktop\\GitWork\\AIT580\\data\\EmployeeAttrition.csv')

print(data)

# Your hypothesis testings here...

Males <- which(data$Gender=='Male')
Females <- which(data$Gender=='Female')

Males
Females

#Q-1.If the Monthly-Income of Males is greater than Females (5 points)

t.test(data$MonthlyIncome[Males],data$MonthlyIncome[Females], alternative="greater", conf.level = 0.95)

#Solution: Null Hypothesis. 
#a.The analysis here shows, the p-value =0.8891 is greater than the significance Level of 0.05. 
#b.There is no 0 as mean value,95 percent confidence interval is from -718.2378 to Infinity. 
#c.As the true difference is greater than 0 we support null hypothesis and not alternative hypothesis.

#Q-2. If the WorkLifeBalance of Males is less than Females (5 points)

t.test(data$WorkLifeBalance[Males],data$WorkLifeBalance[Females], alternative="less", conf.level = 0.95) 

#Solution: Null Hypothesis. 
#a.The analysis shows, the p-value =0.4577 is greater than the significance level of 1 - confidence level (0.95) 
#b.The range of 95 percent confidence interval is from - Infinity to 0.0570346. 
#c.Hence, support the null hypothesis and reject the alternative hypothesis. 

#Q-3. If the YearsAtCompany of Single is less than Married (5 points)
Single_stat <- which(data$MaritalStatus=='Single')
Married_stat <- which(data$MaritalStatus=='Married')

t.test(data$YearsAtCompany[Single_stat],data$YearsAtCompany[Married_stat], alternative="less", conf.level = 0.95)

#Solution: Alternative Hypothesis. 
#1.The analysis shows that, the p-value =0.004973 is lesser than the significance level of 0.05. (significant level = 1 - confidence level (0.95)). 
#2. The range of 95 percent confidence interval is from - Infinity to -0.3382453. 
#3. From the test it's possible to support the alternative hypothesis

#Q-4. If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No (5 points)
Yes<- which(data$Attrition=='Yes')
No <- which(data$Attrition=='No')
t.test(data$EnvironmentSatisfaction[Yes],data$EnvironmentSatisfaction[No], alternative="less", conf.level = 0.95)

#Solution: Alternative Hypothesis. 
#a.The analysis shows that, the p-value =0.0001046 is less than significance level of 0.05. (significant level = 1 - confidence level (0.95)) 
#b.The range of 95 percent confidence interval is from Infinity to -0.172078 and does not contain 0 mean value. 
#c.Therefore, we support to the alternative hypothesis and reject the null hypothesis. 

#Q-5. If the MonthlyIncome of Manager is greater than Laboratory Technician (Hint: Use JobRole to find Manager and Laboratory Technician) (5 points)

Lab_Techi <- which(data$JobRole=='Laboratory Technician')
Manager <- which(data$JobRole=='Manager')
t.test(data$MonthlyIncome[Manager],data$MonthlyIncome[Lab_Techi], alternative="greater",var.equal=T, conf.level = 0.95)

#Solution: Alternative Hypothesis. 
#a.The analysis shows that, the p-value is less than 2.2e-16,which  is less  than the significance level of   0.05. (significant level = 1 - confidence level (0.95)) 
#b.The range of 95 percent confidence interval is from 13642.06 to Infinity.  
#c.Therefore, we support alternative hypothesis and reject the null hypothesis.

#Q-6. If YearsAtCompany and DailyRate are correlated with each other (5 points)

summary(lm( YearsAtCompany ~ DailyRate, data=data))
#Solution: The two variables are correlated too slightly 
#, these are not that correalted as the adjusted R square value is 0.00047. Also the p value is greater than 0.05 hence we fail to reject the null hypothesis.

#Q-7. If YearsAtCompany and MonthlyIncome are correlated with each other (5 points)

summary(lm( YearsAtCompany ~ MonthlyIncome, data=data))

#Solution:Two variables seems to be correlated with each
#other as the adjusted R square value is 0.264 i.e. approx 26%, the p value lesser than 0.05, hence we reject the null hypothesis.

#Q-8. If YearsAtCompany varies depending on individual's MaritalStatus (5 points)

summary(aov(YearsAtCompany ~ MaritalStatus, data=data))
#Solution:
#the p value is less than 0.05 and reject the null hypothesis and looking at the f value we can say the mean is not statistically same.

#Q-9. If MonthlyIncome varies depending on individual's PerformanceRating (5 points)

summary(aov(MonthlyIncome ~ PerformanceRating, data=data))
#Solution: The p value is greater than 0.05, hence we failed to reject the null hypothesis also variables have equal variances.

#Q-10. If MonthlyIncome varies depending on individual's WorkLifeBalance (5 points)
summary(aov(MonthlyIncome ~ WorkLifeBalance, data=data))
#Solution: The p value > 0.05, hence we failed to reject the null, and the varibles have equal variance.





