###--------------------------------------
#Student Name: Akhilesh Keerthi
#GNumber: G01353729
###--------------------------------------

import pandas as pd

data = pd.read_csv(r'C:\Users\akhilesh\Desktop\GitWork\AIT580\data\EmployeeAttrition.csv')

print(data.head())

#Q1: Find the number of entries/rows and columns in the data.
print(len(data))

#Q2: What is the average Monthly Income?
print(data["MonthlyIncome"].mean())

#Q3: What is the highest amount of HourlyRate ?
print(data["HourlyRate"].max())

#Q4: What is the Department, JobRole, MaritalStatus and OverTime of EmployeeNumber 10?
print(data.loc[data["EmployeeNumber"]==10,["Department","JobRole","MaritalStatus","OverTime"]])

#Q5: What is the Employee ID of highest MonthlyIncome paid employee?
print(data[data.MonthlyIncome==data.MonthlyIncome.max()]["EmployeeNumber"])

#Q6: What is the average(mean) DailyRate group by Age for all Employees whose age is greater than 58. (hint: use groupby function)
print(data[data["Age"] > 58].groupby("Age")["DailyRate"].mean())

#Q7: How many unique EducationField are there?
print(data['EducationField'].unique())

#Q8: What are the top 5 most common JobRole?
print(data['JobRole'].value_counts().head(5))

#Q9: How many JobRoles represented by less than 100 employees?
print(sum(data['JobRole'].value_counts()<100))

#Q10: What is the correlation between Education and JobSatisfaction?
print(data['Education'].corr(data['JobSatisfaction']))
