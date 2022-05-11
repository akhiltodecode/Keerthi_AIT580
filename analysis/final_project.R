# theme
hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  #  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

# original dataset
dataset = read.csv("/Users/saili/Documents/data/CollegeScorecard_Raw_Data_04262022/Most-Recent-Cohorts-Institution.csv")

dim(dataset) # o/p 6662 rows, 2989 cols
# analyzing the dataset with around 3000 variables is a tideous task
# We'll divide the dataset into 4 subsets


# column1 contain school metrics ###############################################
col1 = c("INSTNM", "CONTROL", "CITY", "STABBR", "LATITUDE", "LONGITUDE", "TUITIONFEE_IN", "TUITIONFEE_OUT", "COSTT4_A")
data1 = dataset[col1]
str(data1)
summary(data1)

# from the structure of the data1, we can notice that variables Latitude, longitude, TutionIn, TuitionOut, Costt4A are char but it should be numeric
# before that let's check the NULL values since all are chars
sum(data1 == "NULL") #10114
data1[data1=="NULL"] <- NA

temp1 = na.omit(data1)
sum(is.na(temp1))
str(temp1)
temp1$CONTROL = as.factor(temp1$CONTROL)
temp1$TUITIONFEE_IN = as.numeric(temp1$TUITIONFEE_IN)
temp1$TUITIONFEE_OUT = as.numeric(temp1$TUITIONFEE_OUT)
temp1$COSTT4_A = as.numeric(temp1$COSTT4_A)
str(temp1)
summary(temp1)

# School Metrics
# 1. How many Public, Private institutions are there in US? And which state has highest public, private schools?
table(temp1$CONTROL)
# 1 = Public, 2 = Private for non-profit, 3 = Private for-profit
school_type = c("Public(1)", "Private Non-Profit(2)", "Private For-Profit(3)")
library(tidyverse)
ggplot(as.data.frame(table(temp1$CONTROL)), aes(x=Var1, y= Freq)) + 
  geom_bar(stat="identity") +
  xlab("Type of institution") +
  ylab("Frequency") +
  scale_x_discrete(labels= school_type)
  hw
# which state has highest public, private schools?
temp1 %>%
  group_by(STABBR) %>%
  count(CONTROL)
var1 = count(temp1, CONTROL, STABBR)
print(var1[var1$CONTROL==1 & var1$n == max(var1[var1$CONTROL == 1,'n']),]) # California has highest number of public schools
print(var1[var1$CONTROL==2 & var1$n == max(var1[var1$CONTROL == 2,'n']),]) # NY has highest no.of private Non-profit schools
print(var1[var1$CONTROL==3 & var1$n == max(var1[var1$CONTROL == 3,'n']),]) # California has highest number of private For-profit schools


# 2. What is their average tuition fee for instate and outstate students?
mean(temp1$TUITIONFEE_IN) # Average In-state student tuition fee
mean(temp1$TUITIONFEE_OUT) # Average Out-state student tuition fee
# we can notice that avg out-state tuition fee is greater than in-state tuition fee


var2 = count(temp1, STABBR, TUITIONFEE_IN, TUITIONFEE_OUT)
var2_new = aggregate(var2[,2:3], list(var2$STABBR), mean)
var2_plot = var2_new[ , c(1,2)]
var2_plot1 = var2_new[ , c(1,3)]
names(var2_plot)[names(var2_plot) == "Group.1"] <- 'state'
names(var2_plot)[names(var2_plot) == "TUITIONFEE_IN"] <- 'values'
names(var2_plot1)[names(var2_plot1) == "Group.1"] <- 'state'
names(var2_plot1)[names(var2_plot1) == "TUITIONFEE_OUT"] <- 'values'
library(usmap)
library(ggplot2)
plot_usmap(data = var2_plot, values = "values", labels = TRUE) + 
  labs(title = "Average In-state Tuition fee in US States",
       subtitle = "The color code represents blue being low average and red being high average in-state tuition fee") + 
  theme(panel.background=element_blank()) +
  scale_fill_stepsn(colors=c("blue","green","yellow","red"),
                    guide = guide_colorsteps(even.steps = FALSE))
plot_usmap(data = var2_plot1, values = "values", labels = TRUE) + 
  labs(title = "Average Out-state Tuition fee in US States",
       subtitle = "The color code represents blue being low average and orange being high average in-state tuition fee") + 
  theme(panel.background=element_blank()) +
  scale_fill_stepsn(colors=c("blue","green","yellow","red"),
                    guide = guide_colorsteps(even.steps = FALSE))  


#Student Metrics:
#  1. What is the students retention rate for full time and part time students?
#  2. What is the students completion rate for 4 year and less than 4 year degree?
# column2 contain student metrics
col2 = c("RET_FT4_POOLED", "RET_PT4_POOLED", "C100_4_POOLED")
data2 = dataset[col2]
str(data2)
sum(data2 == "NULL") #14201
data2[data2=="NULL"] <- NA
temp2 = na.omit(data2)
sum(is.na(temp2))
str(temp2)
temp2$RET_FT4_POOLED = as.numeric(temp2$RET_FT4_POOLED)
temp2$RET_PT4_POOLED = as.numeric(temp2$RET_PT4_POOLED)
temp2$C100_4_POOLED = as.numeric(temp2$C100_4_POOLED)
summary(temp2)
hist(temp2$RET_FT4_POOLED, xlab="Student retention rate in FullTime 4 year institution", main="Histogram of Students Retention Rate in FT 4yr institution")
hist(temp2$RET_PT4_POOLED, xlab="Student retention rate in PartTime 4 year institution", main="Histogram of Students Retention Rate in PT 4yr institution")
hist(temp2$C100_4_POOLED, xlab="Student Completion rate in FullTime 4 year institution", main="Histogram of Students Completion Rate in FT 4yr institution")



#Admission Metrics:
#  1. Which institution has highest admission rate?
# column3 contain admission metrics
col3 = c("ADM_RATE","ACTCMMID", "SAT_AVG", "CONTROL" )
data3 = dataset[col3]
str(data3)
sum(data3 == "NULL") #15771
data3[data3=="NULL"] <- NA
temp3 = na.omit(data3)
sum(is.na(temp3))
str(temp3)
temp3[,1] = as.numeric(temp3[,1])
temp3[,2] = as.numeric(temp3[,2])
temp3[,3] = as.numeric(temp3[,3])
temp3[,4] = as.factor(temp3[,4])
str(temp3)
summary(temp3)

boxplot(temp3$ADM_RATE, horizontal = TRUE, xlab = "Admission Rate", main="Boxplot of Admission Rate")
# the median admission rate is around 0.75
hist(temp3$ADM_RATE, xlab = "Admission Rate", main = "Histrogram of Admission Rate")
# there are around 160 schools have admission rate 1

# 2. Does public or private institutions have high admission rate?##############
var3 = dataset[,c("ADM_RATE","CONTROL")]
sum(var3=="NULL")
var3[var3=="NULL"] <- NA
var3 = na.omit(var3)
sum(is.na(var3))
str(var3)
var3[,1] = as.numeric(var3[,1])
var3[,2] = as.factor(var3[,2])
aggregate(var3$ADM_RATE, list(var3$CONTROL), mean)
# here we can see that public institutions have highest average admission rate


# 3. What is the average ACT and SAT scores for public and private schools?#######
aggregate(temp3$SAT_AVG, list(temp3$CONTROL), mean)
# private non-profit institutions require high average SAT scores than other schools

aggregate(temp3$ACTCMMID, list(temp3$CONTROL), mean)
# however, all type of institutions require approximately same avg ACT cummulative scores


col3_extend = c("UGDS_MEN", "UGDS_WOMEN", "UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN", "UGDS_AIAN", "UGDS_NHPI", "PCIP01","PCIP03","PCIP04","PCIP05","PCIP09","PCIP10","PCIP11","PCIP12","PCIP13","PCIP14","PCIP15","PCIP16","PCIP19","PCIP22","PCIP23","PCIP24","PCIP25","PCIP26","PCIP27","PCIP29","PCIP30","PCIP31","PCIP38","PCIP39","PCIP40","PCIP41","PCIP42","PCIP43","PCIP44","PCIP45","PCIP46","PCIP47","PCIP48","PCIP49","PCIP50","PCIP51","PCIP52","PCIP54")





# Earning Metrics:
#  1. What are the top15 schools with highest mean salary after 6 & 10 yrs of entry?
# column4 contains earning metrics
col4 = c("PCTPELL", "PCTFLOAN", "DEBT_MDN_SUPP", "CDR3", "MD_EARN_WNE_P10", "MD_EARN_WNE_P6", "INSTNM")
data4 = dataset[col4]
str(data4)
sum(data4 == "NULL") #5796
data4[data4=="NULL"] <- NA
temp4 = na.omit(data4)
sum(is.na(temp4))
str(temp4)
temp4[,1] = as.numeric(temp4[,1])
temp4[,2] = as.numeric(temp4[,2])
temp4[,3] = as.numeric(temp4[,3])
temp4[,4] = as.numeric(temp4[,4])
temp4[,5] = as.numeric(temp4[,5])
temp4[,6] = as.numeric(temp4[,6])
str(temp4)
summary(temp4)
var4 = arrange(temp4, desc(MD_EARN_WNE_P10))
var4_new = arrange(temp4, desc(MD_EARN_WNE_P6))
head(var4)
ggplot(var4[1:20,], aes(y = reorder(INSTNM, -MD_EARN_WNE_P10), x = MD_EARN_WNE_P10)) + 
  geom_bar(stat = "identity", fill="orange") +
  geom_text(aes(label=MD_EARN_WNE_P10), position=position_dodge(width=1.9), hjust=2.15) +
  hw +
  theme(axis.text.x = element_blank()) +
  ylab("Top 20 Schools") +
  xlab("Mean Salaries of students after 10 years of entry with its admission rate")
ggplot(var4_new[1:20,], aes(y = reorder(INSTNM, -MD_EARN_WNE_P6), x = MD_EARN_WNE_P6)) + 
  geom_bar(stat = "identity", fill="orange") +
  geom_text(aes(label=MD_EARN_WNE_P6), position=position_dodge(width=1.9), hjust=2.15) +
  hw +
  theme(axis.text.x = element_blank()) +
  ylab("Top 20 Schools") +
  xlab("Mean Salaries of students after 6 years of entry with its admission rate")




# column5 contain analysis metrics
col5 = c("MD_EARN_WNE_P10", "COSTT4_A", "DEBT_MDN_SUPP", "ADM_RATE","ACTCMMID", "SAT_AVG", "TUITIONFEE_IN", "TUITIONFEE_OUT","PCTPELL", "PCTFLOAN")
col6 = c("MD_EARN_WNE_P6", "COSTT4_A", "DEBT_MDN_SUPP", "ADM_RATE","ACTCMMID", "SAT_AVG", "TUITIONFEE_IN", "TUITIONFEE_OUT","PCTPELL", "PCTFLOAN")
data5 = dataset[col5]
str(data5)
sum(data5 == "NULL") #28438
data5[data5=="NULL"] <- NA
temp5 = na.omit(data5)
sum(is.na(temp5))
str(temp5)
for (i in 1:10) {
  temp5[,i] = as.numeric(temp5[,i])
}
fit1 = lm(MD_EARN_WNE_P10~., temp5)
summary(fit1)
data6 = dataset[col6]
str(data6)
sum(data6 == "NULL") #28053
data6[data6=="NULL"] <- NA
temp6 = na.omit(data6)
sum(is.na(temp6))
str(temp6)
for (i in 1:10) {
  temp6[,i] = as.numeric(temp6[,i])
}
fit2 = lm(MD_EARN_WNE_P6~., temp6)
summary(fit2)
