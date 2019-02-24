############################################################################################
#                                 EMPLOYEE ATTRITION CASE STUDY                            #
############################################################################################

################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the employee data for the year 2015, business wants to find out the driving variables
# resulting into employee attrition 
# This information would help business take right actions to curb employee attrition 

## Aim:

# The aim is to automate the process of predicting 
# Predict whether an employee will resign or not using logistic regression model
# Whether an employee will resign or not will depend on data from various categories 
# like,

# 1. Demographic Information
# 2. Employee remuneration and learning opportunities 
# 3. Workplace and working conditions 
# 4. Manager's perception of the employee's work 

################################################################

### Data Understanding

#Install and Load the required packages
#install.packages("MASS")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("GGally")
#install.packages("ROCR")

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(tidyr)
library(scales)
library(dplyr)
library(lubridate)
library(tidyimpute)
library(GGally) #Use for ggpairs
library(ROCR) #For testing KS Statistic 


#############################################################################
#                               READING DATA                                #
#############################################################################

#Set correct working directory from RStudio Menu 

# Loading all the 5 files
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
employee_survey_data<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey_data<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time<- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

#############################################################################
#                           UNDERSTANDING DATA                              #
#############################################################################

str(general_data)    # 4410 obs of 24 variables including the target variable
str(employee_survey_data) # 4410 obs of 4 variables
str(manager_survey_data) # 4410 obs of 3 variables
str(in_time) # 4410 obs of 262 variables
str(out_time) # 4410 obs of 262 variables

#All the data frames has same number of rows however it is better to check for unique values for 
#employee Id first 

#EmployeeID is a common column but we see that column name is missing in the in and out time files
#Let us correct it first 

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Confirm if the EmployeeID is key
length(unique(general_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(in_time$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(out_time$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(manager_survey_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(employee_survey_data$EmployeeID))    # 4410, confirming EmployeeID is key 

# Verify if the same EmployeeID exits across different data frames 
setdiff(general_data$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets


################################################################################
#                           HANDLING TIME DATA                                 #
################################################################################

#We observe columns with all N/A values. Such columns are holidays if we observe 
#these dates in calendar and what it also implies that no employee worked on these
#days otherwise there would have been time punching data of employees on these dates

in_time <- drop_cols_all_na(in_time)  #Left with 250 columns
out_time <- drop_cols_all_na(out_time) #Left with 250 columns

#We remove all NA columns before further analysis and calculation of derived metrics 
#This helps removal of unwanted columns and also improves the runtime performnce of 
#the script 

#calculting time differnce for each employee
convert_to_datetime <- function(x){return(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"))}

#Converting all the date columns of in and out time of employee to POSIXct
#No need to apply function to the EmployeeID column
daily_in_time <- data.frame(lapply(in_time[2:250], convert_to_datetime))
daily_out_time <- data.frame(lapply(out_time[2:250], convert_to_datetime))

#Find out hours spent on each working day by every employee
time_spent <- in_time

for(i in 1:ncol(daily_in_time)) {
    time_spent[,i+1] <- as.numeric(round(difftime(daily_out_time[,i],daily_in_time[,i],units = "hours"),2))
  }

#If we get NA value, it would mean that employee was on PTO on that day. This is as good as 
#saying number of hours worked by employee on that day is 0 
time_spent[is.na(time_spent)] <- 0

################################################################################
#                       DERIVED METRICs FOR TIME DATA                          #
################################################################################

#Calculating the average time spent by each employee in office
#Remember that each column represents time spent on one day and hence we need to sum
#data in each column 
for (i in 1:nrow(time_spent)) {
  time_spent$timeinoffice[i] <- round((sum(time_spent[i,2:250]))/250,digits = 2)}

#We do not need daily hours spent in office for further analysis 
time_data<-time_spent[,c("EmployeeID","timeinoffice")]

#8 hours a day in office as standard working hours: This is because for all the 
#employees, standard hours column has value 8 hours 
time_data$overtime<-time_data$timeinoffice - 8

################################################################################
#                                 MERGE DATA                                   #
################################################################################

employee_data <- merge(general_data, employee_survey_data, by = "EmployeeID")
employee_data <- merge(employee_data,manager_survey_data,by = "EmployeeID")
employee_data <- merge(employee_data,time_data,by = "EmployeeID") #Total 31 variables now 


################################################################################
#                     DROP UNWANTED VARIABLES FROM DATAFRAME                   #
################################################################################

# Let's remove the extra variables from the analysis. 

# Checking No.of unique entries in all the columns
sort(sapply(employee_data,function(x) length(unique(x))))

#Variables below have same values in all the rows which is very evident 
#So we can remove these 3 columns
employee_data<-employee_data[,!colnames(employee_data)%in%c("EmployeeCount","Over18","StandardHours")]
str(employee_data)


###############################################################################
#                                 NA TREATMENT                                #
###############################################################################

sort(sapply(employee_data, function(x) sum(is.na(x))))

#NA values are found in WorkLifeBalance, EnvironmentSatisfaction, JobSatisfaction,
#NumCompaniesWorked,TotalWorkingYears

#Treating NA values

#We see that "satisfaction" variables are ordinal based on Data Dictrionary Definition 
#So we can not just insert any random value here 

#Let us introduce a new value "0" which may ordinally mean "Not Available" and hence will have
#score below Low. It is much better to massage Na this way rather than assuming any random
#value for the satisfaction

employee_data$WorkLifeBalance[is.na(employee_data$WorkLifeBalance)] <- as.integer(0)
employee_data$EnvironmentSatisfaction[is.na(employee_data$EnvironmentSatisfaction)] <- as.integer(0)
employee_data$JobSatisfaction[is.na(employee_data$JobSatisfaction)] <- as.integer(0)

#For NA in NumCompaniesWorked, we may consider using 1 as default value which is reasonable
#as out of 4410 entries, only 19 entries have NA values and it will be safe to assume that 
#the employee has worked in at least one company without making predictive ability of model
#adversely 
employee_data$NumCompaniesWorked[which(is.na(employee_data$NumCompaniesWorked))] <- 1

#For cases where total working years are unknown, we can assume that number of working years
#is at least equal to number of years worked in company
#There are only 9 such cases out of 4410 and hence we should not make a guess or try to replace
#these values with something like average number of years worked from data. Rather we should 
#use data available for the employement with the current firm as default
temp <- employee_data[is.na(employee_data$TotalWorkingYears),]
temp$TotalWorkingYears <- temp$YearsAtCompany
employee_data <- employee_data[!is.na(employee_data$TotalWorkingYears),]
employee_data <- rbind(employee_data,temp)

sort(sapply(employee_data, function(x) sum(is.na(x)))) #Validate that no NA are left

###############################################################################
#                              OUTLIERS DETECTION                             #
###############################################################################

box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

#Total working years
plot_grid(ggplot(employee_data, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(employee_data, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#outliers found

#Monthly Income
plot_grid(ggplot(employee_data, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000),
          ggplot(employee_data, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#outliers found

#Number of companies worked 
plot_grid(ggplot(employee_data, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 2),
          ggplot(employee_data, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#One outlier found

#Years at company 
plot_grid(ggplot(employee_data, aes(YearsAtCompany))+ geom_histogram(binwidth = 2),
          ggplot(employee_data, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#outliers found

#Years since last promotion
plot_grid(ggplot(employee_data, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 4),
          ggplot(employee_data, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#outliers found

#Years with current manager
plot_grid(ggplot(employee_data, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 4),
          ggplot(employee_data, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#outliers found

#Percent Salary Hike
plot_grid(ggplot(employee_data, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(employee_data, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#no outliers

#Distance from home 
plot_grid(ggplot(employee_data, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(employee_data, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#no outliers

#Training times in the last year
plot_grid(ggplot(employee_data, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 2),
          ggplot(employee_data, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#Outliers found 


#Time in office
plot_grid(ggplot(employee_data, aes(timeinoffice))+ geom_histogram(binwidth = 4),
          ggplot(employee_data, aes(x="",y=timeinoffice))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#Outliers found but we can assume that data was accurate as it came from punching machines


#Overtime 
plot_grid(ggplot(employee_data, aes(overtime))+ geom_histogram(binwidth = 0.5),
          ggplot(employee_data, aes(x="",y=overtime))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)#Outliers found but we can assume that data was accurate because come from machines


#For variables which are more categorical or were reported using survey does not require
#outlier detection as they are assumed to be accurate 


###############################################################################
#                              OUTLIERS TREATMENT                             #
###############################################################################

#----Checking which outliers should be treated 
numeric_data<-c("MonthlyIncome","PercentSalaryHike", "DistanceFromHome","NumCompaniesWorked", 
            "TotalWorkingYears", "YearsAtCompany","TrainingTimesLastYear", 
            "YearsSinceLastPromotion","YearsWithCurrManager","timeinoffice","overtime")

#Look at quantile distribution of these variables 
View(sapply(employee_data[,numeric_data],function(x) quantile(x,seq(0,1,.01),na.rm = T)))

#Monthly income shows sudden jump above ~85%. To assume a reasonable value of cap,
#We can put a cap at around 90% so that we do not remove the higher salary bracket
#completly 
employee_data$MonthlyIncome[which(employee_data$MonthlyIncome > 152020)] <- 152020

#totalworkingyears shows jump above 95% so we can cap at 28 years
employee_data$TotalWorkingYears[which(employee_data$TotalWorkingYears > 28)] <- 28

#yearsatcompnay shows jump at around 92% & has outliers so apply a cap 
employee_data$YearsAtCompany[which(employee_data$YearsAtCompany > 17)] <- 17

#years since last promotion has outliers above 92% , so apply cap 
employee_data$YearsSinceLastPromotion[which(employee_data$YearsSinceLastPromotion > 7)] <- 7

#years with current manager has outliers above 96% , so apply a cap 
employee_data$YearsWithCurrManager[which(employee_data$YearsWithCurrManager > 11)] <- 11


#For other numeric variables we do not see significant jump and hence no cap is applied to the 
#values 

#Similarly, for variables like timeinoffice and overtime calculated using data read from
#punching machine, data is assued to be correct and hence no cap is applied 

#Again check quantile distribution 
View(sapply(employee_data[,numeric_data],function(x) quantile(x,seq(0,1,.01),na.rm = T)))


###############################################################################
#                        BINS OF DATA WHERE APPLICABLE                        #
###############################################################################

#Dervied Metric for Age Group
#Reference for classification,
#http://mentalfloss.com/article/533632/new-guidelines-redefine-birth-years-millennials-gen-x-and-post-millennials
employee_data$Age <- ifelse(employee_data$Age < 22,"1. Gen Z",
                     ifelse(employee_data$Age < 38,"2. Millennials",
                     ifelse(employee_data$Age < 53,"3. Gen X",       
                     "4. Baby Boomers")))


employee_data$Education <- ifelse(employee_data$Education == 1, "1. Below College",
                           ifelse(employee_data$Education == 2, "2. College",
                           ifelse(employee_data$Education == 3, "3. Bachelor",   
                           ifelse(employee_data$Education == 4, "4. Master",        
                           "5. Doctor"))))


#For Survey Variables , use data disctionary as reference

#Environment Satisfaction
employee_data$EnvironmentSatisfaction <- ifelse(employee_data$EnvironmentSatisfaction == 0,"0. Not mentioned",
                                         ifelse(employee_data$EnvironmentSatisfaction == 1,"1. Low",
                                         ifelse(employee_data$EnvironmentSatisfaction == 2,"2. Medium",
                                         ifelse(employee_data$EnvironmentSatisfaction == 3,"3. High",
                                         "4. Very High"))))
# Job Satisfaction
employee_data$JobSatisfaction <- ifelse(employee_data$JobSatisfaction == 0,"0. Not mentioned",
                                 ifelse(employee_data$JobSatisfaction == 1,"1. Low",
                                 ifelse(employee_data$JobSatisfaction == 2,"2. Medium",
                                 ifelse(employee_data$JobSatisfaction == 3,"3. High",
                                 "4. Very High"))))

# Work Life Balance
employee_data$WorkLifeBalance <- ifelse(employee_data$WorkLifeBalance == 0,"1. Not mentioned",
                                 ifelse(employee_data$WorkLifeBalance == 1,"2. Bad",
                                 ifelse(employee_data$WorkLifeBalance == 2,"3. Good",
                                 ifelse(employee_data$WorkLifeBalance == 3,"4. Better",
                                 "5. Best"))))

#Job Involvement 
employee_data$JobInvolvement <- ifelse(employee_data$JobInvolvement == 1,"1. Low",
                                ifelse(employee_data$JobInvolvement == 2,"2. Medium",
                                ifelse(employee_data$JobInvolvement == 3,"3. High",
                                "4. Very High")))

# Performance Rating 
employee_data$PerformanceRating <- ifelse(employee_data$PerformanceRating == 1,"1. Low",
                                   ifelse(employee_data$PerformanceRating == 2,"2. Good",
                                   ifelse(employee_data$PerformanceRating == 3,"3. Excellent",
                                   "4. Outstanding")))


###############################################################################
#                        SCALING OF NUMERIC VARIABLESE                        #
###############################################################################

#11 numeric variables scaled
scaled_data<-employee_data[,colnames(employee_data)%in%c("MonthlyIncome","PercentSalaryHike",
                                                        "DistanceFromHome","timeinoffice","overtime","TotalWorkingYears","YearsAtCompany",
                                                        "YearsSinceLastPromotion","YearsWithCurrManager","TrainingTimesLastYear","NumCompaniesWorked")]

scaled_data <- sapply(scaled_data, function(x) scale(x)) #Returns a matrix 


###############################################################################
#                                    EDA                                      #
###############################################################################

# Converting target variable attrition from No/Yes character to factor with levels 0/1 
employee_data$Attrition<- ifelse(employee_data$Attrition =="Yes",1,0)

# Checking attrition rate
attrition_rate <- sum(employee_data$Attrition)/nrow(employee_data)
attrition_rate # 16.12% attrition rate

# Check Correlation between numeric variables
ggpairs(employee_data[, c("MonthlyIncome", "PercentSalaryHike", "YearsAtCompany")]) #Not correlated
ggpairs(employee_data[, c("MonthlyIncome", "overtime", "TotalWorkingYears")]) #Not correlated
ggpairs(employee_data[, c("NumCompaniesWorked", "TotalWorkingYears", "YearsAtCompany")]) #years at company and total working years are correlated
ggpairs(employee_data[, c("TrainingTimesLastYear", "YearsSinceLastPromotion", "YearsWithCurrManager")]) #yearsincelastpromotion and yearswithcurrmanager are correlated
ggpairs(employee_data[, c("MonthlyIncome", "DistanceFromHome", "timeinoffice")]) # Not correlated 
ggpairs(employee_data[, c("overtime", "DistanceFromHome", "timeinoffice")])

#100% correlation found between overtime and timeinoffice. So we can remove overtime from analysis
scaled_data<-scaled_data[,!colnames(scaled_data)%in%c("overtime")]

str(employee_data)

#List of categorical variables
#"Gender","MaritalStatus","StockOptionLevel","Education","EducationField",
#"Department","BusinessTravel","JobRole","JobLevel",
#"EnvironmentSatisfaction","WorkLifeBalance","JobSatisfaction",
#"JobInvolvement","PerformanceRating","Age"

##############################################################################
#    Barcharts for categorical features with stacked attrition information   #
##############################################################################

#If because of size constraint, the chart is not visible in the plots area, use Zoom
#option in the plot area to view the graph

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="left")

plot_grid(ggplot(employee_data, aes(x=factor(Gender),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(MaritalStatus),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(StockOptionLevel),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(Education),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
#Trend shows more attrition in males over females. However, this is not substantiated
#statistically as it might be also because of the more number of male employees in the data

#Higher attrition for StockOptionLevel 0,1 

temp_data <- employee_data[,c("Gender","Attrition", "EmployeeID")]
gender_attrition <- temp_data %>% group_by(Gender,Attrition) %>% summarise(count = n())
#On quick observation we see that number of male employees is higher in the data and hence 
#we see proportinate increase in male attrition. 

#Both male and female show ~15% attrition
ggplot(gender_attrition, aes(x = Gender, y = count, fill=factor(Attrition))) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_y_continuous(labels = percent_format()) +
  labs(x = "", y = "")


plot_grid(ggplot(employee_data, aes(x=factor(Department),fill=factor(Attrition)))+ geom_bar()+bar_theme1, 
          ggplot(employee_data, aes(x=factor(JobLevel),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
#We see more attrition in R&D department and in lower job levels

plot_grid(ggplot(employee_data, aes(x=factor(BusinessTravel),fill=factor(Attrition)))+ geom_bar()+bar_theme1, 
          ggplot(employee_data, aes(x=factor(JobRole),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "v") 
#Surprisingly those who travel rrely tend to attrite more 
#More attrition in Job roles Research Scientist and Sales Executives

plot_grid(ggplot(employee_data, aes(x=factor(EnvironmentSatisfaction),fill=factor(Attrition)))+ geom_bar()+bar_theme1, 
          ggplot(employee_data, aes(x=factor(WorkLifeBalance),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(JobSatisfaction),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(JobInvolvement),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")

plot_grid(ggplot(employee_data, aes(x=factor(PerformanceRating),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          ggplot(employee_data, aes(x=factor(EducationField),fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
#Life Science and Medical educated employee attrite more
#Employees with excellent rating attrite more 
#NOTE: In the dataset, no employee has low rating

plot_grid(ggplot(employee_data, aes(x=Age,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
#More attrition in Millenials 


plot_grid(ggplot(employee_data, aes(x=DistanceFromHome,fill=factor(Attrition)))+ geom_bar()+bar_theme1,
          align = "h")
#Surprisingly employees with lesser distance from home show more attrition but there is no clear trend

#######################################################################
#           Histogram and Boxplots for numeric variables              #
######################################################################

#List of numeric/continuous variables (10 variables)
#"MonthlyIncome","PercentSalaryHike","NumCompaniesWorked"
#"DistanceFromHome","timeinoffice","YearsWithCurrManager"
#"TotalWorkingYears","YearsAtCompany"
#"YearsSinceLastPromotion","TrainingTimesLastYear"

plot_grid(ggplot(employee_data, aes(x=factor(Attrition),y=MonthlyIncome))+ geom_boxplot(width=0.2)+ 
          coord_flip() +theme(legend.position="left"),
          ggplot(employee_data, aes(x=factor(Attrition),y=PercentSalaryHike))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=factor(Attrition),y=YearsSinceLastPromotion))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#No major shift in the median values of Monthly Income, %Salary Hike and Year Since Last promoted
#between groups of employees who attrited and not-attrited 

plot_grid(ggplot(employee_data, aes(x=factor(Attrition),y=DistanceFromHome))+ geom_boxplot(width=0.2)+ 
          coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=factor(Attrition),y=timeinoffice))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=factor(Attrition),y=YearsWithCurrManager))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Clearly evident that more time spent in office tend to leave the firm
#Employees who leave the firm have spent less years with the current manager 


plot_grid(ggplot(employee_data, aes(x=factor(Attrition),y=TotalWorkingYears))+ geom_boxplot(width=0.2)+ 
          coord_flip() +theme(legend.position="none"),
          ggplot(employee_data, aes(x=factor(Attrition),y=YearsAtCompany))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          ggplot(employee_data, aes(x=factor(Attrition),y=TrainingTimesLastYear))+ geom_boxplot(width=0.2)+
          coord_flip() + box_theme_y,
          align = "v",nrow = 1)
#Attrition is observed for employees with lesser expereince 
#Employees who have spent less years with the firm tend to leave 

#Take a look at trend based on number of companies in which employee has worked
ggplot(employee_data, aes(x=factor(Attrition),y=NumCompaniesWorked))+ geom_boxplot(width=0.2)+ 
  coord_flip() +theme(legend.position="none")


#Run plots for only the subset of data of employee attrition
employee_attrition<-subset(employee_data,employee_data$Attrition==1)

#Try finding out trend or factors which impact attrition 

plot_grid(ggplot(employee_attrition, aes(x=factor(Gender)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(MaritalStatus),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(StockOptionLevel),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(Education),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          align = "h")

#We see that there is more attrition in single male employees with bachelor or mastee degree
#stock option level 0 and 1 also show more attrition

plot_grid(ggplot(employee_attrition, aes(x=factor(Department),fill=factor(JobLevel)))+ geom_bar()+bar_theme1, 
          ggplot(employee_attrition, aes(x=factor(JobLevel),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          align = "h")
#More attrition in R&D Department and Lower Job Levels i.e. Levels (0,1) 

plot_grid(ggplot(employee_attrition, aes(x=factor(BusinessTravel),fill=factor(Gender)))+ geom_bar()+bar_theme1, 
          ggplot(employee_attrition, aes(x=factor(JobRole),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          align = "v") 
#Employee with rare travel and in with job roles Laboratory Technician, Research Scientist and Sales Executive show
#more attrition

plot_grid(ggplot(employee_attrition, aes(x=factor(EnvironmentSatisfaction),fill=factor(PerformanceRating)))+ geom_bar()+bar_theme1, 
          ggplot(employee_attrition, aes(x=factor(WorkLifeBalance),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(JobSatisfaction),fill=factor(PerformanceRating)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(JobInvolvement),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          align = "h")
#Surprise: Employees with high job involvement or better work life fit show more attrition
#Implying there are other factors beyond job involvement and work life fit which impact
#employees decision to quit the firm 

plot_grid(ggplot(employee_attrition, aes(x=factor(PerformanceRating),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          ggplot(employee_attrition, aes(x=factor(EducationField),fill=factor(Gender)))+ geom_bar()+bar_theme1,
          align = "h")
#More Attrition in Life Science & Medical Graduates 

plot_grid(ggplot(employee_attrition, aes(x=Age,fill=factor(Education)))+ geom_bar()+bar_theme1,
          align = "h")
#More attrition in employees with Bachelors or Masters degree and Millenials 


###########################################################################
#                      Data Preparation For Modelling                     #
###########################################################################

#Creating a dataframe of categorical features
factor_data<-employee_data[,colnames(employee_data)%in%c("StockOptionLevel","JobLevel","BusinessTravel",
                                                      "Department","Education","EducationField","Gender","JobRole","MaritalStatus",
                                                      "EnvironmentSatisfaction","WorkLifeBalance","JobSatisfaction",
                                                      "JobInvolvement","PerformanceRating","Age")]

#Converting categorical attributes to factor
factor_data<- data.frame(sapply(factor_data, function(x) factor(x)))
str(factor_data)

#Creating dummy variables for factor attributes
dummies<- data.frame(sapply(factor_data, 
                            function(x) data.frame(model.matrix(~x-1,data = factor_data))[,-1]))

#Combined scaled variables and dummy variables into final data frame to proceed 
employee_final<-cbind(scaled_data,dummies)
employee_final<-cbind(employee_data$Attrition,employee_final)
colnames(employee_final)[1] <- "Attrition"


###############################################################################
#                        SPLITTING DATA FOR TRAIN & TEST                      #
###############################################################################

set.seed(100)

indices = sample.split(employee_final$Attrition, SplitRatio = 0.7)

train = employee_final[indices,]

test = employee_final[!(indices),]


###############################################################################
#                                MODEL BUILDING                               #
###############################################################################

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)## AIC is 2141.5

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)
sort(vif(model_2))
##AIC 2103.2

#BusinessTravel.xTravel_Rarely,BusinessTravel.xTravel_Frequently,Age.x2..Millennials and
#Age.x3..Gen.X have VIF > 4 but they have very low p-value
#All variables with higher VIF are showing statistical significance 
#So remove Education.x3..Bachelor which has relatively higher VIF and p-value

model_3 <- glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x4..Master + 
                 Education.x5..Doctor + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good + WorkLifeBalance.x5..Best + 
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))
##AIC 2103.9 -> increased 

#We do not see any variable with high VIF and high P value 
#Let us try removing Education.x4..Master which is statistically insignificant in last two models 

model_4 <- glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales +  
                 Education.x5..Doctor + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good + WorkLifeBalance.x5..Best + 
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))
##AIC 2102.9 -> decreased  

#Based on what we observe, WorkLifeBalance.x5..Best has VIF of 1.095889 and it is statistically
#insignificant..So let us remove this variable

model_5 <- glm(formula = Attrition ~ DistanceFromHome + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales +  
                 Education.x5..Doctor + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))
##AIC 2103.9 -> Same as model 3

#Based on VIF 1.038126 and higher p-value for DistanceFromHome, remove this variable 
model_6 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales +  
                 Education.x5..Doctor + EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))
##AIC 2104.5 

#Based on details of model above, remove EducationField.xTechnical.Degree owing to it's 
#higher p-value
model_7 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x5..Doctor +  
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))
##AIC 2104.3 


#Based on details of model above, we do not see any variable with high p-value and high VIF
#Remove Education.x5..Doctor owning to a higher p-value
model_8 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xMarried + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_8)
sort(vif(model_8))
##AIC 2106 

#Based on the outcome of above model, we can remove MaritalStatus.xMarried owing to it's higher 
#VIF and p-value

model_9 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_9)
sort(vif(model_9))
##AIC 2108 

#Based on analysis of VIF and p-value, remove JobRole.xResearch.Scientist
model_10 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                 Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                 EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                 JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                 WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                 JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_10)
sort(vif(model_10))
##AIC 2111.4 

#Based on VIF and p-value, remove JobRole.xLaboratory.Technician 

model_11 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                  Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                  JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_11)
sort(vif(model_11))
##AIC 2113.8

#Based on a higher p-value, remove JobRole.xSales.Executive as there is not other candidate with
#a high VIF as well as well as a high p-value 

model_12 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                  Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good +  
                  JobInvolvement.x3..High, family = "binomial", data = train)
summary(model_12)
sort(vif(model_12))
##AIC 2115.1

#Based on a higher p-value, remove JobInvolvement.x3..High as there is not other candidate with
#a high VIF as well as well as a high p-value 

model_13 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                  Age.x3..Gen.X + Age.x4..Baby.Boomers + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good, 
                  family = "binomial", data = train)
summary(model_13)
sort(vif(model_13))
##AIC 2119.4

#Based on a higher p-value and VIF, remove Age.x4..Baby.Boomers
model_14 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good, 
                family = "binomial", data = train)
summary(model_14)
sort(vif(model_14))
##AIC 2124.7


#Based on a higher p-value and VIF, remove BusinessTravel.xTravel_Rarely
model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice + Age.x2..Millennials + 
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good, 
                family = "binomial", data = train)
summary(model_15)
sort(vif(model_15))
##AIC 2131.7

#Based on higher VIF and p-lvaue, remove Age.x2..Millennials
model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good, 
                family = "binomial", data = train)
summary(model_16)
sort(vif(model_16))
##AIC 2139.1

#Based on higher p-lvaue, remove JobLevel.x5
model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad + WorkLifeBalance.x3..Good, 
                family = "binomial", data = train)
summary(model_17)
sort(vif(model_17))
##AIC 2143

#Based on p-value and VIF, remove WorkLifeBalance.x3..Good
model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_18)
sort(vif(model_18))
##AIC 2148.2

#Based on p-value, remove JobRole.xResearch.Director
model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_19)
sort(vif(model_19))
##AIC 2154.4

#We observe that all values are statistically significant now 
#Based on p-value, remove TrainingTimesLastYear (comparatively higher p-value)
model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_20)
sort(vif(model_20))
##AIC 2166

#All variables are stastically significant and hence based on a higher VIF, remove 
#Department.xSales
model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_21)
sort(vif(model_21))
##AIC 2180.2

#Based on a high VIf and p-value, remove Department.xResearch...Development
model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  Age.x3..Gen.X + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_22)
sort(vif(model_22)) 
#AIC 2179.6

#All VIF are below 2 now and all variables are statistically significant 
#Age.x3..Gen.X has relatively higher p-value and hence can be removed
model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  YearsWithCurrManager + timeinoffice +  
                  BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_23)
sort(vif(model_23)) 
##AIC 2193.1

#Becase of higher VIF, remove YearsWithCurrManager
model_24 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  timeinoffice + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + EnvironmentSatisfaction.x4..Very.High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_24)
sort(vif(model_24)) 
##AIC 2221.8

#Becase of higher VIF, remove EnvironmentSatisfaction.x4..Very.High
model_25 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  timeinoffice + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x2..Medium + 
                  EnvironmentSatisfaction.x3..High + JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                  family = "binomial", data = train)
summary(model_25)
sort(vif(model_25)) 
##AIC 2268.9

##Because of a higher p-value, remove EnvironmentSatisfaction.x2..Medium
model_26 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  timeinoffice + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + EnvironmentSatisfaction.x3..High + 
                  JobSatisfaction.x1..Low + JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_26)
sort(vif(model_26)) 
##AIC 2269.1

#Because of a higher p-value, remove EnvironmentSatisfaction.x3..High
model_27 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  timeinoffice + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + JobSatisfaction.x1..Low + 
                  JobSatisfaction.x4..Very.High + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_27)
sort(vif(model_27))
##AIC 2274.8

#All variables are stastitically significant and VIF is < 1.5 
#There are two dummy variables related to Job Satisfacation 
#Based on combination of vif and p-value, remove JobSatisfaction.x4..Very.High
model_28 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + 
                  timeinoffice + BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle + JobSatisfaction.x1..Low + 
                  WorkLifeBalance.x2..Bad , 
                family = "binomial", data = train)
summary(model_28)
sort(vif(model_28))
##AIC 2296

#Now we observe that all variables are statistically significant and also VIF
#is less than 1.4. Number of variables have been reduced to 8 and hence we can stop here

final_model <- model_28


###############################################################################
#                                MODEL TESTING                                #
###############################################################################

#Predicted probabilities of Attrition for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)
test$prob <- test_pred
View(test)

#Using probability cutoff of 50%
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
table(test_actual_attrition,test_pred_attrition)
test_conf

#Sensitivity : 0.18310 & Specificity : 0.98288  

#Using probability cutoff of 40%
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
table(test_actual_attrition,test_pred_attrition)
test_conf

#Sensitivity : 0.28169 & Specificity : 0.95495  

#We see that with cutoff probability of 50% or 40%, we are not getting a good value for
#sensitivity. Let us try below algorithm to find a better cut-off probability

#Finding optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#Summary of test probability
summary(test_pred)
s = seq(.01,.80,length=100)
OUT = matrix(0,100,3)
for(i in 1:100){OUT[i,] = perform_fn(s[i])} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend("bottomleft",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff

#Cut off value choosen 0.154 for final model
test_cutoff_attrition <- factor(ifelse(test_pred >=0.154, "Yes", "No"))
conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc
sens
spec


###############################################################################
#                                 KS STATISTIC                                #
###############################################################################

test_cutoff_attrition <- ifelse(test_cutoff_attrition =="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition =="Yes",1,0)

#Testing model using data for KS Statistic 
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test) #0.4224

#max(ks_table_test) > 40 suggests that the model is decently good model

###############################################################################
#                             LIFT & GAIN CHART                               #
###############################################################################

#Plotting the lift chart
lift <- function(labels , predicted_prob,groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

att_decile = lift(test_actual_attrition, test_pred, groups = 10)
att_decile

Gain <- c(0,att_decile$Gain)
Deciles <- c(0,att_decile$bucket)
plot(y=Gain,x=Deciles,type ="l",lwd = 2,xlab="Buckets",ylab="Gain",main = "Gain_Chart")

Random_Gain <- seq(from=0,to=100,by=10)
lines(y=Random_Gain,x=Deciles,type ="l",lwd = 2, col="blue")

Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11){Perfect_Gain[i] <- 100*min(1,129*(i-1)/209)}
lines(y=Perfect_Gain,x=Deciles,type ="l",lwd = 2, col="red")
legend("bottomright",col=c("red","black","blue"),lwd =c(2,2,2,2),c("Perfect Model","Actual Model","Random Model"), cex = 0.7)


# Plotting the lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y=Lift,x=Deciles,type ="l",ylim=c(0,3.5),lwd = 2,xlab="Bucket",ylab="Lift",main = "Lift Chart",ylim<-c())
lines(y=Random_Lift,x=Deciles,type ="l",lwd = 2, col="red")

legend("topright",col=c("black","red"),lwd =c(2,2,2),c("Actual Model","Random Model"), cex = 0.7)

# Area under the curve (AUC)
auc <- performance(pred_object_test, measure = "auc")
auc <- unlist(auc@y.values)
auc # 0.7112

# AUC > 0.70 is a good sign of model's predictive power

## ROC curve
plot(performance_measures_test, col = "green", main = "ROC Curve for test data")
abline(0, 1, lty = 19, col = "blue")


#################################################################################
#                                  CONCLUSION                                   #
#################################################################################

#Based on our analysis, we find that below variables help predict probability of 
#attrition

#NumCompaniesWorked
#TotalWorkingYears 
#YearsSinceLastPromotion 
#timeinoffice 
#BusinessTravel.xTravel_Frequently 
#MaritalStatus.xSingle
#JobSatisfaction.x1..Low  
#WorkLifeBalance.x2..Bad 

#The model is tested using various techniques and has satisfactory metrics to indicate 
#good effectiveness and performance 

#Accuracy	      0.713
#Sensitivity	  0.709
#Specificity	  0.714
#KS Statistic   42.24%
#Gini           0.7112

#Model also shows goon lift and gain for lower deciles (between 1 and 4)
                

