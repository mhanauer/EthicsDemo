---
title: "Ethics Study Demographics"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
There are a total of 494 recorded resposnes.

Just getting the data.  
```{r}
#setwd("~/Google Drive/PARCS/Projects/Ethics/Data")
#dat = as.data.frame(read.csv("EthicsStudy.csv", header = TRUE))
dat1 = cbind( dat[c("Finished")], dat[c("Q8")], dat[c("Q16")], dat[c("Q18")],dat[c("Q9")], dat[c("Q12")], dat[c("Q7")], dat[c("Q10")], dat[c("Q17")], dat[c("Q21")])
head(dat1)
colnames(dat1) = c("Finished", "Edu", "State", "WorkSetting", "YearsExper", "Gender", "Age", "Eth", "EthDilem", "Courses")
dat1 = dat1[-c(1:2),]
head(dat1)
#dat1 = as.data.frame(subset(dat1, Finished == "True", select = Finished:Courses))
dim(dat1)
```
Now I need to get counts for Edu categories.  Grab the counts and then get the percentages. 
If you precent the number of people in each category with a percentage for that category, then you don't need to worry about the missing data for that category, because the percentages will match the numbers that you displayed.

You don't want to just delete everyone who didn't respond, because some questions are likely obscure you do not know if didn't respond to just some, but not all demographics.  You want to do it one by one, since you do not all the combinations in which someone could have responded to the data.

To get the percentage of respondents for each category you need to drop missing values from each category separately.  If you drop them all at once you will drop people that may have answered some questions but not all of them.  Edu is ok because everyone answered that question.

EDU is good to go.  

Need to clean State. missouri and North Carolina are problems
```{r}
library(plyr)
EduCount =count(dat1, 'Edu'); EduCount
n = sum(EduCount$freq)
EduCount$Per = round(EduCount$freq/n, 3)
sum(EduCount$Per) # Check that it equals 1

dat1 = as.data.frame(apply(dat1, 2, function(x){ifelse(x == "co", "Colorado", ifelse(x == "CO", "Colorado", ifelse(x == "CT", "Connecticut", ifelse(x == "in", "Indiana", ifelse(x == "IN", "Indiana", ifelse(x == "ky", "Kentucky", ifelse(x == "Ky", "Kentucky", ifelse(x == "KY", "Kentucky", ifelse(x == "lndiana", "Indiana", ifelse(x == "ma", "Massachusetts", ifelse(x == "Ma", "Massachusetts", ifelse(x == "maryland", "Maryland", ifelse(x =="Massachssets", "Massachusetts", ifelse(x == "Massachusetss", "Massachusetts", ifelse(x == "massachusetts", "Massachusetts", ifelse(x == "massachusetts", "Massachusetts", ifelse(x == "Massachusetts", "Massachusetts", ifelse(x == "missouri", "Missouri", ifelse(x == "MN" , "Minnesota", ifelse(x == "N.C", "North Carolina", ifelse(x == "NC", "North Carolina", ifelse(x == "new jersey", "New Jersey", ifelse(x == "New jersey", "New Jersey", ifelse(x == "nj", "New Jersey", ifelse(x == "Nj", "New Jersey", ifelse(x == "NJ", "New Jersey", ifelse(x == "north carolina", "North Carolina", x)))))))))))))))))))))))))))}))
head(dat1)

dat1 = as.data.frame(apply(dat1, 2, function(x){ifelse(x == "North Carolina - technically my highest degree is SSP - but that was not listed", "North Carolina", ifelse(x == "NY", "New York", ifelse(x == "oh", "Ohio", ifelse(x == "OH", "Ohio", ifelse(x == "ohio", "Ohio", ifelse(x == "PA", "Pennsylvania", ifelse(x == "PhD", "Other",ifelse( x == "puerto rico", "Other", ifelse(x == "Puerto rico", "Other", ifelse(x == "Puerto Rico", "Other", ifelse(x == "PUERTO RICO", "Other", ifelse(x == "Punjab,India", "Other", ifelse(x == "SD", "South Dakota", ifelse(x== "Seeking employment", "Other", ifelse(x == "south dakota", "South Dakota", ifelse(x == "SOUTH DAKOTA", "South Dakota", ifelse(x == "W V", "West Virginia", ifelse(x == "WA", "Washington", ifelse(x == "west virginia", "West Virginia", ifelse(x =="wv", "West Virginia", ifelse(x == "WV", "West Virginia",ifelse(x == "MA", "Massachusetts", ifelse(x == "missouri", "Missouri", ifelse(x == "mass", "Massachusetts", ifelse(x == "Pa", "Pennsylvania", ifelse(x == "Rhode island", "Rhode Island", ifelse(x == "washington", "Washington", ifelse(x == "missouri", "Missouri", ifelse(x == "NewYork", "New York",x)))))))))))))))))))))))))))))}))
head(dat1)
StateDat1 = dat1
write.csv(dat1, "StateDat1.csv", row.names = FALSE)
StateDat1 = as.data.frame(read.csv("StateDat1.csv", header= TRUE, na.strings = c("")))
StateDat1 = as.data.frame(StateDat1$State)
StateDat1 = as.data.frame(na.omit(StateDat1))

colnames(StateDat1) = c("State")

StateCount =count(StateDat1, 'State'); StateCount

n = sum(StateCount$freq)

StateCount$Per = round(StateCount$freq/n,3)
StateCount

sum(StateCount$Per) # Check that it equals 1

```
Work setting here
```{r}

WorkSettingDat1 = dat1
write.csv(dat1, "WorkSettingDat1.csv", row.names = FALSE)
WorkSettingDat1 = as.data.frame(read.csv("WorkSettingDat1.csv", header= TRUE, na.strings = c("")))
WorkSettingDat1 = as.data.frame(WorkSettingDat1$WorkSetting)
WorkSettingDat1 = as.data.frame(na.omit(WorkSettingDat1))

colnames(WorkSettingDat1) = c("WorkSetting")


WorkSettingCount =count(WorkSettingDat1, 'WorkSetting'); head(WorkSettingCount)
n = sum(WorkSettingCount$freq)
WorkSettingCount$Per = round(WorkSettingCount$freq/n, 3)
WorkSettingCount
```
Years of Expereince
```{r}
YearsExperDat1 = dat1
YearsExperDat1 = as.data.frame(apply(YearsExperDat1, 2, function(x){ifelse(x == "1 (This is my first year.)", NA, x)})) 
write.csv(dat1, "YearsExperDat1.csv", row.names = FALSE)
YearsExperDat1 = as.data.frame(read.csv("YearsExperDat1.csv", header= TRUE, na.strings = c("")))
YearsExperDat1 = as.data.frame(YearsExperDat1$YearsExper)
YearsExperDat1 = as.data.frame(na.omit(YearsExperDat1))

colnames(YearsExperDat1) = c("YearsExper")

YearsExperCount =count(YearsExperDat1, 'YearsExper'); head(YearsExperCount)


n = sum(YearsExperCount$freq)
YearsExperCount$Per = round(YearsExperCount$freq/n, 3)
YearsExperCount
write.csv(YearsExperCount, "YearsExperCount.csv", row.names = FALSE)
```
Gender
```{r}
GenderDat1 = dat1
write.csv(dat1, "GenderDat1.csv", row.names = FALSE)
GenderDat1 = as.data.frame(read.csv("GenderDat1.csv", header= TRUE, na.strings = c("")))
GenderDat1 = as.data.frame(GenderDat1$Gender)
GenderDat1 = as.data.frame(na.omit(GenderDat1))

colnames(GenderDat1) = c("Gender")

GenderCount =count(GenderDat1, 'Gender'); head(GenderCount)


n = sum(GenderCount$freq)
GenderCount$Per = round(GenderCount$freq/n, 3)
GenderCount
```
Age need to clean up (delete)
```{r}
AgeDat1 = dat1
write.csv(dat1, "AgeDat1.csv", row.names = FALSE)
AgeDat1 = as.data.frame(read.csv("AgeDat1.csv", header= TRUE, na.strings = c("")))
AgeDat1 = as.data.frame(AgeDat1$Age)
AgeDat1 = as.data.frame(na.omit(AgeDat1))

colnames(AgeDat1) = c("Age")

AgeCount =count(AgeDat1, 'Age'); head(AgeCount)


n = sum(AgeCount$freq)
AgeCount$Per = round(AgeCount$freq/n, 3)
AgeCount

```
Ethnicity
```{r}
EthDat1 = dat1
write.csv(dat1, "EthDat1.csv", row.names = FALSE)
EthDat1 = as.data.frame(read.csv("EthDat1.csv", header= TRUE, na.strings = c("")))
EthDat1 = as.data.frame(EthDat1$Eth)
EthDat1 = as.data.frame(na.omit(EthDat1))

colnames(EthDat1) = c("Eth")

EthCount =count(EthDat1, 'Eth'); head(EthCount)


n = sum(EthCount$freq)
EthCount$Per = round(EthCount$freq/n, 3)
EthCount

write.csv(EthCount, "EthCount.csv", row.names = FALSE)
```
Ethical Dilemia Question
```{r}
EthDilemDat1 = dat1
write.csv(dat1, "EthDilemDat1.csv", row.names = FALSE)
EthDilemDat1 = as.data.frame(read.csv("EthDilemDat1.csv", header= TRUE, na.strings = c("")))
EthDilemDat1 = as.data.frame(EthDilemDat1$EthDilem)
EthDilemDat1 = as.data.frame(na.omit(EthDilemDat1))

colnames(EthDilemDat1) = c("EthDilem")

EthDilemCount =count(EthDilemDat1, 'EthDilem'); head(EthDilemCount)


n = sum(EthDilemCount$freq)
EthDilemCount$Per = round(EthDilemCount$freq/n, 3)
EthDilemCount
```
Training
```{r}
CoursesDat1 = dat1
write.csv(dat1, "CoursesDat1.csv", row.names = FALSE)
CoursesDat1 = as.data.frame(read.csv("CoursesDat1.csv", header= TRUE, na.strings = c("")))
CoursesDat1 = as.data.frame(CoursesDat1$Courses)
CoursesDat1 = as.data.frame(na.omit(CoursesDat1))

colnames(CoursesDat1) = c("Courses")

CoursesCount =count(CoursesDat1, 'Courses'); head(CoursesCount)


n = sum(CoursesCount$freq)
CoursesCount$Per = round(CoursesCount$freq/n, 3)
CoursesCount
write.csv(CoursesCount, "CoursesCount.csv", row.names = FALSE)
```




