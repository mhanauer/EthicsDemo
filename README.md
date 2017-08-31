---
title: "Ethics Study Demographics"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
There are a total of 494 recorded resposnes.  After getting rid of missing data for the given demographics below, there were 241.

Just getting the data.  
```{r}
#setwd("~/Google Drive/PARCS/Projects/Ethics/Data")
#dat = as.data.frame(read.csv("EthicsStudy.csv", header = TRUE))
dat1 = cbind( dat[c("Finished")], dat[c("Q8")], dat[c("Q16")], dat[c("Q18")],dat[c("Q9")], dat[c("Q12")], dat[c("Q7")], dat[c("Q10")], dat[c("Q17")], dat[c("Q21")])
head(dat1)
colnames(dat1) = c("Finished", "Edu", "State", "WorkSetting", "YearsExper", "Gender", "Age", "Eth", "EthDilem", "Training")
dat1 = dat1[-c(1:2),]
head(dat1)
dat1 = as.data.frame(na.omit(dat1))
write.csv(dat1, "dat1.csv", row.names = FALSE)
dat1 = read.csv("dat1.csv", header =TRUE, na.strings = c(""))
dat1 = as.data.frame(na.omit(dat1))
#dat1 = as.data.frame(subset(dat1, Finished == "True", select = Finished:Courses))

dim(dat1)
```
Now I need to get counts for Edu categories.  Grab the counts and then get the percentages. 
If you precent the number of people in each category with a percentage for that category, then you don't need to worry about the missing data for that category, because the percentages will match the numbers that you displayed.

You don't want to just delete everyone who didn't respond, because some questions are likely obscure you do not know if didn't respond to just some, but not all demographics.  You want to do it one by one, since you do not all the combinations in which someone could have responded to the data.

To get the percentage of respondents for each category you need to drop missing values from each category separately.  If you drop them all at once you will drop people that may have answered some questions but not all of them.  Edu is ok because everyone answered that question.

EDU is good to go.  

Need to clean State. missouri and North Carolina are problems

32 total "states".  There are  3 that are not states including Puertro Rico, Other, and Indiana and Michigan.  So there are 29 states represented and 21 that are not.
```{r}
library(plyr)
EduCount =count(dat1, 'Edu'); EduCount
n = sum(EduCount$freq)
dat1$Edu
EduCount$Per = round(EduCount$freq/n, 3)
write.csv(EduCount, "EduCount.csv", row.names = FALSE)
sum(EduCount$Per) # Check that it equals 1

dat1 = as.data.frame(apply(dat1, 2, function(x){ifelse(x == "co", "Colorado", ifelse(x == "CO", "Colorado", ifelse(x == "CT", "Connecticut", ifelse(x == "in", "Indiana", ifelse(x == "IN", "Indiana", ifelse(x == "ky", "Kentucky", ifelse(x == "Ky", "Kentucky", ifelse(x == "KY", "Kentucky", ifelse(x == "lndiana", "Indiana", ifelse(x == "ma", "Massachusetts", ifelse(x == "Ma", "Massachusetts", ifelse(x == "maryland", "Maryland", ifelse(x =="Massachssets", "Massachusetts", ifelse(x == "Massachusetss", "Massachusetts", ifelse(x == "massachusetts", "Massachusetts", ifelse(x == "massachusetts", "Massachusetts", ifelse(x == "Massachusetts", "Massachusetts", ifelse(x == "missouri", "Missouri", ifelse(x == "MN" , "Minnesota", ifelse(x == "N.C", "North Carolina", ifelse(x == "NC", "North Carolina", ifelse(x == "new jersey", "New Jersey", ifelse(x == "New jersey", "New Jersey", ifelse(x == "nj", "New Jersey", ifelse(x == "Nj", "New Jersey", ifelse(x == "NJ", "New Jersey", ifelse(x == "north carolina", "North Carolina" , ifelse(x == "North Carolina ", "North Carolina", ifelse(x == " North Carolina", "North Carolina", x)))))))))))))))))))))))))))))}))
head(dat1)

dat1 = as.data.frame(apply(dat1, 2, function(x){ifelse(x == "North Carolina - technically my highest degree is SSP - but that was not listed", "North Carolina", ifelse(x == "NY", "New York", ifelse(x == "oh", "Ohio", ifelse(x == "OH", "Ohio", ifelse(x == "ohio", "Ohio", ifelse(x == "PA", "Pennsylvania", ifelse(x == "PhD", "Other",ifelse( x == "puerto rico", "Puerto Rico", ifelse(x == "Puerto rico", "Puerto Rico", ifelse(x == "Puerto Rico", "Puerto Rico", ifelse(x == "PUERTO RICO", "Puerto Rico", ifelse(x == "Punjab,India", "Other", ifelse(x == "SD", "South Dakota", ifelse(x== "Seeking employment", "Other", ifelse(x == "south dakota", "South Dakota", ifelse(x == "SOUTH DAKOTA", "South Dakota", ifelse(x == "W V", "West Virginia", ifelse(x == "WA", "Washington", ifelse(x == "west virginia", "West Virginia", ifelse(x =="wv", "West Virginia", ifelse(x == "WV", "West Virginia",ifelse(x == "MA", "Massachusetts", ifelse(x == "missouri", "Missouri", ifelse(x == "mass", "Massachusetts", ifelse(x == "Pa", "Pennsylvania", ifelse(x == "Rhode island", "Rhode Island", ifelse(x == "washington", "Washington", ifelse(x == "missouri", "Missouri", ifelse(x == "NewYork", "New York",x)))))))))))))))))))))))))))))}))
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
write.csv(StateCount, "StateCount.csv", row.names = FALSE)

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
write.csv(WorkSettingCount, "WorkSettingCount.csv", row.names = FALSE)
```
Years of Expereince
```{r}
YearsExperDat1 = dat1
YearsExperDat1 = as.data.frame(apply(YearsExperDat1, 2, function(x){ifelse(x == "1 (This is my first year.)", 1, ifelse(x == "<1", 0, ifelse(x == "0.", 0, ifelse(x == "1 full year, this is my second year. ", 2, ifelse(x == "1/12 (1st year empoyed as a School Psychologist)", 1.5, ifelse(x == "10 years", 10, ifelse(x == "12 years", 12, ifelse(x == "13 years", 13, ifelse(x == "13 yr.", 13, ifelse(x == "15 years", 15, ifelse(x == "16, not counting 2011-12", 16, ifelse(x == "17 years", 17, ifelse(x== "18 years", 18, ifelse(x == "19 years", 19, ifelse(x =="2-3 months", 0 , ifelse(x == "2.5 years",2.5, ifelse(x == "20 yrs.", 20, ifelse(x == "21 years", 21, ifelse(x== "22 years", 22, ifelse(x== "24 years", 24, ifelse(x == "25 years", 25, ifelse(x == "26 yearss", 26, ifelse(x == "27 years", 27, ifelse(x== "28 years", 28, ifelse(x == "3", 3, ifelse(x == "3 months", 0, ifelse(x == "31 years", 31, ifelse(x == "4 years", 4, ifelse(x == "4.5 yrs", 4.5, ifelse(x =="7 years as a psychologist.  This is my 5th year as a administrator." ,12, ifelse(x == "in second year of employment (past internship)", 2, ifelse(x == "Five", 5, ifelse(x == "Just starting my second year.", 2, ifelse(x == "Less than 1 yr.", 0, ifelse(x == "this is my first year", 0, ifelse(x == "three", 3, ifelse(x == "Three", 3, ifelse(x == "zero", 0 , x))))))))))))))))))))))))))))))))))))))})) 

write.csv(YearsExperDat1, "YearsExperDat1.csv", row.names = FALSE)
YearsExperDat1 = as.data.frame(read.csv("YearsExperDat1.csv", header= TRUE, na.strings = c("")))
YearsExperDat1 = as.data.frame(YearsExperDat1$YearsExper)
YearsExperDat1 = as.data.frame(na.omit(YearsExperDat1))
colnames(YearsExperDat1) = c("YearsExper")
YearsExperDat1 = apply(YearsExperDat1,2, function(x){ifelse(x <=10, "10 years or less", ifelse(x <= 11, "11 to 20 years", ifelse(x <= 21, "21 to 30 years", ifelse(x <= 40, "31 or more years", x))))})
YearsExperDat1 = as.data.frame(YearsExperDat1)
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
write.csv(GenderCount, "GenderCount.csv", row.names = FALSE)
GenderCount
```
Age 
```{r}
AgeDat1 = as.data.frame(dat1$Age)
colnames("Age")
AgeDat1 = apply(AgeDat1, 2, function(x){ifelse(x == "60+", 60, ifelse(x == "over 35", 35, ifelse(x == "over 50", 50, ifelse(x == "over 60", 60, ifelse(x =="60 yrs. old", 60, ifelse(x == "prefer not to answer", NA,ifelse(x == "NA", -9, x)))))))})
FactorAgeDat1 = as.factor(AgeDat1)
levels(FactorAgeDat1)
AgeDat1 = apply(AgeDat1, 2, function(x){ifelse(x == -9, "", x)})
FactorAgeDat1 = as.factor(AgeDat1)
levels(FactorAgeDat1)
write.csv(AgeDat1, "AgeDat1.csv", row.names = FALSE)
AgeDat1 = read.csv("AgeDat1.csv", header = TRUE, na.strings = c(""))
AgeDat1 = as.data.frame(AgeDat1)
AgeDat1[170,]
AgeDat1 = as.data.frame(AgeDat1[-c(170),])
AgeDat1 = apply(AgeDat1,2, function(x){ifelse(x <=30, "30 or younger", ifelse(x <= 40, "31 to 40 years old", ifelse(x <= 50, "41 to 50 years old", ifelse(x <= 80, "51 or older", x))))})
colnames(AgeDat1) = c("Age")
AgeCount =count(AgeDat1, 'Age'); head(AgeCount)
n = sum(AgeCount$freq)
AgeCount$Per = round(AgeCount$freq/n, 3)
write.csv(AgeCount, "AgeCount.csv", row.names = FALSE)
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
write.csv(EthDilemCount, "EthDilemCount.csv", row.names = FALSE)
EthDilemCount
```
Training
```{r}
TrainingDat1 = as.data.frame(dat1$Training)
colnames(TrainingDat1) = c("Training")
write.csv(TrainingDat1, "TrainingDat1.csv", row.names = FALSE)
TrainingDat1 = as.data.frame(read.csv("TrainingDat1.csv", header= TRUE, na.strings = c("")))
TrainingDat1 = as.data.frame(TrainingDat1$Training)
TrainingDat1 = as.data.frame(na.omit(TrainingDat1))
colnames(TrainingDat1) = c("Training")

TrainingCount =count(TrainingDat1, 'Training'); head(TrainingCount)


n = sum(TrainingCount$freq)
TrainingCount$Per = round(TrainingCount$freq/n, 3)
TrainingCount
write.csv(TrainingCount, "TrainingCount.csv", row.names = FALSE)
```




