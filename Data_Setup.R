# Title: OASIS and ADNI Cohort Merge for Data Science Project
# Merging
# Author: Diana Hobbs
# Date: April 2022

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor,zoo)

###### Load data and Merge ###### 
source("OASIS_Setup.R")
source("ADNI_Setup.R")

# Organize the data 
OASIS_data <- OASIS_data%>%
  rename(days_since_mri=Label)%>%
  dplyr::select(RID,AGE,SEX,EDUC,RACE,BMI,APOE4,tothipp,percenthipp,ICV,thick,centiloid,AmyPositivity,CDR,MMSE,days_since_mri)

ADNI_data <- ADNI_data%>%
  mutate(days_since_mri=abs(as.numeric(difftime(MR_Date,CDR_Date,units = "days"))))%>%
  dplyr::select(RID,AGE,SEX,EDUC,RACE,BMI,APOE4,tothipp,percenthipp,ICV,thick,centiloid,AmyPositivity,CDR,MMSE,days_since_mri)

# Create a baseline dataset for each 
OASIS_Base_data <- OASIS_data%>%
  group_by(RID)%>%
  arrange(days_since_mri)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  filter(CDR==0 & MMSE>=27)

ADNI_Base_data <- ADNI_data%>%
  group_by(RID)%>%
  arrange(days_since_mri)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  filter(CDR==0 & MMSE>=27)
  
# Figure out which IDs in full dataset are in the full dataset 
OASIS_Base_IDs <- as.data.frame(table(OASIS_Base_data$RID))
ADNI_Base_IDs <- as.data.frame(table(ADNI_Base_data$RID))

OASIS_data_KEEP <- OASIS_data[(OASIS_data$RID %in% OASIS_Base_IDs$Var1),]
ADNI_data_KEEP <- ADNI_data[(ADNI_data$RID %in% ADNI_Base_IDs$Var1),]





data <- rbind(OASIS_data,ADNI_data)


















rm(demo,centiloid,mri)

data1<-data%>%
  group_by(Subject)%>%
  arrange(Label)%>%
  mutate(Visit = case_when(
    (row_number()==1) ~ 1,
    (row_number()==2) ~ 2,
    (row_number()==3) ~ 3,
    (row_number()==4) ~ 4,
    (row_number()==5) ~ 5,
    (row_number()==6) ~ 6,
    (row_number()==7) ~ 7))%>%
  ungroup()%>%
  filter(Visit == 1 & CDR == "Cognitively Unimpaired")

IDs <- as.data.frame(table(data1$Subject))
data3 <- data[(data$Subject %in% IDs$Var1),]
table(data3$CDR)

#########################
########################
########################


