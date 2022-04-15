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
  mutate(Study = case_when(SEX=="Male" | SEX=="Female" ~ "OASIS"))%>%
  group_by(RID)%>%
  distinct(AGE, .keep_all = T)%>%
  arrange(AGE)%>%
  mutate(Visit = case_when(
    (row_number()==1) ~ 1,
    (row_number()==2) ~ 2,
    (row_number()==3) ~ 3,
    (row_number()==4) ~ 4,
    (row_number()==5) ~ 5,
    (row_number()==6) ~ 6))%>%
  ungroup()%>%
  dplyr::select(RID,Study,AGE,SEX,EDUC,RACE,BMI,APOE4,tothipp,percenthipp,ICV,thick,centiloid,AmyPositivity,CDR,MMSE,days_since_mri,yrs_since_mri,Visit)

ADNI_data <- ADNI_data%>%
  mutate(Study = case_when(SEX=="Male" | SEX=="Female" ~ "ADNI"))%>%
  group_by(RID)%>%
  distinct(CDR_Date, .keep_all = T)%>%
  arrange(yrs_since_mri)%>%
  mutate(Visit = case_when(
    (row_number()==1) ~ 1,
    (row_number()==2) ~ 2,
    (row_number()==3) ~ 3,
    (row_number()==4) ~ 4,
    (row_number()==5) ~ 5,
    (row_number()==6) ~ 6,
    (row_number()==7) ~ 7,
    (row_number()==8) ~ 8,
    (row_number()==9) ~ 9,
    (row_number()==10) ~ 10,
    (row_number()==11) ~ 11))%>%
  ungroup()%>%
  dplyr::select(RID,Study,AGE,SEX,EDUC,RACE,BMI,APOE4,tothipp,percenthipp,ICV,thick,centiloid,AmyPositivity,CDR,MMSE,days_since_mri,yrs_since_mri,Visit)

data <- rbind(OASIS_data,ADNI_data)
rm(ADNI_data,OASIS_data)

# Figure out who has baseline CDR=0, MMSE>=27
data_Base <- data%>%
  filter(Visit==1 & CDR==0 & MMSE>=27)%>%
  filter(yrs_since_mri<=3)
data_Base_IDs <- as.data.frame(table(data_Base$RID))
# Only keep IDs that are included in the above list 
data_KEEP <- data[(data$RID %in% data_Base_IDs$Var1),]

data_CDR <- data_KEEP%>%
  dplyr::select(RID,CDR,MMSE,yrs_since_mri,Visit)
# Convert to wide format
data_CDR_Wide <- data_CDR%>%
  #dplyr::select(-c(45,46))%>%
  pivot_wider(id_cols = 1,
              names_from = Visit,
              names_prefix = "",
              values_from = 2:4)%>%
  drop_na(CDR_2)

# Merge wide CDR data with data_KEEP 
data_Wide <- merge(data_Base,data_CDR_Wide,by="RID")%>%dplyr::select(-c(15:19))
rm(data,data_Base,data_Base_IDs,data_CDR,data_CDR_Wide,data_KEEP)

###### Converters vs Non-converters######
# Determine who is a converter vs non-converter (Go back to the variables we used with MCI conversion setup)
data<-data_Wide
data$CDR_1[is.na(data$CDR_1)] <- "NA"
data$CDR_2[is.na(data$CDR_2)] <- "NA"
data$CDR_3[is.na(data$CDR_3)] <- "NA"
data$CDR_4[is.na(data$CDR_4)] <- "NA"
data$CDR_5[is.na(data$CDR_5)] <- "NA"
data$CDR_6[is.na(data$CDR_6)] <- "NA"
data$CDR_7[is.na(data$CDR_7)] <- "NA"
data$CDR_8[is.na(data$CDR_8)] <- "NA"
data$CDR_9[is.na(data$CDR_9)] <- "NA"
data$CDR_10[is.na(data$CDR_10)] <- "NA"
data$CDR_11[is.na(data$CDR_11)] <- "NA"

# Cognitive (CDR) Converter: 1: converter, 0: didn't convert
data<-data%>%
  mutate(Cog_Convert = case_when(
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==0 & CDR_11==0 ~ "0", # if CDR at all 11 time-points is 0, then non-converter 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==0 & CDR_11=="NA" ~ "0", # "" 10 time-points ""
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10=="NA" & CDR_11=="NA" ~ "0", # "" etc ""
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0", 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0", 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0", 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0", 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0",
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0",
    CDR_1==0 & CDR_2==0 & CDR_3=="NA" & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0",
    CDR_1==0 & CDR_2=="NA" & CDR_3=="NA" & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ "0",
    CDR_1>=0.5 | CDR_2>=0.5 | CDR_3>=0.5 | CDR_4>=0.5 | CDR_5>=0.5 | CDR_6>=0.5 | CDR_7>=0.5 | CDR_8>=0.5 | CDR_9>=0.5 | CDR_10>=0.5 | CDR_11>=0.5 ~ "1"))%>%# if CDR >= 0.5 at ANY time-point, then converter 
  # 'followuptime' variable: non-coverters = total years of follow-up after base; converters = time to conversion
  mutate(followuptime = case_when(
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==0 & CDR_11==0 ~ yrs_since_mri_11, # if all 11 visits have a CDR0, then the lag time will be between base date and 11th CDR date
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==0 & CDR_11=="NA" ~ yrs_since_mri_10, # if there are only 10 visits and all CDR0, then lag time will be between base date and 10th CDR date
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_9, # "" etc
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_8,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_7,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_6,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_5,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_4,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_3,
    CDR_1==0 & CDR_2==0 & CDR_3=="NA" & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_2,
    CDR_1==0 & CDR_2=="NA" & CDR_3=="NA" & CDR_4=="NA" & CDR_5=="NA" & CDR_6=="NA" & CDR_7=="NA" & CDR_8=="NA" & CDR_9=="NA" & CDR_10=="NA" & CDR_11=="NA" ~ yrs_since_mri_1,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==0 & CDR_11==1~ yrs_since_mri_11, # if all CDR visits are 0 except at visit 11, then lag time will be between base date and 11th CDR date
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==0 & CDR_10==1 ~ yrs_since_mri_10, # if all CDR visits are 0 prior to visit 10, then lag time will be between base date and 10th CDR date
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==0 & CDR_9==1 ~ yrs_since_mri_9, # "" etc
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==0 & CDR_8==1 ~ yrs_since_mri_8,
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==0 & CDR_7==1 ~ yrs_since_mri_7, 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==0 & CDR_6==1 ~ yrs_since_mri_6, 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==0 & CDR_5==1 ~ yrs_since_mri_5, 
    CDR_1==0 & CDR_2==0 & CDR_3==0 & CDR_4==1 ~ yrs_since_mri_4,
    CDR_1==0 & CDR_2==0 & CDR_3==1 ~ yrs_since_mri_3,
    CDR_1==0 & CDR_2==1 ~ yrs_since_mri_2))

data_for_python <- data%>%
  rename(ID=RID)%>%
  dplyr::select(-c(15:47))
write.csv(data_for_python,"./Data/data_for_python.csv",row.names = F)

