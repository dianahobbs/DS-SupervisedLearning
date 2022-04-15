# Title: OASIS Cohort Organization for Data Science Project
# Cleaning 
# Author: Diana Hobbs
# Date: April 2022

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor,zoo)

# Functions to set up environment
rm(list=ls()) #removes all variables from the current environment
options(scipen = 999) #forces R not to use scientific notation

###### Load data and select columns of interest ###### 

###### OASIS Cohort Organization ######
# demographics/clinical/genetic/cognitive
demo <- read.csv("./Data/OASIS/Demog.csv")%>%
  group_by(Subject)%>%
  fill(height, .direction="down")%>%
  fill(height, .direction="up")%>%
  # APOE: 0=non-carrier of e4 allele, 1=carrier of e4 allele
  mutate(APOE4 = str_replace_all(apoe,c("22"="0", "33"="0", "23"="0", "32"="0",
                                        "24"="1", "42"="1", 
                                        "34"="1", "43"="1", "44"="1")),
         RACE = str_replace_all(Race,c("Asian"="non-White","African American"="non-White","Caucasian"="White")),
         SEX = str_replace_all(M.F,c("M"="Male","F"="Female")),
         BMI = (weight/(height^2)*703))%>%
  mutate(cdr = case_when(cdr == 0 ~ "0",
                         cdr>0 ~ "1"))%>%
  rename(Dx=dx1, MMSE=mmse, CDR=cdr, AGE=Age, EDUC=Education)%>%
  dplyr::select(Label, Subject, AGE, SEX, EDUC, RACE, BMI, APOE4, MMSE, CDR)%>%
  na.omit()

## mri 
mri <- read.csv("./Data/OASIS/MRI.csv")%>%
  rename(ICV=IntraCranialVol)%>%
  mutate(tothipp = Right.Hippocampus_volume + Left.Hippocampus_volume,
         percenthipp = ((tothipp*100)/ICV),
         thick = ((((lh_entorhinal_thickness + rh_entorhinal_thickness)+
                      (lh_fusiform_thickness + rh_fusiform_thickness)+
                      (lh_inferiortemporal_thickness + rh_inferiortemporal_thickness)+
                      (lh_middletemporal_thickness + rh_middletemporal_thickness))/8)))%>%
  dplyr::select(Label,Subject,tothipp,percenthipp,ICV,thick)

## pet - Amyloid Positivity 21.9 cutoff 
centiloid <- read.csv("./Data/OASIS/Centiloid.csv")%>%
  rename(centiloid = Centil_fSUVR_TOT_CORTMEAN,
         Label = MRId)%>%
  mutate(AmyPositivity = case_when(centiloid >= 21.9 ~ "A+",
                                   centiloid < 21.9 ~ "A-"))%>%
  dplyr::select(-Centil_fSUVR_rsf_TOT_CORTMEAN)
  
image <- merge(mri,centiloid,by="Label")
data <- merge(demo,image,by=c("Subject","Label"))%>%
  rename(RID=Subject)
rm(centiloid,demo,image,mri)

data$Label <- substring(data$Label, nchar(as.character(data$Label)) - 3)
data$Label <- as.numeric(data$Label)
data$RID <- substring(data$RID, nchar(as.character(data$RID)) - 4)
data$RID <- as.numeric(data$RID)

OASIS_data <- data%>%
  mutate(yrs_since_mri = Label/365.5)%>%
  group_by(RID)%>%
  distinct(Label, .keep_all = T)%>% # Grouping by ID & keeping only distinct CDR visits for each participant
  ungroup()
rm(data)
