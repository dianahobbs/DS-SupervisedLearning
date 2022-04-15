# Title: ADNI Cohort Organization for Data Science Project
# Cleaning 
# Author: Diana Hobbs
# Date: April 2022

# Functions to set up environment
#rm(list=ls()) #removes all variables from the current environment
options(scipen = 999) #forces R not to use scientific notation

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor,zoo)

###### Load data and select columns of interest ###### 

###### ADNI Cohort Organization ######
# demographics 
demo <- read.csv("./Data/ADNI/PTDEMOG.csv")%>%
  replace_with_na(replace=list(PTGENDER=c(-4), PTEDUCAT=c(-1,-4), PTRACCAT=c(-4)))%>%
  mutate(PTDOBDD = 1,
         BIRTH = make_date(PTDOBYY,PTDOBMM,PTDOBDD))%>%
  #PTGENDER: 1=Male; 2=Female
  mutate(SEX = str_replace_all(PTGENDER, c("1"="Male", "2"="Female")))%>%
  #PTRACCAT: 1=American Indian or Alaskan Native; 2=Asian; 3=Native Hawaiian or Other Pacific Islander;
  #          4=Black or African American; 5=White; 6=More than one race; 7=Unknown - replace with NA
  mutate(PTRACCAT = str_replace_all(PTRACCAT,c("1"="non-White", "2"="non-White","3"="non-White","4"="non-White","5"="White","6"="non-White","7"="Unknown")))%>%
  rename(EDUC=PTEDUCAT, RACE=PTRACCAT)%>%
  group_by(RID)%>%
  filter(row_number()==1)%>%
  ungroup(RID)%>%
  dplyr::select(RID,BIRTH,SEX,EDUC,RACE)

# apoe
gene <- read.csv("./Data/ADNI/APOERES.csv")%>%
  mutate(APOE = paste(APGEN1, APGEN2, sep=""))%>%
  # APOE: 0=non-carrier of e4 allele, 1=carrier of e4 allele
  mutate(APOE4 = str_replace_all(APOE,c("22"="0", "33"="0", "23"="0", "32"="0",
                                        "24"="1", "42"="1", 
                                        "34"="1", "43"="1", "44"="1")))%>%
  dplyr::select(RID,APOE4)

# Merge Demo & Gene
data <- merge(demo, gene, by="RID", all=F)
rm(demo,gene)

# BMI - USERDATE has more data AND the dates are within a year of EXAMDATE !!*** USE USERDATE !!!!*****
##****first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
BMI <- read.csv("Data/ADNI/VITALS.csv")%>%
  mutate(BMI_Date = as.Date(USERDATE, "%m/%d/%Y"))%>%
  replace_with_na(replace=list(VSWEIGHT=c(-1),VSWTUNIT=c(-1),VSHEIGHT=c(-1,-4),VSHTUNIT=c(-1,-4),VSBPSYS=c(-1)))%>%
  group_by(RID)%>%
  fill(VSHEIGHT, .direction = "down")%>%
  fill(VSHEIGHT, .direction = "up")%>%
  fill(VSHTUNIT, .direction = "down")%>%
  fill(VSHTUNIT, .direction = "up")%>%
  ungroup(RID)%>%
  mutate(BMI = case_when(VSHTUNIT == "1" & VSWTUNIT == "1" ~ (VSWEIGHT/(VSHEIGHT^2)*703),
                         VSHTUNIT == "1" & VSWTUNIT == "2" ~ ((VSWEIGHT * 2.20462)/(VSHEIGHT^2)*703),
                         VSHTUNIT == "2" & VSWTUNIT == "1" ~ (VSWEIGHT/((VSHEIGHT/2.54)^2)*703),
                         VSHTUNIT == "2" & VSWTUNIT == "2" ~ ((VSWEIGHT * 2.20462)/((VSHEIGHT/2.54)^2)*703)))%>%
  filter(VISCODE2 != 'f')%>%
  dplyr::select(RID,VISCODE2,BMI_Date,BMI)

data<- merge(data, BMI, by="RID", all=FALSE)
rm(BMI)

# MRI ***first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
ADNI1GO2_MR <- read.csv("./Data/ADNI/UCSFFSX51_11_08_19.csv")%>%
  mutate(MR_Date = as.Date(EXAMDATE, "%m/%d/%Y"),
         tothipp =ST29SV + ST88SV,
         ICV = ST10CV,
         percenthipp = ((tothipp*100)/ICV),
         # thickness = average of entorhinal, fusiform, inferior temporal, and middle temporal 
         thick = ((ST24TA + ST83TA + ST26TA + ST85TA + ST32TA + ST91TA + ST40TA + ST99TA)/8))%>%
  filter(VISCODE2 != 'f', OVERALLQC == "Pass")%>%
  dplyr::select(RID,MR_Date,tothipp,percenthipp,ICV,thick)%>%
  na.omit()

ADNI3_MR <- read.csv("./Data/ADNI/UCSFFSX6_03_02_21.csv")%>%
  mutate(MR_Date = as.Date(EXAMDATE, "%m/%d/%Y"),
         tothipp =ST29SV + ST88SV,
         ICV = ST10CV,
         percenthipp = ((tothipp*100)/ICV),
         # thickness = average of entorhinal, fusiform, inferior temporal, and middle temporal 
         thick = ((ST24TA + ST83TA + ST26TA + ST85TA + ST32TA + ST91TA + ST40TA + ST99TA)/8))%>%
  filter(VISCODE2 != 'f', OVERALLQC == "Pass")%>%
  dplyr::select(RID,MR_Date,tothipp,percenthipp,ICV,thick)%>%
  na.omit()

mri <- rbind(ADNI1GO2_MR,ADNI3_MR)        
rm(ADNI1GO2_MR,ADNI3_MR)        
 
# av45 - Centiloid conversion = 163.6*av45_3060_SUVR-181.0 Amyloid Positivity 21.9 cutoff 
av45 <- read.csv("./Data/ADNI/UCBERKELEYAV45_01_14_21.csv")%>%
  mutate(amy_Date = as.Date(EXAMDATE, "%m/%d/%Y"),
         av45_fsuvr_tot_cortmean = ((CTX_LH_PRECUNEUS_SUVR + CTX_RH_PRECUNEUS_SUVR +
                                       CTX_LH_SUPERIORFRONTAL_SUVR + CTX_RH_SUPERIORFRONTAL_SUVR +
                                       CTX_LH_ROSTRALMIDDLEFRONTAL_SUVR + CTX_RH_ROSTRALMIDDLEFRONTAL_SUVR +
                                       CTX_LH_LATERALOCCIPITAL_SUVR + CTX_RH_LATERALOCCIPITAL_SUVR + 
                                       CTX_LH_MEDIALORBITOFRONTAL_SUVR + CTX_RH_MEDIALORBITOFRONTAL_SUVR +
                                       CTX_LH_SUPERIORTEMPORAL_SUVR + CTX_RH_SUPERIORTEMPORAL_SUVR + 
                                       CTX_LH_MIDDLETEMPORAL_SUVR + CTX_RH_MIDDLETEMPORAL_SUVR)/14),
         centiloid = 163.6 * av45_fsuvr_tot_cortmean - 181.0,
         AmyPositivity = case_when(centiloid >= 21.9 ~ "A+",
                                   centiloid < 21.9 ~ "A-"))%>%
  dplyr::select(RID,amy_Date,centiloid,AmyPositivity)

image <- merge(mri,av45, by="RID", all=T)%>%
  mutate(mriamy_DIFF = abs(as.numeric(difftime(MR_Date,amy_Date, unit="weeks"))/52.25))%>%
  filter(between(mriamy_DIFF, 0, 1))%>%
  dplyr::select(-c(mriamy_DIFF,amy_Date)) 
rm(mri,av45)

data <- merge(data,image,by="RID",keep.all=T)%>%
  mutate(mribmi_DIFF = abs(as.numeric(difftime(MR_Date,BMI_Date, unit="weeks"))/52.25),
         AGE = as.numeric(difftime(MR_Date, BIRTH, unit="weeks"))/52.25)%>%
  filter(between(mribmi_DIFF, 0, 1))%>%
  #group_by(RID)%>%
  #arrange(MR_Date)%>%
  #filter(row_number()==1)%>%
  #ungroup()%>%
  dplyr::select(-c(mribmi_DIFF,BMI_Date,BIRTH)) 
rm(image)

# CDR - USERDATE has more data AND the dates are within a year of EXAMDATE !!*** USE USERDATE !!!!*****
##****first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
CDR <- read.csv("./Data/ADNI/CDR.csv")%>%
  mutate(CDR_Date = as.Date(USERDATE, "%m/%d/%Y"))%>%
  replace_with_na(replace=list(CDGLOBAL=c(-1)))%>%
  mutate(CDR = case_when(CDGLOBAL == "0" ~ "0", CDGLOBAL>0 ~ "1"))%>%
  filter(VISCODE2 != 'f')%>%
  dplyr::select(RID,CDR_Date,VISCODE2,CDR)

# MMSE ***first open in excel and format USERDATE as 3/14/2012 and THEN save to csv
MMSE <- read.csv("./Data/ADNI/MMSE.csv")%>%
  mutate(MMSE_Date = as.Date(USERDATE, "%m/%d/%Y"))%>%
  filter(VISCODE2 != 'f')%>%
  replace_with_na(replace=list(MMSCORE=c(-1)))%>%
  rename(MMSE=MMSCORE)%>%
  select(RID,MMSE_Date,VISCODE2,MMSE)

cog <- merge(CDR,MMSE,by=c("RID","VISCODE2"))%>%
  mutate(cdrmmse_DIFF = abs(as.numeric(difftime(MMSE_Date,CDR_Date, unit="weeks"))/52.25))%>%
  filter(between(cdrmmse_DIFF, 0, 1))%>%
  dplyr::select(-c(cdrmmse_DIFF,MMSE_Date))
rm(CDR,MMSE)

ADNI_data <- merge(data,cog,by=c("RID","VISCODE2"),all=F)%>%
  mutate(days_since_mri=as.numeric(difftime(CDR_Date,MR_Date,units = "days")))%>%
  mutate(yrs_since_mri=as.numeric(difftime(CDR_Date,MR_Date, unit="weeks"))/52.25)%>%
  group_by(RID)%>% # Grouping by ID & keeping only distinct CDR visits for each participant
  distinct(CDR_Date, .keep_all = T)%>%
  ungroup()
rm(cog,data)