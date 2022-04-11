# Title: OASIS and ADNI Cohort Organization for Data Science Project
# Cleaning and merging
# Author: Diana Hobbs
# Date: April 2022

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor,zoo)

###### Load data and select columns of interest ###### 

###### OASIS Cohort Organization ######
# demographics/clinical/genetic/cognitive
demo_OASIS <- read.csv("./Data/ClinDataFull.csv")%>%
  group_by(Subject)%>%
  fill(height, .direction="down")%>%
  fill(height, .direction="up")%>%
  mutate(Apoe4 = str_replace_all(apoe,c("22"="0", "33"="0", "23"="0", "32"="0",
                                        "24"="1", "42"="1", 
                                        "34"="1", "43"="1", "44"="1")),
         Race = str_replace_all(Race,c("Asian"="non-White","African American"="non-White","Caucasian"="White")),
                  BMI = (weight/(height^2)*703))%>%
  filter(dx1 != "nonAD Dementia", dx1 != "nonAD")%>%
  replace_with_na(replace = list(dx1=c("",".")))%>%
  mutate(cdr = case_when(cdr == 0 ~ "0",
                         cdr>0 ~ "1"),
         Study = case_when(M.F == "M" | M.F == "F" ~ "OASIS"))%>%
  rename(Sex=M.F, Dx=dx1,MMSE=mmse,CDR=cdr)%>%
  dplyr::select(Label, Study,Subject, Age, Sex, Education, Race, BMI, Apoe4, MMSE, CDR)%>%
  na.omit()

## mri 
mri <- read.csv("./Data/MRI.csv")
mri_id <- mri%>%rename(ICV=IntraCranialVol)%>%dplyr::select(Label,Subject,ICV)

# extract volumes 
mri_vol <- mri[,grepl("_volume", names(mri))]
mri_vol <- cbind(mri_id, mri_vol)
mri_vol <- mri_vol%>%
  mutate(tothipp = Right.Hippocampus_volume + Left.Hippocampus_volume)%>%
  mutate(percenthipp = ((tothipp*100)/ICV))%>%
  dplyr::select(Label,Subject,tothipp,percenthipp,ICV)%>%
  na.omit()

mri_thick <-  mri[,grepl("_thickness", names(mri))]
mri_thick <- cbind(mri_id, mri_thick)
mri_thick <- mri_thick%>%
  mutate(thick = ((((lh_entorhinal_thickness + rh_entorhinal_thickness)+
                     (lh_fusiform_thickness + rh_fusiform_thickness)+
                     (lh_inferiortemporal_thickness + rh_inferiortemporal_thickness)+
                     (lh_middletemporal_thickness + rh_middletemporal_thickness))/8)))%>%
  dplyr::select(Label,Subject,thick)%>%
  na.omit()

mri <- merge(mri_vol,mri_thick,by=c("Label","Subject"))
rm(mri_id,mri_thick,mri_vol)

## pet 
centiloid <- read.csv("./Data/centiloid.csv")%>%
  rename(centiloid = Centil_fSUVR_rsf_TOT_CORTMEAN,
         Label = MRId)

data <- merge(demo,mri,by=c("Label","Subject"))
data <- merge(data,centiloid,by="Label")
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


###### ADNI Cohort Organization ######

# demographics 
demo_ADNI <- read.csv("./Data/ADNIMERGE_May15.2014.csv")%>%
  mutate(PTGENDER = str_replace_all(PTGENDER, c("Male"="M", "Female"="F")))%>%
  #PTRACCAT: 1=American Indian or Alaskan Native; 2=Asian; 3=Native Hawaiian or Other Pacific Islander;
  #          4=Black or African American; 5=White; 6=More than one race; 7=Unknown
  mutate(PTRACCAT = str_replace_all(PTRACCAT,c("Am Indian/Alaskan"="non-White","Asian"="non-White","Hawaiian/Other PI"="non-White",
                                               "Black"="non-White","5"="White")),
         Study = case_when(PTGENDER == "M" | PTGENDER == "F" ~ "ADNI"))%>%
  filter(PTRACCAT != "More than one")%>%
  rename(Age=AGE,Sex=PTGENDER,Education=PTEDUCAT,Race=PTRACCAT)%>%
  select(RID,EXAMDATE,Study,Age,Sex,Education,Race,Hippocampus,ICV,MMSE)%>%
  na.omit()%>%
  group_by(RID)%>%
  arrange(EXAMDATE)%>%
  filter(row_number()==1)%>%
  ungroup()

# apoe
apoe_ADNI <- read.csv("./Data/APOERES.csv")%>%
  mutate(apoe = paste(APGEN1, APGEN2, sep=""))%>%
  mutate(Apoe4 = str_replace_all(apoe, c("22"="0", "33"="0", "23"="0", "32"="0",
                                         "24"="1", "42"="1",
                                         "34"="1", "43"="1", "44"="1")))%>%
  dplyr::select(RID,Apoe4)

# Merge demo & apoe
data_ADNI <- merge(demo_ADNI, apoe_ADNI, by="RID", all=FALSE)
rm(demo_ADNI,apoe_ADNI)

# BMI - USERDATE has more data AND the dates are within a year of EXAMDATE !!*** USE USERDATE !!!!*****
##****first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
BMI_ADNI <- read.csv("./Data/VITALS.csv")%>%
  mutate(USERDATE = as.Date(USERDATE, "%m/%d/%Y"))%>%
  mutate(EXAMDATE = as.Date(EXAMDATE, "%m/%d/%Y"))%>%
  replace_with_na(replace=list(VSWEIGHT=c(-1),VSWTUNIT=c(-1),VSHEIGHT=c(-1,-4),VSHTUNIT=c(-1,-4),VSBPSYS=c(-1)))%>%
  mutate(BMI = case_when(VSHTUNIT == "1" & VSWTUNIT == "1" ~ (VSWEIGHT/(VSHEIGHT^2)*703),
                         VSHTUNIT == "1" & VSWTUNIT == "2" ~ ((VSWEIGHT * 2.20462)/(VSHEIGHT^2)*703),
                         VSHTUNIT == "2" & VSWTUNIT == "1" ~ (VSWEIGHT/((VSHEIGHT/2.54)^2)*703),
                         VSHTUNIT == "2" & VSWTUNIT == "2" ~ ((VSWEIGHT * 2.20462)/((VSHEIGHT/2.54)^2)*703)))%>%
  filter(VISCODE2 != 'f')%>%
  filter(VISCODE2 == 'sc')%>%
  dplyr::select(RID,VISCODE2,BMI)%>%
  na.omit()

data_ADNI <- merge(data_ADNI, BMI_ADNI, by="RID", all=FALSE)
  
# CDR - USERDATE has more data AND the dates are within a year of EXAMDATE !!*** USE USERDATE !!!!*****
##****first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
CDR_ADNI <- read.csv("./Data/CDR.csv")%>%
  mutate(USERDATE = as.Date(USERDATE, "%m/%d/%Y"))%>%
  replace_with_na(replace=list(CDGLOBAL=c(-1)))%>%
  mutate(CDR = case_when(CDGLOBAL == "0" ~ "0", CDGLOBAL>0 ~ "1"))%>%
  filter(VISCODE2 != 'f')%>%
  dplyr::select(RID,USERDATE,VISCODE2,CDR)

data_ADNI <- merge(data_ADNI, CDR_ADNI, by=c("RID","VISCODE2"))
rm(BMI_ADNI)
data_ADNI <- data_ADNI%>%filter(CDR==0)

CDR_Follow_ADNI <- CDR_ADNI%>%
  filter(VISCODE2 != "sc")%>%
  na.omit()


IDs <- as.data.frame(table(data_ADNI$RID))
data3 <- CDR_Follow_ADNI[(CDR_Follow_ADNI$RID %in% IDs$Var1),]
table(data3$CDR)


###### Cognitive Tests ######
# MMSE ***first open in excel and format USERDATE and EXAMDATE as 3/14/2012 and THEN save to csv
MMSE_ADNI <- read.csv("./Data/MMSE.csv")%>%
  mutate(USERDATE = as.Date(USERDATE, "%m/%d/%Y"))%>%
  mutate(EXAMDATE = as.Date(EXAMDATE, "%m/%d/%Y"))%>%
  mutate(DIFF = EXAMDATE - USERDATE)%>%
  filter(VISCODE2 != 'f')%>%
  replace_with_na(replace=list(MMSCORE=c(-1)))%>%
  #group_by(RID)%>%
  #arrange(USERDATE)%>%
  #filter(row_number()==1)%>%
  #ungroup()%>%
  rename(MMSE=MMSCORE)%>%
  select(RID,VISCODE2,MMSE)

data1_ADNI <- merge(data1_ADNI, MMSE_ADNI, by=c("RID","VISCODE2"), all=FALSE)%>%
  na.omit()
data_ADNI <- merge(data_ADNI, data1_ADNI, by="RID", all=FALSE)%>%
  na.omit()
rm(MMSE_ADNI)




