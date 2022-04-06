# Title: OASIS Cohort Organization for Data Science Project
# Cleaning and merging
# Author: Diana Hobbs
# Date: April 2022

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor)

###### Load data and select columns of interest ###### 

# demographics/clinical/genetic/cognitive
demo<-read.csv("./Data/ClinDataFull.csv")%>%
  group_by(Subject)%>%
  fill(height, .direction="down")%>%
  fill(height, .direction="up")%>%
  mutate(Apoe4 = str_replace_all(apoe,c("22"="0", "33"="0", "23"="0", "32"="0",
                                        "24"="1", "42"="1", 
                                        "34"="1", "43"="1", "44"="1")),
         Race = str_replace_all(Race,c("Asian"="non-White","African American"="non-White","Caucasian"="White")),
         dx1 = str_replace_all(dx1, c("0.5 in memory only"="Uncertain Impairment",
                                      "AD dem cannot be primary"="AD Dementia",
                                      "AD dem distrubed social- prior"="AD Dementia",
                                      "AD dem distrubed social- with"="AD Dementia",
                                      "AD dem Language dysf after"="AD Dementia",
                                      "AD dem Language dysf prior"="AD Dementia",
                                      "AD dem Language dysf with"="AD Dementia",
                                      "AD dem visuospatial- with"="AD Dementia",
                                      "AD dem w/CVD contribut"="AD Dementia",
                                      "AD dem w/CVD not contrib"="AD Dementia",
                                      "AD dem w/depresss  not contribut"="AD Dementia",
                                      "AD dem w/depresss- contribut"="AD Dementia",
                                      "AD dem w/depresss- not contribut"="AD Dementia",
                                      "AD dem w/depresss, not contribut"="AD Dementia",
                                      "AD dem w/oth \\(\\list B\\)\\ contribut"="AD Dementia",
                                      "AD dem w/oth \\(\\list B\\)\\ not contrib"="AD Dementia",
                                      "AD dem w/oth unusual feat/subs demt"="AD Dementia",
                                      "AD dem w/oth unusual features/demt on"="AD Dementia",
                                      "AD dem w/PDI after AD dem contribut"="AD Dementia",
                                      "AD dem w/PDI after AD dem not contrib"="AD Dementia",
                                      "AD dem/FLD prior to AD dem"="AD Dementia",
                                      "Cognitively normal"="Cognitively Normal",
                                      "DAT"="AD Dementia",
                                      "DAT w/depress not contribut"="AD Dementia",
                                      "Dementia/PD- primary"="nonAD Dementia",
                                      "DLBD, primary"="nonAD",
                                      "DLBD- primary"="nonAD",
                                      "Frontotemporal demt. prim"="nonAD Dementia",
                                      "Incipient demt PTP"="nonAD Dementia",
                                      "Incipient Non-AD dem"="nonAD Dementia",
                                      "No dementia"="Cognitively Normal",
                                      "Non AD dem- Other primary"="nonAD Dementia",
                                      "Unc: impair reversible"="Uncertain Impairment",
                                      "Unc: ques. Impairment"="Uncertain Impairment",
                                      "uncertain  possible NON AD dem"="Uncertain Dementia",
                                      "uncertain dementia"="Uncertain Dementia",
                                      "uncertain- possible NON AD dem"="Uncertain Dementia",
                                      "Vascular Demt- primary"="nonAD Dementia")),
         BMI = (weight/(height^2)*703))%>%
  filter(dx1 != "nonAD Dementia", dx1 != "nonAD")%>%
  replace_with_na(replace = list(dx1=c("",".")))%>%
  mutate(cdr = case_when(cdr == 0 ~ "Cognitively Unimpaired",
                         cdr>0 ~ "Cognitively Impaired"))%>%
  rename(Sex=M.F, Dx=dx1,MMSE=mmse,CDR=cdr)%>%
  dplyr::select(Label, Subject, Age, Sex, Education, Race, BMI, Dx, Apoe4, MMSE, CDR)

MR<-read.csv("./Data/MRI.csv")
Label, Subject, 















  