# Title: ADNI Gene Expression for Data Science Project
# Author: Diana Hobbs
# Date: April 2022

###### Set Up ######
# Load packages
pacman::p_load(tidyverse,jtools,naniar,lubridate,janitor,zoo)

# https://www.genenames.org/data/genegroup/#!/group/358
# uploading genetic expression data for rho family GTPases and ras related GTP binding proteins 
gene <- read.csv("./Data/ADNI/ADNI_Gene_Expression_Profile.csv")%>%
  filter(Symbol=="Visit" | Symbol=="SubjectID" | Symbol=="260/280" | Symbol=="260/230" | Symbol=="RIN" | Symbol=="YearofCollection" |
    Symbol=="CDC42"| Symbol=="RAC1" | Symbol=="RAC2" | Symbol=="RAC3" | Symbol=="RHOA" | Symbol=="RHOB" | Symbol=="RHOBTB1" | Symbol=="RHOBTB2" | 
         Symbol=="RHOC" | Symbol=="RHOD" | Symbol=="RHOF" | Symbol=="RHOG" | Symbol=="RHOH" | Symbol=="RHOJ" | Symbol=="RHOQ" | Symbol=="RHOU" | Symbol=="RHOV" | 
         Symbol=="RND1" | Symbol=="RND2" | Symbol=="RND3" | Symbol=="RHOT1" | Symbol=="RHOT2" | Symbol=="RRAGA" | Symbol=="RRAGB" | Symbol=="RRAGC" | Symbol=="RRAGD")
write.csv(gene, "./Data/ADNI/SelectedRasRhoGenes.csv", row.names=F)        

gene1 <- gene%>%
  group_by(Symbol)%>%
  arrange(Phase)%>%
  filter(row_number()==1)%>%
  ungroup()
gene1 <- as.data.frame(t(gene1), header=F, row.names=F)
colnames(gene1) <- gene1[3,] 

gene1$SubjectID <- substring(gene1$SubjectID, nchar(as.character(gene1$SubjectID)) - 3)
gene1$SubjectID <- as.numeric(gene1$SubjectID)
gene1 <- gene1%>%
  select(SubjectID,YearofCollection,Visit,1:26)%>%
  group_by(SubjectID)%>%
  arrange(YearofCollection)

geneIDs <- as.data.frame(table(gene1$SubjectID))

pythonData <- read.csv("./Data/data_for_python.csv")
pythonIDs <- as.data.frame(table(pythonData$ID))
data_KEEP <- data[(data$RID %in% data_Base_IDs$Var1),]

testIDs <- geneIDs[(geneIDs$Var1 %in% pythonIDs$Var1),]                         

only n = 142 observations in both gene file and oasis/adni n = 460 file                           