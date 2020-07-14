#upload hrv files; HRV files are csv files created from inital cleaning in vivosense
# Author: Cara

library(plyr)
setwd("/Applications/Rdata")

HRV_prog2 <- list.files("HRV/HRVfiles_phase2", recursive = T, pattern = "^(BIKE)", full.names = T)
length(HRV_prog2)


hrv.import <- function(HRV){
  hrvhex <- read.csv(HRV, stringsAsFactors = F, header = T,)  [,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)] 
  names(hrvhex) <- c('subjectID', 'sessionID', 'stage', 'Annotation', 'Timestamp', 'Duration', 'ANN', 'SDNN', 'RMSSD', 'SDSD', 'SDNNi', 'SDANN', 'NN50', 'pNN50', 'VLF', 'LF',
                     'HF', 'LF(NORM)', 'HF(NORM)', 'LF/HF', 'RSA', 'SD1', 'SD2', 'SamplEn', 'Correlation Dimension')
  
  hrvhex}

names(HRV_prog2) <- HRV_prog2

HRVprog2 <- ldply(HRV_prog2, hrv.import, .progress = "text")
View(HRVprog2)

# change time format 
HRVprog2$time <- as.POSIXct(HRVprog2$Timestamp, format = "%H:%M:%S" , tz = "America/New_York")
# set staged in order 
HRVprog2$stage <- factor(HRVprog2$stage, levels =c("baseline","immed","post1","post2", "post3", "post4", "eve_base", "eve_immed", "eve_post1", "eve_post2", "eve_post3", "eve_post4" ))


summary(HRVprog2$stage=="baseline")
summary(HRVprog2$stage=="immed")
summary(HRVprog2$stage=="post1")
summary(HRVprog2$stage=="post2")
summary(HRVprog2$stage=="post3")
summary(HRVprog2$stage=="post4")

summary(HRVprog2$stage=="eve_base")
summary(HRVprog2$stage=="eve_immed")
summary(HRVprog2$stage=="eve_post1")
summary(HRVprog2$stage=="eve_post2")


library(dplyr)

# Wide data file
#basline 
HRV_base2 <- filter(HRVprog2, stage == "baseline")
names(HRV_base2)[names(HRV_base2)=="Annotation"] <- "Annotation_base"
names(HRV_base2)[names(HRV_base2)=="ANN"] <- "ANN_base"
names(HRV_base2)[names(HRV_base2)=="SDNN"] <- "SDNN_base"
names(HRV_base2)[names(HRV_base2)=="RMSSD"] <- "RMSSD_base"
names(HRV_base2)[names(HRV_base2)=="SDSD"] <- "SDSD_base"
names(HRV_base2)[names(HRV_base2)=="SDANN"] <- "SDANN_base"
names(HRV_base2)[names(HRV_base2)=="NN50"] <- "NN50_base"
names(HRV_base2)[names(HRV_base2)=="SDNNi"] <- "SDNNi_base"
names(HRV_base2)[names(HRV_base2)=="pNN50"] <- "pNN50_base"
names(HRV_base2)[names(HRV_base2)=="VLF"] <- "VLF_base"
names(HRV_base2)[names(HRV_base2)=="VLF"] <- "VLF_base"
names(HRV_base2)[names(HRV_base2)=="LF"] <- "LF_base"
names(HRV_base2)[names(HRV_base2)=="HF"] <- "HF_base"
names(HRV_base2)[names(HRV_base2)=="LF (NORM)"] <- "LF (NORM)_base"
names(HRV_base2)[names(HRV_base2)=="HF (NORM)"] <- "HF (NORM)_base"
names(HRV_base2)[names(HRV_base2)=="LF/HF"] <- "LF/HF_base"
names(HRV_base2)[names(HRV_base2)=="RSA"] <- "RSA_base"
names(HRV_base2)[names(HRV_base2)=="SD1"] <- "SD1_base"
names(HRV_base2)[names(HRV_base2)=="SD2"] <- "SD2_base"
names(HRV_base2)[names(HRV_base2)=="SamplEn"] <- "SamplEn_base"
names(HRV_base2)[names(HRV_base2)=="Correlation Dimension"] <- "Correlation Dimension_base"
#drop columns for stage, timestamp, duration, time
HRV_base2 <- HRV_base2[,-1]
HRV_base2 <- HRV_base2[,-3]
HRV_base2 <- HRV_base2[,-4]
HRV_base2 <- HRV_base2[,-4]
HRV_base2 <- HRV_base2[,-23]


#immed 
HRV_immed2 <- filter(HRVprog2, stage == "immed")
names(HRV_immed2)[names(HRV_immed2)=="Annotation"] <- "Annotation_immed"
names(HRV_immed2)[names(HRV_immed2)=="ANN"] <- "ANN_immed"
names(HRV_immed2)[names(HRV_immed2)=="SDNN"] <- "SDNN_immed"
names(HRV_immed2)[names(HRV_immed2)=="RMSSD"] <- "RMSSD_immed"
names(HRV_immed2)[names(HRV_immed2)=="SDSD"] <- "SDSD_immed"
names(HRV_immed2)[names(HRV_immed2)=="SDANN"] <- "SDANN_immed"
names(HRV_immed2)[names(HRV_immed2)=="NN50"] <- "NN50_immed"
names(HRV_immed2)[names(HRV_immed2)=="SDNNi"] <- "SDNNi_immed"
names(HRV_immed2)[names(HRV_immed2)=="pNN50"] <- "pNN50_immed"
names(HRV_immed2)[names(HRV_immed2)=="VLF"] <- "VLF_immed"
names(HRV_immed2)[names(HRV_immed2)=="VLF"] <- "VLF_immed"
names(HRV_immed2)[names(HRV_immed2)=="LF"] <- "LF_immed"
names(HRV_immed2)[names(HRV_immed2)=="HF"] <- "HF_immed"
names(HRV_immed2)[names(HRV_immed2)=="LF (NORM)"] <- "LF (NORM)_immed"
names(HRV_immed2)[names(HRV_immed2)=="HF (NORM)"] <- "HF (NORM)_immed"
names(HRV_immed2)[names(HRV_immed2)=="LF/HF"] <- "LF/HF_immed"
names(HRV_immed2)[names(HRV_immed2)=="RSA"] <- "RSA_immed"
names(HRV_immed2)[names(HRV_immed2)=="SD1"] <- "SD1_immed"
names(HRV_immed2)[names(HRV_immed2)=="SD2"] <- "SD2_immed"
names(HRV_immed2)[names(HRV_immed2)=="SamplEn"] <- "SamplEn_immed"
names(HRV_immed2)[names(HRV_immed2)=="Correlation Dimension"] <- "Correlation Dimension_immed"
#drop columns for stage, timestamp, duration, time
HRV_immed2 <- HRV_immed2[,-1]
HRV_immed2 <- HRV_immed2[,-3]
HRV_immed2 <- HRV_immed2[,-4]
HRV_immed2 <- HRV_immed2[,-4]
HRV_immed2 <- HRV_immed2[,-23]

#post1
HRV_post1_2 <- filter(HRVprog2, stage == "post1")
names(HRV_post1_2)[names(HRV_post1_2)=="Annotation"] <- "Annotation_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="ANN"] <- "ANN_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SDNN"] <- "SDNN_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="RMSSD"] <- "RMSSD_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SDSD"] <- "SDSD_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SDANN"] <- "SDANN_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="NN50"] <- "NN50_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SDNNi"] <- "SDNNi_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="pNN50"] <- "pNN50_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="VLF"] <- "VLF_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="VLF"] <- "VLF_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="LF"] <- "LF_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="HF"] <- "HF_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="LF (NORM)"] <- "LF (NORM)_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="HF (NORM)"] <- "HF (NORM)_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="LF/HF"] <- "LF/HF_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="RSA"] <- "RSA_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SD1"] <- "SD1_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SD2"] <- "SD2_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="SamplEn"] <- "SamplEn_post1"
names(HRV_post1_2)[names(HRV_post1_2)=="Correlation Dimension"] <- "Correlation Dimension_post1"
#drop columns for stage, timestamp, duration, time
HRV_post1_2 <- HRV_post1_2[,-1]
HRV_post1_2 <- HRV_post1_2[,-3]
HRV_post1_2 <- HRV_post1_2[,-4]
HRV_post1_2 <- HRV_post1_2[,-4]
HRV_post1_2 <- HRV_post1_2[,-23]

#post2 
HRV_post2_2 <- filter(HRVprog2, stage == "post2")
names(HRV_post2_2)[names(HRV_post2_2)=="Annotation"] <- "Annotation_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="ANN"] <- "ANN_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SDNN"] <- "SDNN_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="RMSSD"] <- "RMSSD_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SDSD"] <- "SDSD_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SDANN"] <- "SDANN_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="NN50"] <- "NN50_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SDNNi"] <- "SDNNi_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="pNN50"] <- "pNN50_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="VLF"] <- "VLF_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="VLF"] <- "VLF_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="LF"] <- "LF_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="HF"] <- "HF_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="LF (NORM)"] <- "LF (NORM)_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="HF (NORM)"] <- "HF (NORM)_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="LF/HF"] <- "LF/HF_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="RSA"] <- "RSA_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SD1"] <- "SD1_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SD2"] <- "SD2_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="SamplEn"] <- "SamplEn_post2"
names(HRV_post2_2)[names(HRV_post2_2)=="Correlation Dimension"] <- "Correlation Dimension_post2"
#drop columns for stage, timestamp, duration, time
HRV_post2_2 <- HRV_post2_2[,-1]
HRV_post2_2 <- HRV_post2_2[,-3]
HRV_post2_2 <- HRV_post2_2[,-4]
HRV_post2_2 <- HRV_post2_2[,-4]
HRV_post2_2 <- HRV_post2_2[,-23]

#post3 
HRV_post3_2 <- filter(HRVprog2, stage == "post3")
names(HRV_post3_2)[names(HRV_post3_2)=="Annotation"] <- "Annotation_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="ANN"] <- "ANN_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SDNN"] <- "SDNN_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="RMSSD"] <- "RMSSD_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SDSD"] <- "SDSD_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SDANN"] <- "SDANN_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="NN50"] <- "NN50_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SDNNi"] <- "SDNNi_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="pNN50"] <- "pNN50_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="VLF"] <- "VLF_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="VLF"] <- "VLF_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="LF"] <- "LF_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="HF"] <- "HF_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="LF (NORM)"] <- "LF (NORM)_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="HF (NORM)"] <- "HF (NORM)_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="LF/HF"] <- "LF/HF_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="RSA"] <- "RSA_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SD1"] <- "SD1_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SD2"] <- "SD2_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="SamplEn"] <- "SamplEn_post3"
names(HRV_post3_2)[names(HRV_post3_2)=="Correlation Dimension"] <- "Correlation Dimension_post3"
#drop columns for stage, timestamp, duration, time
HRV_post3_2 <- HRV_post3_2[,-1]
HRV_post3_2 <- HRV_post3_2[,-3]
HRV_post3_2 <- HRV_post3_2[,-4]
HRV_post3_2 <- HRV_post3_2[,-4]
HRV_post3_2 <- HRV_post3_2[,-23]

#post4 
HRV_post4_2 <- filter(HRVprog2, stage == "post4")
names(HRV_post4_2)[names(HRV_post4_2)=="Annotation"] <- "Annotation_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="ANN"] <- "ANN_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SDNN"] <- "SDNN_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="RMSSD"] <- "RMSSD_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SDSD"] <- "SDSD_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SDANN"] <- "SDANN_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="NN50"] <- "NN50_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SDNNi"] <- "SDNNi_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="pNN50"] <- "pNN50_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="VLF"] <- "VLF_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="VLF"] <- "VLF_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="LF"] <- "LF_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="HF"] <- "HF_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="LF (NORM)"] <- "LF (NORM)_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="HF (NORM)"] <- "HF (NORM)_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="LF/HF"] <- "LF/HF_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="RSA"] <- "RSA_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SD1"] <- "SD1_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SD2"] <- "SD2_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="SamplEn"] <- "SamplEn_post4"
names(HRV_post4_2)[names(HRV_post4_2)=="Correlation Dimension"] <- "Correlation Dimension_post4"
#drop columns for stage, timestamp, duration, time
HRV_post4_2 <- HRV_post4_2[,-1]
HRV_post4_2 <- HRV_post4_2[,-3]
HRV_post4_2 <- HRV_post4_2[,-4]
HRV_post4_2 <- HRV_post4_2[,-4]
HRV_post4_2 <- HRV_post4_2[,-23]

#Eve_base 
HRV_Ebase2 <- filter(HRVprog2, stage == "eve_base")
names(HRV_Ebase2)[names(HRV_Ebase2)=="Annotation"] <- "Annotation_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="ANN"] <- "ANN_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SDNN"] <- "SDNN_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="RMSSD"] <- "RMSSD_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SDSD"] <- "SDSD_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SDANN"] <- "SDANN_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="NN50"] <- "NN50_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SDNNi"] <- "SDNNi_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="pNN50"] <- "pNN50_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="VLF"] <- "VLF_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="VLF"] <- "VLF_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="LF"] <- "LF_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="HF"] <- "HF_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="LF (NORM)"] <- "LF (NORM)_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="HF (NORM)"] <- "HF (NORM)_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="LF/HF"] <- "LF/HF_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="RSA"] <- "RSA_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SD1"] <- "SD1_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SD2"] <- "SD2_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="SamplEn"] <- "SamplEn_Ebase"
names(HRV_Ebase2)[names(HRV_Ebase2)=="Correlation Dimension"] <- "Correlation Dimension_Ebase"
#drop columns for stage, timestamp, duration, time
HRV_Ebase2 <- HRV_Ebase2[,-1]
HRV_Ebase2 <- HRV_Ebase2[,-3]
HRV_Ebase2 <- HRV_Ebase2[,-4]
HRV_Ebase2 <- HRV_Ebase2[,-4]
HRV_Ebase2 <- HRV_Ebase2[,-23]

#Eve immed
HRV_Eimmed2 <- filter(HRVprog2, stage == "eve_immed")
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="Annotation"] <- "Annotation_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="ANN"] <- "ANN_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SDNN"] <- "SDNN_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="RMSSD"] <- "RMSSD_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SDSD"] <- "SDSD_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SDANN"] <- "SDANN_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="NN50"] <- "NN50_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SDNNi"] <- "SDNNi_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="pNN50"] <- "pNN50_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="VLF"] <- "VLF_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="VLF"] <- "VLF_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="LF"] <- "LF_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="HF"] <- "HF_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="LF (NORM)"] <- "LF (NORM)_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="HF (NORM)"] <- "HF (NORM)_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="LF/HF"] <- "LF/HF_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="RSA"] <- "RSA_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SD1"] <- "SD1_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SD2"] <- "SD2_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="SamplEn"] <- "SamplEn_Eimmed"
names(HRV_Eimmed2)[names(HRV_Eimmed2)=="Correlation Dimension"] <- "Correlation Dimension_Eimmed"
#drop columns for stage, timestamp, duration, time
HRV_Eimmed2 <- HRV_Eimmed2[,-1]
HRV_Eimmed2 <- HRV_Eimmed2[,-3]
HRV_Eimmed2 <- HRV_Eimmed2[,-4]
HRV_Eimmed2 <- HRV_Eimmed2[,-4]
HRV_Eimmed2 <- HRV_Eimmed2[,-23]


#Epost1 
HRV_Epost1_2 <- filter(HRVprog2, stage == "eve_post1")
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="Annotation"] <- "Annotation_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="ANN"] <- "ANN_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SDNN"] <- "SDNN_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="RMSSD"] <- "RMSSD_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SDSD"] <- "SDSD_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SDANN"] <- "SDANN_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="NN50"] <- "NN50_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SDNNi"] <- "SDNNi_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="pNN50"] <- "pNN50_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="VLF"] <- "VLF_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="VLF"] <- "VLF_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="LF"] <- "LF_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="HF"] <- "HF_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="LF (NORM)"] <- "LF (NORM)_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="HF (NORM)"] <- "HF (NORM)_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="LF/HF"] <- "LF/HF_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="RSA"] <- "RSA_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SD1"] <- "SD1_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SD2"] <- "SD2_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="SamplEn"] <- "SamplEn_Epost1"
names(HRV_Epost1_2)[names(HRV_Epost1_2)=="Correlation Dimension"] <- "Correlation Dimension_Epost1"
#drop columns for stage, timestamp, duration, time
HRV_Epost1_2 <- HRV_Epost1_2[,-1]
HRV_Epost1_2 <- HRV_Epost1_2[,-3]
HRV_Epost1_2 <- HRV_Epost1_2[,-4]
HRV_Epost1_2 <- HRV_Epost1_2[,-4]
HRV_Epost1_2 <- HRV_Epost1_2[,-23]


#eve post 2
HRV_Epost2_2 <- filter(HRVprog2, stage == "eve_post2")
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="Annotation"] <- "Annotation_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="ANN"] <- "ANN_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SDNN"] <- "SDNN_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="RMSSD"] <- "RMSSD_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SDSD"] <- "SDSD_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SDANN"] <- "SDANN_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="NN50"] <- "NN50_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SDNNi"] <- "SDNNi_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="pNN50"] <- "pNN50_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="VLF"] <- "VLF_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="VLF"] <- "VLF_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="LF"] <- "LF_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="HF"] <- "HF_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="LF (NORM)"] <- "LF (NORM)_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="HF (NORM)"] <- "HF (NORM)_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="LF/HF"] <- "LF/HF_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="RSA"] <- "RSA_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SD1"] <- "SD1_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SD2"] <- "SD2_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="SamplEn"] <- "SamplEn_Epost2"
names(HRV_Epost2_2)[names(HRV_Epost2_2)=="Correlation Dimension"] <- "Correlation Dimension_Epost2"
#drop columns for stage, timestamp, duration, time
HRV_Epost2_2 <- HRV_Epost2_2[,-1]
HRV_Epost2_2 <- HRV_Epost2_2[,-3]
HRV_Epost2_2 <- HRV_Epost2_2[,-4]
HRV_Epost2_2 <- HRV_Epost2_2[,-4]
HRV_Epost2_2 <- HRV_Epost2_2[,-23]

#eve post 3 
HRV_Epost3_2 <- filter(HRVprog2, stage == "eve_post3")
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="Annotation"] <- "Annotation_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="ANN"] <- "ANN_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SDNN"] <- "SDNN_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="RMSSD"] <- "RMSSD_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SDSD"] <- "SDSD_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SDANN"] <- "SDANN_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="NN50"] <- "NN50_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SDNNi"] <- "SDNNi_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="pNN50"] <- "pNN50_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="VLF"] <- "VLF_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="VLF"] <- "VLF_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="LF"] <- "LF_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="HF"] <- "HF_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="LF (NORM)"] <- "LF (NORM)_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="HF (NORM)"] <- "HF (NORM)_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="LF/HF"] <- "LF/HF_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="RSA"] <- "RSA_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SD1"] <- "SD1_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SD2"] <- "SD2_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="SamplEn"] <- "SamplEn_Epost3"
names(HRV_Epost3_2)[names(HRV_Epost3_2)=="Correlation Dimension"] <- "Correlation Dimension_Epost3"
#drop columns for stage, timestamp, duration, time
HRV_Epost3_2 <- HRV_Epost3_2[,-1]
HRV_Epost3_2 <- HRV_Epost3_2[,-3]
HRV_Epost3_2 <- HRV_Epost3_2[,-4]
HRV_Epost3_2 <- HRV_Epost3_2[,-4]
HRV_Epost3_2 <- HRV_Epost3_2[,-23]

#eve post 4
HRV_Epost4_2 <- filter(HRVprog2, stage == "eve_post4")
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="Annotation"] <- "Annotation_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="ANN"] <- "ANN_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SDNN"] <- "SDNN_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="RMSSD"] <- "RMSSD_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SDSD"] <- "SDSD_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SDANN"] <- "SDANN_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="NN50"] <- "NN50_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SDNNi"] <- "SDNNi_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="pNN50"] <- "pNN50_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="VLF"] <- "VLF_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="VLF"] <- "VLF_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="LF"] <- "LF_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="HF"] <- "HF_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="LF (NORM)"] <- "LF (NORM)_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="HF (NORM)"] <- "HF (NORM)_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="LF/HF"] <- "LF/HF_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="RSA"] <- "RSA_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SD1"] <- "SD1_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SD2"] <- "SD2_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="SamplEn"] <- "SamplEn_Epost4"
names(HRV_Epost4_2)[names(HRV_Epost4_2)=="Correlation Dimension"] <- "Correlation Dimension_Epost4"
#drop columns for stage, timestamp, duration, time
HRV_Epost4_2 <- HRV_Epost4_2[,-1]
HRV_Epost4_2 <- HRV_Epost4_2[,-3]
HRV_Epost4_2 <- HRV_Epost4_2[,-4]
HRV_Epost4_2 <- HRV_Epost4_2[,-4]
HRV_Epost4_2 <- HRV_Epost4_2[,-23]



HRV_wide_2 <- full_join(HRV_base2, HRV_immed2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_post1_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_post2_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_post3_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_post4_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Ebase2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Eimmed2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Epost1_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Epost2_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Epost3_2, by= c("subjectID", "sessionID"))
HRV_wide_2 <- full_join(HRV_wide_2, HRV_Epost4_2, by= c("subjectID", "sessionID"))

#Add difference from basline column 

HRV_wide_2$RMSSD_immed_diff <- HRV_wide_2$RMSSD_immed - HRV_wide_2$RMSSD_base
HRV_wide_2$RMSSD_p1_diff <- HRV_wide_2$RMSSD_post1 - HRV_wide_2$RMSSD_base
HRV_wide_2$RMSSD_p2_diff <- HRV_wide_2$RMSSD_post2 - HRV_wide_2$RMSSD_base
HRV_wide_2$RMSSD_p3_diff <- HRV_wide_2$RMSSD_post3 - HRV_wide_2$RMSSD_base
HRV_wide_2$RMSSD_p4_diff <- HRV_wide_2$RMSSD_post4 - HRV_wide_2$RMSSD_base

HRV_wide_2$RMSSD_Eimmed_diff <- HRV_wide_2$RMSSD_immed - HRV_wide_2$RMSSD_Ebase
HRV_wide_2$RMSSD_Ep1_diff <- HRV_wide_2$RMSSD_Epost1 - HRV_wide_2$RMSSD_Ebase
HRV_wide_2$RMSSD_Ep2_diff <- HRV_wide_2$RMSSD_Epost2 - HRV_wide_2$RMSSD_Ebase
HRV_wide_2$RMSSD_Ep3_diff <- HRV_wide_2$RMSSD_Epost3 - HRV_wide_2$RMSSD_Ebase
HRV_wide_2$RMSSD_Ep4_diff <- HRV_wide_2$RMSSD_Epost4 - HRV_wide_2$RMSSD_Ebase


HRV_wide_2$SDNN_immed_diff <- HRV_wide_2$SDNN_immed - HRV_wide_2$SDNN_base
HRV_wide_2$SDNN_p1_diff <- HRV_wide_2$SDNN_post1 - HRV_wide_2$SDNN_base
HRV_wide_2$SDNN_p2_diff <- HRV_wide_2$SDNN_post2 - HRV_wide_2$SDNN_base
HRV_wide_2$SDNN_p3_diff <- HRV_wide_2$SDNN_post3 - HRV_wide_2$SDNN_base
HRV_wide_2$SDNN_p4_diff <- HRV_wide_2$SDNN_post4 - HRV_wide_2$SDNN_base

HRV_wide_2$SDNN_Eimmed_diff <- HRV_wide_2$SDNN_Eimmed - HRV_wide_2$SDNN_Ebase
HRV_wide_2$SDNN_Ep1_diff <- HRV_wide_2$SDNN_Epost1 - HRV_wide_2$SDNN_Ebase
HRV_wide_2$SDNN_Ep2_diff <- HRV_wide_2$SDNN_Epost2 - HRV_wide_2$SDNN_Ebase
HRV_wide_2$SDNN_Ep3_diff <- HRV_wide_2$SDNN_Epost3 - HRV_wide_2$SDNN_Ebase
HRV_wide_2$SDNN_Ep4_diff <- HRV_wide_2$SDNN_Epost4 - HRV_wide_2$SDNN_Ebase

#add demographic data

phase2_demo <- read.csv("Phase2_demo.csv")

HRV_wide_2 <- merge(HRV_wide_2, phase2_demo, by="subjectID")


write.csv(HRV_wide_2, "HRV_wide_phase2.csv")

# Merge with exposure data

# upload file with bc and PM averages 

BCPMdaily <- read.csv("")

#join with wide file 
library(dplyr)

HRV_p2_bcpm <- full_join(HRV_wide_2, BCPMdaily, by=c("subjectid", "session_id"))


# full combined file
write.csv(HRV_p2_bcpm, "HRV_p2_bcpm.csv")

#file only with sessions with morning baseline

HRV_p2_bcpm_am <- filter(HRV_p2_bcpm, !is.na(SDNN_base) )

#file for morning HRV analysis 
write.csv(HRV_p2_bcpm_am, "HRV_p2_bcpm_am.csv")

#File set up for SAS for morn analysis 

HRV_p2_sessions <- select(HRV_p2_bcpm_am,  subjectid, session_id, 
                       Age, Weight, Height, BMI, Sex, morn_bc_mean, morn_bcdose_mean,
                       morn_bcdose_ug, morn_pm_mean, morn_pmdose_mean, morn_pmdose_ug ,
                       RMSSD_base, SDNN_base, RMSSD_immed_diff, RMSSD_p1_diff , RMSSD_p2_diff, RMSSD_p3_diff, RMSSD_p4_diff, SDNN_immed_diff, SDNN_p1_diff , SDNN_p2_diff, SDNN_p3_diff, SDNN_p4_diff)


sessions_.5 <- select(HRV_p2_sessions, subjectid, session_id, RMSSD_immed_diff, SDNN_immed_diff)
sessions_.5$Lag  <- 0.5
names(sessions_.5)[names(sessions_.5)=="RMSSD_immed_diff"] <- "d_RMSSD"
names(sessions_.5)[names(sessions_.5)=="SDNN_immed_diff"] <- "d_SDNN"

sessions_1 <- select(HRV_p2_sessions, subjectid, session_id, RMSSD_p1_diff, SDNN_p1_diff)
sessions_1$Lag  <- 1
names(sessions_1)[names(sessions_1)=="RMSSD_p1_diff"] <- "d_RMSSD"
names(sessions_1)[names(sessions_1)=="SDNN_p1_diff"] <- "d_SDNN"


sessions_2 <- select(HRV_p2_sessions, subjectid, session_id, RMSSD_p2_diff, SDNN_p2_diff)
sessions_2$Lag  <- 2
names(sessions_2)[names(sessions_2)=="RMSSD_p2_diff"] <- "d_RMSSD"
names(sessions_2)[names(sessions_2)=="SDNN_p2_diff"] <- "d_SDNN"

sessions_3 <- select(HRV_p2_sessions, subjectid, session_id, RMSSD_p3_diff, SDNN_p3_diff)
sessions_3$Lag  <- 3
names(sessions_3)[names(sessions_3)=="RMSSD_p3_diff"] <- "d_RMSSD"
names(sessions_3)[names(sessions_3)=="SDNN_p3_diff"] <- "d_SDNN"

sessions_4 <- select(HRV_p2_sessions, subjectid, session_id, RMSSD_p4_diff, SDNN_p4_diff)
sessions_4$Lag  <- 4
names(sessions_4)[names(sessions_4)=="RMSSD_p4_diff"] <- "d_RMSSD"
names(sessions_4)[names(sessions_4)=="SDNN_p4_diff"] <- "d_SDNN"


sessions_lags <- bind_rows(sessions_.5, sessions_1)
sessions_lags <- bind_rows(sessions_lags, sessions_2)
sessions_lags <- bind_rows(sessions_lags, sessions_3)
sessions_lags <- bind_rows(sessions_lags, sessions_4)

session2 <- select(HRV_p2_sessions ,subjectid, session_id, 
                   Age, Weight, Height, BMI, Sex, morn_bc_mean, morn_bcdose_mean,
                   morn_bcdose_ug, morn_pm_mean, morn_pmdose_mean, morn_pmdose_ug ,
                   RMSSD_base, SDNN_base )

sessions_long <- full_join(session2, sessions_lags, by=c("subjectid", "session_id"))


write.csv(sessions_long, "HRV_p2_sessions.csv", na="")
