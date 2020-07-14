#combined ABP and exposure data file

# upload file with bc and PM averages 

dailyBCPM <- read.csv("")

library(dplyr)
#join with wide file 
abp_p2_bcpm <- full_join(abp_wide_y5, dailyBCPM, by=c("subjectid", "session_id"))

#filter out sessions without morning baseline data 
abp_p2_bcpm_am <- filter(abp_p2_bcpm, !is.na(sys_base_length))

#make change columns (outcome compared to basline)

abp_p2_bcpm_am$sys_immed_diff <- abp_p2_bcpm_am$sys_immed_mean - abp_p2_bcpm_am$sys_base_mean
abp_p2_bcpm_am$sys_p1_diff <- abp_p2_bcpm_am$sys_post1_mean - abp_p2_bcpm_am$sys_base_mean
abp_p2_bcpm_am$sys_p2_diff <- abp_p2_bcpm_am$sys_post2_mean - abp_p2_bcpm_am$sys_base_mean
abp_p2_bcpm_am$sys_p3_diff <- abp_p2_bcpm_am$sys_post3_mean - abp_p2_bcpm_am$sys_base_mean
abp_p2_bcpm_am$sys_p4_diff <- abp_p2_bcpm_am$sys_post4_mean - abp_p2_bcpm_am$sys_base_mean
abp_p2_bcpm_am$sys_Eimmed_diff <- abp_p2_bcpm_am$sys_Eimmed_mean - abp_p2_bcpm_am$sys_Ebase_mean
abp_p2_bcpm_am$sys_Ep1_diff <- abp_p2_bcpm_am$sys_Epost1_mean - abp_p2_bcpm_am$sys_Ebase_mean
abp_p2_bcpm_am$sys_Ep2_diff <- abp_p2_bcpm_am$sys_Epost2_mean - abp_p2_bcpm_am$sys_Ebase_mean
abp_p2_bcpm_am$sys_Ep3_diff <- abp_p2_bcpm_am$sys_Epost3_mean - abp_p2_bcpm_am$sys_Ebase_mean
abp_p2_bcpm_am$sys_Ep4_diff <- abp_p2_bcpm_am$sys_Epost4_mean - abp_p2_bcpm_am$sys_Ebase_mean

abp_p2_bcpm_am$dia_immed_diff <- abp_p2_bcpm_am$dia_immed_mean - abp_p2_bcpm_am$dia_base_mean
abp_p2_bcpm_am$dia_p1_diff <- abp_p2_bcpm_am$dia_post1_mean - abp_p2_bcpm_am$dia_base_mean
abp_p2_bcpm_am$dia_p2_diff <- abp_p2_bcpm_am$dia_post2_mean - abp_p2_bcpm_am$dia_base_mean
abp_p2_bcpm_am$dia_p3_diff <- abp_p2_bcpm_am$dia_post3_mean - abp_p2_bcpm_am$dia_base_mean
abp_p2_bcpm_am$dia_p4_diff <- abp_p2_bcpm_am$dia_post4_mean - abp_p2_bcpm_am$dia_base_mean
abp_p2_bcpm_am$dia_Eimmed_diff <- abp_p2_bcpm_am$dia_Eimmed_mean - abp_p2_bcpm_am$dia_Ebase_mean
abp_p2_bcpm_am$dia_Ep1_diff <- abp_p2_bcpm_am$dia_Epost1_mean - abp_p2_bcpm_am$dia_Ebase_mean
abp_p2_bcpm_am$dia_Ep2_diff <- abp_p2_bcpm_am$dia_Epost2_mean - abp_p2_bcpm_am$dia_Ebase_mean
abp_p2_bcpm_am$dia_Ep3_diff <- abp_p2_bcpm_am$dia_Epost3_mean - abp_p2_bcpm_am$dia_Ebase_mean
abp_p2_bcpm_am$dia_Ep4_diff <- abp_p2_bcpm_am$dia_Epost4_mean - abp_p2_bcpm_am$dia_Ebase_mean

abp_p2_bcpm_am$mean_arterial_immed_diff <- abp_p2_bcpm_am$mean_arterial_immed - abp_p2_bcpm_am$mean_arterial_base
abp_p2_bcpm_am$mean_arterial_p1_diff <- abp_p2_bcpm_am$mean_arterial_post1 - abp_p2_bcpm_am$mean_arterial_base
abp_p2_bcpm_am$mean_arterial_p2_diff <- abp_p2_bcpm_am$mean_arterial_post2 - abp_p2_bcpm_am$mean_arterial_base
abp_p2_bcpm_am$mean_arterial_p3_diff <- abp_p2_bcpm_am$mean_arterial_post3 - abp_p2_bcpm_am$mean_arterial_base
abp_p2_bcpm_am$mean_arterial_p4_diff <- abp_p2_bcpm_am$mean_arterial_post4 - abp_p2_bcpm_am$mean_arterial_base
abp_p2_bcpm_am$mean_arterial_Eimmed_diff <- abp_p2_bcpm_am$mean_arterial_Eimmed - abp_p2_bcpm_am$mean_arterial_Ebase
abp_p2_bcpm_am$mean_arterial_Ep1_diff <- abp_p2_bcpm_am$mean_arterial_Epost1 - abp_p2_bcpm_am$mean_arterial_Ebase
abp_p2_bcpm_am$mean_arterial_Ep2_diff <- abp_p2_bcpm_am$mean_arterial_Epost2 - abp_p2_bcpm_am$mean_arterial_Ebase
abp_p2_bcpm_am$mean_arterial_Ep3_diff <- abp_p2_bcpm_am$mean_arterial_Epost3 - abp_p2_bcpm_am$mean_arterial_Ebase
abp_p2_bcpm_am$mean_arterial_Ep4_diff <- abp_p2_bcpm_am$mean_arterial_Epost4 - abp_p2_bcpm_am$mean_arterial_Ebase

#add demograhic data 

phase2_demo <- read.csv("Phase2_demo.csv")

abp_p2_bcpm_am <- merge(abp_p2_bcpm_am, phase2_demo, by="subjectID")

# file for morning abp analysis on session level
# look for any outliers 
write.csv(abp_p1_bcpm_am, "abp_p1_bcpm_am.csv")

# data frame for SAS 

sessions <- select(abp_p1_bcpm_am_out, subjectid, session_id, 
                   Age, Weight, Height, BMI, Sex, morn_bc_mean, morn_bcdose_mean,
                   morn_bcdose_ug, morn_pm_mean, morn_pmdose_mean, morn_pmdose_ug ,
                   sys_base_mean, dia_base_mean, mean_arterial_base ,sys_immed_diff, sys_p1_diff , sys_p2_diff, sys_p3_diff, sys_p4_diff, dia_immed_diff, dia_p1_diff , dia_p2_diff, dia_p3_diff, dia_p4_diff, mean_arterial_immed_diff, mean_arterial_p1_diff, mean_arterial_p2_diff, mean_arterial_p3_diff, mean_arterial_p4_diff)


sessions_.5 <- select(sessions, subjectid, session_id, sys_immed_diff, dia_immed_diff,mean_arterial_immed_diff )
sessions_.5$Lag  <- 0.5
names(sessions_.5)[names(sessions_.5)=="sys_immed_diff"] <- "d_SysBP"
names(sessions_.5)[names(sessions_.5)=="dia_immed_diff"] <- "d_DiasBP"
names(sessions_.5)[names(sessions_.5)=="mean_arterial_immed_diff"] <- "d_Arterial"

sessions_1 <- select(sessions, subjectid, session_id, sys_p1_diff, dia_p1_diff,mean_arterial_p1_diff)
sessions_1$Lag  <- 1
names(sessions_1)[names(sessions_1)=="sys_p1_diff"] <- "d_SysBP"
names(sessions_1)[names(sessions_1)=="dia_p1_diff"] <- "d_DiasBP"
names(sessions_1)[names(sessions_1)=="mean_arterial_p1_diff"] <- "d_Arterial"

sessions_2 <- select(sessions, subjectid, session_id, sys_p2_diff, dia_p2_diff,mean_arterial_p2_diff)
sessions_2$Lag  <- 2
names(sessions_2)[names(sessions_2)=="sys_p2_diff"] <- "d_SysBP"
names(sessions_2)[names(sessions_2)=="dia_p2_diff"] <- "d_DiasBP"
names(sessions_2)[names(sessions_2)=="mean_arterial_p2_diff"] <- "d_Arterial"

sessions_3 <- select(sessions, subjectid, session_id, sys_p3_diff, dia_p3_diff,mean_arterial_p3_diff)
sessions_3$Lag  <- 3
names(sessions_3)[names(sessions_3)=="sys_p3_diff"] <- "d_SysBP"
names(sessions_3)[names(sessions_3)=="dia_p3_diff"] <- "d_DiasBP"
names(sessions_3)[names(sessions_3)=="mean_arterial_p3_diff"] <- "d_Arterial"

sessions_4 <- select(sessions, subjectid, session_id, sys_p4_diff, dia_p4_diff,mean_arterial_p4_diff)
sessions_4$Lag  <- 4
names(sessions_4)[names(sessions_4)=="sys_p4_diff"] <- "d_SysBP"
names(sessions_4)[names(sessions_4)=="dia_p4_diff"] <- "d_DiasBP"
names(sessions_4)[names(sessions_4)=="mean_arterial_p4_diff"] <- "d_Arterial"


sessions_lags <- bind_rows(sessions_.5, sessions_1)
sessions_lags <- bind_rows(sessions_lags, sessions_2)
sessions_lags <- bind_rows(sessions_lags, sessions_3)
sessions_lags <- bind_rows(sessions_lags, sessions_4)

session2 <- select(sessions,subjectid, session_id, 
                   Age, Weight, Height, BMI, Sex, morn_bc_mean, morn_bcdose_mean,
                   morn_bcdose_ug, morn_pm_mean, morn_pmdose_mean, morn_pmdose_ug ,
                   sys_base_mean, dia_base_mean, mean_arterial_base )

sessions_long <- full_join(session2, sessions_lags, by=c("subjectid", "session_id"))

write.csv(sessions_long, "sessions.csv", na="")



