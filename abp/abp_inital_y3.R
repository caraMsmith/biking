# upload raw abp data ; file obtained from database output
# Aurthor: Cara 

library(readr)
raw_abp_y3 <- read.delim("abp_y3.txt", header=TRUE, sep="\t")
View(raw_abp_y3)

library(lubridate)
library(dplyr)
Sys.setenv(TZ="America/New_York")

#upload checklist of times

checklist <- read.csv("checklistoftimes.csv")
View(checklist)
#format dates
checklist$date_bike <- mdy(checklist$date_bike)
checklist$date_start <- mdy(checklist$date_start)
checklist$date_sleep <- mdy(checklist$date_sleep)
checklist$date_end <- mdy(checklist$date_end)
checklist$date_wake <- mdy(checklist$date_wake)
checklist$date_eve_start<- mdy(checklist$date_eve_start)

#combine dates and time to one column 

checklist$sleep <- as.POSIXct(paste(checklist$date_sleep, checklist$sleep), format = "%Y-%m-%d %H:%M" , tz = "America/New_York")
checklist$wakeup <- as.POSIXct(paste(checklist$date_wake, checklist$wakeup), format = "%Y-%m-%d %H:%M",  tz = "America/New_York")
checklist$morn_bike_start <- as.POSIXct(paste(checklist$date_bike, checklist$morn_bike_start), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
checklist$morn_bike_end <- as.POSIXct(paste(checklist$date_bike, checklist$morn_bike_end), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
checklist$eve_bike_start <- as.POSIXct(paste(checklist$date_eve_start, checklist$eve_bike_start), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
checklist$eve_bike_end <- as.POSIXct(paste(checklist$date_end, checklist$eve_bike_end), format = "%Y-%m-%d %H:%M", tz = "America/New_York")



#merge abp data and checklist of times
#I used two methods, reight_join and merge. both produced same results. 
raw_abp_y3$date_abp <- as.Date(raw_abp_y3$datetime)
raw_abp_y3$date_start <- as.Date(raw_abp_y3$datetime)
raw_abp_y3$date_wake <- as.Date(raw_abp_y3$datetime)


abp_check_y3_start <- right_join(raw_abp_y3, checklist, by = c("subjectid", "date_start"), all.x = TRUE)
abp_check_y3_wake <- right_join(raw_abp_y3, checklist, by = c("subjectid", "date_wake"), all.x = TRUE)

abp_check_y3_start_2 <- merge(raw_abp_y3, checklist, by = c("subjectid", "date_start"), all.x = TRUE)
abp_check_y3_wake_2 <- merge(raw_abp_y3, checklist, by = c("subjectid", "date_wake"), all.x = TRUE)


abp_check_y3_start <- filter(abp_check_y3_start, !is.na(systolic_bp))
abp_check_y3_wake <- filter(abp_check_y3_wake, !is.na(systolic_bp))

abp_check_y3_start_2 <- filter(abp_check_y3_start_2, !is.na(date_bike))
abp_check_y3_wake_2 <- filter(abp_check_y3_wake_2, !is.na(date_bike))


abp_check_y3 <- bind_rows(abp_check_y3_start, abp_check_y3_wake)

abp_check_y3_2 <- bind_rows(abp_check_y3_start_2, abp_check_y3_wake_2)

#  abp_check_y3 and abp_check_y3_2  are same data table 


#adding sleep wake 
abp_check_y3$timeA <- as.POSIXct(abp_check_y3$datetime,format = "%Y-%m-%d %H:%M", tz = "America/New_York" )

abp_check_y3$sleep_wake <- ifelse(abp_check_y3$timeA >= abp_check_y3$sleep  & 
                                    abp_check_y3$timeA <= abp_check_y3$wakeup, "sleep", "wake") 

abp_check_y3$stage [abp_check_y3$sleep_wake == "sleep"] <- "sleep"

#add baseline; created numeric times for each because that is the only way I could get it to work

abp_check_y3$morn_bike_start_nu <- as.numeric(abp_check_y3$morn_bike_start)
abp_check_y3$wakeup_nu <- as.numeric(abp_check_y3$wakeup)
abp_check_y3$timeA_nu <- as.numeric(abp_check_y3$timeA)

# subtracted an hour from start of bike ride to get begining of baseline
abp_check_y3$baseT <- abp_check_y3$morn_bike_start_nu - 3600

abp_check_y3$stage [abp_check_y3$timeA_nu < abp_check_y3$morn_bike_start_nu & 
                      abp_check_y3$timeA_nu >= abp_check_y3$baseT] <- "Baseline"


#add immed after morning ride ( one hour period); add hour to end of bike time to get end of immed period

abp_check_y3$morn_bike_end_nu <- as.numeric(abp_check_y3$morn_bike_end)
abp_check_y3$immedT <- abp_check_y3$morn_bike_end_nu + 3600

abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$morn_bike_end_nu & 
                      abp_check_y3$timeA_nu <= abp_check_y3$immedT] <- "immed"

#post1 hour after morning ride
abp_check_y3$post1T <- abp_check_y3$immedT + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$immedT & 
                      abp_check_y3$timeA_nu <= abp_check_y3$post1T] <- "post1"

#post 2 hours after morning ride
abp_check_y3$post2T <- abp_check_y3$post1T + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post1T & 
                      abp_check_y3$timeA_nu <= abp_check_y3$post2T] <- "post2"

#post 3 hours after morning ride
abp_check_y3$post3T <- abp_check_y3$post2T + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post2T & 
                      abp_check_y3$timeA_nu <= abp_check_y3$post3T] <- "post3"

#post 4 hours after morning ride 
abp_check_y3$post4T <- abp_check_y3$post3T + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post3T &
                      abp_check_y3$timeA_nu <= abp_check_y3$post4T] <- "post4"



# baseline period before evening ride

abp_check_y3$eve_bike_start_nu <- as.numeric(abp_check_y3$eve_bike_start)
abp_check_y3$eve_bike_end_nu <- as.numeric(abp_check_y3$eve_bike_end)

abp_check_y3$baseT_eve <- abp_check_y3$eve_bike_start_nu - 3600

abp_check_y3$stage [abp_check_y3$timeA_nu < abp_check_y3$eve_bike_start_nu & 
                      abp_check_y3$timeA_nu >= abp_check_y3$baseT_eve] <- "Eve_Baseline"

#immed after evening ride 
abp_check_y3$immedT_eve <- abp_check_y3$eve_bike_end_nu + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$eve_bike_end_nu 
                    & abp_check_y3$timeA_nu < abp_check_y3$immedT_eve] <- "Eve_immed"

# post 1 hour after evening ride
abp_check_y3$post1T_eve <- abp_check_y3$immedT_eve + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$immedT_eve & abp_check_y3$timeA_nu <= abp_check_y3$post1T_eve] <- "Eve_post1"

#post 2 hours after evening ride
abp_check_y3$post2T_eve <- abp_check_y3$post1T_eve + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post1T_eve & abp_check_y3$timeA_nu <= abp_check_y3$post2T_eve] <- "Eve_post2"

#post 3 hours after evening ride
abp_check_y3$post3T_eve <- abp_check_y3$post2T_eve + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post2T_eve & abp_check_y3$timeA_nu <= abp_check_y3$post3T_eve] <- "Eve_post3"

#post 4 hours after evening ride
abp_check_y3$post4T_eve <- abp_check_y3$post3T_eve + 3600
abp_check_y3$stage [abp_check_y3$timeA_nu > abp_check_y3$post3T_eve & abp_check_y3$timeA_nu <= abp_check_y3$post4T_eve] <- "Eve_post4"

# Mark all sleep to indicate sleeping and not post evening readings 
abp_check_y3$stage [abp_check_y3$sleep_wake == "sleep"] <- "sleep"


#### summary statistics 

# number of total sleep readings 

summary(abp_check_y3$stage=="sleep")

# 681 total sleep readings

#number of non-error sleep readings

summary(abp_check_y3$stage=="sleep" & is.na(abp_check_y3$event_code))

# 562 night readings (non-error) 

#get rid of 'sessionid' as it has incorrect session id. use 'session_id' instead

abp_check_y3$sessionid <- NULL

# subset of data with only non-error readings 

abp_check_y3_read <- filter(abp_check_y3, abp_check_y3$event_code!= "EE" | is.na(abp_check_y3$event_code))
# succussfull readings  : 3902

# add binary columns for sleep and wake to easily get sums 

abp_check_y3_read$sleep_read <- ifelse(abp_check_y3_read$sleep_wake=="sleep",1,0)
abp_check_y3_read$wake_read <- ifelse(abp_check_y3_read$sleep_wake=="wake",1,0)

abp_check_y3_read$stage [is.na(abp_check_y3_read$stage)] <- "wake"

#add binary for baseline, immed, post1, ect... 
abp_check_y3_read$base_read <- ifelse(abp_check_y3_read$stage=="Baseline",1,0)
abp_check_y3_read$immed_read <- ifelse(abp_check_y3_read$stage=="immed",1,0)
abp_check_y3_read$post1_read <- ifelse(abp_check_y3_read$stage=="post1",1,0)
abp_check_y3_read$post2_read <- ifelse(abp_check_y3_read$stage=="post2",1,0)
abp_check_y3_read$post3_read <- ifelse(abp_check_y3_read$stage=="post3",1,0)
abp_check_y3_read$post4_read <- ifelse(abp_check_y3_read$stage=="post4",1,0)
abp_check_y3_read$eve_base_read <- ifelse(abp_check_y3_read$stage=="Eve_Baseline",1,0)
abp_check_y3_read$eve_immed_read <- ifelse(abp_check_y3_read$stage=="Eve_immed",1,0)
abp_check_y3_read$eve_post1_read <- ifelse(abp_check_y3_read$stage=="Eve_post1",1,0)
abp_check_y3_read$eve_post2_read <- ifelse(abp_check_y3_read$stage=="Eve_post2",1,0)
abp_check_y3_read$eve_post3_read <- ifelse(abp_check_y3_read$stage=="Eve_post3",1,0)
abp_check_y3_read$eve_post4_read <- ifelse(abp_check_y3_read$stage=="Eve_post4",1,0)


# create summary table for data where each line is one session. Contains the min,mean,max, and SD for 
# sbp and dbp. Creates sum of all non-error readings for each stage
ABP_summary_y3 <- abp_check_y3_read %>% group_by(subjectid, session_id) %>% dplyr::summarise(subjectid[1], session_id[1], datetime[1], length(systolic_bp), min(systolic_bp), mean(systolic_bp), max(systolic_bp), sd(systolic_bp), min(diastolic_bp), mean(diastolic_bp), max(diastolic_bp), sd(diastolic_bp), mean(mean_arterial_p), mean(heart_rate), sum(sleep_read), sum(wake_read), sum(base_read), sum(immed_read), sum(post1_read),
                                                                                             sum(post2_read), sum(post3_read), sum(post4_read), sum(eve_base_read), sum(eve_immed_read), sum(eve_post1_read), sum(eve_post2_read), sum(eve_post3_read), sum(eve_post4_read))

# average # of night readings
names(ABP_summary_y3)[names(ABP_summary_y3)=="sum(sleep_read)"] <- "sleep_read"

mean(ABP_summary_y3$sleep_read, na.rm = TRUE)

###########################
# Make table with all of days readings in one line
# control for outliers here 
#SBP: 70-260 mmHg, DBP: 40-150 mmHg, pulse pressure: 20-150 mmHg, heart rate: 40-130 bpm. 

abp_check_y3_read<- filter(abp_check_y3_read, systolic_bp < 260)
abp_check_y3_read<- filter(abp_check_y3_read, systolic_bp > 70)
abp_check_y3_read<- filter(abp_check_y3_read, diastolic_bp < 150)
abp_check_y3_read<- filter(abp_check_y3_read, diastolic_bp > 40)

# table of means
abp_stages_mean <- abp_check_y3_read %>% group_by(subjectid, session_id, stage) %>% dplyr::summarise(length(systolic_bp), min(systolic_bp), mean(systolic_bp), max(systolic_bp), sd(systolic_bp), min(diastolic_bp), mean(diastolic_bp), max(diastolic_bp), sd(diastolic_bp), mean(mean_arterial_p))

#filter our baseline and rename columns                                                                                                                                                                                   
base_means <- filter(abp_stages_mean, stage=="Baseline")
names(base_means)[names(base_means)=="mean(systolic_bp)"] <- "sys_base_mean"
names(base_means)[names(base_means)=="min(systolic_bp)"] <- "sys_base_min"
names(base_means)[names(base_means)=="max(systolic_bp)"] <- "sys_base_max"
names(base_means)[names(base_means)=="sd(systolic_bp)"] <- "sys_base_sd"
names(base_means)[names(base_means)=="length(systolic_bp)"] <- "sys_base_length"
names(base_means)[names(base_means)=="mean(diastolic_bp)"] <- "dia_base_mean"
names(base_means)[names(base_means)=="min(diastolic_bp)"] <- "dia_base_min"
names(base_means)[names(base_means)=="max(diastolic_bp)"] <- "dia_base_max"
names(base_means)[names(base_means)=="sd(diastolic_bp)"] <- "dia_base_sd"
names(base_means)[names(base_means)=="length(diastolic_bp)"] <- "dia_base_length"
names(base_means)[names(base_means)=="mean(mean_arterial_p)"] <- "mean_arterial_base"

#drop stage column
base_means <- base_means[,-3]

#filter our immed and rename columns                                                                                                                                                                                   
immed_means <- filter(abp_stages_mean, stage=="immed")
names(immed_means)[names(immed_means)=="mean(systolic_bp)"] <- "sys_immed_mean"
names(immed_means)[names(immed_means)=="min(systolic_bp)"] <- "sys_immed_min"
names(immed_means)[names(immed_means)=="max(systolic_bp)"] <- "sys_immed_max"
names(immed_means)[names(immed_means)=="sd(systolic_bp)"] <- "sys_immed_sd"
names(immed_means)[names(immed_means)=="length(systolic_bp)"] <- "sys_immed_length"
names(immed_means)[names(immed_means)=="mean(diastolic_bp)"] <- "dia_immed_mean"
names(immed_means)[names(immed_means)=="min(diastolic_bp)"] <- "dia_immed_min"
names(immed_means)[names(immed_means)=="max(diastolic_bp)"] <- "dia_immed_max"
names(immed_means)[names(immed_means)=="sd(diastolic_bp)"] <- "dia_immed_sd"
names(immed_means)[names(immed_means)=="length(diastolic_bp)"] <- "dia_immed_length"
names(immed_means)[names(immed_means)=="mean(mean_arterial_p)"] <- "mean_arterial_immed"
#drop stage column
immed_means <- immed_means[,-3]

#filter out post1 and rename columns                                                                                                                                                                                   
post1_means <- filter(abp_stages_mean, stage=="post1")
names(post1_means)[names(post1_means)=="mean(systolic_bp)"] <- "sys_post1_mean"
names(post1_means)[names(post1_means)=="min(systolic_bp)"] <- "sys_post1_min"
names(post1_means)[names(post1_means)=="max(systolic_bp)"] <- "sys_post1_max"
names(post1_means)[names(post1_means)=="sd(systolic_bp)"] <- "sys_post1_sd"
names(post1_means)[names(post1_means)=="length(systolic_bp)"] <- "sys_post1_length"
names(post1_means)[names(post1_means)=="mean(diastolic_bp)"] <- "dia_post1_mean"
names(post1_means)[names(post1_means)=="min(diastolic_bp)"] <- "dia_post1_min"
names(post1_means)[names(post1_means)=="max(diastolic_bp)"] <- "dia_post1_max"
names(post1_means)[names(post1_means)=="sd(diastolic_bp)"] <- "dia_post1_sd"
names(post1_means)[names(post1_means)=="length(diastolic_bp)"] <- "dia_post1_length"
names(post1_means)[names(post1_means)=="mean(mean_arterial_p)"] <- "mean_arterial_post1"
#drop stage column
post1_means <- post1_means[,-3]

#filter out post2 and rename columns                                                                                                                                                                                   
post2_means <- filter(abp_stages_mean, stage=="post2")
names(post2_means)[names(post2_means)=="mean(systolic_bp)"] <- "sys_post2_mean"
names(post2_means)[names(post2_means)=="min(systolic_bp)"] <- "sys_post2_min"
names(post2_means)[names(post2_means)=="max(systolic_bp)"] <- "sys_post2_max"
names(post2_means)[names(post2_means)=="sd(systolic_bp)"] <- "sys_post2_sd"
names(post2_means)[names(post2_means)=="length(systolic_bp)"] <- "sys_post2_length"
names(post2_means)[names(post2_means)=="mean(diastolic_bp)"] <- "dia_post2_mean"
names(post2_means)[names(post2_means)=="min(diastolic_bp)"] <- "dia_post2_min"
names(post2_means)[names(post2_means)=="max(diastolic_bp)"] <- "dia_post2_max"
names(post2_means)[names(post2_means)=="sd(diastolic_bp)"] <- "dia_post2_sd"
names(post2_means)[names(post2_means)=="length(diastolic_bp)"] <- "dia_post2_length"
names(post2_means)[names(post2_means)=="mean(mean_arterial_p)"] <- "mean_arterial_post2"
#drop stage column
post2_means <- post2_means[,-3]

#filter out post3 and rename columns                                                                                                                                                                                   
post3_means <- filter(abp_stages_mean, stage=="post3")
names(post3_means)[names(post3_means)=="mean(systolic_bp)"] <- "sys_post3_mean"
names(post3_means)[names(post3_means)=="min(systolic_bp)"] <- "sys_post3_min"
names(post3_means)[names(post3_means)=="max(systolic_bp)"] <- "sys_post3_max"
names(post3_means)[names(post3_means)=="sd(systolic_bp)"] <- "sys_post3_sd"
names(post3_means)[names(post3_means)=="length(systolic_bp)"] <- "sys_post3_length"
names(post3_means)[names(post3_means)=="mean(diastolic_bp)"] <- "dia_post3_mean"
names(post3_means)[names(post3_means)=="min(diastolic_bp)"] <- "dia_post3_min"
names(post3_means)[names(post3_means)=="max(diastolic_bp)"] <- "dia_post3_max"
names(post3_means)[names(post3_means)=="sd(diastolic_bp)"] <- "dia_post3_sd"
names(post3_means)[names(post3_means)=="length(diastolic_bp)"] <- "dia_post3_length"
names(post3_means)[names(post3_means)=="mean(mean_arterial_p)"] <- "mean_arterial_post3"
#drop stage column
post3_means <- post3_means[,-3]

#filter out post4 and rename columns                                                                                                                                                                                   
post4_means <- filter(abp_stages_mean, stage=="post4")
names(post4_means)[names(post4_means)=="mean(systolic_bp)"] <- "sys_post4_mean"
names(post4_means)[names(post4_means)=="min(systolic_bp)"] <- "sys_post4_min"
names(post4_means)[names(post4_means)=="max(systolic_bp)"] <- "sys_post4_max"
names(post4_means)[names(post4_means)=="sd(systolic_bp)"] <- "sys_post4_sd"
names(post4_means)[names(post4_means)=="length(systolic_bp)"] <- "sys_post4_length"
names(post4_means)[names(post4_means)=="mean(diastolic_bp)"] <- "dia_post4_mean"
names(post4_means)[names(post4_means)=="min(diastolic_bp)"] <- "dia_post4_min"
names(post4_means)[names(post4_means)=="max(diastolic_bp)"] <- "dia_post4_max"
names(post4_means)[names(post4_means)=="sd(diastolic_bp)"] <- "dia_post4_sd"
names(post4_means)[names(post4_means)=="length(diastolic_bp)"] <- "dia_post4_length"
names(post4_means)[names(post4_means)=="mean(mean_arterial_p)"] <- "mean_arterial_post4"
#drop stage column
post4_means <- post4_means[,-3]

#filter out eve_base and rename columns                                                                                                                                                                                   
Ebase_means <- filter(abp_stages_mean, stage=="Eve_Baseline")
names(Ebase_means)[names(Ebase_means)=="mean(systolic_bp)"] <- "sys_Ebase_mean"
names(Ebase_means)[names(Ebase_means)=="min(systolic_bp)"] <- "sys_Ebase_min"
names(Ebase_means)[names(Ebase_means)=="max(systolic_bp)"] <- "sys_Ebase_max"
names(Ebase_means)[names(Ebase_means)=="sd(systolic_bp)"] <- "sys_Ebase_sd"
names(Ebase_means)[names(Ebase_means)=="length(systolic_bp)"] <- "sys_Ebase_length"
names(Ebase_means)[names(Ebase_means)=="mean(diastolic_bp)"] <- "dia_Ebase_mean"
names(Ebase_means)[names(Ebase_means)=="min(diastolic_bp)"] <- "dia_Ebase_min"
names(Ebase_means)[names(Ebase_means)=="max(diastolic_bp)"] <- "dia_Ebase_max"
names(Ebase_means)[names(Ebase_means)=="sd(diastolic_bp)"] <- "dia_Ebase_sd"
names(Ebase_means)[names(Ebase_means)=="length(diastolic_bp)"] <- "dia_Ebase_length"
names(Ebase_means)[names(Ebase_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Ebase"
#drop stage column
Ebase_means <- Ebase_means[,-3]

#filter out eve_immed and rename columns                                                                                                                                                                                   
Eimmed_means <- filter(abp_stages_mean, stage=="Eve_immed")
names(Eimmed_means)[names(Eimmed_means)=="mean(systolic_bp)"] <- "sys_Eimmed_mean"
names(Eimmed_means)[names(Eimmed_means)=="min(systolic_bp)"] <- "sys_Eimmed_min"
names(Eimmed_means)[names(Eimmed_means)=="max(systolic_bp)"] <- "sys_Eimmed_max"
names(Eimmed_means)[names(Eimmed_means)=="sd(systolic_bp)"] <- "sys_Eimmed_sd"
names(Eimmed_means)[names(Eimmed_means)=="length(systolic_bp)"] <- "sys_Eimmed_length"
names(Eimmed_means)[names(Eimmed_means)=="mean(diastolic_bp)"] <- "dia_Eimmed_mean"
names(Eimmed_means)[names(Eimmed_means)=="min(diastolic_bp)"] <- "dia_Eimmed_min"
names(Eimmed_means)[names(Eimmed_means)=="max(diastolic_bp)"] <- "dia_Eimmed_max"
names(Eimmed_means)[names(Eimmed_means)=="sd(diastolic_bp)"] <- "dia_Eimmed_sd"
names(Eimmed_means)[names(Eimmed_means)=="length(diastolic_bp)"] <- "dia_Eimmed_length"
names(Eimmed_means)[names(Eimmed_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Eimmed"
#drop stage column
Eimmed_means <- Eimmed_means[,-3]

#filter out eve_p1 and rename columns                                                                                                                                                                                   
Epost1_means <- filter(abp_stages_mean, stage=="Eve_post1")
names(Epost1_means)[names(Epost1_means)=="mean(systolic_bp)"] <- "sys_Epost1_mean"
names(Epost1_means)[names(Epost1_means)=="min(systolic_bp)"] <- "sys_Epost1_min"
names(Epost1_means)[names(Epost1_means)=="max(systolic_bp)"] <- "sys_Epost1_max"
names(Epost1_means)[names(Epost1_means)=="sd(systolic_bp)"] <- "sys_Epost1_sd"
names(Epost1_means)[names(Epost1_means)=="length(systolic_bp)"] <- "sys_Epost1_length"
names(Epost1_means)[names(Epost1_means)=="mean(diastolic_bp)"] <- "dia_Epost1_mean"
names(Epost1_means)[names(Epost1_means)=="min(diastolic_bp)"] <- "dia_Epost1_min"
names(Epost1_means)[names(Epost1_means)=="max(diastolic_bp)"] <- "dia_Epost1_max"
names(Epost1_means)[names(Epost1_means)=="sd(diastolic_bp)"] <- "dia_Epost1_sd"
names(Epost1_means)[names(Epost1_means)=="length(diastolic_bp)"] <- "dia_Epost1_length"
names(Epost1_means)[names(Epost1_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Epost1"
#drop stage column
Epost1_means <- Epost1_means[,-3]

#filter out eve_p2 and rename columns                                                                                                                                                                                   
Epost2_means <- filter(abp_stages_mean, stage=="Eve_post2")
names(Epost2_means)[names(Epost2_means)=="mean(systolic_bp)"] <- "sys_Epost2_mean"
names(Epost2_means)[names(Epost2_means)=="min(systolic_bp)"] <- "sys_Epost2_min"
names(Epost2_means)[names(Epost2_means)=="max(systolic_bp)"] <- "sys_Epost2_max"
names(Epost2_means)[names(Epost2_means)=="sd(systolic_bp)"] <- "sys_Epost2_sd"
names(Epost2_means)[names(Epost2_means)=="length(systolic_bp)"] <- "sys_Epost2_length"
names(Epost2_means)[names(Epost2_means)=="mean(diastolic_bp)"] <- "dia_Epost2_mean"
names(Epost2_means)[names(Epost2_means)=="min(diastolic_bp)"] <- "dia_Epost2_min"
names(Epost2_means)[names(Epost2_means)=="max(diastolic_bp)"] <- "dia_Epost2_max"
names(Epost2_means)[names(Epost2_means)=="sd(diastolic_bp)"] <- "dia_Epost2_sd"
names(Epost2_means)[names(Epost2_means)=="length(diastolic_bp)"] <- "dia_Epost2_length"
names(Epost2_means)[names(Epost2_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Epost2"
#drop stage column
Epost2_means <- Epost2_means[,-3]

#filter out eve_p3 and rename columns                                                                                                                                                                                   
Epost3_means <- filter(abp_stages_mean, stage=="Eve_post3")
names(Epost3_means)[names(Epost3_means)=="mean(systolic_bp)"] <- "sys_Epost3_mean"
names(Epost3_means)[names(Epost3_means)=="min(systolic_bp)"] <- "sys_Epost3_min"
names(Epost3_means)[names(Epost3_means)=="max(systolic_bp)"] <- "sys_Epost3_max"
names(Epost3_means)[names(Epost3_means)=="sd(systolic_bp)"] <- "sys_Epost3_sd"
names(Epost3_means)[names(Epost3_means)=="length(systolic_bp)"] <- "sys_Epost3_length"
names(Epost3_means)[names(Epost3_means)=="mean(diastolic_bp)"] <- "dia_Epost3_mean"
names(Epost3_means)[names(Epost3_means)=="min(diastolic_bp)"] <- "dia_Epost3_min"
names(Epost3_means)[names(Epost3_means)=="max(diastolic_bp)"] <- "dia_Epost3_max"
names(Epost3_means)[names(Epost3_means)=="sd(diastolic_bp)"] <- "dia_Epost3_sd"
names(Epost3_means)[names(Epost3_means)=="length(diastolic_bp)"] <- "dia_Epost3_length"
names(Epost3_means)[names(Epost3_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Epost3"
#drop stage column
Epost3_means <- Epost3_means[,-3]

#filter out eve_p4 and rename columns                                                                                                                                                                                   
Epost4_means <- filter(abp_stages_mean, stage=="Eve_post4")
names(Epost4_means)[names(Epost4_means)=="mean(systolic_bp)"] <- "sys_Epost4_mean"
names(Epost4_means)[names(Epost4_means)=="min(systolic_bp)"] <- "sys_Epost4_min"
names(Epost4_means)[names(Epost4_means)=="max(systolic_bp)"] <- "sys_Epost4_max"
names(Epost4_means)[names(Epost4_means)=="sd(systolic_bp)"] <- "sys_Epost4_sd"
names(Epost4_means)[names(Epost4_means)=="length(systolic_bp)"] <- "sys_Epost4_length"
names(Epost4_means)[names(Epost4_means)=="mean(diastolic_bp)"] <- "dia_Epost4_mean"
names(Epost4_means)[names(Epost4_means)=="min(diastolic_bp)"] <- "dia_Epost4_min"
names(Epost4_means)[names(Epost4_means)=="max(diastolic_bp)"] <- "dia_Epost4_max"
names(Epost4_means)[names(Epost4_means)=="sd(diastolic_bp)"] <- "dia_Epost4_sd"
names(Epost4_means)[names(Epost4_means)=="length(diastolic_bp)"] <- "dia_Epost4_length"
names(Epost4_means)[names(Epost4_means)=="mean(mean_arterial_p)"] <- "mean_arterial_Epost4"
#drop stage column
Epost4_means <- Epost4_means[,-3]

#sleeptime data
#filter out sleep and rename columns                                                                                                                                                                                   
sleep_means <- filter(abp_stages_mean, stage=="sleep")
names(sleep_means)[names(sleep_means)=="mean(systolic_bp)"] <- "sys_sleep_mean"
names(sleep_means)[names(sleep_means)=="min(systolic_bp)"] <- "sys_sleep_min"
names(sleep_means)[names(sleep_means)=="max(systolic_bp)"] <- "sys_sleep_max"
names(sleep_means)[names(sleep_means)=="sd(systolic_bp)"] <- "sys_sleep_sd"
names(sleep_means)[names(sleep_means)=="length(systolic_bp)"] <- "sys_sleep_length"
names(sleep_means)[names(sleep_means)=="mean(diastolic_bp)"] <- "dia_sleep_mean"
names(sleep_means)[names(sleep_means)=="min(diastolic_bp)"] <- "dia_sleep_min"
names(sleep_means)[names(sleep_means)=="max(diastolic_bp)"] <- "dia_sleep_max"
names(sleep_means)[names(sleep_means)=="sd(diastolic_bp)"] <- "dia_sleep_sd"
names(sleep_means)[names(sleep_means)=="length(diastolic_bp)"] <- "dia_sleep_length"
names(sleep_means)[names(sleep_means)=="mean(mean_arterial_p)"] <- "mean_arterial_sleep"

#drop stage column
sleep_means <- sleep_means[,-3]

#all waketime data
wakesleep_mean <- abp_check_y3_read %>% group_by(subjectid, session_id, sleep_wake) %>% dplyr::summarise(length(systolic_bp), min(systolic_bp), mean(systolic_bp), max(systolic_bp), sd(systolic_bp), min(diastolic_bp), mean(diastolic_bp), max(diastolic_bp), sd(diastolic_bp), mean(mean_arterial_p))

#filter our baseline and rename columns                                                                                                                                                                                   
wake_means <- filter(wakesleep_mean, sleep_wake=="wake")
names(wake_means)[names(wake_means)=="mean(systolic_bp)"] <- "sys_wake_mean"
names(wake_means)[names(wake_means)=="min(systolic_bp)"] <- "sys_wake_min"
names(wake_means)[names(wake_means)=="max(systolic_bp)"] <- "sys_wake_max"
names(wake_means)[names(wake_means)=="sd(systolic_bp)"] <- "sys_wake_sd"
names(wake_means)[names(wake_means)=="length(systolic_bp)"] <- "sys_wake_length"
names(wake_means)[names(wake_means)=="mean(diastolic_bp)"] <- "dia_wake_mean"
names(wake_means)[names(wake_means)=="min(diastolic_bp)"] <- "dia_wake_min"
names(wake_means)[names(wake_means)=="max(diastolic_bp)"] <- "dia_wake_max"
names(wake_means)[names(wake_means)=="sd(diastolic_bp)"] <- "dia_wake_sd"
names(wake_means)[names(wake_means)=="length(diastolic_bp)"] <- "dia_wake_length"
names(wake_means)[names(wake_means)=="mean(mean_arterial_p)"] <- "mean_arterial_wake"

#drop stage column
wake_means <- wake_means[,-3]


#join dataframes to make one (wide) file
abp_wide_y3 <- full_join(base_means, immed_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, post1_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, post2_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, post3_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, post4_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Ebase_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Eimmed_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Epost1_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Epost2_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Epost3_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, Epost4_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, sleep_means, by= c("subjectid", "session_id"))
abp_wide_y3 <- full_join(abp_wide_y3, wake_means, by= c("subjectid", "session_id"))

############################


# table with sessions with a baseline readind

abp_base_y3_read <- filter(abp_check_y3_read, base_read == 1 | eve_base_read == 1)
ABP_summary_y3_base <- abp_base_y3_read %>% group_by(subjectid, session_id) %>% dplyr::summarise(subjectid[1], session_id[1], datetime[1], length(systolic_bp), 
                                                                                           min(systolic_bp), mean(systolic_bp), max(systolic_bp), sd(systolic_bp),
                                                                                           min(diastolic_bp), mean(diastolic_bp), max(diastolic_bp), sd(diastolic_bp), 
                                                                                           mean(mean_arterial_p), mean(heart_rate))


#####
# times series plot 
library(dplyr)
library(ggplot2)
abp_check_y3_read$time_short <- format(as.POSIXct(strptime(abp_check_y3_read$datetime, "%Y-%m-%d %H:%M", tz= "America/New_York")), format = "%H:%M")
#keeps staged in order
abp_check_y3_read$stage <- factor(abp_check_y3_read$stage, levels = c("Baseline", "immed", "post1","post2","post3","post4","Eve_Baseline", "Eve_immed", "Eve_post1","Eve_post2","Eve_post3","Eve_post4", "sleep"))

b2076 <- filter(abp_check_y3_read, subjectid == "BIKE2076")

ggplot(b2031s1_ts, aes(x= datetime, y=systolic_bp,  group= subjectid)) + geom_line() + geom_point() +
  xlab("Time") + ylab("Systolic BP (mmHg)") + ggtitle("Session 1: Systolic Blood Pressure") +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), title = element_text(size=18), axis.text.x = element_text(size=12, angle =90), axis.text.y = element_text(size=16), axis.title.x = element_text(size=18), axis.title.y = element_text(size = 18))

write.csv(b2076, "b2076.csv")
###########################

#sleeptime data
abp_wide_y3$changescore_sys <- (abp_wide_y3$sys_sleep_mean * abp_wide_y3$sys_wake_mean)/abp_wide_y3$sys_wake_mean
abp_wide_y3$dnratio_sys <- abp_wide_y3$sys_sleep_mean / abp_wide_y3$sys_wake_mean
#Nocturnal decline
abp_wide_y3$nocdecline <- ((abp_wide_y3$sys_wake_mean - abp_wide_y3$sys_sleep_mean)/abp_wide_y3$sys_wake_mean) * 100
#Dippers ; Non dippers if noturnal decline <= 10%
abp_wide_y3$dippers [abp_wide_y3$nocdecline <= 10] <- "Non-dipper"
abp_wide_y3$dippers [abp_wide_y3$nocdecline > 10] <- "Dipper"

abp_wide_y3_sleep <- select(abp_wide_y3, subjectid, session_id, changescore_sys, dnratio_sys, nocdecline, dippers)

write.csv(abp_wide_y3, "abp_wide_y3.csv")

