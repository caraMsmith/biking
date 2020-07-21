#Phase2 biking maps
#2019 riding season

#Set up subject files to create maps and plot. These are the maps sent to the participants
#These are maps created based on second gps data
# Author: Cara
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
#load file with cleaned hexoskin , bc, and PM2.5 data. 
#Files resides on labarchieves Biking Potential Inhaled Dose/Cleaned Files/pm_bc_hex
pmbchex_y5 <- read.csv("phase2_2019_dataset_as_of_20200131.csv", stringsAsFactors = FALSE)
#load gps file. GPS file obtained directly from database and at 1 second intervals.
# File resides on labarchieves Biking Potential Inhaled Dose/GPS/ GPS files
gps_raw_y5 <- read.csv("gps_y5_sec.csv", stringsAsFactors = FALSE)

#timestamps were off from database
gps_raw_y5$datetime1 <- ymd_hms(gps_raw_y5$datetime)
#gps_raw_y5$datetime2 <- force_tz(gps_raw_y5$datetime2, tz="UTC")
#attributes(gps_raw_y5$datetime2)$tzone <- "America/New_York"  

#match time variables

gps_raw_y5$date <- date(gps_raw_y5$datetime1)
gps_raw_y5$h <- hour(gps_raw_y5$datetime1)
gps_raw_y5$m <- minute(gps_raw_y5$datetime1)

pmbchex_y5$date <- date(pmbchex_y5$datetime)
pmbchex_y5$h <- hour(pmbchex_y5$datetime)
pmbchex_y5$m <- minute(pmbchex_y5$datetime)

#change gps sujectid to subject
names(gps_raw_y5)[names(gps_raw_y5)=="subjectid"] <- "subject"

#join data
pmbc_gps_sec_y5 <- full_join(gps_raw_y5, pmbchex_y5, by=c("subject", "date","h", "m"))

# Create cateorgies for BC data
# bc in ug/m3 

pmbc_gps_sec_y5$BC_cat [pmbc_gps_sec_y5$bc.clean <1 ] <- "<1"
pmbc_gps_sec_y5$BC_cat [pmbc_gps_sec_y5$bc.clean >=1 & pmbc_gps_sec_y5$bc.clean <=2] <- "1-2"
pmbc_gps_sec_y5$BC_cat [pmbc_gps_sec_y5$bc.clean >2 & pmbc_gps_sec_y5$bc.clean <=4] <- "2-4"
pmbc_gps_sec_y5$BC_cat [pmbc_gps_sec_y5$bc.clean >=4] <- ">4"

# bc dose in ng/min

pmbc_gps_sec_y5$BCdose_cat [ pmbc_gps_sec_y5$bc.dose < 40] <- "0-40"
pmbc_gps_sec_y5$BCdose_cat [pmbc_gps_sec_y5$bc.dose >= 40 & pmbc_gps_sec_y5$bc.dose < 100] <- "40-100"
pmbc_gps_sec_y5$BCdose_cat [pmbc_gps_sec_y5$bc.dose >= 100 & pmbc_gps_sec_y5$bc.dose < 200] <- "100-200"
pmbc_gps_sec_y5$BCdose_cat [pmbc_gps_sec_y5$bc.dose > 200] <- ">200"

#Create catorgories for minute ventilation data
pmbc_gps_sec_y5$MV_cat [pmbc_gps_sec_y5$ve.clean < 35] <- "3-35"
pmbc_gps_sec_y5$MV_cat [pmbc_gps_sec_y5$ve.clean >= 35 & pmbc_gps_sec_y5$ve.clean < 50] <- "35-50"
pmbc_gps_sec_y5$MV_cat [pmbc_gps_sec_y5$ve.clean >= 50 & pmbc_gps_sec_y5$ve.clean < 70] <- "50-70"
pmbc_gps_sec_y5$MV_cat [pmbc_gps_sec_y5$ve.clean >= 70 & pmbc_gps_sec_y5$ve.clean < 170 ] <- "70-170"


# cats for PM data
#PM in ug/m3 

pmbc_gps_sec_y5$PM_cat [pmbc_gps_sec_y5$pm.clean < 10] <- "<10"
pmbc_gps_sec_y5$PM_cat [pmbc_gps_sec_y5$pm.clean >= 10 & pmbc_gps_sec_y5$pm.clean <= 25] <- "10-25"
pmbc_gps_sec_y5$PM_cat [pmbc_gps_sec_y5$pm.clean > 25 & pmbc_gps_sec_y5$pm.clean <= 50] <- "25-50"
pmbc_gps_sec_y5$PM_cat [pmbc_gps_sec_y5$pm.clean > 50] <- ">50"

# pm dose ng/min cat 

pmbc_gps_sec_y5$PMdose_cat [pmbc_gps_sec_y5$pm.dose < 200] <- "<200"
pmbc_gps_sec_y5$PMdose_cat [pmbc_gps_sec_y5$pm.dose >= 200 & pmbc_gps_sec_y5$pm.dose < 350] <- "200-350"
pmbc_gps_sec_y5$PMdose_cat [pmbc_gps_sec_y5$pm.dose >= 350 & pmbc_gps_sec_y5$pm.dose < 500] <- "350-500"
pmbc_gps_sec_y5$PMdose_cat [pmbc_gps_sec_y5$pm.dose > 400] <- ">500"

# morning and evening rides. For mapping simpliciy, using 13:00 as the divide
pmbc_gps_sec_y5$tod [pmbc_gps_sec_y5$h < 13] <-"Morning"
pmbc_gps_sec_y5$tod [pmbc_gps_sec_y5$h >= 13] <-"Evening"
pmbc_gps_sec_y5$tod <- factor(pmbc_gps_sec_y5$tod, levels = c("Morning", "Evening"))

# create subset of data for a single participant pmbc_gps_sec_y5
bike2076_sec <- subset(pmbc_gps_sec_y5, subject== "BIKE2076")


#create sessions if subject did not already have their sessions marked (only pertains to a few subjects)
bike1034_sec$session [bike1034_sec$datetime.y >= "2017-01-28 05:12:00" & bike1034_sec$datetime.y <= "2017-01-29 05:11:00" ] <- 1
bike1034_sec$session [bike1034_sec$datetime.y >= "2017-02-01 05:12:00" & bike1034_sec$datetime.y <= "2017-02-02 15:12:00" ] <- 2
bike1034_sec$session [bike1034_sec$datetime.y >= "2017-02-06 05:12:00" & bike1034_sec$datetime.y <= "2017-02-07 15:12:00" ] <- 3
bike1034_sec$session [bike1034_sec$datetime.y >= "2017-02-16 05:12:00" & bike1034_sec$datetime.y <= "2017-02-17 15:12:00" ] <- 4
bike1034_sec$session [bike1034_sec$datetime.y >= "2017-02-28 05:12:00" & bike1034_sec$datetime.y <= "2017-03-01 15:12:00" ] <- 5

#create a day variable for labeling during facet_wrap 
bike2076_sec$day [bike2076_sec$session == "1"] <-"12/3/19"
bike2076_sec$day [bike2076_sec$session == "2"] <-"12/10/19"
bike2076_sec$day [bike2076_sec$session == "3"] <-"12/12/19"
bike2076_sec$day [bike2076_sec$session == "4"] <-"12/18/19"
bike2076_sec$day [bike2076_sec$session == "5"] <-"12/24/19"
bike2076_sec$day [bike2076_sec$session == "6"] <-"12/26/19"

#subset a session to test map
bike1035_sec_s2 <- subset(bike1035_sec, session== "2")
bike1035_sec_s5 <- subset(bike1035_sec, session== "5")

#test map to adjust zoom and location
qmap("Rambling House, Bronx, NY", zoom = 11, color= "color", legend = "right") + 
  geom_point(data = bike1035_sec_s2, aes(longitude, latitude, color=BC_cat), size =2) + facet_wrap(tod ~day) +
  scale_color_manual(name = "Black Carbon (ug/m^3)" , values = c("#00FF00","#CC0000", "#FFFF00", "#FF6600"), breaks= c("<1", "1-2", "2-4", ">4")) +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), strip.text.x = element_text(size=16), strip.text.y = element_text(size = 16)) 

qmap("Rambling House, Bronx, NY", zoom = 11, color= "color", legend = "right") + 
  geom_point(data = bike1035_sec_s2, aes(longitude, latitude, color=PM_cat), size =2) + facet_wrap(tod ~day) +
  scale_color_manual(name = "PM2.5 (ug/m^3)" , values = c("#00FF00","#FFFF00", "#FF6600","#CC0000"), breaks= c("<10", "10-25", "25-50", ">50")) +
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 18), strip.text.x = element_text(size=16), strip.text.y = element_text(size = 16)) 



#write individual file for easier read into the R markdown file.
write.csv(bike2076_sec, "bike2076_sec.csv")



#timeseries 

bike2076_plot <- subset(pmbchex_y5, subject== "BIKE2076")

#create a day variable for labeling during facet_wrap 
bike2076_plot$day [bike2076_plot$session == "1"] <-"12/3/19"
bike2076_plot$day [bike2076_plot$session == "2"] <-"12/10/19"
bike2076_plot$day [bike2076_plot$session == "3"] <-"12/12/19"
bike2076_plot$day [bike2076_plot$session == "4"] <-"12/18/19"
bike2076_plot$day [bike2076_plot$session == "5"] <-"12/24/19"
bike2076_plot$day [bike2076_plot$session == "6"] <-"11/26/19"

bike2076_plot$TimeA <- as.POSIXct(bike2076_plot$datetime)

write.csv(bike2076_plot, "bike2076_plot.csv")


#subset a session to test map
bike1035_sec_s2 <- subset(bike1035_sec, session== "2")
bike1035_sec_s5 <- subset(bike1035_sec, session== "5")


bike1056_sec_s1 <- subset(bike1056_plot, session== "1")

bike1056_sec_s1$TimeA <- as.POSIXct(bike1056_sec_s1$datetime)

ggplot(bike1056_sec_s1, aes(x= TimeA , y= bc.clean, col = biking)) + geom_point() + 
  xlab("Time of Day") + ylab("Black Carbon (ug/m^3)") + ggtitle("Black Carbon: Session 1") + 
  theme(legend.position = "none", axis.text.x=element_text(size=18), axis.text.y= element_text(size=18), axis.title.x= element_text(size=18), axis.title.y= element_text(size=18), title= element_text(size=18))
