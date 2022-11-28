#RAPSODO
#Scraping Data
#MLB
#AllStar Game

###

#Graphs the launch angle and the bat speed for 5 All starts baseball player that work with Rapsodo
#Ronald Acuna
#Nolan Arenado
#Aaron Judge 
#Shohei Ohtani
#Juan Soto
# Those playes has 20+ Hr by season

#Using Savant stats 


library(baseballr)
library(tidyverse)
library(reshape2)
library(zoo)
library(ggplot2)
library(ggpubr)


### Braves###
RA= playerid_lookup('Acuna','Ronald')%>%
  select(mlbam_id, first_name,last_name)
RA21= scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                    batterid =RA$mlbam_id)

RA20= scrape_statcast_savant_batter(start_date = '2020-04-01', end_date = '2020-09-27',
                                    batterid =RA$mlbam_id)

RA19= scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                    batterid =RA$mlbam_id)
RA18= scrape_statcast_savant_batter(start_date = '2018-03-01', end_date = '2018-10-01',
                                    batterid =RA$mlbam_id)
RA= rbind(RA21,RA20,RA19,RA18)
RA

help(rm)
rm(RA21,RA20,RA19,RA18)  

RA2= RA %>% 
  filter(type=='X') %>% #pitches in play , where the ball is in play 
  filter(launch_speed !=0)

ra_avg_yr= RA2 %>%
  group_by(game_year) %>%
  summarise(wOBACON = round(sum(woba_value,na.rm = TRUE)/sum(woba_denom,na.rm = TRUE),3),
            LS= round(mean(launch_speed,na.rm= TRUE),1))

ra_group= RA2 %>%
  group_by(game_date)%>%
  summarise(`Average Launch Angle`= mean(launch_angle,na.rm= TRUE),
            `Average Batted Ball Speed`=mean(launch_speed, na.rm= TRUE)) %>%
  ungroup() %>%
  melt(id=c('game_date')) %>%
  mutate(Year= as.factor(substr(game_date,1,4)))


ra_group %>%
  ggplot(aes(game_date,value))+
  geom_point()+
  stat_smooth(aes(group = Year, color= Year))+
  facet_wrap(~variable, scales = "free_y",nrow= 2)+
  ggtitle('\nRonald Acuna JR: 2018-2021\n')+
  labs(subtitle = "\nAcuna JR increased his average batted ball speed from 90.8 MPH in 2018 to 93.8 MPH in 2021.
Exit Speed Average 88.4\n ")+
  xlab('\nDate')+theme(axis.line.x.bottom=element_line(size=1))+
  ylab(' Speed = MPH           Angle = Degress')


#### Cardinals####
NAR= playerid_lookup('Arenado','Nolan')%>%
  select(mlbam_id, first_name,last_name)
NAR21= scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                    batterid =NAR$mlbam_id)

NAR20= scrape_statcast_savant_batter(start_date = '2020-04-01', end_date = '2020-09-27',
                                    batterid =NAR$mlbam_id)

NAR19= scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                    batterid =NAR$mlbam_id)
NAR18= scrape_statcast_savant_batter(start_date = '2018-03-01', end_date = '2018-10-01',
                                    batterid =NAR$mlbam_id)
NAR= rbind(NAR21,NAR20,NAR19,NAR18)
NAR

help(rm)
rm(NAR21,NAR20,NAR19,NAR18)  

NAR2= NAR %>% 
  filter(type=='X') %>% #pitches in play , where the ball is in play 
  filter(launch_speed !=0)

nar_avg_yr= NAR2 %>%
  group_by(game_year) %>%
  summarise(wOBACON = round(sum(woba_value,na.rm = TRUE)/sum(woba_denom,na.rm = TRUE),3),
            LS= round(mean(launch_speed,na.rm= TRUE),1))

nar_group= NAR2 %>%
  group_by(game_date)%>%
  summarise(`Average Launch Angle`= mean(launch_angle,na.rm= TRUE),
            `Average Batted Ball Speed`=mean(launch_speed, na.rm= TRUE)) %>%
  ungroup() %>%
  melt(id=c('game_date')) %>%
  mutate(Year= as.factor(substr(game_date,1,4)))


nar_group %>%
  ggplot(aes(game_date,value))+
  geom_point()+
  stat_smooth(aes(group = Year, color= Year))+
  facet_wrap(~variable, scales = "free_y",nrow= 2)+
  ggtitle('\nNolan Arenado: 2018-2021\n')+
  labs(subtitle = "\nNolan Arenado kept  his batted ball speed between 89.8 MPH in 2018 to 89 MPH in 2021. 
Exit Velocity Average is 88.4\n ")+
  xlab('\nDate')+theme(axis.line.x.bottom=element_line(size=1))+
  ylab('Speed = MPH           Angle = Degress')


### Yankees##

AR= playerid_lookup('Judge','Aaron')%>%
  select(mlbam_id, first_name,last_name)
AR21= scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                     batterid =AR$mlbam_id)

AR20= scrape_statcast_savant_batter(start_date = '2020-04-01', end_date = '2020-09-27',
                                     batterid =AR$mlbam_id)

AR19= scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                     batterid =AR$mlbam_id)
AR18= scrape_statcast_savant_batter(start_date = '2018-03-01', end_date = '2018-10-01',
                                     batterid =AR$mlbam_id)
AR= rbind(AR21,AR20,AR19,AR18)
AR

help(rm)
rm(AR21,AR20,AR19,AR18)  

AR2= AR %>% 
  filter(type=='X') %>% #pitches in play , where the ball is in play 
  filter(launch_speed !=0)

ar_avg_yr= AR2 %>%
  group_by(game_year) %>%
  summarise(wOBACON = round(sum(woba_value,na.rm = TRUE)/sum(woba_denom,na.rm = TRUE),3),
            LS= round(mean(launch_speed,na.rm= TRUE),1))

ar_group= AR2 %>%
  group_by(game_date)%>%
  summarise(`Average Launch Angle`= mean(launch_angle,na.rm= TRUE),
            `Average Batted Ball Speed`=mean(launch_speed, na.rm= TRUE)) %>%
  ungroup() %>%
  melt(id=c('game_date')) %>%
  mutate(Year= as.factor(substr(game_date,1,4)))


ar_group %>%
  ggplot(aes(game_date,value))+
  geom_point()+
  stat_smooth(aes(group = Year, color= Year))+
  facet_wrap(~variable, scales = "free_y",nrow= 2)+
  ggtitle('\n Aaron Judge: 2018-2021\n')+
  labs(subtitle = "\nAaron Judge increased his average batted ball speed from 94.7 MPH in 2018 to 95.8 in 2021 MPH  
Average of 94.57 MPH\n")+
  xlab('\nDate')+theme(axis.line.x.bottom=element_line(size=1))+
  ylab('Speed = MPH             Angle = Degress')

### Angels ###

OT= playerid_lookup('Ohtani','Shohei')%>%
  select(mlbam_id, first_name,last_name)
OT21= scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                    batterid =OT$mlbam_id)

OT20= scrape_statcast_savant_batter(start_date = '2020-04-01', end_date = '2020-09-27',
                                    batterid =OT$mlbam_id)

OT19= scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                    batterid =OT$mlbam_id)
OT18= scrape_statcast_savant_batter(start_date = '2018-03-01', end_date = '2018-10-01',
                                    batterid =OT$mlbam_id)
OT= rbind(OT21,OT20,OT19,OT18)
OT

help(rm)
rm(OT21,OT20,OT19,OT18)  

OT2= OT %>% 
  filter(type=='X') %>% #pitches in play , where the ball is in play 
  filter(launch_speed !=0)

ot_avg_yr= OT2 %>%
  group_by(game_year) %>%
  summarise(wOBACON = round(sum(woba_value,na.rm = TRUE)/sum(woba_denom,na.rm = TRUE),3),
            Exit_Speed= round(mean(launch_speed,na.rm= TRUE),1))

ot_group= OT2 %>%
  group_by(game_date)%>%
  summarise(`Average Launch Angle`= mean(launch_angle,na.rm= TRUE),
            `Average Batted  Exit Speed`=mean(launch_speed, na.rm= TRUE)) %>%
  ungroup() %>%
  melt(id=c('game_date')) %>%
  mutate(Year= as.factor(substr(game_date,1,4)))


ot_group %>%
  ggplot(aes(game_date,value))+
  geom_point()+
  stat_smooth(aes(group = Year, color= Year))+
  facet_wrap(~variable, scales = "free_y",nrow= 2)+
  ggtitle('\n Shohei Ohtani: 2018-2021\n')+labs(subtitle = "\nShohei Ohtani kept his batted ball speed between 92.9 MPH in 2018 to 93.6 MPH in 2021.
Exit Speed Average\n ")+
  xlab('\nDate')+theme(axis.line.x.bottom=element_line(size=1))+
  ylab(' Speed = MPH           Angle = Degress')

  

## Vladimir Guerreo ##

VG= playerid_lookup('Guerrero','Vladimir')%>%
  select(mlbam_id, first_name,last_name)
VG=VG[1,]
VG

VG21= scrape_statcast_savant_batter(start_date = '2021-04-01', end_date = '2021-10-03',
                                    batterid =VG$mlbam_id)
VG22= scrape_statcast_savant_batter(start_date = '2022-04-01', end_date = '2022-07-019',
                                    batterid =VG$mlbam_id)

VG20= scrape_statcast_savant_batter(start_date = '2020-04-01', end_date = '2020-09-27',
                                    batterid =VG$mlbam_id)

VG19= scrape_statcast_savant_batter(start_date = '2019-03-01', end_date = '2019-10-01',
                                    batterid =VG$mlbam_id)
VG= rbind(VG21,VG20,VG19,VG22)
VG

help(rm)
rm(VG21,VG20,VG19,VG22)  

VG2= VG %>% 
  filter(type=='X') %>% #pitches in play , where the ball is in play 
  filter(launch_speed !=0)

vg_avg_yr= VG2 %>%
  group_by(game_year) %>%
  summarise(wOBACON = round(sum(woba_value,na.rm = TRUE)/sum(woba_denom,na.rm = TRUE),3),
            LS= round(mean(launch_speed,na.rm= TRUE),1))

vg_group= VG2 %>%
  group_by(game_date)%>%
  summarise(`Average Launch Angle`= mean(launch_angle,na.rm= TRUE),
            `Average Batted Exit Speed`=mean(launch_speed, na.rm= TRUE)) %>%
  ungroup() %>%
  melt(id=c('game_date')) %>%
  mutate(Year= as.factor(substr(game_date,1,4)))


vg_group %>%
  ggplot(aes(game_date,value))+
  geom_point()+
  stat_smooth(aes(group = Year, color= Year))+
  facet_wrap(~variable, scales = "free_y",nrow= 2)+
  ggtitle('\nRonald Acuna JR: 2018-2021\n')+
  labs(subtitle = "\nAcuna JR increased his average batted ball speed from 90.8 MPH in 2018 to 93.8 MPH\n ")+
  xlab('\nDate')+theme(axis.line.x.bottom=element_line(size=1))+
  ylab(' Speed = MPH           Angle = Degress')




#########################



library(tidyverse)
library(baseballr)
guerrerojr <- scrape_statcast_savant(start_date = "2022-03-01", 
                                 end_date = "2022-07-20", 
                                 playerid = 665489,
                                 player_type = "batter")

## ----hc------------------------------------------------------------------
guerrerojr %>%
  select(events, hc_x, hc_y) %>%
  head()

## ----Guerrero_bip----------------------------------------------------------
guerrerojr_bip <- guerrerojr %>%
  filter(type == "X")
guerrerojr_bip %>%
  select(events, hc_x, hc_y) %>%
  head()

## ------------------------------------------------------------------------
# ggspraychart(data = correa, fill_value = "events")
infield_chart<- function(...) {
  ggplot(...) + 
    geom_curve(x = 33, xend = 223, y = -100, yend = -100,
               curvature = -.65) +
    geom_segment(x = 128, xend = 33, y = -208, yend = -100) +
    geom_segment(x = 128, xend = 223, y = -208, yend = -100) +
    geom_curve(x = 83, xend = 173, y = -155, yend = -156,
               curvature = -.70, linetype = "dotted") +
    coord_fixed() + 
    scale_x_continuous(NULL, limits = c(25, 225)) + 
    scale_y_continuous(NULL, limits = c(-225, -25)) +ggtitle('a')
}

## ----spray_chart, warning=FALSE, fig.cap="Spray chart of balls in play hit by Carlos Correa in May, 2017."----
infield_chart(guerrerojr_bip, aes(x = hc_x, y = -hc_y, color = events)) + 
  geom_point() + 
  scale_color_discrete()+ggtitle('Vladimir Guerrero JR tendency to hit the Ball')+
  labs(subtitle = "\n Vladimir Guerrero JR, 2022 Season \n ")


#####

# install.packages("statcast")
# library(statcastr)
# db <- src_mysql_cnf("statcast")
# sc <- etl("statcastr", db, dir = "~/dumps/statcastr")
# sc %>%
#   etl_extract(year = 2017, month = 3:10) %>%
#   etl_transform() %>%
#   etl_load(tablenames = "statcast")
# 
# sc_2017 <- sc %>%
#   tbl("statcast") %>%
#   collect()
# 
# write_csv(sc_2017, path = "data/statcast2017.csv")



library(baseballr)
library(readr)
s1 <- scrape_statcast_savant_batter_all("2021-04-02", 
                                        "2021-04-08")
s2 <- scrape_statcast_savant_batter_all("2021-04-09", 
                                        "2021-04-15")
s3 <- scrape_statcast_savant_batter_all("2021-04-16", 
                                        "2021-04-22")
s4 <- scrape_statcast_savant_batter_all("2021-04-23", 
                                        "2021-04-29")
s5 <- scrape_statcast_savant_batter_all("2021-04-30", 
                                        "2021-05-06")
s6 <- scrape_statcast_savant_batter_all("2021-05-07", 
                                        "2021-05-13")
s7 <- scrape_statcast_savant_batter_all("2021-05-14", 
                                        "2021-05-20")
s8 <- scrape_statcast_savant_batter_all("2021-05-21", 
                                        "2021-05-27")
s9 <- scrape_statcast_savant_batter_all("2021-05-28", 
                                        "2021-06-03")
s10 <- scrape_statcast_savant_batter_all("2021-06-04", 
                                         "2021-06-10")
s11 <- scrape_statcast_savant_batter_all("2021-06-11", 
                                         "2021-06-17")
s12 <- scrape_statcast_savant_batter_all("2021-06-18", 
                                         "2021-06-24")
s13 <- scrape_statcast_savant_batter_all("2021-06-25", 
                                         "2021-07-01")
s14 <- scrape_statcast_savant_batter_all("2021-07-02", 
                                         "2021-07-08")
s15 <- scrape_statcast_savant_batter_all("2021-07-09", 
                                         "2021-07-15")
s16 <- scrape_statcast_savant_batter_all("2021-07-16", 
                                         "2021-07-22")
s17 <- scrape_statcast_savant_batter_all("2021-07-23", 
                                         "2021-07-29")
s18 <- scrape_statcast_savant_batter_all("2021-07-30", 
                                         "2021-08-05")
s19 <- scrape_statcast_savant_batter_all("2021-08-06", 
                                         "2021-08-12")
s20 <- scrape_statcast_savant_batter_all("2021-08-13", 
                                         "2021-08-19")
s21 <- scrape_statcast_savant_batter_all("2021-08-20", 
                                         "2021-08-26")
s22 <- scrape_statcast_savant_batter_all("2021-08-27", 
                                         "2021-09-02")
s23 <- scrape_statcast_savant_batter_all("2021-09-03", 
                                         "2021-09-09")
s24 <- scrape_statcast_savant_batter_all("2021-09-10", 
                                         "2021-09-16")
s25 <- scrape_statcast_savant_batter_all("2021-09-17", 
                                         "2021-09-23")
s26 <- scrape_statcast_savant_batter_all("2021-09-24", 
                                         "2021-09-30")
s27 <- scrape_statcast_savant_batter_all("2021-10-01", 
                                         "2021-11-01")
sc1 <- rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,
             s11, s12, s13, s14)
sc2 <- rbind(s15, s16, s17, s18, s19, s20, s21,
             s22, s23, s24, s25, s26, s27)
sc_all <- rbind(sc1, sc2)

write_csv(sc_all, "statcast2021.csv")


sc_bips= sc_all %>%
  filter(type == 'X')

### Scatter Plot of Launch Angle vs Exit Velocity

## Launch Angle and Speed Velocity##
guidelines = tibble(
  launch_angle = c(10,25,50),
  launch_speed= 40,
  label= c("Ground Balls", 'Lines Drives', "Flyballs"))

ev_plot = sc_bips 
crcblue <- "#2905a1"
install.packages("RColorBrewer")

library(RColorBrewer)
ev_plot <- sc_bips %>%
 sample_n(nrow(.) / 2) %>%
  ggplot(aes(x = launch_speed, y = launch_angle, 
             color = estimated_ba_using_speedangle)) + 
  geom_hline(data = guidelines, aes(yintercept = launch_angle), 
             color = "black", linetype = 2) + 
  geom_text(data = guidelines, 
            aes(label = label, y = launch_angle - 4), 
            color = "black", hjust = "left") + 
  geom_point(alpha = 0.05) +
  scale_color_gradient("BA", low = crcblue, high = "white") +
  scale_x_continuous("Exit velocity (mph)", 
                     limits = c(40, 120)) + 
  scale_y_continuous("Launch angle (degrees)", 
                     breaks = seq(-75, 75, 25))
ev_plot+ggtitle(' Launch Angle vs Exit Velocity')+ labs(subtitle = "\n\n ")
ev_plot + facet_grid(p_throws ~ stand)

### Latin american Players at their Debut###
library(readr)
Master <- read_csv("C:/Users/camil/OneDrive/Desktop/R Projects/2022/Analyzing Baseball with R/2016/baseballdatabank-master/core/Master.csv")
View(Master)

# Subsetting the Players Table for the Top 5 Latin American countries with more MLB Players
colnames(Players)
Players=Master
Players_LA = Players[Players$birthCountry == "Venezuela" | Players$birthCountry == "D.R." | Players$birthCountry == "P.R." | Players$birthCountry == "Mexico" | Players$birthCountry == "Cuba", Players$birthCountry == "Colombia", ]

# Remove rows for 6 players who his debut date doesn't appears (some LA managers)
Players_LA = Players_LA[!is.na(Players_LA$debut), ]

# Converting format of dates & concatenating Birthdate
install.packages('ymd')
library(ymd)

Players_LA$debut = ymd(Players_LA$debut)
Players_LA$Birthdate = as.Date(ISOdatetime(Players_LA$birthYear, Players_LA$birthMonth, Players_LA$birthDay, min=0, hour = 0, sec=0))

# Calculating Age of Player at debut
library(lubridate)
library(ggplot2)
library(ggthemes)

Players_LA$AgeAtDebut = as.integer(Players_LA$debut - Players_LA$Birthdate)/365
meanAge=mean(Players_LA$AgeAtDebut)
h_line= 23.926
ggplot(data=Players_LA, aes(x=debut, y=AgeAtDebut)) +
  geom_point(alpha=0.2, size=4) +
  geom_smooth(aes(color=birthCountry), se = FALSE, size=1.5, n = 200) +
  ggtitle("Age of Latin American Players at their debut")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  theme_economist()+scale_color_few()+
  xlab("Year of Debut") + ylab("Age")+geom_hline(yintercept = h_line, linetype='dashed', color= 'red')+
  labs(subtitle = "\nFor the 5 Countries , Colombia has the lowest Age in the MLB Debut
Avegare Debut Age is 24\n ")
  

max(Players_LA$AgeAtDebut)

Players_LA[Players_LA$AgeAtDebut== 41.64932,]

mean(Players_LA$AgeAtDebut)

structure(Players)

Players[Players$birthCountry]

unique(Players$birthCountry)

countries =Players %>% group_by(birthCountry) %>% summarize(number_rows=n())
countries
length(unique(Players$birthCountry)) 


write.csv(Players_LA, 'Players_df1.csv')

getwd()

