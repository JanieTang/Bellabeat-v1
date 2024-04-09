library('tidyverse')
library('janitor')
library('skimr')
library('lubridate')
library('here')
library('patchwork')

# create weekday & awake time
sleepday_cleaned <- sleepday_cleaned %>% 
  mutate(MinAwakeInBed = TotalTimeInBed- TotalMinutesAsleep, 
         DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE))


# sleepday_cleaned %>%
#   group_by(DayOfWeek) %>%
#   summarise(AvgAsleep = mean(TotalMinutesAsleep),
#             MedAsleep = median(TotalMinutesAsleep), 
#             SdAsleep = sd(TotalMinutesAsleep),
#             AvgAwake = mean(MinAwakeInBed), 
#             MedAwake = median(MinAwakeInBed), 
#             SdAwake = sd(MinAwakeInBed),
#             AvgInBed = mean(TotalTimeInBed), 
#             MedInBed = median(TotalTimeInBed), 
#             SdInBed = sd(TotalTimeInBed))


# sleepday_cleaned %>% 
#   select(-c(TotalSleepRecords, TotalTimeInBed)) %>% 
#   group_by(DayOfWeek) %>%
#   summarise(AvgAsleep = mean(TotalMinutesAsleep), AvgAwake = mean(MinAwakeInBed) ) %>% 
#   pivot_longer(cols = starts_with('Avg'), names_to = 'statistics', values_to = "stat_value") %>%
#   ggplot(aes(x = DayOfWeek, y = stat_value, fill = statistics)) + 
#   geom_bar(position = "stack", stat = 'identity')+labs(x = "day of week", y ="minutes/day")
# 
# ggsave("weekday_sleep.png", width = 7, height = 9)

 
dailyCalories_cleaned <- distinct(dailyCalories_) %>%
  mutate(DayOfWeek = wday(ActivityDay, label= TRUE, abbr = FALSE)) 

dailyCalories_cleaned %>%
  group_by(DayOfWeek) %>%
  summarise(AvgCal = mean(Calories)) %>%
  ggplot(aes(x = DayOfWeek, y = AvgCal)) + 
  geom_col(fill = "#F8AFA8", width  = 0.6, position = position_dodge(0.4)) +   
  geom_text(aes(label= round(AvgCal,0)), size = 4, vjust = -0.5) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        # Don't show the legend 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 10),
        axis.text.y = element_blank()) +
  labs(title = 'Average Calories Burned by the Days of the Week',x = NULL,y = 'Calories Burned') 

ggsave("weekday_calories.png", width = 6, height = 5)


#daily min portion of different activity level
dailyActMin <- dailyActMin %>% mutate(DayOfWeek = wday(ActivityDate, label= TRUE, abbr = FALSE)) 
dailyActMin %>% summarise(VeryActivePortion = sum(VeryActiveMinutes) / sum(TotalMin),
                          FairActivePortion = sum(FairlyActiveMinutes) / sum(TotalMin), 
                          LightActivePortion = sum(LightlyActiveMinutes) / sum(TotalMin),
                          SedPortion = sum(SedentaryMinutes)/ sum(TotalMin))

# day of week portion of different activity level
# activeness level is determine by Fitbit algorithm
dailyActMin %>% group_by(DayOfWeek) %>% 
  summarise(VeryPort = sum(VeryActiveMinutes) / sum(TotalMin),
  FairPort = sum(FairlyActiveMinutes) / sum(TotalMin), 
  LightPort = sum(LightlyActiveMinutes) / sum(TotalMin),
  SedPort = sum(SedentaryMinutes)/ sum(TotalMin),
  VeryMin = mean(VeryActiveMinutes) ,
  FairMin = mean(FairlyActiveMinutes) , 
  LightMin = mean(LightlyActiveMinutes),
  SedMin = mean(SedentaryMinutes))

# daily sleep minutes & average sleep min by day of week

sleepday_cleaned %>% summarise(avgSleep = mean(TotalMinutesAsleep))

sleepday_cleaned %>%  
  mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>%
  group_by(DayOfWeek) %>% 
  summarise(avg = mean(TotalMinutesAsleep), 
            avgBed = mean(TotalTimeInBed), 
            avgRecord = mean(TotalSleepRecords), 
            sleepEfficiency = sum(TotalMinutesAsleep) /sum(TotalTimeInBed), 
            midRecord = median(TotalSleepRecords))

# day of week more than one sleep distribution
sleepday_cleaned %>%  select(TotalSleepRecords,SleepDay) %>% 
  filter(TotalSleepRecords >1) %>%
  mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>% 
  group_by(DayOfWeek) %>% 
  summarise(MoreThan1 = n())
  
sleepday_cleaned %>% 
  mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>%
  select(TotalSleepRecords,DayOfWeek) %>%
  group_by(DayOfWeek) %>% summarise(recordsCount = n())

moreThan1Sleep <- full_join(sleepday_cleaned %>%  
                              select(TotalSleepRecords,SleepDay) %>% 
                              filter(TotalSleepRecords >1) %>%
                              mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>% 
                              group_by(DayOfWeek) %>% 
                              summarise(MoreThan1 = n()), 
                            sleepday_cleaned %>% 
                              mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>%
                              select(TotalSleepRecords,DayOfWeek) %>%
                              group_by(DayOfWeek) %>% summarise(recordsCount = n()),
                            by = "DayOfWeek") %>% mutate(moreThan1SleepPort = MoreThan1 / recordsCount )

sleepEfficiency_weekday <- full_join( moreThan1Sleep %>% 
                                        rename(DiscontinuousSleepPort = moreThan1SleepPort),
                                      sleepday_cleaned %>%  
                                        mutate(DayOfWeek = wday(SleepDay, label= TRUE, abbr = FALSE)) %>%
                                        group_by(DayOfWeek) %>% 
                                        summarise(AvgMinSleep = mean(TotalMinutesAsleep),
                                                  AvgMinInBed = mean(TotalTimeInBed),
                                                  SleepEfficiency = sum(TotalMinutesAsleep) /sum(TotalTimeInBed)), 
                                      by = 'DayOfWeek') %>% rename(MoreThan1Count = MoreThan1)

# weekday discontinuous sleep rate
sleepEfficiency_weekday %>% 
  ggplot(aes(x = DayOfWeek, y = DiscontinuousSleepPort)) + 
  geom_col(fill = "#C6CDF7", width  = 0.6, position = position_dodge(0.4)) +   
  geom_text(aes(label= paste(round(100*DiscontinuousSleepPort, 2), "%", sep="")), size = 4, vjust = -0.5) +
  # scale_fill_hue(c = 20) +  for less hue
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        # Don't show the legend 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 10),
        axis.text.y = element_blank()) +
  labs(title = 'Discontinuous Sleep Rate by the Days of the Week', x = NULL,y = 'Discontinueous Sleep Rate') 
ggsave("weekday_discontinueousSleep.png", width = 6, height = 4.5)



# weekday sleep efficiency rate
sleepEfficiency_weekday %>% 
  ggplot(aes(x = DayOfWeek, y = SleepEfficiency)) + 
  geom_col(fill = "#C6CDF7",width  = 0.6, position = position_dodge(0.2)) +   
  geom_text(aes(label= paste(round(100*SleepEfficiency, 2), "%", sep="")), size = 4, vjust = -0.5) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 10),
        axis.text.y = element_blank()) +
  labs(x = NULL,y = 'Sleep Efficiency (min asleep / min in bed)') 
ggsave("weekdaySleepEfficiency.png", width = 6, height = 4.5)

# weekday asleep min
aa <-hms::as_hms(round(sleepEfficiency_weekday$AvgMinSleep,0)*60)

sleepEfficiency_weekday %>% 
  ggplot(aes(x = DayOfWeek, y = AvgMinSleep)) + 
  geom_col(fill = "#C6CDF7",width  = 0.6, position = position_dodge(0.2)) +   
  geom_text(aes(label= aa), size = 4, vjust = -0.5) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 10),
        axis.text.y = element_blank()) +
  labs(title = 'Average Asleep Time by the Days of the Week', x = NULL, y = 'Asleep Time') 
ggsave("weekdayAsleepTime.png", width = 6, height = 5.5)


# average daily calories burned
dailyCalories_cleaned %>% summarise( avg =mean(Calories))
#  = 2304 calories


# activity level by hours
hourlyIntensity_cleaned <- hourlyIntensities_ %>% distinct()

Intensity_hourly <- 
  hourlyIntensity_cleaned %>% 
  mutate(hour_24 = hour(ActivityHour)) %>% 
  group_by(hour_24) %>%
  summarise(AvgHourlyInt = sum(TotalIntensity)/ (n()*60) ) 

Intensity_hourly %>% 
  ggplot(aes(x = hour_24, y = AvgHourlyInt)) +
  geom_col(fill = "#F8AFA8", width  = 0.6, position = position_dodge(0.4)) +   
  geom_text(aes(label= round(AvgHourlyInt,2), size = 4, vjust = -0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        # Don't show the legend 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 8),
        axis.text.y = element_blank()) +
  labs(x = NULL,y = 'Average Activity Intensity') 

Intensity_hourly %>% 
  ggplot(aes(x = hour_24, y = AvgHourlyInt)) + 
  geom_area(fill = "#F8AFA8", linetype = 1) +
  labs(title = 'Average Activity Intensity by Hour', x = 'Hour', y = 'Average Intensity') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  theme_classic()

  # theme(panel.background = element_rect(fill = "white"),
  #       axis.ticks.x = element_line(size = 0.5, color = 'gray', linetype = "dashed"), 
  #       axis.ticks.y = element_line(size = 0.5, color = 'gray', linetype = "dashed"),
  #       axis.text.x = element_text(vjust = 8, size = 0.5),
  #       axis.text.y = element_text(vjust = 8, size = 0.5)) +
  
ggsave("HourlyActivityIntensity.png", width = 6, height = 4.5)



# multi linear regression 
# put steps, total minutes of different levels of activeness in a day, and day of week as regressor
# to analyze what's the variables to explain sleep time

sleep_model <- lm(TotalMinutesAsleep ~ TotalSteps + 
              VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + 
              SedentaryMinutes + DayOfWeek , data = daily_Act_Sleep)
summary(sleep_model)

influence(sleep_model)
fitted(sleep_model)



# steps analysis
dailySteps_cleaned %>% summarize(avgSteps = mean(StepTotal), midSteps = median(StepTotal))

avg_steps_by_weekday 

dailySteps_cleaned %>% 
  group_by(DayOfWeek) %>% 
  summarise(dayAvgStep = mean(StepTotal)) %>%
  ggplot(aes(x = DayOfWeek, y = dayAvgStep)) +
  geom_col(fill = "#F8AFA8", width  = 0.6, position = position_dodge(0.4)) +   
  geom_text(aes(label= round(dayAvgStep,0), size = 4, vjust = -0.8, )) +
  labs(title = 'Average Steps by The Days of the Week', x = NULL,y = NULL) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none", 
        # Don't show the legend 
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 10),
        axis.text.y = element_blank())

ggsave("")


# calories and steps freq by days of week
Mon_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Monday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Monday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Tue_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Tuesday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Tuesday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Wed_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Wednesday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Wednesday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Thu_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Thursday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Thursday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Fri_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Friday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Friday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Sat_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Saturday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8 ) +
  labs(title = 'Saturday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

Sun_Cal_Distrb <-hourlyCalories_cleaned %>%
  filter(DayOfWeek == 'Sunday') %>%
  group_by(Hr)%>%
  summarise(avgCal = mean(Calories)) %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8" , size = 3)+
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Sunday', x = NULL, y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  theme_classic()

# for plots layout in a graph, use patchwork library
(Mon_Cal_Distrb|Tue_Cal_Distrb|Wed_Cal_Distrb|Thu_Cal_Distrb)/(Fri_Cal_Distrb|Sat_Cal_Distrb|Sun_Cal_Distrb)& scale_y_continuous(limits = c(60, 150))
ggsave("calDistrbOnHrInWkDy_big.png", height = 12, width = 15)

aaa <- hourlyCalories_cleaned %>%
  group_by(DayOfWeek,Hr) %>%
  summarise(avgCal = mean(Calories)) 
aaa %>%
  ggplot(aes(x = Hr, y = avgCal)) +
  geom_line(color = "#F8AFA8", size = 1.5, linetype = 6) +
  # geom_text(aes(label= round(avgCal,0)), size = 4, vjust = -0.8) +
  labs(title = 'Average Calories Burned over Hours', x = "Hour", y = 'Calories Burned') +
  scale_x_continuous(minor_breaks = seq(0, 23, by = 1)) +
  scale_y_continuous(minor_breaks = seq(60, 150, by = 30)) +
  # scale_color_manual(values = c("darkred","darkorange","yellow","darkgreen","steelblue","navy","purple"))+
  theme_classic()+
  facet_wrap(vars(DayOfWeek))
ggsave("calDistrbOnHrInWkDy.png", height = 12, width = 15)
