library('tidyverse')
library('janitor')
library('skimr')
library('lubridate')
library('here')
#------- find all data ----------
data_list <- list.files(path="./Data", pattern=".csv", all.files=FALSE,full.names=FALSE)

#------- import dataset ---------
# minuteMETsNarrow_ <- read_csv("Data/minuteMETsNarrow_merged.csv", col_types = cols(ActivityMinute = col_datetime(format = "%m/%d/%Y %H:%M:%S %p")))
# View(minuteMETsNarrow_)

# for (i in data_list){
#   data_name_list<- append(data_name_list,stringr::str_extract(i,"^\\w\\w+_"))
# } 

# data description 
sleepDay_ %>% skimr::skim()
heartRateSeconds_ %>% skimr::skim()

dailyIntensities_ %>% dplyr::group_by(Id) %>% summarise(min())

# modify data name list by indexing and assign new value into list
data_name_list[[5]] <- "heartRateSeconds_"
data_name_list[[9]] <- NULL
data_name_list[[11]] <- NULL
data_name_list[[15]] <- NULL

#distinct ids in each df
dailyActivity_ %>% dplyr::distinct(Id) %>% arrange(Id) %>% print( n= 33)


# get all column names
str(dailyActivity_)
str(sleepDay_)

# daily minutes in sleep, sedentary, and different level of activeness
daily_Act_Sleep <- dailyActivity_ %>% 
  select(c('Id','TotalSteps', 'ActivityDate','VeryActiveMinutes','FairlyActiveMinutes','LightlyActiveMinutes','SedentaryMinutes')) %>% 
  left_join(sleepDay_, by = c('Id'='Id', 'ActivityDate'='SleepDay')) %>% 
  mutate(DayOfWeek = wday(ActivityDate,label = FALSE, week_start = 1))

# from the number of rows in dailyCalories_, dailyIntensity_, and dailySteps_
# we can found that they probrably share the same users, and the same date, but need inspect to verify our guess

dailyCalories_ %>% group_by(ActivityDay) %>% summarise(n(Id))
id_diff <- data.frame(act_id = c( arrange( distinct( dailyActivity_ , Id), desc(Id) ) ),
                      cal_id = c( arrange( distinct( dailyCalories_, Id), desc(Id)) ),
                      int_id = c( arrange( distinct( dailyIntensities_, Id), desc(Id) ) ),
                      ste_id = c( arrange( distinct( dailySteps_, Id), desc(Id) ) ) )
id_diff %>% mutate(chck = abs(Id-Id.1)+ abs(Id.2-Id.3)) %>% filter( chck != 0)
#-------result: ids are identical in above 4 tables---------------------------

date_diff <- tibble(day = arrange(distinct(select(dailyActivity_, ActivityDate)),ActivityDate),
                        act_count = count(dailyActivity_, ActivityDate),
                        cal_count = count(dailyCalories_, ActivityDay),
                        int_count = count(dailyIntensities_, ActivityDay),
                        stp_count = count(dailySteps_, ActivityDay))
dailyActivity_ %>% group_by(ActivityDate) %>% summarise(n_distinct(Id))

#-----finding 1: the number of participants who shares the data of daily activity, calories, steps and distances declinded day by day

sleepDay_ %>% count(SleepDay) %>% print(n = 31)
sleepDay_ %>% count(Id) %>% arrange(desc(n)) %>% print(n = 24)
sleepday_cleaned <- distinct(sleepDay_)
sleepday_cleaned %>% count(SleepDay) %>% print(n=31)
sleepday_cleaned %>% count(Id) %>% arrange(desc(n)) %>% print(n = 24)

daily_data_trend <- tibble(day = count(dailyActivity_, ActivityDate)$ActivityDate,
                           actCalStp_data_count = count(dailyActivity_, ActivityDate)$n,
                           slp_data_count = count(sleepday_cleaned, SleepDay)$n) %>% 
  mutate(DayOfWeek = lubridate::wday(day,label = TRUE, abbr = FALSE) )

daily_data_trend %>% group_by(day_of_week) %>% 
  summarise(avg_slp_by_wd = mean(slp_data_count)) %>% 
  ggplot(aes(day_of_week, avg_slp_by_wd)) + 
  geom_col() + 
  labs(x = "sleep data trend", y ="average of data points")
#------finding 2: Monday is the least sleep data reported day -------------------------



# exam distinct data of minuteSleep_

distinct(minuteSleep_) %>% group_by( date) %>% count( Id ) %>% arrange( Id )
# after inspection, the timestamps are not shared among users

distinct(minuteSleep_) %>% 
  filter( Id == 1503960366) %>% 
  group_by( date(date), Id ) %>% count( value ) %>% 
  summarise(totalMin = sum(n)) %>% summarise(s = sum(totalMin))

summary_of_minuteSleep <- minuteSleep_%>%  group_by( date(date), Id ) %>% 
  count( value ) %>% summarise(totalMin = sum(n)) 

summary_of_minuteSleep <- summary_of_minuteSleep %>% rename(day_of_ms = 'date(date)')
s_o_ms_wider <- summary_of_minuteSleep %>% 
  pivot_wider(names_from = day_of_ms, values_from = totalMin, names_sort = TRUE) %>% 
  arrange(Id)
s_o_ds_wider <- distinct(sleepDay_) %>% 
  select(Id, SleepDay, TotalTimeInBed) %>% 
  pivot_wider(names_from = SleepDay, values_from = TotalTimeInBed, names_sort = TRUE) %>% 
  arrange(Id)

# ------- finding 3: totalTimeInBed in sleepDay_ table is not corresponding to sum of minutes in minuteSleep_ ---------------

#---------------------finding sleep trends over sleepDay_
# i. exam data if not only one record under one date and Id: if it passed, then use it with less doubt, otherwise, sum them up 
#     CONCLUSION: NO NEED to check, R sum it up for me

sleepday_cleaned <- distinct(sleepDay_)

# exam table statistics
sleepday_cleaned %>% skimr::skim()
  # avg of min asleep = 419, avg total time in bed = 458, avg min of in bed but awake = 39.3;



# temp <- daily_minutes_spend %>% 
#   select( -c(TotalSteps,VeryActiveDistance,ModeratelyActiveDistance,Calories,TotalSleepRecords,TotalMinutesAsleep) ) %>%
#   rowwise(ActivityDate) %>%
#   mutate( TotalMin = sum(c_across(VeryActiveMinutes:TotalTimeInBed)), na.rm = TRUE ) 

# the combination across tables is garbage

# remove(temp, daily_minutes_spend)

# finding the data count trends by bar chart
daily_data_trend %>% 
  pivot_longer(col = c(actCalStp_data_count,slp_data_count), names_to = 'catagory', values_to = 'counts') %>% 
  group_by(DayOfWeek, catagory) %>% 
  summarise(avgCounts = mean(counts)) %>%
  ggplot(aes(x = DayOfWeek, y = avgCounts, fill = catagory)) + 
  geom_bar(position = 'dodge',stat = 'identity') + 
  geom_text( aes(label = avgCounts, vjust = -0.5), position = position_dodge(width = 0.9)) +
  scale_fill_manual(labels = c('activity data', 'sleep data'), values = c("#F8AFA8","#C6CDF7")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 8, size = 12)) +
  labs(x = NULL,y = "Average Data Counts") 
ggsave("DataPointsTrends.png",width = 9, height = 7)






#data counts in daily activity and daily sleep
daily_data_trend %>% summarise(avgRespSleep = mean(slp_data_count), 
                               avgRespAct = mean(actCalStp_data_count))

daily_data_trend %>%
  pivot_longer(col = c(actCalStp_data_count,slp_data_count), names_to = 'catagory', values_to = 'counts') %>% 
  ggplot(aes(x = day, y = counts, fill = catagory)) + 
  geom_bar(position = 'dodge',stat = 'identity') + 
  geom_text( aes(label = counts, vjust = -0.5), position = position_dodge(width = 0.9)) +
  scale_fill_manual(labels = c('activity data', 'sleep data'), values = c("#F9AFA9","#C9CDF9")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust =3),
        axis.title.y = element_text(margin = margin(r = -30)),
        legend.position = c(0.93, 0.9))+
  labs(x = NULL,y = "Data Counts") 

ggsave("DailyDataPointsTrends.png",width = 12, height = 6)

day_seq = seq(ymd('20170412'),ymd('2017-05-12'),'days')

daily_data_trend %>%
  pivot_longer(col = c(actCalStp_data_count,slp_data_count), names_to = 'catagory', values_to = 'counts') %>% 
  ggplot(aes(x = day, y = counts, color = catagory)) + 
  geom_line() + 
  geom_text( aes(label = counts, vjust = -1.5, hjust = 0.5), position = 'identity') +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 days ") + 
  scale_discrete_manual(aes(day)) +
  scale_color_manual(labels = c('activity data', 'sleep data'), values = c("#F8665E","#7492F7")) +
  # theme_minimal()+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = c(0.93, 0.9),
        panel.grid.major = element_line(colour = 'gray', size =0.25, linetype = 2),
        panel.grid.minor = element_line(colour = 'gray', size =0.25, linetype = 2),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust =3),
        axis.title.y = element_text(margin = margin(r = 2)))+
  labs(x = NULL,y = "Data Counts") 

ggsave("DailyDataPointsTrendsbyLine.png",width = 12, height = 7.5)








# trends on Activity level and minutes
temp <- dailyActivity_ %>% 
  select(Id, ActivityDate,VeryActiveMinutes:SedentaryMinutes) %>% 
  mutate(d_id = str_c(Id,ActivityDate)) %>% rowwise(d_id) %>% 
  mutate(sum  = sum(c(VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes,SedentaryMinutes)))

temp %>% filter(sum > 1440)
# check the reliability of dailyActivity_ table
# great! no row has more than 1440 min of activities in a day

temp <- temp %>% ungroup()
# to cancel the row-wise calculation and return to column based calsulation

dailyActMin <- temp %>% select(Id:SedentaryMinutes,sum) %>% rename(TotalMin = 'sum')
remove(temp)

# hourly data points
# 1. intensity
hourlyIntensity_cleaned <- hourlyIntensities_ %>% distinct()

hourlyIntensity_cleaned %>% 
  mutate(hour_24 = hour(ActivityHour)) %>% 
  group_by(hour_24) %>%
  summarise(n()) %>% print( n = 24)

# 2. calories

distinct(hourlyCalories_) %>% 
  mutate(hour_24 = hour(ActivityHour)) %>% 
  group_by(hour_24) %>%
  summarise(n()) %>% print( n = 24)

# 3. steps
distinct(hourlySteps_) %>%
  mutate(hour_24 = hour(ActivityHour)) %>% 
  group_by(hour_24) %>%
  summarise(n()) %>% print( n = 24) 

hourlyData <- distinct(hourlySteps_) %>%
  mutate(hour_24 = hour(ActivityHour)) %>% 
  group_by(hour_24) %>%
  summarise(n()) %>% rename(dataCount = "n()")

hourlyData %>%
  ggplot(aes(x = hour_24, y = dataCount)) + 
  geom_col(fill = '#479289') + 
  geom_text( aes(label = dataCount, vjust = -0.5), position = position_dodge(width = 0.3)) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 6),
        axis.title.y = element_text(margin = margin(r = -30)))+
  labs(x = NULL,y = "Data Counts") 


# clean dailySteps

dailySteps_cleaned <- distinct(dailySteps_) %>% 
  mutate(DayOfWeek = wday(ActivityDay,label = TRUE, abbr = FALSE))


# clean minute_calories to hourly calories with days of week
hourlyCalories_cleaned <- distinct(minuteCaloriesWide_) %>%
  rowwise()%>%
  mutate(HourlyCalories = sum(c_across(Calories00:Calories59))) %>%
  select("Id" ,"ActivityHour","HourlyCalories") %>%
  mutate(DayOfWeek = wday(ActivityHour, label= TRUE, abbr = FALSE))
    # ----------!! the cleaned minutes -> hourly is different from originally given hourly_calories data!!!???? ------------ 
minToHrCalories_cleanned <- hourlyCalories_cleaned
# I decided to change name because original hourly data has more information

hourlyCalories_cleaned <- distinct(hourlyCalories_) %>% 
  mutate(DayOfWeek = wday(ActivityHour, label= TRUE, abbr = FALSE), 
         Hr = hour(ActivityHour))


# clean hourly steps with day of week
hourlySteps_cleaned <- hourlySteps_ %>% distinct(hourlySteps_) %>%
  mutate(DayOfWeek = wday(ActivityHour, label= TRUE, abbr = FALSE))