library(tidyverse)
library(ggplot2)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(datasauRus)
library(lubridate)
library(readr)
library(readxl)
library(data.table)
library(tidyr)
library(metR)

daily_activity <- read_csv("dailyactivity_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_intensities <- read_csv("hourlyintensities_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")


view(daily_activity)


daily_activity %>% group_by(ActivityDay) %>% summarise(sum(TotalSteps),mean(TotalSteps),max(TotalSteps),min(TotalSteps))
daily_activity %>% group_by(ActivityDay) %>% summarise(sum(TotalDistance),mean(TotalDistance),max(TotalDistance),min(TotalDistance))
daily_activity %>% group_by(ActivityDay) %>% summarise(sum(Calories),mean(Calories),max(Calories),min(Calories))
daily_activity %>% group_by(ActivityDay) %>% summarise(sum(TotalSteps),sum(TotalDistance),sum(Calories))

daily_activity$Day <- ordered(daily_activity$ActivityDay, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
daily_activity %>% group_by(Day) %>% summarise(sum(TotalSteps),sum(TotalDistance),sum(Calories))

view(sleep_day)

sleep_day <- sleep_day %>% mutate(Totalhoursasleep = TotalMinutesAsleep/60, Totaltimeinbedhours = TotalTimeInBed/60, wasted_bedtime_min = TotalTimeInBed-TotalMinutesAsleep)
sleep_day %>% group_by(Day) %>% summarise(sum(TotalTimeInBed), sum(TotalMinutesAsleep), sum(wasted_bedtime_min), sum(Totalhoursasleep), sum(Totaltimeinbedhours))
sleep_day %>% group_by(Day) %>% summarise(mean(TotalTimeInBed), mean(TotalMinutesAsleep), mean(wasted_bedtime_min), mean(Totalhoursasleep), mean(Totaltimeinbedhours))

unique(daily_activity$Id)
unique(sleep_day$Id)

daily_activity <- daily_activity %>% rename(Days =ActivityDay)
sleep_day <- sleep_day %>% rename(Days = Day)

daily_activity_sleep <- daily_activity %>% inner_join(sleep_day, by="Id","Days")
view(daily_activity_sleep)

daily_activity_sleep %>% group_by(Days.x) %>% summarise(sum(Calories), sum(Totalhoursasleep), sum(TotalDistance))
daily_activity_sleep$Days.x <- ordered(daily_activity_sleep$Days.x, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
daily_activity_sleep$Days.y <- ordered(daily_activity_sleep$Days.y, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

daily_activity_sleep %>% group_by(Days.x) %>% summarise(mean(Calories), mean(Totalhoursasleep), mean(TotalDistance))

