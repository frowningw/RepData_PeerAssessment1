#1. Code for reading in the dataset and/or processing the data
setwd("~/Desktop")
library(dplyr)
library(lubridate)
act_file <- read.csv("activity.csv")
act_file$date <- ymd(act_file$date)
act_file_clean <- act_file[complete.cases(act_file),]

#v2. Histogram of the total number of steps taken each day
sub1 <- group_by(act_file_clean[,1:2], date) %>% 
  summarize(totalsteps = sum(steps))
png("plot1.png", width = 480, height = 480)
with(sub1, barplot(totalsteps))
dev.off()

#v3. Mean and median number of steps taken each day
sub2<- group_by(act_file_clean[,1:2], date) %>% 
  summarize(mean = mean(steps), median = median(steps))

#v4. Time series plot of the average number of steps taken
png("plot2.png", width = 480, height = 480)
with(sub2, plot(date,mean,type="l"))
dev.off()

#v5. The 5-minute interval that, on average, contains the maximum number of steps
sub3 <- group_by(act_file_clean, interval) %>% 
  summarize(steps=max(steps)) 
maxinterval <- sub3 %>% 
  filter(steps==max(sub3$steps)) %>% 
  select(interval)

#6. Code to describe and show a strategy for imputing missing data
  #replace NA with 0
nafile <- act_file[!complete.cases(act_file),]
nafile$steps <- 0
act_imputing <- rbind(nafile, act_file_clean)

#7. Histogram of the total number of steps taken each day after 
#   missing values are imputed (redo 2 and 3)
  #7.1 total steps with the imputed
sub7 <- group_by(act_imputing[,1:2], date) %>% 
  summarize(totalsteps = sum(steps))
png("plot3.png", width = 480, height = 480)
with(sub7, barplot(totalsteps))
dev.off()

  #7.2 the mean and median with the imputed
sub7_2<- group_by(act_imputing[,1:2], date) %>% 
  summarize(mean = mean(steps), median = median(steps))

#v8. Panel plot comparing the average number of steps taken 
#   per 5-minute interval across weekdays and weekends
library(lattice)
act_file_clean$wday <- wday(act_file_clean$date)
act_file_clean<-act_file_clean %>% 
  mutate(wd = wday>=6)
sub8 <- act_file_clean[,c(1,3,5)]
wkdy <- sub8 %>% 
  filter(wd=="FALSE") %>%
  select(steps, interval) %>% 
  group_by(interval) %>% 
  summarize(steps=mean(steps)) %>%
  mutate(id = "weekday")
wked <- sub8 %>% 
  filter(wd=="TRUE") %>%
  select(steps, interval) %>% 
  group_by(interval) %>% 
  summarize(steps=mean(steps)) %>% 
  mutate(id = "weekend")
sub8 <- rbind(wkdy, wked)
png("plot4.png", width = 480, height = 480)
xyplot(steps ~ interval | id, data = sub8, layout = c(1, 2), type= "l")
dev.off()
