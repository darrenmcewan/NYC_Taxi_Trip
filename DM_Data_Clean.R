library(dplyr)
library(lubridate)
library(chron)
library(geosphere)
library(MLmetrics)
library(caret)
library(tidyverse)
library(bestglm)
library(plyr)


train <- read.csv(file="train.csv")
train <- train %>% select(-dropoff_datetime)
test <- read.csv(file="test.csv")
test$trip_duration <- NA
weather <- read.csv("weather.csv")

complete <- rbind(train,test)
attach(complete)

complete$pickup_datetime <- as.POSIXct(pickup_datetime, tz="EST", "%Y-%m-%d %H:%M:%S")

complete$date  = date(pickup_datetime)
complete$day  <- wday(pickup_datetime, label=TRUE)
complete$hour <-  hour(pickup_datetime)
complete$weekend <- is.weekend(complete$date)

weather$date <- date(weather$date)

complete <- merge(complete, weather)


complete <- complete %>% rowwise() %>% 
  mutate(distance = distHaversine(cbind(pickup_longitude, pickup_latitude), cbind(dropoff_longitude, dropoff_latitude)))

complete$rush_hour <- ifelse(complete$hour >= 7 & complete$hour <= 10 | complete$hour >= 4 & complete$hour <= 6, TRUE, FALSE)
complete$work = (complete$hour %in% seq(8,18)) & (complete$day %in% c("Mon","Tues","Wed","Thurs","Fri"))
complete$blizzard = !( (complete$date < ymd("2016-01-22") | (complete$date > ymd("2016-01-29"))) )



complete$pickup_long_group <- cut(complete$pickup_longitude, 3)
complete$pickup_lat_group = cut(complete$pickup_longitude, 3)
complete$dropoff_long_group = cut(complete$pickup_longitude, 3)
complete$dropoff_lat_group = cut(complete$pickup_longitude, 3)



cats <- select(complete,c("vendor_id","store_and_fwd_flag", "hour", "precipitation"))
for (f in colnames(cats)) {
  #cat("VARIABLE : ",f,"\n")
  levels <- unique(cats[[f]])
  cats[[f]] <- as.numeric(factor(cats[[f]], levels=levels))
}

complete <- cbind(complete, cats)
complete <- complete[ -c(9) ]



train <- complete %>% filter(!is.na(trip_duration))
test <- complete %>% filter(is.na(trip_duration))


myControl_model <- trainControl(method = "repeatedcv", 
                                number = 5,
                                repeats = 4,
                                classProbs = TRUE,
                                summaryFunction = defaultSummary)

#############################################
#xgb_model <- train(trip_duration~.-id,
#                   data = train, 
#                   method = "xgbTree",
#                   tuneLength = 2,
#                   metric = "F",
#                   trControl = myControl_model
#)
################################################


gbm <- train(trip_duration~.-id,
                       data = train, 
                       method = "gbm",
                       verbose = FALSE,
                       
)

preds <- predict(gbm, newdata = test)
preds.frame <- data.frame(id = test$id, trip_duration = preds)


write_csv(preds.frame, "gbm3.csv")




