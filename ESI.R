library(tidyverse)
fitbit_interday<-read_csv("data2/Fitbit_Interday_Oct2023-March2024.csv")
tail(fitbit_interday)                         
head(fitbit_interday)
ncol(fitbit_interday)
nrow(fitbit_interday)
colnames(fitbit_interday)
fitbit_interday %>% select(activities_minuteslightlyactive,	activities_minutesfairlyactive,
                           activities_minutesveryactive,	activities_activitycalories) %>% summary()

