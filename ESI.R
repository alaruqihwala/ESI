library(tidyverse)
fitbit_interday<-read_csv("data2/Fitbit_Interday_Oct2023-March2024.csv")
tail(fitbit_interday)                         
head(fitbit_interday)
ncol(fitbit_interday)
nrow(fitbit_interday)
colnames(fitbit_interday)

# Doing aggregate the code that works the best 
fitbit_agger <- fitbit_interday %>%
  group_by(identifier) %>%
  mutate(Minutes_Moderatley_Active = activities_minutesfairlyactive + activities_minutesveryactive) %>%
  summarise(
    Daily_Steps_Mean = mean(activities_steps),
    Minutes_Sedentary_Mean = mean(activities_minutessedentary),
    Minutes_Moderatley_Active_Mean = mean(Minutes_Moderatley_Active)
  )

# Parent code that has minutes and hours for sedentary to make it more readable 
fitbit_agger <- fitbit_interday %>%
  group_by(identifier) %>%
  mutate(Minutes_Moderatley_Active = activities_minutesfairlyactive + activities_minutesveryactive) %>%
  summarise(
    Daily_Steps_Mean = mean(activities_steps),
    Minutes_Sedentary_Mean = mean(activities_minutessedentary),
    Minutes_Moderatley_Active_Mean = mean(Minutes_Moderatley_Active), Hours_Sedentary = mean(activities_minutessedentary) / 60 
  )

# Duration parent code 
fitbit_agger <- fitbit_interday %>%
  group_by(identifier) %>%
  summarise(
    Days_Collected = n_distinct(as.Date(date)),
    Daily_Steps_Mean = mean(activities_steps),
    Minutes_Sedentary_Mean = mean(activities_minutessedentary),
    Hours_Sedentary_Mean = mean(activities_minutessedentary / 60),
    Minutes_Moderatley_Active_Mean = mean(activities_minutesfairlyactive + activities_minutesveryactive)
  )


