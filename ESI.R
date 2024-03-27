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
# Doing the same code, but for ambassador families
fitbit_amba <- read_csv("/Users/wala/Documents/data2/Fitbit_Interday_Ambassadors.csv")

fitbit_2 <- fitbit_amba %>%
  group_by(identifier) %>%
  mutate(Minutes_Moderatley_Active = activities_minutesfairlyactive + activities_minutesveryactive) %>%
  summarise(
    Daily_Steps_Mean = mean(activities_steps),
    Minutes_Sedentary_Mean = mean(activities_minutessedentary),
    Minutes_Moderatley_Active_Mean = mean(Minutes_Moderatley_Active), Hours_Sedentary = mean(activities_minutessedentary) / 60 
  )
  
# Combining the ambassadors and others together into one df
combined_data<-rbind(fitbit_2,fitbit_agger)

# Making the data to long format for better analysis 

combined_data_long <- pivot_longer(merged_data, cols = -identifier, names_to = "variable",values_to = "value")

# Adding sex and DOB 
merged_data <- merge(combined_data, DOB_Gen_032224, by = "identifier")

# DOB and sex in long format
merged_data_long <- merged_data %>%
  pivot_longer(
    cols = c("Daily_Steps_Mean", "Minutes_Sedentary_Mean", "Minutes_Moderatley_Active_Mean", "Hours_Sedentary", "Sex", "Age"),
    names_to = "Variable",
    values_to = "Value"
  )

# View the long format data
View(merged_data_long)