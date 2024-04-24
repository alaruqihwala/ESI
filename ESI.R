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
fitbit_agger2 <- fitbit_interday %>%
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
    cols = c("Daily_Steps_Mean", "Minutes_Sedentary_Mean", "Minutes_Moderately_Active_Mean", "Hours_Sedentary", "Sex", "Age", "Hours_Active"),
    names_to = "Variable",
    values_to = "Value"
  )

# View the long format data
View(merged_data_long)
# Adding hours for readability 
merged_data$Hours_Active <- merged_data$Minutes_Moderatley_Active_Mean / 60


# One sample t-test for sedentary 
hours_sedentary <- merged_data$Hours_Sedentary

# Perform one-sample t-test with significance level set to 0.05
t_test_result <- t.test(minutes_active, mu = 6, alternative = "two.sided", conf.level = 0.95)


# Violin plot 

violin_plot <- ggplot(merged_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_violin(scale = "width") +
  labs(x = "Variable", y = "Value", title = "Comparison of Minutes Moderately Active and Minutes Sedentary") +
  theme_minimal()

# Show the violin plot
print(violin_plot)


# Boxplot 

filtered_data <- merged_data_long %>%
  filter(Variable %in% c("Hours_Active", "Hours_Sedentary"))

# Create a box plot using filtered_data
boxplot <- ggplot(filtered_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Value", title = "Comparison of Hours Active and Hours Sedentary") +
  theme_minimal()

# Show the box plot
print(boxplot)

library(ggplot2)

# Your existing code
p <- ggplot(data = data, aes(x = label, y = value)) +
  geom_bar(stat = "identity", fill = "#0492C2") + 
  geom_text(aes(label = value), hjust = 0.5, color = "black", size = 15, position = position_stack(vjust = 0.5)) +  # Center text labels on bars
  labs(title = "Comparing Groups's Daily Moderate Physical Activity to Recommended Daily Guideline",
       x = "Category",
       y = "Minutes") +
  coord_flip() +  # Flip the plot horizontally
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title.x = element_text(size = 27, color = "black"),  # Font size and color for x-axis title
    axis.title.y = element_text(size = 27, color = "black")   # Font size and color for y-axis title
  )

# Adjusting font size and color of y-axis labels ("label" column in your data frame)
print(p + theme(axis.text.y = element_text(size = 27, color = "black")))


p <- ggplot(data = data, aes(x = label, y = value)) +
  geom_bar(stat = "identity", fill = "#0492C2", width = 0.5) +  # Adjust width here
  geom_text(aes(label = value), hjust = -0.2, color = "black", size = 4) +
  labs(title = "Comparing Groups's Daily Moderate Physical Activity to Recommended Daily Guideline",
       x = "Category",
       y = "Minutes") +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

print(p)

histogram_plot <- ggplot(merged_data, aes(x = Minutes_Moderatley_Active_Mean)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Histogram of Minutes Moderately Active Mean",
       x = "Minutes Moderately Active Mean",
       y = "Frequency") +
  theme_minimal() 


histogram_plot <- ggplot(merged_data, aes(x = Minutes_Moderatley_Active_Mean)) +
  geom_histogram(binwidth = 2, fill = "#0492C2", color = "black") +
  labs(title = "Histogram of Minutes Moderately Active Mean",
       x = "Minutes Moderately Active Mean",
       y = "Frequency") +
  theme_classic() + theme(panel.grid.major = element_blank() + panel.grid.minor = element_blank())


# Add a vertical line at 21.4
histogram_plot +
  geom_vline(xintercept = 21.4, linetype = "dashed", color = "red")


# Histo 
histogram_plot <- ggplot(merged_data, aes(x = Minutes_Moderatley_Active_Mean)) +
  geom_histogram(binwidth = 2, fill = "#00CC66", color = "black") +
  labs(title = "Histogram of Minutes Moderately Active Mean",
       x = "Minutes Moderately Active Mean",
       y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"))  # Adjusted theme

# Add a vertical line at 21.4
histogram_plot +
  geom_vline(xintercept = 21.4, linetype = "dashed", color = "black")

# PIE CHART THAT ACTUALLY WORKS 
library(ggplot2)

# Your existing code with the label "Category" in the legend
merged_data <- merged_data %>%
  mutate(Category = case_when(
    Minutes_Moderatley_Active_Mean > 21.4 ~ "Over 21.4 Minutes",
    TRUE ~ "Under 21.4 Minutes"
  ))

summary_counts <- merged_data %>%
  group_by(Category) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

custom_colors <- c("#b9e192", "#029356")

pie_chart <- ggplot(summary_counts, aes(x = "", y = Percentage, fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = custom_colors) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of People Over and Under The 21.4 Minute Daily Moderate Activity Guideline",
       fill = "Category",  # Change "Status" to "Category" in the legend
       x = NULL,
       y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 17)  # Adjust font size here

# Adjusting font sizes and changing legend label
pie_chart +
  theme(
    legend.text = element_text(size = 25),  # Font size for legend
    legend.title = element_text(size = 25),  # Font size for legend title
    plot.title = element_text(size = 20),  # Font size for plot title
    axis.text = element_blank(),  # Remove axis text
    axis.title = element_blank(),  # Remove axis titles
    plot.margin = margin(1, 1, 1, 1, "cm")  # Adjust plot margin if needed
  )


# Histo
histogram_plot <- ggplot(merged_data, aes(x = Minutes_Sedentary_Mean) +
  geom_histogram(binwidth = 0.5, fill = "Blue", color = "black")) +
  labs(title = "Histogram of Minutes Moderately Active Mean",
       x = "Minutes Moderately Active Mean",
       y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"))  # Adjusted theme
histogram_plot


histogram_plot <- ggplot(merged_data, aes(x = Hours_Sedentary)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Minutes Sedentary Mean",
       x = "Minutes Sedentary Mean",
       y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"))  # Adjusted theme
histogram_plot




summary_counts <- merged_data %>%
  group_by(Status) %>%
  summarize(Count = n())

# Calculate percentages
summary_counts <- summary_counts %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Create pie chart with black and grey colors
pie_chart <- ggplot(summary_counts, aes(x = "", y = Percentage, fill = Statu)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Percentage of People Over and Under The 21.4 Minute Daily Moderate Activity Guideline",
       fill = "Status",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("Over 21.4 Minutes" = "#6BCAE2", "Under 21.4 Minutes" = "#005A9C")) +  # Set colors
  theme_void()

# Print the pie chart
print(pie_chart)




# Load the ggplot2 library
library(ggplot2)

# Create a boxplot using ggplot
ggplot(data = combined_data, aes(x = "", y = Minutes_Sedentary_Mean)) +
  geom_boxplot(fill = "skyblue", color = "blue") +
  labs(x = NULL, y = "Minutes Sedentary Mean") +
  ggtitle("Boxplot of Minutes Sedentary Mean")

# Load the ggplot2 library
library(ggplot2)

library(tidyr)

# Assuming combined_data is your wide-format data frame
long_data <- combined_data %>%
  pivot_longer(
    cols = starts_with("your_prefix_for_wide_columns_"), # Adjust the prefix
    names_to = "variable",
    values_to = "value"
  )

# Load necessary library
library(ggplot2)

# Assuming merged_data_long is your data frame

# Calculate scaling factor for Minutes_Sedentary_Mean to adjust for its larger values
scaling_factor <- max(merged_data_long$Value[merged_data_long$Variable == "Minutes_Sedentary_Mean"]) / 
  max(merged_data_long$Value[merged_data_long$Variable == "Minutes_Moderately_Active_Mean"])

# Create a new column for scaled Minutes_Sedentary_Mean values
merged_data_long$Scaled_Sedentary <- merged_data_long$Value
merged_data_long$Scaled_Sedentary[merged_data_long$Variable == "Minutes_Sedentary_Mean"] <- 
  merged_data_long$Value[merged_data_long$Variable == "Minutes_Sedentary_Mean"] / scaling_factor

# Create boxplot using ggplot
ggplot(merged_data_long, aes(x = Variable, y = Scaled_Sedentary)) +
  geom_boxplot(fill = "lightblue") +
  geom_text(aes(label = Value), vjust = -0.5, color = "black") +
  labs(title = "Boxplot of Minutes Sedentary (Scaled) vs Minutes Moderately Active",
       y = "Scaled Values",
       x = "Variables") +
  theme_minimal()
# Assuming you have already loaded your data frame merged_data_long

# Assuming you have already loaded your data frame merged_data_long

library(ggplot2)

# Filter the data frame
filtered_data <- merged_data_long %>%
  filter(Variable %in% c("Minutes_Sedentary_Mean", "Minutes_Moderatley_Active_Mean"))

# Create the plot with log transformation
ggplot(filtered_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +  # Boxplot to compare distribution
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for individual data points
  scale_y_log10() +  # Log transformation for y-axis
  labs(title = "Comparison of Sedentary and Moderately Active Minutes",
       x = "Variable", y = "Value (Log Scale)") +
  theme_minimal()

library(ggplot2)

# Filter the data frame
filtered_data <- merged_data_long %>%
  filter(Variable %in% c("Minutes_Sedentary_Mean", "Minutes_Moderatley_Active_Mean"))

# Create the plot with a modified log transformation
ggplot(filtered_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +  # Boxplot to compare distribution
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for individual data points
  scale_y_continuous(trans = 'log1p', labels = scales::comma) +  # Modified log transformation
  labs(title = "Comparison of Sedentary and Moderately Active Minutes",
       x = "Variable", y = "Value (Log Scale)") +
  theme_minimal()
# BAR GRAPHH
library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Filter the data frame
filtered_data <- merged_data_long %>%
  filter(Variable %in% c("Minutes_Sedentary_Mean", "Minutes_Moderatley_Active_Mean"))

# Calculate mean values
mean_values <- filtered_data %>%
  group_by(Variable) %>%
  summarize(mean_value = mean(Value))

# Font size and color settings
label_font_size <- 9  # Change to desired size for data labels
x_axis_label_font_size <- 15  # Change to desired size for x-axis labels
y_axis_label_font_size <- 15  # Change to desired size for y-axis labels
legend_font_size <- 12  # Change to desired size for legend text
legend_title_font_size <- 14  # Change to desired size for legend title

# Define custom colors (replace these with your desired hex codes)
custom_colors <- c("#FF5733", "#33FF57")

# Create the bar plot with custom colors
ggplot(mean_values, aes(x = Variable, y = mean_value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar plot
  geom_text(aes(label = round(mean_value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, color = "black", size = label_font_size) +  # Add text labels with adjusted font size and color
  labs(title = "Mean Values of Sedentary and Moderately Active Minutes",
       x = "Variable", y = "Mean Value") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_classic() +  # Use classic theme
  theme(text = element_text(color = "black", size = label_font_size),  # Adjust font color and size globally
        axis.text.x = element_text(color = "black", size = x_axis_label_font_size),  # Adjust x-axis labels font color and size
        axis.text.y = element_text(color = "black", size = y_axis_label_font_size),  # Adjust y-axis labels font color and size
        axis.title.x = element_text(color = "black", size = x_axis_label_font_size),  # Adjust x-axis title font color and size
        axis.title.y = element_text(color = "black", size = y_axis_label_font_size),  # Adjust y-axis title font color and size
        legend.text = element_text(size = legend_font_size, color = "black"),  # Adjust legend text size and color
        legend.title = element_text(size = legend_title_font_size, color = "black"))  # Adjust legend title size and color
