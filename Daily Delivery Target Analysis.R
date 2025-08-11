################################################################################
#                                                                              #
#                    Daily Delivery Target Data Analysis                       #
#                          Freetown, Kenema, Makeni                            #
#                                                                              #
#                           To Be Used With Files:                             #
# mcc_2025, kcc_2024, kcc_2025, fcc_2021_team_1-3, fcc_2021_team_1-3,          #
# fcc_2021_team_4-6, fcc_2022_team_1-3, fcc_2022_team_4-6,  fcc_2023_team_1-3, #
# fcc_2023_team_4-6, fcc_2024_internal_staff,fcc_2024_team_1-3,fcc_2024_team_4-6, 
# fcc_2025_rnd1_team_1-3, fcc_2025_rnd1_team_1-3, fcc_2025_rnd2                #
#                                                                              #
#                              By: Lorena Edah                                 #
#                                                                              #
################################################################################

################################################################################
#                                  Summary                                     #
################################################################################

# Objective: 
# To determine a realistic and accurate daily delivery pace for each city—Freetown,
# Kenema, and Makeni—based on past delivery data.

# Methodology:
#1.Looking at Unfiltered Data: For each city and year, I looked at all the deliveries 
# (inaccurate and accurate) made by enumerators and ran through steps 3-6 before looking
# at the filter data to be able to compare both and see if there are substantial 
# differences.

#2. Filtering for Accuracy: For each city and year, I filtered the dataset to 
# include only deliveries that occurred within 80 meters or less of the assigned 
# property. This 80m threshold serves as a proxy for accurately completed deliveries.

#3. Grouping by Enumerator and Date: After filtering, I grouped the data by 
# enumerator and date to calculate the number of accurate deliveries made by each 
# enumerator on each day.

#4. Aggregating by Date: These counts were then aggregated by date only, 
# resulting in the average number of accurate deliveries made per day.

#5. Removing Outliers: To avoid skewed results, I removed statistical outliers 
# using the Interquartile Range (IQR) method, ensuring only typical delivery 
# days were included.

#6. Calculating the Mean: Finally, I calculated the mean daily delivery target 
# across the cleaned dataset, providing a benchmark for setting realistic delivery targets in each city.


# Key Findings:

#1. When comparing all deliveries to only accurate deliveries there is very 
#   minimal difference with the all deliveries average being only slightly higher than the accurate one.
#   a. For 2025:
#       Makeni: 1.37% difference (32.19 vs. 31.75)
#       Kenema: 1.14% difference (41.37 vs. 40.90)

#2. For 2025:
#   a. Makeni's average accurate deliver pace was 31 deliveries daily.
#   b. Kenema's average accurate delivery pace was 40 deliveries daily.

################################################################################
#                           Combining All Delivery Data                        #
################################################################################

# By City (Across Years)
# Across All Cities, Across All Years

###############################################################################
#                             Makeni 2025 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

# Importing the data
mcc_2025 <- read_excel("mcc_2025.xlsx")
View(mcc_2025)

# Cleaning the data
mcc_2025 <- mcc_2025 |> janitor::clean_names() # Fixing non-syntactic column names
View(mcc_2025) # Viewing the cleaned dataset again to confirm changes

# Understanding the data
mcc_2025 # Looking at a summary of the dataset in the console
glimpse (mcc_2025) # Looking at a concise glimpse of dataset structure
skim(mcc_2025) # Looking at a detailed summary of the dataset


# Transforming the data: Looking at Unfiltered vs. Filtered Data
mcc_2025 |> # Unfiltered data; analyzing all data even inaccurate deliveries
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |> # Creating a new column 'date' by converting 'delivered_on' string variable into a usable format
  count(date, mobile_user_a_username, name = "enum_acc_daily_deliv") |> # Counting number of deliveries for each enumerator on each date
  group_by(date) |> # Grouping the data by date (aggregating across enumerators)
  summarise(avg_acc_daily_deliv = ceiling(mean(enum_acc_daily_deliv)), .groups = "drop") |> # Calculating the average of daily deliveries across all enumerators, rounding up with ceiling()
  mutate( # Removing outliers using the IQR method
    Q1 = quantile(avg_acc_daily_deliv, 0.25),
    Q3 = quantile(avg_acc_daily_deliv, 0.75),
    IQR = Q3 - Q1
  ) |>
  filter(
    avg_acc_daily_deliv >= Q1 - 1.5 * IQR,
    avg_acc_daily_deliv <= Q3 + 1.5 * IQR
  ) |>
  summarise(mcc_2025_avg = mean(avg_acc_daily_deliv)) |> # Calculating the mean of daily delivery across the entire delivery period from the filtered data (outliers removed)
  view() # Displaying the result which is 32.19 deliveries.

mcc_2025 |> # Filtered data; analyzing only accurate deliveries 
  filter(distance <= 80) |> # Filtering rows where delivery 'distance' is 80 meters or less to keep only accurate deliveries
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |>   
  count(date, mobile_user_a_username, name = "enum_acc_daily_deliv") |> 
  group_by(date) |> 
  summarise(avg_acc_daily_deliv = ceiling(mean(enum_acc_daily_deliv)), .groups = "drop") |> 
  mutate( 
    Q1 = quantile(avg_acc_daily_deliv, 0.25),
    Q3 = quantile(avg_acc_daily_deliv, 0.75),
    IQR = Q3 - Q1
  ) |>
  filter(
    avg_acc_daily_deliv >= Q1 - 1.5 * IQR,
    avg_acc_daily_deliv <= Q3 + 1.5 * IQR
  ) |>
  summarise(mcc_2025_avg = mean(avg_acc_daily_deliv)) |>   
  view() # Displaying the result which is 31.75 deliveries.

((32.19-31.75)/32.19)*100 # Calculating the percentage difference between average of all daily deliveries vs. only accurate daily deliveries

# Visualizing the data
daily_avg_mcc <- mcc_2025 |> # Preparing data for visualization
  filter(distance <= 80) |>
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |>
  count(date, mobile_user_a_username, name = "enum_accurate_daily_deliveries") |>
  group_by(date) |>
  summarise(average_accurate_daily_deliveries = ceiling(mean(enum_accurate_daily_deliveries)), .groups = "drop")
ggplot(daily_avg_mcc, aes(x = date, y = average_accurate_daily_deliveries)) + # Plotting the average accurate daily deliveries over time using ggplot2 (time-series plot)
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Average Accurate Daily Deliveries Over Time",
    x = "Date",
    y = "Average Daily Accurate Deliveries"
  ) 
ggsave("makeni_2025_avg_daily_deliveries.png")

################################################################################
#                           Kenema 2025 Delivery Data                          #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

# Importing the data
kcc_2025 <- read_excel("kcc_2025.xlsx")
View(kcc_2025)

# Cleaning the data
kcc_2025 <- kcc_2025 |> janitor::clean_names() 
View(kcc_2025) 

# Understanding the data
kcc_2025
glimpse (kcc_2025)


# Transforming the data
kcc_2025 |>
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |>
  count(date, user_name, name = "enum_acc_daily_deliv") |>
  group_by(date) |>
  summarise(avg_acc_daily_deliv = ceiling(mean(enum_acc_daily_deliv)), .groups = "drop") |>
  mutate(
    Q1 = quantile(avg_acc_daily_deliv, 0.25),
    Q3 = quantile(avg_acc_daily_deliv, 0.75),
    IQR = Q3 - Q1
  ) |>
  filter(
    avg_acc_daily_deliv >= Q1 - 1.5 * IQR,
    avg_acc_daily_deliv <= Q3 + 1.5 * IQR
  ) |>
  summarise(kcc_2025_avg = mean(avg_acc_daily_deliv)) |>
  view() # Displaying the result which is 41.37 deliveries.

kcc_2025 |>
  filter(distance <= 80) |>
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |>
  count(date, full_name, name = "enum_acc_daily_deliv") |>
  group_by(date) |>
  summarise(avg_acc_daily_deliv = ceiling(mean(enum_acc_daily_deliv)), .groups = "drop") |>
  mutate(
    Q1 = quantile(avg_acc_daily_deliv, 0.25),
    Q3 = quantile(avg_acc_daily_deliv, 0.75),
    IQR = Q3 - Q1
  ) |>
  filter(
    avg_acc_daily_deliv >= Q1 - 1.5 * IQR,
    avg_acc_daily_deliv <= Q3 + 1.5 * IQR
  ) |>
  summarise(kcc_2025_avg = mean(avg_acc_daily_deliv)) |>
  view() # Displaying the result which is 40.90 deliveries.

((41.37-40.90)/41.37)*100 # Calculating the percentage difference between average of all daily deliveries vs. only accurate daily deliveries



# Visualizing the data
daily_avg_kcc <- kcc_2025 |>
  filter(distance <= 80) |>
  mutate(date = as.Date(delivered_on, format = "%Y-%m-%d")) |>
  count(date, full_name, name = "enum_accurate_daily_deliveries") |>
  group_by(date) |>
  summarise(average_accurate_daily_deliveries = ceiling(mean(enum_accurate_daily_deliveries)), .groups = "drop")
ggplot(daily_avg_kcc, aes(x = date, y = average_accurate_daily_deliveries)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Average Accurate Daily Deliveries Over Time",
    x = "Date",
    y = "Average Daily Accurate Deliveries"
  ) 
ggsave("kenema_2025_avg_daily_deliveries.png")

################################################################################
#                           Kenema 2024 Delivery Data                          #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

  
################################################################################
#                           Freetown 2025 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)


################################################################################
#                           Freetown 2024 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

################################################################################
#                           Freetown 2023 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

################################################################################
#                           Freetown 2022 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)

################################################################################
#                           Freetown 2021 Delivery Data                        #
################################################################################


# Loading the necessary packages
library(tidyverse)
library(janitor)
library(readr)
library (skimr)
library(ggthemes)
library(readxl)











