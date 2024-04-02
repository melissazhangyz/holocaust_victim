#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)
library(lubridate)

#### Clean data ####
raw_data <- read_csv("raw.csv")

cleaned_data <-
  raw_data %>% 
  drop_na() %>% 
  mutate(Religion = case_when(
    Religion == "Catholic" | Religion == "Greek Catholic" ~ "Catholic",
    Religion == "andere" | Religion == "Believes in God" | Religion == "Jehovah's Witness" | 
      Religion == "Czech-Moravian" |  Religion == "Agnostic" ~ "Other",
    Religion == "Unaffiliated" | Religion == "Unknown" ~ "Unknown",
    Religion == "Greek Orthodox" | Religion == "Eastern Orthodox" | Religion == "Russian Orthodox" ~ "Orthodox",
    TRUE ~ Religion
  )) %>% 
  mutate(
    `Date of Birth` = ymd(`Date of Birth`),
    `Date of Death` = ymd(`Date of Death`),
    # Calculate Age at Death
    Age = round(interval(`Date of Birth`, `Date of Death`) / years(1)),
    # Create a New Column for Year and Month of Death
    Death_Year_Month = format(`Date of Death`, "%Y-%m")
  )

colnames(cleaned_data)[c(1,2,3,4,5)] <- c("Last_Name", "First_Name", "Date_of_Birth", "Date_of_Death", "Birth_Place")

unique(cleaned_data$Religion)
head(cleaned_data)
#### Save data ####
write_csv(cleaned_data, "cleaned.csv")
