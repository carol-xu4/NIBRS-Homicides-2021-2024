## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

# stacked homicide offenders data
offenders = readRDS("data/output/offenders_homicide_2021_2024.rds")

nrow(offenders)

names(offenders)

total_offenders = table(offenders$year)

print(total_offenders)

# offenders race + ethnicity + age by year
offenders_table = offenders %>%
  mutate(
    race = str_remove(as.character(V5009), "^\\(-?\\d+\\)\\s*"),
    ethnicity = case_when(
      str_detect(as.character(V5011), "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(as.character(V5011), "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_),
    race_ethnicity = paste0(race, ", ", ethnicity),
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_offenders")

