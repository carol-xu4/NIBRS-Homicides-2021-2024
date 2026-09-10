## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

# stacked homicide victims data
victims = readRDS("data/output/victims_homicide_2021_2024.rds")

nrow(victims)

names(victims)

total_victims = table(victims$year)

print(total_victims)

# NIBRS victims: race + ethnicity + age by year
victims_table = victims %>%
  mutate(
    race = str_remove(as.character(V4020), "^\\(-?\\d+\\)\\s*"),
    ethnicity = case_when(
      str_detect(as.character(V4021), "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(as.character(V4021), "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_),
    race_ethnicity = paste0(race, ", ", ethnicity),
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_victims")

victims_table %>% as_tibble() %>% print(n = Inf)

