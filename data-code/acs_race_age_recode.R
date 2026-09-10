## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

# read in ACS 2021-2024
ddi_acs = read_ipums_ddi("data/input/usa_00030.xml")
acs = read_ipums_micro(ddi_acs)

acs = acs %>% rename_with(tolower) %>%
  select(year, perwt, sex, age, race, hispan,
       racamind, racasian, racblk, racpacis, racwht) %>%
  filter(year >= 2021)

# ACS population estimates, by original race, age, sex, ethnicity variables
age_levels_5yr = c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

acs = acs %>%
  mutate(
    age_group_5yr = case_when(
      is.na(age) ~ "Unknown",
      age >= 80 ~ "80+",
      TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr))

# recode ACS demographics to match NIBRS coding, race and ethnicity combined
    # Hierarchical single-race assignment (NCHS bridged-race convention) to
    # resolve multiracial ACS respondents into NIBRS's single-race field.
    # Priority order: Black > AIAN > Asian > NHPI > White.
acs = acs %>%
  mutate(
    sex_nibrs = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    race_nibrs = case_when(
      racblk == 2 ~ "Black or African American",
      racamind == 2 ~ "American Indian or Alaska Native",
      racasian == 2 ~ "Asian",
      racpacis == 2 ~ "Native Hawaiian or Other Pacific Islander",
      racwht == 2 ~ "White",
      TRUE ~ NA_character_
    ),
    ethnicity_nibrs = case_when(
      hispan == 0 ~ "Not Hispanic/Latino",
      hispan %in% 1:4 ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ),
    race_ethnicity_nibrs = paste0(race_nibrs, ", ", ethnicity_nibrs) %>%
  factor(levels = c(
    "White, Not Hispanic/Latino", "White, Hispanic/Latino",
    "Black or African American, Not Hispanic/Latino", "Black or African American, Hispanic/Latino",
    "American Indian or Alaska Native, Not Hispanic/Latino", "American Indian or Alaska Native, Hispanic/Latino",
    "Asian, Not Hispanic/Latino", "Asian, Hispanic/Latino",
    "Native Hawaiian or Other Pacific Islander, Not Hispanic/Latino", "Native Hawaiian or Other Pacific Islander, Hispanic/Latino",
    "NA, Not Hispanic/Latino", "NA, Hispanic/Latino"
  )))

## Group: n and weighted population, NIBRS-comparable categories -------------
acs_table_nibrs = acs %>%
  group_by(race_ethnicity_nibrs, age_group_5yr, sex_nibrs) %>%
  summarise(
    n = n(),
    weighted = sum(perwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(race_ethnicity = race_ethnicity_nibrs, sex = sex_nibrs) %>%
  arrange(race_ethnicity, age_group_5yr, sex)

print(acs_table_nibrs, n = Inf)

write_csv(acs_table_nibrs, "results/acs_race_ethnicity_age_sex_nibrs.csv")

saveRDS(acs, "data/output/acs.rds")
