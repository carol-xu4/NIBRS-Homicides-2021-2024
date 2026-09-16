## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, ggpubr, car, broom)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

# recode state
recode_nibrs_state = function(x) {
  abbrev <- str_extract(as.character(x), "[A-Z]{2}$")
  case_when(
    abbrev == "NB" ~ "NE",   # NIBRS uses "NB" for Nebraska
    TRUE ~ abbrev)}

# read n victims and offenders data, add race/ethnicity
victims_ca = readRDS("data/output/victims_homicide_2021_2024.rds") %>%
  mutate(state = recode_nibrs_state(STATE)) %>%
  filter(state == "CA") %>%
  mutate(
    race = str_remove(as.character(V4020), "^\\(-?\\d+\\)\\s*"),
    ethnicity = case_when(
      str_detect(as.character(V4021), "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(as.character(V4021), "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ),
    race_ethnicity = case_when(
      !is.na(race) & is.na(ethnicity) ~ paste0(race, ", Not Hispanic/Latino"),
      TRUE ~ paste0(race, ", ", ethnicity)
    ),
    year = as.numeric(year)
  )

offenders_ca = readRDS("data/output/offenders_homicide_2021_2024.rds") %>%
  mutate(state = recode_nibrs_state(STATE)) %>%
  filter(state == "CA") %>%
  mutate(
    race = str_remove(as.character(V5009), "^\\(-?\\d+\\)\\s*"),
    ethnicity = case_when(
      str_detect(as.character(V5011), "^\\(0\\)") ~ "Not Hispanic/Latino",
      str_detect(as.character(V5011), "^\\(1\\)") ~ "Hispanic/Latino",
      TRUE ~ NA_character_
    ),
    race_ethnicity = case_when(
      !is.na(race) & is.na(ethnicity) ~ paste0(race, ", Not Hispanic/Latino"),
      TRUE ~ paste0(race, ", ", ethnicity)
    ),
    year = as.numeric(year)
  )

nrow(victims_ca)
nrow(offenders_ca)

# victims and offenders by year
table(victims_ca$year)

table(offenders_ca$year)

# race/ethnicity x year counts
ca_victims_table = victims_ca %>%
  count(year, race_ethnicity, name = "n_victims")

ca_offenders_table = offenders_ca %>%
  count(year, race_ethnicity, name = "n_offenders")

ca_counts_table = ca_victims_table %>%
    full_join(ca_offenders_table, by = c("year", "race_ethnicity")) %>%
    mutate(
        n_victims = replace_na(n_victims, 0),
        n_offenders = replace_na(n_offenders, 0)) %>%
    arrange(year, race_ethnicity)

write_csv(ca_counts_table, "results/california_victims_offenders.csv")

# ACS California population, race/ethnicity + state derived fresh (acs.rds is the raw extract)
acs = readRDS("data/output/acs.rds")

fips_to_state = tibble::tribble(
  ~statefip, ~state,
  1,"AL", 2,"AK", 4,"AZ", 5,"AR", 6,"CA", 8,"CO", 9,"CT", 10,"DE", 11,"DC", 12,"FL",
  13,"GA", 15,"HI", 16,"ID", 17,"IL", 18,"IN", 19,"IA", 20,"KS", 21,"KY", 22,"LA", 23,"ME",
  24,"MD", 25,"MA", 26,"MI", 27,"MN", 28,"MS", 29,"MO", 30,"MT", 31,"NE", 32,"NV", 33,"NH",
  34,"NJ", 35,"NM", 36,"NY", 37,"NC", 38,"ND", 39,"OH", 40,"OK", 41,"OR", 42,"PA", 44,"RI",
  45,"SC", 46,"SD", 47,"TN", 48,"TX", 49,"UT", 50,"VT", 51,"VA", 53,"WA", 54,"WV", 55,"WI", 56,"WY")

acs = acs %>%
  left_join(fips_to_state, by = "statefip") %>%
  mutate(
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
    race_ethnicity_nibrs = case_when(
      !is.na(race_nibrs) & is.na(ethnicity_nibrs) ~ paste0(race_nibrs, ", Not Hispanic/Latino"),
      TRUE ~ paste0(race_nibrs, ", ", ethnicity_nibrs)
    )
  )

acs_ca_race_year = acs %>%
  filter(state == "CA", !is.na(race_ethnicity_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs) %>%
  mutate(year = as.numeric(year))

# combine + rates
ca_race_year_table = ca_victims_table %>%
  full_join(ca_offenders_table, by = c("year", "race_ethnicity")) %>%
  full_join(acs_ca_race_year, by = c("year", "race_ethnicity")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  filter(!is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k))

fit_ca_race_year = lm(offender_rate_per_100k ~ victim_rate_per_100k, data = ca_race_year_table)
cat("California, race/ethnicity x year, R²:", round(summary(fit_ca_race_year)$r.squared, 6), "\n")

ggplot(ca_race_year_table, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1), color = "black", label.x.npc = "left", label.y.npc = "top",
           method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  labs(
    title = "Offender Rate vs. Victimization Rate, California",
    subtitle = "NIBRS 2021-2024 / ACS 2021-2024; rates per 100,000 population by race/ethnicity x year",
    x = "Victim Rate per 100,000",
    y = "Offender Rate per 100,000",
    caption = "Source: NIBRS 2021-2024 via ICPSR; ACS 2021-2024 via IPUMS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/offender_vs_victim_rate_scatter_race_ethnicity_year_california.png", width = 14, height = 12)

# raw victims vs offenders by year
ca_counts_table = ca_victims_table %>%
  full_join(ca_offenders_table, by = c("year", "race_ethnicity")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0)
  )

ggplot(ca_counts_table, aes(x = n_victims, y = n_offenders, color = factor(year))) +
  geom_point(alpha = 0.7, size = 2.5) +
  scale_color_viridis_d(option = "D", name = "Year") +
  labs(
    title = "Offenders vs. Victims, California",
    subtitle = "NIBRS 2021-2024; raw counts by race/ethnicity x year",
    x = "Number of Victims",
    y = "Number of Offenders",
    caption = "Source: NIBRS 2021-2024 via ICPSR"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 16, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("results/victims_offenders_count_scatter_california.png", width = 14, height = 12)