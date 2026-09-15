## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr, ggpubr, car, broom)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

offenders = readRDS("data/output/offenders_homicide_2021_2024.rds")
victims = readRDS("data/output/victims_homicide_2021_2024.rds") 

# defaulting race known hispanic NA to [race] nonhispanic
# also making hispanic its own race category, all hispanic + any NIBRS race or race NA

age_levels_5yr <- c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

race_ethnicity_levels <- c(
  "White, Not Hispanic/Latino", "White, Hispanic/Latino",
  "Black or African American, Not Hispanic/Latino", "Black or African American, Hispanic/Latino",
  "American Indian or Alaska Native, Not Hispanic/Latino", "American Indian or Alaska Native, Hispanic/Latino",
  "Asian, Not Hispanic/Latino", "Asian, Hispanic/Latino",
  "Native Hawaiian or Other Pacific Islander, Not Hispanic/Latino", "Native Hawaiian or Other Pacific Islander, Hispanic/Latino",
  "NA, Not Hispanic/Latino", "NA, Hispanic/Latino"
)

# ---- NIBRS: victims ----
victims_table <- victims %>%
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
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_victims")

# ---- NIBRS: offenders ----
offenders_table <- offenders %>%
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
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_offenders")

# missing victims and offenders race
victims_table %>%
  mutate(unmapped = str_detect(race_ethnicity, "^NA")) %>%
  group_by(unmapped) %>%
  summarise(n_victims = sum(n_victims), .groups = "drop")

offenders_table %>%
  mutate(unmapped = str_detect(race_ethnicity, "^NA")) %>%
  group_by(unmapped) %>%
  summarise(n_offenders = sum(n_offenders), .groups = "drop")

# ---- ACS ----
acs = readRDS("data/output/acs.rds")

acs <- acs %>%
  mutate(
    sex_nibrs = case_when(sex == 1 ~ "Male", sex == 2 ~ "Female", TRUE ~ NA_character_),
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
    ) %>% factor(levels = race_ethnicity_levels),
    age_group_5yr = case_when(
      age >= 80 ~ "80+",
      TRUE ~ paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr)
  )

acs_table_year <- acs %>%
  filter(!is.na(race_ethnicity_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs, age_group_5yr) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs) %>%
  mutate(year = as.numeric(year))

# ---- combine + rates ----
combined_table <- victims_table %>%
  full_join(offenders_table, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  full_join(acs_table_year, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  arrange(year, race_ethnicity, age_group_5yr)

as_tibble(combined_table) %>% print(n = Inf, width = Inf)

write_csv(combined_table, "results/nibrs_acs_combined_race_ethnicity_age_v2.csv")

# race + ethnicity groups together (v2: race known + ethnicity unknown -> default to Not Hispanic/Latino)
race_eth_by_year = bind_rows(
  offenders %>%
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
      )
    ) %>%
    count(year, race_ethnicity) %>% mutate(role = "Offenders"),
  victims %>%
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
      )
    ) %>%
    count(year, race_ethnicity) %>% mutate(role = "Victims")
) %>%
  mutate(year = as.numeric(year))

race_eth_by_year %>% as_tibble() %>% print(n = Inf)

colors_role = c("Offenders" = "#3043B4", "Victims" = "#C97703")

ggplot(race_eth_by_year, aes(x = year, y = n, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colors_role) +
  scale_x_continuous(breaks = 2021:2024) +
  scale_y_continuous(breaks = function(x) unique(round(scales::extended_breaks()(x)))) +
  facet_wrap(~ race_ethnicity, scales = "free_y", ncol = 5, labeller = label_wrap_gen(width = 40)) +
  labs(
    title = "Homicide Victims and Offenders by Race and Ethnicity, 2021-2024",
    subtitle = "Total counts by year; NIBRS 2021-2024 (unknown ethnicity defaulted to non-Hispanic when race is known)",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NIBRS 2021-2024 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 11, color = "gray40"),
    axis.text.y = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victims_offenders_by_year_race_ethnicity_v2.png", width = 22, height = 16)

# combined victims, offenders, + ACS, race/ethnicity and age table by year
# (victims_table, offenders_table, acs_table_year already built with the v2 rule)
combined_table = victims_table %>%
  full_join(offenders_table, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  full_join(acs_table_year, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0)
  ) %>%
  arrange(year, race_ethnicity, age_group_5yr)

combined_table %>% as_tibble() %>% print(n = Inf)

combined_table = combined_table %>%
  mutate(
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000)

combined_table %>% as_tibble() %>% print(n = Inf, width = Inf)

write_csv(combined_table, "results/nibrs_acs_combined_race_ethnicity_age_v2.csv")

# pooled counts
pooled_table = combined_table %>%
    filter(!str_detect(race_ethnicity, "NA"),
        age_group_5yr != "Unknown") %>%
    group_by(race_ethnicity, age_group_5yr) %>%
    summarise(
        n_victims = sum(n_victims, na.rm = TRUE),
        n_offenders = sum(n_offenders, na.rm = TRUE),
        weighted = sum(weighted, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(victim_rate_per_100k = n_victims / weighted * 100000,
        offender_rate_per_100k = n_offenders / weighted * 100000)

pooled_long = pooled_table %>%
  select(race_ethnicity, age_group_5yr, victim_rate_per_100k, offender_rate_per_100k) %>%
  pivot_longer(cols = ends_with("rate_per_100k"), names_to = "role", values_to = "rate") %>%
  mutate(role = ifelse(role == "victim_rate_per_100k", "Victims", "Offenders"))

ggplot(pooled_long, aes(x = age_group_5yr, y = rate, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colors_role) +
  facet_wrap(~ race_ethnicity, scales = "free_y", ncol = 4, labeller = label_wrap_gen(width = 40)) +
  labs(
    title = "Homicide victimization and offending rates by age and race, 2021–2024",
    subtitle = "Rate per 100,000 population, pooled across four years (unknown ethnicity defaulted to non-Hispanic when race is known)",
    x = NULL,
    y = "Rate per 100,000",
    color = NULL,
    caption = "Sources: NIBRS via ICPSR; ACS via IPUMS USA."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 20, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 16, color = "gray30", margin = margin(b = 10)),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray40", margin = margin(t = 10)))

ggsave("results/rates_by_age_race_ethnicity_pooled_v2.png", width = 22, height = 16)

# victimization rates vs offending rates 
scatter_data = combined_table %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k),
    !age_group_5yr %in% c("0-4", "5-9", "10-14", "Unknown"))

library(ggpubr)

ggplot(scatter_data, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1), color = "black", label.x.npc = "left", label.y.npc = "top",
           method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(
    title = "Offender Rate vs. Victimization Rate (Zoomed)",
    subtitle = "NIBRS 2021-2024 / ACS 2021-2024; rates per 100,000 population by race x ethnicity x \nage x year; age 15+",
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

ggsave("results/offender_vs_victim_rate_scatter_pooled_years2.png", width = 14, height = 12)

fit = lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_data)
summary(fit)$r.squared

scatter_all_ages <- combined_table %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k),
    age_group_5yr != "Unknown"
  )

scatter_15plus <- scatter_all_ages %>%
  filter(!age_group_5yr %in% c("0-4", "5-9", "10-14"))

fit_all <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_all_ages)
fit_15plus <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_15plus)

cat("All ages, R²:", round(summary(fit_all)$r.squared, 6), "\n")
cat("Ages 15+,  R²:", round(summary(fit_15plus)$r.squared, 6), "\n")

# lm no age groups
race_year_table <- victims_table %>%
  group_by(year, race_ethnicity) %>%
  summarise(n_victims = sum(n_victims, na.rm = TRUE), .groups = "drop") %>%
  full_join(
    offenders_table %>%
      group_by(year, race_ethnicity) %>%
      summarise(n_offenders = sum(n_offenders, na.rm = TRUE), .groups = "drop"),
    by = c("year", "race_ethnicity")
  ) %>%
  full_join(
    acs_table_year %>%
      group_by(year, race_ethnicity) %>%
      summarise(weighted = sum(weighted, na.rm = TRUE), .groups = "drop"),
    by = c("year", "race_ethnicity")
  ) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  filter(!is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k))

fit_race_year <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = race_year_table)
cat("Race/ethnicity x year (no age breakdown), R²:", round(summary(fit_race_year)$r.squared, 6), "\n")

ggplot(race_year_table, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1), color = "black", label.x.npc = "left", label.y.npc = "top",
           method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(
    title = "Offender Rate vs. Victimization Rate (Zoomed)",
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

ggsave("results/offender_vs_victim_rate_scatter_race_ethnicity_year.png", width = 14, height = 12)

# add in sex
victims %>% count(V4019)
offenders %>% count(V5008)

victims_table_sex = victims %>%
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
    sex = case_when(
      str_detect(as.character(V4019), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V4019), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, sex, name = "n_victims")

offenders_table_sex = offenders %>%
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
    sex = case_when(
      str_detect(as.character(V5008), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V5008), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, sex, name = "n_offenders")

acs_table_year_sex = acs %>%
  filter(!is.na(race_ethnicity_nibrs), !is.na(sex_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs, sex_nibrs) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs, sex = sex_nibrs) %>%
  mutate(year = as.numeric(year))

race_year_sex_table = victims_table_sex %>%
  full_join(offenders_table_sex, by = c("year", "race_ethnicity", "sex")) %>%
  full_join(acs_table_year_sex, by = c("year", "race_ethnicity", "sex")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  filter(!is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k), !is.na(sex))

write_csv(race_year_sex_table, "results/nibrs_acs_combined_race_sex.csv")

fit_race_year_sex = lm(offender_rate_per_100k ~ victim_rate_per_100k, data = race_year_sex_table)
cat("Race/ethnicity x year x sex, R²:", round(summary(fit_race_year_sex)$r.squared, 6), "\n")

ggplot(race_year_sex_table, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year), shape = sex)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1, shape = NULL), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1, shape = NULL), color = "black", label.x.npc = "left", label.y.npc = "top",
         method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  scale_shape_manual(values = c("Male" = 16, "Female" = 17), name = "Sex") +
  labs(
    title = "Offender Rate vs. Victimization Rate",
    subtitle = "NIBRS 2021-2024 / ACS 2021-2024; rates per 100,000 population by race x ethnicity x year x sex ",
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

ggsave("results/offender_vs_victim_rate_scatter_race_ethnicity_year_sex.png", width = 14, height = 12)
  
# how often multiple victims or multiple offenders
incidents_homicide = readRDS("data/output/incidents_homicide_2021_2024.rds")

offenders_per_incident = offenders %>% count(incident_key, name = "n_offenders_in_incident")
victims_per_incident   = victims   %>% count(incident_key, name = "n_victims_in_incident")

pct_multi_offender = mean(offenders_per_incident$n_offenders_in_incident > 1) * 100
pct_multi_victim   = mean(victims_per_incident$n_victims_in_incident > 1) * 100

pct_multi_offender
pct_multi_victim

offenders_per_incident %>% count(n_offenders_in_incident, name = "n_incidents") %>%
  mutate(pct = round(n_incidents / sum(n_incidents) * 100, 2))

victims_per_incident %>% count(n_victims_in_incident, name = "n_incidents") %>%
  mutate(pct = round(n_incidents / sum(n_incidents) * 100, 2))

# victims with no offender?
victims %>%
  filter(!incident_key %in% offenders$incident_key) %>%
  summarise(n_victims_no_offender = n(),
            pct = n() / nrow(victims) * 100)

victims %>%
  filter(!incident_key %in% offenders$incident_key) %>%
  count(year) %>%
  left_join(victims %>% count(year, name = "total_victims"), by = "year") %>%
  mutate(pct = n / total_victims * 100)

offenders_per_incident <- offenders %>% count(incident_key, name = "n_offenders_in_incident")

incidents_homicide %>%
  left_join(offenders_per_incident, by = "incident_key") %>%
  mutate(n_offenders_in_incident = replace_na(n_offenders_in_incident, 0)) %>%
  count(n_offenders_in_incident == 0)

unsolved_incidents <- offenders %>%
  mutate(offender_race_raw = as.character(V5009)) %>%
  group_by(incident_key) %>%
  summarise(all_unknown = all(is.na(offender_race_raw) | str_detect(offender_race_raw, "Unknown")), .groups = "drop")

unsolved_incidents <- offenders %>%
  mutate(
    offender_race_raw = as.character(V5009),
    offender_sex_raw = as.character(V5008),
    offender_age_raw = as.numeric(as.character(V5007))
  ) %>%
  group_by(incident_key) %>%
  summarise(
    all_unknown = all(
      (is.na(offender_race_raw) | str_detect(offender_race_raw, "Unknown")) &
      (is.na(offender_sex_raw) | str_detect(offender_sex_raw, "Unknown")) &
      (is.na(offender_age_raw) | offender_age_raw <= 0)
    ),
    .groups = "drop"
  )

incidents_homicide %>%
  left_join(unsolved_incidents, by = "incident_key") %>%
  count(all_unknown)

arrest_cols_incidents <- names(incidents_homicide)[sapply(incidents_homicide, function(x) {
  vals <- unique(as.character(x))
  any(str_detect(vals, regex("arrest", ignore_case = TRUE)))
})]
arrest_cols_incidents

arrest_cols_offenders <- names(offenders)[sapply(offenders, function(x) {
  vals <- unique(as.character(x))
  any(str_detect(vals, regex("arrest", ignore_case = TRUE)))
})]
arrest_cols_offenders

incidents_homicide %>% count(V60091) %>% arrange(desc(n))
incidents_homicide %>% count(V60101) %>% arrange(desc(n))
incidents_homicide %>% count(V4017A1) %>% arrange(desc(n))

incidents_homicide <- incidents_homicide %>%
  mutate(had_arrest = !is.na(V60091) | !is.na(V60092) | !is.na(V60093))

incidents_homicide %>%
  left_join(unsolved_incidents, by = "incident_key") %>%
  filter(all_unknown) %>%
  count(had_arrest) %>%
  mutate(pct = n / sum(n) * 100)

incidents_homicide %>%
  group_by(year) %>%
  count(had_arrest) %>% 
  mutate(pct = round(n / sum(n) * 100, 2))

# victims level, no arrest
victims %>%
  left_join(incidents_homicide %>% select(incident_key, had_arrest), by = "incident_key") %>%
  count(had_arrest) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

victims %>%
  left_join(incidents_homicide %>% select(incident_key, had_arrest), by = "incident_key") %>%
  group_by(year) %>%
  count(had_arrest) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

unsolved_incidents <- offenders %>%
  mutate(
    offender_race_raw = as.character(V5009),
    offender_ethnicity_raw = as.character(V5011),
    offender_sex_raw = as.character(V5008),
    offender_age_raw = as.numeric(as.character(V5007))
  ) %>%
  group_by(incident_key) %>%
  summarise(
    all_unknown = all(
      (is.na(offender_race_raw) | str_detect(offender_race_raw, "Unknown")) &
      (is.na(offender_ethnicity_raw) | str_detect(offender_ethnicity_raw, "Undetermined")) &
      (is.na(offender_sex_raw) | str_detect(offender_sex_raw, "Unknown")) &
      (is.na(offender_age_raw) | offender_age_raw <= 0)
    ),
    .groups = "drop"
  )

incidents_homicide %>%
  left_join(unsolved_incidents, by = "incident_key") %>%
  filter(all_unknown) %>%
  group_by(year) %>%
  count(had_arrest) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

offenders %>%
  left_join(incidents_homicide %>% select(incident_key, had_arrest), by = "incident_key") %>%
  group_by(year) %>%
  count(had_arrest) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

# incidents with no arrest, how many have suspect demographic
incidents_homicide %>%
  left_join(unsolved_incidents, by = "incident_key") %>%
  filter(!had_arrest) %>%
  count(all_unknown) %>%
  mutate(pct = round(n / sum(n) * 100, 2))

# restricting to age 15-54
scatter_15_54 <- combined_table %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k),
    age_group_5yr %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")
  )

fit_15_54 <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_15_54)
plot(fit_15_54)
cat("Ages 15-54, R²:", round(summary(fit_15_54)$r.squared, 6), "\n")

summary(fit_15_54)

ggplot(scatter_15_54, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1), color = "black", label.x.npc = "left", label.y.npc = "top",
           method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  labs(
    title = "Offender Rate vs. Victimization Rate",
    subtitle = "NIBRS 2021-2024 / ACS 2021-2024; rates per 100,000 population by race x ethnicity x\nage x year; ages 15-54",
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

ggsave("results/offender_vs_victim_rate_scatter_race_ethnicity_age_15_54.png", width = 14, height = 12)


# restricting to age 15-54 and incidents with arrest (victims where offender was arrested, and arrested offenders)
arrested_incident_keys <- incidents_homicide %>% filter(had_arrest) %>% pull(incident_key)

victims_table_arrested <- victims %>%
  filter(incident_key %in% arrested_incident_keys) %>%
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
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_victims")

offenders_table_arrested <- offenders %>%
  filter(incident_key %in% arrested_incident_keys) %>%
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
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, name = "n_offenders")

combined_table_arrested <- victims_table_arrested %>%
  full_join(offenders_table_arrested, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  full_join(acs_table_year, by = c("year", "race_ethnicity", "age_group_5yr")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  )

scatter_arrested_15_54 <- combined_table_arrested %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k),
    age_group_5yr %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")
  )

fit_arrested_15_54 <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_arrested_15_54)

cat("Arrested only, ages 15-54, R²:", round(summary(fit_arrested_15_54)$r.squared, 6), "\n")

summary(fit_arrested_15_54)

ggplot(scatter_arrested_15_54, aes(x = victim_rate_per_100k, y = offender_rate_per_100k, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 1) +
  stat_cor(aes(group = 1), color = "black", label.x.npc = "left", label.y.npc = "top",
           method = "pearson", r.accuracy = 0.01) +
  scale_color_viridis_d(option = "D", name = "Year") +
  labs(
    title = "Offender Rate vs. Victimization Rate (Arrested Only)",
    subtitle = "NIBRS 2021-2024 / ACS 2021-2024; rates per 100,000 population by race x ethnicity x\nage x year; ages 15-54; incidents with an arrest only",
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

ggsave("results/offender_vs_victim_rate_scatter_arrested_15_54.png", width = 14, height = 12)

# add in sex column
acs_table_year_race_age_sex <- acs %>%
  filter(!is.na(race_ethnicity_nibrs), !is.na(sex_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs, age_group_5yr, sex_nibrs) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs, sex = sex_nibrs) %>%
  mutate(year = as.numeric(year))

victims_table_arrested_sex <- victims %>%
  filter(incident_key %in% arrested_incident_keys) %>%
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
    sex = case_when(
      str_detect(as.character(V4019), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V4019), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_victims")

offenders_table_arrested_sex <- offenders %>%
  filter(incident_key %in% arrested_incident_keys) %>%
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
    sex = case_when(
      str_detect(as.character(V5008), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V5008), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_offenders")

combined_table_arrested_sex <- victims_table_arrested_sex %>%
  full_join(offenders_table_arrested_sex, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  full_join(acs_table_year_race_age_sex, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  )

  scatter_arrested_15_54_sex <- combined_table_arrested_sex %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k), !is.na(sex),
    age_group_5yr %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")
  )

fit_arrested_15_54_sex <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_arrested_15_54_sex)
cat("Arrested only, ages 15-54, race x age x sex, R²:", round(summary(fit_arrested_15_54_sex)$r.squared, 6), "\n")

# arrested and non arrested
victims_table_sex_age <- victims %>%
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
    sex = case_when(
      str_detect(as.character(V4019), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V4019), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_victims")

offenders_table_sex_age <- offenders %>%
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
    sex = case_when(
      str_detect(as.character(V5008), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V5008), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, name = "n_offenders")

combined_table_sex_age <- victims_table_sex_age %>%
  full_join(offenders_table_sex_age, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  full_join(acs_table_year_race_age_sex, by = c("year", "race_ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  )

scatter_15_54_sex <- combined_table_sex_age %>%
  filter(
    !is.na(victim_rate_per_100k), !is.na(offender_rate_per_100k), !is.na(sex),
    age_group_5yr %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")
  )

fit_15_54_sex <- lm(offender_rate_per_100k ~ victim_rate_per_100k, data = scatter_15_54_sex)
cat("All incidents, ages 15-54, race x age x sex, R²:", round(summary(fit_15_54_sex)$r.squared, 6), "\n")

incidents_homicide %>%
  left_join(
    victims %>%
      mutate(race = str_remove(as.character(V4020), "^\\(-?\\d+\\)\\s*")) %>%
      distinct(incident_key, race) %>%
      group_by(incident_key) %>%
      slice(1) %>%
      ungroup(),
    by = "incident_key"
  ) %>%
  group_by(race) %>%
  summarise(
    n_incidents = n(),
    clearance_rate = mean(had_arrest, na.rm = TRUE) * 100
  ) %>%
  arrange(desc(n_incidents))

# cook's distance (race/ethnicity, age, year)
diag_15_54 <- augment(fit_15_54, data = scatter_15_54)

ggplot(diag_15_54, aes(x = .hat, y = .std.resid, size = .cooksd)) +
  geom_point(alpha = 0.6, color = "#3043B4") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  labs(
    title = "Influence Plot",
    subtitle = "Ages 15-54; race x ethnicity x age x year; leverage vs. standardized residuals, point size = Cook's distance",
    x = "Leverage",
    y = "Standardized Residual",
    size = "Cook's D"
  ) +
  theme_minimal()

ggsave("results/influence_plot_race_ethnicity_age_year_15_54.png", width = 12, height = 9)

diag_15_54 %>%
  arrange(desc(.cooksd)) %>%
  select(race_ethnicity, age_group_5yr, year, victim_rate_per_100k, offender_rate_per_100k, .cooksd) %>%
  head(20)

# FE
fit_15_54_FE <- lm(offender_rate_per_100k ~ victim_rate_per_100k + as.factor(race_ethnicity) + as.factor(age_group_5yr), data = scatter_15_54)

summary(fit_15_54_FE)
anova(fit_15_54_FE)

# recode NIBRS state codes
recode_nibrs_state <- function(x) {
  abbrev <- str_extract(as.character(x), "[A-Z]{2}$")
  case_when(
    abbrev == "NB" ~ "NE",   # NIBRS uses "NB" for Nebraska; ACS/USPS uses "NE"
    TRUE ~ abbrev
  )
}

victims = victims %>% mutate(state = recode_nibrs_state(STATE))
offenders = offenders %>% mutate(state = recode_nibrs_state(STATE))

fips_to_state = tibble::tribble(
  ~statefip, ~state,
  1,"AL", 2,"AK", 4,"AZ", 5,"AR", 6,"CA", 8,"CO", 9,"CT", 10,"DE", 11,"DC", 12,"FL",
  13,"GA", 15,"HI", 16,"ID", 17,"IL", 18,"IN", 19,"IA", 20,"KS", 21,"KY", 22,"LA", 23,"ME",
  24,"MD", 25,"MA", 26,"MI", 27,"MN", 28,"MS", 29,"MO", 30,"MT", 31,"NE", 32,"NV", 33,"NH",
  34,"NJ", 35,"NM", 36,"NY", 37,"NC", 38,"ND", 39,"OH", 40,"OK", 41,"OR", 42,"PA", 44,"RI",
  45,"SC", 46,"SD", 47,"TN", 48,"TX", 49,"UT", 50,"VT", 51,"VA", 53,"WA", 54,"WV", 55,"WI", 56,"WY")

acs = acs %>% left_join(fips_to_state, by = "statefip")

victims_table_full <- victims %>%
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
    sex = case_when(
      str_detect(as.character(V4019), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V4019), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, state, name = "n_victims")

offenders_table_full <- offenders %>%
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
    sex = case_when(
      str_detect(as.character(V5008), "^\\(0\\)") ~ "Female",
      str_detect(as.character(V5008), "^\\(1\\)") ~ "Male",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    year = as.numeric(year)
  ) %>%
  count(year, race_ethnicity, age_group_5yr, sex, state, name = "n_offenders")

acs_table_full <- acs %>%
  filter(!is.na(race_ethnicity_nibrs), !is.na(sex_nibrs), !is.na(state)) %>%
  group_by(year, race_ethnicity_nibrs, age_group_5yr, sex_nibrs, state) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs, sex = sex_nibrs) %>%
  mutate(year = as.numeric(year))

combined_table_full <- victims_table_full %>%
  full_join(offenders_table_full, by = c("year", "race_ethnicity", "age_group_5yr", "sex", "state")) %>%
  full_join(acs_table_full, by = c("year", "race_ethnicity", "age_group_5yr", "sex", "state")) %>%
  mutate(
    n_victims = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0),
    victim_rate_per_100k = n_victims / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  ) %>%
  arrange(year, race_ethnicity, age_group_5yr, sex, state)

write_csv(combined_table_full, "results/nibrs_acs_combined_race_ethnicity_age_sex_state.csv")

