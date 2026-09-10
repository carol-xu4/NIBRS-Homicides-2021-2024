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

# ---- ACS ----
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
