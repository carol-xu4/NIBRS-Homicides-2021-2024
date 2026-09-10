# total victims and offenders over time
totals_by_year = bind_rows(
  offenders %>% count(year) %>% mutate(role = "Offenders"),
  victims %>% count(year) %>% mutate(role = "Victims")
) %>%
  mutate(year = as.numeric(year))

colors_role = c("Offenders" = "#3043B4", "Victims" = "#C97703")

ggplot(totals_by_year, aes(x = year, y = n, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = colors_role) +
  scale_x_continuous(breaks = 2021:2024) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(totals_by_year$n) * 1.15)) +
  labs(
    title = "Homicide Victims and Offenders, 2021-2024",
    subtitle = "Total counts by year; NIBRS 2021-2024",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NIBRS 2021-2024 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 30, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 20, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 20, color = "gray40"),
    axis.text.y = element_text(size = 20, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victims_offenders_by_year.png", width = 15, height = 10)

# total by race
race_by_year = bind_rows(
  offenders %>%
    mutate(race = str_remove(as.character(V5009), "^\\(\\d+\\)\\s*")) %>%
    count(year, race) %>% mutate(role = "Offenders"),
  victims %>%
    mutate(race = str_remove(as.character(V4020), "^\\(\\d+\\)\\s*")) %>%
    count(year, race) %>% mutate(role = "Victims")
) %>%
  mutate(year = as.numeric(year))

print(race_by_year, n = Inf)  # check the actual race labels before plotting -- confirm no stray "(n)" prefixes or unexpected categories

ggplot(race_by_year, aes(x = year, y = n, color = role, group = role)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = colors_role) +
  scale_x_continuous(breaks = 2021:2024) +
  facet_wrap(~ race, scales = "free_y") +
  labs(
    title = "Homicide Victims and Offenders by Race, 2021-2024",
    subtitle = "Total counts by year; NIBRS 2021-2024",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: NIBRS 2021-2024 via ICPSR") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", hjust = 0, color = "black"),
    plot.subtitle = element_text(size = 18, color = "gray40", hjust = 0, margin = margin(b = 12)),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16, color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 14, color = "gray40"),
    axis.text.y = element_text(size = 14, color = "gray40"),
    plot.caption = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA))

ggsave("results/victims_offenders_by_year_race.png", width = 16, height = 10)


# race + ethnicity groups together
race_eth_by_year = bind_rows(
  offenders %>%
    mutate(
      race = str_remove(as.character(V5009), "^\\(\\d+\\)\\s*"),
      ethnicity = case_when(
        str_detect(as.character(V5011), "^\\(0\\)") ~ "Not Hispanic/Latino",
        str_detect(as.character(V5011), "^\\(1\\)") ~ "Hispanic/Latino",
        TRUE ~ NA_character_
      ),
      race_ethnicity = paste0(race, ", ", ethnicity)
    ) %>%
    count(year, race_ethnicity) %>% mutate(role = "Offenders"),
  victims %>%
    mutate(
      race = str_remove(as.character(V4020), "^\\(\\d+\\)\\s*"),
      ethnicity = case_when(
        str_detect(as.character(V4021), "^\\(0\\)") ~ "Not Hispanic/Latino",
        str_detect(as.character(V4021), "^\\(1\\)") ~ "Hispanic/Latino",
        TRUE ~ NA_character_
      ),
      race_ethnicity = paste0(race, ", ", ethnicity)
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
    subtitle = "Total counts by year; NIBRS 2021-2024",
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

ggsave("results/victims_offenders_by_year_race_ethnicity.png", width = 22, height = 16)

# combined victims, offenders, + ACS, race/ethnicity and age table by year
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

write_csv(combined_table, "results/nibrs_acs_combined_race_ethnicity_age.csv")

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
    subtitle = "Rate per 100,000 population, pooled across four years by race and ethnicity",
    x = NULL,
    y = "Rate per 100,000",
    color = NULL,
    caption = "Sources: FBI, National Incident-Based Reporting System (NIBRS) Extract Files, 2021–2024, via ICPSR; and US Census Bureau, American Community Survey, 2021–2024, via IPUMS USA."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 16, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 12, color = "gray30", margin = margin(b = 10)),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.caption = element_text(size = 8, hjust = 0, color = "gray40", margin = margin(t = 10)))

ggsave("results/rates_by_age_race_ethnicity_pooled.png", width = 22, height = 16)

