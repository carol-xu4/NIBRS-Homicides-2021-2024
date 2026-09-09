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
