## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory 
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

# ACS data 
acs = readRDS("data/output/acs.rds")

acs_race_eth_by_year = acs %>%
  filter(!is.na(race_ethnicity_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs) %>%
  mutate(year = as.numeric(year))

print(acs_race_eth_by_year, n = Inf)

ggplot(acs_race_eth_by_year, aes(x = year, y = weighted, group = race_ethnicity)) +
  geom_line(color = "#3043B4", linewidth = 1.5) +
  geom_point(color = "#3043B4", size = 2.5) +
  scale_x_continuous(breaks = 2021:2024) +
  scale_y_continuous(
    breaks = function(x) unique(round(scales::extended_breaks()(x))),
    labels = scales::label_comma()
  ) +
  facet_wrap(~ race_ethnicity, scales = "free_y", ncol = 5, labeller = label_wrap_gen(width = 40)) +
  labs(
    title = "ACS Population by Race and Ethnicity, 2021-2024",
    subtitle = "Weighted population estimates, ignoring age; ACS via IPUMS",
    x = NULL, y = NULL, color = NULL,
    caption = "Source: ACS 2021-2024 via IPUMS") +
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

ggsave("results/acs_population_by_year_race_ethnicity.png", width = 22, height = 16)

# ACS: race_ethnicity + age, by year 
acs_table_year = acs %>%
  filter(!is.na(race_ethnicity_nibrs)) %>%
  group_by(year, race_ethnicity_nibrs, age_group_5yr) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs) %>%
  mutate(year = as.numeric(year))
