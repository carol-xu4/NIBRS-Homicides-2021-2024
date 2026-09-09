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
