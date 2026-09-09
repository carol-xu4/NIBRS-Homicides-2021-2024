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
