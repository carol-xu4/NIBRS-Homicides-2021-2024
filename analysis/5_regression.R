## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggthemes, readxl, data.table, gdata, ipumsr)

# Set working directory
setwd("C:/Users/CarolXu/OneDrive - Cato Institute/Desktop/NIBRS Homicides 2021-2024")

age_levels_5yr <- c(paste0(seq(0, 75, by = 5), "-", seq(4, 79, by = 5)), "80+", "Unknown")

race_ethnicity_levels <- c(
  "White, Not Hispanic/Latino", "White, Hispanic/Latino",
  "Black or African American, Not Hispanic/Latino", "Black or African American, Hispanic/Latino",
  "American Indian or Alaska Native, Not Hispanic/Latino", "American Indian or Alaska Native, Hispanic/Latino",
  "Asian, Not Hispanic/Latino", "Asian, Hispanic/Latino",
  "Native Hawaiian or Other Pacific Islander, Not Hispanic/Latino", "Native Hawaiian or Other Pacific Islander, Hispanic/Latino",
  "NA, Not Hispanic/Latino", "NA, Hispanic/Latino"
)

# ---- read in stacked NIBRS victim + offender files ----
victims   <- readRDS("data/output/victims_homicide_2021_2024.rds")
offenders <- readRDS("data/output/offenders_homicide_2021_2024.rds")

# ---- NIBRS: victims, now grouped by state too ----
# FIPS_STATE arrives as a plain zero-padded string ("01", "04", ...) -- this
# is the real Census FIPS code (unlike STATE, whose numeric prefix is an
# internal NIBRS ordering, not FIPS) -- so it's the right join key to match
# IPUMS's STATEFIP.
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
    # V4019 is coded "(1) Male" / "(0) Female" -- note this is flipped from
    # IPUMS's sex coding (1 = Male, 2 = Female), so map both to the same
    # "Male"/"Female" strings rather than relying on the underlying codes.
    sex = case_when(
      str_detect(as.character(V4019), "^\\(1\\)") ~ "Male",
      str_detect(as.character(V4019), "^\\(0\\)") ~ "Female",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V4018)),
    age_group_5yr = case_when(
      is.na(age_num) ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    state_fips = as.integer(as.character(FIPS_STATE)),
    year = as.numeric(year)
  ) %>%
  count(year, state_fips, race_ethnicity, age_group_5yr, sex, name = "n_victims")

# ---- NIBRS: offenders, now grouped by state too ----
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
    # V5008 uses the same "(1) Male" / "(0) Female" coding as V4019
    sex = case_when(
      str_detect(as.character(V5008), "^\\(1\\)") ~ "Male",
      str_detect(as.character(V5008), "^\\(0\\)") ~ "Female",
      TRUE ~ NA_character_
    ),
    age_num = as.numeric(as.character(V5007)),
    age_group_5yr = case_when(
      is.na(age_num) | age_num <= 0 ~ "Unknown",
      age_num >= 80 ~ "80+",
      TRUE ~ paste0(floor(age_num / 5) * 5, "-", floor(age_num / 5) * 5 + 4)
    ) %>% factor(levels = age_levels_5yr),
    state_fips = as.integer(as.character(FIPS_STATE)),
    year = as.numeric(year)
  ) %>%
  count(year, state_fips, race_ethnicity, age_group_5yr, sex, name = "n_offenders")

# ---- ACS: population by year, state, race/ethnicity, age, sex ----
# acs.rds must carry `statefip` (lowercase, from rename_with(tolower) in
# acs_race_age_recode.R) plus the race_ethnicity_nibrs / age_group_5yr /
# sex_nibrs fields already built there.
acs <- readRDS("data/output/acs.rds")

acs_table_year <- acs %>%
  filter(!is.na(race_ethnicity_nibrs), !is.na(sex_nibrs)) %>%
  group_by(year, statefip, race_ethnicity_nibrs, age_group_5yr, sex_nibrs) %>%
  summarise(weighted = sum(perwt, na.rm = TRUE), .groups = "drop") %>%
  rename(race_ethnicity = race_ethnicity_nibrs, state_fips = statefip, sex = sex_nibrs) %>%
  mutate(year = as.numeric(year))

# ---- combine + rates ----
combined_table <- victims_table %>%
  full_join(offenders_table, by = c("year", "state_fips", "race_ethnicity", "age_group_5yr", "sex")) %>%
  full_join(acs_table_year,  by = c("year", "state_fips", "race_ethnicity", "age_group_5yr", "sex")) %>%
  mutate(
    n_victims   = replace_na(n_victims, 0),
    n_offenders = replace_na(n_offenders, 0)
  ) %>%
  arrange(year, state_fips, race_ethnicity, age_group_5yr, sex)

# Rows with no ACS population match -- NIBRS territories (e.g. FIPS 66 =
# Guam) that IPUMS USA/ACS doesn't cover, plus any victim/offender records
# with unknown sex (V4019/V5008 blank) -- get weighted = NA here. Drop them
# explicitly rather than let them silently become zero-population cells.
n_no_acs_match <- sum(is.na(combined_table$weighted))
message(n_no_acs_match, " state/year/race/age/sex cells had no ACS population match (territories, unknown sex) -- dropped.")

combined_table <- combined_table %>%
  filter(!is.na(weighted)) %>%
  mutate(
    victim_rate_per_100k   = n_victims   / weighted * 100000,
    offender_rate_per_100k = n_offenders / weighted * 100000
  )

as_tibble(combined_table) %>% print(n = Inf, width = Inf) # DO NOT DO THIS BRUH

write_csv(combined_table, "results/nibrs_acs_combined_race_ethnicity_age_state_sex.csv")

## ---- Logit model -----------------------------------------------------
# Binomial GLM: ACS population is the trial count (N), NIBRS offender count
# is the success count (Y). p_rtsa = P(offender | race, year, state, age, sex).
# Swap n_offenders for n_victims below to fit the victim-side model instead.

model_data <- combined_table %>%
  filter(
    !str_detect(race_ethnicity, "NA"),
    age_group_5yr != "Unknown",
    !age_group_5yr %in% c("0-4", "5-9", "10-14"),
    !is.na(sex)
  ) %>%
  mutate(
    race_ethnicity = relevel(factor(race_ethnicity), ref = "White, Not Hispanic/Latino"),
    age_group_5yr  = relevel(factor(age_group_5yr),  ref = "15-19"),
    year           = factor(year),
    state_fips     = factor(state_fips),
    sex            = relevel(factor(sex), ref = "Male")
  )

# sanity check: population (trials) must be >= offender count (successes)
stopifnot(all(model_data$weighted >= model_data$n_offenders))

# heads-up: race x year x state x age x sex is a LOT of cells (51 states x
# 4 years x 12 race/ethnicity groups x 13 age bands x 2 sexes) -- expect
# many state coefficients to be imprecise for small race groups (AIAN,
# NHPI) in small states. Check cell sparsity before trusting state-level
# SEs; consider glmmTMB with state as a random effect if fixed-effect
# estimates look unstable.
table(model_data$state_fips) %>% summary()

m_offenders <- glm(
  cbind(n_offenders, weighted - n_offenders) ~ race_ethnicity + year + age_group_5yr + state_fips + sex,
  family = binomial(link = "logit"),
  data   = model_data
)

summary(m_offenders)

# odds ratios with 95% CI (easier to read than raw log-odds coefficients)
exp(cbind(OR = coef(m_offenders), confint(m_offenders)))

# dispersion check: residual deviance / df should be near 1 under binomial;
# well above 1 signals overdispersion (common in count/rate criminology data)
m_offenders$deviance / m_offenders$df.residual

# actual number of data points we have, between NIBRS and ACS
nrow(model_data)
nrow(combined_table) - nrow(model_data)  # rows dropped by the filters above

# breakdown of why rows got dropped
combined_table %>%
  mutate(
    dropped_reason = case_when(
      str_detect(race_ethnicity, "NA") ~ "missing race",
      age_group_5yr == "Unknown" ~ "missing age",
      age_group_5yr %in% c("0-4", "5-9", "10-14") ~ "under 15",
      TRUE ~ "kept"
    )
  ) %>%
  count(dropped_reason)

# summarize

## ---- NB model: offenders as a function of victims ---------------------
# Different question from the binomial logit above. That model asked
# "what's the offending RATE relative to population" (population = trials).
# This one asks "how many offenders does a given number of VICTIMS of the
# same demographic group translate to, and does that ratio differ by
# race/ethnicity" -- n_victims is the exposure/predictor here, population
# doesn't enter this model at all.
#
# NOTE: n_victims and n_offenders in combined_table are each counted on
# their OWN demographics (victims_table and offenders_table were built
# separately, joined only on shared race/year/state/age/sex keys) -- NOT
# via incident-level linkage (ORI + INCNUM / incident_key). So this model
# answers "within a given demographic group, do victim counts and offender
# counts move together across state/year/age/sex cells" -- a group-level
# (ecological) association, not the incident-level P(offender race |
# victim race) relationship the original project plan also calls for.
# Both are useful; they are not the same estimate.
pacman::p_load(glmmTMB)

# log(n_victims) is undefined at 0 -- drop cells with zero victims. An
# offender count with no victims in the same demographic cell doesn't
# inform a victims -> offenders ratio for that cell (it's still valid
# data, just not usable as a row in *this* model).
nb_data <- model_data %>%
  filter(n_victims > 0)

n_zero_victim_cells <- sum(model_data$n_victims == 0)
message(n_zero_victim_cells, " cells with zero victims dropped for the NB model (kept in model_data/the binomial model above).")

# Race-specific intercept AND slope on log(n_victims), fit as random
# effects rather than a fully separate fixed interaction per race. This
# partially pools the victims -> offenders ratio across race/ethnicity
# groups: small groups (AIAN, NHPI) borrow strength from the overall
# pattern instead of getting a wildly unstable slope estimated from only
# their own (sparse) cells. Per-race point estimates -- combining the
# fixed effect with each group's random deviation (BLUPs) -- come out of
# coef(m_nb), not a separate race_ethnicity fixed-effect block, since a
# fixed race_ethnicity term AND a race_ethnicity random effect on the same
# variable would be redundant/aliased with each other.
m_nb <- glmmTMB(
  n_offenders ~ log(n_victims) + year + age_group_5yr + state_fips + sex +
    (1 + log(n_victims) | race_ethnicity),
  family = nbinom2(link = "log"),
  data   = nb_data
)

summary(m_nb)

# per-race intercept + slope (fixed effect + that race's random deviation)
# -- this is the race-specific victims -> offenders translation to carry
# forward to the NVSS projection step.
coef(m_nb)$cond$race_ethnicity

# incidence rate ratios: exp(coef) here means "multiplicative change in
# EXPECTED OFFENDER COUNT" (a count-model IRR), NOT an odds ratio -- this
# model has a log link on a count outcome, not a logit link on a
# proportion, so don't read these the same way as the binomial ORs above.
exp(fixef(m_nb)$cond)

# dispersion / fit check for the NB model (analogous to the binomial
# deviance/df check above, different mechanics under the hood)
summary(m_nb)$AICtab

## ---- NB model variants: does population matter beyond victims? --------
# m_nb above deliberately left population out -- per the original project
# plan, population "enters as an offset only if a per-capita rate
# interpretation is wanted, otherwise it's implicit in the victim count
# itself." That's a simplifying assumption, not a settled fact: if
# population independently drives BOTH victim and offender counts (bigger
# places mechanically have more of both), log(n_victims)'s coefficient in
# m_nb could partly be standing in for population size rather than a real
# victims -> offenders relationship. These two variants test that.
#
#   m_nb   (above): log(n_victims) only               -- population absent entirely
#   m_nb2  (below): log(n_victims) + log(weighted)     -- population as a FREE covariate
#   m_nb3  (below): log(n_victims) + offset(log(weighted)) -- population forced to
#                    scale exactly 1:1 (the same assumption the binomial
#                    logit model made by using population as trials)
#
# m_nb is nested inside m_nb2 (the restriction population's coefficient = 0);
# m_nb3 is a different constrained case of m_nb2 (population's coefficient
# fixed at exactly 1 instead of freely estimated).

# The convergence warnings from m_nb2/m_nb3 are a scale-mismatch problem,
# not evidence the model is wrong: log(weighted) sits around 9-15
# (population counts in the tens of thousands to millions) while
# log(n_victims) sits around 0-6 -- that gap, on top of the random-slope
# structure below, makes the likelihood surface hard for TMB to navigate.
#
# Fix: center/scale ONLY the newly-added population term. log(n_victims)
# is left exactly as in m_nb, so its coefficient stays directly comparable
# across all three models and m_nb's nesting inside m_nb2 still holds.
#   log_weighted_z: centered AND scaled -- used as the FREE covariate in
#     m_nb2. Its coefficient is estimated anyway, so rescaling just moves
#     it to "per SD of log-population" units rather than "per unit" --
#     doesn't change what it means substantively.
#   log_weighted_c: centered ONLY (not scaled) -- used as the OFFSET in
#     m_nb3. Scaling an offset would break the "population scales exactly
#     1:1" assumption the offset exists to impose; centering just shifts
#     the intercept by a constant and changes nothing else.
nb_data <- nb_data %>%
  mutate(
    log_weighted_z = as.numeric(scale(log(weighted))),
    log_weighted_c = as.numeric(scale(log(weighted), scale = FALSE))
  )

m_nb2 <- glmmTMB(
  n_offenders ~ log(n_victims) + log_weighted_z + year + age_group_5yr + state_fips + sex +
    (1 + log(n_victims) | race_ethnicity),
  family  = nbinom2(link = "log"),
  data    = nb_data,
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

m_nb3 <- glmmTMB(
  n_offenders ~ log(n_victims) + offset(log_weighted_c) + year + age_group_5yr + state_fips + sex +
    (1 + log(n_victims) | race_ethnicity),
  family  = nbinom2(link = "log"),
  data    = nb_data,
  control = glmmTMBControl(optCtrl = list(iter.max = 1e4, eval.max = 1e4))
)

summary(m_nb2)
summary(m_nb3)

# FALLBACK -- only if either model above still throws a convergence
# warning. The random-effect correlation between intercept and slope was
# already sitting near its boundary (-0.96) in m_nb, which is often what
# blocks convergence once another term is added; dropping the estimated
# correlation frees up the optimizer. This is a real change to the random-
# effects structure, not just a rescaling, so treat it as a fallback, not
# the default. Swap the random-effects term in either model to:
#   (1 | race_ethnicity) + (0 + log(n_victims) | race_ethnicity)

# quick collinearity check between the two exposure variables -- if this
# is very high (roughly > 0.8-0.9), expect m_nb2's individual coefficients
# (population and victims both) to be less stable/more uncertain than
# either model's overall fit would suggest.
cor(log(nb_data$n_victims), log(nb_data$weighted))

# does population add anything beyond what victim count already explains?
# m_nb is nested in m_nb2, so this is a valid likelihood ratio test of
# H0: population's coefficient = 0.
anova(m_nb, m_nb2)

# how far is population's freely-estimated effect (in m_nb2) is from the
# "exactly proportional" assumption m_nb3 imposes. log_weighted_z is
# standardized, so divide by its SD to get back to the raw log(weighted)
# scale before comparing to 1 (the coefficient the offset in m_nb3
# effectively imposes). Close to 1 -> m_nb3's offset is a reasonable
# simplification; far from 1 -> the offset is forcing something the data
# doesn't actually support.
fixef(m_nb2)$cond["log_weighted_z"] / sd(log(nb_data$weighted))

# side-by-side fit comparison across all three specifications
AIC(m_nb, m_nb2, m_nb3)

# does log(n_victims) still matter once population is accounted for,
# under either way of including it? (directly comparable across all
# three -- log(n_victims) was left unscaled in every model)
list(
  victims_only            = fixef(m_nb)$cond["log(n_victims)"],
  victims_plus_pop_free   = fixef(m_nb2)$cond["log(n_victims)"],
  victims_plus_pop_offset = fixef(m_nb3)$cond["log(n_victims)"]
)

## ---- Plain NB regression: race + year + state + age + sex, -------------
## ---- ACS population as the exposure -------------------------------------
# Starting over, simple version: no victims term, no random effects/partial
# pooling -- just a standard negative binomial regression with race, year,
# age, state, and sex as ordinary fixed effects (same reference categories
# as the binomial logit model above), and ACS population as the exposure.
#
# "Weight by ACS population" here means population enters as
# offset(log(weighted)) -- the standard way a Poisson/NB count model
# incorporates a population denominator (same idea as the binomial model
# using population as trials, just on the count/log-link scale instead of
# the proportion/logit scale). It is NOT a `weights =` argument (that
# means something different -- prior weights on the likelihood, e.g. for
# survey weighting -- not a population-at-risk exposure).
m_nb_simple <- MASS::glm.nb(
  n_offenders ~ race_ethnicity + year + age_group_5yr + state_fips + sex + offset(log(weighted)),
  data = model_data
)

summary(m_nb_simple)

# incidence rate ratios (IRR) with 95% CI -- exp(coef) here means
# "multiplicative change in expected OFFENDER COUNT," not an odds ratio
# (this model has a log link on a count outcome, not a logit link on a
# proportion). Using confint.default (Wald-based) instead of profile
# CIs -- profiling this many parameters took a long time for the earlier
# binomial model; swap in confint() for profile-likelihood CIs if you want
# the more accurate (but slower) version.
exp(cbind(IRR = coef(m_nb_simple), confint.default(m_nb_simple)))

# dispersion: MASS::glm.nb estimates theta directly as part of fitting
# (unlike glm(family=poisson), which would need a separate check). Small
# theta = a lot of overdispersion beyond what Poisson would allow.
m_nb_simple$theta
m_nb_simple$SE.theta

# AIC, for comparing against the other NB variants above if useful
AIC(m_nb_simple)

## ---- Plain NB regression: victims + race + year + state + age + sex, ---
## ---- ACS population as a CONTROL (not an offset) ------------------------
# Same simple, no-random-effects style as m_nb_simple, but two changes:
#   1. log(n_victims) is added back in as an ordinary fixed effect.
#   2. Population enters as log(weighted), a freely estimated control
#      variable -- NOT offset(log(weighted)). That means the model
#      estimates how much population matters on its own, instead of
#      assuming it scales the offender count exactly 1:1. This directly
#      answers the earlier question of whether victims still predict
#      offenders once population size is accounted for.
#
# log(n_victims) is undefined at 0, so this reuses nb_data (already
# filtered to n_victims > 0 earlier in this script) instead of model_data.
m_nb_simple2 <- MASS::glm.nb(
  n_offenders ~ log(n_victims) + log(weighted) + race_ethnicity + year + age_group_5yr + state_fips + sex,
  data = nb_data
)

summary(m_nb_simple2)

# incidence rate ratios (IRR) with 95% CI
exp(cbind(IRR = coef(m_nb_simple2), confint.default(m_nb_simple2)))

# log(n_victims) and log(weighted) are likely correlated (more population
# tends to mean more victims) -- check before trusting either coefficient
# individually. High correlation (roughly > .8-.9) means treat both
# coefficients with some caution even if the overall model fits fine.
cor(log(nb_data$n_victims), log(nb_data$weighted))

# dispersion
m_nb_simple2$theta
m_nb_simple2$SE.theta

AIC(m_nb_simple2)
