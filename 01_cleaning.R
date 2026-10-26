# =============================================================================
# 01_cleaning.R 

#
# Prerequisite: run 00_setup.R first (creates `values`, `labels`,
#               `question_text`). This script starts from `values`.
#
#
# CONVENTIONS (apply throughout):
#   - Never overwrite raw `values`. Work on `dat`, then `dat2`.
#   - Ordered factors: hand-specified level orders. Opt-out responses
#     ("Prefer not to say", "Unsure") fall outside the scale -> NA in the
#     *_ord column, but the original column is preserved.
#   - Multi-selects ("select all that apply", comma-combined cells) -> split
#     into TRUE/FALSE indicator columns via str_detect on a UNIQUE anchor.
#     *** WATCH FOR SUBSTRING COLLISIONS *** e.g. "Pig" matches "Guinea Pig";
#     "Small" matches "Extra small"; "Large" matches "Large predators";
#     "Pig (s)" needed escaped parens. Anchor on text that cannot nest.
#   - Internal-comma labels (e.g. "Reptiles - turtle, snake, frog"): NEVER
#     split on comma; detect on the unique prefix instead.
#   - Verify everything: ordered factors via NA counts (typo detector);
#     multi-selects via "did anyone fall through" + per-category sums.
#     Branch-NA is "not applicable", not missing data, so NA counts are large.
#
# R version: 4.2.2
# =============================================================================


# --- 1. Row filtering --------------------------------------------------------
dat <- filter(values, Status == "IP Address")          # 1031 -> 1018
dat <- filter(dat, Finished == TRUE)                    # 1018 -> 1006
dat <- filter(dat, `Duration (in seconds)` >= 120)      # 1006 -> 1004



# --- 2. Duplicate / IP checks (diagnostic only) ------------------------------
dat |> count(PROLIFIC_PID, sort = TRUE) |> filter(n > 1)   # expect 0 rows


# --- 3. Batch labeling -------------------------------------------------------
dat <- dat |>
  mutate(batch = case_when(
    STUDY_ID == "6a207e757a1f577556885722" ~ "demographic",   # n = 605
    STUDY_ID == "6a2afd6b39e8a953935d82f2" ~ "geographic",    # n = 399 (oversamples
    TRUE ~ NA_character_                                       #   low-reporting geos)
  ))


# --- 4. Column trimming ------------------------------------------------------
dat2 <- dat |>
  select(-Status, -Progress, -Finished, -DistributionChannel,
         -UserLanguage, -Q_RecaptchaScore, -Q107, -Q108, -SESSION_ID)


# --- 5. Q1 DEMOGRAPHICS ------------------------------------------------------
dat2 <- dat2 |>
  mutate(across(c(Q1.2, Q1.3, Q1.7, Q1.8, Q1.9, Q1.12), as.factor))

income_levels <- c(
  "Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999",
  "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999",
  "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $89,999",
  "$90,000 - $99,999", "$100,000 - $149,999", "$150,000 - $199,999",
  "$200,000 or more")
age_levels   <- c("18 - 24", "25 - 34", "35 - 44", "45 - 54",
                  "55 - 64", "65 - 74", "75 - 84")
edu_levels   <- c("Less than high school", "High school graduate",
                  "Some college", "Associate's degree", "Bachelor's degree",
                  "Master's degree", "Professional degree (MD, JD, DDS, etc.)",
                  "Doctorate degree")
pol_levels   <- c("Very liberal", "Liberal", "Moderate",
                  "Conservative", "Very conservative")   # Unsure/Prefer-not -> NA
child_levels <- c("None", "1", "2", "3", "4", "5 or more")
drive_levels <- c("Never, I do not drive", "Rarely or almost never",
                  "A few times a year", "A few times a month",
                  "A few times a week", "Daily or almost every day")

dat2 <- dat2 |>
  mutate(
    Q1.5_ord  = factor(Q1.5,  levels = income_levels, ordered = TRUE),
    Q1.1_ord  = factor(Q1.1,  levels = age_levels,    ordered = TRUE),
    Q1.6_ord  = factor(Q1.6,  levels = edu_levels,    ordered = TRUE),
    Q1.10_ord = factor(Q1.10, levels = pol_levels,    ordered = TRUE),
    Q1.11_ord = factor(Q1.11, levels = child_levels,  ordered = TRUE),
    Q1.14_ord = factor(Q1.14, levels = drive_levels,  ordered = TRUE)
  )
# Verify NA: Q1.1_ord 0 | Q1.5_ord 29 | Q1.6_ord 8 | Q1.10_ord 46 | Q1.11_ord 8 | Q1.14_ord 0


# --- 6. Q2 PETS --------------------------------------------------------------
# 6a. Q2.2 pet type (detect dog/cat substrings to also catch the "both" option)
dat2 <- dat2 |>
  mutate(
    owns_dog   = str_detect(Q2.2, "dog")       & !is.na(Q2.2),   # 550
    owns_cat   = str_detect(Q2.2, "cat")       & !is.na(Q2.2),   # 431
    owns_other = str_detect(Q2.2, "Other pet") & !is.na(Q2.2)    # 81
  )
# 6b. Q2.3 other-pet -> 7 buckets. "Pig \\(s\\)" avoids matching "Guinea Pig".
dat2 <- dat2 |>
  mutate(
    other_small_mammal = str_detect(Q2.3, "Small mammal")                 & !is.na(Q2.3),
    other_bird         = str_detect(Q2.3, "Bird|Poultry|Chicken")         & !is.na(Q2.3),
    other_reptile      = str_detect(Q2.3, "Reptile")                      & !is.na(Q2.3),
    other_amphibian    = str_detect(Q2.3, "Amphibian")                    & !is.na(Q2.3),
    other_fish_aquatic = str_detect(Q2.3, "Fish|Hermit Crab|mollusk")     & !is.na(Q2.3),
    other_livestock    = str_detect(Q2.3, "Horse|Donkey|Mule|Goat|Pig \\(s\\)") & !is.na(Q2.3),
    other_exotic       = str_detect(Q2.3, "Insects|Other animal species") & !is.na(Q2.3)
  )
# 6c. Q2.6 dog size -> anchor on WEIGHT RANGES (band names nest). "." = en-dash.
dat2 <- dat2 |>
  mutate(
    dog_toy         = str_detect(Q2.6, "under 10 lbs") & !is.na(Q2.6),
    dog_small       = str_detect(Q2.6, "10.25 lbs")    & !is.na(Q2.6),
    dog_medium      = str_detect(Q2.6, "25.50 lbs")    & !is.na(Q2.6),
    dog_large       = str_detect(Q2.6, "50.90 lbs")    & !is.na(Q2.6),
    dog_giant       = str_detect(Q2.6, "over 90 lbs")  & !is.na(Q2.6),
    dog_size_unsure = str_detect(Q2.6, "still grow")   & !is.na(Q2.6)
  )
# 6d. Count variables (digits + "5 or more" cap -> ordered)
count_levels <- c("1", "2", "3", "4", "5 or more")
dat2 <- dat2 |>
  mutate(
    Q2.4_ord  = factor(Q2.4,  levels = count_levels, ordered = TRUE),
    Q2.8_ord  = factor(Q2.8,  levels = count_levels, ordered = TRUE),
    Q2.16_ord = factor(Q2.16, levels = count_levels, ordered = TRUE),
    Q2.18_ord = factor(Q2.18, levels = count_levels, ordered = TRUE)
  )
# 6e. Ordered scales
travel_freq_dog <- c("I have never been in a vehicle with my dog",
                     "Rarely or almost never", "A few times a year",
                     "A few times a month", "A few times a week",
                     "Daily or almost every day")
travel_freq_cat <- travel_freq_dog[-1]   # cat version lacks the "never" level
distract_levels <- c("Not at all distracting", "Slightly distracting",
                     "Somewhat distracting", "Moderately distracting",
                     "Very distracting")   # convention: Somewhat < Moderately
dat2 <- dat2 |>
  mutate(
    Q2.7_ord  = factor(Q2.7,  levels = travel_freq_dog, ordered = TRUE),
    Q2.19_ord = factor(Q2.19, levels = travel_freq_cat, ordered = TRUE),
    Q2.9_ord  = factor(Q2.9,  levels = distract_levels, ordered = TRUE),
    Q2.22_ord = factor(Q2.22, levels = distract_levels, ordered = TRUE)
  )
# 6f. Single-select factors
dat2 <- dat2 |>
  mutate(across(c(Q2.1, Q2.5, Q2.10, Q2.11, Q2.12, Q2.13, Q2.14,
                  Q2.15, Q2.17, Q2.20, Q2.21, Q2.23, Q2.24), as.factor))


# --- 7. Q3 PET STRUCK BY VEHICLE --------------------------------------------
# 7a. Q3.2 species struck
dat2 <- dat2 |>
  mutate(
    struck_cat   = str_detect(Q3.2, "Cat")          & !is.na(Q3.2),   # 168
    struck_dog   = str_detect(Q3.2, "Dog")          & !is.na(Q3.2),   # 286
    struck_other = str_detect(Q3.2, "Other animal") & !is.na(Q3.2)    # 18
  )
# 7b. Q3.9 decade (note "Before the 1960s" also trips decade_1960s by design)
dat2 <- dat2 |>
  mutate(
    decade_pre1960 = str_detect(Q3.9, "Before the 1960s") & !is.na(Q3.9),
    decade_1960s   = str_detect(Q3.9, "1960s") & !is.na(Q3.9),
    decade_1970s   = str_detect(Q3.9, "1970s") & !is.na(Q3.9),
    decade_1980s   = str_detect(Q3.9, "1980s") & !is.na(Q3.9),
    decade_1990s   = str_detect(Q3.9, "1990s") & !is.na(Q3.9),
    decade_2000s   = str_detect(Q3.9, "2000s") & !is.na(Q3.9),
    decade_2010s   = str_detect(Q3.9, "2010s") & !is.na(Q3.9),
    decade_2020s   = str_detect(Q3.9, "2020s") & !is.na(Q3.9)
  )
# 7c. Ordered scales + Q3.3/Q3.8
work_missed_levels <- c("< 1 Workday", "1 - 2 Workdays", "2 - 3 Workdays",
                        "3 - 4 Workdays", "5 - 6 Workdays", "> 6  Workdays")  # NB double space
behav_dur_levels   <- c("<1 week", "Between 1 - 2 weeks",
                        "Between 2 weeks - 1 month", "1 Month - 2 Months",
                        "3 Months or More",
                        "Indefinitely - My behavior remains changed as a result of the incident")
dat2 <- dat2 |>
  mutate(
    Q3.8_ord  = factor(Q3.8,  levels = count_levels,       ordered = TRUE),
    Q3.7_ord  = factor(Q3.7,  levels = work_missed_levels, ordered = TRUE),
    Q3.11_ord = factor(Q3.11, levels = behav_dur_levels,   ordered = TRUE),
    Q3.3      = as.factor(Q3.3)
  )
# 7d. Q3.10 behavior change -> 8 indicators (distinct tails: residential vs parked)
dat2 <- dat2 |>
  mutate(
    bc_alert_driveways  = str_detect(Q3.10, "alert for animals near driveways") & !is.na(Q3.10),
    bc_slow_residential = str_detect(Q3.10, "residential and neighborhood")     & !is.na(Q3.10),
    bc_cautious_signs   = str_detect(Q3.10, "posted pet or animal crossing")    & !is.na(Q3.10),
    bc_emotional        = str_detect(Q3.10, "emotionally cautious")             & !is.na(Q3.10),
    bc_slow_parked      = str_detect(Q3.10, "parked cars")                      & !is.na(Q3.10),
    bc_no_change        = str_detect(Q3.10, "did not change")                   & !is.na(Q3.10),
    bc_unsure           = str_detect(Q3.10, "not sure / I don't remember")      & !is.na(Q3.10),
    bc_other            = str_detect(Q3.10, "Another behavior not shown")       & !is.na(Q3.10)
  )
# 7e. Q3.5 vet cost: strip -> numeric (warning expected), then bucket
dat2 <- dat2 |>
  mutate(
    vet_cost_num = Q3.5 |> str_remove_all("[$,]") |> str_remove_all("\\s") |> as.numeric(),
    vet_cost_bucket = cut(vet_cost_num,
      breaks = c(-Inf, 250, 500, 1000, 2500, 5000, Inf),
      labels = c("Under $250", "$250-$499", "$500-$999",
                 "$1,000-$2,499", "$2,500-$4,999", "$5,000+"),
      right = FALSE)
  )


# --- 8. Q4 COLLISIONS WHILE DRIVING -----------------------------------------
times_levels <- c("Once", "Twice", "Three times", "Four times", "Five or more times")

# 8a. WILD: count, animal-type multi-select (anchor "Large deer" vs "Large predators"),
dat2 <- dat2 |>
  mutate(
    Q4.2_ord = factor(Q4.2, levels = times_levels, ordered = TRUE),
    across(c(Q4.4, Q4.5, Q4.6, Q4.7, Q4.8, Q4.9, Q4.10), as.factor),
    Q4.12_ord = factor(Q4.12, levels = behav_dur_levels, ordered = TRUE),
    wild_deer_elk   = str_detect(Q4.3, "Large deer")       & !is.na(Q4.3),
    wild_med_mammal = str_detect(Q4.3, "Medium mammals")   & !is.na(Q4.3),
    wild_sml_mammal = str_detect(Q4.3, "Small mammals")    & !is.na(Q4.3),
    wild_predator   = str_detect(Q4.3, "Large predators")  & !is.na(Q4.3),
    wild_bird       = str_detect(Q4.3, "Birds")            & !is.na(Q4.3),
    wild_reptile    = str_detect(Q4.3, "Reptiles or amph") & !is.na(Q4.3),
    wild_farm       = str_detect(Q4.3, "Farm animals")     & !is.na(Q4.3),
    wild_other      = str_detect(Q4.3, "Another animal")   & !is.na(Q4.3),
    wbc_reduce_speed = str_detect(Q4.11, "reduced my speed")       & !is.na(Q4.11),
    wbc_observant    = str_detect(Q4.11, "more observant")         & !is.na(Q4.11),
    wbc_times_of_day = str_detect(Q4.11, "times of day")          & !is.na(Q4.11),
    wbc_avoid_roads  = str_detect(Q4.11, "avoiding certain roads") & !is.na(Q4.11),
    wbc_high_beams   = str_detect(Q4.11, "high beams")            & !is.na(Q4.11),
    wbc_other        = str_detect(Q4.11, "Other behaviors not")    & !is.na(Q4.11)
  )
# 8b. DOG: Q4.21 behavior-change reuses the Q3.10 (pet) option set
dat2 <- dat2 |>
  mutate(
    Q4.14_ord = factor(Q4.14, levels = times_levels,     ordered = TRUE),
    across(c(Q4.13, Q4.15, Q4.16, Q4.17, Q4.18, Q4.19, Q4.20), as.factor),
    Q4.22_ord = factor(Q4.22, levels = behav_dur_levels, ordered = TRUE),
    dbc_alert_driveways  = str_detect(Q4.21, "alert for animals near driveways") & !is.na(Q4.21),
    dbc_slow_residential = str_detect(Q4.21, "residential and neighborhood")     & !is.na(Q4.21),
    dbc_cautious_signs   = str_detect(Q4.21, "posted pet or animal")             & !is.na(Q4.21),
    dbc_emotional        = str_detect(Q4.21, "emotionally cautious")             & !is.na(Q4.21),
    dbc_slow_parked      = str_detect(Q4.21, "parked cars")                      & !is.na(Q4.21)
  )
# 8c. CAT: same as dog PLUS a "cbc_other" ("Other change not listed") this time
dat2 <- dat2 |>
  mutate(
    Q4.24_ord = factor(Q4.24, levels = times_levels,     ordered = TRUE),
    across(c(Q4.23, Q4.25, Q4.26, Q4.27, Q4.28, Q4.29, Q4.30), as.factor),
    Q4.32_ord = factor(Q4.32, levels = behav_dur_levels, ordered = TRUE),
    cbc_alert_driveways  = str_detect(Q4.31, "alert for animals near driveways") & !is.na(Q4.31),
    cbc_slow_residential = str_detect(Q4.31, "residential and neighborhood")     & !is.na(Q4.31),
    cbc_cautious_signs   = str_detect(Q4.31, "posted pet or animal")             & !is.na(Q4.31),
    cbc_emotional        = str_detect(Q4.31, "emotionally cautious")             & !is.na(Q4.31),
    cbc_slow_parked      = str_detect(Q4.31, "parked cars")                      & !is.na(Q4.31),
    cbc_other            = str_detect(Q4.31, "Other change not listed")          & !is.na(Q4.31)
  )


# --- 9. Q5 AV SCENARIOS ------------------------------------------------------
risk_levels <- c("No, I would prioritize my own safety regardless",
                 "Yes, but only if the risk to me was low (e.g., brake hard on an empty road)",
                 "Yes, I would swerve or brake hard even if it put me at risk (e.g., swerve into the opposite lane or off the road)")
dat2 <- dat2 |>
  mutate(
    across(c(Q5.1, Q5.2, Q5.3, Q5.4, Q5.5), as.factor),
    Q5.6_ord = factor(Q5.6, levels = risk_levels, ordered = TRUE),
    Q5.7_ord = factor(Q5.7, levels = risk_levels, ordered = TRUE),
    Q5.8_ord = factor(Q5.8, levels = risk_levels, ordered = TRUE)
  )


# --- 10. Q6 AV PREFERENCES ---------------------------------------------------
comfort_levels <- c("Extremely uncomfortable", "Somewhat uncomfortable",
                    "Neither comfortable nor uncomfortable",
                    "Somewhat comfortable", "Extremely comfortable")
dat2 <- dat2 |>
  mutate(
    across(c(Q6.1, Q6.2, Q6.4, Q6.5, Q6.6, Q6.7, Q6.8, Q6.9,
             Q6.10, Q6.11, Q6.13), as.factor),
    Q6.14_ord = factor(Q6.14, levels = comfort_levels, ordered = TRUE),
    Q6.15_ord = factor(Q6.15, levels = comfort_levels, ordered = TRUE),
    notaxi_unavailable = str_detect(Q6.3, "not available in my area") & !is.na(Q6.3),
    notaxi_distrust    = str_detect(Q6.3, "do not trust")             & !is.na(Q6.3),
    notaxi_expensive   = str_detect(Q6.3, "Too expensive")            & !is.na(Q6.3),
    notaxi_uncomfort   = str_detect(Q6.3, "uncomfortable")            & !is.na(Q6.3),
    notaxi_other       = str_detect(Q6.3, "Other options not")        & !is.na(Q6.3)
  )
# Q6.12_1 .. Q6.12_6 imported numeric (rank 1-6, 64 NA each). No conversion needed.


# --- 11. Save ----------------------------------------------------------------
saveRDS(dat2, "dat_clean_final.rds")   # 1004 x 242
