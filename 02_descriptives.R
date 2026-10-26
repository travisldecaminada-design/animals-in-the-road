
library(tidyverse)
library(tidycensus)
library(flextable)
library(gssr)     # install once: install.packages("gssr",
                  #   repos = c("https://kjhealy.r-universe.dev",
                  #             "https://cloud.r-project.org"))

# census_api_key("YOUR_KEY", install = TRUE)   

dat2 <- readRDS("dat_clean_final.rds")


# --- 1. Q1.4 RACE (deferred from cleaning) ----------------------------------
# Multi-select with THREE substring traps:
#   "African"  matches BOTH "Black or African American" AND "Middle Eastern or
#              North African"      -> anchor on "Black or African"
#   "Native"   matches BOTH "Native Hawaiian..." AND "American Indian or Alaska
#              Native"             -> anchor on "Native Hawaiian" / "American Indian"
#   "Asian"    is a substring of "Asian Indian"
#              -> negative lookahead "Asian(?! Indian)" keeps them SEPARATE
# Also: "Other, not shown above" has an internal comma -> detect, never split.

dat2 <- dat2 |>
  mutate(
    race_white      = str_detect(Q1.4, "White")             & !is.na(Q1.4),  # 741
    race_black      = str_detect(Q1.4, "Black or African")  & !is.na(Q1.4),  # 158
    race_asian      = str_detect(Q1.4, "Asian(?! Indian)")  & !is.na(Q1.4),  # 90
    race_asian_ind  = str_detect(Q1.4, "Asian Indian")      & !is.na(Q1.4),  # 3
    race_aian       = str_detect(Q1.4, "American Indian")   & !is.na(Q1.4),  # 32
    race_mena       = str_detect(Q1.4, "Middle Eastern")    & !is.na(Q1.4),  # 16
    race_nhpi       = str_detect(Q1.4, "Native Hawaiian")   & !is.na(Q1.4),  # 8
    race_other      = str_detect(Q1.4, "Other, not shown")  & !is.na(Q1.4),  # 33
    race_prefer_not = str_detect(Q1.4, "Prefer not to say") & !is.na(Q1.4)   # 8
  )
# VERIFY: all 1000 non-NA respondents land in >= 1 bucket (0 fall through).


# --- 2. CENSUS-COMPARABLE RACE/ETHNICITY (1997 "bridged" standard) ----------
# Published ACS/Census tables still use the 1997 SPD-15 standard: race and
# Hispanic origin asked SEPARATELY, Hispanic reported as "any race", and
# MENA tabulated WITHIN White. 
#
# Three forced mappings:
#   MENA         -> White        (matches 1997 Census tabulation)
#   Asian Indian -> Asian        (detailed subcategory within Asian)
#   2+ groups    -> "Two or more races"  ("alone" means ALONE)
# Detail is NOT lost: the race_* indicators above preserve everything.


dat2 <- dat2 |>
  mutate(
    br_white = race_white | race_mena,
    br_black = race_black,
    br_asian = race_asian | race_asian_ind,
    br_aian  = race_aian,
    br_nhpi  = race_nhpi,
    br_sor   = race_other,
    n_race_groups = br_white + br_black + br_asian + br_aian + br_nhpi + br_sor,
    race_eth_census = case_when(
      Q1.3 == "Yes"                             ~ "Hispanic or Latino (any race)",
      is.na(Q1.3) | Q1.3 == "Prefer not to say" ~ "Not reported",
      n_race_groups >= 2                        ~ "Two or more races",
      n_race_groups == 1 & br_white             ~ "White alone",
      n_race_groups == 1 & br_black             ~ "Black or African American alone",
      n_race_groups == 1 & br_asian             ~ "Asian alone",
      n_race_groups == 1 & br_aian              ~ "American Indian or Alaska Native alone",
      n_race_groups == 1 & br_nhpi              ~ "Native Hawaiian or Pacific Islander alone",
      n_race_groups == 1 & br_sor               ~ "Some other race alone",
      TRUE                                      ~ "Not reported"
    )
  )
saveRDS(dat2, "dat_clean_final.rds")


# --- 3. ACS PUMS BENCHMARKS -------------------------------------------------
## 3a. Race/ethnicity
pums_race <- get_pums(variables = c("AGEP", "RAC1P", "HISP"),
                      state = "all", survey = "acs1", year = 2023)

race_bench_18plus <- pums_race |>
  filter(AGEP >= 18) |>
  mutate(race_eth_census = case_when(
    HISP != "01" ~ "Hispanic or Latino (any race)",   # 01 = NOT Hispanic
    RAC1P == "1" ~ "White alone",
    RAC1P == "2" ~ "Black or African American alone",
    RAC1P %in% c("3","4","5") ~ "American Indian or Alaska Native alone",  # 3 AIAN variants
    RAC1P == "6" ~ "Asian alone",
    RAC1P == "7" ~ "Native Hawaiian or Pacific Islander alone",
    RAC1P == "8" ~ "Some other race alone",
    RAC1P == "9" ~ "Two or more races"
  )) |>
  count(race_eth_census, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))
rm(pums_race); gc()

race_compare <- dat2 |>
  count(race_eth_census, name = "n") |>
  filter(race_eth_census != "Not reported") |>   # no Census equivalent
  mutate(sample_pct = round(100 * n / sum(n), 1)) |>
  left_join(race_bench_18plus |> select(race_eth_census, census_pct),
            by = "race_eth_census") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  arrange(desc(sample_pct))

## 3b. Age, sex, education, income
pums2 <- get_pums(variables = c("AGEP", "SEX", "SCHL", "HINCP", "ADJINC"),
                  state = "all", survey = "acs1", year = 2023)

age_levels <- c("18 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65 - 74","75 - 84")

age_bench <- pums2 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(age_band = cut(AGEP, breaks = c(18,25,35,45,55,65,75,85),
                        labels = age_levels, right = FALSE)) |>  # left-closed
  count(age_band, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1),
         age_band = as.character(age_band))

# NB: ordered factors won't left_join to plain factors -> convert to character.
age_compare <- dat2 |>
  count(Q1.1_ord, name = "n") |> filter(!is.na(Q1.1_ord)) |>
  mutate(sample_pct = round(100 * n / sum(n), 1),
         age_band = as.character(Q1.1_ord)) |>
  left_join(age_bench |> select(age_band, census_pct), by = "age_band") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(age_band, n, sample_pct, census_pct, diff)

sex_bench <- pums2 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(sex = if_else(SEX == "1", "Male", "Female")) |>
  count(sex, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))


sex_compare <- dat2 |>
  filter(Q1.2 %in% c("Male","Female")) |>
  count(Q1.2, name = "n") |>
  mutate(sample_pct = round(100 * n / sum(n), 1), sex = as.character(Q1.2)) |>
  left_join(sex_bench |> select(sex, census_pct), by = "sex") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(sex, n, sample_pct, census_pct, diff)

edu_levels <- c("Less than high school","High school graduate","Some college",
                "Associate's degree","Bachelor's degree","Master's degree",
                "Professional degree (MD, JD, DDS, etc.)","Doctorate degree")

# SCHL codes: 01-15 = < HS | 16-17 = HS/GED | 18-19 = some college |
#             20 = AA | 21 = BA | 22 = MA | 23 = professional | 24 = doctorate
edu_bench <- pums2 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(edu = case_when(
    SCHL %in% sprintf("%02d", 1:15) ~ "Less than high school",
    SCHL %in% c("16","17")          ~ "High school graduate",
    SCHL %in% c("18","19")          ~ "Some college",
    SCHL == "20" ~ "Associate's degree",
    SCHL == "21" ~ "Bachelor's degree",
    SCHL == "22" ~ "Master's degree",
    SCHL == "23" ~ "Professional degree (MD, JD, DDS, etc.)",
    SCHL == "24" ~ "Doctorate degree"
  )) |>
  count(edu, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))


edu_compare <- dat2 |>
  count(Q1.6_ord, name = "n") |> filter(!is.na(Q1.6_ord)) |>
  mutate(sample_pct = round(100 * n / sum(n), 1), edu = as.character(Q1.6_ord)) |>
  left_join(edu_bench |> select(edu, census_pct), by = "edu") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(edu, n, sample_pct, census_pct, diff)

income_levels <- c("Less than $10,000","$10,000 - $19,999","$20,000 - $29,999",
                   "$30,000 - $39,999","$40,000 - $49,999","$50,000 - $59,999",
                   "$60,000 - $69,999","$70,000 - $79,999","$80,000 - $89,999",
                   "$90,000 - $99,999","$100,000 - $149,999","$150,000 - $199,999",
                   "$200,000 or more")

# tidycensus returns ADJINC ALREADY DIVIDED (e.g. 1.019518,
# not the raw integer 1019518). Do NOT divide by 1e6 -- that collapses every
# household into the bottom bin. Multiply directly.

income_bench <- pums2 |>
  filter(AGEP >= 18, AGEP <= 84, !is.na(HINCP)) |>
  mutate(hh_income = HINCP * as.numeric(ADJINC),
         income_band = cut(hh_income,
           breaks = c(-Inf,10000,20000,30000,40000,50000,60000,70000,
                      80000,90000,100000,150000,200000,Inf),
           labels = income_levels, right = FALSE)) |>
  count(income_band, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1),
         income_band = as.character(income_band))

income_compare <- dat2 |>
  count(Q1.5_ord, name = "n") |> filter(!is.na(Q1.5_ord)) |>
  mutate(sample_pct = round(100 * n / sum(n), 1),
         income_band = as.character(Q1.5_ord)) |>
  left_join(income_bench |> select(income_band, census_pct), by = "income_band") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(income_band, n, sample_pct, census_pct, diff)

rm(pums2); gc()

## 3c. Marital, veteran, state
pums4 <- get_pums(variables = c("AGEP", "MAR", "MIL"),
                  state = "all", survey = "acs1", year = 2023)

mar_bench <- pums4 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(marital = case_when(MAR == "1" ~ "Married",   MAR == "2" ~ "Widowed",
                             MAR == "3" ~ "Divorced",  MAR == "4" ~ "Separated",
                             MAR == "5" ~ "Never married")) |>
  count(marital, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))

mar_compare <- dat2 |>
  filter(!is.na(Q1.8), Q1.8 != "Prefer not to say") |>   # no Census equivalent
  count(Q1.8, name = "n") |>
  mutate(sample_pct = round(100 * n / sum(n), 1), marital = as.character(Q1.8)) |>
  left_join(mar_bench |> select(marital, census_pct), by = "marital") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(marital, n, sample_pct, census_pct, diff) |> arrange(desc(sample_pct))


vet_bench <- pums4 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(veteran = case_when(MIL == "1" ~ "Active Duty",
                             MIL == "2" ~ "Veteran",
                             MIL %in% c("3","4") ~ "Non-Veteran")) |>
  count(veteran, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))

vet_compare <- dat2 |>
  filter(!is.na(Q1.9), Q1.9 != "Prefer not to say") |>
  count(Q1.9, name = "n") |>
  mutate(sample_pct = round(100 * n / sum(n), 1), veteran = as.character(Q1.9)) |>
  left_join(vet_bench |> select(veteran, census_pct), by = "veteran") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |>
  select(veteran, n, sample_pct, census_pct, diff) |> arrange(desc(sample_pct))

# GEOGRAPHY
state_xwalk <- fips_codes |> select(state_code, state_name) |>
  distinct() |> filter(state_code <= "56")   # drop territories

state_bench <- pums4 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  count(STATE, wt = PWGTP, name = "pop") |>
  left_join(state_xwalk, by = c("STATE" = "state_code")) |>
  mutate(census_pct = round(100 * pop / sum(pop), 2)) |>
  select(state_name, census_pct)

# full_join, NOT left_join
state_compare <- dat2 |>
  filter(!is.na(Q1.12)) |>
  count(Q1.12, name = "n_total") |>
  mutate(pooled_pct = round(100 * n_total / sum(n_total), 2),
         state_name = as.character(Q1.12)) |>
  full_join(state_bench, by = "state_name") |>
  mutate(diff = round(pooled_pct - census_pct, 2)) |>
  arrange(desc(census_pct)) |>
  select(state_name, n_total, pooled_pct, census_pct, diff)


state_sample <- dat2 |>
  filter(!is.na(Q1.12)) |>
  count(batch, Q1.12, name = "n") |>
  group_by(batch) |> mutate(pct = round(100 * n / sum(n), 2)) |> ungroup() |>
  select(batch, state_name = Q1.12, n, pct) |>
  pivot_wider(names_from = batch, values_from = c(n, pct))

rm(pums4); gc()

## 3d. Employment

pums6 <- get_pums(variables = c("AGEP", "ESR", "WKHP"),
                  state = "all", survey = "acs1", year = 2023)

emp_bench <- pums6 |>
  filter(AGEP >= 18, AGEP <= 84) |>
  mutate(employment = case_when(
    ESR %in% c("1","2","4","5") & WKHP >= 35 ~ "Employed full time",
    ESR %in% c("1","2","4","5") & WKHP <  35 ~ "Employed part time",
    ESR == "3" ~ "Unemployed",          # ESR 3 = actively seeking = our wording
    ESR == "6" ~ "Not in labor force"
  )) |>
  count(employment, wt = PWGTP, name = "pop") |>
  mutate(census_pct = round(100 * pop / sum(pop), 1))

emp_compare <- dat2 |>
  filter(!is.na(Q1.7), Q1.7 != "Prefer not to say") |>
  mutate(employment = case_when(
    Q1.7 == "Employed full time"          ~ "Employed full time",
    Q1.7 == "Employed part time"          ~ "Employed part time",
    Q1.7 == "Unemployed looking for work" ~ "Unemployed",
    Q1.7 %in% c("Retired","Student","Disabled or unable to work",
                "Unemployed not looking for work") ~ "Not in labor force"
  )) |>
  count(employment, name = "n") |>
  mutate(sample_pct = round(100 * n / sum(n), 1)) |>
  left_join(emp_bench |> select(employment, census_pct), by = "employment") |>
  mutate(diff = round(sample_pct - census_pct, 1)) |> arrange(desc(sample_pct))

rm(pums6); gc()


# --- 4. GSS BENCHMARK: children ever had ------------------------------------

gss22 <- gss_get_yr(2022)   # variable names are LOWERCASE

kids_bench <- gss22 |>
  filter(!is.na(childs), !is.na(age), age >= 18, age <= 84, !is.na(wtssps)) |>
  mutate(kids = case_when(childs == 0 ~ "None", childs == 1 ~ "1",
                          childs == 2 ~ "2",    childs == 3 ~ "3",
                          childs == 4 ~ "4",    childs >= 5 ~ "5 or more")) |>
  count(kids, wt = wtssps, name = "pop") |>
  mutate(gss_pct = round(100 * pop / sum(pop), 1))

kids_compare <- dat2 |>
  count(Q1.11_ord, name = "n") |> filter(!is.na(Q1.11_ord)) |>
  mutate(sample_pct = round(100 * n / sum(n), 1),
         kids = as.character(Q1.11_ord)) |>
  left_join(kids_bench |> select(kids, gss_pct), by = "kids") |>
  mutate(diff = round(sample_pct - gss_pct, 1)) |>
  select(kids, n, sample_pct, gss_pct, diff)


kids_by_age_gss <- gss22 |>
  filter(!is.na(childs), !is.na(age), age >= 18, age <= 84, !is.na(wtssps)) |>
  mutate(kids_num = pmin(as.numeric(childs), 5),   # top-code at 5 to match ours
         wt       = as.numeric(wtssps),            # strip the labelled class
         age_band = cut(as.numeric(age), breaks = c(18,25,35,45,55,65,75,85),
                        labels = age_levels, right = FALSE)) |>
  group_by(age_band) |>
  summarise(gss_pct_none  = round(100 * weighted.mean(kids_num == 0, wt), 1),
            gss_mean_kids = round(weighted.mean(kids_num, wt), 2),
            .groups = "drop") |>
  mutate(age_band = as.character(age_band))

rm(gss22); gc()   # ~47 MB


# --- 5. GALLUP BENCHMARK: political ideology --------------------------------

gallup_ideology <- tibble::tribble(
  ~ideology,      ~gallup_pct,
  "Liberal",       29.2,   # 28 / 96 * 100
  "Moderate",      34.4,   # 33 / 96 * 100
  "Conservative",  36.5    # 35 / 96 * 100
)

pol_compare <- dat2 |>
  filter(!is.na(Q1.10_ord)) |>
  mutate(ideology = case_when(
    Q1.10_ord %in% c("Very liberal","Liberal")          ~ "Liberal",
    Q1.10_ord == "Moderate"                             ~ "Moderate",
    Q1.10_ord %in% c("Conservative","Very conservative") ~ "Conservative"
  )) |>
  count(ideology, name = "n") |>
  mutate(sample_pct = round(100 * n / sum(n), 1)) |>
  left_join(gallup_ideology, by = "ideology") |>
  mutate(diff = round(sample_pct - gallup_pct, 1))


# --- 6. FHWA BENCHMARK: driving status --------------------------------------
# SOURCE: FHWA Highway Statistics 2023, Table DL-20 (published Jan 2025).
#   https://www.fhwa.dot.gov/policyinformation/statistics/2023/dl20.cfm


fhwa_licensed <- tibble::tribble(
  ~age_band,   ~fhwa_licensed_pct,
  "18 - 24",   78.0,   # 18 (60.3), 19 (68.4), 20-24 (81.3)
  "25 - 34",   90.3,   # 25-29 (89.9), 30-34 (90.7)
  "35 - 44",   92.0,   # 35-39 (92.2), 40-44 (91.8)
  "45 - 54",   92.4,   # 45-49 (92.3), 50-54 (92.4)
  "55 - 64",   92.9,   # 55-59 (92.8), 60-64 (92.9)
  "65 - 74",   92.4,   # 65-69 (92.8), 70-74 (92.0)
  "75 - 84",   89.0    # 75-79 (90.7), 80-84 (87.2)
)

driving_compare <- dat2 |>
  filter(!is.na(Q1.1_ord), !is.na(Q1.14_ord)) |>
  mutate(drives = Q1.14_ord != "Never, I do not drive") |>
  group_by(age_band = as.character(Q1.1_ord)) |>
  summarise(n = n(),
            sample_pct_drives = round(100 * mean(drives), 1), .groups = "drop") |>
  left_join(fhwa_licensed, by = "age_band") |>
  mutate(diff = round(sample_pct_drives - fhwa_licensed_pct, 1))


# --- 7. SAVE ALL BENCHMARKS -------------------------------------------------
saveRDS(list(race = race_compare, age = age_compare, sex = sex_compare,
             income = income_compare, edu = edu_compare, marital = mar_compare,
             veteran = vet_compare, state = state_compare,
             state_batch = state_sample, employment = emp_compare,
             driving = driving_compare, fhwa_source = fhwa_licensed,
             kids = kids_compare, kids_by_age = kids_by_age_gss,
             politics = pol_compare, gallup_source = gallup_ideology),
        "census_benchmarks.rds")


# --- 8. TABLES --------------------------------------------------------------
# Three tables, because the grain differs:
#   Table 1 -- core demographics (benchmark cols blank where none exists)
#   Table 2 -- driving status by age (different comparison: FHWA, not Census)
#   Table 3 -- geography (51 rows would swamp Table 1)


std <- function(df, var_label, cat_col, bench_col, source_label) {
  df |> transmute(
    Variable      = var_label,
    Category      = as.character(.data[[cat_col]]),
    n             = n,
    `Sample %`    = sample_pct,
    `Benchmark %` = .data[[bench_col]],
    Diff          = diff,
    Source        = source_label
  )
}

table1 <- bind_rows(
  std(age_compare,    "Age",              "age_band",        "census_pct", "ACS 2023 PUMS"),
  std(sex_compare,    "Sex",              "sex",             "census_pct", "ACS 2023 PUMS"),
  std(race_compare,   "Race/ethnicity",   "race_eth_census", "census_pct", "ACS 2023 PUMS"),
  std(edu_compare,    "Education",        "edu",             "census_pct", "ACS 2023 PUMS"),
  std(income_compare, "Household income", "income_band",     "census_pct", "ACS 2023 PUMS"),
  std(emp_compare,    "Employment",       "employment",      "census_pct", "ACS 2023 PUMS"),
  std(mar_compare,    "Marital status",   "marital",         "census_pct", "ACS 2023 PUMS"),
  std(vet_compare,    "Veteran status",   "veteran",         "census_pct", "ACS 2023 PUMS"),
  std(kids_compare,   "Children (ever)",  "kids",            "gss_pct",    "GSS 2022"),
  std(pol_compare,    "Political views",  "ideology",        "gallup_pct", "Gallup 2025"),
  dat2 |>
    filter(!is.na(Q1.14_ord)) |>
    count(Q1.14_ord, name = "n") |>
    transmute(Variable = "Driving frequency", Category = as.character(Q1.14_ord), n,
              `Sample %` = round(100 * n / sum(n), 1),
              `Benchmark %` = NA_real_, Diff = NA_real_,
              Source = "No comparable benchmark")
)

table2 <- driving_compare |>
  transmute(`Age band` = age_band, n,
            `Sample % who drive` = sample_pct_drives,
            `FHWA % licensed`    = fhwa_licensed_pct, Diff = diff)

table3 <- state_compare |>
  transmute(State = state_name, n = n_total,
            `Sample %` = pooled_pct, `ACS %` = census_pct, Diff = diff)

# Blank the NAs for presentation (underlying data unchanged)
table1_out <- table1 |>
  mutate(across(c(`Benchmark %`, Diff), ~ifelse(is.na(.), "", as.character(.))))

write.csv(table1_out, "table1_demographics.csv", row.names = FALSE)
write.csv(table2,     "table2_driving.csv",      row.names = FALSE)
write.csv(table3,     "table3_geography.csv",    row.names = FALSE)

ft1 <- flextable(table1_out) |>
  set_caption("Table 1. Sample demographics compared to national benchmarks (N = 1,004)") |>
  merge_v(j = "Variable") |> valign(j = "Variable", valign = "top") |>
  theme_vanilla() |> autofit()

ft2 <- flextable(table2) |>
  set_caption("Table 2. Driving status by age, compared to FHWA licensure rates") |>
  theme_vanilla() |> autofit()

ft3 <- flextable(table3) |>
  set_caption("Table 3. Geographic distribution compared to ACS 2023") |>
  theme_vanilla() |> autofit()

save_as_docx(ft1, ft2, ft3, path = "demographics_tables.docx")


# =============================================================================
