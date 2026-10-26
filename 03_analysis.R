library(tidyverse)

# =============================================================================
# SECTION 0 — LOAD + REBUILD DERIVED VARIABLES
# =============================================================================
dat2 <- readRDS("dat_clean_final.rds")

# --- 0.1 Risk score (RQ5 outcome; mean of the three Q5 risk items) ----------
dat2 <- dat2 |>
  mutate(
    risk6 = as.integer(Q5.6_ord),
    risk7 = as.integer(Q5.7_ord),
    risk8 = as.integer(Q5.8_ord),
    risk_score = rowMeans(cbind(risk6, risk7, risk8), na.rm = TRUE)
  )
# NaN when all three missing -> make it NA
dat2$risk_score[is.nan(dat2$risk_score)] <- NA

# --- 0.2 Collapsed demographics (shared across RQ3/RQ5/RQ6/Q2) --------------
dat2 <- dat2 |>
  mutate(
    age3 = case_when(
      Q1.1_ord %in% c("18 - 24","25 - 34")           ~ "Under 35",
      Q1.1_ord %in% c("35 - 44","45 - 54","55 - 64") ~ "35-64",
      Q1.1_ord %in% c("65 - 74","75 - 84")           ~ "65+"
    ) |> factor(levels = c("Under 35","35-64","65+")),

    gender2 = factor(Q1.2, levels = c("Male","Female")),   # non-binary/PNS -> NA

    pol3 = case_when(
      Q1.10_ord %in% c("Very liberal","Liberal")           ~ "Liberal",
      Q1.10_ord == "Moderate"                              ~ "Moderate",
      Q1.10_ord %in% c("Conservative","Very conservative") ~ "Conservative"
    ) |> factor(levels = c("Moderate","Liberal","Conservative")),   # Moderate = ref

    income3 = case_when(
      Q1.5_ord %in% c("Less than $10,000","$10,000 - $19,999","$20,000 - $29,999",
                      "$30,000 - $39,999","$40,000 - $49,999")                 ~ "Under $50k",
      Q1.5_ord %in% c("$50,000 - $59,999","$60,000 - $69,999","$70,000 - $79,999",
                      "$80,000 - $89,999","$90,000 - $99,999")                 ~ "$50-100k",
      Q1.5_ord %in% c("$100,000 - $149,999","$150,000 - $199,999","$200,000 or more") ~ "$100k+"
    ) |> factor(levels = c("Under $50k","$50-100k","$100k+")),

    edu3 = case_when(
      Q1.6_ord %in% c("Less than high school","High school graduate","Some college") ~ "HS/some college",
      Q1.6_ord %in% c("Associate's degree","Bachelor's degree")                      ~ "2yr/4yr degree",
      Q1.6_ord %in% c("Master's degree","Professional degree (MD, JD, DDS, etc.)","Doctorate degree") ~ "Graduate"
    ) |> factor(levels = c("HS/some college","2yr/4yr degree","Graduate")),

    married2 = case_when(
      Q1.8 == "Married"                                            ~ "Married",
      Q1.8 %in% c("Never married","Divorced","Widowed","Separated") ~ "Not married"
    ) |> factor(levels = c("Not married","Married")),

    has_kids = case_when(
      Q1.11_ord == "None" ~ "No kids",
      TRUE                ~ "Has kids"
    ) |> factor(levels = c("No kids","Has kids")),

    veteran2 = case_when(
      Q1.9 %in% c("Veteran","Active Duty") ~ "Veteran",
      Q1.9 == "Non-Veteran"                ~ "Non-veteran"
    ) |> factor(levels = c("Non-veteran","Veteran")),

    owns_pet   = factor(if_else(Q2.1 == "Yes", "Owner", "Non-owner"),
                        levels = c("Non-owner","Owner")),
    pet_struck = factor(if_else(Q3.1 == "Yes", "Yes", "No"),
                        levels = c("No","Yes"))
  )

# --- 0.3 race5 (RQ5 exploratory; collapse tiny groups) ----------------------
dat2 <- dat2 |>
  mutate(race5 = case_when(
    race_eth_census == "White alone"                     ~ "White",
    race_eth_census == "Black or African American alone" ~ "Black",
    race_eth_census == "Hispanic or Latino (any race)"    ~ "Hispanic",
    race_eth_census == "Asian alone"                      ~ "Asian",
    race_eth_census == "Two or more races"                ~ "Two or more",
    race_eth_census == "Not reported"                     ~ NA_character_,
    TRUE                                                  ~ "Other"   # AIAN/NHPI/SOR
  ) |> factor(levels = c("White","Black","Hispanic","Asian","Two or more","Other")))

# --- 0.4 Urbanicity from modified RUCA (RQ4 + used as control) ---------------
# Modified RUCA zip crosswalk (adds Principal-city code standard RUCA lacks).
# zip_code is character w/ leading zeros -> matches our zip5. 2010 vintage.
ruca_url <- "https://raw.githubusercontent.com/jdjohn215/modified-RUCA-codes/master/Modified_RUCA_Zip_Codes_2010.csv"
ruca <- readr::read_csv(ruca_url, show_col_types = FALSE)

dat2 <- dat2 |>
  mutate(zip5 = str_sub(str_extract(Q1.13, "\\d+"), 1, 5)) |>
  left_join(ruca |> dplyr::select(zip_code, ruca6), by = c("zip5" = "zip_code")) |>
  mutate(urbanicity = case_when(
    ruca6 == "Principal city"                  ~ "Urban",
    ruca6 %in% c("Suburban core","Exurbs")     ~ "Suburban",
    ruca6 %in% c("Large towns (10-50k)","Small towns (2.5-10k)","Isolated rural") ~ "Town/Rural"
  ) |> factor(levels = c("Urban","Suburban","Town/Rural")))
# Rural alone was n=21 -> merged into Town/Rural for stable modeling.
# Coverage: 994/1004 matched.

# --- 0.5 ZIP income + density (RQ4) -----------------------------------------
# Requires tidycensus + API key. Pulls are ZCTA-level ACS 5-year.
library(tidycensus)

zip_income <- get_acs(geography = "zcta", variables = "B19013_001",
                      year = 2023, survey = "acs5")
dat2 <- dat2 |>
  left_join(zip_income |> dplyr::select(GEOID, zip_median_income = estimate),
            by = c("zip5" = "GEOID"))

zip_pop <- get_acs(geography = "zcta", variables = "B01003_001",
                   year = 2023, survey = "acs5", geometry = FALSE)

# land area from the Census gazetteer (no sf needed for this part)
gaz_url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_zcta_national.zip"
gtmp <- tempfile(fileext = ".zip"); download.file(gaz_url, gtmp, mode = "wb")
gaz <- readr::read_tsv(unzip(gtmp, exdir = tempdir()), show_col_types = FALSE)

zip_density <- zip_pop |>
  dplyr::select(GEOID, population = estimate) |>
  left_join(gaz |> dplyr::select(GEOID, land_sqmi = ALAND_SQMI), by = "GEOID") |>
  mutate(density = population / land_sqmi)

dat2 <- dat2 |>
  left_join(zip_density |> dplyr::select(GEOID, zip_density = density),
            by = c("zip5" = "GEOID")) |>
  mutate(log_density = log10(zip_density + 1))   # +1 for the one zero-density zip

# --- 0.6 Save the fully-derived frame ---------------------------------------
saveRDS(dat2, "dat_clean_final.rds")


# =============================================================================
# SECTION 1 — RQ1 / H1: response by animal type
# =============================================================================
library(DescTools)   # CochranQTest

## 1a. Wild-animal scenarios Q5.1-5.5 (moose/deer/coyote/raccoon/squirrel).
##     Unordered responses; build two binaries and test within-person.
# "took action vs drove straight"
cq <- dat2 |>
  dplyr::select(Q5.1, Q5.2, Q5.3, Q5.4, Q5.5) |>
  mutate(across(everything(),
                ~case_when(. == "Continue straight and risk a collision" ~ 0L,
                           . == "Other options not listed here"          ~ NA_integer_,
                           is.na(.)                                       ~ NA_integer_,
                           TRUE                                           ~ 1L),
                .names = "act_{.col}")) |>
  dplyr::select(starts_with("act_")) |>
  filter(if_all(everything(), ~!is.na(.)))
colMeans(cq)                      # 99.1% moose -> 84.8% squirrel (monotonic)
CochranQTest(as.matrix(cq))       # Q=366, df=4, p<2.2e-16

# pairwise McNemar (Holm): all adjacent pairs differ EXCEPT moose vs deer
pairs <- combn(colnames(cq), 2, simplify = FALSE)
pvals_A <- sapply(pairs, function(p) mcnemar.test(table(cq[[p[1]]], cq[[p[2]]]))$p.value)
names(pvals_A) <- sapply(pairs, paste, collapse = " vs ")
round(p.adjust(pvals_A, method = "holm"), 5)

# "swerve vs not" — inverse gradient (swerve MORE for small animals)
sw <- dat2 |>
  dplyr::select(Q5.1, Q5.2, Q5.3, Q5.4, Q5.5) |>
  mutate(across(everything(),
                ~case_when(. == "Swerve to try and avoid the animal" ~ 1L,
                           . == "Other options not listed here"       ~ NA_integer_,
                           is.na(.)                                    ~ NA_integer_,
                           TRUE                                        ~ 0L),
                .names = "sw_{.col}")) |>
  dplyr::select(starts_with("sw_")) |>
  filter(if_all(everything(), ~!is.na(.)))
colMeans(sw)                      # 22% moose -> 39% squirrel
CochranQTest(as.matrix(sw))       # Q=206, p<2.2e-16

## 1b. Risk items Q5.6-5.8 (endangered vs pet dog vs pet cat) — within-person.
##     THE ownership test: people risk more for owned pets than endangered.
friedman_dat <- dat2 |>
  filter(!is.na(risk6), !is.na(risk7), !is.na(risk8)) |>
  dplyr::select(risk6, risk7, risk8)     # n=921
colMeans(friedman_dat)            # endangered 1.85 < cat 2.04 < dog 2.07
friedman.test(as.matrix(friedman_dat))   # chi2=222, p<2.2e-16

pairwise.wilcox.test(
  x = c(friedman_dat$risk6, friedman_dat$risk7, friedman_dat$risk8),
  g = rep(c("endangered","dog","cat"), each = nrow(friedman_dat)),
  paired = TRUE, p.adjust.method = "holm")
# both pets >> endangered (p<1e-15); dog vs cat sig (p=.005) but trivial in size.


# =============================================================================
# SECTION 2 — RQ3 / H3: behavior change after a collision
# =============================================================================
library(logistf)   # Firth penalized logistic

# person-level "hit any animal" frame (wild Q4.6 / dog Q4.16 / cat Q4.26)
collided <- dat2 |>
  mutate(
    hit_wild = Q4.6  == "Yes" & !is.na(Q4.6),
    hit_dog  = Q4.16 == "Yes" & !is.na(Q4.16),
    hit_cat  = Q4.26 == "Yes" & !is.na(Q4.26),
    hit_any  = hit_wild | hit_dog | hit_cat,
    # outcome: changed behavior after ANY collision
    changed_any = (Q4.10=="Yes"&!is.na(Q4.10)) | (Q4.20=="Yes"&!is.na(Q4.20)) |
                  (Q4.30=="Yes"&!is.na(Q4.30)),
    # person-level severity: ANY across blocks (injury/fatal), worst damage
    any_injury = (!is.na(Q4.8) & !str_detect(Q4.8,"^No injury")) |
                 (!is.na(Q4.18)& !str_detect(Q4.18,"^No injury")) |
                 (!is.na(Q4.28)& !str_detect(Q4.28,"^No injury")),
    any_fatal = (Q4.7=="Yes"&!is.na(Q4.7)) | (Q4.17=="Yes"&!is.na(Q4.17)) |
                (Q4.27=="Yes"&!is.na(Q4.27)),
    any_major_damage = (!is.na(Q4.9) & str_detect(Q4.9,"^Major|totaled")) |
                       (!is.na(Q4.19)& str_detect(Q4.19,"^Major|totaled")) |
                       (!is.na(Q4.29)& str_detect(Q4.29,"^Major|totaled"))
  )

model_dat <- collided |> filter(hit_any)   # n=252; changed=149, not=103
# 50/252 hit >1 animal type -> person-level collapse avoids repeated-respondent
# non-independence (why we did NOT stack collisions).

## m2: all-animal severity + type. INJURY is the only predictor (OR 5.5).
m2 <- glm(changed_any ~ any_injury + any_fatal + any_major_damage + hit_dog + hit_cat,
          data = model_dat, family = binomial)
summary(m2); exp(cbind(OR = coef(m2), confint(m2)))
anova(m2, test = "Chisq")               # only any_injury sig (p=.0008)
1 - (m2$deviance / m2$null.deviance)    # McFadden ~0.04

## m3 focused + demographics; m3_full adds politics+income (both exploratory).
m3 <- glm(changed_any ~ any_injury + pet_struck + age3 + gender2 + owns_pet,
          data = model_dat, family = binomial)
summary(m3); exp(cbind(OR = coef(m3), confint(m3)))

m3_full <- glm(changed_any ~ any_injury + pet_struck + age3 + gender2 +
                             owns_pet + pol3 + income3,
               data = model_dat, family = binomial)
summary(m3_full)      # politics/income null; injury robust

m4 <- glm(changed_any ~ any_injury + pet_struck + age3 + gender2 + owns_pet + urbanicity,
          data = model_dat, family = binomial)
summary(m4)

## Firth robustness on the injury effect (rests on 29 injured) -> holds, OR 3.82
m_firth <- logistf(changed_any ~ any_injury + pet_struck + age3 + gender2 + owns_pet,
                   data = model_dat)
summary(m_firth)

## H3 persistence half: duration is near-universally "Indefinitely" -> ceiling.
## 86.6% (129/149) permanent -> supports "lasting" but severity-grading untestable.
dur_dat <- model_dat |>
  mutate(any_indef =
      (str_detect(Q4.12,"^Indefinitely")&!is.na(Q4.12)) |
      (str_detect(Q4.22,"^Indefinitely")&!is.na(Q4.22)) |
      (str_detect(Q4.32,"^Indefinitely")&!is.na(Q4.32)),
    gave_duration = !is.na(Q4.12)|!is.na(Q4.22)|!is.na(Q4.32))
dur_dat |> filter(gave_duration) |>
  summarise(n_changed=n(), n_indef=sum(any_indef), pct=round(100*mean(any_indef),1))


# =============================================================================
# SECTION 3 — RQ4 / H4: geography (collision EXPOSURE)
# =============================================================================
# Build the analysis frame once (hit_any on full sample + geo predictors).
rq4_dat <- dat2 |>
  mutate(
    hit_any = as.integer((Q4.6=="Yes"&!is.na(Q4.6)) |
                         (Q4.16=="Yes"&!is.na(Q4.16)) |
                         (Q4.26=="Yes"&!is.na(Q4.26))),
    income_10k = zip_median_income / 10000
  )

## Bivariate: hit rate Urban 18% / Suburban 24% / Town-Rural 52% (chi2 p<1e-12)
## Exposure model: Town/Rural OR ~5x, robust to driving freq/age/gender.
m_exp2 <- glm(hit_any ~ urbanicity + Q1.14_ord + age3 + gender2,
              data = rq4_dat, family = binomial)
summary(m_exp2); exp(cbind(OR = coef(m_exp2), confint(m_exp2)))
# Town/Rural OR 5.34 (CI 3.35-8.59, p=2.7e-12). Age = lifetime accumulation.

## Piece 1 — neighborhood income (controlling categorical urbanicity): sig.
m_income <- glm(hit_any ~ income_10k + urbanicity + Q1.14_ord + age3 + gender2,
                data = rq4_dat, family = binomial)
summary(m_income)     # income_10k OR 0.94, p=.026 (lower-income -> more)

## Piece 2 — continuous density: income ATTENUATES to n.s. once density in.
m_density <- glm(hit_any ~ log_density + income_10k + Q1.14_ord + age3 + gender2,
                 data = rq4_dat, family = binomial)
summary(m_density)    # log_density OR 0.44 (p<1e-6); income_10k p=.077 (n.s.)

## Piece 3a — suburban washes out with density AND urbanicity both in model.
m_arterial <- glm(hit_any ~ log_density + income_10k + urbanicity +
                            Q1.14_ord + age3 + gender2,
                  data = rq4_dat, family = binomial)
summary(m_arterial)   # Suburban OR 1.15, p=.49 -> no independent suburban effect


# =============================================================================
# SECTION 4 — RQ5 / H5: risk-willingness by demographics
# =============================================================================
## Primary: linear regression on the combined risk_score.
m_rq5 <- lm(risk_score ~ age3 + gender2 + pol3 + income3, data = dat2)
summary(m_rq5)  

## Full model: theory-backed additions. adj R2 0.072 -> 0.10 (real gain).
m_rq5_full <- lm(risk_score ~ age3 + gender2 + pol3 + income3 +
                              edu3 + married2 + has_kids + veteran2 +
                              owns_pet + pet_struck + urbanicity, data = dat2)
summary(m_rq5_full)
# SIGNIFICANT: gender+, has_kids- (parents less willing, strong), owns_pet+,


## Race check — null, and leaves other coefficients unchanged.
m_rq5_race <- lm(risk_score ~ age3 + gender2 + pol3 + income3 + has_kids +
                              owns_pet + pet_struck + urbanicity + race5, data = dat2)
summary(m_rq5_race)   # all race5 terms n.s.

## Robustness: ordinal logistic on the dog (Q5.7) and cat (Q5.8) items.
library(MASS)   # NOTE: masks dplyr::select from here on -> use dplyr::select()
m_ord_dog <- polr(Q5.7_ord ~ age3 + gender2 + pol3 + income3 + edu3 + married2 +
                             has_kids + veteran2 + owns_pet + pet_struck + urbanicity,
                  data = dat2, Hess = TRUE)
m_ord_cat <- polr(Q5.8_ord ~ age3 + gender2 + pol3 + income3 + edu3 + married2 +
                             has_kids + veteran2 + owns_pet + pet_struck + urbanicity,
                  data = dat2, Hess = TRUE)
# p-values helper (polr gives t only):
polr_p <- function(m){ ct <- coef(summary(m));
  cbind(ct, p = round(2*pnorm(abs(ct[,"t value"]), lower.tail=FALSE),4)) }
polr_p(m_ord_dog); polr_p(m_ord_cat)
detach("package:MASS")   # restore dplyr::select


# =============================================================================
# SECTION 5 — RQ6 / H6: AV moral alignment
# =============================================================================
## Core test: does OWN risk-willingness predict wanting the AV to swerve? NO.
align_dat <- dat2 |>
  mutate(av_swerve = case_when(
    Q6.4 == "Swerve"                       ~ 1L,
    Q6.4 == "Other option not listed here" ~ NA_integer_,
    is.na(Q6.4)                            ~ NA_integer_,
    TRUE                                   ~ 0L))
m_align <- glm(av_swerve ~ risk_score + owns_pet + pet_struck + age3 + gender2,
               data = align_dat, family = binomial)
summary(m_align)   # risk_score OR 1.11, p=.50 -> H6 NOT supported.

## Q6.13 universal policy: 91% of those w/ opinion want a mandated rule.
dat2 |> filter(Q6.13 %in% c("Yes","No")) |> count(Q6.13) |>
  mutate(pct = round(100*n/sum(n),1))     # 626 Yes / 62 No
policy_dat <- dat2 |> filter(Q6.13 %in% c("Yes","No")) |>
  mutate(wants_policy = factor(Q6.13, levels = c("No","Yes")))
m_policy <- glm(wants_policy ~ risk_score + owns_pet + pet_struck + age3 + gender2 + pol3,
                data = policy_dat, family = binomial)
summary(m_policy)  

## Q6.12 priority ranking (6 items, n=940). Strong consensus.
rank_wide <- dat2 |> dplyr::select(starts_with("Q6.12")) |>
  filter(if_all(everything(), ~!is.na(.)))
friedman.test(as.matrix(rank_wide))       # chi2=2930.9, df=5, p<2.2e-16


# =============================================================================
# SECTION 6 — Q2 / RQ2: pet safety behavior
# =============================================================================
library(nnet)   # multinom

restraint_dat <- dat2 |>
  filter(!is.na(Q2.10)) |>
  mutate(
    dog_restraint3 = case_when(
      Q2.10 == "Dog is in a pet carrier/crate"                            ~ "Crate",
      Q2.10 == "Dog is secured in a seat via a specific product or leash" ~ "Seat-secured",
      Q2.10 == "Other method not listed here"                            ~ NA_character_,
      TRUE                                                                ~ "Unrestrained"
    ) |> factor(levels = c("Unrestrained","Crate","Seat-secured")),
    distract_dog = as.integer(Q2.9_ord)
  )
m_restraint <- multinom(
  dog_restraint3 ~ distract_dog + age3 + gender2 + urbanicity + pet_struck + income3,
  data = restraint_dat, trace = FALSE)
z <- summary(m_restraint)$coefficients / summary(m_restraint)$standard.errors
round(exp(summary(m_restraint)$coefficients), 3)   # OR vs Unrestrained
round(2*(1-pnorm(abs(z))), 4)                       # p-values

## Dog off-leash on walks (never vs ever)
offleash_dat <- dat2 |> filter(!is.na(Q2.15)) |>
  mutate(offleash = if_else(Q2.15 == "No, never", 0L, 1L))
m_offleash <- glm(offleash ~ age3 + gender2 + urbanicity + pet_struck + income3 + has_kids,
                  data = offleash_dat, family = binomial)
summary(m_offleash)

outcat_dat <- dat2 |> filter(!is.na(Q2.24)) |>
  mutate(outdoor_cat = if_else(Q2.24 == "No", 0L, 1L))
m_outcat <- glm(outdoor_cat ~ age3 + gender2 + urbanicity + pet_struck + income3 + has_kids,
                data = outcat_dat, family = binomial)
summary(m_outcat)