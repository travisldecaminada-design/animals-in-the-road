# Animals in the Road

The study is a national survey of 1,004 U.S. adult drivers recruited through Prolific and fielded in Qualtrics. 

## Files

| Script | What it does | What it writes |
|---|---|---|
| `00_setup.R` | Loads the two Qualtrics exports and builds a lookup table of question wording | Nothing (objects stay in memory) |
| `01_cleaning.R` | Filters to the 1,004 valid responses, labels the two recruitment waves, and recodes every survey block into typed variables | `dat_clean_final.rds` |
| `02_descriptives.R` | Builds Census-comparable race and ethnicity, then benchmarks the sample against ACS 2023 PUMS, GSS 2022, Gallup 2025, and FHWA 2023 | `census_benchmarks.rds`, `table1_demographics.csv`, `table2_driving.csv`, `table3_geography.csv`, `demographics_tables.docx` |
| `03_analysis.R` | Builds the derived and geographic variables, then runs all hypothesis tests (RQ1 through RQ6) and the pet restraint models | Updated `dat_clean_final.rds`; model results print to the console |

## How to run

1. Install R (the code was written on R 4.2.2) and RStudio.
2. Install the packages:

   ```r
   install.packages(c("tidyverse", "janitor", "tidycensus", "flextable",
                      "DescTools", "logistf"))
   install.packages("gssr", repos = c("https://kjhealy.r-universe.dev",
                                      "https://cloud.r-project.org"))
   ```

   `MASS` and `nnet` come with R.
3. Get a free Census API key at https://api.census.gov/data/key_signup.html and store it once:

   ```r
   tidycensus::census_api_key("YOUR_KEY", install = TRUE)
   ```

4. Put the two Qualtrics exports, `AVSurveyValues.csv` and `AVSurveyLabels.csv`, in the project folder.
5. Open the folder as an RStudio Project so the working directory is set automatically.
6. Run the scripts in order. `00_setup.R` and `01_cleaning.R` must run in the same R session, because `01` starts from the `values` object that `00` creates. `02` and `03` each start by reading `dat_clean_final.rds`.

`02_descriptives.R` downloads four ACS PUMS files of roughly 300 MB each, so it needs a good connection and enough memory. The script removes each file once its benchmark is computed.

## Data

The survey data are not included in this repository. The raw export contains respondent ZIP codes, Prolific participant IDs, and free-text answers. Together these could identify individual respondents. Contact the author about access.

The scripts download every outside dataset they use:

- ACS 2023 5-year estimates by ZIP Code Tabulation Area: median household income (B19013_001) and total population (B01003_001), through `tidycensus`
- 2023 Census Gazetteer file for ZCTAs (land area): https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_zcta_national.zip
- Modified RUCA codes by ZIP code (2010): https://github.com/jdjohn215/modified-RUCA-codes
- ACS 2023 1-year PUMS, through `tidycensus`
- General Social Survey 2022, through `gssr`

Two benchmarks are typed into `02_descriptives.R` from published tables, with the source link next to each one: Gallup's 2025 political ideology figures and FHWA Highway Statistics 2023, Table DL-20.

## Notes

- All analyses are unweighted.
- `03_analysis.R`, Section 3, reports the result of the 75-ZIP road network pilot (TIGER/Line 2020 roads through `tigris`). The code for that pilot is not part of this repository and it does not change any conclusion.
- The primary model for each research question is commented with its headline result so the output can be checked against the dissertation.
