library(data.table)
library(haven)
library(foreign)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(paste0(path, "/survey_data"))

# Load RCVS-2021
rcvs2021 <- readRDS("rcvs/rcvs_2021_dataset_2022-06-14.Rds")
setDT(rcvs2021)

# Load raw RCVS data to get interviewer IDs and carriers
rcvs2021_raw <- as.data.table(readRDS("D:/serebrennikov/RCVS_2021/RCVS_Wave_2/russian_victimization_survey_2021/intermediate_files/survey_data_raw.Rda"))

# Add interviewer ID
rcvs2021[, interviewer_id := rcvs2021_raw[match(rcvs2021$ID, ID)]$UserID ]

# Create the universal sets of variables with appropriate names
setnames(rcvs2021, c("Q1", "Q2", "Q61", "resp_ses_is_employed", "resp_is_living_alone", "Q79", "resp_household_size", "Q57", "Q70", "resp_household_income", "resp_mean_household_income", "Q664", "Q5_1T"), c("sex", "age", "education_level", "employed", "lives_alone", "marital_status", "household_size", "income_level", "receives_pension", "household_income", "mean_household_income", "favorite_browser", "crime_description"))

# Keep only variables of interest
rcvs2021 <- rcvs2021[, c("ID", "interviewer_id", "household_income")]
save(rcvs2021, file = "rcvs/rcvs2021_extract_income_interviewer_id.rdata", compress = "gzip")
