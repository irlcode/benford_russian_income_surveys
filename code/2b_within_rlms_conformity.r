library(data.table)
library(digitTests)
library(cvmdisc)
library(sads)
library(DEoptim)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# Load helper functions
source("code/helper_functions/helper_functions.r")

# Load the data prepared in prepare_surveys/2_combine_surveys.r
load("survey_data/income_surveys.rdata")

#################
# Survey quality per secondary sampling unit

# Observations per secondary sampling unit-year
rlms_household_observations_per_area_id_year <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income), list(observations = .N), by = c("area_id", "year")]

# Keep only secondary sampling units with over 15 observations
rlms_household <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income) ]
rlms_household <- merge(rlms_household, rlms_household_observations_per_area_id_year, by = c("area_id", "year"), all.x = T, all.y = F)
rlms_household <- rlms_household[ observations >= 15 ]

# Conformity of household income in RLMS surveys by secondary sampling unit and year
# NB: this takes around one day on one CPU core
rlms_household_income_conformity_by_area_id_year <- rlms_household[, test_conformity(income, bayesian = T), by = c("area_id", "year") ]
rlms_household_income_conformity_by_area_id_year <- merge(rlms_household_income_conformity_by_area_id_year, rlms_household_observations_per_area_id_year, by = c("area_id", "year"), all.x = T, all.y = F)

# Save point
save(rlms_household_income_conformity_by_area_id_year, file = "output/rlms_household_income_conformity_by_area_id_year.rdata", compress = "gzip")

#################
# Special case of RLMS: conformity of new and old respondents per wave
# (we observed non-Benfordness starting from the 2010 wave)

########
# Identify new households

# Re-create an object with RLMS data
rlms_household <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income) ]

# Load the earliest years households were present data
# produced in code/prepare_surveys/3_identify_new_observations_per_wave.r
load("survey_data/income_surveys_first_year.rdata")

# Earliest year per individual
rlms_household <- merge(rlms_household, income_surveys_first_year, by = c("survey", "level", "id", "year"), all.x = T, all.y = F)

# New households by year
#rlms_household[, uniqueN(id), by = "first_year"][order(first_year)]

# Mark new household
rlms_household[, new := as.numeric(year == first_year)]

########
# Conformity among new and old households per wave

rlms_household_income_conformity_by_novelty <- rlms_household[!is.na(new), test_conformity(income, bayesian = T), by = c("new", "year") ]
setorderv(rlms_household_income_conformity_by_novelty, c("year", "new", "type"))

# Negative or infinite probablities to zero
vars_convert <- c("benford", "stigler", "uniform", "contaminated_benford", "generalized_benford", "rodriguez", "hurlimann")

rlms_household_income_conformity_by_novelty[type == "posterior", c(vars_convert) := lapply(.SD, function(x) {
	
	x <- ifelse(x < 0, 0, x)
	x <- ifelse(x > 1, 1, x)

	x <- round(x, 4)
	}), .SDcols = vars_convert]

# Round optimal parameters and Bayes factors
rlms_household_income_conformity_by_novelty[type %in% c("optim", "log10BF01"), c(vars_convert) := lapply(.SD, function(x) {
	
	x <- round(x, 2)
	
	}), .SDcols = vars_convert]

# Save point
save(rlms_household_income_conformity_by_novelty, file = "output/rlms_household_income_conformity_by_novelty.rdata", compress = "gzip")

#################
# Special case of VODPF: conformity of new and old respondents per wave
# (we observed non-Benfordness starting from the 2010 wave)

# Re-create an object with vodpf data
vodpf_household <- income_surveys[survey == "VODPF" & level == "household" & !is.na(income) ]

# Load earliest years households were present data
# produced in code/prepare_surveys/3_identify_new_observations_per_wave.r
load("survey_data/income_surveys_first_year.rdata")

# Earliest year per individual
vodpf_household <- merge(vodpf_household, income_surveys_first_year[, -"year"], by = c("survey", "level", "id"), all.x = T, all.y = F)

# New households by year
#vodpf_household[, uniqueN(id), by = "first_year"][order(first_year)]

# Mark new household
vodpf_household[, new := as.numeric(year == first_year)]

########
# Conformity among new and old households per wave

vodpf_household_income_conformity_by_novelty <- vodpf_household[!is.na(new), test_conformity(income, bayesian = T), by = c("new", "year") ]
setorderv(vodpf_household_income_conformity_by_novelty, c("year", "new", "type"))

# Negative or infinite probablities to zero
vars_convert <- c("benford", "stigler", "uniform", "contaminated_benford", "generalized_benford", "rodriguez", "hurlimann")

vodpf_household_income_conformity_by_novelty[type == "posterior", c(vars_convert) := lapply(.SD, function(x) {
	
	x <- ifelse(x < 0, 0, x)
	x <- ifelse(x > 1, 1, x)

	x <- round(x, 4)
	}), .SDcols = vars_convert]

# Round optimal parameters and Bayes factors
vodpf_household_income_conformity_by_novelty[type %in% c("optim", "log10BF01"), c(vars_convert) := lapply(.SD, function(x) {
	
	x <- round(x, 2)
	
	}), .SDcols = vars_convert]

# Save point
save(vodpf_household_income_conformity_by_novelty, file = "output/vodpf_household_income_conformity_by_novelty.rdata", compress = "gzip")
