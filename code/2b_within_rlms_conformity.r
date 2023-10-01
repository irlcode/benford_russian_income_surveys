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




#################
# Survey quality



rlms2020_income_conformity_by_area_id <- income_surveys[survey == "RLMS" & year == 2020 & level == "household", test_conformity(income), by = c("area_id") ]
temp <- income_surveys[survey == "RLMS" & year == 2020 & level == "household" & !is.na(income), list(observations = .N), by = "area_id"]
rlms2020_income_conformity_by_area_id <- merge(rlms2020_income_conformity_by_area_id, temp, by = "area_id", all.x = T, all.y = F)
save(rlms2020_income_conformity_by_area_id, file = "output/rlms2020_income_conformity_by_area_id.rdata", compress = "gzip")

sum(rlms2020_income_conformity_by_area_id[type == "posterior" & generalized_benford >= 0.9]$observations)/sum(rlms2020_income_conformity_by_area_id[type == "posterior"]$observations)

# RLMS 2021 — can we bring it closer to benford
rlms2021_income_conformity_by_area_id <- income_surveys[survey == "RLMS" & year == 2021 & level == "household", test_conformity(income), by = c("area_id") ]

# Observations per area_id
temp <- income_surveys[survey == "RLMS" & year == 2021 & level == "household" & !is.na(income), list(observations = .N), by = "area_id"]
rlms2021_income_conformity_by_area_id <- merge(rlms2021_income_conformity_by_area_id, temp, by = "area_id", all.x = T, all.y = F)

save(rlms2021_income_conformity_by_area_id, file = "output/rlms2021_income_conformity_by_area_id.rdata", compress = "gzip")

# Share of observations conforming
sum(rlms2021_income_conformity_by_area_id[type == "posterior" & generalized_benford >= 0.9]$observations)/sum(rlms2021_income_conformity_by_area_id[type == "posterior"]$observations)

rlms2020_problematic_area_ids <- unique(rlms2020_income_conformity_by_area_id[type == "posterior" & generalized_benford < 0.9]$area_id)
rlms2021_problematic_area_ids <- unique(rlms2021_income_conformity_by_area_id[type == "posterior" & generalized_benford < 0.9]$area_id)

temp <- test_conformity(income_surveys[survey == "RLMS" & year == 2020 & level == "household" & !(area_id %in% rlms2020_problematic_area_ids)]$income)

temp <- test_conformity(income_surveys[survey == "RLMS" & year == 2020 & level == "household" ]$income)

temp <- test_conformity(income_surveys[survey == "RLMS" & year == 2021 & level == "household" & !(area_id %in% rlms2021_problematic_area_ids)]$income)


# area_id with large deviation
table(extract_digits(income_surveys[survey == "RLMS" & year == 2021 & level == "household" & area_id == "117"]$income, check = "first", include.zero = FALSE))



income_surveys_income_conformity[type == "posterior" & rodriguez >= 0.9]

#############################

# Example satisfying Benford's law
temp <- test_conformity(income_surveys[survey == "RLMS" & year == 2003 & level == "household" & income > 0]$income)

# Year when it all breaks down in RLMS
temp2 <- test_conformity(income_surveys[survey == "RLMS" & year == 2011 & level == "household" & income > 0]$income)

# Example estimated variable in Kouzh
temp3 <- test_conformity(income_surveys[survey == "KOUZh" & year == 2016 & level == "household"]$amount_living_poorly)

# Example average from categorical variable in Kouzh
temp5 <- test_conformity(income_surveys[survey == "KOUZh" & year == 2022 & level == "household" & income > 0]$income)

# Example with reliable survey
temp6 <- test_conformity(income_surveys[survey == "RCVS" & year == 2021 & level == "household" & income > 0]$income)

# VNDN
temp7 <- test_conformity(income_surveys[survey == "VNDN" & year == 2021 & level == "household" & income > 0]$income)

# Lognormal distribution matching 2021's RLMS moments
fit_lognorm_rlms <- sads::fitlnorm(income_surveys[survey == "RLMS" & year == 2021 & level == "household" & income > 0]$income)

# Pareto distribution matching 2021's RLMS moments
fit_pareto_rlms <- sads::fitpareto(income_surveys[survey == "RLMS" & year == 2021 & level == "household" & income > 0]$income)

# Example with log-Normal 
# When https://latex.codecogs.com/gif.latex?%20\sigma is too small, it is clearly not Benford’s distribution: for half (or more) of our samples, the https://latex.codecogs.com/gif.latex?%20p-value is lower than 5%. On the other hand, when https://latex.codecogs.com/gif.latex?%20\sigma is large (enough), Benford’s distribution is the distribution of the first digit of lognormal samples

temp8 <- test_conformity(rlnorm(n = 5000, meanlog = fit_lognorm_rlms@details$par[["meanlog"]], sdlog = fit_lognorm_rlms@details$par[["sdlog"]]))

#temp9 <- test_conformity(rlnorm_exact(n = 5000, mean = 51750.4, sd = 91095.51))

https://stats.stackexchange.com/a/571439

rlnorm_exact(n = 5000, mean = 66218, sd = 91095.51)

# EUSILC Germany
temp4 <- test_conformity(income_surveys[survey == "EUSILC" & year == 2013 & level == "household" & area_id == "DE" & income > 0]$income)

# EUSILC Italy
temp5 <- test_conformity(income_surveys[survey == "EUSILC" & year == 2010 & level == "household" & area_id == "IT" & income > 0]$income)

# Bayes factor interpretation
# http://cbsuapps.tc.cornell.edu/bayescan.help.htm
