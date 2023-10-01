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
# Test conformity of all surveys to various distributions (Bayesian approach)
income_surveys_income_conformity <- income_surveys[, test_conformity(income, bayesian = T), by = c("survey", "year", "level")]

# Negative or infinite probablities to zero
vars_convert <- c("benford", "stigler", "uniform", "contaminated_benford", "generalized_benford", "rodriguez", "hurlimann")

income_surveys_income_conformity[type == "posterior", c(vars_convert) := lapply(.SD, function(x) {
	
	x <- ifelse(x < 0, 0, x)
	x <- ifelse(x > 1, 1, x)

	x <- round(x, 4)
	}), .SDcols = vars_convert]

# Round optimal parameters and Bayes factors
income_surveys_income_conformity[type %in% c("optim", "log10BF01"), c(vars_convert) := lapply(.SD, function(x) {
	
	x <- round(x, 2)
	
	}), .SDcols = vars_convert]
	
# Proper order
setorderv(income_surveys_income_conformity, c("survey", "year", "level", "type"), c(-1, -1, 1, 1))

# Save point
save(income_surveys_income_conformity, file = "output/income_surveys_income_conformity.rdata", compress = "gzip")

#################
# Test conformity of all surveys to Benford's law (frequentist approach)

income_surveys_income_conformity_frequentist <- income_surveys[, test_conformity(income, bayesian = F), by = c("survey", "year", "level")]

# Negative or infinite probablities to zero
vars_convert <- c("contaminated_benford_chisq", 
"contaminated_benford_asq", "contaminated_benford_wsq", "contaminated_benford_usq", 
"generalized_benford_chisq", "generalized_benford_asq", "generalized_benford_wsq", 
"generalized_benford_usq", "rodriguez_chisq", "rodriguez_asq", 
"rodriguez_wsq", "rodriguez_usq", "hurlimann_chisq", "hurlimann_asq", 
"hurlimann_wsq", "hurlimann_usq", "benford_chisq", "benford_asq", 
"benford_wsq", "benford_usq", "stigler_chisq", "stigler_asq", 
"stigler_wsq", "stigler_usq", "uniform_chisq", "uniform_asq", 
"uniform_wsq", "uniform_usq")

income_surveys_income_conformity_frequentist[type == "pvalue", c(vars_convert) := lapply(.SD, function(x) {
	
	x <- ifelse(x < 0, 0, x)
	x <- ifelse(x > 1, 1, x)

	x <- round(x, 4)
	}), .SDcols = vars_convert]

# Round values of test statistics
income_surveys_income_conformity_frequentist[type %in% c("stat"), c(vars_convert) := lapply(.SD, function(x) {
	
	x <- round(x, 2)
	
	}), .SDcols = vars_convert]
	
# Proper order
setorderv(income_surveys_income_conformity_frequentist, c("survey", "year", "level", "type"), c(-1, -1, 1, 1))

save(income_surveys_income_conformity_frequentist, file = "output/income_surveys_income_conformity_frequentist.rdata", compress = "gzip")
