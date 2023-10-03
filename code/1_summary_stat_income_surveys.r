library(data.table)
library(digitTests)
library(vtable)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# Load the data prepared in prepare_surveys/2_combine_surveys.r
load("survey_data/income_surveys.rdata")

#############
# Function that takes a vector of values and returns
# a table with summary statistics
compute_summary_stat <- function(values) {
	
	# Debug: values <- income_surveys[survey == "RLMS" & year == 2011 & level == "household"]$income
	suppressWarnings(out <- as.data.table(sumtable(data = data.table(income = values), out = "csv", summ = c("notNA(x)", "countNA(x)", "mean(x)", "median(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[75]", "max(x)"), summ.names = c("N", "Missing", "Mean", "Median", "SD", "Min", "Pctl_25", "Pctl_75", "Max"))))
	out[, Variable := NULL]
	
	return(out)
	
}

# Compute summary statistics
income_surveys_summary_stat <- income_surveys[, compute_summary_stat(income), by = c("survey", "year", "level")]

# Proper order
setorderv(income_surveys_summary_stat, c("survey", "year", "level"), c(-1, -1, 1))

# Export to CSV
fwrite(income_surveys_summary_stat, file = "output/income_surveys_summary_stat.csv")

#############
# First digit distributions
# This function reports proportions
# of first digits in all surveys

compute_first_digit_distrib <- function(values) {

	# Debug: values <- income_surveys[survey == "RLMS" & year == 2011 & level == "household"]$income
	
	values <- values[!is.na(values)]
	values <- values[!is.infinite(values)]
	
	# Extract first significant digits
	x <- extract_digits(values, check = "first", include.zero = FALSE)
	x <- x[!is.na(x)]
	n <- length(x)
	
	# Observed counts of first significant digits
	x_tab <- table(x)
	dig <- 1:9
	x <- rep(0, length(dig))
	x_included <- as.numeric(names(x_tab))
	index <- x_included
	x[index] <- as.numeric(x_tab)

	# Proportions
	x_prop <- data.table(digit = 1:9, prop = x/sum(x))
	
	return(x_prop)
	
}

####
# Compute digit distribution in all surveys
income_surveys_digit_distrib <- income_surveys[, compute_first_digit_distrib(income), by = c("survey", "year", "level")]

# Long to wide
income_surveys_digit_distrib <- dcast(income_surveys_digit_distrib, survey + year + level ~ digit)

# Round
vars_to_round <- as.character(1:9)
income_surveys_digit_distrib[, c(vars_to_round) := lapply(.SD, round, 3), .SDcols = vars_to_round]

# Proper order
setorderv(income_surveys_digit_distrib, c("survey", "year", "level"), c(-1, -1, 1))

# Export to CSV
fwrite(income_surveys_digit_distrib, file = "output/income_surveys_digit_distrib.csv")


####
# Compute digit distribution in RLMS

# Observations per secondary sampling unit-year
rlms_household_observations_per_area_id_year <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income), list(observations = .N), by = c("area_id", "year")]

# Keep only secondary sampling units with over 15 observations
rlms_household <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income) ]
rlms_household <- merge(rlms_household, rlms_household_observations_per_area_id_year, by = c("area_id", "year"), all.x = T, all.y = F)
rlms_household <- rlms_household[ observations >= 15 ]

# Compute digit distribution in all surveys
rlms_household_digit_distrib_per_area_id_year <- rlms_household[, compute_first_digit_distrib(income), by = c("survey", "year", "level", "area_id")]

# Long to wide
rlms_household_digit_distrib_per_area_id_year <- dcast(rlms_household_digit_distrib_per_area_id_year, survey + year + level + area_id ~ digit)

# Round
vars_to_round <- as.character(1:9)
rlms_household_digit_distrib_per_area_id_year[, c(vars_to_round) := lapply(.SD, round, 3), .SDcols = vars_to_round]
rlms_household_digit_distrib_per_area_id_year[, area_id := as.numeric(area_id)]

# Proper order
setorderv(rlms_household_digit_distrib_per_area_id_year, c("survey", "year", "level", "area_id"), c(-1, -1, -1, 1))

# Export to CSV
fwrite(rlms_household_digit_distrib_per_area_id_year, file = "output/rlms_household_digit_distrib_per_area_id_year.csv")
