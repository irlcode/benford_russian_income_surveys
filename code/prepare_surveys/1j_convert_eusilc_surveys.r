library(data.table)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# We use EUSILC data
# from https://ec.europa.eu/eurostat/web/microdata/public-microdata/statistics-on-income-and-living-conditions

# List all household/individual sur
eusilc_household_csvs <- paste0("survey_data/eusilc/", list.files("survey_data/eusilc/", recursive = T, pattern = ".*h_EUSILC.csv"))
eusilc_individual_csvs <- paste0("survey_data/eusilc/", list.files("survey_data/eusilc/", recursive = T, pattern = ".*p_EUSILC.csv"))

# Row-bind household data
eusilc_household <- rbindlist(lapply(eusilc_household_csvs, function(x) {
	
	out <- fread(x, na.strings = "")
	out[, file := x ]
	
}
), fill = T)

# Row-bind individual data
eusilc_individual <- rbindlist(lapply(eusilc_individual_csvs, function(x) {
	
	out <- fread(x, na.strings = "")
	out[, file := x ]
	
}
), fill = T)

# Save point
save(eusilc_household, file = "survey_data/eusilc/eusilc_household.rdata", compress = "gzip")
save(eusilc_individual, file = "survey_data/eusilc/eusilc_individual.rdata", compress = "gzip")