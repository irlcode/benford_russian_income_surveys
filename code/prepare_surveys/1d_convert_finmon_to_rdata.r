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

##########
# Household-level

# List all HSE financial monitoring individual-level representative .sav files 
survey_files <- list.files("finmon/", pattern = ".sav", recursive = T)

for(f in survey_files) {
	
	# Debug: f <- survey_files[1]
	message(f)

	surveypath <- paste0(getwd(), "/finmon/", f)

	# Proper survey name
	survey_year <- str_extract(surveypath, "[\\d+]{4}")
	survey_name <- paste0("finmon_", survey_year, "_household")

	# Import data
	temp <- as.data.table(read_sav(surveypath))
	
	# To a designated object
	assign(survey_name, temp)
	
	# Save point
	save(list = survey_name, file = paste0("finmon/rdata/", survey_name, ".rdata"), compress = "gzip")
		
	# Clean up
	rm(list = survey_name)
	rm(temp)
	gc()
	
}
