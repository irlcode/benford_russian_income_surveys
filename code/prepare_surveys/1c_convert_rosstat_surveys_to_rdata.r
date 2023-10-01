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

surveys <- c("vndn", "kouzh")

for(s in surveys) {

	# Debug: s <- "vndn"
	
	# List all .sav files manually exported from Nesstar raw files
	survey_files <- list.files(paste0(s, "/"), pattern = ".sav", recursive = T)
	
	for(f in survey_files) {
	
		# Debug: f <- survey_files[1]

		# Full path
		surveypath <- paste0(s, "/", f)
				
		#temp <- as.data.table(read.spss(surveypath, reencode = "cp1251"))
		temp <- as.data.table(read_sav(surveypath, encoding = "cp1251"))

		# Proper survey name
		survey_year <- str_extract(surveypath, "[\\d+]{4}")
		
		if( !grepl("obdx", surveypath) ) {
			
			# For non-OBDH survey the unit is in the file name
			survey_unit <- ifelse(grepl("hold", surveypath, ignore.case = T), "household", "individual")
			survey_name <- paste(s, survey_year, survey_unit, sep = "_")
		
		} else {
			
			# For OBDH FL files have the data on individuals, FC — on households
			survey_unit <- ifelse(grepl("FC", surveypath, ignore.case = T), "household", "individual")
			
		}
		
		# To a designated object
		assign(survey_name, temp)
		
		# Save point
		save(list = survey_name, file = paste0(s, "/rdata/", survey_name, ".rdata"), compress = "gzip")
		
		# Clean up
		rm(list = survey_name)
		rm(temp)
		gc()
		message(surveypath)
		
	}
	
}
