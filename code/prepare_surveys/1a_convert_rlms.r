library(data.table)
library(haven)
library(labelled)
library(stringi)
library(stringr)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(paste0(path, "/survey_data/rlms"))

##########
# Household-level

# List all RLMS household-level representative .sav files 
survey_files <- list.files("Полная выборка_волны 5-31_01.09.2023", pattern = ".sav", recursive = T)
survey_files <- survey_files[ grepl("домох", survey_files, ignore.case = T)]

# Init an empty object
rlms_household <- data.table()

for(f in survey_files) {
	
		# Debug: f <- survey_files[22]
		message(f)

		surveypath <- paste0(getwd(), "/Полная выборка_волны 5-31_01.09.2023/", f)

		# Import data
		temp <- as.data.table(read_sav(surveypath, encoding = "cp1251"))
		
		# Manually keep only the variables to use in the research
		# Variables of interest
		# ID_W — wave ID
		# ID_H — HOUSEHOLD ID (unique within round)
		# SITE — survey area ID (unique within round)
		# G5 — ASSESS THE RESPONDENTS SHARPNESS
		# F14 TOTAL INCOME IN LAST 30 DAYS
		# F2 How much money does your family need per month, in order to live normally? rubles. 
		# We also add household ids in previous rounds

		# Variables of interest
		varnames <- names(temp)
		varnames_interest <- varnames[ grepl("(^[A-z]{1,2}id_h)|site|(f14$)|(g5$)|(f2$)", varnames, ignore.case = T)] 
		#varnames_interest <- varnames_interest[order(varnames_interest)]
		temp <- temp[, c(varnames_interest), with = F]
		
		# Rename variables to uniform
		varnames_new <- str_extract(varnames_interest, "f14|g5|f2")
		varnames_interest <- varnames_interest[!is.na(varnames_new)]
		varnames_new <- varnames_new[!is.na(varnames_new)]
		setnames(temp, varnames_interest, varnames_new)
		
		# id_h from the first id
		first_id_h <- names(temp)[grepl("id_h", names(temp), ignore.case = T)][1]
		temp[, id_h := get(first_id_h)]
		
		# Wave id from letter of the wave
		wave_letter <- substr(first_id_h, 1, 2)
		temp[ wave_letter == "ai", id_w := 1994 ]
		temp[ wave_letter == "bi", id_w := 1995 ]
		temp[ wave_letter == "ci", id_w := 1996 ]
		temp[ wave_letter == "di", id_w := 1998 ]
		temp[ wave_letter == "ei", id_w := 2000 ]
		temp[ wave_letter == "fi", id_w := 2001 ]
		temp[ wave_letter == "gi", id_w := 2002 ]
		temp[ wave_letter == "hi", id_w := 2003 ]
		temp[ wave_letter == "ii", id_w := 2004 ]
		temp[ wave_letter == "ji", id_w := 2005 ]
		temp[ wave_letter == "ki", id_w := 2006 ]
		temp[ wave_letter == "li", id_w := 2007 ]
		temp[ wave_letter == "mi", id_w := 2008 ]
		temp[ wave_letter == "ni", id_w := 2009 ]
		temp[ wave_letter == "oi", id_w := 2010 ]
		temp[ wave_letter == "pi", id_w := 2011 ]
		temp[ wave_letter == "qi", id_w := 2012 ]
		temp[ wave_letter == "ri", id_w := 2013 ]
		temp[ wave_letter == "si", id_w := 2014 ]
		temp[ wave_letter == "ti", id_w := 2015 ]
		temp[ wave_letter == "ui", id_w := 2016 ]
		temp[ wave_letter == "vi", id_w := 2017 ]
		temp[ wave_letter == "wi", id_w := 2018 ]
		temp[ wave_letter == "xi", id_w := 2019 ]
		temp[ wave_letter == "yi", id_w := 2020 ]
		temp[ wave_letter == "zi", id_w := 2021 ]
		temp[ wave_letter == "aa", id_w := 2022 ]
		
		# Proper format
		if( any(grepl("f14", varnames_new, ignore.case = T)) ) {
			
			temp[, f14 := as.numeric(f14)]
			
		}
		
		if( any(grepl("g7", varnames_new, ignore.case = T)) ) {

			temp[, g7 := as.numeric(g7)]
			
		}

		# Remove all attributes
		temp <- remove_attributes(temp, attributes = c("label", "format.spss", "display_width", "labels"))

		rlms_household <- rbind(rlms_household, temp, fill = T)
		
}

# Proper order
setcolorder(rlms_household, c("id_h", "id_w", "site", "f14", "f2", "g5"))
setkeyv(rlms_household, c("id_w", "id_h"))

save(rlms_household, file = "rlms_household.rdata", compress = "gzip")

##########
# Individual-level

# List all RLMS individual-level representative .sav files 
survey_files <- list.files("Полная выборка_волны 5-31_01.09.2023", pattern = ".sav", recursive = T)
survey_files <- survey_files[ grepl("индив", survey_files, ignore.case = T)]

# Init an empty object
rlms_individual <- data.table()

for(f in survey_files) {
	
		# Debug: f <- survey_files[22]
		message(f)

		surveypath <- paste0(getwd(), "/Полная выборка_волны 5-31_01.09.2023/", f)

		# Import data
		temp <- as.data.table(read_sav(surveypath, encoding = "cp1251"))
		
		# Manually keep only the variables to use in the research
		# id_w — wave ID
		# idind — PERSON ID (same across rounds)
		# id_h — HOUSEHOLD ID (unique within round)
		# S5 — ASSESS THE RESPONDENTS SHARPNESS: 1 VERY SLOW-WITTED, 2 SLOW-WITTED, NEEDED ADDITIONAL EXPLANATIONS, 3 AS BRIGHT AS THE MAJORITY OF RESPONDENTS, 4 NOTABLY BRIGHTER THAN THE MAJORITY OF RESPONDENTS
		# J60 TOTAL INCOME IN LAST 30 DAYS
		# J69.8A — MONEY TO LIVE NORMALLY
		# J69.8B — MONEY TO LIVE WELL
		# J69.8C MONEY TO LIVE POORLY

		# Variables of interest
		varnames <- names(temp)
		varnames_interest <- varnames[ grepl("(^[A-z]{1,2}id_h)|(^idind$)|site|(s5$)|(j60$)|(j69.8a$)|(j69.8b$)|(j69.8c$)", varnames, ignore.case = T)] 
		#varnames_interest <- varnames_interest[order(varnames_interest)]
		temp <- temp[, c(varnames_interest), with = F]
		
		# Rename variables to uniform
		varnames_new <- str_extract(varnames_interest, "j60|s5|j69.8a|j69.8b|j69.8c")
		varnames_interest <- varnames_interest[!is.na(varnames_new)]
		varnames_new <- varnames_new[!is.na(varnames_new)]
		setnames(temp, varnames_interest, varnames_new)
		
		# id_h from the first id
		first_id_h <- names(temp)[grepl("id_h", names(temp), ignore.case = T)][1]
		temp[, id_h := get(first_id_h)]
		
		# Wave id from letter of the wave
		wave_letter <- substr(first_id_h, 1, 2)
		temp[ wave_letter == "ai", id_w := 1994 ]
		temp[ wave_letter == "bi", id_w := 1995 ]
		temp[ wave_letter == "ci", id_w := 1996 ]
		temp[ wave_letter == "di", id_w := 1998 ]
		temp[ wave_letter == "ei", id_w := 2000 ]
		temp[ wave_letter == "fi", id_w := 2001 ]
		temp[ wave_letter == "gi", id_w := 2002 ]
		temp[ wave_letter == "hi", id_w := 2003 ]
		temp[ wave_letter == "ii", id_w := 2004 ]
		temp[ wave_letter == "ji", id_w := 2005 ]
		temp[ wave_letter == "ki", id_w := 2006 ]
		temp[ wave_letter == "li", id_w := 2007 ]
		temp[ wave_letter == "mi", id_w := 2008 ]
		temp[ wave_letter == "ni", id_w := 2009 ]
		temp[ wave_letter == "oi", id_w := 2010 ]
		temp[ wave_letter == "pi", id_w := 2011 ]
		temp[ wave_letter == "qi", id_w := 2012 ]
		temp[ wave_letter == "ri", id_w := 2013 ]
		temp[ wave_letter == "si", id_w := 2014 ]
		temp[ wave_letter == "ti", id_w := 2015 ]
		temp[ wave_letter == "ui", id_w := 2016 ]
		temp[ wave_letter == "vi", id_w := 2017 ]
		temp[ wave_letter == "wi", id_w := 2018 ]
		temp[ wave_letter == "xi", id_w := 2019 ]
		temp[ wave_letter == "yi", id_w := 2020 ]
		temp[ wave_letter == "zi", id_w := 2021 ]
		temp[ wave_letter == "aa", id_w := 2022 ]
		
		# Proper format
		if( any(grepl("j60", varnames_new, ignore.case = T)) ) {
			
			temp[, j60 := as.numeric(j60)]
			
		}
		
		if( any(grepl("s5", varnames_new, ignore.case = T)) ) {

			temp[, s5 := as.numeric(s5)]
			
		}
		
		if( any(grepl("j69.8a", varnames_new, ignore.case = T)) ) {

			temp[, j69.8a := as.numeric(j69.8a)]
			
		}
		
		if( any(grepl("j69.8b", varnames_new, ignore.case = T)) ) {

			temp[, j69.8b := as.numeric(j69.8b)]
			
		}
		
				
		if( any(grepl("j69.8c", varnames_new, ignore.case = T)) ) {

			temp[, j69.8c := as.numeric(j69.8c)]
			
		}

		# Remove all attributes
		temp <- remove_attributes(temp, attributes = c("label", "format.spss", "display_width", "labels"))

		rlms_individual <- rbind(rlms_individual, temp, fill = T)
		
}

# Proper order
setcolorder(rlms_individual, c("idind", "id_h", "id_w", "site", "j60", "j69.8a", "j69.8b", "j69.8c", "s5"))
setkeyv(rlms_individual, c("idind", "id_w", "id_h"))

save(rlms_individual, file = "rlms_individual.rdata", compress = "gzip")
