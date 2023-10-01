library(data.table)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# iconv -f CP1251 -t UTF-8 cbr_survey_waves_1_5_individuals.csv > cbr_survey_waves_1_5_individuals_utf8.csv
# iconv -f CP1251 -t UTF-8 cbr_survey_waves_1_5_households.csv > cbr_survey_waves_1_5_households_utf8.csv

cbr_survey_waves_1_5_individual <- fread("survey_data/cbr_consumer_finance_survey/cbr_survey_waves_1_5_individuals.csv.gz", na.strings = c("NA", "", "99999996", "99999997", "99999998", "99999999"))
save(cbr_survey_waves_1_5_individual, file = "survey_data/cbr_consumer_finance_survey/cbr_survey_waves_1_5_individual.rdata", compress = "gzip")

cbr_survey_waves_1_5_household <- fread("survey_data/cbr_consumer_finance_survey/cbr_survey_waves_1_5_households.csv.gz", na.strings = c("NA", "", "99999996", "99999997", "99999998", "99999999"))
save(cbr_survey_waves_1_5_household, file = "survey_data/cbr_consumer_finance_survey/cbr_survey_waves_1_5_household.rdata", compress = "gzip")
