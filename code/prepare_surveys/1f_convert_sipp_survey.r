library(data.table)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# We use 2022 SIPP data
# from https://www.census.gov/programs-surveys/sipp/data/datasets/2022-data/2022.html

# SSUID Sample unit identifier. This identifier is created by scrambling together PSU, Sequence #1, Sequence #2, and the Frame Indicator for a case. It may be used in matching sample units from different waves.
# SHHADID Household address ID. Used to differentiate households spawned from an original sample household.
# SPANEL Panel year
# PNUM Person number
# MONTHCODE — month
# THTOTINC — Sum of monthly earnings and income received by household members age 15 and older, as well as SSI payments received by children under age 15
# TPTOTINC — Sum of personal monthly earnings and income for people age 15 and older, as well as children under age 15 who received SSI payments

sipp_2022 <- fread("survey_data/sipp/pu2022.csv.gz", na.strings = c("NA", "", "99999996", "99999997", "99999998", "99999999"), select = c("SSUID", "SHHADID", "SPANEL", "PNUM", "MONTHCODE", "THTOTINC", "TPTOTINC"))

# Household- and individual-level data
sipp_2022_household <- unique(sipp_2022[, -c("TPTOTINC", "PNUM")])
sipp_2022_individual <- unique(sipp_2022[, -c("THTOTINC")])

save(sipp_2022_household, file = "survey_data/sipp/sipp_2022_household.rdata", compress = "gzip")
save(sipp_2022_individual, file = "survey_data/sipp/sipp_2022_individual.rdata", compress = "gzip")
