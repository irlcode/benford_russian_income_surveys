library(data.table)
library(foreign)
library(stringi)
library(stringr)
library(labelled)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(paste0(path, "/survey_data"))

############################
# RLMS, 1994-2021 combined

##########
# Household-level
load("rlms/rlms_household.rdata")

# Variables of interest
# ID_W — wave ID
# ID_H — HOUSEHOLD ID (unique within round)
# SITE — survey area ID (unique within round), dvor
# G5 — ASSESS THE RESPONDENTS SHARPNESS: 1 VERY SLOW-WITTED, 2 SLOW-WITTED, NEEDED ADDITIONAL EXPLANATIONS, 3 AS BRIGHT AS THE MAJORITY OF RESPONDENTS, 4 NOTABLY BRIGHTER THAN THE MAJORITY OF RESPONDENTS
# 99999997 DOES NOT KNOW
# 99999998 REFUSES TO ANSWER
# 99999999 NO ANSWER
# F14 TOTAL INCOME IN LAST 30 DAYS
# F2 How much money does your family need per month, in order to live normally? rubles. 
# No interviewer IDs

rlms_household <- rlms_household[, c("id_w", "id_h", "site", "g5", "f14", "f2"), with = F]

# Year variable
rlms_household[, year := as.numeric(as.character(id_w))]

# Proper names and order
rlms_household[, survey := "RLMS"]
rlms_household[, level := "household"]
setnames(rlms_household, c("id_h", "site", "g5", "f14", "f2"), c("id", "area_id", "confidence", "income", "amount_living_normally"))
rlms_household <- rlms_household[, c("survey", "level", "year", "id", "area_id", "confidence", "income", "amount_living_normally"), with = F]

# Refusals and don't knows to NAs
rlms_household[, names(rlms_household) := lapply(.SD, function(x) ifelse(x %in% c("99999996", "99999997", "99999998", "99999999"), NA, x)) ]
gc()

##########
# Individual-level

# id_w — wave ID
# idind — PERSON ID (same across rounds)
# id_h — HOUSEHOLD ID (unique within round)
# S5 — ASSESS THE RESPONDENTS SHARPNESS: 1 VERY SLOW-WITTED, 2 SLOW-WITTED, NEEDED ADDITIONAL EXPLANATIONS, 3 AS BRIGHT AS THE MAJORITY OF RESPONDENTS, 4 NOTABLY BRIGHTER THAN THE MAJORITY OF RESPONDENTS
# J60 TOTAL INCOME IN LAST 30 DAYS
# J69.8A — MONEY TO LIVE NORMALLY
# J69.8B — MONEY TO LIVE WELL
# J69.8C MONEY TO LIVE POORLY
# No interviewer IDs

load("rlms/rlms_individual.rdata")

rlms_individual <- rlms_individual[, c("id_w", "idind", "site", "s5", "j60", "j69.8a", "j69.8b", "j69.8c"), with = F]

# Year variable
rlms_individual[, year := as.numeric(as.character(id_w))]

# Confidence variable
rlms_individual[, confidence := s5]

# Proper names and order
rlms_individual[, survey := "RLMS"]
rlms_individual[, level := "individual"]
setnames(rlms_individual, c("idind", "site", "j60", "j69.8a", "j69.8b", "j69.8c"), c("id", "area_id", "income", "amount_living_normally", "amount_living_well", "amount_living_poorly"))
rlms_individual <- rlms_individual[, c("survey", "level", "year", "id", "area_id", "confidence", "income", "amount_living_normally", "amount_living_well", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
rlms_individual[, names(rlms_individual) := lapply(.SD, function(x) ifelse(x %in% c("99999996", "99999997", "99999998", "99999999"), NA, x)) ]
gc()

############################
# KOUZH 2011 (household-level)
# https://gks.ru/free_doc/new_site/KOUZ/survey0/index.html

load("kouzh/rdata/kouzh_2011_household.rdata")

# ID_HH — household id
# CitySize — city population (used as area ID)
# H_04_03_01_00 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая?
# H_04_03_02_00 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо?
# H_04_03_03_00 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно?
# Income — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)

# No interviewer IDs

kouzh_2011_household <- kouzh_2011_household[, c("ID_HH", "CitySize", "H_04_03_01_00", "H_04_03_02_00", "H_04_03_03_00", "Income"), with = F]

# Remove attributes
kouzh_2011_household[, names(kouzh_2011_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2011_household <- remove_attributes(kouzh_2011_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2011_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2011_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2011_household[, year := 2011]

# Proper names and order
kouzh_2011_household[, survey := "KOUZh"]
kouzh_2011_household[, level := "household"]
setnames(kouzh_2011_household, c("ID_HH", "CitySize", "H_04_03_01_00", "H_04_03_02_00", "H_04_03_03_00", "Income"), c("id", "area_id", "amount_living_well", "amount_living_normally", "amount_living_poorly", "income"))
kouzh_2011_household <- kouzh_2011_household[, c("survey", "level", "year", "id", "area_id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
kouzh_2011_household[, names(kouzh_2011_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# KOUZH 2014 (household-level)
# https://rosstat.gov.ru/free_doc/new_site/KOUZ14/survey0/index.html

# H00_06 — household number
# H04_03_01 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая?
# H04_03_02 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо?
# H04_03_03 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно?
# DOX_mean — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)
# No area IDs, no interviewer IDs

load("kouzh/rdata/kouzh_2014_household.rdata")

kouzh_2014_household <- kouzh_2014_household[, c("H00_06", "H04_03_01", "H04_03_02", "H04_03_03", "DOX_mean"), with = F]

# Remove attributes
kouzh_2014_household[, names(kouzh_2014_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2014_household <- remove_attributes(kouzh_2014_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2014_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2014_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2014_household[, year := 2014]

# Proper names and order
kouzh_2014_household[, survey := "KOUZh"]
kouzh_2014_household[, level := "household"]
setnames(kouzh_2014_household, c("H00_06", "DOX_mean", "H04_03_01", "H04_03_02", "H04_03_03"), c("id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"))
kouzh_2014_household <- kouzh_2014_household[, c("survey", "level", "year", "id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
kouzh_2014_household[, names(kouzh_2014_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# KOUZH 2016 (household-level)
# https://gks.ru/free_doc/new_site/KOUZ16/index.html

# H00_06 — household number
# H04_051 — Какой минимальный месячный доход необходим Вашему домохозяйству, чтобы домохозяйство могло "свести концы с концами" при покупке самого необходимого?
# DOX_mean — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)
# No area IDs, no interviewer IDs

load("kouzh/rdata/kouzh_2016_household.rdata")

kouzh_2016_household <- kouzh_2016_household[, c("H00_06", "H04_051", "DOX_mean"), with = F]

# Remove attributes
kouzh_2016_household[, names(kouzh_2016_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2016_household <- remove_attributes(kouzh_2016_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2016_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2016_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2016_household[, year := 2016]

# Proper names and order
kouzh_2016_household[, survey := "KOUZh"]
kouzh_2016_household[, level := "household"]
setnames(kouzh_2016_household, c("H00_06", "DOX_mean", "H04_051"), c("id", "income", "amount_living_poorly"))
kouzh_2016_household <- kouzh_2016_household[, c("survey", "level", "year", "id", "income", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
kouzh_2016_household[, names(kouzh_2016_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# KOUZH 2018 (household-level)
# https://gks.ru/free_doc/new_site/KOUZ18/index.html

# H00_06 — household number
# H04_051 — Какой минимальный месячный доход необходим Вашему домохозяйству, чтобы домохозяйство могло "свести концы с концами" при покупке самого необходимого?
# ^ this variable is missing
# DOX_mean — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)
# Variable is missing in actual data, despite documentation
# No area IDs, no interviewer IDs

load("kouzh/rdata/kouzh_2018_household.rdata")

kouzh_2018_household <- kouzh_2018_household[, c("H00_06", "DOX_mean"), with = F]

# Remove attributes
kouzh_2018_household[, names(kouzh_2018_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2018_household <- remove_attributes(kouzh_2018_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2018_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2018_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2018_household[, year := 2018]

# Proper names and order
kouzh_2018_household[, survey := "KOUZh"]
kouzh_2018_household[, level := "household"]
setnames(kouzh_2018_household, c("H00_06", "DOX_mean"), c("id", "income"))
kouzh_2018_household <- kouzh_2018_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
kouzh_2018_household[, names(kouzh_2018_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()


############################
# KOUZH 2020 (household-level)
# https://rosstat.gov.ru/free_doc/new_site/GKS_KOUZH-2020/index.html

# H00_06 — household number
# H04_051 — Какой минимальный месячный доход необходим Вашему домохозяйству, чтобы домохозяйство могло "свести концы с концами" при покупке самого необходимого?
# DOX_mean — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)
# No area IDs, no interviewer IDs

load("kouzh/rdata/kouzh_2020_household.rdata")

kouzh_2020_household <- kouzh_2020_household[, c("H00_06", "H04_051", "DOX_mean"), with = F]

# Remove attributes
kouzh_2020_household[, names(kouzh_2020_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2020_household <- remove_attributes(kouzh_2020_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2020_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2020_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2020_household[, year := 2020]

# Proper names and order
kouzh_2020_household[, survey := "KOUZh"]
kouzh_2020_household[, level := "household"]
setnames(kouzh_2020_household, c("H00_06", "DOX_mean", "H04_051"), c("id", "income", "amount_living_poorly"))
kouzh_2020_household <- kouzh_2020_household[, c("survey", "level", "year", "id", "income", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
kouzh_2020_household[, names(kouzh_2020_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# KOUZH 2022 (household-level)
# https://gks.ru/free_doc/new_site/GKS_KOUZH_2022/index.html

# H00_06 — household number
# H04_051 — Какой минимальный месячный доход необходим Вашему домохозяйству, чтобы домохозяйство могло "свести концы с концами" при покупке самого необходимого?
# DOX_mean — Доход домохозяйства в месяц (расчетная категориальная, не опросная переменная)
# No area IDs, no interviewer IDs

load("kouzh/rdata/kouzh_2022_household.rdata")

kouzh_2022_household <- kouzh_2022_household[, c("H00_06", "H04_051", "DOX_mean"), with = F]

# Remove attributes
kouzh_2022_household[, names(kouzh_2022_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
kouzh_2022_household <- remove_attributes(kouzh_2022_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(kouzh_2022_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
kouzh_2022_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
kouzh_2022_household[, year := 2022]

# Proper names and order
kouzh_2022_household[, survey := "KOUZh"]
kouzh_2022_household[, level := "household"]
setnames(kouzh_2022_household, c("H00_06", "DOX_mean", "H04_051"), c("id", "income", "amount_living_poorly"))
kouzh_2022_household <- kouzh_2022_household[, c("survey", "level", "year", "id", "income", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
kouzh_2022_household[, names(kouzh_2022_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2012
# https://rosstat.gov.ru/free_doc/new_site/USP/survey0/index.html

##########
# Household-level

# H_R0V6 — Household number
# H_R8V4_1_1 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая
# H_R8V4_2_1 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо
# H_R8V4_3_1 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно
# DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable, we will not use it

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2012_household.rdata")

vndn_2012_household <- vndn_2012_household[, c("H_R0V6", "H_R8V4_1_1", "H_R8V4_2_1", "H_R8V4_3_1", "DEN"), with = F]

# Remove attributes
vndn_2012_household[, names(vndn_2012_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2012_household <- remove_attributes(vndn_2012_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2012_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2012_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2012_household[, year := 2012]

# Proper names and order
vndn_2012_household[, survey := "VNDN"]
vndn_2012_household[, level := "household"]
setnames(vndn_2012_household, c("H_R0V6", "DEN", "H_R8V4_1_1", "H_R8V4_2_1", "H_R8V4_3_1"), c("id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"))
vndn_2012_household <- vndn_2012_household[, c("survey", "level", "year", "id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
vndn_2012_household[, names(vndn_2012_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# I_R0V6 – Household number
# I_R0V7 – Individual number
# R_I_FOT_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2012_individual.rdata")

vndn_2012_individual <- vndn_2012_individual[, c("I_R0V6", "I_R0V7", "R_I_FOT_DEN"), with = F]

# Remove attributes
vndn_2012_individual[, names(vndn_2012_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2012_individual <- remove_attributes(vndn_2012_individual, attributes = c("label"))

# Convert columns to numeric
#coltypes <- sapply(vndn_2012_individual, typeof)
#coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
#vndn_2012_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2012_individual[, id := paste0(I_R0V6, "_", I_R0V7)]

# Year variable
vndn_2012_individual[, year := 2012]

# Proper names and order
vndn_2012_individual[, survey := "VNDN"]
vndn_2012_individual[, level := "individual"]
setnames(vndn_2012_individual, c("R_I_FOT_DEN"), c("income"))
vndn_2012_individual <- vndn_2012_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2012_individual[, names(vndn_2012_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2014
# https://rosstat.gov.ru/free_doc/new_site/vndn/index.html

##########
# Household-level

# H00_06 — Household number
# H07_08_01 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая
# H07_08_02 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо
# H07_08_03 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2014_household.rdata")

vndn_2014_household <- vndn_2014_household[, c("H00_06", "H07_08_01", "H07_08_02", "H07_08_03", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2014_household[, names(vndn_2014_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2014_household <- remove_attributes(vndn_2014_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2014_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2014_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2014_household[, year := 2014]

# Proper names and order
vndn_2014_household[, survey := "VNDN"]
vndn_2014_household[, level := "household"]
setnames(vndn_2014_household, c("H00_06", "R_H_DOXOD_DEN", "H07_08_01", "H07_08_02", "H07_08_03"), c("id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"))
vndn_2014_household <- vndn_2014_household[, c("survey", "level", "year", "id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
vndn_2014_household[, names(vndn_2014_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2014_individual.rdata")

vndn_2014_individual <- vndn_2014_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2014_individual[, names(vndn_2014_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2014_individual <- remove_attributes(vndn_2014_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2014_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2014_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2014_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2014_individual[, year := 2014]

# Proper names and order
vndn_2014_individual[, survey := "VNDN"]
vndn_2014_individual[, level := "individual"]
setnames(vndn_2014_individual, c("R_DEN"), c("income"))
vndn_2014_individual <- vndn_2014_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2014_individual[, names(vndn_2014_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2015
# http://gks.ru/free_doc/new_site/vndn-2015/index.html

##########
# Household-level

# H00_06 — Household number
# H07_08_01 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая
# H07_08_02 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо
# H07_08_03 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2015_household.rdata")

vndn_2015_household <- vndn_2015_household[, c("H00_06", "H07_08_01", "H07_08_02", "H07_08_03", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2015_household[, names(vndn_2015_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2015_household <- remove_attributes(vndn_2015_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2015_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2015_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2015_household[, year := 2015]

# Proper names and order
vndn_2015_household[, survey := "VNDN"]
vndn_2015_household[, level := "household"]
setnames(vndn_2015_household, c("H00_06", "R_H_DOXOD_DEN", "H07_08_01", "H07_08_02", "H07_08_03"), c("id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"))
vndn_2015_household <- vndn_2015_household[, c("survey", "level", "year", "id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
vndn_2015_household[, names(vndn_2015_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2015_individual.rdata")

vndn_2015_individual <- vndn_2015_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2015_individual[, names(vndn_2015_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2015_individual <- remove_attributes(vndn_2015_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2015_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2015_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2015_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2015_individual[, year := 2015]

# Proper names and order
vndn_2015_individual[, survey := "VNDN"]
vndn_2015_individual[, level := "individual"]
setnames(vndn_2015_individual, c("R_DEN"), c("income"))
vndn_2015_individual <- vndn_2015_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2015_individual[, names(vndn_2015_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()


############################
# Rosstat All-Russia Income Survey 2016
# http://gks.ru/free_doc/new_site/vndn-2016/index.html

##########
# Household-level

# H00_06 — Household number
# H07_08_01 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить очень хорошо, ни в чем себе не отказывая
# H07_08_02 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить хорошо
# H07_08_03 — Сколько денег, на Ваш взгляд, нужно иметь Вашему домохозяйству В МЕСЯЦ, чтобы жить удовлетворительно
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2016_household.rdata")

vndn_2016_household <- vndn_2016_household[, c("H00_06", "H07_08_01", "H07_08_02", "H07_08_03", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2016_household[, names(vndn_2016_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2016_household <- remove_attributes(vndn_2016_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2016_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2016_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2016_household[, year := 2016]

# Proper names and order
vndn_2016_household[, survey := "VNDN"]
vndn_2016_household[, level := "household"]
setnames(vndn_2016_household, c("H00_06", "R_H_DOXOD_DEN", "H07_08_01", "H07_08_02", "H07_08_03"), c("id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"))
vndn_2016_household <- vndn_2016_household[, c("survey", "level", "year", "id", "income", "amount_living_well", "amount_living_normally", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
vndn_2016_household[, names(vndn_2016_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2016_individual.rdata")

vndn_2016_individual <- vndn_2016_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2016_individual[, names(vndn_2016_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2016_individual <- remove_attributes(vndn_2016_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2016_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2016_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2016_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2016_individual[, year := 2016]

# Proper names and order
vndn_2016_individual[, survey := "VNDN"]
vndn_2016_individual[, level := "individual"]
setnames(vndn_2016_individual, c("R_DEN"), c("income"))
vndn_2016_individual <- vndn_2016_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2016_individual[, names(vndn_2016_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2017
# http://gks.ru/free_doc/new_site/vndn-2017/index.html

##########
# Household-level

# H00_06 — Household number
# H07_04 — Какой минимальный месячный доход необходим Вашему домохозяйству, чтобы "свести концы с концами", т.е. оплатить все необходимые ежеднев- ные платежи?
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2017_household.rdata")

vndn_2017_household <- vndn_2017_household[, c("H00_06", "H07_04", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2017_household[, names(vndn_2017_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2017_household <- remove_attributes(vndn_2017_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2017_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2017_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2017_household[, year := 2017]

# Proper names and order
vndn_2017_household[, survey := "VNDN"]
vndn_2017_household[, level := "household"]
setnames(vndn_2017_household, c("H00_06", "R_H_DOXOD_DEN", "H07_04"), c("id", "income", "amount_living_poorly"))
vndn_2017_household <- vndn_2017_household[, c("survey", "level", "year", "id", "income", "amount_living_poorly"), with = F]

# Refusals and don't knows to NAs
vndn_2017_household[, names(vndn_2017_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2017_individual.rdata")

vndn_2017_individual <- vndn_2017_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2017_individual[, names(vndn_2017_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2017_individual <- remove_attributes(vndn_2017_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2017_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2017_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2017_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2017_individual[, year := 2017]

# Proper names and order
vndn_2017_individual[, survey := "VNDN"]
vndn_2017_individual[, level := "individual"]
setnames(vndn_2017_individual, c("R_DEN"), c("income"))
vndn_2017_individual <- vndn_2017_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2017_individual[, names(vndn_2017_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2018
# http://gks.ru/free_doc/new_site/vndn-2018/index.html

##########
# Household-level

# H00_06 — Household number
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2018_household.rdata")

vndn_2018_household <- vndn_2018_household[, c("H00_06", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2018_household[, names(vndn_2018_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2018_household <- remove_attributes(vndn_2018_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2018_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2018_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2018_household[, year := 2018]

# Proper names and order
vndn_2018_household[, survey := "VNDN"]
vndn_2018_household[, level := "household"]
setnames(vndn_2018_household, c("H00_06", "R_H_DOXOD_DEN"), c("id", "income"))
vndn_2018_household <- vndn_2018_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2018_household[, names(vndn_2018_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2018_individual.rdata")

vndn_2018_individual <- vndn_2018_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2018_individual[, names(vndn_2018_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2018_individual <- remove_attributes(vndn_2018_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2018_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2018_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2018_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2018_individual[, year := 2018]

# Proper names and order
vndn_2018_individual[, survey := "VNDN"]
vndn_2018_individual[, level := "individual"]
setnames(vndn_2018_individual, c("R_DEN"), c("income"))
vndn_2018_individual <- vndn_2018_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2018_individual[, names(vndn_2018_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2019
# http://gks.ru/free_doc/new_site/vndn-2019/index.html

##########
# Household-level

# H00_06 — Household number
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2019_household.rdata")

vndn_2019_household <- vndn_2019_household[, c("H00_06", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2019_household[, names(vndn_2019_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2019_household <- remove_attributes(vndn_2019_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2019_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2019_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2019_household[, year := 2019]

# Proper names and order
vndn_2019_household[, survey := "VNDN"]
vndn_2019_household[, level := "household"]
setnames(vndn_2019_household, c("H00_06", "R_H_DOXOD_DEN"), c("id", "income"))
vndn_2019_household <- vndn_2019_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2019_household[, names(vndn_2019_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2019_individual.rdata")

vndn_2019_individual <- vndn_2019_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2019_individual[, names(vndn_2019_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2019_individual <- remove_attributes(vndn_2019_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2019_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2019_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2019_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2019_individual[, year := 2019]

# Proper names and order
vndn_2019_individual[, survey := "VNDN"]
vndn_2019_individual[, level := "individual"]
setnames(vndn_2019_individual, c("R_DEN"), c("income"))
vndn_2019_individual <- vndn_2019_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2019_individual[, names(vndn_2019_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2020
# http://gks.ru/free_doc/new_site/vndn-2020/index.html

##########
# Household-level

# H00_06 — Household number
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2020_household.rdata")

vndn_2020_household <- vndn_2020_household[, c("H00_06", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2020_household[, names(vndn_2020_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2020_household <- remove_attributes(vndn_2020_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2020_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2020_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2020_household[, year := 2020]

# Proper names and order
vndn_2020_household[, survey := "VNDN"]
vndn_2020_household[, level := "household"]
setnames(vndn_2020_household, c("H00_06", "R_H_DOXOD_DEN"), c("id", "income"))
vndn_2020_household <- vndn_2020_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2020_household[, names(vndn_2020_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2020_individual.rdata")

vndn_2020_individual <- vndn_2020_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2020_individual[, names(vndn_2020_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2020_individual <- remove_attributes(vndn_2020_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2020_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2020_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2020_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2020_individual[, year := 2020]

# Proper names and order
vndn_2020_individual[, survey := "VNDN"]
vndn_2020_individual[, level := "individual"]
setnames(vndn_2020_individual, c("R_DEN"), c("income"))
vndn_2020_individual <- vndn_2020_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2020_individual[, names(vndn_2020_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2021
# http://gks.ru/free_doc/new_site/vndn-2021/index.html

##########
# Household-level

# H00_06 — Household number
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2021_household.rdata")

vndn_2021_household <- vndn_2021_household[, c("H00_06", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2021_household[, names(vndn_2021_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2021_household <- remove_attributes(vndn_2021_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2021_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2021_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2021_household[, year := 2021]

# Proper names and order
vndn_2021_household[, survey := "VNDN"]
vndn_2021_household[, level := "household"]
setnames(vndn_2021_household, c("H00_06", "R_H_DOXOD_DEN"), c("id", "income"))
vndn_2021_household <- vndn_2021_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2021_household[, names(vndn_2021_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2021_individual.rdata")

vndn_2021_individual <- vndn_2021_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2021_individual[, names(vndn_2021_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2021_individual <- remove_attributes(vndn_2021_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2021_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2021_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2021_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2021_individual[, year := 2021]

# Proper names and order
vndn_2021_individual[, survey := "VNDN"]
vndn_2021_individual[, level := "individual"]
setnames(vndn_2021_individual, c("R_DEN"), c("income"))
vndn_2021_individual <- vndn_2021_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2021_individual[, names(vndn_2021_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

############################
# Rosstat All-Russia Income Survey 2022
# http://gks.ru/free_doc/new_site/vndn-2022/index.html

##########
# Household-level

# H00_06 — Household number
# R_H_DOXOD_DEN — Денежный доход, всего (денежное вознаграждение до выплаты подоходного налога, включая денежную оценку льгот, предоставленных работодателем по основному месту работы + доход от самостоятельной деятельности + доход от другой трудовой деятельности). NB: this is an estimated variable

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2022_household.rdata")

vndn_2022_household <- vndn_2022_household[, c("H00_06", "R_H_DOXOD_DEN"), with = F]

# Remove attributes
vndn_2022_household[, names(vndn_2022_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2022_household <- remove_attributes(vndn_2022_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2022_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2022_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
vndn_2022_household[, year := 2022]

# Proper names and order
vndn_2022_household[, survey := "VNDN"]
vndn_2022_household[, level := "household"]
setnames(vndn_2022_household, c("H00_06", "R_H_DOXOD_DEN"), c("id", "income"))
vndn_2022_household <- vndn_2022_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2022_household[, names(vndn_2022_household) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()

##########
# Individual-level

# H00_06 – Household number
# H01_00 – Individual number
# R_DEN — Сумма денежного вознаграждения до выплаты подоходного налога по основному месту работы. NB: this is an estimated variable.

# No area IDs, no interviewer IDs

load("vndn/rdata/vndn_2022_individual.rdata")

vndn_2022_individual <- vndn_2022_individual[, c("H00_06", "H01_00", "R_DEN"), with = F]

# Remove attributes
vndn_2022_individual[, names(vndn_2022_individual) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
vndn_2022_individual <- remove_attributes(vndn_2022_individual, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(vndn_2022_individual, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
vndn_2022_individual[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# ID variable is concatenation of household and individual id
vndn_2022_individual[, id := paste0(H00_06, "_", H01_00)]

# Year variable
vndn_2022_individual[, year := 2022]

# Proper names and order
vndn_2022_individual[, survey := "VNDN"]
vndn_2022_individual[, level := "individual"]
setnames(vndn_2022_individual, c("R_DEN"), c("income"))
vndn_2022_individual <- vndn_2022_individual[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
vndn_2022_individual[, names(vndn_2022_individual) := lapply(.SD, function(x) ifelse(x %in% c("-7", "-1"), NA, x)) ]
gc()


############################
# Central Bank/Ministry of Finance consumer finance survey
# 2013-2022 combinded
# https://www.cbr.ru/ec_research/vserossiyskoe-obsledovanie-domokhozyaystv-po-potrebitel-skim-finansam/

##########
# Household-level

# id_w - wave id
# id_h — household id

# Waves 2013, 2015, 2018, 2020
# h31_r И, заканчивая эту часть нашей беседы, скажите, пожалуйста, каким был денежный доход все- го Вашего домохозяйства в течение последних 30 дней? Включите сюда все денежные поступления всех членов домохозяйства: заработную плату, пенсии, стипендии, любые другие де- нежные поступления. Укажите сумму в рублях.

# Wave 2022
# h48_r Скажите, пожалуйста, каков весь средний ежемесячный суммарный доход Вашего домохозяйства? Теперь включите сюда все денежные поступления всех членов домохозяйства: заработную плату, пенсии, стипендии, любые другие денежные поступления, в том числе и нерегулярные, например, премии, выплачиваемые раз в год. Укажите сумму в рублях.

# h_x5 ОЦЕНИТЕ СООБРАЗИТЕЛЬНОСТЬ РЕСПОНДЕНТА ОТНОСИТЕЛЬНО ДРУГИХ РЕСПОНДЕНТОВ ИЗ ВАШЕГО ОПЫТА
# ОЧЕНЬ НЕСООБРАЗИТЕЛЬНЫЙ 1
# МЕНЕЕ СООБРАЗИТЕЛЬНЫЙ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ 2
# СООБРАЗИТЕЛЕН, КАК БОЛЬШИНСТВО РЕСПОНДЕНТОВ 3
# ЗНАЧИТЕЛЬНО СООБРАЗИТЕЛЬНЕЕ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ 4
# (this variable is not available for wave 2022)

# No area IDs, no interviewer IDs

load("cbr_consumer_finance_survey/cbr_survey_waves_1_5_household.rdata")

cbr_survey_waves_1_5_household <- cbr_survey_waves_1_5_household[, c("id_w", "id_h", "h31_r", "h48_r", "h_x5")]

# Unify income variable
cbr_survey_waves_1_5_household[, income := h31_r]
cbr_survey_waves_1_5_household[is.na(income), income := h48_r]

# Year variable
cbr_survey_waves_1_5_household[, year := as.numeric(id_w)]

# Confidence variable
cbr_survey_waves_1_5_household[, confidence := NA_real_]
cbr_survey_waves_1_5_household[h_x5 == "ОЧЕНЬ НЕСООБРАЗИТЕЛЬНЫЙ", confidence := 1]
cbr_survey_waves_1_5_household[h_x5 == "МЕНЕЕ СООБРАЗИТЕЛЬНЫЙ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ", confidence := 2]
cbr_survey_waves_1_5_household[h_x5 == "СООБРАЗИТЕЛЕН, КАК БОЛЬШИНСТВО РЕСПОНДЕНТОВ", confidence := 3]
cbr_survey_waves_1_5_household[h_x5 == "ЗНАЧИТЕЛЬНО СООБРАЗИТЕЛЬНЕЕ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ", confidence := 4]

# Confidence variable is unavailable for 2022, use individual-level variable here
load("cbr_consumer_finance_survey/cbr_survey_waves_1_5_individual.rdata")
min_confidence_per_household_wave <- cbr_survey_waves_1_5_individual[id_w == 2022, list(confidence_inferred = min(i_x5)), by = c("id_h", "id_w")]
cbr_survey_waves_1_5_household <- merge(cbr_survey_waves_1_5_household, min_confidence_per_household_wave, by = c("id_h", "id_w"), all.x = T, all.y = F)
cbr_survey_waves_1_5_household[is.na(confidence) & !is.na(confidence_inferred), confidence := confidence_inferred]
cbr_survey_waves_1_5_household[, confidence_inferred := NULL ]
# Proper names and order
cbr_survey_waves_1_5_household[, survey := "VODPF"]
cbr_survey_waves_1_5_household[, level := "household"]
setnames(cbr_survey_waves_1_5_household, c("id_h"), c("id"))
cbr_survey_waves_1_5_household <- cbr_survey_waves_1_5_household[, c("survey", "level", "year", "id", "confidence", "income"), with = F]

# Refusals and don't knows to NAs
cbr_survey_waves_1_5_household[, names(cbr_survey_waves_1_5_household) := lapply(.SD, function(x) ifelse(x %in% c("99999996", "99999997", "99999998", "99999999"), NA, x)) ]
gc()

##########
# Individual-level

# id_w - wave id
# id_i — individual id

# Waves 2013, 2015, 2018, 2020
#k66_r Пожалуйста, попробуйте вспомнить, сколько всего денег в течение последних 30 дней Вы лично получили. Посчитайте всё: зарплату, пенсии, премии, прибыли, пособия, стипендии, материальную помощь, случайные заработки и другие денежные поступления, в том числе и в валюте, но валюту переведите в рубли.

# Wave 2022
# k66a_r

# i_x5 ОЦЕНИТЕ СООБРАЗИТЕЛЬНОСТЬ РЕСПОНДЕНТА ОТНОСИТЕЛЬНО ДРУГИХ РЕСПОНДЕНТОВ ИЗ ВАШЕГО ОПЫТА
# ОЧЕНЬ НЕСООБРАЗИТЕЛЬНЫЙ 1
# МЕНЕЕ СООБРАЗИТЕЛЬНЫЙ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ 2
# СООБРАЗИТЕЛЕН, КАК БОЛЬШИНСТВО РЕСПОНДЕНТОВ 3
# ЗНАЧИТЕЛЬНО СООБРАЗИТЕЛЬНЕЕ, ЧЕМ БОЛЬШИНСТВО РЕСПОНДЕНТОВ 4

load("cbr_consumer_finance_survey/cbr_survey_waves_1_5_individual.rdata")

cbr_survey_waves_1_5_individual <- cbr_survey_waves_1_5_individual[, c("id_w", "i_x5", "id_i", "k66_r", "k66a_r")]

# Unify income variable
cbr_survey_waves_1_5_individual[, income := k66_r]
cbr_survey_waves_1_5_individual[is.na(income), income := k66a_r]

# Year variable
cbr_survey_waves_1_5_individual[, year := as.numeric(id_w)]

# Confidence variable
cbr_survey_waves_1_5_individual[, confidence := as.numeric(i_x5)]
cbr_survey_waves_1_5_individual[confidence > 4, confidence := NA]

# Proper names and order
cbr_survey_waves_1_5_individual[, survey := "VODPF"]
cbr_survey_waves_1_5_individual[, level := "individual"]
setnames(cbr_survey_waves_1_5_individual, c("id_i"), c("id"))
cbr_survey_waves_1_5_individual <- cbr_survey_waves_1_5_individual[, c("survey", "level", "year", "id", "confidence", "income"), with = F]

# Refusals and don't knows to NAs
cbr_survey_waves_1_5_individual[, names(cbr_survey_waves_1_5_individual) := lapply(.SD, function(x) ifelse(x %in% c("99999996", "99999997", "99999998", "99999999"), NA, x)) ]
gc()


############################
# HSE Financial Monitoring, 2011
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2411&T=m
# (numeric income variable first appeared this year)

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2011_household.rdata")

finmon_2011_household <- finmon_2011_household[, c("Respondent_Serial", "D26", "D26_dk"), with = F]

# Remove attributes
finmon_2011_household[, names(finmon_2011_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2011_household <- remove_attributes(finmon_2011_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2011_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2011_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2011_household[, year := 2011]

# Proper names and order
finmon_2011_household[, survey := "FINMON"]
finmon_2011_household[, level := "household"]
setnames(finmon_2011_household, c("Respondent_Serial", "D26"), c("id", "income"))
finmon_2011_household[!is.na(D26_dk), income := NA]
finmon_2011_household <- finmon_2011_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
#finmon_2011_household[, names(finmon_2011_household) := lapply(.SD, function(x) ifelse(x %in% c("99"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2012
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2411&T=m

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2012_household.rdata")

finmon_2012_household <- finmon_2012_household[, c("Respondent_Serial", "D26", "D26_dk"), with = F]

# Remove attributes
finmon_2012_household[, names(finmon_2012_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2012_household <- remove_attributes(finmon_2012_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2012_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2012_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2012_household[, year := 2012]

# Proper names and order
finmon_2012_household[, survey := "FINMON"]
finmon_2012_household[, level := "household"]
setnames(finmon_2012_household, c("Respondent_Serial", "D26"), c("id", "income"))
finmon_2012_household[!is.na(D26_dk), income := NA]
finmon_2012_household <- finmon_2012_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
#finmon_2012_household[, names(finmon_2012_household) := lapply(.SD, function(x) ifelse(x %in% c("99"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2013
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2428&T=q

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2013_household.rdata")

finmon_2013_household <- finmon_2013_household[, c("Respondent_Serial", "D26", "D26_DK"), with = F]

# Remove attributes
finmon_2013_household[, names(finmon_2013_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2013_household <- remove_attributes(finmon_2013_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2013_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2013_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2013_household[, year := 2013]

# Proper names and order
finmon_2013_household[, survey := "FINMON"]
finmon_2013_household[, level := "household"]
setnames(finmon_2013_household, c("Respondent_Serial", "D26"), c("id", "income"))
finmon_2013_household[!is.na(D26_DK), income := NA]
finmon_2013_household <- finmon_2013_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
#finmon_2013_household[, names(finmon_2013_household) := lapply(.SD, function(x) ifelse(x %in% c("99"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2014
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2411&T=m

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2014_household.rdata")

finmon_2014_household <- finmon_2014_household[, c("Respondent_Serial", "D26", "D26_DK"), with = F]

# Remove attributes
finmon_2014_household[, names(finmon_2014_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2014_household <- remove_attributes(finmon_2014_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2014_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2014_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2014_household[, year := 2014]

# Proper names and order
finmon_2014_household[, survey := "FINMON"]
finmon_2014_household[, level := "household"]
setnames(finmon_2014_household, c("Respondent_Serial", "D26"), c("id", "income"))
finmon_2014_household[!is.na(D26_DK), income := NA]
finmon_2014_household <- finmon_2014_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
#finmon_2014_household[, names(finmon_2014_household) := lapply(.SD, function(x) ifelse(x %in% c("99"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2015
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2338&T=m

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2015_household.rdata")

finmon_2015_household <- finmon_2015_household[, c("ID", "Д26"), with = F]

# Remove attributes
finmon_2015_household[, names(finmon_2015_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2015_household <- remove_attributes(finmon_2015_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2015_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2015_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2015_household[, year := 2015]

# Proper names and order
finmon_2015_household[, survey := "FINMON"]
finmon_2015_household[, level := "household"]
setnames(finmon_2015_household, c("ID", "Д26"), c("id", "income"))
finmon_2015_household <- finmon_2015_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
finmon_2015_household[, names(finmon_2015_household) := lapply(.SD, function(x) ifelse(x %in% c("99", "98"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2016
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2339&T=m

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2016_household.rdata")

finmon_2016_household <- finmon_2016_household[, c("ID", "Д26"), with = F]

# Remove attributes
finmon_2016_household[, names(finmon_2016_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2016_household <- remove_attributes(finmon_2016_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2016_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2016_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2016_household[, year := 2016]

# Proper names and order
finmon_2016_household[, survey := "FINMON"]
finmon_2016_household[, level := "household"]
setnames(finmon_2016_household, c("ID", "Д26"), c("id", "income"))
finmon_2016_household <- finmon_2016_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
finmon_2016_household[, names(finmon_2016_household) := lapply(.SD, function(x) ifelse(x %in% c("99", "98"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2017
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2429&T=m

# Respondent_Serial — Household number
# qD26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2017_household.rdata")

finmon_2017_household <- finmon_2017_household[, c("ID", "qД26"), with = F]

# Remove attributes
finmon_2017_household[, names(finmon_2017_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2017_household <- remove_attributes(finmon_2017_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2017_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2017_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2017_household[, year := 2017]

# Proper names and order
finmon_2017_household[, survey := "FINMON"]
finmon_2017_household[, level := "household"]
setnames(finmon_2017_household, c("ID", "qД26"), c("id", "income"))
finmon_2017_household <- finmon_2017_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
finmon_2017_household[, names(finmon_2017_household) := lapply(.SD, function(x) ifelse(x %in% c("99", "98"), NA, x)) ]
gc()

############################
# HSE Financial Monitoring, 2018
# http://sophist.hse.ru/db/oprview.shtml?ID_S=2473&T=m

# Respondent_Serial — Household number
# D26 — А ТЕПЕРЬ, УЧИТЫВАЯ ВСЕ ОТМЕЧЕННЫЕ ВАМИ ИСТОЧНИКИ ДЕНЕЖНЫХ ДОХОДОВ, ПОДСЧИТАЙТЕ, ПОЖАЛУЙСТА, КАКИМ БЫЛ ОБЩИЙ ДОХОД ЗА ПРОШЛЫЙ МЕСЯЦ, ПОЛУЧЕННЫЙ ВСЕМИ ЧЛЕНАМИ ВАШЕЙ СЕМЬИ, ПРОЖИВАЮЩИХ ВМЕСТЕ С ВАМИ, ЗА ВЫЧЕТОМ НАЛОГОВ?	

# No area IDs, no interviewer IDs
load("finmon/rdata/finmon_2018_household.rdata")

finmon_2018_household <- finmon_2018_household[, c("ID", "qД26"), with = F]

# Remove attributes
finmon_2018_household[, names(finmon_2018_household) := lapply(.SD, remove_attributes, attributes = c("label", "format.spss", "display_width", "labels"))]
finmon_2018_household <- remove_attributes(finmon_2018_household, attributes = c("label"))

# Convert columns to numeric
coltypes <- sapply(finmon_2018_household, typeof)
coltypes_to_numeric <- names(coltypes)[grepl("double", coltypes)]
finmon_2018_household[, c(coltypes_to_numeric) := lapply(.SD, as.numeric)]

# Year variable
finmon_2018_household[, year := 2018]

# Proper names and order
finmon_2018_household[, survey := "FINMON"]
finmon_2018_household[, level := "household"]
setnames(finmon_2018_household, c("ID", "qД26"), c("id", "income"))
finmon_2018_household <- finmon_2018_household[, c("survey", "level", "year", "id", "income"), with = F]

# Refusals and don't knows to NAs
finmon_2018_household[, names(finmon_2018_household) := lapply(.SD, function(x) ifelse(x %in% c("99", "98"), NA, x)) ]
gc()


############################
# Russian Crime Victimization Survey 2021
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/C2OTH9

load("rcvs/rcvs2021_extract_income_interviewer_id.rdata")

rcvs_2021_household <- copy(rcvs2021)

# Proper names and order
rcvs_2021_household[, survey := "RCVS"]
rcvs_2021_household[, year := 2021]
rcvs_2021_household[, level := "household"]

setnames(rcvs_2021_household, c("ID", "interviewer_id", "household_income"), c("id", "area_id", "income"))
rcvs_2021_household <- rcvs_2021_household[, c("survey", "level", "year", "id", "income"), with = F]

############################
# Survey of Income and Program Participation (USA) 2022
# https://www.census.gov/programs-surveys/sipp/data/datasets/2022-data/2022.html

# SSUID Sample unit identifier. This identifier is created by scrambling together PSU, Sequence #1, Sequence #2, and the Frame Indicator for a case. It may be used in matching sample units from different waves.
# SHHADID Household address ID. Used to differentiate households spawned from an original sample household.
# SPANEL Panel year
# PNUM Person number
# THTOTINC — Sum of monthly earnings and income received by household members age 15 and older, as well as SSI payments received by children under age 15
# TPTOTINC — Sum of personal monthly earnings and income for people age 15 and older, as well as children under age 15 who received SSI payments

##########
# Household-level

load("sipp/sipp_2022_household.rdata")

# Create id variable (household_month)
sipp_2022_household[, id := paste0(SSUID, "_", SHHADID, "_", MONTHCODE)]

# Year variable
sipp_2022_household[, year := as.numeric(SPANEL)]

# Proper names and order
sipp_2022_household[, survey := "SIPP"]
sipp_2022_household[, level := "household"]
setnames(sipp_2022_household, c("THTOTINC"), c("income"))
sipp_2022_household <- sipp_2022_household[, c("survey", "level", "year", "id", "income"), with = F]

##########
# Inidividual-level

load("sipp/sipp_2022_individual.rdata")

# Create id variable (household_month)
sipp_2022_individual[, id := paste0(SSUID, "_", SHHADID, "_", PNUM, "_", MONTHCODE)]

# Year variable
sipp_2022_individual[, year := as.numeric(SPANEL)]

# Proper names and order
sipp_2022_individual[, survey := "SIPP"]
sipp_2022_individual[, level := "individual"]
setnames(sipp_2022_individual, c("TPTOTINC"), c("income"))
sipp_2022_individual <- sipp_2022_individual[, c("survey", "level", "year", "id", "income"), with = F]


############################
# European Union Statistics on Income and Living Conditions (EU SILC) Synthetic Data
# https://ec.europa.eu/eurostat/web/microdata/european-union-statistics-on-income-and-living-conditions

##########
# Household-level

# HB010 — year
# HB020 — country two-letter ID
# HB030 — household id
# HY010 — total household gross income

load("eusilc/eusilc_household.rdata")

eusilc_household <- eusilc_household[, c("HB010", "HB020", "HB030", "HY010")]

# Proper names and order
setnames(eusilc_household, c("HB030", "HB020", "HB010", "HY010"), c("id", "area_id", "year", "income"))
eusilc_household[, survey := "EUSILC"]
eusilc_household[, level := "household"]
eusilc_household <- eusilc_household[, c("survey", "level", "year", "id", "area_id", "income"), with = F]
gc()

##########
# Individual-level

# PB010 — year
# PB020 — country two-letter ID
# PB030 — individual id
# PY010G — Employee cash or near cash income (gross)
# PY010N — Employee cash or near cash income (net)

load("eusilc/eusilc_individual.rdata")

eusilc_individual <- eusilc_individual[, c("PB010", "PB020", "PB030", "PY010G", "PY010N")]

# Income is gross
eusilc_individual[, income := PY010G]
# Not used: income is filled with net when gross is not available
#eusilc_individual[is.na(income) & !is.na(PY010N), income := PY010N]
eusilc_individual[, c("PY010G", "PY010N") := NULL ]

# Proper names and order
setnames(eusilc_individual, c("PB030", "PB020", "PB010"), c("id", "area_id", "year"))
eusilc_individual[, survey := "EUSILC"]
eusilc_individual[, level := "individual"]
eusilc_individual <- eusilc_individual[, c("survey", "level", "year", "id", "area_id", "income"), with = F]


############################
# All surveys to one object

income_surveys <- rbindlist(list(rlms_household, rlms_individual, vndn_2012_household, vndn_2012_individual, vndn_2014_household, vndn_2014_individual, vndn_2015_household, vndn_2015_individual, vndn_2016_household, vndn_2016_individual, vndn_2017_household, vndn_2017_individual, vndn_2018_household, vndn_2018_individual, vndn_2019_household, vndn_2019_individual, vndn_2020_household, vndn_2020_individual, vndn_2021_household, vndn_2021_individual, vndn_2022_household, vndn_2022_individual, cbr_survey_waves_1_5_household, cbr_survey_waves_1_5_individual, finmon_2011_household, finmon_2012_household, finmon_2013_household, finmon_2014_household, finmon_2015_household, finmon_2016_household, finmon_2017_household, finmon_2018_household, rcvs_2021_household, sipp_2022_household, sipp_2022_individual, eusilc_household, eusilc_individual), fill = T)

# Coerce to numeric
income_surveys[, income := gsub(",", ".", income, fixed = T)]
income_surveys[, income := as.numeric(income)]

# Save point
save(income_surveys, file = "income_surveys.rdata", compress = "gzip")
