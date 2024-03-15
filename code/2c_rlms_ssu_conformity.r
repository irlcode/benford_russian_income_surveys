library(data.table)
library(openxlsx)
library(lubridate)
library(strucchange)
library(fixest)
library(multcomp)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# Load the survey data prepared in prepare_surveys/2_combine_surveys.r
load("survey_data/income_surveys.rdata")

# Load the earliest years households were present data
# produced in code/prepare_surveys/3_identify_new_observations_per_wave.r
load("survey_data/income_surveys_first_year.rdata")

# Load the per-survey wave conformity with Benford
# produced in 2a_test_conformity_all_surveys.r
load("output/income_surveys_income_conformity.rdata")

# Load the per-secondary sampling unit conformity with Benford
# created by 2b_within_rlms_conformity.r
load("output/rlms_household_income_conformity_by_area_id_year.rdata")

# Load the consumer price index from Rosstat
# (https://rosstat.gov.ru/statistics/price)
temp <- as.data.table(read.xlsx("aux_data/ipc_mes-01.xlsx", sheet = 2))
cpi <- data.table(year = as.numeric(unlist(temp[3,-1])), index = as.numeric(unlist(temp[18,-1])))
cpi[, index := index/100]
cpi <- cpi[ year <= 2022 ]
# To 2022 base year
cpi[ year == 2022, index_base := 1 ]
for(y in 2021:min(cpi$year)) {

	cpi[year == y, index_base := cpi[year == (y + 1)]$index_base/(cpi[year == (y + 1)]$index)]

}
cpi[, index := NULL]

# Overall RLMS per-wave conformity
rlms_conformity <- income_surveys_income_conformity[ survey == "RLMS" & level == "household" & type == "posterior" ]

# Only household-level RLMS data
rlms_household <- income_surveys[survey == "RLMS" & level == "household" & !is.na(income) ]

# Add deflator
rlms_household <- merge(rlms_household, cpi, by = "year", all.x = T, all.y = F)

# Account for denomination that occurred in January 1998
rlms_household[year < 1998, income := income/1000 ]

# Compute real income using CPI
rlms_household[, realincome := income]
rlms_household[, realincome := realincome/index_base ]

# Add per-wave income to overall Benford conformity data
rlms_income_per_wave <- rlms_household[year >= 2000 & !is.na(income) & income > 0, list(median_income = median(income), mean_income = mean(income), median_realincome = median(realincome), mean_realincome = mean(realincome)), by = "year"]
rlms_conformity <- merge(rlms_conformity, rlms_income_per_wave, by = "year")

# Earliest survey year per household
rlms_household <- merge(rlms_household, income_surveys_first_year, by = c("survey", "level", "id", "year"), all.x = T, all.y = F)

# Mark new households
rlms_household[, new := as.numeric(year == first_year)]

# Number of sampled households, average interview duration and survey time span per secondary sampling unit-year
rlms_household_chars_by_area_id_year <- rlms_household[, list(
		households = .N,
		interview_duration = mean(interview_duration, na.rm = T),
		mean_income = mean(income, na.rm = T),
		median_income = median(income, na.rm = T),
		mean_realincome = mean(realincome, na.rm = T),
		median_realincome = median(realincome, na.rm = T),
		sd_realincome = sd(income, na.rm = T),
		sd_income = sd(realincome, na.rm = T),
		interview_span = round(as.numeric(max(interview_date, na.rm = T) - min(interview_date, na.rm = T))),
		share_new_households = mean(new, na.rm = T)
	), by = c("area_id", "year")
]

# Add SSU characteristics to Benford conformity data
rlms_ssu_conformity <- merge(rlms_household_income_conformity_by_area_id_year[type == "posterior", -"observations"], rlms_household_chars_by_area_id_year, by = c("area_id", "year"), all.x = T, all.y = T)

# Count how many years since 2000 the SSU was observed
# and had over 15 households (Benford computation cutoff)
rlms_ssu_conformity[, years_observed := sum(year >= 2000 & households >= 15), by = "area_id"]

# Logit-transformation of proportions
#lms_conformity[, benford_logit := log(benford/(1 - benford))]
#rlms_conformity[, generalized_benford_logit := log(generalized_benford/(1 - generalized_benford))]

#rlms_ssu_conformity[, benford_logit := log(benford/(1 - benford))]
#rlms_ssu_conformity[, generalized_benford_logit := log(generalized_benford/(1 - generalized_benford))]

rlms_ssu_conformity[, share_new_households_logit := log(share_new_households/(1 - share_new_households))]
rlms_ssu_conformity[share_new_households == 1, share_new_households_logit := log(0.9999/(1-0.9999))]
rlms_ssu_conformity[share_new_households == 0, share_new_households_logit := log(0.0001/(1-0.0001))]

# Important to sort by year within area
setorderv(rlms_conformity, c("year"))
setorderv(rlms_ssu_conformity, c("area_id", "year"))

#################
# Compute overall RLMS income and Benford breakpoint F-stats based on Chow test
rlms_fstats <- rlms_conformity[
	year >= 2000,
	list(fstat_medianincome = Fstats(log(median_income) ~ 1, from = 2, to = 21)[[1]],
		fstat_meanincome = Fstats(log(mean_income) ~ 1, from = 2, to = 21)[[1]],
		#fstat_meanrealincome = Fstats(log(mean_realincome) ~ 1, from = 2, to = 21)[[1]],
		#fstat_medianrealincome = Fstats(log(median_realincome) ~ 1, from = 2, to = 21)[[1]],
		fstat_benford = Fstats(benford ~ 1, from = 2)[[1]],
		fstat_generalized_benford = Fstats(generalized_benford ~ 1, from = 2, to = 21)[[1]]
	)
]
rlms_fstats[, year := 2001:2020]

#################
# Compute per-SSU income breakpoint years based on Chow test
rlms_ssu_breakpoints <- rlms_ssu_conformity[
	year >= 2000 & years_observed >= 20,
	list(breakyear_medianincome = year[breakpoints(Fstats(log(median_income) ~ 1, from = 2, to = 21))$breakpoints],
		breakyear_meanincome = year[breakpoints(Fstats(log(mean_income) ~ 1, from = 2, to = 21))$breakpoints]
	),
	by = "area_id"
]

# Add the identified breakpoint years to the data
rlms_ssu_conformity <- merge(rlms_ssu_conformity, rlms_ssu_breakpoints, by = "area_id", all = T)

# Post-income break dummies
rlms_ssu_conformity[, postbreak_medianincome := as.numeric(year >= breakyear_medianincome) ]
rlms_ssu_conformity[, postbreak_meanincome := as.numeric(year >= breakyear_meanincome) ]
rlms_ssu_conformity[ is.na(breakyear_medianincome), postbreak_medianincome := 0 ]
rlms_ssu_conformity[ is.na(breakyear_meanincome), postbreak_meanincome := 0 ]

#################
# Compute per-SSU Benford breakpoint stats based on Chow test
# conditional in survey characteristics and income breakpoint years

rlms_ssu_benford_breakpoints <- rlms_ssu_conformity[
	year >= 2000 & years_observed >= 20,
	list(breakyear_benford_unconditional = year[breakpoints(Fstats(benford ~ 1, from = 2, to = 21))$breakpoints],
		breakyear_generalized_benford_unconditional = year[breakpoints(Fstats(generalized_benford ~ 1, from = 2, to = 21))$breakpoints],
		breakyear_benford_conditional = year[breakpoints(Fstats(benford ~ 1 + postbreak_medianincome + log(households) + log(interview_duration) + share_new_households_logit + log(median_realincome) + log(sd_realincome) + log(interview_span), from = 2, to = 21))$breakpoints],
		breakyear_generalized_benford_conditional = year[breakpoints(Fstats(generalized_benford ~ 1 + postbreak_medianincome + log(households) + log(interview_duration) + share_new_households_logit + log(median_realincome) + log(sd_realincome) + log(interview_span), from = 2, to = 21))$breakpoints]
	),
	by = "area_id"
]

#################
# Regression of per-SSU Benfordness

fit_benford_nocontrols <- feols(benford ~ postbreak_medianincome + i(year, ref = 2009) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])
fit_benford_controls <- feols(benford ~ postbreak_medianincome + i(year, ref = 2009) + log(households) + log(interview_duration) + share_new_households_logit + log(median_realincome) + log(sd_realincome) + log(interview_span) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])

fit_generalized_benford_nocontrols <- feols(generalized_benford ~ postbreak_medianincome + i(year, ref = 2009) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])
fit_generalized_benford_controls <- feols(generalized_benford ~ postbreak_medianincome + i(year, ref = 2009) + log(households) + log(interview_duration) + share_new_households_logit + log(median_realincome) + log(sd_realincome) + log(interview_span) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])

fit_uniform_nocontrols <- feols(uniform ~ postbreak_medianincome + i(year, ref = 2009) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])
fit_uniform_controls <- feols(uniform ~ postbreak_medianincome + i(year, ref = 2009) + log(households) + log(interview_duration) + share_new_households_logit + log(median_realincome) + log(sd_realincome) + log(interview_span) | area_id, cluster = ~ area_id, data = rlms_ssu_conformity[year >= 2000 & years_observed >= 20])

# Wald test joint sum of quarter
# dummies equals zero
hypothesis <- paste0(paste("`year::", 2010:2022, sep = "", collapse = "` + "), "` = ", 0)

wald_tstat_benford_nocontrols <- as.numeric(summary(glht(model = fit_benford_nocontrols, linfct = hypothesis))$test$tstat)
wald_pvalue_benford_nocontrols <- as.numeric(summary(glht(model = fit_benford_nocontrols, linfct = hypothesis))$test$pvalues)

wald_tstat_benford_controls <- as.numeric(summary(glht(model = fit_benford_controls, linfct = hypothesis))$test$tstat)
wald_pvalue_benford_controls <- as.numeric(summary(glht(model = fit_benford_controls, linfct = hypothesis))$test$pvalues)

wald_tstat_generalized_benford_nocontrols <- as.numeric(summary(glht(model = fit_generalized_benford_nocontrols, linfct = hypothesis))$test$tstat)
wald_pvalue_generalized_benford_nocontrols <- as.numeric(summary(glht(model = fit_generalized_benford_nocontrols, linfct = hypothesis))$test$pvalues)

wald_tstat_generalized_benford_controls <- as.numeric(summary(glht(model = fit_generalized_benford_controls, linfct = hypothesis))$test$tstat)
wald_pvalue_generalized_benford_controls <- as.numeric(summary(glht(model = fit_generalized_benford_controls, linfct = hypothesis))$test$pvalues)

wald_tstat_uniform_nocontrols <- as.numeric(summary(glht(model = fit_uniform_nocontrols, linfct = hypothesis))$test$tstat)
wald_pvalue_uniform_nocontrols <- as.numeric(summary(glht(model = fit_uniform_nocontrols, linfct = hypothesis))$test$pvalues)

wald_tstat_uniform_controls <- as.numeric(summary(glht(model = fit_uniform_controls, linfct = hypothesis))$test$tstat)
wald_pvalue_uniform_controls <- as.numeric(summary(glht(model = fit_uniform_controls, linfct = hypothesis))$test$pvalues)

# Report the results in a table
etable(fit_benford_nocontrols, fit_benford_controls, fit_generalized_benford_nocontrols, fit_generalized_benford_controls, fit_uniform_nocontrols, fit_uniform_controls,
		fitstat = c("my", "n", "r2", "war2"), drop = "Intercept", signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10), file = "tables/regression_rlms_ssu.tex",
		extralines = list("Wald t-stat after 2009" = round(c(wald_tstat_benford_nocontrols, wald_tstat_benford_controls, wald_tstat_generalized_benford_nocontrols, wald_tstat_generalized_benford_controls, wald_tstat_uniform_nocontrols, wald_tstat_uniform_controls), 3),
				"Wald t-stat pvalue after 2009" = c(wald_pvalue_benford_nocontrols, wald_pvalue_benford_controls, wald_pvalue_generalized_benford_nocontrols, wald_pvalue_generalized_benford_controls, wald_pvalue_uniform_nocontrols, wald_pvalue_uniform_controls))
		)

#################
# Gather the results for export

# Log income and F-stats for overall RLMS
rlms_structural_breaks <- merge(rlms_conformity[, c("year", "median_income", "mean_income", "benford", "generalized_benford")], rlms_fstats, by = "year")
rlms_structural_breaks[, median_income := log(median_income)]
rlms_structural_breaks[, mean_income := log(mean_income)]

# Count of SSUs with breaks in income
temp1 <- rlms_ssu_conformity[!is.na(breakyear_medianincome), list(count_ssu_breakyear_medianincome = uniqueN(area_id)), by = "breakyear_medianincome"]
temp2 <- rlms_ssu_conformity[!is.na(breakyear_meanincome), list(count_ssu_breakyear_meanincome = uniqueN(area_id)), by = "breakyear_meanincome"]
setnames(temp1, "breakyear_medianincome", "year")
setnames(temp2, "breakyear_meanincome", "year")

rlms_structural_breaks <- merge(rlms_structural_breaks, temp1, all = T)
rlms_structural_breaks <- merge(rlms_structural_breaks, temp2, all = T)

# Count of SSUs with breaks in Benford
temp3 <- rlms_ssu_benford_breakpoints[!is.na(breakyear_benford_unconditional), list(count_ssu_breakyear_benford_unconditional = uniqueN(area_id)), by = "breakyear_benford_unconditional"]
temp4 <- rlms_ssu_benford_breakpoints[!is.na(breakyear_generalized_benford_unconditional), list(count_ssu_breakyear_generalized_benford_unconditional = uniqueN(area_id)), by = "breakyear_generalized_benford_unconditional"]
temp5 <- rlms_ssu_benford_breakpoints[!is.na(breakyear_benford_conditional), list(count_ssu_breakyear_benford_conditional = uniqueN(area_id)), by = "breakyear_benford_conditional"]
temp6 <- rlms_ssu_benford_breakpoints[!is.na(breakyear_generalized_benford_conditional), list(count_ssu_breakyear_generalized_benford_conditional = uniqueN(area_id)), by = "breakyear_generalized_benford_conditional"]

setnames(temp3, "breakyear_benford_unconditional", "year")
setnames(temp4, "breakyear_generalized_benford_unconditional", "year")
setnames(temp5, "breakyear_benford_conditional", "year")
setnames(temp6, "breakyear_generalized_benford_conditional", "year")

rlms_structural_breaks <- merge(rlms_structural_breaks, temp3, all = T)
rlms_structural_breaks <- merge(rlms_structural_breaks, temp4, all = T)
rlms_structural_breaks <- merge(rlms_structural_breaks, temp5, all = T)
rlms_structural_breaks <- merge(rlms_structural_breaks, temp6, all = T)

# Add total number of SSUs
rlms_structural_breaks <- merge(rlms_structural_breaks, rlms_ssu_conformity[year >= 2001 & year <= 2020 & years_observed >= 20, list( count_ssu = uniqueN(area_id)), by = "year"], by = "year", all = T)
fwrite(rlms_structural_breaks, file = "output/rlms_structural_breaks.csv")

#################
# Other structural characteristsics of RLMS

rlms_per_wave_characteristics <- rlms_household[, list(
	ssus = uniqueN(area_id),
	households = uniqueN(id),
	new_households = sum(new),
	iqr_interview_span = quantile(as.numeric(interview_date), .75, na.rm = T) - quantile(as.numeric(interview_date), .25, na.rm = T),
	median_interview_duration = median(interview_duration, na.rm = T),
	sd_realincome = round(sd(realincome, na.rm = T), 0)

	), by = "year"][order(year)]

# Counts of SSUs with over 15 household that are participating
# in Benford computations
temp7 <- rlms_ssu_conformity[!is.na(benford), list( ssus_benford = uniqueN(area_id), households_benford = sum(households)), by = "year" ]
rlms_per_wave_characteristics <- merge(rlms_per_wave_characteristics, temp7, by = "year", all = T)

fwrite(rlms_per_wave_characteristics, file = "output/rlms_per_wave_characteristics.csv")
