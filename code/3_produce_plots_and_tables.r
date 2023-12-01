library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(showtext)

# Add the font to use
#font_add("FreeSetLight", regular = "FRS45.otf", symbol = "cmunrm.ttf")
#showtext_auto()

# No scientific notation
options(scipen=999)

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

# Load income conformity per survey (Bayesian approach)
# computed in 2a_test_conformity_all_surveys.r
load("output/income_surveys_income_conformity.rdata")

# Load income conformity per survey (frequentist approach)
# computed in 2a_test_conformity_all_surveys.r
load("output/income_surveys_income_conformity_frequentist.rdata")

# Load per area-id conformity of RLMS computed by 2b_within_rlms_conformity.r
load("output/rlms_household_income_conformity_by_area_id_year.rdata")

# Load conformity of RLMS/VODPF individuals by novelty computed by 2b_within_rlms_conformity.r
load("output/rlms_household_income_conformity_by_novelty.rdata")
load("output/vodpf_household_income_conformity_by_novelty.rdata")

# Load results of power simulations computed by 2c_power_simulations.r
load("output/benford_power_simulations.rdata")

#############################
# Figure showcasing various distributions under their parameters

# Optimal parameters of distributions at 0th, 25th, 50th, 75th, and 100th percentiles
# in the data
epsilons <- unname(quantile(income_surveys_income_conformity[type == "optim"]$contaminated_benford))
# Manually replace second item to avoid ties due to duplicates
epsilons[2] <- 0.00001

alphas <- unname(quantile(income_surveys_income_conformity[type == "optim"]$generalized_benford))
betas <- unname(quantile(income_surveys_income_conformity[type == "optim"]$rodriguez)) # betas <- -1
gammas <- unname(quantile(income_surveys_income_conformity[type == "optim"]$hurlimann))

# To one object
digit_probs <- data.table(distribution = "benford", param = 1, paramgroup = 1, digit = 1:9, probability = prob.genbenford(alpha = 1))
digit_probs <- rbind(digit_probs, data.table(distribution = "stigler", param = 0, paramgroup = 1, digit = 1:9, probability = prob.rodriguez(0)))
digit_probs <- rbind(digit_probs, rbindlist(lapply(epsilons, function(x) data.table(distribution = "contaminated_benford", param = x, paramgroup = which(epsilons == x), digit = 1:9, probability = prob.contambenford(x)))))
digit_probs <- rbind(digit_probs, rbindlist(lapply(alphas, function(x) data.table(distribution = "generalized_benford", param = x, paramgroup = which(alphas == x), digit = 1:9, probability = prob.genbenford(x)))))
digit_probs <- rbind(digit_probs, rbindlist(lapply(betas, function(x) data.table(distribution = "rodriguez", param = x, paramgroup = which(betas == x), digit = 1:9, probability = prob.rodriguez(x)))))
digit_probs <- rbind(digit_probs, rbindlist(lapply(gammas, function(x) data.table(distribution = "hurlimann", param = x, paramgroup = which(gammas == x), digit = 1:9, probability = prob.hurlimann(x)))))

# Parameter value to text
digit_probs[, paramgroup := factor(paramgroup, labels = c("мин", "25-й", "50-й", "75-й", "макс"))]
#digit_probs[ paramgroup == 1, paramgroup := "мин"]
#digit_probs[ paramgroup == 2, paramgroup := "25-й"]
#digit_probs[ paramgroup == 3, paramgroup := "50-й"]
#digit_probs[ paramgroup == 4, paramgroup := "75-й"]
#digit_probs[ paramgroup == 5, paramgroup := "макс"]

# Proper order of distributions
digit_probs[ distribution == "benford", distribution := "А. Закон Бенфорда"]
digit_probs[ distribution == "contaminated_benford", distribution := "Б. «Загрязненный» закон Бенфорда"]
digit_probs[ distribution == "stigler", distribution := "В. Закон Стиглера"]
digit_probs[ distribution == "generalized_benford", distribution := "Г. Обобщенный закон Бенфорда"]
digit_probs[ distribution == "rodriguez", distribution := "Д. Закон Родригеза"]
digit_probs[ distribution == "hurlimann", distribution := "Е. Закон Хюрлимана"]

# Create a plot
digit_probs_plot_func <- by(data = digit_probs, INDICES = digit_probs$distribution, FUN = function(m) {
	
		m <- droplevels(m)	
		
		print(levels(m$paramgroup))
		
		# Update parameter group labels by distribution (used for legend)
		if( unique(m$distribution) == "Б. «Загрязненный» закон Бенфорда" ) {
			
			levels(m$paramgroup) <- paste0("\u03B5 = ", unique(m$param), " (", unique(m$paramgroup), ")")
			
		} else if ( unique(m$distribution) == "Г. Обобщенный закон Бенфорда" ) {
			
			levels(m$paramgroup) <- paste0("\u03B1 = ", unique(m$param), " (", unique(m$paramgroup), ")")

		} else if ( unique(m$distribution) == "Д. Закон Родригеза" ) {
			
			levels(m$paramgroup) <- paste0("\u03B2 = ", unique(m$param), " (", unique(m$paramgroup), ")")

		} else if ( unique(m$distribution) == "Е. Закон Хюрлимана" ) {
			
			levels(m$paramgroup) <- paste0("\u03B3 = ", unique(m$param), " (", unique(m$paramgroup), ")")

		}		
		
		m_plot <- ggplot(aes(x = digit, y = probability, label = paramgroup, fill = paramgroup, color = paramgroup), data = m) +
		geom_point() +
  		geom_line() +
  		scale_x_continuous(name = NULL, breaks = 1:9) +
  		#scale_color_discrete(name = "", label = "", fill = "", color = "") +
  		ggtitle(unique(m$distribution)) +
  		ylim(0, 0.45) +
  		labs(y = "") +
  		theme_minimal() +
  		theme(text = element_text(size = 8), plot.title = element_text(size = 8), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  		
  		# Add legend for certain distributions
  		if( !(unique(m$distribution) %in% c("А. Закон Бенфорда", "В. Закон Стиглера") ) ) {
  			  			
  			m_plot <- m_plot + theme(legend.position = c(0.7, 0.7), legend.box = "horizontal", legend.title = element_blank())
  			
  		} else {
  			
  			  m_plot <- m_plot + theme(legend.position = "none")

  			
  		}
		#  theme(axis.text.x = element_blank())
		
		return(m_plot)

})

# Create a plot
digit_probs_plot <- do.call(grid.arrange, c(digit_probs_plot_func, ncol = 3))

ggsave(digit_probs_plot, file = "figures/digit_probs_plot.pdf", width = 710, height = 430, scale = 3, device = cairo_pdf, units = "px")


#############################
# First digit distribution table (recent waves)

# Load a table produced by 1_summary_stat_income_surveys.r
income_surveys_digit_distrib <- fread("output/income_surveys_digit_distrib.csv")

# Only most recent surveys and waves
recent_income_surveys_digit_distrib <- income_surveys_digit_distrib[ (survey %in% c("VNDN", "VODPF", "RLMS", "SIPP") & year == 2022) | (survey == "RCVS" & year == 2021) | (survey == "FINMON" & year == 2018) | (survey == "EUSILC" & year == 2013)]
recent_income_surveys_digit_distrib[ level == "household", level := "Дом-ва"]
recent_income_surveys_digit_distrib[ level == "individual", level := "Инд."]

recent_income_surveys_digit_distrib[ survey == "VNDN", survey := "ВНДН"]
recent_income_surveys_digit_distrib[ survey == "VODPF", survey := "ВОДПФ"]
recent_income_surveys_digit_distrib[ survey == "RLMS", survey := "РМЭЗ"]
recent_income_surveys_digit_distrib[ survey == "FINMON", survey := "ФИНМОН"]

# Custom order
recent_income_surveys_digit_distrib <- recent_income_surveys_digit_distrib[c(3, 4, 1, 2, 7, 8, 10, 9, 5, 6, 11, 12)]
fwrite(recent_income_surveys_digit_distrib, file = "tables/recent_income_surveys_digit_distrib.csv", scipen = 999)

#############################
# Watson statistic, p-value and posterior digit distribution table (recent waves)

recent_income_surveys_conformity_bayesian <- income_surveys_income_conformity[((survey %in% c("VNDN", "VODPF", "RLMS", "SIPP") & year == 2022) | (survey == "RCVS" & year == 2021) | (survey == "FINMON" & year == 2018) | (survey == "EUSILC" & year == 2013) )& type == "posterior"]

recent_income_surveys_conformity_frequentist <- income_surveys_income_conformity_frequentist[((survey %in% c("VNDN", "VODPF", "RLMS", "SIPP") & year == 2022) | (survey == "RCVS" & year == 2021) | (survey == "FINMON" & year == 2018) | (survey == "EUSILC" & year == 2013) ) & type %in% c("pvalue"), c("survey", "year", "level", "type", names(income_surveys_income_conformity_frequentist)[grepl("usq", names(income_surveys_income_conformity_frequentist))]), with = F]
names(recent_income_surveys_conformity_frequentist) <- gsub("_usq", "", names(recent_income_surveys_conformity_frequentist))

recent_income_surveys_conformity <- rbind(recent_income_surveys_conformity_bayesian, recent_income_surveys_conformity_frequentist, fill = T)

fwrite(recent_income_surveys_conformity, file = "tables/recent_income_surveys_conformity.csv", scipen = 999)

#############################
# Tables with per-survey conformity to various distributions

# Bayesian
household_survey_conformity_bayesian <- income_surveys_income_conformity[type == "posterior" & level == "household",
			list(benford = paste0(sum(benford > 0.9), "/", .N),
				stigler = paste0(sum(stigler > 0.9), "/", .N),
				uniform = paste0(sum(uniform > 0.9), "/", .N),
				contaminated_benford = paste0(sum(contaminated_benford > 0.9), "/", .N),
				generalized_benford = paste0(sum(generalized_benford > 0.9), "/", .N),
				rodriguez = paste0(sum(rodriguez > 0.9), "/", .N),
				hurlimann = paste0(sum(hurlimann > 0.9), "/", .N)
			)
, by = "survey" ]

household_survey_conformity_bayesian[, approach := "байез."]


# Frequentist
## Only p-value from U-stat
household_survey_conformity_frequentist <- income_surveys_income_conformity_frequentist[type == "pvalue" & level == "household", c("survey", "year", names(income_surveys_income_conformity_frequentist)[grepl("usq", names(income_surveys_income_conformity_frequentist))]), with = F ]
names(household_survey_conformity_frequentist) <- gsub("_usq", "", names(household_survey_conformity_frequentist))

household_survey_conformity_frequentist <- household_survey_conformity_frequentist[,
			list(benford = paste0(sum(benford > 0.1), "/", .N),
				stigler = paste0(sum(stigler > 0.1), "/", .N),
				uniform = paste0(sum(uniform > 0.1), "/", .N),
				contaminated_benford = paste0(sum(contaminated_benford > 0.1), "/", .N),
				generalized_benford = paste0(sum(generalized_benford > 0.1), "/", .N),
				rodriguez = paste0(sum(rodriguez > 0.1), "/", .N),
				hurlimann = paste0(sum(hurlimann > 0.1), "/", .N)
			)
, by = "survey" ]

household_survey_conformity_frequentist[, approach := "фрекв."]

# Add to one object
household_survey_conformity <- rbind(household_survey_conformity_bayesian, household_survey_conformity_frequentist)
setorderv(household_survey_conformity, c("survey", "approach"), c(1, -1))
setcolorder(household_survey_conformity, c("survey", "approach"))

# Proper order
household_survey_conformity <- household_survey_conformity[c(11, 12, 13, 14, 7, 8, 3, 4, 5, 6, 9, 10, 1, 2)]

# Proper survey names
household_survey_conformity[ survey == "VNDN", survey := "ВНДН"]
household_survey_conformity[ survey == "VODPF", survey := "ВОДПФ"]
household_survey_conformity[ survey == "RLMS", survey := "РМЭЗ"]
household_survey_conformity[ survey == "FINMON", survey := "ФИНМОН"]

fwrite(household_survey_conformity, file = "tables/household_survey_conformity.csv")

#############################
# Figure with Bayesian conformity of various waves of household-level RLMS

rlms_conformity <- income_surveys_income_conformity[type == "posterior" & level == "household" & survey == "RLMS", -c("type", "level", "survey", "uniform", "benford", "contaminated_benford")]

rlms_conformity <- melt(rlms_conformity, id.vars = "year", variable.name = "distribution", value.name = "posterior")

# Rename distributions
rlms_conformity[ distribution == "stigler", distribution := "Закон Стиглера"]
rlms_conformity[ distribution == "generalized_benford", distribution := "Обобщ. закон Бенфорда"]
rlms_conformity[ distribution == "rodriguez", distribution := "Закон Родригеза"]
rlms_conformity[ distribution == "hurlimann", distribution := "Закон Хюрлимана"]

rlms_conformity_plot <- ggplot(aes(x = year, y = posterior, group = distribution, fill = distribution, colour = distribution, shape = distribution), data = rlms_conformity[posterior > 0.6]) +
	geom_vline(aes(xintercept=2010), color = "grey") +
	geom_text(aes(x = 2010.1, y = 0.85, label = "ВШЭ становится\nадминистрирующей\nорганизацией"), color = "black", size = 4, hjust = 0) + 
	geom_line(alpha = 0.5, linewidth = 2) + 
	geom_point(alpha = 0.5) + 
	scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
	labs(x = "Год волны опроса РМЭЗ", y = "Апостер. вероятн. соответств. закону") + 
	theme_minimal() +
	theme(text = element_text(size = 12), legend.position = "bottom", , legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave(rlms_conformity_plot, file = "figures/rlms_conformity_plot.pdf", width = 710, height = 430, scale = 3, device = cairo_pdf, units = "px")

#############################
# Figure with RLMS/VODPF observations confirming to Benford's law
# by novelty

rlms_conformity_novelty <- rlms_household_income_conformity_by_novelty[type == "posterior", -c("type", "uniform", "benford", "stigler", "contaminated_benford", "rodriguez", "hurlimann")]
rlms_conformity_novelty <- melt(rlms_conformity_novelty, id.vars = c("year", "new"), variable.name = "distribution", value.name = "posterior")
rlms_conformity_novelty[, survey := "А. РМЭЗ"]

vodpf_conformity_novelty <- vodpf_household_income_conformity_by_novelty[type == "posterior", -c("type", "uniform", "benford", "stigler", "contaminated_benford", "rodriguez", "hurlimann")]
vodpf_conformity_novelty <- melt(vodpf_conformity_novelty, id.vars = c("year", "new"), variable.name = "distribution", value.name = "posterior")
vodpf_conformity_novelty[, survey := "Б. ВОДПФ"]

conformity_novelty <- rbind(rlms_conformity_novelty, vodpf_conformity_novelty, fill = T)
conformity_novelty[, new := as.factor(new)]
levels(conformity_novelty$new) <- c("повторно опрошенные домохозяйства", "впервые опрошенные домохозяйства")

conformity_novelty_plot <- ggplot(aes(x = year, y = posterior, group = new, fill = new, colour = new, shape = new), data = conformity_novelty[ distribution == "generalized_benford"]) +
	geom_vline(data = conformity_novelty[survey == "А. РМЭЗ"], aes(xintercept=2010), color = "grey") +
	geom_text(data = conformity_novelty[survey == "А. РМЭЗ"], aes(x = 2010.1, y = 0.3, label = "ВШЭ становится\nадминистрирующей\nорганизацией"), color = "black", size = 2, hjust = 0) + 
	geom_point() + 
	scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
	labs(x = "Год волны опроса", y = "Апостер. вероятн. соответств.\nОбобщенному закону Бенфорда") + 
	theme_minimal() +
	facet_wrap(.~survey, scales = "free_x", nrow = 2) +
	theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave(conformity_novelty_plot, file = "figures/conformity_novelty_plot.pdf", width = 710, height = 430, scale = 3, device = cairo_pdf, units = "px")

#############################
# Literature review table

# Load list of Elibrary Economic papers mentioning each of the three major
# surveys as of August 21, 2023
elibrary_papers <- fread("output/elibrary_papers_mentioning_income_surveys.csv", na.strings = "")

# Keep only top-6 journals
top_journals <- c("Экономический журнал Высшей школы экономики", "Вопросы экономики", "Журнал Новой экономической ассоциации", "Экономика и математические методы", "Прикладная эконометрика", "Деньги и кредит")

# Top papers since 2012
top_papers <- elibrary_papers[ journal %in% top_journals & year >= 2012 ]
uniqueN(top_papers$elibrary_id) # 193
sum(unique(top_papers, by = "elibrary_id")$cited) # 193

# Yearly counts of mentions per survey-journal
yearly_survey_mentions_top_journals <- top_papers[, .N, by = c("journal", "survey", "year")]

yearly_survey_mentions_top_journals_plot <- ggplot(aes(x = year, y = N, fill = survey, colour = survey, shape = survey), data = yearly_survey_mentions_top_journals) +
	geom_line() +
	geom_point() +
	labs(x = "", y = "Статей") +
	scale_x_continuous(breaks = scales::pretty_breaks(n = 10), guide = guide_axis(n.dodge = 2)) +
	theme_minimal() +
	facet_wrap(.~journal, scales = "fixed", nrow = 3) +
	theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave(yearly_survey_mentions_top_journals_plot, file = "figures/yearly_survey_mentions_top_journals_plot.pdf", width = 710, height = 860, scale = 3, device = cairo_pdf, units = "px")

#############################
# Figure with results of power simulations

benford_power_simulations[ alternative == "Contaminated Benford", alternative := "А. «Загрязненный» закон Бенфорда"]
benford_power_simulations[ alternative == "Generalized Benford", alternative := "Б. Обобщенный закон Бенфорда"]
benford_power_simulations[ alternative == "Rodriguez", alternative := "В. Закон Родригеза"]
benford_power_simulations[ alternative == "Hurlimann", alternative := "Г. Закон Хюрлимана"]

benford_power_simulations[ test == "chisqpval", test := "Пирсон"]
benford_power_simulations[ test == "asqpval", test := "Андерсон-Дарлинг"]
benford_power_simulations[ test == "wsqpval", test := "Крамер-Мизес-Смирнов"]
benford_power_simulations[ test == "usqpval", test := "Ватсон"]

benford_power_simulations_plot <- ggplot(aes(x = param, y = power, fill = test, colour = test, shape = test), data = benford_power_simulations) +
	geom_line() +
	geom_point() +
	labs(x = "параметр закона", y = "Статистическая мощность") +
	scale_x_continuous(breaks = scales::pretty_breaks(n = 10), guide = guide_axis(n.dodge = 2)) +
	theme_minimal() +
	facet_wrap(.~alternative, scales = "free_x", nrow = 3) +
	theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave(benford_power_simulations_plot, file = "figures/benford_power_simulations_plot.pdf", width = 710, height = 860, scale = 3, device = cairo_pdf, units = "px")

#############################
# Summary statistics table (recent waves)

# Load a table produced by 1_summary_stat_income_surveys.r
income_surveys_summary_stat <- fread("output/income_surveys_summary_stat.csv")

# Only most recent surveys and waves
recent_income_surveys_summary_stat <- income_surveys_summary_stat[ (survey %in% c("VNDN", "VODPF", "RLMS", "SIPP") & year == 2022) | (survey == "RCVS" & year == 2021) | (survey == "FINMON" & year == 2018) | (survey == "EUSILC" & year == 2013)]
recent_income_surveys_summary_stat[ level == "household", level := "Дом-ва"]
recent_income_surveys_summary_stat[ level == "individual", level := "Инд."]

recent_income_surveys_summary_stat[ survey == "VNDN", survey := "ВНДН"]
recent_income_surveys_summary_stat[ survey == "VODPF", survey := "ВОДПФ"]
recent_income_surveys_summary_stat[ survey == "RLMS", survey := "РМЭЗ"]
recent_income_surveys_summary_stat[ survey == "FINMON", survey := "ФИНМОН"]

# Custom order
recent_income_surveys_summary_stat <- recent_income_surveys_summary_stat[c(3, 4, 1, 2, 7, 8, 10, 9, 5, 6, 11, 12)]
fwrite(recent_income_surveys_summary_stat, file = "tables/recent_income_surveys_summary_stat.csv")

#############################
# Table with examples of conformers and non-conformers in recent RLMS

rlms_household_conformity_2022 <- rlms_household_income_conformity_by_area_id_year[ type == "posterior" & year == 2022 ]
setorderv(rlms_household_conformity_2022, "generalized_benford", 1)

# Top-5 worst/best conformers
area_id_worst <- head(rlms_household_conformity_2022$area_id, 5)
area_id_best <- rev(tail(rlms_household_conformity_2022$area_id, 5))

# Load a table produced by 1_summary_stat_income_surveys.r
rlms_household_digit_distrib_per_area_id_year <- fread("output/rlms_household_digit_distrib_per_area_id_year.csv")
rlms_household_digit_distrib_per_area_id_year[, area_id := as.character(area_id)]

# Top/worst conformers to one table
rlms_household_conformers_per_area_id_year_worst <- rlms_household_digit_distrib_per_area_id_year[ match(area_id_worst, area_id)]
rlms_household_conformers_per_area_id_year_worst[, type := "worst"]
rlms_household_conformers_per_area_id_year_worst[, rank := .I]
rlms_household_conformers_per_area_id_year_best <- rlms_household_digit_distrib_per_area_id_year[ match(area_id_best, area_id)]
rlms_household_conformers_per_area_id_year_best[, type := "best"]
rlms_household_conformers_per_area_id_year_best[, rank := .I]

rlms_household_conformers_per_area_id_year <- rbind(rlms_household_conformers_per_area_id_year_worst, rlms_household_conformers_per_area_id_year_best, fill = T)

# Add observations, posterior, and optimal alpha
rlms_household_conformers_per_area_id_year <- merge(rlms_household_conformers_per_area_id_year, unique(rlms_household_conformity_2022[, c("area_id", "observations")], by = "area_id"), by = "area_id", sort = F)

rlms_household_conformers_per_area_id_year <- merge(rlms_household_conformers_per_area_id_year, unique(rlms_household_income_conformity_by_area_id_year[ type == "optim" & year == 2022 ][, c("area_id", "generalized_benford")], by = "area_id"), by = "area_id", sort = F)
setnames(rlms_household_conformers_per_area_id_year, "generalized_benford", "alphastar")
rlms_household_conformers_per_area_id_year[, alphastar := round(alphastar, 3)]

rlms_household_conformers_per_area_id_year <- merge(rlms_household_conformers_per_area_id_year, unique(rlms_household_income_conformity_by_area_id_year[ type == "posterior" & year == 2022 ][, c("area_id", "generalized_benford")], by = "area_id"), by = "area_id", sort = F)
setnames(rlms_household_conformers_per_area_id_year, "generalized_benford", "posterior")
rlms_household_conformers_per_area_id_year[, posterior := round(posterior, 3)]

# Export point
fwrite(rlms_household_conformers_per_area_id_year, "tables/rlms_household_conformers_per_area_id_year.csv")

# In-text statement for Земетчинский район Пензенской области
mean(rlms_household_income_conformity_by_area_id_year[area_id == 117 & type == "posterior" & year < 2015]$generalized_benford)

# In-text statement for Липецк
mean(rlms_household_income_conformity_by_area_id_year[area_id == 72 & type == "posterior" & year < 2010]$generalized_benford)


#############################
# Figure with RLMS share of SSU observations conforming with Benford's law

rlms_household_income_conformity_by_area_id_year <- rlms_household_income_conformity_by_area_id_year[ type == "posterior" ]

rlms_households_conforming_benford_count <- rlms_household_income_conformity_by_area_id_year[generalized_benford > 0.9, list(count_benford = sum(observations)), by = "year"]
rlms_households_count <- rlms_household_income_conformity_by_area_id_year[, list(count_households = sum(observations)), by = "year"]

rlms_households_conforming_benford <- merge(rlms_households_count, rlms_households_conforming_benford_count, by = "year", all = T)
rlms_households_conforming_benford[, share_conforming := count_benford/count_households ]

rlms_ssu_share_plot <- ggplot(aes(x = year, y = share_conforming), data = rlms_households_conforming_benford) +
	geom_line() + 
	scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
	labs(x = "Год волны опроса РМЭЗ", y = "% наблюдений, приходящихся на вторичные\nединицы отбора, соотв. Обобщ. закону Бенфорда") + 
	scale_y_continuous(labels = scales::percent_format(accuracy = 2L), breaks = scales::pretty_breaks(n = 5)) +
	theme_minimal() +
	theme(text = element_text(size = 12), legend.position = "bottom", legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank())

ggsave(rlms_ssu_share_plot, file = "figures/rlms_ssu_share_plot.pdf", width = 710, height = 430, scale = 3, device = cairo_pdf, units = "px")
