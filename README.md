# Data and Code For `Russian Household Income Surveys: Ensuring Data Quality`

This is the code and data to replicate the analysis in Skougarevskiy (2023)'s "Russian Household Income Surveys: Ensuring Data Quality" paper.

## Installation

To replicate the analysis you need to clone this repository to your local machine. Then you need to install the required versions of R dependencies listed in `DEPENDENCIES`. `code/helper_functions/install_dependencies.r` automates this step, but you may still need to install the underlying libraries manually with [Homebrew](https://brew.sh) or `apt-get`, depending on your platform. Finally, you need to declare the environment variable `BENFORD_RUSSIAN_INCOME_SURVEYS_PATH` in bash pointing to the repository. Or, better yet, you can add it in your `.Renviron` with
```console
user:~$ echo 'BENFORD_RUSSIAN_INCOME_SURVEYS_PATH="path_to_cloned_repository"' >> ~/.Renviron
```

## Repository structure

```
/
├── code/
|    ├── 1_summary_stat_income_surveys.r                # Summary statistics and first-digit distributions
|    ├── 2a_test_conformity_all_surveys.r               # Test all surveys' conformity to Benford's law
|    ├── 2b_within_rlms_conformity.r                    # Test RLMS conformity within SSU/new/old respondents
|    ├── 2c_power_simulations.r                         # Conduct power simulations of Benford tests
|    ├── 3_produce_plots_and_tables.r                   # Produce all Figures and Tables in the paper
|    ├── helper_functions/
|    |   ├── install_dependencies.r                     # Install dependencies
|    |   └── helper_functions.r                         # Functions performing all statistical tests
|    └──prepare_surveys/                                # Code to prepare survey data
|       ├── 1a_convert_rlms.r                           # HSE's Russian longitudinal monitoring survey
|       ├── 1b_convert_cb_finances_survey.r             # Central Bank's Consumer finances survey
|       ├── 1c_convert_rosstat_surveys_to_rdata.r       # Rosstat's VNDN survey
|       ├── 1d_convert_finmon_to_rdata.r                # HSE's FINMON survey
|       ├── 1e_convert_rcsv_to_rdata.r                  # EUSPb's Russian Crime Victimization survey
|       ├── 1f_convert_sipp_survey.r                    # BLS's Survey of Income and Program Participation
|       ├── 1j_convert_eusilc_surveys.r                 # European Union Statistics on Income and Living Conditions surveys
|       ├── 2_combine_surveys.r                         # Combines all surveys into survey_data/income_surveys.rdata object
|       └── 3_identify_new_observations_per_wave.r      # Indentify the year when household/individual appeared in RLMS/CB survey
├── survey_data/                                          
|    ├── income_surveys.rdata                           # Data with income and indentifiers from all surveys
|    └── income_surveys_first_year.rdata                # Years when household/individual appeared in RLMS/CB survey
├── output/
|    ├── elibrary_papers_mentioning_income_surveys.csv  # Papers citing surveys from eLIBRARY.ru (as of August 23, 2023)
|    ├── income_surveys_digit_distrib.csv               # First digit distribution in all surveys
|    ├── income_surveys_income_conformity_frequentist.rdata # Frequentist Benford test statistics and p-values per survey
|    ├── income_surveys_income_conformity.rdata         # Bayes factors and posterior probability of Benford conformity per survey
|    ├── income_surveys_summary_stat.csv                # Summary statistics per survey
|    ├── rlms_household_digit_distrib_per_area_id_year.csv  # First digit distribution per RLMS secondary sampling unit
|    ├── rlms_household_income_conformity_by_area_id_year.rdata  # Benford conformity per RLMS secondary sampling unit
|    ├── rlms_household_income_conformity_by_novelty.rdata  # Benford conformity by RLMS household first/non-first year in survey
|    └── vodpf_household_income_conformity_by_novelty.rdata  # Benford conformity by CB survey respondent novelty
├── tables/                                             
|    ├── recent_income_surveys_digit_distrib.csv        # Table 1
|    ├── recent_income_surveys_conformity.csv           # Table 2
|    ├── household_survey_conformity.csv                # Table 3, household-level
|    ├── individual_survey_conformity.csv               # Table 3, individual-level
|    ├── recent_income_surveys_summary_stat.csv         # Table A.1
|    └── rlms_household_conformers_per_area_id_year.csv # Table A.2
└── figures/                                             
     ├── digit_probs_plot.pdf                           # Figure 1
     ├── rlms_conformity_plot.pdf                       # Figure 2
     ├── conformity_novelty_plot.pdf                    # Figure 3
     ├── yearly_survey_mentions_top_journals_plot.pdf   # Figure A.1
     ├── benford_power_simulations_plot.pdf             # Figure A.2
     └── rlms_ssu_share_plot.pdf                        # Figure A.2
```

## Data availability statement

The data that support the findings of this study were derived from the following resources available in the public domain:
- [RLMS](https://www.hse.ru/en/rlms/downloads) conducted by National Research University "Higher School of Economics" and OOO “Demoscope” together with Carolina Population Center, University of North Carolina at Chapel Hill and the Institute of Sociology of the Federal Center of Theoretical and Applied Sociology of the Russian Academy of Sciences. (RLMS-HSE web sites: https://rlms-hse.cpc.unc.edu, https://www.hse.ru/org/hse/rlms) 
- [VNDN](https://rosstat.gov.ru/free_doc/new_site/vndn-2022/index.html) conducted by Rosstat and freely available
- [VODPF](https://www.cbr.ru/ec_research/vserossiyskoe-obsledovanie-domokhozyaystv-po-potrebitel-skim-finansam/) published by the Central Bank of Russia
- [FINMON](http://sophist.hse.ru/db/oprosy.shtml?ts=188&en=0) published by the Higher School of Economics
- [RCVS](https://doi.org/10.7910/DVN/SGRQTI) published by the European University at St. Petersburg
- [SIPP](https://www.census.gov/programs-surveys/sipp/data/datasets/2022-data/2022.html) published by the U.S. Bureau of Labor Statistics 
- [EUSILC](https://ec.europa.eu/eurostat/web/microdata/public-microdata/statistics-on-income-and-living-conditions) public use data by the European Commission

## Licence
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />
Creative Commons License Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

Copyright © the respective contributors, as shown by the `AUTHORS` file.

## Contacts
Dmitriy Skougarevskiy, Ph.D.
[dskougarevskiy@eu.spb.ru](mailto:dskougarevskiy@eu.spb.ru)