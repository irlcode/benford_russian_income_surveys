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

# Load survey data created by code/prepare_surveys/2_combine_surveys.r
load("income_surveys.rdata")

############################
# RLMS, 1994-2022 combined

##########
# Household-level
load("rlms/rlms_household.rdata")

# First year when household appeared is the wave with its first non-
# -missing identifier
rlms_household[, first_year := NA_real_]
rlms_household[ !is.na(aid_h) & is.na(first_year), first_year := 1994 ]
rlms_household[ !is.na(bid_h) & is.na(first_year), first_year := 1995 ]
rlms_household[ !is.na(cid_h) & is.na(first_year), first_year := 1996 ]
rlms_household[ !is.na(did_h) & is.na(first_year), first_year := 1998 ]
rlms_household[ !is.na(eid_h) & is.na(first_year), first_year := 2000 ]
rlms_household[ !is.na(fid_h) & is.na(first_year), first_year := 2001 ]
rlms_household[ !is.na(gid_h) & is.na(first_year), first_year := 2002 ]
rlms_household[ !is.na(hid_h) & is.na(first_year), first_year := 2003 ]
rlms_household[ !is.na(iid_h) & is.na(first_year), first_year := 2004 ]
rlms_household[ !is.na(jid_h) & is.na(first_year), first_year := 2005 ]
rlms_household[ !is.na(kid_h) & is.na(first_year), first_year := 2006 ]
rlms_household[ !is.na(lid_h) & is.na(first_year), first_year := 2007 ]
rlms_household[ !is.na(mid_h) & is.na(first_year), first_year := 2008 ]
rlms_household[ !is.na(nid_h) & is.na(first_year), first_year := 2009 ]
rlms_household[ !is.na(oid_h) & is.na(first_year), first_year := 2010 ]
rlms_household[ !is.na(pid_h) & is.na(first_year), first_year := 2011 ]
rlms_household[ !is.na(qid_h) & is.na(first_year), first_year := 2012 ]
rlms_household[ !is.na(rid_h) & is.na(first_year), first_year := 2013 ]
rlms_household[ !is.na(sid_h) & is.na(first_year), first_year := 2014 ]
rlms_household[ !is.na(tid_h) & is.na(first_year), first_year := 2015 ]
rlms_household[ !is.na(uid_h) & is.na(first_year), first_year := 2016 ]
rlms_household[ !is.na(vid_h) & is.na(first_year), first_year := 2017 ]
rlms_household[ !is.na(wid_h) & is.na(first_year), first_year := 2018 ]
rlms_household[ !is.na(xid_h) & is.na(first_year), first_year := 2019 ]
rlms_household[ !is.na(yid_h) & is.na(first_year), first_year := 2020 ]
rlms_household[ !is.na(zid_h) & is.na(first_year), first_year := 2021 ]
rlms_household[ !is.na(aaid_h) & is.na(first_year), first_year := 2022 ]

# NB: id_h is not unique across waves, so we need to carry year variable as well
rlms_household_first_year <- rlms_household[, c("id_h", "id_w", "first_year"), with = F]
setnames(rlms_household_first_year, c("id_h", "id_w"), c("id", "year"))
rlms_household_first_year[, survey := "RLMS"]
rlms_household_first_year[, level := "household"]

##########
# Individual-level

load("rlms/rlms_individual.rdata")

# First year when individual appeared is the wave with its first non-
# -missing identifier
rlms_individual[, first_year := NA_real_]
rlms_individual[ !is.na(aid_h) & is.na(first_year), first_year := 1994 ]
rlms_individual[ !is.na(bid_h) & is.na(first_year), first_year := 1995 ]
rlms_individual[ !is.na(cid_h) & is.na(first_year), first_year := 1996 ]
rlms_individual[ !is.na(did_h) & is.na(first_year), first_year := 1998 ]
rlms_individual[ !is.na(eid_h) & is.na(first_year), first_year := 2000 ]
rlms_individual[ !is.na(fid_h) & is.na(first_year), first_year := 2001 ]
rlms_individual[ !is.na(gid_h) & is.na(first_year), first_year := 2002 ]
rlms_individual[ !is.na(hid_h) & is.na(first_year), first_year := 2003 ]
rlms_individual[ !is.na(iid_h) & is.na(first_year), first_year := 2004 ]
rlms_individual[ !is.na(jid_h) & is.na(first_year), first_year := 2005 ]
rlms_individual[ !is.na(kid_h) & is.na(first_year), first_year := 2006 ]
rlms_individual[ !is.na(lid_h) & is.na(first_year), first_year := 2007 ]
rlms_individual[ !is.na(mid_h) & is.na(first_year), first_year := 2008 ]
rlms_individual[ !is.na(nid_h) & is.na(first_year), first_year := 2009 ]
rlms_individual[ !is.na(oid_h) & is.na(first_year), first_year := 2010 ]
rlms_individual[ !is.na(pid_h) & is.na(first_year), first_year := 2011 ]
rlms_individual[ !is.na(qid_h) & is.na(first_year), first_year := 2012 ]
rlms_individual[ !is.na(rid_h) & is.na(first_year), first_year := 2013 ]
rlms_individual[ !is.na(sid_h) & is.na(first_year), first_year := 2014 ]
rlms_individual[ !is.na(tid_h) & is.na(first_year), first_year := 2015 ]
rlms_individual[ !is.na(uid_h) & is.na(first_year), first_year := 2016 ]
rlms_individual[ !is.na(vid_h) & is.na(first_year), first_year := 2017 ]
rlms_individual[ !is.na(wid_h) & is.na(first_year), first_year := 2018 ]
rlms_individual[ !is.na(xid_h) & is.na(first_year), first_year := 2019 ]
rlms_individual[ !is.na(yid_h) & is.na(first_year), first_year := 2020 ]
rlms_individual[ !is.na(zid_h) & is.na(first_year), first_year := 2021 ]
rlms_individual[ !is.na(aaid_h) & is.na(first_year), first_year := 2022 ]
 
rlms_individual_first_year <- unique(rlms_individual[, c("idind", "first_year"), with = F], by = "idind")
setnames(rlms_individual_first_year, c("idind"), c("id"))
rlms_individual_first_year[, survey := "RLMS"]
rlms_individual_first_year[, level := "individual"]

############################
# For other surveys with recurring identifiers we simply take the min year

income_surveys_first_year <- income_surveys[ survey %in% c( "VODPF"), list(first_year = min(year)), by = c("survey", "level", "id")] 

# Attach RLMS and save in one object
income_surveys_first_year <- rbind(income_surveys_first_year, rlms_household_first_year, fill = T)
income_surveys_first_year <- rbind(income_surveys_first_year, rlms_individual_first_year, fill = T)

# Save point
save(income_surveys_first_year, file = "income_surveys_first_year.rdata", compress = "gzip")
