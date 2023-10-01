# Install the required dependencies

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)

# Read in the file with dependencies
packages <- readLines(con <- file("DEPENDENCIES", encoding = "UTF-8"))
packages <- gsub("^\\s+", "", packages[2:length(packages)])
packages <- gsub("\\s+.*$", "", packages)

# Install the uninstalled packages
missing_packages <- setdiff(packages, rownames(installed.packages()))

if ( length(missing_packages) > 0 ) {

	# Install the latest versions of all the packages
	install.packages(missing_packages, quiet = T) 

}