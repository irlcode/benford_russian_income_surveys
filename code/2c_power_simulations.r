library(data.table)
library(digitTests)
library(benford.analysis)
library(cvmdisc)
library(sads)

# Declare working directory beforehand in an environment variable
# BENFORD_RUSSIAN_INCOME_SURVEYS_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("BENFORD_RUSSIAN_INCOME_SURVEYS_PATH")
setwd(path)
set.seed(42)

# Load helper functions
source("code/helper_functions/helper_functions.r")

# Benford first digit probabilities
pr_benford <- log10((1:9 + 1)/1:9)

# Function to sample data with specified first digit probabilities
# and test them against Benford with various tests

simulation_run <- function(digit_probs, n, simulations, alpha) {

	# Debug: digit_probs <- prob.rodriguez(-0.5)
	# n <- 1000
	# simulations <- 1000
	# alpha <- 0.1
	
	out <- list()
	
	for(s in 1:simulations) {
		
		values <- sample(1:9, n, replace = T, prob = digit_probs)
		
		# Extract first significant digits
		x <- extract_digits(values, check = "first", include.zero = FALSE)
		x <- x[!is.na(x)]
		n <- length(x)
		
		# Observed counts of first significant digits
		x_tab <- table(x)
		dig <- 1:9
		x <- rep(0, length(dig))
		x_included <- as.numeric(names(x_tab))
		index <- x_included
		x[index] <- as.numeric(x_tab)
		
		# Pearson χ statistic (Pearson, 1900) as well as 
		# goodness-of-fit tests: Anderson-Darling's A statistic, Cramér-von Mises' W statistic, Watson' U statistic (Watson, 1961)
		temp <- groupFit(counts = x, distr = "user", pfixed = pr_benford, imhof = FALSE, pave = TRUE)
		
		chisq <- temp$stats["Chi-squared","test_statistics"]
		chisqpval <- temp$pvals["Chi-squared","pvals"]
		asq <- temp$stats["A-squared","test_statistics"]
		asqpval <- temp$pvals["A-squared","pvals"]
		wsq <- temp$stats["W-squared","test_statistics"]
		wsqpval <- temp$pvals["W-squared","pvals"]
		usq <- temp$stats["U-squared","test_statistics"]
		usqpval <- temp$pvals["U-squared","pvals"]
		
		rm(temp)
		
		# Freedman’s modification of Watson’s U statistic (Freedman, 1981; Watson, 1961)
		#temp <- usq.benftest(values, pvalmethod = "simulate", pvalsims = 1000)
		#frusq <- unname(temp$statistic)
		#frusqpval <- unname(temp$p.value)
		#rm(temp)
		
		# Judge-Schechter mean deviation a* statistic
		#temp <- meandigit.benftest(values, digits = 1, pvalmethod = "asymptotic")
		#meandigit <- unname(temp$statistic)
		#meandigitpval <- unname(temp$p.value)
		#rm(temp)
		
		# Joint Digit Test T^2 statistic, a Hotelling type test (Hotelling, 1931)
		temp <- jointdigit.benftest(values, pvalmethod = "asymptotic")
		jointdigit <- unname(temp$statistic)
		jointdigitpval <- unname(as.numeric(temp$p.value))
		rm(temp)
		
		# Kolmogorov-Smirnov D statistic (Kolmogorov, 1933)
		#temp <- ks.benftest(values, pvalmethod = "simulate", pvalsims = 1000)
		#ks <- unname(temp$statistic)
		#kspval <- unname(temp$p.value)
		#rm(temp)
		
		# Joenssen’s JP statistic, a Shapiro-Francia type correlation test (Shapiro and Francia, 1972)
		#temp <- jpsq.benftest(values, pvalmethod = "simulate", pvalsims = 1000)
		#jpsq <- unname(temp$statistic)
		#jpsqpval <- unname(temp$p.value)
		#rm(temp)
		
		# Chebyshev distance m statistic (Leemis, 2000)
		#temp <- mdist.benftest(values, pvalmethod = "simulate", pvalsims = 1000)
		#mdist <- temp$statistic
		#mdistpval <- unname(temp$p.value)
		#rm(temp)
		
		# Euclidean distance d statistic (Cho and Gaines, 2007)
		#temp <- edist.benftest(values, pvalmethod = "simulate", pvalsims = 1000)
		#edist <- unname(temp$statistic)
		#edistpval <- unname(temp$p.value)
		#rm(temp)
		
		# Gather output
		out[[s]] <- data.table(chisqpval, asqpval, wsqpval, usqpval)#, frusqpval, meandigitpval, jointdigitpval, kspval, jpsqpval, mdistpval, edistpval)
		
	}
	
	# Rejection rate at specified level
	out <- rbindlist(out)
	out <- out[, lapply(.SD, function(x) mean(x < alpha))]
	
	return(out)
	
}

# Simulations at various levels of parameters of distributions
# for alternative hypotheses
# N is set at 71 to be equal to mean number of observations per
# secondary sampling unit in RLMS
# Every simulation is performed 1000 times

# Contaminated Benford
contambenford_res <- rbindlist(lapply(seq(0, 0.5, 0.01), function(x) simulation_run(digit_probs = prob.contambenford(x), n = 71, simulations = 1000, alpha = 0.1)))
contambenford_res[, alternative := "Contaminated Benford"]
contambenford_res[, param := seq(0, 0.5, 0.01)]

# Generalized Benford
genbenford_res <- rbindlist(lapply(seq(0.5, 1.5, 0.05), function(x) simulation_run(digit_probs = prob.genbenford(x), n = 71, simulations = 1000, alpha = 0.1)))
genbenford_res[, alternative := "Generalized Benford"]
genbenford_res[, param := seq(0.5, 1.5, 0.05)]

# Rodriguez
rodriguez_res <- rbindlist(lapply(seq(-2, 2, 0.1), function(x) simulation_run(digit_probs = prob.rodriguez(x), n = 71, simulations = 1000, alpha = 0.1)))
rodriguez_res[, alternative := "Rodriguez"]
rodriguez_res[, param := seq(-2, 2, 0.1)]

# Hurlimann
hurlimann_res <- rbindlist(lapply(seq(0.5, 2, 0.05), function(x) simulation_run(digit_probs = prob.hurlimann(x), n = 71, simulations = 1000, alpha = 0.1)))
hurlimann_res[, alternative := "Hurlimann"]
hurlimann_res[, param := seq(0.5, 2, 0.05)]

# To a single object
benford_power_simulations <- rbind(contambenford_res, genbenford_res)
benford_power_simulations <- rbind(benford_power_simulations, rodriguez_res)
benford_power_simulations <- rbind(benford_power_simulations, hurlimann_res)

# To a long object
benford_power_simulations <- melt(benford_power_simulations, id.vars = c("alternative", "param"), variable.name = "test", value.name = "power")
save(benford_power_simulations, file = "output/benford_power_simulations.rdata", compress = "gzip")
