library(digitTests)
library(sads)
library(DEoptim)
library(cvmdisc)
library(BenfordTests)

# First digit probabilities under Contaminated Benford’s Law
# Each digit Benford occurrence probability is increased by espilon,
# all probabilities are scaled to sum to one.
# When epsilon = 0 we have Benford's law
# https://doi.org/10.1371/journal.pone.0151235
# "The “contaminated” distributions arise from contaminating one digit by γ, the amount specified in the table. Each digit is contaminated in turn, increasing that digit’s Benford probability by γ, then the remaining digit probabilities are scaled so that all sum to one. This type of distribution was found to arise, in practice, for example when one specific accounting transaction had been processed many times."

prob.contambenford <- function(epsilon) {

	aux <- log10(1 + 1 / 1:9)
	
	for( i in 1:9 ) {
		
		# Increase i-th digit probability
		aux[i] <- aux[i] + epsilon
		
		# Scale the remaining digits
		# https://stackoverflow.com/a/66430328
		target_sum <- 1 - aux[i]
		
		aux[-i] <- aux[-i] * target_sum/sum(aux[-i])
				
	}
		
	return(aux)
	
}


########################################################
# First digit probabilities under Generalized Benford’s Law
# When alpha -> 1 (but not = 1) it becomes Benford's law
# When alpha = 0 we have uniform distribution

prob.genbenford <- function(alpha) {
	
	if( alpha != 1 ) {
	
		aux <- ((2:10)^(1-alpha) - (1:9)^(1-alpha))/(1-alpha)
			
	} else {
	
		aux <- log10(1 + 1 / 1:9)
		
	}
	
	# Proportions
	aux <- aux/sum(aux)
	return(aux)
	
}


# First digit probabilities under Rodriguez distribution
# Stigler’s law, "which arises from a mixture scheme in which all uniform distributions are equally likely"
# https://doi.org/10.1198/0003130042782
# is when beta = 0
prob.rodriguez <- function(beta) {

	if( beta == 0 ) {
		
		aux <- 1/9*(1 + (10/9)*log(10) + (1:9)*log(1:9) - (2:10)*log(2:10) )

	} else if( beta == -1 ) {
		
		aux <- log10(1 + 1 / 1:9)

	} else {
	
		aux <- (beta + 1)/(9*beta) - ( (2:10)^(beta+1) - (1:9)^(beta+1) )/(beta*(10^(beta+1) - 1))
			
	}

	return(aux)
}

# First digit probabilities under Hurlimann distribution
prob.hurlimann <- function(gamma) {

	aux <- 1/2*(log10(2:10)^gamma - log10(1:9)^gamma - (1 - log10(2:10))^gamma + (1 - log10(1:9))^gamma )

	# Proportions
	aux <- aux/sum(aux)
	return(aux)
	
}

# Mean square deviation empirical vs theoretical
msd.stigler <- function(empirical_proportions) {
	
	sum((prob.rodriguez(0) - empirical_proportions)^2)
	
}

msd.contambenford <- function(epsilon, empirical_proportions) {
	
	sum((prob.contambenford(epsilon) - empirical_proportions)^2)
	
}

msd.genbenford <- function(alpha, empirical_proportions) {
	
	sum((prob.genbenford(alpha) - empirical_proportions)^2)
	
}

msd.rodriguez <- function(beta, empirical_proportions) {
	
	sum((prob.rodriguez(beta) - empirical_proportions)^2)
	
}

msd.hurlimann <- function(gamma, empirical_proportions) {
	
	sum((prob.hurlimann(gamma) - empirical_proportions)^2)
	
}

# Log-normal random variable generator
#rlnorm_exact <- function(n, mean, sd) {
#	
#	m2 <-  log(mean^2 / sqrt(sd^2 + mean^2))
#	sd2 <- sqrt(log(1 + (sd^2 / mean^2)))
#	r <- c(scale(rnorm(n)))
#	return(exp(sd2*r+m2))
#	
#}

# Function that takes a vector of values and returns
# a table with conformity to various distributions
test_conformity <- function(values, bayesian = TRUE) {
	
	# Debug:
	#data("sinoForest", package = "digitTests")
	#values <- sinoForest$value
	
	# Filter values
	values <- values[!is.na(values)]
	values <- values[!is.infinite(values)]
	values <- values[ values != 0]

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
    
	# Various counts for debugging
	# x <- c(231, 124, 97, 70, 64, 54, 40, 54, 38) # Sino Forest (https://psyarxiv.com/kzqp5)
	# x <- c(509, 353, 177, 114, 77, 77, 53, 73, 64) # Greek Fiscal (multibridge, https://doi.org/10.3758/s13428-022-02020-1)

	# Proportions
	x_prop <- x/sum(x)
	
	# Observations
	N <- sum(x)
	digits <- 1:9
	
	
	########################################################
	# Optimize parameters of each distribution
	# NB: parameter space is as in https://doi.org/10.1371/journal.pone.0271969
	# We encapsulate in tryCatch to avoid the problem of NaN value of objective function
	iterations <- 1000
	
	# Init parameters to optimize
	gamma.star <- beta.star <- alpha.star <- epsilon.star <- NULL
	
	tryCatch({
		
		epsilon.star <- DEoptim(fn = msd.contambenford, lower = 0, upper = 1,
							control = list(NP = 200, itermax = iterations, strategy = 3, trace = F),
							empirical_proportions = x_prop)$optim$bestmem[[1]]
							
	}, error = function(e) {})
	
	tryCatch({
		
		alpha.star <- DEoptim(fn = msd.genbenford, lower = -1.5, upper = 1.5,
							control = list(NP = 200, itermax = iterations, strategy = 3, trace = F),
							empirical_proportions = x_prop)$optim$bestmem[[1]]
							
	}, error = function(e) {})
	
	tryCatch({

		beta.star <- DEoptim(fn = msd.rodriguez, lower = -14, upper = 10,
								control = list(NP = 200, itermax = iterations, strategy = 3, trace = F),
								empirical_proportions = x_prop)$optim$bestmem[[1]]
	}, error = function(e) {})

	tryCatch({

		gamma.star <- DEoptim(fn = msd.hurlimann, lower = 1e-4, upper = 10,
								control = list(NP = 200, itermax = iterations, strategy = 3, trace = F),
								empirical_proportions = x_prop)$optim$bestmem[[1]]
	}, error = function(e) {})
	
	# Expected proportions for Benford's Law and alternative distributions
	# under optimal parameters
	if( !any( is.null(gamma.star) | is.null(beta.star) | is.null(alpha.star) | is.null(epsilon.star) ) ) {

		pr_benford <- log10((1:9 + 1)/1:9)
		pr_stigler <- prob.rodriguez(beta = 0)
		pr_uniform <- rep(1/9, 9)
		pr_contaminated_benford <- prob.contambenford(epsilon = epsilon.star)
		pr_generalized_benford <- prob.genbenford(alpha = alpha.star)
		pr_rodriguez <- prob.rodriguez(beta = beta.star)
		pr_hurlimann <- prob.hurlimann(gamma = gamma.star)
		
	}
	
	if( bayesian == T ) {

		if( !any( is.null(gamma.star) | is.null(beta.star) | is.null(alpha.star) | is.null(epsilon.star) ) ) {
			
			########################################################
			# Bayesian testing
		
			
			# Compute Bayes factor
			# The prior distribution for 𝐻1 is a Dirichlet(α1, α2, ... , α9) distribution with all α parameters set to 1.
			# After seeing the observed counts 𝑥𝑑 of each digit d, the posterior distribution for 𝐻1is a Dirichlet(α1 + 𝑥1, α2 + 𝑥2, ... , α9 + 𝑥9) distribution.
			
			# The Bayes factor in favor of 𝐻1 can be obtained by calculating the Savage-Dickey
			# density ratio, that is, the height under the posterior distribution 𝑝(θ = 𝑐 | 𝑥, 𝐻1) at the point
			# of interest c divided by the height under the prior distribution 𝑝(θ = 𝑐 | 𝐻1) at this point, see
			# equation on page 8 of https://psyarxiv.com/bux7p/download?format=pdf
			# Good (1967) or on page 10
			
			BF01_benford <- unname(distr.btest(values, check = "first", reference = pr_benford, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_stigler <- unname(distr.btest(values, check = "first", reference = pr_stigler, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_uniform <- unname(distr.btest(values, check = "first", reference = pr_uniform, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_contaminated_benford <- unname(distr.btest(values, check = "first", reference = pr_contaminated_benford, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_generalized_benford <- unname(distr.btest(values, check = "first", reference = pr_generalized_benford, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_rodriguez <- unname(distr.btest(values, check = "first", reference = pr_rodriguez, alpha = rep(1, 9), BF10 = F, log = F)$bf)
			BF01_hurlimann <- unname(distr.btest(values, check = "first", reference = pr_hurlimann, alpha = rep(1, 9), BF10 = F, log = F)$bf)
		
			# Posterior probabilities for the Null hypothesis
			# (assuming prior probabilities are 0.5)
			prior_H0 <- 0.5
		
			posterior_H0_benford <- (BF01_benford*prior_H0)/(BF01_benford*prior_H0 + (1-prior_H0))
			posterior_H0_stigler <- (BF01_stigler*prior_H0)/(BF01_stigler*prior_H0 + (1-prior_H0))
			posterior_H0_uniform <- (BF01_uniform*prior_H0)/(BF01_uniform*prior_H0 + (1-prior_H0))
			posterior_H0_contaminated_benford <- (BF01_contaminated_benford*prior_H0)/(BF01_contaminated_benford*prior_H0 + (1-prior_H0))
			posterior_H0_generalized_benford <- (BF01_generalized_benford*prior_H0)/(BF01_generalized_benford*prior_H0 + (1-prior_H0))
			posterior_H0_rodriguez <- (BF01_rodriguez*prior_H0)/(BF01_rodriguez*prior_H0 + (1-prior_H0))
			posterior_H0_hurlimann <- (BF01_hurlimann*prior_H0)/(BF01_hurlimann*prior_H0 + (1-prior_H0))
		
			## Optimal parameters
			optimal_row <- data.table(type = "optim", contaminated_benford = epsilon.star, generalized_benford = alpha.star, rodriguez = beta.star, hurlimann = gamma.star)
			## Log Bayes factors
			bf_row <- data.table(type = "log10BF01", benford = log10(BF01_benford), stigler = log10(BF01_stigler), uniform = log10(BF01_uniform), contaminated_benford = log10(BF01_contaminated_benford), generalized_benford = log10(BF01_generalized_benford), rodriguez = log10(BF01_rodriguez), hurlimann = log10(BF01_hurlimann))
			## Posterior probabilities
			posterior_row <- data.table(type = "posterior", bf_row[, lapply(.SD, function(x)  1 - 1/exp(x)), .SDcols = c("benford", "stigler", "uniform", "contaminated_benford", "generalized_benford", "rodriguez", "hurlimann")])
			
			out <- rbindlist(list(posterior_row, bf_row, optimal_row), fill = T)

		} else {
			
			# In case optimization failed
			out <- data.table(type = c("optim", "log10BF01", "posterior"), benford = NA_real_, uniform = NA_real_, contaminated_benford = NA_real_, generalized_benford = NA_real_, rodriguez = NA_real_, hurlimann = NA_real_)
			
		}
		
	} else {
		
		if( !any( is.null(gamma.star) | is.null(beta.star) | is.null(alpha.star) | is.null(epsilon.star) ) ) {


			########################################################
			# Frequentist testing
			
			# Benford's law	
			# Pearson χ statistic (Pearson, 1900) as well as 
			# goodness-of-fit tests: Anderson-Darling's A statistic, Cramér-von Mises' W statistic, Watson' U statistic (Watson, 1961)
	    	temp <- groupFit(counts = x, distr = "user", pfixed = pr_benford, imhof = FALSE, pave = TRUE)
	    	
	    	benford_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	benford_chisqpval <- temp$pvals["Chi-squared","pvals"]
			benford_asq <- temp$stats["A-squared","test_statistics"]
			benford_asqpval <- temp$pvals["A-squared","pvals"]
			benford_wsq <- temp$stats["W-squared","test_statistics"]
			benford_wsqpval <- temp$pvals["W-squared","pvals"]
			benford_usq <- temp$stats["U-squared","test_statistics"]
			benford_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Stigler's law
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_stigler, imhof = FALSE, pave = TRUE)
	    	
	    	stigler_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	stigler_chisqpval <- temp$pvals["Chi-squared","pvals"]
			stigler_asq <- temp$stats["A-squared","test_statistics"]
			stigler_asqpval <- temp$pvals["A-squared","pvals"]
			stigler_wsq <- temp$stats["W-squared","test_statistics"]
			stigler_wsqpval <- temp$pvals["W-squared","pvals"]
			stigler_usq <- temp$stats["U-squared","test_statistics"]
			stigler_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Uniform distribution
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_uniform, imhof = FALSE, pave = TRUE)
	    	
	    	uniform_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	uniform_chisqpval <- temp$pvals["Chi-squared","pvals"]
			uniform_asq <- temp$stats["A-squared","test_statistics"]
			uniform_asqpval <- temp$pvals["A-squared","pvals"]
			uniform_wsq <- temp$stats["W-squared","test_statistics"]
			uniform_wsqpval <- temp$pvals["W-squared","pvals"]
			uniform_usq <- temp$stats["U-squared","test_statistics"]
			uniform_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Contaminated Benford
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_contaminated_benford, imhof = FALSE, pave = TRUE)
	    	
	    	contaminated_benford_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	contaminated_benford_chisqpval <- temp$pvals["Chi-squared","pvals"]
			contaminated_benford_asq <- temp$stats["A-squared","test_statistics"]
			contaminated_benford_asqpval <- temp$pvals["A-squared","pvals"]
			contaminated_benford_wsq <- temp$stats["W-squared","test_statistics"]
			contaminated_benford_wsqpval <- temp$pvals["W-squared","pvals"]
			contaminated_benford_usq <- temp$stats["U-squared","test_statistics"]
			contaminated_benford_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Generalized benford's law
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_generalized_benford, imhof = FALSE, pave = TRUE)
	    	
	    	generalized_benford_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	generalized_benford_chisqpval <- temp$pvals["Chi-squared","pvals"]
			generalized_benford_asq <- temp$stats["A-squared","test_statistics"]
			generalized_benford_asqpval <- temp$pvals["A-squared","pvals"]
			generalized_benford_wsq <- temp$stats["W-squared","test_statistics"]
			generalized_benford_wsqpval <- temp$pvals["W-squared","pvals"]
			generalized_benford_usq <- temp$stats["U-squared","test_statistics"]
			generalized_benford_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Rodriguez distribution
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_rodriguez, imhof = FALSE, pave = TRUE)
	    	
	    	rodriguez_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	rodriguez_chisqpval <- temp$pvals["Chi-squared","pvals"]
			rodriguez_asq <- temp$stats["A-squared","test_statistics"]
			rodriguez_asqpval <- temp$pvals["A-squared","pvals"]
			rodriguez_wsq <- temp$stats["W-squared","test_statistics"]
			rodriguez_wsqpval <- temp$pvals["W-squared","pvals"]
			rodriguez_usq <- temp$stats["U-squared","test_statistics"]
			rodriguez_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)
			
			# Hurlimann's distribution
			temp <- groupFit(counts = x, distr = "user", pfixed = pr_hurlimann, imhof = FALSE, pave = TRUE)
	    	
	    	hurlimann_chisq <- temp$stats["Chi-squared","test_statistics"]
	    	hurlimann_chisqpval <- temp$pvals["Chi-squared","pvals"]
			hurlimann_asq <- temp$stats["A-squared","test_statistics"]
			hurlimann_asqpval <- temp$pvals["A-squared","pvals"]
			hurlimann_wsq <- temp$stats["W-squared","test_statistics"]
			hurlimann_wsqpval <- temp$pvals["W-squared","pvals"]
			hurlimann_usq <- temp$stats["U-squared","test_statistics"]
			hurlimann_usqpval <- temp$pvals["U-squared","pvals"]
			
			rm(temp)

			# Test that are unused:
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
			#temp <- jointdigit.benftest(values, pvalmethod = "asymptotic")
			#jointdigit <- unname(temp$statistic)
			#jointdigitpval <- unname(as.numeric(temp$p.value))
			#rm(temp)
			
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
					
			# Mean Absolute Deviation & Mantissa
			#fit <- benford(financials)
			#mad <- unname(fit$MAD)
			#mantissa <- unname(fit$stats$mantissa.arc.test$statistic)
			#mantissapval <- unname(fit$stats$mantissa.arc.test$p.value)
			
			optimal_row <- data.table(type = "optim",
							contaminated_benford_chisq = epsilon.star,
							contaminated_benford_asq = epsilon.star,
							contaminated_benford_wsq = epsilon.star,
							contaminated_benford_usq = epsilon.star,
							generalized_benford_chisq = alpha.star,
							generalized_benford_asq = alpha.star,
							generalized_benford_wsq = alpha.star,
							generalized_benford_usq = alpha.star,
							rodriguez_chisq = beta.star,
							rodriguez_asq = beta.star,
							rodriguez_wsq = beta.star,
							rodriguez_usq = beta.star,
							hurlimann_chisq = gamma.star,
							hurlimann_asq = gamma.star,
							hurlimann_wsq = gamma.star,
							hurlimann_usq = gamma.star)
							
			stat_row <- data.table(type = "stat",
							benford_chisq,
							benford_asq,
							benford_wsq,
							benford_usq,
							stigler_chisq,
							stigler_asq,
							stigler_wsq,
							stigler_usq,
							uniform_chisq,
							uniform_asq,
							uniform_wsq,
							uniform_usq,
							contaminated_benford_chisq,
							contaminated_benford_asq,
							contaminated_benford_wsq,
							contaminated_benford_usq,
							generalized_benford_chisq,
							generalized_benford_asq,
							generalized_benford_wsq,
							generalized_benford_usq,
							rodriguez_chisq,
							rodriguez_asq,
							rodriguez_wsq,
							rodriguez_usq,
							hurlimann_chisq,
							hurlimann_asq,
							hurlimann_wsq,
							hurlimann_usq)
										
			pvalue_row <- data.table(type = "pvalue",
							benford_chisq = benford_chisqpval,
							benford_asq = benford_asqpval,
							benford_wsq = benford_wsqpval,
							benford_usq = benford_usqpval,
							stigler_chisq = stigler_chisqpval,
							stigler_asq = stigler_asqpval,
							stigler_wsq = stigler_wsqpval,
							stigler_usq = stigler_usqpval,
							uniform_chisq = uniform_chisqpval,
							uniform_asq = uniform_asqpval,
							uniform_wsq = uniform_wsqpval,
							uniform_usq = uniform_usqpval,
							contaminated_benford_chisq = contaminated_benford_chisqpval,
							contaminated_benford_asq = contaminated_benford_asqpval,
							contaminated_benford_wsq = contaminated_benford_wsqpval,
							contaminated_benford_usq = contaminated_benford_usqpval,
							generalized_benford_chisq = generalized_benford_chisqpval,
							generalized_benford_asq = generalized_benford_asqpval,
							generalized_benford_wsq = generalized_benford_wsqpval,
							generalized_benford_usq = generalized_benford_usqpval,
							rodriguez_chisq = rodriguez_chisqpval,
							rodriguez_asq  = rodriguez_asqpval,
							rodriguez_wsq = rodriguez_wsqpval,
							rodriguez_usq = rodriguez_usqpval,
							hurlimann_chisq = hurlimann_chisqpval,
							hurlimann_asq = hurlimann_asqpval,
							hurlimann_wsq = hurlimann_wsqpval,
							hurlimann_usq= hurlimann_usqpval)
							
											
			out <- rbindlist(list(optimal_row, pvalue_row, stat_row), fill = T)
			
		} else {
			
			# In case optimization failed
			out <- data.table(type = c("optim", "pvalue", "stat"), benford_chisq = NA_real_, benford_asq = NA_real_, benford_wsq = NA_real_, benford_usq = NA_real_, stigler_chisq = NA_real_, stigler_asq = NA_real_, stigler_wsq = NA_real_, stigler_usq = NA_real_, uniform_chisq = NA_real_, uniform_asq = NA_real_, uniform_wsq = NA_real_, uniform_usq = NA_real_, contaminated_benford_chisq = NA_real_, contaminated_benford_asq = NA_real_, contaminated_benford_wsq = NA_real_, contaminated_benford_usq = NA_real_, rodriguez_chisq = NA_real_, rodriguez_asq = NA_real_, rodriguez_wsq = NA_real_, rodriguez_usq = NA_real_, hurlimann_chisq = NA_real_, hurlimann_asq = NA_real_, hurlimann_wsq = NA_real_, hurlimann_usq = NA_real_)
			
		}
		
	}
	
	return(out)
	
}