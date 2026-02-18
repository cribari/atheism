# =====================================================================
# Reprodutibility Script
#
# Article:
# Atheism and Country-Level Intelligence: Elasticity and Cross-Regional
# Variation in Secularization
#
# Journal:
# Humanities and Social Sciences Communications 
#
# Authors:
# Tatiene C. Souza and Francisco Cribari-Neto 
#  
# ---------------------------------------------------------------------
# Purpose
# ---------------------------------------------------------------------
# This script reproduces some of the empirical results reported in the 
# article, including:
# - Descriptive statistics (Table 1)
# - Beta regression model fit (Table 2)
# - Goodness of fit statistics (AIC, BIC, Pseudo R2, Nagelkerke R2)
# - Plot of observed versus predicted values (Figure 1 (b))
#
# ---------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------
# Sample size: 131 countries
#
# Dependent variable:
# NOGOD — proportion of religious disbelievers at the country level
#
# Main regressors:
# IQ          — national intelligence estimate
# INCOME      — gross national income per capita
# OPEN        — trade openness (exports + imports as % of GDP)
# MUSLIM      — percentage Muslim population
# M           — Muslim-majority country dummy
# LA          — dummy variable for Latin American countries
# AF          — dummy variable for African countries 
#
# Constructed variables:
# INCOME.K  = INCOME / 10^4
# OPEN.L    = log(OPEN) / 10
# INT1      = INCOME.K^2 * OPEN.L^2
# INT2      = AF * OPEN.L^1.5
# IQ2       = IQ^2
# IQ_sqrt   = sqrt(IQ)
#
# Software:
# R statistical computing environment (https://www.r-project.org/)
# Required packages: betareg, moments
# =====================================================================

# ---------------------------------------------------------------------
# remove all existing objects
# ---------------------------------------------------------------------

rm(list = ls())

# ---------------------------------------------------------------------
# load the required packages (if not present, install them) 
# ---------------------------------------------------------------------

required_packages <- c("betareg", "moments")

if (!require(pkg, character.only = TRUE)) {
  install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---------------------------------------------------------------------
# read dataset
# ---------------------------------------------------------------------

df <- read.csv("data_atheism.csv", stringsAsFactors = FALSE)

# ---------------------------------------------------------------------
# make sure that all response values are in (0,1)
# ---------------------------------------------------------------------

if (any(df$NOGOD <= 0 | df$NOGOD >= 1)) {
  stop("NOGOD must lie strictly between 0 and 1.")
}

# ---------------------------------------------------------------------
# descriptive statistics – continuous variables
# ---------------------------------------------------------------------

desc_continuous <- function(x) {
   
x <- as.numeric(x)
x <- x[!is.na(x)]
   
m   <- mean(x)
sdv <- sd(x)
   
c(
   min      = min(x),
   q1       = unname(quantile(x, 0.25)),
   median   = median(x),
   mean     = m,
   q3       = unname(quantile(x, 0.75)),
   max      = max(x),
   sd       = sdv,
   cv       = ifelse(m != 0, sdv / m, NA),
   skewness = moments::skewness(x),
   kurtosis = moments::kurtosis(x)
    )
}

cont_vars <- c("IQ", "INCOME", "OPEN", "MUSLIM")

desc_table <- round(t(sapply(df[cont_vars], desc_continuous)), 4)

print(desc_table, quote = FALSE)

# ---------------------------------------------------------------------
# frequency distributions of dummy variables
# ---------------------------------------------------------------------

dummy_vars <- c("M", "LA", "AF")

do.call(rbind, lapply(dummy_vars, function(v) {
  tab <- table(df[[v]])
  pct <- 100 * tab / sum(tab)
  data.frame(
    variable = v,
    value = names(tab),
    count = as.vector(tab),
    percent = round(as.vector(pct), 2)
  )
}))

# ---------------------------------------------------------------------
# create additional regressors
# ---------------------------------------------------------------------

df_model <- df
df_model$IQ2     <- df_model$IQ^2
df_model$IQ_sqrt <- sqrt(df_model$IQ)

# ---------------------------------------------------------------------
# beta regression model
# mean submodel: complementary log-log link
# precision submodel: log link
# ---------------------------------------------------------------------

fit <- betareg(
  NOGOD ~ IQ + IQ2 + INT1 + M + LA + INT2 |
    IQ_sqrt + INCOME.K,
  data = df_model,
  link = "cloglog",
  link.phi = "log"
)

summary(fit)

# ---------------------------------------------------------------------
# goodness of fit statistics
# ---------------------------------------------------------------------

AIC_value  <- AIC(fit)
BIC_value  <- BIC(fit)
Pseudo_R2  <- summary(fit)$pseudo.r.squared

fit_null <- betareg(
  NOGOD ~ 1 | 1,
  data = df_model,
  link = fit$link$mean$name,
  link.phi = fit$link$precision$name
)

L0 <- exp(as.numeric(logLik(fit_null)))
L1 <- exp(as.numeric(logLik(fit)))
N  <- nrow(df_model)

# likelihood-based pseudo-R2 (Nagelkerke) 
Nagelkerke_R2 <- (1 - (L0 / L1)^(2 / N))

precisions <- predict(fit, type = "precision")
Precision_Ratio <- max(precisions) / min(precisions)

round(data.frame(
  AIC = AIC_value,
  BIC = BIC_value,
  Pseudo_R2 = Pseudo_R2,
  Nagelkerke_R2 = Nagelkerke_R2,
  Precision_Ratio = Precision_Ratio
), 4)

# ---------------------------------------------------------------------
# plot of observed versus predicted values
# ---------------------------------------------------------------------

predicted_values <- predict(fit, type = "response")

plot(df_model$NOGOD, predicted_values,
     xlab = "Observed values",
     ylab = "Predicted values",
     xlim = c(0, 0.8),
     ylim = c(0, 0.8))

abline(0, 1) # 45-degree line 
