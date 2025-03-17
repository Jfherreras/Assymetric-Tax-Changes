################################################################################
######################### Assymetrical Taxation  ###############################
##################### Juan Felipe Herrera Sarmiento ############################
################################################################################

# This code produces: Non-linear Local Projections for tax cuts shocks
################################################################################
rm(list = ls())
# Load package
library(vctrs)
library(lpirfs)
library(gridExtra)
library(ggpubr)
library(rlang)
library(haven)
library(readxl)
# Load data
data <- read_excel("C:/Users/juanf/OneDrive - Università Commerciale Luigi Bocconi/ESS/Classes/4rd semester/Advanced Macroeconomics 4/Research proposal/Empirics/final_lp_dataset_v2.xlsx")

# The setting is narrative approach, hence LP-IV, where the instrument is the narrative shock
attach(data)
endog$RGDP <- data$RGDP
endog$ACITR <- data$ACITR
variable_shock_CI <- data$ACITR
variable_shock_PI <- data$APITR
narrative_shock_CI <- data$T_CI
narrative_shock_PI <- data$T_PI

#endog <- data[,c("RGDP","ACITR")]
#exog_vars <- data[,c("ACITR" , "PITB" , "CITB", "GOV" , "DEBT")]

endog <- data[,c("RGDP","PITB" , "CITB", "GOV" , "DEBT")]
exog_vars <- data[,c("ACITR" )]

results_nl_iv <- lp_nl_iv(endog_data = endog,
                    shock = narrative_shock_CI,
                    lags_endog_nl = 3,
                    #exo_data = exog_vars,
                    lags_exog = 4,
                    trend             = 0,
                    confint           = 1.96,
                    switching = RGDP,
                    use_hp = T,
                    lambda = 1600,
                    hor = 12,
                    gamma = 1.5)

# Show all impulse responses
plot(results_nl_iv)

# Make and save individual plots
plots_nl_iv <- plot_nl(results_nl_iv)

# Show single impulse responses
x11()
par(mfcol=c(2,2))
# Compare with red line of left plot (lower panel) in Figure 12 in Supplementary Appendix of RZ-18.
plot(plots_nl_iv$gg_s1[[1]])
# Compare with blue line of left plot (lower panel) in Figure 12 in Supplementary Appendix of RZ-18.
plot(plots_nl_iv$gg_s2[[1]])

# Show diagnostics. The first element shows the reaction of the first endogenous variable.
summary(results_nl_iv)


####
