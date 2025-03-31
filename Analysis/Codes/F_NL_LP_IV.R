################################################################################
######################### Assymetrical Taxation  ###############################
##################### Juan Felipe Herrera Sarmiento ############################
################################################################################

# This code produces: Non-linear Local Projections for income tax shocks
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
library(ggplot2)
library(dplyr)
library(tidyr)

# General comment: Blue is high (as in boosts or high ineq) and Yellow is low (busts, low ineq)
setwd('C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Codes')
# Load data
data <- read_excel("C:/Users/juanf/OneDrive - Università Commerciale Luigi Bocconi/ESS/Classes/4rd semester/Advanced Macroeconomics 4/Research proposal/Empirics/final_lp_dataset_v2.xlsx")

# The setting is narrative approach, hence LP-IV, where the instrument is the narrative shock
attach(data)
# endog$RGDP <- data$RGDP
# endog$ACITR <- data$ACITR
# variable_shock_CI <- data$ACITR
# variable_shock_PI <- data$APITR
 narrative_shock_CI <- data$T_CI
 narrative_shock_PI <- data$T_PI

#endog <- data[,c("RGDP","ACITR")]
#exog_vars <- data[,c("ACITR" , "PITB" , "CITB", "GOV" , "DEBT")]

 ## Discriminating tax increases vs cuts
 # Narrative shocks
 # CI
data$T_CI_inc <- data$T_CI
data$T_CI_inc[data$T_CI_inc < 0] <- 0

data$T_CI_cut <- data$T_CI
data$T_CI_cut[data$T_CI_cut > 0] <- 0
 # PI
data$T_PI_inc <- data$T_PI
data$T_PI_inc[data$T_PI_inc < 0] <- 0

data$T_PI_cut <- data$T_PI
data$T_PI_cut[data$T_PI_cut > 0] <- 0

 # Proxy shock
# CI
data$m_CI_inc <- data$m_CI
data$m_CI_inc[data$m_CI_inc < 0] <- 0
data$m_CI_cut <- data$m_CI
data$m_CI_cut[data$m_CI_cut > 0] <- 0
# PI
data$m_PI_inc <- data$m_PI
data$m_PI_inc[data$m_PI_inc < 0] <- 0
data$m_PI_cut <- data$m_PI
data$m_PI_cut[data$m_PI_cut > 0] <- 0

##### NL-LP-IV CITR general narrative shock effects
## Figure 2: APITR, OUTPUT, PITB, PITR, ACITR, GOV
#endog <- data[,c("APITR", "RGDP", "PITB", "PITR", "ACITR", "GOV")]
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]
#endog <- data[,c("RGDP","PITB" , "CITB", "GOV" , "DEBT")]
#exog_vars <- data[,c("ACITR" )]

# CITR narrative general shock! Non linear, booms vs busts
nl_iv_CITR_gdp <- lp_nl_iv(endog_data = endog,
                    shock = narrative_shock_CI,
                    lags_endog_nl = 3,
                    #exo_data = exog_vars,
                    lags_exog = 4,
                    trend             = 0,
                    confint           = 1.96,
                    switching = RGDP,
                    use_logistic = T,
                    use_hp = T,
                    lambda = 1600,
                    hor = 12,
                    gamma = 3)

# Show all impulse responses
plot(nl_iv_CITR_gdp)

#### Plot
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')
  #c('GDP','APITR','ACITR','PITB','CITB','GOV','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_CITR_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_CITR_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_CITR_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_CITR_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_CITR_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_CITR_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

# By default, "Horizon" might be "V1", "V2", or just "1", "2". 
# If it's "V1", "V2", you can parse out the numeric part:
# irf_all$Horizon <- as.numeric(gsub("V", "", irf_all$Horizon))

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)
# Conclusion: 
ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_CITR_general_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
###############################################################################

######## NL-LP-IV: CITR CUT
narrative_shock_CI <- data$T_CI_cut
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_CITR_cut_gdp <- lp_nl_iv(endog_data = endog,
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
                           gamma = 3)

# Show all impulse responses
plot(nl_iv_CITR_cut_gdp)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_CITR_cut_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

# By default, "Horizon" might be "V1", "V2", or just "1", "2". 
# If it's "V1", "V2", you can parse out the numeric part:
# irf_all$Horizon <- as.numeric(gsub("V", "", irf_all$Horizon))

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)
ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_CITR_CUT_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
# Conclusion: 
###############################################################################

######## NL-LP-IV: CITR INCREASE
narrative_shock_CI <- data$T_CI_inc
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_CITR_inc_gdp <- lp_nl_iv(endog_data = endog,
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
                               gamma = 3)

# Show all impulse responses
plot(nl_iv_CITR_inc_gdp)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_CITR_inc_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_CITR_INC_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
# Conclusion: 


###############################################################################
######## NL-LP-IV: PITR GENERAL
narrative_shock_PI <- data$T_PI
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_PITR_gdp <- lp_nl_iv(endog_data = endog,
                               shock = narrative_shock_PI,
                               lags_endog_nl = 3,
                               #exo_data = exog_vars,
                               lags_exog = 4,
                               trend             = 0,
                               confint           = 1.96,
                               switching = RGDP,
                               use_hp = T,
                               lambda = 1600,
                               hor = 12,
                               gamma = 3)

# Show all impulse responses
plot(nl_iv_PITR_gdp)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_PITR_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_PITR_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_PITR_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_PITR_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_PITR_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_PITR_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_PITR_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
# Conclusion: 

#################################################################
######## NL-LP-IV: PITR CUT
narrative_shock_PI <- data$T_PI_cut
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_PITR_cut_gdp <- lp_nl_iv(endog_data = endog,
                           shock = narrative_shock_PI,
                           lags_endog_nl = 3,
                           #exo_data = exog_vars,
                           lags_exog = 4,
                           trend             = 0,
                           confint           = 1.96,
                           switching = RGDP,
                           use_hp = T,
                           lambda = 1600,
                           hor = 12,
                           gamma = 3)

# Show all impulse responses
plot(nl_iv_PITR_cut_gdp)
# Conclusion: no differences
#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_PITR_cut_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_PITR_cut_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
# Conclusion: 


################################################################
######## NL-LP-IV: PITR INCREASE
narrative_shock_PI <- data$T_PI_inc
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_PITR_inc_gdp <- lp_nl_iv(endog_data = endog,
                               shock = narrative_shock_PI,
                               lags_endog_nl = 3,
                               #exo_data = exog_vars,
                               lags_exog = 4,
                               trend             = 0,
                               confint           = 1.96,
                               switching = RGDP,
                               use_hp = T,
                               lambda = 1600,
                               hor = 12,
                               gamma = 3)

# Show all impulse responses
plot(nl_iv_PITR_inc_gdp)
# Conclusion: no differences
#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (Booms)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_PITR_inc_gdp$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Busts)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_PITR_inc_cycle.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)
# Conclusion: 

#########################################################################
################ ONLY INEQUALITY STATE DEPENDING RESULTS


#########################################################################
######## NL-LP-IV: CITR GENERAL - GINI PRE
narrative_shock_CI <- data$T_CI
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_pre_tax')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_CITR_gini_pre <- lp_nl_iv(endog_data = endog,
                           shock = narrative_shock_CI,
                           lags_endog_nl = 3,
                           #exo_data = exog_vars,
                           lags_exog = 4,
                           trend             = 0,
                           confint           = 1.96,
                           switching = Gini_pre_tax,
                           use_hp = T,
                           lambda = 1600,
                           hor = 12,
                           gamma = 3)

# Show all impulse responses
plot(nl_iv_CITR_gini_pre)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_pre')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_CITR_gini_pre$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_CITR_gini_pre$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_CITR_gini_pre$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (High Inequality)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_CITR_gini_pre$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_CITR_gini_pre$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_CITR_gini_pre$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Low Inequality)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_CITR_Gini_pre.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)



#########################################################################
######## NL-LP-IV: CITR GENERAL - GINI POST
narrative_shock_CI <- data$T_CI
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_post_tax')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_CITR_gini_post <- lp_nl_iv(endog_data = endog,
                                shock = narrative_shock_CI,
                                lags_endog_nl = 3,
                                #exo_data = exog_vars,
                                lags_exog = 4,
                                trend             = 0,
                                confint           = 1.96,
                                switching = Gini_post_tax,
                                use_hp = T,
                                lambda = 1600,
                                hor = 12,
                                gamma = 3)

# Show all impulse responses
# plot(nl_iv_CITR_gini_post)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_post')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_CITR_gini_post$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_CITR_gini_post$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_CITR_gini_post$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (High Inequality)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_CITR_gini_post$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_CITR_gini_post$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_CITR_gini_post$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Low Inequality)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_CITR_Gini_post.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)



#########################################################################
##### NL-LP-IV: PITR GENERAL - GINI PRE
narrative_shock_PI <- data$T_PI
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_pre_tax')]

# PITR narrative cut shock! Non linear, booms vs busts
nl_iv_PITR_gini_pre <- lp_nl_iv(endog_data = endog,
                                shock = narrative_shock_PI,
                                lags_endog_nl = 3,
                                #exo_data = exog_vars,
                                lags_exog = 4,
                                trend             = 0,
                                confint           = 1.96,
                                switching = Gini_pre_tax,
                                use_hp = T,
                                lambda = 1600,
                                hor = 12,
                                gamma = 3)

# Show all impulse responses
# plot(nl_iv_PITR_gini_pre)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_pre')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_PITR_gini_pre$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_PITR_gini_pre$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_PITR_gini_pre$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (High Inequality)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_PITR_gini_pre$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_PITR_gini_pre$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_PITR_gini_pre$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Low Inequality)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_PITR_Gini_pre.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)



##########################################################################
######## NL-LP-IV: PITR GENERAL - GINI POST
narrative_shock_PI <- data$T_PI
endog <- data[,c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_post_tax')]

# CITR narrative cut shock! Non linear, booms vs busts
nl_iv_PITR_gini_post <- lp_nl_iv(endog_data = endog,
                                 shock = narrative_shock_PI,
                                 lags_endog_nl = 3,
                                 #exo_data = exog_vars,
                                 lags_exog = 4,
                                 trend             = 0,
                                 confint           = 1.96,
                                 switching = Gini_post_tax,
                                 use_hp = T,
                                 lambda = 1600,
                                 hor = 12,
                                 gamma = 3)

# Show all impulse responses
# plot(nl_iv_PITR_gini_post)

#####
###############################################################################
# 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
###############################################################################
# Labels, Real GDP first:
my_vars <- c('APITR','ACITR','PITB','CITB','GOV','RGDP','DEBT','Gini_post')

# -- Regime 1: Mean, Low, Up
df_mean_1 <- as.data.frame(nl_iv_PITR_gini_post$irf_s1_mean)
df_low_1  <- as.data.frame(nl_iv_PITR_gini_post$irf_s1_low)
df_up_1   <- as.data.frame(nl_iv_PITR_gini_post$irf_s1_up)

# Add a column "Variable" that identifies each row
df_mean_1$Variable <- my_vars
df_low_1$Variable  <- my_vars
df_up_1$Variable   <- my_vars

###############################################################################
# 2) Reshape from wide (columns = horizons) to long format
###############################################################################

mean_long_1 <- pivot_longer(
  df_mean_1, 
  cols = -Variable,
  names_to = "Horizon", 
  values_to = "mean"
)

low_long_1 <- pivot_longer(
  df_low_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "lower"
)

up_long_1 <- pivot_longer(
  df_up_1,
  cols = -Variable,
  names_to = "Horizon",
  values_to = "upper"
)

# Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
regime1_df <- mean_long_1 %>%
  left_join(low_long_1, by = c("Variable", "Horizon")) %>%
  left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 1 (High Inequality)")

###############################################################################
# 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
###############################################################################
df_mean_2 <- as.data.frame(nl_iv_PITR_gini_post$irf_s2_mean)
df_low_2  <- as.data.frame(nl_iv_PITR_gini_post$irf_s2_low)
df_up_2   <- as.data.frame(nl_iv_PITR_gini_post$irf_s2_up)

df_mean_2$Variable <- my_vars
df_low_2$Variable  <- my_vars
df_up_2$Variable   <- my_vars

mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")

regime2_df <- mean_long_2 %>%
  left_join(low_long_2, by = c("Variable", "Horizon")) %>%
  left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
  mutate(Regime = "Regime 2 (Low Inequality)")

###############################################################################
# 4) Combine both regimes & optionally convert "Horizon" to numeric
###############################################################################
irf_all <- bind_rows(regime1_df, regime2_df)

###############################################################################
# 5) Plot with ggplot2
###############################################################################

# 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
irf_all <- irf_all %>%
  mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# Removes the "V" and converts to numeric

#Re order variabes
irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)

# 2) Plot using Horizon_num on the x-axis

p=ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
  geom_line(color = "blue") +
  # Here is the red horizontal line at y=0:
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_grid(Variable ~ Regime, scales = "free_y") +
  labs(title = "Nonlinear LP-IV (narrative shock)",
       x = "Horizon",
       y = "Response") +
  theme_minimal(base_size = 14)

ggsave("C:/Users/juanf/OneDrive/Documentos/GitHub/Assymetric-Tax-Changes/Analysis/Output/Figures/FiguresMR2013/NL_LP_IV_NS_PITR_Gini_post.pdf", plot = p, device = "pdf", 
       width = 8, height = 7)


##########################################################################


##########################################################################


##########################################################################


##########################################################################


##########################################################################

# #### Annual data #####
# data_anual <- read_excel("C:/Users/juanf/OneDrive - Università Commerciale Luigi Bocconi/ESS/Classes/4rd semester/Advanced Macroeconomics 4/Research proposal/Empirics/final_lp_dataset_v2.xlsx", 
#                                   sheet = "Annualy")
# #View(data_anual)
# attach(data_anual)
# ## PITR
# # endog <- c()
# # endog$RGDP <- data_anual$RGDP
# # endog$APITR <- data_anual$APITR
# # endog$PITB <- data_anual$PITB
# # endog$GOV <- data_anual$GOV
# # endog$DEBT <- data_anual$DEBT
# #   
# #   
# # variable_shock_PI <- data_anual[,'APITR']
# # narrative_shock_PI <- data_anual[,'T_PI']
# 
# 
# endog <- data_anual[,c("RGDP","APITR" , "PITB", "GOV" , "DEBT")]
# variable_shock_PI <- data_anual[,c("APITR" )]
# narrative_shock_PI <- data_anual[,c("T_PI")]
# ## 0. Replication of the paper to see if it works
# # description: lineal local projections with IV (narrative shock)
# lp_replication = lp_lin_iv(endog_data = endog,
#                            shock = variable_shock_PI,
#                            cumul_mult = F,
#                            use_twosls = F,
#                            instrum = narrative_shock_PI,
#                            lags_endog_lin = 4,
#                            exog_data = NULL,
#                            lags_exog = 4,
#                            contemp_data = NULL,
#                            lags_criterion = NaN,
#                            max_lags = NaN,
#                            trend = 0,
#                            confint = 1.96,
#                            use_nw = TRUE,
#                            nw_lag = NULL,
#                            nw_prewhite = FALSE,
#                            adjust_se = FALSE,
#                            hor = 10,
#                            num_cores = 1
#                            )
# # graph
# plot(lp_replication) # Works!
# summary.lpirfs_lin_iv_obj(lp_replication)
# 
# ## 1. PITR with different economic cycle
# #switching data
# switching_data <- endog$RGDP
# lp_nl_cycle = lp_nl_iv(endog_data = endog,
#                        lags_endog_nl = 3,
#                        shock = narrative_shock_PI,
#                        cumul_mult = FALSE,
#                        instr = NULL,
#                        exog_data = NULL,
#                        lags_exog = NULL,
#                        contemp_data = NULL,
#                        lags_criterion = NaN,
#                        max_lags = NaN,
#                        trend = 0,
#                        confint = 1.96,
#                        use_nw = TRUE,
#                        nw_lag = NULL,
#                        nw_prewhite = FALSE,
#                        adjust_se = FALSE,
#                        hor = 10,
#                        switching = RGDP,
#                        lag_switching = TRUE,
#                        use_logistic = FALSE,
#                        use_hp = T,
#                        lambda = 100,
#                        gamma = 3,
#                        num_cores = 1)
# # Plot
# plot(lp_nl_cycle) # No significant effects for asymmetry of shock... (good?)4
# summary(lp_nl_cycle)
# 
# ##personalize plot
# 
# ###############################################################################
# # 1) Convert s1_mean, s1_low, s1_up into data frames and label rows as variables
# ###############################################################################
# # Suppose you have 5 variables in this order:
# my_vars <- c("RGDP", "APITR", "PITB", "GOV", "DEBT")
# 
# # -- Regime 1: Mean, Low, Up
# df_mean_1 <- as.data.frame(lp_nl_cycle$irf_s1_mean)
# df_low_1  <- as.data.frame(lp_nl_cycle$irf_s1_low)
# df_up_1   <- as.data.frame(lp_nl_cycle$irf_s1_up)
# 
# # Add a column "Variable" that identifies each row
# df_mean_1$Variable <- my_vars
# df_low_1$Variable  <- my_vars
# df_up_1$Variable   <- my_vars
# 
# ###############################################################################
# # 2) Reshape from wide (columns = horizons) to long format
# ###############################################################################
# 
# mean_long_1 <- pivot_longer(
#   df_mean_1, 
#   cols = -Variable,
#   names_to = "Horizon", 
#   values_to = "mean"
# )
# 
# low_long_1 <- pivot_longer(
#   df_low_1,
#   cols = -Variable,
#   names_to = "Horizon",
#   values_to = "lower"
# )
# 
# up_long_1 <- pivot_longer(
#   df_up_1,
#   cols = -Variable,
#   names_to = "Horizon",
#   values_to = "upper"
# )
# 
# # Merge mean, low, up by (Variable, Horizon) & label "Regime 1"
# regime1_df <- mean_long_1 %>%
#   left_join(low_long_1, by = c("Variable", "Horizon")) %>%
#   left_join(up_long_1,  by = c("Variable", "Horizon")) %>%
#   mutate(Regime = "Regime 1 (Booms)")
# 
# ###############################################################################
# # 3) Repeat Steps (1) & (2) for Regime 2 (if it exists)
# ###############################################################################
# df_mean_2 <- as.data.frame(lp_nl_cycle$irf_s2_mean)
# df_low_2  <- as.data.frame(lp_nl_cycle$irf_s2_low)
# df_up_2   <- as.data.frame(lp_nl_cycle$irf_s2_up)
# 
# df_mean_2$Variable <- my_vars
# df_low_2$Variable  <- my_vars
# df_up_2$Variable   <- my_vars
# 
# mean_long_2 <- pivot_longer(df_mean_2, -Variable, names_to = "Horizon", values_to = "mean")
# low_long_2  <- pivot_longer(df_low_2,  -Variable, names_to = "Horizon", values_to = "lower")
# up_long_2   <- pivot_longer(df_up_2,   -Variable, names_to = "Horizon", values_to = "upper")
# 
# regime2_df <- mean_long_2 %>%
#   left_join(low_long_2, by = c("Variable", "Horizon")) %>%
#   left_join(up_long_2,  by = c("Variable", "Horizon")) %>%
#   mutate(Regime = "Regime 2 (Busts)")
# 
# ###############################################################################
# # 4) Combine both regimes & optionally convert "Horizon" to numeric
# ###############################################################################
# irf_all <- bind_rows(regime1_df, regime2_df)
# 
# # By default, "Horizon" might be "V1", "V2", or just "1", "2". 
# # If it's "V1", "V2", you can parse out the numeric part:
# # irf_all$Horizon <- as.numeric(gsub("V", "", irf_all$Horizon))
# 
# ###############################################################################
# # 5) Plot with ggplot2
# ###############################################################################
# 
# # 1) Convert horizon labels from "V1" ..."V10" to numeric 1..10
# irf_all <- irf_all %>%
#   mutate(Horizon_num = as.numeric(gsub("^V", "", Horizon))) 
# # Removes the "V" and converts to numeric
# 
# #Re order variabes
# irf_all$Variable <- factor(irf_all$Variable, levels = my_vars)
# 
# # 2) Plot using Horizon_num on the x-axis
# 
# ggplot(irf_all, aes(x = Horizon_num, y = mean)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.3) +
#   geom_line(color = "blue") +
#   facet_grid(Variable ~ Regime, scales = "free_y") +
#   labs(title = "Nonlinear LP-IV (narrative shock)", x = "Horizon", y = "Response") +
#   theme_minimal(base_size = 14)
# # Conclusion: No effect regarding business cycle!!
# ###############################################################################
# ####
# 
# 
# 
# ## 2. PITR with different inequality states
# 
# # 2.1. PITR with Gini pre tax
# # data starting from 1960 for gini
# data_anual_sub <- subset(data_anual, DATES >= 1960)
# #
# endog <- data_anual_sub[,c("RGDP","APITR" , "PITB", "GOV" , "DEBT", "Gini_pre_tax")]
# variable_shock_PI <- data_anual_sub[,c("APITR" )]
# narrative_shock_PI <- data_anual_sub[,c("T_PI")]
# #
# # Median
# # 1) Compute the median (ignoring missing data, if any)
# med_value <- median(endog$Gini_pre_tax, na.rm = TRUE)
# 
# # 2) Create a dummy that is 1 if above median, 0 otherwise
# endog$high_gini <- ifelse(endog$Gini_pre_tax > med_value, 1, 0)
# #
# 
# 
# switching_data <- endog$Gini_pre_tax
# #switching_data <- endog$high_gini
# endog$Gini_pre_tax <- NULL
# #narrative_shock_PI <- data_anual_sub[,c("T_PI")]
# lp_nl_gini_pre = lp_nl_iv(endog_data = endog,
#                        lags_endog_nl = 3,
#                        shock = narrative_shock_PI,
#                        cumul_mult = FALSE,
#                        instr = NULL,
#                        exog_data = NULL,
#                        lags_exog = NULL,
#                        contemp_data = NULL,
#                        lags_criterion = NaN,
#                        max_lags = NaN,
#                        trend = 0,
#                        confint = 1.96,
#                        use_nw = TRUE,
#                        nw_lag = NULL,
#                        nw_prewhite = FALSE,
#                        adjust_se = FALSE,
#                        hor = 10,
#                        switching = endog$Gini_pre_tax,
#                        lag_switching = TRUE,
#                        use_logistic = F,
#                        use_hp = T,
#                        lambda = 100,
#                        gamma = 3,
#                        num_cores = 1)
# # Plot
# plot(lp_nl_gini_pre) # There is difference between states. The first graph is low ineq, the second is high ineq
# summary(lp_nl_gini_pre)
# 
# # 2.2. PITR with Gini post tax
# # first drop gini pre tax and add gini post tax
# endog$Gini_pre <- NULL
# endog$Gini_post <- data[,c("Gini_post_tax" )]
# switching_data <- endog$Gini_post
# lp_nl_gini_pre = lp_nl_iv(endog_data = endog,
#                           lags_endog_nl = 4,
#                           shock = narrative_shock_PI,
#                           cumul_mult = FALSE,
#                           instr = NULL,
#                           exog_data = NULL,
#                           lags_exog = NULL,
#                           contemp_data = NULL,
#                           lags_criterion = NaN,
#                           max_lags = NaN,
#                           trend = 0,
#                           confint = 1.96,
#                           use_nw = TRUE,
#                           nw_lag = NULL,
#                           nw_prewhite = FALSE,
#                           adjust_se = FALSE,
#                           hor = 10,
#                           switching = switching_data,
#                           lag_switching = TRUE,
#                           use_logistic = TRUE,
#                           use_hp = T,
#                           lambda = 1600,
#                           gamma = 3,
#                           num_cores = 1)
# # Plot
# plot(lp_nl_gini_pre) # There is difference between states. The first graph is high ineq, the second is low ineq
# summary(lp_nl_gini_pre)
# 
# 
# # 2.3. PITR with 10-50 share pre tax
# # first drop gini post tax and add share 50-10
# endog$Gini_post <- NULL
# endog$Top_10_50_pre <- data[,c("Top_10_50_pre_tax" )]
# switching_data <- endog$Top_10_50_pre
# lp_nl_top_pre = lp_nl_iv(endog_data = endog,
#                           lags_endog_nl = 4,
#                           shock = narrative_shock_PI,
#                           cumul_mult = FALSE,
#                           instr = NULL,
#                           exog_data = NULL,
#                           lags_exog = NULL,
#                           contemp_data = NULL,
#                           lags_criterion = NaN,
#                           max_lags = NaN,
#                           trend = 0,
#                           confint = 1.96,
#                           use_nw = TRUE,
#                           nw_lag = NULL,
#                           nw_prewhite = FALSE,
#                           adjust_se = FALSE,
#                           hor = 10,
#                           switching = switching_data,
#                           lag_switching = TRUE,
#                           use_logistic = TRUE,
#                           use_hp = T,
#                           lambda = 1600,
#                           gamma = 3,
#                           num_cores = 1)
# # Plot
# plot(lp_nl_top_pre) # results consistent with gini
# # 2.4. PITR with 10-50 share post tax
# # first drop gini post tax and add share 50-10
# endog$Top_10_50_pre <- NULL
# endog$Top_10_50_post <- data[,c("Top_10_50_post_tax")]
# switching_data <- endog$Top_10_50_post
# lp_nl_top_post = lp_nl_iv(endog_data = endog,
#                          lags_endog_nl = 4,
#                          shock = narrative_shock_PI,
#                          cumul_mult = FALSE,
#                          instr = NULL,
#                          exog_data = NULL,
#                          lags_exog = NULL,
#                          contemp_data = NULL,
#                          lags_criterion = NaN,
#                          max_lags = NaN,
#                          trend = 0,
#                          confint = 1.96,
#                          use_nw = TRUE,
#                          nw_lag = NULL,
#                          nw_prewhite = FALSE,
#                          adjust_se = FALSE,
#                          hor = 10,
#                          switching = switching_data,
#                          lag_switching = TRUE,
#                          use_logistic = TRUE,
#                          use_hp = T,
#                          lambda = 1600,
#                          gamma = 3,
#                          num_cores = 1)
# # Plot
# plot(lp_nl_top_post) # Results consistent!
