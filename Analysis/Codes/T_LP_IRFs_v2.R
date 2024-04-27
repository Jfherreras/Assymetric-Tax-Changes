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

# Load data
Panel <- read_dta("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Output/Datasets/Panel.dta")

# 21: Country
# 2: Year
# 3. GDP
# 4. Corporateincometaxrate
# 5. Taxes_on_income_prof_k_corp_tax_rev_gdp
# 6. Taxes_on_income_prof_corp_tax_rev_gdp
# 7. Unemploymentrate
# 8. Unemploymentratemen
# 9. Unemployment.rate.women
# 10. gini
# 11. Pre_tax_national_income_Top_10.
# 12. Net_personal_wealth_Top_10.
# 13. Inflation
# 14. Interest_Rate
# 15. Net_operating_surplus_mix_income
# 16. CAPN
# 17. CAPN_ICT
# 18. CPNK
# 19. CPNK_ICT
# 20. wages_lc
# 21. wages_lc_c
# 22. wages_ppp
# 23. Growth
# 24. Country
# 25. tax_change
# 26. tax_cut
# 27. tax_inc 
# 28. d_tax_cut
# 29. d_tax_inc
# 30. profit_rate1
# 31. profit_rate2
# 32. profit_rate3
# 33. profit_rate4
# 36. output_gap
# 37. pos_gap
# 38. neg_gap

Panel$corporateincometaxrate <- -Panel$corporateincometaxrate # Changing the sign to get -1pp shock

###########
# (4/25) Tax -> (30/31/32/33)Profit -> (20/21/22)Wages -> (10/11/12) Inequity -> (23) growth | GAP
endog_data <- Panel[,c(24,2,4,30,20,10,23,3,37)]
exog_data <- Panel[,c(24,2,30,20,10)]
# Smoothing Function
results_panel_SF <-  lp_nl_panel(data_set = endog_data,
                                 endog_data = "growth", cumul_mult = TRUE,
                                 shock = "corporateincometaxrate", diff_shock = FALSE,
                                 
                                 panel_model = "within", panel_effect = "twoways",
                                 robust_cov = "vcovSCC", 
                                 c_exog_data = c("gini","profit_rate1"),
                                 switching = "gdp",
                                 lag_switching     = TRUE,      use_hp         = TRUE,
                                 lambda            = 6.25,      gamma          = 10,
                                 
                                 confint = 1.96,
                                 hor = 12)
plot(results_panel_SF)

# Dummy
# results_panel_D <-  lp_nl_panel(data_set = endog_data,
#                                 endog_data = "growth", cumul_mult = TRUE,
#                                 shock = "corporateincometaxrate", diff_shock = FALSE,
#                                 
#                                 panel_model = "within", panel_effect = "twoways",
#                                 robust_cov = "vcovSCC", 
#                                 c_exog_data = "gini",
#                                 # robust_maxlag = 2,
#                                 #
#                                 # use_logistic = FALSE,
#                                 # use_gmm = TRUE, gmm_model = "onestep",
#                                 use_logistic = FALSE,
#                                 switching = "pos_gap",
#                                 # lag_switching     = TRUE,      
#                                 # use_hp         = TRUE,
#                                 # lambda         = 6.25,      
#                                 # gamma          = 10,
#                                 
#                                 
#                                 
#                                 confint = 1.67,
#                                 hor = 12)
# plot(results_panel_D)
# 
# 
