################################################################################
######################### Assymetrical Taxation  ###############################
##################### Juan Felipe Herrera Sarmiento ############################
################################################################################

# This code produces: Non-linear Local Projections for tax cuts shocks
################################################################################

library(haven)
Panel <- read_dta("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Output/Datasets/Panel.dta")

# Load package
library(vctrs)
library(lpirfs)
library(gridExtra)
library(ggpubr)
library(rlang)

# Load data
#Database <- read_excel("G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/Analysis/Raw/US_DATA.xlsx")
# Load (endogenous) data

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
# Panel["gdp"] = log(Panel$gdp)
# (4/25) Tax -> (30/31/32/33)Profit -> (20/21/22)Wages -> (10/11/12) Inequity -> (23) growth | GAP
endog_data <- Panel[,c(24,2,25,30,20,10,23,3,37)]
# 4 -> 15/27/28/29/30 -> 10/11/12 -> 20 | 33
# 1.
# endog_data <- Panel[,c(21,2,4,15,10,20,3)]
# endog_data <- Dataset[,c(1,2,3,4,7,12,13,14)]
# endog_data <- Panel[,c(21,2,20,4,7,10,12,13,14,33)]
# endog_data <- Panel[,c(21,2,14,20,4,10,7,12,13,33)]
x11()
adf.test(Panel$corporateincometaxrate) 
Panel$corporateincometaxrate <- -Panel$corporateincometaxrate 
######
## IRFs on GROWTH
# Smoothing Function
 results_panel_growth_SF <-  lp_nl_panel(data_set = endog_data,
                               endog_data = "growth", cumul_mult = TRUE,
                               shock = "tax_change", diff_shock = TRUE,
                               
                               panel_model = "within", panel_effect = "twoways",
                               robust_cov = "vcovSCC", 
                              # robust_maxlag = 2,
                              #
                              # use_logistic = FALSE,
                              # use_gmm = TRUE, gmm_model = "onestep",
                              switching = "gdp",
                              lag_switching     = TRUE,      use_hp         = TRUE,
                              lambda            = 6.25,      gamma          = 10,
                              
                              confint = 1.67,
                              hor = 12)
 plot(results_panel_growth_SF)
 plots = plot_nl(results_panel_growth_SF)
 
 # Using dummy
 results_panel_growth_D <-  lp_nl_panel(data_set = endog_data,
                               endog_data = "growth", cumul_mult = TRUE,
                               shock = "tax_change", diff_shock = TRUE,
                               
                               panel_model = "within", panel_effect = "twoways",
                               robust_cov = "vcovSCC", 
                               # robust_maxlag = 2,
                               #
                               # use_logistic = FALSE,
                               # use_gmm = TRUE, gmm_model = "onestep",
                               use_logistic = FALSE,
                               switching = "pos_gap",
                               # lag_switching     = TRUE,      
                               # use_hp         = TRUE,
                               # lambda            = 6.25,      
                               # gamma          = 10,
                               
                               
                               
                               confint = 1.67,
                               hor = 12)
 plot(results_panel_growth_D)
 plots = plot_nl(results_panel_growth_D)
 
 
 ## IRFs on GINI
 # Smoothing Function
 results_panel_gini_SF <-  lp_nl_panel(data_set = endog_data,
                                         endog_data = "gini", cumul_mult = TRUE,
                                         shock = "tax_change", diff_shock = TRUE,
                                         
                                         panel_model = "within", panel_effect = "twoways",
                                         robust_cov = "vcovSCC", 
                                         # robust_maxlag = 2,
                                         #
                                         # use_logistic = FALSE,
                                         # use_gmm = TRUE, gmm_model = "onestep",
                                         switching = "gdp",
                                         lag_switching     = TRUE,      use_hp         = TRUE,
                                         lambda            = 6.25,      gamma          = 10,
                                         
                                         confint = 1.67,
                                         hor = 12)
 plot(results_panel_gini_SF)
 
 
 # Using dummy
 
 results_panel_gini_D <-  lp_nl_panel(data_set = endog_data,
                                        endog_data = "gini", cumul_mult = TRUE,
                                        shock = "tax_change", diff_shock = TRUE,
                                        
                                        panel_model = "within", panel_effect = "twoways",
                                        robust_cov = "vcovSCC", 
                                        # robust_maxlag = 2,
                                        #
                                        # use_logistic = FALSE,
                                        # use_gmm = TRUE, gmm_model = "onestep",
                                        use_logistic = FALSE,
                                        switching = "pos_gap",
                                        # lag_switching     = TRUE,      
                                        # use_hp         = TRUE,
                                        # lambda            = 6.25,      
                                        # gamma          = 10,
                                        
                                        
                                        
                                        confint = 1.67,
                                        hor = 12)
 plot(results_panel_gini_D)
 
 ## IRFs on Top Income
 # (4/25) Tax -> (30/31/32/33)Profit -> (20/21/22)Wages -> (10/11/12) Inequity -> (23) growth | GAP
 endog_data <- Panel[,c(24,2,25,30,20,11,23,3,37)]
 
 # Smoothing Function
 results_panel_top_income_SF <-  lp_nl_panel(data_set = endog_data,
                                       endog_data = "pre_tax_national_income_top_10", cumul_mult = TRUE,
                                       shock = "tax_change", diff_shock = TRUE,
                                       
                                       panel_model = "within", panel_effect = "twoways",
                                       robust_cov = "vcovSCC", 
                                       # robust_maxlag = 2,
                                       #
                                       # use_logistic = FALSE,
                                       # use_gmm = TRUE, gmm_model = "onestep",
                                       switching = "gdp",
                                       lag_switching     = TRUE,      use_hp         = TRUE,
                                       lambda            = 6.25,      gamma          = 10,
                                       
                                       confint = 1.67,
                                       hor = 12)
 plot(results_panel_top_income_SF)
 
 
 # Using dummy
 
 results_panel_top_income_D <-  lp_nl_panel(data_set = endog_data,
                                      endog_data = "pre_tax_national_income_top_10", cumul_mult = TRUE,
                                      shock = "tax_change", diff_shock = TRUE,
                                      
                                      panel_model = "within", panel_effect = "twoways",
                                      robust_cov = "vcovSCC", 
                                      # robust_maxlag = 2,
                                      #
                                      # use_logistic = FALSE,
                                      # use_gmm = TRUE, gmm_model = "onestep",
                                      use_logistic = FALSE,
                                      switching = "pos_gap",
                                      # lag_switching     = TRUE,      
                                      # use_hp         = TRUE,
                                      # lambda            = 6.25,      
                                      # gamma          = 10,
                                      
                                      
                                      
                                      confint = 1.67,
                                      hor = 12)
 plot(results_panel_top_income_D)
 
 ## IRFs on Top Wealth
 # (4/25) Tax -> (30/31/32/33)Profit -> (20/21/22)Wages -> (10/11/12) Inequity -> (23) growth | GAP
 endog_data <- Panel[,c(24,2,25,30,20,12,23,3,37)]
 
 # Smoothing Function
 results_panel_top_wealth_SF <-  lp_nl_panel(data_set = endog_data,
                                             endog_data = "net_personal_wealth_top_10", cumul_mult = TRUE,
                                             shock = "tax_change", diff_shock = TRUE,
                                             
                                             panel_model = "within", panel_effect = "twoways",
                                             robust_cov = "vcovSCC", 
                                             # robust_maxlag = 2,
                                             #
                                             # use_logistic = FALSE,
                                             # use_gmm = TRUE, gmm_model = "onestep",
                                             switching = "gdp",
                                             lag_switching     = TRUE,      use_hp         = TRUE,
                                             lambda            = 6.25,      gamma          = 10,
                                             
                                             confint = 1.67,
                                             hor = 12)
 plot(results_panel_top_wealth_SF)
 
 
 # Using dummy
 
 results_panel_top_wealth_D <-  lp_nl_panel(data_set = endog_data,
                                            endog_data = "net_personal_wealth_top_10", cumul_mult = TRUE,
                                            shock = "tax_change", diff_shock = TRUE,
                                            
                                            panel_model = "within", panel_effect = "twoways",
                                            robust_cov = "vcovSCC", 
                                            # robust_maxlag = 2,
                                            #
                                            # use_logistic = FALSE,
                                            # use_gmm = TRUE, gmm_model = "onestep",
                                            use_logistic = FALSE,
                                            switching = "pos_gap",
                                            # lag_switching     = TRUE,      
                                            # use_hp         = TRUE,
                                            # lambda         = 6.25,      
                                            # gamma          = 10,
                                            
                                            
                                            
                                            confint = 1.67,
                                            hor = 12)
 plot(results_panel_top_wealth_D)
 
 
 ## IRFs on Wages
 # (4/25) Tax -> (30/31/32/33)Profit -> (20/21/22)Wages -> (10/11/12) Inequity -> (23) growth | GAP
 endog_data <- Panel[,c(24,2,25,30,20,12,23,3,37)]
 
 #
 results_panel_top_wealth_D <-  lp_nl_panel(data_set = endog_data,
                               endog_data = "net_personal_wealth_top_10", cumul_mult = TRUE,
                               shock = "tax_change", diff_shock = TRUE,
                                            
                               panel_model = "within", panel_effect = "twoways",
                               robust_cov = "vcovSCC", 
                                            # robust_maxlag = 2,
                                            #
                                            # use_logistic = FALSE,
                                            # use_gmm = TRUE, gmm_model = "onestep",
                               use_logistic = FALSE,
                               switching = "pos_gap",
                                            # lag_switching     = TRUE,      
                                            # use_hp         = TRUE,
                                            # lambda         = 6.25,      
                                            # gamma          = 10,
                                            
                                            
                                            
                              confint = 1.67,
                              hor = 12)
 
 plot(results_panel_top_wealth_D)
 
 ###########
 ### Insanity Test
 endog_data <- Panel[,c(24,2,4,30,20,10,23,3,37)]
 exog_data <- Panel[,c(24,2,30,20,10)]
 # Smoothing Function
 results_panel_SF <-  lp_nl_panel(data_set = endog_data,
                                  endog_data = "growth", cumul_mult = TRUE,
                                  shock = "corporateincometaxrate", diff_shock = FALSE,
                                             
                                  panel_model = "within", panel_effect = "twoways",
                                  robust_cov = "vcovSCC", 
                                             # robust_maxlag = 2,
                                             #
                                             # use_logistic = FALSE,
                                             # use_gmm = TRUE, gmm_model = "onestep",
                                  # c_fd_exog_data = colnames(exog_data)[c(seq(3,5),11)],
                                  c_exog_data = "gini",
                                  switching = "gdp",
                                  lag_switching     = TRUE,      use_hp         = TRUE,
                                  lambda            = 6.25,      gamma          = 10,
                                             
                                  confint = 1.96,
                                  hor = 12)
 plot(results_panel_SF)
 
 # Dummy
 results_panel_D <-  lp_nl_panel(data_set = endog_data,
                               endog_data = "growth", cumul_mult = TRUE,
                               shock = "corporateincometaxrate", diff_shock = FALSE,
                               
                               panel_model = "within", panel_effect = "twoways",
                               robust_cov = "vcovSCC", 
                               c_exog_data = "gini",
                               # robust_maxlag = 2,
                               #
                               # use_logistic = FALSE,
                               # use_gmm = TRUE, gmm_model = "onestep",
                               use_logistic = FALSE,
                               switching = "pos_gap",
                               # lag_switching     = TRUE,      
                               # use_hp         = TRUE,
                               # lambda         = 6.25,      
                               # gamma          = 10,
                               
                               
                               
                               confint = 1.67,
                               hor = 12)
 plot(results_panel_D)
 
 #############
 # Make to list to save all plots
 combine_plots <- list()
 # Save nonlinear plots for expansion period
 combine_plots[[1]] <- plots$gg_s1[[1]]
 # combine_plots[[2]] <- plots$gg_s1[[3]]
 
 # Save nonlinear plots for recession period
 combine_plots[[2]] <- plots$gg_s2[[1]]
 # combine_plots[[4]] <- plots$gg_s2[[3]]
 
 nlin_plots_all     <- sapply(combine_plots, ggplotGrob)
 combine_plots_all <- marrangeGrob(nlin_plots_all, nrow = 2, ncol = 3, top = NULL)
 
 # Show all plots
 combine_plots_all
 