********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Panel Regression
********************************************************************************

*Load data
use "$datasets/Panel.dta", clear

* Declare Panel
xtset country year

* simple regression
* tax cut
reghdfe growth tax_cut, abs(country year) // Not significant
reghdfe growth d_tax_cut, abs(country year) // Not significant

* tax increase
reghdfe growth tax_inc, abs(country year) // Significant
reghdfe growth d_tax_inc, abs(country year) // Significant

* Controlling by output gap
* tax cut
reghdfe growth tax_cut output_gap, abs(country year) cluster(country) // Not significant
reghdfe growth d_tax_cut output_gap, abs(country year) cluster(country) // Not significant

reghdfe growth tax_cut pos_gap, abs(country year) cluster(country) // Not significant
reghdfe growth tax_cut neg_gap, abs(country year) cluster(country) // Not significant
reghdfe growth d_tax_cut pos_gap, abs(country year) cluster(country) // Not significant
reghdfe growth d_tax_cut neg_gap, abs(country year) cluster(country) // Not significant

* tax increase 
reghdfe growth tax_inc output_gap, abs(country year) cluster(country) // Significant at 10
reghdfe growth d_tax_inc output_gap, abs(country year) cluster(country) // Significant


* Inequality
* Gini
* tax cut
reghdfe gini tax_cut, abs(country year) cluster(country) // Not significant

reghdfe gini d_tax_cut, abs(country year) cluster(country) // Not significant

* tax inc
reghdfe gini tax_inc, abs(country year) cluster(country) // Not significant

reghdfe gini d_tax_inc, abs(country year) cluster(country) // Not significant

* Top income
* tax cut
reghdfe pre_tax_national_income_top_10 tax_cut, abs(country year) cluster(country) // Not significant

reghdfe pre_tax_national_income_top_10 d_tax_cut, abs(country year) cluster(country) // Not significant

* tax increase
reghdfe pre_tax_national_income_top_10 tax_inc, abs(country year) cluster(country) // Not significant

reghdfe pre_tax_national_income_top_10 d_tax_inc, abs(country year) cluster(country) // Not significant


* Top wealth
* tax cut
reghdfe net_personal_wealth_top_10 tax_cut, abs(country year) cluster(country) // Not significant

reghdfe net_personal_wealth_top_10 d_tax_cut, abs(country year) cluster(country) // Not significant

* tax increase
reghdfe net_personal_wealth_top_10 tax_inc, abs(country year) cluster(country) // Not significant

reghdfe net_personal_wealth_top_10 d_tax_inc, abs(country year) cluster(country) // Not significant


** Unemployment
* tax cut
reghdfe unemploymentrate tax_cut, abs(country year) cluster(country) // Este da algo // Not significant

reghdfe unemploymentrate d_tax_cut, abs(country year) cluster(country) // Not significant


* tax increase
reghdfe unemploymentrate tax_inc, abs(country year) cluster(country) // Not significant

reghdfe unemploymentrate d_tax_inc, abs(country year) cluster(country) // Not significant


* Controlling by inflation
* tax cut
reghdfe unemploymentrate tax_cut inflation, abs(country year) cluster(country) // Este da algo // Not significant

reghdfe unemploymentrate d_tax_cut, abs(country year) cluster(country) // Not significant


*tax increase
reghdfe unemploymentrate tax_inc inflation, abs(country year) cluster(country) // Este da algo // Not significant

reghdfe unemploymentrate d_tax_inc, abs(country year) cluster(country) // Not significant


**** Tax reduction on profit rate -* didn't work
reghdfe profit_rate1 d_tax_cut output_gap, abs(country year) cluster(country)
reghdfe profit_rate2 d_tax_cut output_gap, abs(country year) cluster(country)
reghdfe profit_rate3 d_tax_cut output_gap, abs(country year) cluster(country)
reghdfe profit_rate4 tax_cut output_gap, abs(country year) cluster(country)
reghdfe net_operating_surplus_mix_income tax_cut output_gap, abs(country year) cluster(country)
reghdfe capn tax_cut output_gap, abs(country year) cluster(country)
reghdfe capn_ict tax_cut output_gap, abs(country year) cluster(country)
reghdfe cpnk tax_cut output_gap, abs(country year) cluster(country)
reghdfe cpnk_ict tax_cut output_gap, abs(country year) cluster(country)

**** IV strategy
ivreghdfe growth (gini = tax_inc), abs(country year) cluster(country)
local adja2: di %9.3f e(r2_a)
local f3 = round(e(F),.001)

******** Inflation effect
* simple effect
reghdfe 
iverg