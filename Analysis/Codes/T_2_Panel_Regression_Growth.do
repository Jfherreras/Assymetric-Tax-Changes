********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Panel Regression on Growth
********************************************************************************

*Load data
use "$datasets/Panel.dta", clear

* Declare Panel
xtset country year

* wages divided by 100 in order to the coefficient to appear
replace wages_lc_c = wages_lc_c/1000000

* Label variables
label var growth "Growth"
label var tax_change "Corporate Income Tax Change"
label var corporateincometaxrate "Corporate Income Tax"
label var profit_rate1 "Profit Rate"
label var wages_lc_c "Wages"
label var unemploymentrate "Unemplyoment Rate"
drop if profit_rate1 == .
drop if wages_lc_c == .
* clear eststo
eststo clear
* simple regression
**** on growth
* #1 no effect on growth
* Col 1: simple no FE
eststo: reghdfe growth corporateincometaxrate, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 2: controlling by profits and wages no FE
eststo: reghdfe growth corporateincometaxrate profit_rate1 wages_lc_c unemploymentrate, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 3: simple country FE
eststo: reghdfe growth corporateincometaxrate, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 	
* Col 4: controlling by profits and wages
eststo: reghdfe growth corporateincometaxrate profit_rate1 wages_lc_c unemploymentrate, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 
* Col 5: simple time FE
eststo: reghdfe growth corporateincometaxrate, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$ 	
* Col 6: controlling by profits and wages time FE
eststo: reghdfe growth corporateincometaxrate profit_rate1 wages_lc_c unemploymentrate, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$	
* Col 7: simple country year FE
eststo: reghdfe growth corporateincometaxrate, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
* Col 8: controlling by profits and wages country year FE
eststo: reghdfe growth corporateincometaxrate profit_rate1 wages_lc_c unemploymentrate, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
	
* Output table
esttab using "$tabfolder/Table_2_Panel_Regression_Growth.tex", replace f se  ///
stats(N r2 country year, fmt("%6.0f" "%5.3gc") labels("Observations" "R-squared" "Country F.E." "Time F.E.")) b("%05.4f")  star(* 0.1 ** 0.05 *** 0.01) nocons nomtitle ///
label booktab  ///
prehead("\begin{tabular}{lcccccccc} \\ \hline ") /// 
posthead("& \multicolumn{8}{c}{Growth} \\ \hline &  &  &  &  &  &  &  &  &  \\") ///
prefoot("\arrayrulecolor{black!10}\midrule") ///
postfoot("\arrayrulecolor{black}\bottomrule" "\multicolumn{9}{c}{*** p$<$0.01, ** p$<$0.05, * p$<$0.1}" "\end{tabular}")
/*

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