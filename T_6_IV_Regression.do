********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: IV regression
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
label var net_personal_wealth_top_10 "Inequality"
label var pre_tax_national_income_top_10 "Inequality"
drop if profit_rate1 == .
drop if wages_lc_c == .
drop if gini ==.
drop if corporateincometaxrate ==.
local regr  "gini"

* clear eststo
eststo clear
* simple regression

*Col1
reghdfe growth `regr', absorb(country year) cluster(country)
local adja1: di %9.3f e(r2_a)

eststo: reghdfe growth `regr', absorb(country year) cluster(country)
 estadd local adj `adja1'
 estadd local CFE $\checkmark$
 estadd local TFE $\checkmark$
 estadd local Inst "-"
 estadd local Estim "OLS"
 estadd local KBF " "


*Col2
reghdfe growth corporateincometaxrate, absorb(country year) cluster(country)
local adja2: di %9.3f e(r2_a)
local f3 = round(e(F),.001)

eststo: reghdfe growth corporateincometaxrate, absorb(country year) cluster(country)
 estadd local adj `adja2'
 estadd local CFE $\checkmark$
 estadd local TFE $\times$
 estadd local Inst "-"
 estadd local Estim "OLS"
 estadd local KBF " "

*Col 4
ivreghdfe growth (`regr'=corporateincometaxrate), absorb(country year) cluster(country)
local adja4: di %9.3f e(r2_a)
local rkwa1 = round(e(widstat),.001)

eststo: ivreghdfe growth (`regr'=corporateincometaxrate), absorb(country year) cluster(country)
 estadd local adj `adja4'
 estadd local CFE $\checkmark$
 estadd local TFE $\times$
 estadd local Inst "CITR"
 estadd local Estim "IV"
 estadd local KBF "-"

*Col 6
reghdfe `regr' corporateincometaxrate, absorb(country year) cluster(country)
local adja6: di %9.3f e(r2_a)

eststo: reghdfe `regr' corporateincometaxrate, absorb(country year) cluster(country)
 estadd local adj `adja6'
 estadd local CFE $\checkmark$
 estadd local TFE $\times$
 estadd local Inst "-"
 estadd local Estim "OLS"
 estadd local KBF `rkwa1'

 
esttab using "$tabfolder/Table_6_IV_Regression", replace ///
stats (N r2 adj Estim CFE TFE Inst KBF, fmt (%12.0fc 3 3 3 3 3 3 3 3) labels("Observations" "R-squared" "Adjusted R-squared" "Estimation" "Country F.E" "Year F.E." "Instrument" "Kleibergen-Paap F")) star(* 0.1 ** 0.05 *** 0.01) se nocons nonotes nomtitle  ///
label booktab ///
prehead("\begin{tabular}{lccccc} \\ \hline") ///
posthead(" & \multicolumn{3}{c}{Growth} &\multicolumn{1}{c}{Inequality} \\ \cline{2-4} & OLS &  \multicolumn{1}{c}{Reduced form} & \multicolumn{1}{c}{IV}& \multicolumn{1}{c}{First Stage} \\ \hline & & & & & &  \\") ///
prefoot("\arrayrulecolor{black!10}\midrule") ///
postfoot("\arrayrulecolor{black}\bottomrule" "\end{tabular}")
/*


**** on growth
* #1 no effect on growth
/*
* Col 1: simple no FE
eststo: reghdfe growth corporateincometaxrate, noabs nocons 
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 2: controlling by profits and wages no FE
eststo: ivreghdfe growth (`regr'=corporateincometaxrate) profit_rate1 wages_lc_c unemploymentrate, noabs nocons cluster(country)
	local adja4: di %9.3f e(r2_a)
	estadd local country $\times$ 
	estadd local year $\times$ */
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