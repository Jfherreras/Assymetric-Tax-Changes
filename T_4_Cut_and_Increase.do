********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Panel Regression on Growth (tax cuts and increases)
********************************************************************************

*Load data
use "$datasets/Panel.dta", clear

* Declare Panel
xtset country year

* wages divided by 100000 in order to the coefficient to appear
replace wages_lc_c = wages_lc_c/1000000

* Label variables
label var growth "Growth"
label var tax_change "Corporate Income Tax Change"
label var corporateincometaxrate "Corporate Income Tax"
label var profit_rate1 "Profit Rate"
label var wages_lc_c "Wages"
label var unemploymentrate "Unemplyoment Rate"
label var tax_cut "Tax cut"
label var tax_inc "Tax increase"
drop if profit_rate1 == .
drop if wages_lc_c == .
* clear eststo
eststo clear
* simple regression
**** on growth
******* TAX CUT
* #1 no effect on growth
* Col 1: simple no FE
eststo: reghdfe growth tax_cut, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 2: controlling by profits and wages no FE
eststo: reghdfe growth tax_cut profit_rate1 wages_lc_c unemploymentrate, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 3: simple country FE
eststo: reghdfe growth tax_cut, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 	
* Col 4: controlling by profits and wages
eststo: reghdfe growth tax_cut profit_rate1 wages_lc_c unemploymentrate, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 
* Col 5: simple time FE
eststo: reghdfe growth tax_cut, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$ 	
* Col 6: controlling by profits and wages time FE
eststo: reghdfe growth tax_cut profit_rate1 wages_lc_c unemploymentrate, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$	
* Col 7: simple country year FE
eststo: reghdfe growth tax_cut, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
* Col 8: controlling by profits and wages country year FE
eststo: reghdfe growth tax_cut profit_rate1 wages_lc_c unemploymentrate, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
	
* Output table
esttab using "$tabfolder/Table_4_Panel_Regression_Growth_tax_cut.tex", replace f se  ///
stats(N r2 country year, fmt("%6.0f" "%5.3gc") labels("Observations" "R-squared" "Country F.E." "Time F.E.")) b("%05.4f")  star(* 0.1 ** 0.05 *** 0.01) nocons nomtitle ///
label booktab  ///
prehead("\begin{tabular}{lcccccccc} \\ \hline ") /// 
posthead("& \multicolumn{8}{c}{Growth} \\ \hline &  &  &  &  &  &  &  &  &  \\") ///
prefoot("\arrayrulecolor{black!10}\midrule") ///
postfoot("\arrayrulecolor{black}\bottomrule" "\multicolumn{9}{c}{*** p$<$0.01, ** p$<$0.05, * p$<$0.1}" "\end{tabular}")


******* TAX INCREASE
eststo clear
* #1 no effect on growth
* Col 1: simple no FE
eststo: reghdfe growth tax_inc, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 2: controlling by profits and wages no FE
eststo: reghdfe growth tax_inc profit_rate1 wages_lc_c unemploymentrate, noabs nocons
	estadd local country $\times$ 
	estadd local year $\times$ 
* Col 3: simple country FE
eststo: reghdfe growth tax_inc, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 	
* Col 4: controlling by profits and wages
eststo: reghdfe growth tax_inc profit_rate1 wages_lc_c unemploymentrate, abs(country) nocons
	estadd local country $\checkmark$ 
	estadd local year $\times$ 
* Col 5: simple time FE
eststo: reghdfe growth tax_inc, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$ 	
* Col 6: controlling by profits and wages time FE
eststo: reghdfe growth tax_inc profit_rate1 wages_lc_c unemploymentrate, abs(year) nocons
	estadd local country $\times$ 
	estadd local year $\checkmark$	
* Col 7: simple country year FE
eststo: reghdfe growth tax_inc, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
* Col 8: controlling by profits and wages country year FE
eststo: reghdfe growth tax_inc profit_rate1 wages_lc_c unemploymentrate, abs(country year) nocons
	estadd local country $\checkmark$ 
	estadd local year $\checkmark$	
	
* Output table
esttab using "$tabfolder/Table_5_Panel_Regression_Growth_tax_inc.tex", replace f se  ///
stats(N r2 country year, fmt("%6.0f" "%5.3gc") labels("Observations" "R-squared" "Country F.E." "Time F.E.")) b("%05.4f")  star(* 0.1 ** 0.05 *** 0.01) nocons nomtitle ///
label booktab  ///
prehead("\begin{tabular}{lcccccccc} \\ \hline ") /// 
posthead("& \multicolumn{8}{c}{Growth} \\ \hline &  &  &  &  &  &  &  &  &  \\") ///
prefoot("\arrayrulecolor{black!10}\midrule") ///
postfoot("\arrayrulecolor{black}\bottomrule" "\multicolumn{9}{c}{*** p$<$0.01, ** p$<$0.05, * p$<$0.1}" "\end{tabular}")
