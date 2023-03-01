********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Datasets
********************************************************************************

* Load data
*import delimited "$data\Database.csv", numericcols(4 6 7 8 9 10 11 12 13 14 15 ) clear 
import delimited "$data\Database.csv", numericcols(4(1)24) clear 
* numeric country for the panel set
encode country, gen(ID)
drop country
rename ID country

* Declare Panel
xtset country year

* Tax cuts
gen tax_change = D.corporateincometaxrate
gen tax_cut = abs(tax_change) if tax_change < 0
gen tax_inc = tax_change if tax_change > 0

replace tax_cut = 0 if tax_cut == .
replace tax_inc = 0 if tax_inc == .
* Dummies for tax cut
gen d_tax_cut = (tax_cut > 0)
gen d_tax_inc = (tax_inc > 0)

* drop 1999, 2020-22 because of lacking years
drop if year == 1999
drop if year == 2020
drop if year == 2021
drop if year == 2022

* Profit rate
gen profit_rate1 = 100*(net_operating_surplus_mix_income/capn)
gen profit_rate2 = 100*(net_operating_surplus_mix_income/capn_ict)
gen profit_rate3 = 100*(net_operating_surplus_mix_income/cpnk)
gen profit_rate4 = 100*(net_operating_surplus_mix_income/cpnk_ict)


* Output gap
tsfilter hp cycle_dem_countries = gdp, smooth(6.25) trend(hp_trend_dem_countries)
gen output_gap = (gdp/hp_trend_dem_countries-1)*100

* Positive and negative output gap dummies
gen pos_gap = (output_gap > 0)
gen neg_gap = (output_gap < 0)

* Save dataset
save "$datasets/Panel.dta", replace

* High inequality regimen
drop if gini == .
bys country: egen gini_m = median(gini)
gen high_ineq = (gini > gini_m)
keep country gini_m high_ineq
save "$datasets/High_Ineq.dta", replace

use "$datasets/Panel.dta", clear
merge country using "$datasets/High_Ineq.dta"

sort country year
* Save dataset
save "$datasets/Panel.dta", replace