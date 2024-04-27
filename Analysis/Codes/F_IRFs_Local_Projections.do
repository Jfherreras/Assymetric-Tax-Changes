********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Local Projections
********************************************************************************

clear all

set more 1
set matsize 1000
set mem 400m

* load data
use "$datasets/Panel.dta", clear
import excel "$data\US_DATA.xlsx", sheet("FRED Graph") firstrow clear
gen t = _n

/*

bys country: gen t = _n
keep if country == 6
*/
egen time = group(Year), label
tsset Year, y
/*
tsset time, y*/
ac EF_TAX, lags(10)
pac EF_TAX, lags(10)

* Setup fpr IRF plotting
foreach var in UNEM GINI INFLATION GROWTH {
	
	quietly gen b`var'=.
	quietly gen up90b`var' =.
	quietly gen lo90b`var' =.
	
}

gen h = t-1

local p = 2
local q = 1
local s = 0

global schock EF_TAX

forvalues i = 0/24 {
	
	
	foreach var in UNEM GINI INFLATION GROWTH {
		
		newey F`i'.`var' L(`s'/`p').$schock L(`q'/`p').UNEM L(`q'/`p').GINI L(`q'/`p').INFLATION L(`q'/`p').GROWTH, lag(`=`i' + 1')
		*newey F`i'.`var' L(0).$schock L(1).UNEM L(1).GINI L(1).INFLATION L(1).GROWTH ,lag(10)
		
		gen b`var'h`i'=_b[$shock]
		
		boottest $schock, boottype(wild) stat(t) l(90) reps(500) seed(123) // nograph
		
		matrix se`var'h`i' = r(CI)
		
		quietly replace b`var' = b`var'h`i' if h==`i'
		quietly replace up90b`var' = se`var'h`i'[1,2] if h==`i'
		quietly replace lo90b`var' = se`var'h`i'[1,1] if h==`i'
		
	}
	
}

set graphics off

foreach var in  UNEM GINI INFLATION GROWTH {
	local labtext: variable label `var'
	tw (rarea up90b`var' lo90b`var' h, clw(medthin medthin)) ///
	(scatter b`var' h, c(l) clp(l) ms(i) clc(black) mc(black) clw(medthick)) if h <= 24, ///
	xtitle("") ytitle(`labtext') legend(off) name(vargk_`var',replace)
}

set graphics on

graph combine vargk_unemploymentrate vargk_net_personal_wealth_top_10 vargk_inflation vargk_growth, title(Figure 2)