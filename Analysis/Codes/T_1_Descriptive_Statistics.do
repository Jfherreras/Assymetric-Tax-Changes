********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************
* This do-file produces: Descriptive Statistics
********************************************************************************

* load data
use "$datasets/Panel.dta", clear

replace wages_lc_c = wages_lc_c/1000000

mat define A=J(6,9,.)

pwcorr growth unemploymentrate gini corporate* wages_lc_c profit_rate1, st(0.01)

matpwcorr growth unemploymentrate gini corporate* wages_lc_c profit_rate1

matlist corr
forvalues i = 1/6 {
	forvalues k = 1/6 {
		mat A[`k',`i'] = corr[`k',`i']
	}
}
* Triangular
forvalues i = 1/6 {
    forvalues h = 1/3 {
			mat A[`i',`i'+`h'] = .
	} 
}

mat A[1,5] = .
mat A[1,6] = .
mat A[2,6] = .

* Mean, SD and observations
summ growth
mat A[1,7] = r(mean)
mat A[1,8] = r(sd)
mat A[1,9] = r(N)

summ unemploymentrate
mat A[2,7] = r(mean)
mat A[2,8] = r(sd)
mat A[2,9] = r(N)

summ gini
mat A[3,7] = r(mean)
mat A[3,8] = r(sd)
mat A[3,9] = r(N)

summ corporate*
mat A[4,7] = r(mean)
mat A[4,8] = r(sd)
mat A[4,9] = r(N)

summ wages_lc_c
mat A[5,7] = r(mean)
mat A[5,8] = r(sd)
mat A[5,9] = r(N)

summ profit_rate1
mat A[6,7] = r(mean)
mat A[6,8] = r(sd)
mat A[6,9] = r(N)

matlist A
frmttable using "$tabfolder/Table_1_Descriptive_Statistics.tex", tex fragment replace asymbol(*,**,***) ///
statmat(A) sdec(2,2,2,2,2,2,2,2,0) ///
rtitles("(1): Output Growth"\"(2): Unemployment Rate"\"(3): Gini's Coefficient'"\"(4): CITR"\"(5): Constant Wages (divided by 1000)"\"(6): Profit Rate") ctitles("","\multicolumn{6}{c}{\textbf{Correlations}}","\vline \multicolumn{3}{c}{\textbf{Statistics}}",""\"","(1)","(2)","(3)","(4)","(5)","(6)","Mean","SD","Obs")