********************************************************************************
*				Writing Sample: Asymmetries in Corporate Tax Rates
*									2023
*						Juan Felipe Herrera Sarmiento
*					MPhil Economics - University of Oxford
********************************************************************************

	cls
	clear all
	macro drop _all
	set more off
 	graph set window fontface "Garamond"
	
	ssc install ivreghdfe
	ssc install reghdfe
	ssc install outreg
	ssc install unique
	ssc install poi2hdfe
	ssc install ereplace
	ssc install tabout
	ssc install moremata
	ssc install binscatter
	ssc install outreg2
	ssc install edfreg
*	ssc install eststo
*	ssc install frmttable
	ssc install lassopack
	ssc install rforest
	ssc install pdslasso
	ssc install weakivtest
	ssc install estout
	ssc install multidensity
	ssc install matpwcorr, replace
	ssc install ivreg2, replace
*	ssc install nardl
	ssc install ftools
*Housekeeping
	

*-Laptop
if "`c(username)'"=="juanf" {

	global rootdir "G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation"
	cd "${rootdir}"
} 
*-Desktop
else if "`c(username)'"=="USER" {

	global rootdir "G:/Mi unidad/1 Drive Escritorio/Investigaciones/Assymetric impacts of income taxation/"
	cd "${rootdir}"

} 



	
*Globals for folders
	
	global dofile "${rootdir}/Analysis/Codes"
	global data="${rootdir}/Analysis/Raw"
	global datasets="${rootdir}/Analysis/output/Datasets" 
	global figfolder="${rootdir}/Analysis/output/Figures"
	global tabfolder="${rootdir}/Analysis/output/Tables"



	//////////////////////////////////////////////
	///		Do-files
	//
	//////////////////////////////////////////////
	
	**Table 1: Descriptive Statistics
	do "$dofile/T_1_Descriptive_Statistics"
	
	**Table 2: Panel Regression on Growth
	do "$dofile/T_2_Panel_Regression_Growth"
	
	**Table 3: Panel Regression on Inequality
	do "$dofile/T_3_Panel_Regression_Inequality"
	
	**Tables 4 and 5: Panel Regressions using Cuts and Increases
	do "$dofile/T_4_Cut_and_Increase"
	
	**Table 6: IV Regression
	do "$dofile/T_6_IV_Regression"
		
