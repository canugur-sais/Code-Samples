
********* Re-Examining the Feldstein-Horioka Puzzle: A Broader Range of Countries, 1980-2019 ********

****** Get the Data and Perform an Initial Data Cleaning *****

* clear the workspace and set working directory
clear
cd "\\Client\C$\Users\C:\Users\cugur\OneDrive - Johns Hopkins University\SAIS\2023 Spring - DC\Programming\Feldstein Horioka"

* import the desired Excel file
import delimited "Data Input\WEOOct2022all.xls"

* focus on the variables we need - country names, subject codes, and columns for each year
keep country weosubjectcode v* 

* rename the year columns to reflect actual year information
rename v* yr#, addnumber(1980)

* remove missing values
foreach v of varlist yr* {
	replace `v' = "" if `v' == "n/a"
}

* destring years
destring yr*, replace force


****** Focus on Savings and Investment, Reshape Years from Wide to Long, then Reshape to Get Columns for Savings and Investment ****** 

* focus on savings and investments
keep if weosubjectcode == "NGSD_NGDP" | weosubjectcode == "NID_NGDP"
rename weosubjectcode subject
replace subject = "savings" if subject == "NGSD_NGDP"
replace subject = "investment" if subject == "NID_NGDP"

* reshape years from wide to long, rename the new column
reshape long yr, i(country subject) j(year)
rename yr value

* reshape subject from long to wide, rename the new columns
reshape wide value, i(country year) j(subject) string
rename valuesavings savings
rename valueinvestment investment

* quick reordering of variables
order year country savings investment

***** Create Country Groups using Dummy Variables ****** 

* Group 1 = Feldstein-Horioka's original 16
gen group1 = 0
replace group1 = 1 if inlist(country, "Australia", "Austria", "Belgium", "Canada", "Denmark") ///
 | inlist(country, "Finland", "Germany", "Greece", "Ireland", "Italy", "Japan") ///
 | inlist(country, "Netherlands", "New Zealand", "Sweden", "United Kingdom", "United States")

* Group 2 = Group 1 + new OECD countries 
gen group2 = 0
replace group2 = 1 if group1 == 1 ///
 | inlist(country, "France", "Luxembourg","Norway", "Spain", "Switzerland") ///
 | inlist(country, "Chile", "Colombia", "Costa Rica", "Czech Republic", "Estonia", "Hungary", "Iceland", "Korea") ///
 | inlist(country, "Latvia", "Lithuania", "Mexico", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Türkiye")

* Group 3 = Group 2 + a selection of important emerging market economies
gen group3 = 0
replace group3 = 1 if group2 == 1 ///
 | inlist(country, "Argentina", "Brazil", "China", "Egypt", "India", "Indonesia", "Nigeria", "Philippines", "Russia") ///
 | inlist(country, "Saudi Arabia", "Singapore", "South Africa", "Thailand", "United Arab Emirates")
	
***** Create Time Period Dummy Variables ****** 

* Period 1: 1980-1989
gen period1 = 0
replace period1 = 1 if year>=1980 & year<=1989

* Period 2: 1990-1999
gen period2 = 0
replace period2 = 1 if year>=1990 & year<=1999

* Period 3: 2000-2009
gen period3 = 0
replace period3 = 1 if year>=2000 & year<=2009

* Period 4: 2010-2019
gen period4 = 0
replace period4 = 1 if year>=2010 & year<=2019

* some relabeling for tidiness
label data "Re-examining the Feldstein-Horioka Puzzle Using WEO's Savings and Investment Data"
label variable investment "Investment (% of GDP)"
label variable savings "Savings (% of GDP)"
label variable year "Year"
label variable group1 "FH16"
label variable group2 "FH16 + new OECD"
label variable group3 "FH16 + new OECD + EMEs"	
label variable period1 "1980-1989"
label variable period2 "1990-1999"
label variable period3 "2000-2009"
label variable period4 "2010-2019"

* save progress before running regressions
save "Code\FH_cleandata.dta", replace

	
************** Running Regressions for each Country Group and Period Combination ****************

clear
use "Code\FH_cleandata.dta"

*** Part 1: the Original F-H Analysis Method using Averages Per Period ****

forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		preserve
			keep if group`j' == 1
			keep if period`i' == 1
			bysort country: egen avg_investment = mean(investment)
			bysort country: egen avg_savings = mean(savings)
			reg avg_investment avg_savings
			mat avgG`j'P`i' = r(table)\ (e(r2),0)
		restore
	}
}


* now let's put our results in an excel, focusing the regression coefficients, standard errors and r-squared values *

putexcel set "Data Output\FHcoefficients.xls", sheet("Averages") replace
putexcel A1 = "Country Group" B1 = "Time Period" C1 = "Savings Coefficient" D1 = "Savings s.e." E1 = "Constant Coefficient" ///
	F1 = "Constant s.e." G1 = "R-squared"

forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		local xlsrow = 1 + `i' + 4*(`j'-1)
		putexcel A`xlsrow' = "Group `j'" B`xlsrow' = "Period `i'"
		putexcel C`xlsrow' = avgG`j'P`i'[1,1] D`xlsrow' = avgG`j'P`i'[2,1] E`xlsrow' = avgG`j'P`i'[1,2] F`xlsrow' = avgG`j'P`i'[2,2] G`xlsrow' = avgG`j'P`i'[10,1] 
	}
}


*** Part 2: Panel Analysis, using non-Averaged Values ****

encode country, gen(countrycode)
xtset countrycode year

forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		preserve
			keep if group`j' == 1
			keep if period`i' == 1
			xtreg investment savings
			mat regG`j'P`i' = r(table)\ (e(r2_o),0)
		restore
	}
}

* now let's put our results in an excel *

putexcel set "Data Output\FHcoefficients.xls", sheet("Panel") modify
putexcel A1 = "Country Group" B1 = "Time Period" C1 = "Savings Coefficient" D1 = "Savings s.e." E1 = "Constant Coefficient" ///
	F1 = "Constant s.e." G1 = "R-squared"

forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		local xlsrow = 1 + `i' + 4*(`j'-1)
		putexcel A`xlsrow' = "Group `j'" B`xlsrow' = "Period `i'"
		putexcel C`xlsrow' = regG`j'P`i'[1,1] D`xlsrow' = regG`j'P`i'[2,1] E`xlsrow' = regG`j'P`i'[1,2] F`xlsrow' = regG`j'P`i'[2,2] G`xlsrow' = regG`j'P`i'[10,1] 
	}
}

