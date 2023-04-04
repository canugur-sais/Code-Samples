
********* Re-Examining the Feldstein-Horioka Puzzle: A Broader Range of Countries, 1980-2019 ********

****** Get the Data and Perform an Initial Data Cleaning *****

* clear the workspace and set working directory
clear
cd "\\Client\C$\Users\cugur\OneDrive - Johns Hopkins University\SAIS\2023 Spring - DC\Programming\Feldstein Horioka"

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

use "Code\FH_cleandata.dta", clear

*** Part 1: the Original F-H Analysis Method using Averages Per Period ****

* first, let's create a matrix to store all the relevant regression outputs
mat AvgResults = J(12,7,.)
mat rownames AvgResults = g1p1 g1p2 g1p3 g1p4 g2p1 g2p2 g2p3 g2p4 g3p1 g3p2 g3p3 g3p4
mat colnames AvgResults = "Country Group" "Time Period" "Savings Coefficient" "Savings s.e." "Constant Coefficient" "Constant s.e." "R-squared"
matlist AvgResults

* now, let's run a loop for each country group and time period combination, putting relevant regression outputs into our results matrix
forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		local row = `i' + 4*(`j'-1)
		preserve
			keep if group`j' == 1
			keep if period`i' == 1
			bysort country: egen avg_investment = mean(investment)
			bysort country: egen avg_savings = mean(savings)
			reg avg_investment avg_savings
			mat R = r(table)
			mat AvgResults[`row',1] = `j' // country group
			mat AvgResults[`row',2] = `i' // time period
			mat AvgResults[`row',3] = R[1,1] // savings coef
			mat AvgResults[`row',4] = R[2,1] // savings s.e.
			mat AvgResults[`row',5] = R[1,2] // constant coef
			mat AvgResults[`row',6] = R[2,2] // constant s.e.
			mat AvgResults[`row',7] = e(r2) // r-squared
		restore
	}
}


* now let's put our results in an excel
putexcel set "Data Output\FHcoefficients.xls", sheet("Averages") replace
putexcel A1=matrix(AvgResults), names


*** Part 2: Panel Analysis, using non-Averaged Values ****

* first, let's create a matrix to store all the relevant regression outputs
mat RegResults = J(12,7,.)
mat rownames RegResults = g1p1 g1p2 g1p3 g1p4 g2p1 g2p2 g2p3 g2p4 g3p1 g3p2 g3p3 g3p4
mat colnames RegResults = "Country Group" "Time Period" "Savings Coefficient" "Savings s.e." "Constant Coefficient" "Constant s.e." "R-squared"
matlist RegResults

* now, let's run a loop for each country group and time period combination, putting relevant regression outputs into our results matrix
encode country, gen(countrycode)
xtset countrycode year

forvalues j = 1/3{ // specifying loops for country groups
	forvalues i = 1/4 { // specifying loops for time periods
		local row = `i' + 4*(`j'-1)
		preserve
			keep if group`j' == 1
			keep if period`i' == 1
			xtreg investment savings
			mat R = r(table)
			mat RegResults[`row',1] = `j' // country group
			mat RegResults[`row',2] = `i' // time period
			mat RegResults[`row',3] = R[1,1] // savings coef
			mat RegResults[`row',4] = R[2,1] // savings s.e.
			mat RegResults[`row',5] = R[1,2] // constant coef
			mat RegResults[`row',6] = R[2,2] // constant s.e.
			mat RegResults[`row',7] = e(r2_o) // r-squared
		restore
	}
}


* now let's put our results in an excel
putexcel set "Data Output\FHcoefficients.xls", sheet("Panel") modify
putexcel A1=matrix(RegResults), names


