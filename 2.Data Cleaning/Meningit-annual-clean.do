clear

* Set working directory
cd "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data"

* Import data from Excel file "Annual_Cumul_Dist2.xlsx" and create district_country variable
import excel "Annual_Cumul_Dist2.xlsx", sheet("Sheet1") firstrow clear
gen district_country = District_proper + " " + COUNTRY
gen district_country2 = District_proper + " " + COUNTRY

* Check district_country values
tab district_country

* Sort and save the dataset
sort district_country
save "annual_inc_total2.dta", replace

clear

* Use the saved dataset and replace certain values in district_country
use "annual_inc_total2.dta"
replace district_country = subinstr(district_country, "Democratic Republic of the Congo", "Democratic Republic Of The Congo", .)
sort district_country

* Save the modified dataset
save "annual_inc_total3.dta", replace

clear

* Use dataset "DRC_polio_weekly.dta" and prepare for merging
use "DRC_polio_weekly.dta"
sort district_country

* Drop duplicate district_country values
duplicates drop district_country, force
sort district_country

* Merge with dataset "annual_inc_total3.dta" and drop non-matching observations
merge district_country using "annual_inc_total3.dta"
drop if _merge == 1
drop if _merge == 2

* Save the merged dataset
save "annualreducedincidence.dta", replace

* Export the dataset to Excel
export excel using "annualreducedincidence.xlsx", firstrow(variables) replace
//this section is done post Intersection cleaning in step 3
* Use dataset "filtered_intersections50_annual2.dta" and prepare for merging
use "filtered_intersections50_annual2.dta"
sort district_country

* Merge with dataset "Epidemic YN.xlsx" and update epidemic variable
import excel using "Epidemic YN.xlsx", firstrow clear
sort district_country
merge district_country using "filtered_intersections50_annual2.dta"
replace epidemic = 1 if _merge == 3
drop _merge

* Save the merged dataset
save "annual_inc_totalmerge2.dta", replace

* Drop unnecessary variables
drop YEAR
drop DISTRICT
drop POP
drop Attack_Rate
drop dup
drop Annual_cumulative_cases

* Export the dataset to Excel
export excel using "totalannualepidemic2.xlsx", firstrow(variables) replace