
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
