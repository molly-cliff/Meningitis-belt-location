//this section is done post Intersection cleaning in step 3
clear
* Import data from Excel file "filtered_intersections30_weekly.xlsx" and save it as Stata data file
import excel "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data\filtered_intersections30_weekly.xlsx", sheet("Sheet1") firstrow
sort district_country
save "filtered_intersections30_weekly.dta", replace

clear

* Import data from Excel file "filtered_intersections50_weekly.xlsx" and save it as Stata data file
import excel "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data\filtered_intersections50_weekly.xlsx", sheet("Sheet1") firstrow
sort district_country
save "filtered_intersections50_weekly.dta", replace

clear

* Import data from Excel file "Epidemic YN.xlsx" and prepare for merging
import excel using "Epidemic YN.xlsx", firstrow clear
sort district_country

* Merge with the dataset "weekly_inc_easymerge.dta"
merge district_country using "weekly_inc_easymerge.dta"
tab epidemic

* Save the merged dataset with updated epidemic variable
save "weekly_inc_easymerge.dta", replace
sort district_country
use "weekly_inc_easymerge.dta"
drop _merge
sort district_country

* Merge with the dataset "filtered_intersections50_weekly.dta"
merge district_country using "filtered_intersections50_weekly.dta"

* Update epidemic variable based on merge results
drop weekly_epidemics
replace epidemic = 1 if _merge == 3
drop _merge
save "weekly_inc_easymerge.dta", replace

clear

* Use the updated dataset "weekly_inc_easymerge.dta"
use "weekly_inc_easymerge.dta"
sort district_country

* Merge with the dataset "filtered_intersections30_weekly.dta"
merge district_country using "filtered_intersections30_weekly.dta"

* Update epidemic variable based on merge results
replace epidemic = 1 if _merge == 3
drop _merge
save "weekly_inc_totalmerge.dta", replace

* Export the final dataset to Excel, excluding unnecessary variables
export excel using "totalweeklyepidemic", firstrow(variables) replace

* Note: Additional code for dropping unnecessary variables
drop Epidemic_YN
drop A
drop YEAR
drop DISTRICT
drop POP
drop Attack_Rate
drop dup

save "weekly_inc_totalmerge.dta", replace
export excel using "totalweeklyepidemic", firstrow(variables) replace
