clear

* Set working directory
cd "C:\Users\mvc32\OneDrive - University of Cambridge\Documents\Climate_meningitis_belt\Disease_data"

* Import data from "weeklyincidencemissing.dta"
use "weeklyincidencemissing.dta"

* Import data from Excel file "filtered_shapefile.xlsx" and create district_country variable
import excel "filtered_shapefile.xlsx", sheet("Sheet1") firstrow clear
gen district_country = ADM2_NAME + " " + ADM0_NAME
sort district_country

* Save the dataset as "DRC_polio_weekly.dta"
save "DRC_polio_weekly.dta", replace

clear

* Use "weeklyincidencemissing.dta" and modify district_country values
use "weeklyincidencemissing.dta"
replace district_country = subinstr(district_country, "Democratic Republic of the Congo", "Democratic Republic Of The Congo", .)
replace district_country = "3E Arrondissement Central African Republic" if district_country == "3e Arrondissement Central African Republic"
... (additional replace statements)

* Save the modified dataset
sort district_country
save "weeklyincidencemissing.dta", replace

clear

* Use "DRC_polio_weekly.dta" and prepare for merging
use "DRC_polio_weekly.dta"
sort district_country

* Drop duplicate district_country values
duplicates drop district_country, force
sort district_country

* Merge with "weeklyincidencemissing.dta" and drop non-matching observations
merge district_country using "weeklyincidencemissing.dta"
drop if _merge == 1
drop if _merge == 2

* Save the merged dataset
save "weeklyincidencepoliomerge.dta", replace

* Export the dataset to Excel
export excel using "Weeklyincidencepoliomergetest.xlsx", firstrow(variables) replace

clear

* Use "annualincidencemissing.dta" and modify district_country values
use "annualincidencemissing.dta"
replace district_country = subinstr(district_country, "Democratic Republic of the Congo", "Democratic Republic Of The Congo", .)
... (additional replace statements)

* Save the modified dataset
sort district_country
save "annualincidencemissing.dta", replace

clear

* Use "DRC_polio_weekly.dta" and prepare for merging
use "DRC_polio_weekly.dta"
sort district_country

* Merge with "annualincidencemissing.dta" and drop non-matching observations
merge district_country using "annualincidencemissing.dta"
drop if _merge == 1
drop if _merge == 2

* Save the merged dataset
save "annualincidencemissingmerged.dta", replace

* Export the dataset to Excel
export excel using "annualincidencepoliomergetest.xlsx", firstrow(variables) replace