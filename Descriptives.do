/********* Social Economics: Final Paper - Descriptives********* 
Avery Atencio
Michael Nahhas
Maria Paula Salcedo
*/

clear all
set more off, perm

capture log close
log using "Descriptives_LogFile", replace

**# Setup
cd "C:\Users\apait\Documents\BSE\Term 3\Social Economics\"
import excel "combined_London.xlsx", sheet("Sheet1") firstrow

**Var creation
gen treat=1 if region=="London"
replace treat=0 if region!="London"

gen postt=1 if year>=2005
replace postt=0 if year<2005

gen treatpost=1 if treat==1 & postt==1
replace treatpost=0 if treatpost!=1

**#Locals
local wellbeing "health stflife impsafe"
local attitudes "imueclt imwbcnt"
local institutional "ipstrgv polintr stfgov stfdem trstplt trstprl trstun trstplc"
local covariates "age gndr edulvla yrbrn brncntr edulvlfa edulvlma edulvlpa eduyrs hinctnt chldhhe chldhm facntr mocntr blgetmg dscrgrp hlthhmp ctzcntr mmbprty"

**#Labels
*Covariates 
label var age "Age"
label var gndr "Gender"
label var edulvla "Highest Education Lavel"
label var yrbrn "Year or birth"
label var brncntr "Born in country"
label var edulvlfa "Father's Highest Education Level"
label var edulvlma "Mother's Highest Education Level"
label var edulvlpa "Partner's Highest Education Level"
label var eduyrs "Years of Education"
label var hinctnt "Household's net income"
label var chldhhe "Ever Had Children Living in House"
label var chldhm "Children Living at Home"
label var facntr "Father Born in Country"
label var mocntr "Mother Born in Country"
label var blgetmg " Belong to Minority Ethnic Group"
label var dscrgrp  "Member of Discriminated Against Group"
label var hlthhmp  "Hampered by Illness/Disability/Infirmity/Mental Problem"
label var ctzcntr  "Citizenship"
label var mmbprty "Member of Political Party"

*Indep vars
label var health "Health"
label var stflife "Life Satisfaction"
label var impsafe "Security Importance"
label var imueclt "Cultural Immigrant Impact"
label var imwbcnt "Immigrants effect on country"
label var ipstrgv "Importance Gov Strength and Safety"
label var polintr "Interest in Politics"
label var stfgov "National Gov Satisfaction"
label var stfdem "Democracy Satisfaction"
label var trstplt "Trust Politicians"
label var trstprl "Trust Parliament"
label var trstun "Trust UN"
label var trstplc "Trust Police"


**# Tables
*Dep variables
dtable, by(treatpost, tests total) continuous(`wellbeing' `attitudes' `institutional', stat(mean)) sample(, stat(frequency)) nformat(%9.3f mean) export("C:\Users\apait\Documents\BSE\Term 3\Social Economics.docx", replace) 


*Covariates
dtable, by(treatpost, tests total) continuous( `covariates', stat(mean)) sample(, stat(frequency)) nformat(%9.3f mean) export("C:\Users\apait\Documents\BSE\Term 3\Social Economics.docx", replace) 



*************************************************************



**#Robustness checks 
clear all
set more off, perm

**# Setup
import excel "C:\Users\apait\Documents\BSE\Term 3\Social Economics\ess_avg.xlsx", sheet("Sheet1") firstrow

**Var creation
gen treat=1 if region=="London"
replace treat=0 if region!="London"

gen postt=1 if year>=2005
replace postt=0 if year<2005

gen treatpost=1 if treat==1 & postt==1
replace treatpost=0 if treatpost!=1

**#Locals
local wellbeing "health stflife impsafe"
local attitudes "imueclt imwbcnt"
local institutional "ipstrgv polintr stfgov stfdem trstplt trstprl trstun trstplc"
local covariates "age gndr edulvla yrbrn brncntr edulvlfa edulvlma edulvlpa eduyrs hinctnt chldhhe chldhm facntr mocntr blgetmg dscrgrp hlthhmp ctzcntr mmbprty"


**#Labels
*Covariates 
label var age "Age"
label var gndr "Gender"
label var edulvla "Highest Education Lavel"
label var yrbrn "Year or birth"
label var brncntr "Born in country"
label var edulvlfa "Father's Highest Education Level"
label var edulvlma "Mother's Highest Education Level"
label var edulvlpa "Partner's Highest Education Level"
label var eduyrs "Years of Education"
label var hinctnt "Household's net income"
label var chldhhe "Ever Had Children Living in House"
label var chldhm "Children Living at Home"
label var facntr "Father Born in Country"
label var mocntr "Mother Born in Country"
label var blgetmg " Belong to Minority Ethnic Group"
label var dscrgrp  "Member of Discriminated Against Group"
label var hlthhmp  "Hampered by Illness/Disability/Infirmity/Mental Problem"
label var ctzcntr  "Citizenship"
label var mmbprty "Member of Political Party"

*Indep vars
label var health "Health"
label var stflife "Life Satisfaction"
label var impsafe "Security Importance"
label var imueclt "Cultural Immigrant Impact"
label var imwbcnt "Immigrants effect on country"
label var ipstrgv "Importance Gov Strength and Safety"
label var polintr "Interest in Politics"
label var stfgov "National Gov Satisfaction"
label var stfdem "Democracy Satisfaction"
label var trstplt "Trust Politicians"
label var trstprl "Trust Parliament"
label var trstun "Trust UN"
label var trstplc "Trust Police"


**# Tables
*Dep variables
dtable, by(treatpost, tests total) continuous(`wellbeing' `attitudes' `institutional', stat(mean)) sample(, stat(frequency)) nformat(%9.3f mean) export("C:\Users\apait\Documents\BSE\Term 3\Social Economics.docx", replace) 

*Covariates
dtable, by(treatpost, tests total) continuous(`covariates', stat(mean)) sample(, stat(frequency)) nformat(%9.3f mean) export("C:\Users\apait\Documents\BSE\Term 3\Social Economics.docx", replace) 

log close
