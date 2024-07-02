*********************************** DiD ***************************************

import excel "/Users/averyatencio/Documents/Term 3 /Social Economics/ess_avg.xlsx", sheet("Sheet1") firstrow clear


********************************************************************************
******************************** Immediate zone ********************************
********************************************************************************

drop if inlist(region, "North East", "North West", "Northern Ireland", "Scotland", "Wales", "West Midlands", "Yorkshire and The Humber")

gen treat = (region == "London")

gen postt = (year >= 2006)

gen treatpost = treat*postt

********************************* Wellbeing ***********************************

reg health treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt

outreg2 using "Zonal Immediate DiD.doc", replace title("Table XX") label addtext("Controls", "Yes")

reg stflife treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "Zonal Immediate DiD.doc", append title("Table XX") label addtext("Controls", "Yes")

reg impsafe treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "Zonal Immediate DiD.doc", append title("Table XX") label addtext("Controls", "Yes")

****************************** Attitudes toward out groups ********************

reg imueclt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "attitude1.doc", replace title("Table attidude1") keep (treat postt treatpost) 

reg imwbcnt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "attitude1.doc", append title("Table attidude1") keep (treat postt treatpost) 

****************************** Institutional Trust ****************************

reg ipstrgv treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust1.doc", replace title("Table trust1") keep (treat postt treatpost) 

reg polintr treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust1.doc", append title("Table trust1") keep (treat postt treatpost) 

reg stfgov treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust1.doc", append title("Table trust1") keep (treat postt treatpost) 

reg stfdem treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust1.doc", append title("Table trust1") keep (treat postt treatpost) 

reg trstplt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust2.doc", replace title("Table trust2") keep (treat postt treatpost) 

reg trstprl treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust2.doc", append title("Table trust2") keep (treat postt treatpost) 

reg trstun treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust2.doc", append title("Table trust2") keep (treat postt treatpost) 

reg trstplc treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust2.doc", append title("Table trust2") keep (treat postt treatpost) 


********************************************************************************
******************************** Whole country *********************************
********************************************************************************

import excel "/Users/averyatencio/Documents/Term 3 /Social Economics/ess_avg.xlsx", sheet("Sheet1") firstrow clear

gen treat = (region == "London")

gen postt = (year >= 2006)

gen treatpost = treat*postt

********************************* Wellbeing ***********************************
reg health treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "Zonal whole DiD.doc", replace title("Table XX") label addtext("Controls", "Yes")

reg stflife treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "Zonal whole DiD.doc", append title("Table XX") label addtext("Controls", "Yes")

reg impsafe treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "Zonal whole DiD.doc", append title("Table XX") label addtext("Controls", "Yes")

****************************** Attitudes toward out groups ********************

reg imueclt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "attitude2.doc", replace title("Table attidude2") keep (treat postt treatpost) 

reg imwbcnt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "attitude2.doc", append title("Table attidude2") keep (treat postt treatpost) 

****************************** Institutional Trust ****************************

reg ipstrgv treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust3.doc", replace title("Table trust3") keep (treat postt treatpost) 

reg polintr treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust3.doc", append title("Table trust3") keep (treat postt treatpost) 

reg stfgov treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust3.doc", append title("Table trust3") keep (treat postt treatpost) 

reg stfdem treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust3.doc", append title("Table trust3") keep (treat postt treatpost) 

reg trstplt treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust4.doc", replace title("Table trust4") keep (treat postt treatpost) 

reg trstprl treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust4.doc", append title("Table trust4") keep (treat postt treatpost) 

reg trstun treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust4.doc", append title("Table trust4") keep (treat postt treatpost) 

reg trstplc treat postt treatpost agea gndr eduyrs ctzcntr edulvlfa chldhm dscrgrp hinctnt 

outreg2 using "trust4.doc", append title("Table trust4") keep (treat postt treatpost) 
