** below is the code used to average the variables **

********************************** Collapsing ********************************

import excel "/Users/averyatencio/Documents/Term 3 /Social Economics/ess_edited.xlsx", sheet("Sheet 1") firstrow clear


collapse (mean) pplfair-ipstrgv, by(region year)

