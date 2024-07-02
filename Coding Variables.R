library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)

############################### Encoding Variabeles ###########################
df <- read_excel("Documents/Term 3 /Social Economics/ESS1.xlsx")

df <- df %>%
  mutate(pplfair = case_when(
    pplfair == "Most people try to be fair" ~ "10",
    pplfair == "Most people try to take advantage of me" ~ "0",
    pplfair %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ pplfair
  ))

df <- df %>%
  mutate(pplhlp = case_when(
    pplhlp == "People mostly try to be helpful" ~ "10",
    pplhlp == "People mostly look out for themselves" ~ "0",
    pplhlp %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ pplhlp
  ))

df <- df %>%
  mutate(ppltrst = case_when(
    ppltrst == "Most people can be trusted" ~ "10",
    ppltrst == "You can't be too careful" ~ "0",
    ppltrst %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ ppltrst
  ))

df <- df %>%
  mutate(rdpol = case_when(
    rdpol == "More than 3 hours" ~ "7",
    rdpol == "More than 2,5 hours, up to 3 hours" ~ "6",
    rdpol == "More than 2 hours, up to 2,5 hours" ~ "5",
    rdpol == "More than 1,5 hours, up to 2 hours" ~ "4",
    rdpol == "More than 1 hour, up to 1,5 hours" ~ "3",
    rdpol == "0,5 hour to 1 hour" ~ "2",
    rdpol == "Less than 0,5 hour" ~ "1",
    rdpol == "No time at all" ~ "0",
    rdpol %in% c("Don't know", "Not applicable") ~ NA_character_,
    TRUE ~ rdpol
  ))

df <- df %>%
  mutate(lrscale = case_when(
    lrscale == "Right" ~ "10",
    lrscale == "Left" ~ "0",
    lrscale %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ lrscale
  ))

df <- df %>%
  mutate(mmbprty = case_when(
    mmbprty == "Yes" ~ "1",
    mmbprty == "No" ~ "0",
    mmbprty %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ mmbprty
  ))


df <- df %>%
  mutate(poldcs = case_when(
    poldcs == "Very difficult" ~ "1",
    poldcs == "Difficult" ~ "2",
    poldcs == "Neither difficult nor easy" ~ "3",
    poldcs == "Easy" ~ "4",
    poldcs == "Very easy" ~ "5",
    poldcs %in% c("Don't know", "Not applicable") ~ NA_character_,
    TRUE ~ poldcs
  ))

df <- df %>%
  mutate(polintr = case_when(
    polintr == "Very interested" ~ "1",
    polintr == "Quite interested" ~ "2",
    polintr == "Hardly interested" ~ "3",
    polintr == "Not at all interested" ~ "4",
    polintr == "Don't know" ~ NA_character_,
    TRUE ~ polintr
  ))

df <- df %>%
  mutate(prtyban = case_when(
    prtyban == "Agree strongly" ~ "1",
    prtyban == "Agree" ~ "2",
    prtyban == "Neither agree nor disagree" ~ "3",
    prtyban == "Disagree" ~ "4",
    prtyban == "Disagree strongly" ~ "5",
    prtyban %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ prtyban
  ))

df <- df %>%
  mutate(stfdem = case_when(
    stfdem == "Extremely satisfied" ~ "10",
    stfdem == "Extremely dissatisfied" ~ "0",
    stfdem %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ stfdem
  ))

df <- df %>%
  mutate(stfgov = case_when(
    stfgov == "Extremely satisfied" ~ "10",
    stfgov == "Extremely dissatisfied" ~ "0",
    stfgov %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ stfgov
  ))

df <- df %>%
  mutate(stflife = case_when(
    stflife == "Extremely satisfied" ~ "10",
    stflife == "Extremely dissatisfied" ~ "0",
    stflife %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ stflife
  ))

df <- df %>%
  mutate(trstep = case_when(
    trstep == "Complete trust" ~ "10",
    trstep == "No trust at all" ~ "0",
    trstep %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstep
  ))

df <- df %>%
  mutate(trstlgl = case_when(
    trstlgl == "Complete trust" ~ "10",
    trstlgl == "No trust at all" ~ "0",
    trstlgl %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstlgl
  ))

df <- df %>%
  mutate(trstplc = case_when(
    trstplc == "Complete trust" ~ "10",
    trstplc == "No trust at all" ~ "0",
    trstplc %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstplc
  ))

df <- df %>%
  mutate(trstplt = case_when(
    trstplt == "Complete trust" ~ "10",
    trstplt == "No trust at all" ~ "0",
    trstplt %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstplt
  ))

df <- df %>%
  mutate(trstprl = case_when(
    trstprl == "Complete trust" ~ "10",
    trstprl == "No trust at all" ~ "0",
    trstprl %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstprl
  ))

df <- df %>%
  mutate(trstun = case_when(
    trstun == "Complete trust" ~ "10",
    trstun == "No trust at all" ~ "0",
    trstun %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ trstun
  ))

df <- df %>%
  mutate(imsmetn = case_when(
    imsmetn == "Allow many to come and live here" ~ "1",
    imsmetn == "Allow some" ~ "2",
    imsmetn == "Allow a few" ~ "3",
    imsmetn == "Allow none" ~ "4",
    imsmetn %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ imsmetn
  ))

df <- df %>%
  mutate(imdfetn = case_when(
    imdfetn == "Allow many to come and live here" ~ "1",
    imdfetn == "Allow some" ~ "2",
    imdfetn == "Allow a few" ~ "3",
    imdfetn == "Allow none" ~ "4",
    imdfetn %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ imdfetn
  ))

df <- df %>%
  mutate(impcntr = case_when(
    impcntr == "Allow many to come and live here" ~ "1",
    impcntr == "Allow some" ~ "2",
    impcntr == "Allow a few" ~ "3",
    impcntr == "Allow none" ~ "4",
    impcntr %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ impcntr
  ))

df <- df %>%
  mutate(imueclt = case_when(
    imueclt == "Cultural life enriched" ~ "10",
    imueclt == "Cultural life undermined" ~ "0",
    imueclt %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ imueclt
  ))

df <- df %>%
  mutate(imwbcnt = case_when(
    imwbcnt == "Better place to live" ~ "10",
    imwbcnt == "Worse place to live" ~ "0",
    imwbcnt %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ imwbcnt
  ))

df <- df %>%
  mutate(aesfdrk = case_when(
    aesfdrk == "Very safe" ~ "1",
    aesfdrk == "Safe" ~ "2",
    aesfdrk == "Unsafe" ~ "3",
    aesfdrk == "Very unsafe" ~ "4",
    aesfdrk %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ aesfdrk
  ))

df <- df %>%
  mutate(blgetmg = case_when(
    blgetmg == "Yes" ~ "1",
    blgetmg == "No" ~ "0",
    blgetmg %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ blgetmg
  ))

df <- df %>%
  mutate(brncntr = case_when(
    brncntr == "Yes" ~ "1",
    brncntr == "No" ~ "0",
    brncntr %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ brncntr
  ))

df <- df %>%
  mutate(cntbrth = case_when(
    cntbrth == "66" ~ NA_character_,
    cntbrth == "99" ~ NA_character_,
    TRUE ~ cntbrth
  ))

df <- df %>%
  mutate(ctzcntr = case_when(
    ctzcntr == "Yes" ~ "1",
    ctzcntr == "No" ~ "0",
    ctzcntr %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ ctzcntr
  ))

df <- df %>%
  mutate(ctzship = case_when(
    ctzship == "66" ~ NA_character_,
    ctzship == "99" ~ NA_character_,
    TRUE ~ ctzship
  ))

df <- df %>%
  mutate(dscrgrp = case_when(
    dscrgrp == "Yes" ~ "1",
    dscrgrp == "No" ~ "0",
    dscrgrp %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ dscrgrp
  ))

df <- df %>%
  mutate(facntr = case_when(
    facntr == "Yes" ~ "1",
    facntr == "No" ~ "0",
    facntr %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ facntr
  ))

df <- df %>%
  mutate(happy = case_when(
    happy == "Extremely happy" ~ "10",
    happy == "Extremely unhappy" ~ "0",
    happy %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ happy
  ))

df <- df %>%
  mutate(health = case_when(
    health == "Very good" ~ "1",
    health == "Good" ~ "2",
    health == "Fair" ~ "3",
    health == "Bad" ~ "4",
    health == "Very bad" ~ "5",
    health %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ health
  ))

df <- df %>%
  mutate(hlthhmp = case_when(
    hlthhmp == "Yes a lot" ~ "1",
    hlthhmp == "Yes to some extent" ~ "2",
    hlthhmp == "No" ~ "3",
    hlthhmp %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ hlthhmp
  ))

df <- df %>%
  mutate(mocntr = case_when(
    mocntr == "Yes" ~ "1",
    mocntr == "No" ~ "0",
    mocntr %in% c("Don't know", "Refusal") ~ NA_character_,
    TRUE ~ mocntr
  ))

df <- df %>%
  mutate(gndr = case_when(
    gndr == "Male" ~ "0",
    gndr == "Female" ~ "1",
    gndr == "No answer" ~ NA_character_,
    TRUE ~ gndr
  ))

df <- df %>%
  mutate(yrbrn = case_when(
    yrbrn %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ yrbrn
  ))

df <- df %>%
  mutate(agea = case_when(
    agea %in% c("123", "Not available") ~ NA_character_,
    TRUE ~ agea
  ))

df <- df %>%
  mutate(chldhhe = case_when(
    chldhhe == "No" ~ "0",
    chldhhe == "Yes" ~ "1",
    chldhhe  %in% c("Don't know", "Refusal", "No answer", "Not applicable") ~ NA_character_,
    TRUE ~ chldhhe
  ))

df <- df %>%
  mutate(chldhm = case_when(
    chldhm == "Does not" ~ "0",
    chldhm == "Respondent lives with children at household grid" ~ "1",
    chldhm == "Not available" ~ NA_character_,
    TRUE ~ chldhm
  ))

df <- df %>%
  mutate(edulvla = case_when(
    edulvla == "Not possible to harmonise into 5-level ISCED" ~ "0",
    edulvla == "Less than lower secondary education (ISCED 0-1)" ~ "1",
    edulvla == "Lower secondary education completed (ISCED 2)" ~ "2",
    edulvla == "Upper secondary education completed (ISCED 3)" ~ "3",
    edulvla == "Post-secondary non-tertiary education completed (ISCED 4)" ~ "4",
    edulvla == "Tertiary education completed (ISCED 5-6)" ~ "5",
    edulvla %in% c("Don't know", "Refusal", "No answer", "Other") ~ NA_character_,
    TRUE ~ edulvla
  ))

df <- df %>%
  mutate(edulvlfa = case_when(
    edulvlfa == "Not possible to harmonise into 5-level ISCED" ~ "0",
    edulvlfa == "Less than lower secondary education (ISCED 0-1)" ~ "1",
    edulvlfa == "Lower secondary education completed (ISCED 2)" ~ "2",
    edulvlfa == "Upper secondary education completed (ISCED 3)" ~ "3",
    edulvlfa == "Post-secondary non-tertiary education completed (ISCED 4)" ~ "4",
    edulvlfa == "Tertiary education completed (ISCED 5-6)" ~ "5",
    edulvlfa %in% c("Don't know", "Refusal", "No answer", "Other") ~ NA_character_,
    TRUE ~ edulvlfa
  ))

df <- df %>%
  mutate(edulvlma = case_when(
    edulvlma == "Not possible to harmonise into 5-level ISCED" ~ "0",
    edulvlma == "Less than lower secondary education (ISCED 0-1)" ~ "1",
    edulvlma == "Lower secondary education completed (ISCED 2)" ~ "2",
    edulvlma == "Upper secondary education completed (ISCED 3)" ~ "3",
    edulvlma == "Post-secondary non-tertiary education completed (ISCED 4)" ~ "4",
    edulvlma == "Tertiary education completed (ISCED 5-6)" ~ "5",
    edulvlma %in% c("Don't know", "Refusal", "No answer", "Other") ~ NA_character_,
    TRUE ~ edulvlma
  ))

df <- df %>%
  mutate(edulvlpa = case_when(
    edulvlpa == "Not possible to harmonise into 5-level ISCED" ~ "0",
    edulvlpa == "Less than lower secondary education (ISCED 0-1)" ~ "1",
    edulvlpa == "Lower secondary education completed (ISCED 2)" ~ "2",
    edulvlpa == "Upper secondary education completed (ISCED 3)" ~ "3",
    edulvlpa == "Post-secondary non-tertiary education completed (ISCED 4)" ~ "4",
    edulvlpa == "Tertiary education completed (ISCED 5-6)" ~ "5",
    edulvlpa %in% c("Don't know", "Refusal", "No answer", "Other", "Not applicable") ~ NA_character_,
    TRUE ~ edulvlpa
  ))

df <- df %>%
  mutate(eduyrs = case_when(
    eduyrs %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ eduyrs
  ))

df <- df %>%
  mutate(hinctnt = case_when(
    hinctnt == "J" ~ "1",
    hinctnt == "R" ~ "2",
    hinctnt == "C" ~ "3",
    hinctnt == "M" ~ "4",
    hinctnt == "F" ~ "5",
    hinctnt == "S" ~ "6",
    hinctnt == "K" ~ "7",
    hinctnt == "P" ~ "8",
    hinctnt == "D" ~ "9",
    hinctnt == "H" ~ "10",
    hinctnt == "U" ~ "11",
    hinctnt == "N" ~ "12",
    hinctnt %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ hinctnt
  ))

df <- df %>%
  mutate(impsafe = case_when(
    impsafe == "Very much like me" ~ "1",
    impsafe == "Like me" ~ "2",
    impsafe == "Somewhat like me" ~ "3",
    impsafe == "A little like me" ~ "4",
    impsafe == "Not like me" ~ "5",
    impsafe == "Not like me at all" ~ "6",
    impsafe %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ impsafe
  ))

df <- df %>%
  mutate(ipgdtim = case_when(
    ipgdtim == "Very much like me" ~ "1",
    ipgdtim == "Like me" ~ "2",
    ipgdtim == "Somewhat like me" ~ "3",
    ipgdtim == "A little like me" ~ "4",
    ipgdtim == "Not like me" ~ "5",
    ipgdtim == "Not like me at all" ~ "6",
    ipgdtim %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ ipgdtim
  ))

df <- df %>%
  mutate(ipstrgv = case_when(
    ipstrgv == "Very much like me" ~ "1",
    ipstrgv == "Like me" ~ "2",
    ipstrgv == "Somewhat like me" ~ "3",
    ipstrgv == "A little like me" ~ "4",
    ipstrgv == "Not like me" ~ "5",
    ipstrgv == "Not like me at all" ~ "6",
    ipstrgv %in% c("Don't know", "Refusal", "No answer") ~ NA_character_,
    TRUE ~ ipstrgv
  ))

write.xlsx(df, file = "~/Documents/Term 3 /Social Economics/ess_edited.xlsx")
