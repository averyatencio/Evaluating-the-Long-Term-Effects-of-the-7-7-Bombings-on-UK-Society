library(panelView)
library(gsynth)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidysynth)
library(purrr)

########################### generating ATT graphs #############################

ess_att <- read_excel("~/Documents/Term 3 /Social Economics/ess_att.xlsx")

## people help ##

synth1 <- 
  df %>% 
  synthetic_control(outcome = pplhlp, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth1, time_window = 2002:2008)

## people can be trusted ##

synth2 <- 
  df %>% 
  synthetic_control(outcome = ppltrst, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth2, time_window = 2002:2008)

## radio listening weekly ##

synth3 <- 
  df %>% 
  synthetic_control(outcome = rdpol, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth3, time_window = 2002:2008)

## left right scale ##

synth19 <- 
  df %>% 
  synthetic_control(outcome = lrscale, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth19, time_window = 2002:2008)

## intrested in politics ##

synth4 <- 
  df %>% 
  synthetic_control(outcome = polintr, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth4, time_window = 2002:2008)

## satisfied with democracy ##

synth5 <- 
  df %>% 
  synthetic_control(outcome = stfdem, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth5, time_window = 2002:2008)

## satisfied with government ##

synth6 <- 
  df %>% 
  synthetic_control(outcome = stfgov, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth6, time_window = 2002:2008)

## satisfied with life ##

synth7 <- 
  df %>% 
  synthetic_control(outcome = stflife, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth7, time_window = 2002:2008)

## trust in police ##

synth8 <- 
  df %>% 
  synthetic_control(outcome = trstplc, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth8, time_window = 2002:2008)

## trust in politicians ##

synth9 <- 
  df %>% 
  synthetic_control(outcome = trstplt, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth9, time_window = 2002:2008)

## trust in parliment ##

synth10 <- 
  df %>% 
  synthetic_control(outcome = trstprl, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth10, time_window = 2002:2008)

## trust in UN ##

synth11 <- 
  df %>% 
  synthetic_control(outcome = trstun, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth11, time_window = 2002:2008)

## country's cultural life undermined by immigrants ##

synth12 <- 
  df %>% 
  synthetic_control(outcome = imueclt, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth12, time_window = 2002:2008)

## immigrants make country worse or better ##

synth13 <- 
  df %>% 
  synthetic_control(outcome = imwbcnt, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth13, time_window = 2002:2008)

## feeling safe walking after dark ##

synth14 <- 
  df %>% 
  synthetic_control(outcome = aesfdrk, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth14, time_window = 2002:2008)

## health ##

synth15 <- 
  df %>% 
  synthetic_control(outcome = health, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth15, time_window = 2002:2008)

## important to live in safe and secure surroundings ##

synth16 <- 
  df %>% 
  synthetic_control(outcome = impsafe, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth16, time_window = 2002:2008)

## important to have a good time ##

synth17 <- 
  df %>% 
  synthetic_control(outcome = ipgdtim, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth17, time_window = 2002:2008)

## important that gov is strong ##

synth18 <- 
  df %>% 
  synthetic_control(outcome = ipstrgv, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
                     gender = mean(gndr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     yearbirth = mean(yrbrn, na.rm = T),
                     borncnt = mean(brncntr, na.rm = T),
                     edu = mean(edulvla, na.rm = T),
                     eduf = mean(edulvlfa, na.rm = T),
                     edum = mean(edulvlma, na.rm = T),
                     edup = mean(edulvlpa, na.rm = T),
                     eduyr = mean(eduyrs, na.rm = T),
                     hhinc = mean(hinctnt, na.rm = T),
                     chlde = mean(chldhhe, na.rm = T),
                     chldn = mean(chldhm, na.rm = T),
                     fborn = mean(facntr, na.rm = T),
                     mborn = mean(mocntr, na.rm = T),
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

plot_differences(synth18, time_window = 2002:2008)
