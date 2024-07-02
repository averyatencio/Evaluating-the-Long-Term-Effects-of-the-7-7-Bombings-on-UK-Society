## below is the R code used to generate the synthetic control ##

library(panelView)
library(gsynth)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidysynth)
library(purrr)


############################## synthetic control: trust #######################

df <- read_excel("~/Documents/Term 3 /Social Economics/ess_avg.xlsx")

## people are fair
synth1 <- 
  df %>% 
  synthetic_control(outcome = pplfair, # outcome
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


synth1 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth1, time_window = 2002:2008)

synth1 %>% plot_weights()

synth1 %>% plot_placebos()

synth1 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth1 %>% grab_synthetic_control(placebo = TRUE)

pplfair <- synth1 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

write.xlsx(pplfair, "~/Documents/Term 3 /Social Economics/synth.xlsx")

## people help

synth2 <- 
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


synth2 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth2, time_window = 2002:2008)

synth2 %>% plot_weights()

synth2 %>% plot_placebos()

synth2 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth2 %>% grab_synthetic_control(placebo = TRUE)

pplhlp <- synth2 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## people can be trusted 

synth3 <- 
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


synth3 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth3, time_window = 2002:2008)


synth3 %>% plot_weights()

synth3 %>% plot_placebos()

synth3 %>% plot_mspe_ratio()

att <- synth3 %>% grab_treatment_effects()

## grabbing the synthetic variable ##
synth3 %>% grab_synthetic_control(placebo = TRUE)

ppltrst <- synth3 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## radio listening etc average weekly

synth4 <- 
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


synth4 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth4, time_window = 2002:2008)

synth4 %>% plot_weights()

synth4 %>% plot_placebos()

synth4 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth4 %>% grab_synthetic_control(placebo = TRUE)

rdpol <- synth4 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## left right scale

synth5 <- 
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


synth5 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth5, time_window = 2002:2008)

synth5 %>% plot_weights()

synth5 %>% plot_placebos()

synth5 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth5 %>% grab_synthetic_control(placebo = TRUE)

lrscale <- synth5 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## member of a political party

synth6 <- 
  df %>% 
  synthetic_control(outcome = mmbprty, # outcome
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
                     citizen = mean(ctzcntr, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 


synth6 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth6, time_window = 2002:2008)

synth6 %>% plot_weights()

synth6 %>% plot_placebos()

synth6 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth6 %>% grab_synthetic_control(placebo = TRUE)

mmbparty <- synth6 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## making mind up about political issues

synth7 <- 
  df %>% 
  synthetic_control(outcome = poldcs, # outcome
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


synth7 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth7, time_window = 2002:2008)

synth7 %>% plot_weights()

synth7 %>% plot_placebos()

synth7 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth7 %>% grab_synthetic_control(placebo = TRUE)

poldcs <- synth7 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## interested in politics ##

synth8 <- 
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


synth8 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth8, time_window = 2002:2008)

synth8 %>% plot_weights()

synth8 %>% plot_placebos()

synth8 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth8 %>% grab_synthetic_control(placebo = TRUE)

polintr <- synth8 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## party ban ##

synth9 <- 
  df %>% 
  synthetic_control(outcome = prtyban, # outcome
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


synth9 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth9, time_window = 2002:2008)

synth9 %>% plot_weights()

synth9 %>% plot_placebos()

synth9 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth9 %>% grab_synthetic_control(placebo = TRUE)

prtyban <- synth9 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## satisfied with democracy

synth10 <- 
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


synth10 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth10, time_window = 2002:2008)

synth10 %>% plot_weights()

synth10 %>% plot_placebos()

synth10 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth10 %>% grab_synthetic_control(placebo = TRUE)

stfdem <- synth10 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## satisfied with government

synth11 <- 
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


synth11 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth11, time_window = 2002:2008)

synth11 %>% plot_weights()

synth11 %>% plot_placebos()

synth11 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth11 %>% grab_synthetic_control(placebo = TRUE)

stfgov <- synth11 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## satisfied with life

synth12 <- 
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


synth12 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth12, time_window = 2002:2008)

synth12 %>% plot_weights()

synth12 %>% plot_placebos()

synth12 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth12 %>% grab_synthetic_control(placebo = TRUE)

stflife <- synth12 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## trust in european parliment ##

synth13 <- 
  df %>% 
  synthetic_control(outcome = trstep, # outcome
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


synth13 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth13, time_window = 2002:2008)

synth13 %>% plot_weights()

synth13 %>% plot_placebos()

synth13 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth13 %>% grab_synthetic_control(placebo = TRUE)

trstep <- synth13 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## trust in legal system ##

synth14 <- 
  df %>% 
  synthetic_control(outcome = trstlgl, # outcome
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


synth14 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth14, time_window = 2002:2008)

synth14 %>% plot_weights()

synth14 %>% plot_placebos()

synth14 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth14 %>% grab_synthetic_control(placebo = TRUE)

trstlgl <- synth14 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## trust in police ## 

synth15 <- 
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


synth15 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth15, time_window = 2002:2008)

synth15 %>% plot_weights()

synth15 %>% plot_placebos()

synth15 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth15 %>% grab_synthetic_control(placebo = TRUE)

trstplc <- synth15 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## Trust in politicians ##

synth16 <- 
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

synth16 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth20, time_window = 2002:2008)

synth16 %>% plot_weights()

synth16 %>% plot_placebos()

synth16 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth16 %>% grab_synthetic_control(placebo = TRUE)

trstplt <- synth16 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## Trust in country's parliament ##
synth17 <- 
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


synth17 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth17, time_window = 2002:2008)

synth17 %>% plot_weights()

synth17 %>% plot_placebos()

synth17 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth17 %>% grab_synthetic_control(placebo = TRUE)

trstprl <- synth17 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## Trust in the United Nations ##
synth18 <- 
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


synth18 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth18, time_window = 2002:2008)

synth18 %>% plot_weights()

synth18 %>% plot_placebos()

synth18 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth18 %>% grab_synthetic_control(placebo = TRUE)

trstun <- synth18 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## Allow many/few immigrants of same race/ethnic group as majority ##
synth19 <- 
  df %>% 
  synthetic_control(outcome = imsmetn, # outcome
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


synth19 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth19, time_window = 2002:2008)

synth19 %>% plot_weights()

synth19 %>% plot_placebos()

synth19 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth19 %>% grab_synthetic_control(placebo = TRUE)

imsmetn <- synth19 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

##  Allow many/few immigrants of different race/ethnic group from majority ##
synth20 <- 
  df %>% 
  synthetic_control(outcome = imdfetn, # outcome
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


synth20 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth20, time_window = 2002:2008)

synth20 %>% plot_weights()

synth20 %>% plot_placebos()

synth20 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth20 %>% grab_synthetic_control(placebo = TRUE)

imdfetn <- synth20 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

##Allow many/few immigrants from poorer countries outside Europe ##

##  Allow many/few immigrants of different race/ethnic group from majority
synth21 <- 
  df %>% 
  synthetic_control(outcome = impcntr, # outcome
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


synth21 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth21, time_window = 2002:2008)

synth21 %>% plot_weights()

synth21 %>% plot_placebos()

synth21 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth21 %>% grab_synthetic_control(placebo = TRUE)

impcntr <- synth21 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## country's cultural life underminded by immigrants
synth22 <- 
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


synth22 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth22, time_window = 2002:2008)

synth22 %>% plot_weights()

synth22 %>% plot_placebos()

synth22 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth22 %>% grab_synthetic_control(placebo = TRUE)

imueclt <- synth22 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## immigrants make country worse or better ##

synth23 <- 
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


synth23 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth23, time_window = 2002:2008)

synth23 %>% plot_weights()

synth23 %>% plot_placebos()

synth23 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth23 %>% grab_synthetic_control(placebo = TRUE)

imwbcnt <- synth23 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## feeling of safety walking alone after dark ##

synth24 <- 
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


synth24 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth24, time_window = 2002:2008)

synth24 %>% plot_weights()

synth24 %>% plot_placebos()

synth24 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth24 %>% grab_synthetic_control(placebo = TRUE)

aesfdrk <- synth24 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## belonging to a minority or ethnic group ##

synth25 <- 
  df %>% 
  synthetic_control(outcome = blgetmg, # outcome
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
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 


synth25 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth25, time_window = 2002:2008)

synth25 %>% plot_weights()

synth25 %>% plot_placebos()

synth25 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth25 %>% grab_synthetic_control(placebo = TRUE)

blgetmg <- synth25 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## born in country ##

synth26 <- 
  df %>% 
  synthetic_control(outcome = brncntr, # outcome
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


synth26 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth26, time_window = 2002:2008)

synth26 %>% plot_weights()

synth26 %>% plot_placebos()

synth26 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth26 %>% grab_synthetic_control(placebo = TRUE)

brncntr <- synth26 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## citizen of country ##

synth27 <- 
  df %>% 
  synthetic_control(outcome = ctzcntr, # outcome
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
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

synth27 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth27, time_window = 2002:2008)

synth27 %>% plot_weights()

synth27 %>% plot_placebos()

synth27 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth27 %>% grab_synthetic_control(placebo = TRUE)

ctzcntr <- synth27 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## member of a group discriminated against ##

synth28 <- 
  df %>% 
  synthetic_control(outcome = dscrgrp, # outcome
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
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

synth28 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth28, time_window = 2002:2008)

synth28 %>% plot_weights()

synth28 %>% plot_placebos()

synth28 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth28 %>% grab_synthetic_control(placebo = TRUE)

dscrgrp <- synth28 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## father born in country ##

synth29 <- 
  df %>% 
  synthetic_control(outcome = facntr, # outcome
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

synth29 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth29, time_window = 2002:2008)

synth29 %>% plot_weights()

synth29 %>% plot_placebos()

synth29 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth29 %>% grab_synthetic_control(placebo = TRUE)

facntr <- synth29 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## happy ##

synth30 <- 
  df %>% 
  synthetic_control(outcome = happy, # outcome
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

synth30 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth30, time_window = 2002:2008)

synth30 %>% plot_weights()

synth30 %>% plot_placebos()

synth30 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth30 %>% grab_synthetic_control(placebo = TRUE)

happy <- synth30 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## health ##

synth31 <- 
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

synth31 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth31, time_window = 2002:2008)

synth31 %>% plot_weights()

synth31 %>% plot_placebos()

synth31 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth31 %>% grab_synthetic_control(placebo = TRUE)

health <- synth31 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## disabled ##

synth32 <- 
  df %>% 
  synthetic_control(outcome = hlthhmp, # outcome
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
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

synth32 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth32, time_window = 2002:2008)

synth32 %>% plot_weights()

synth32 %>% plot_placebos()

synth32 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth32 %>% grab_synthetic_control(placebo = TRUE)

hlthhmp <- synth32 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## mother born in country ##

synth33 <- 
  df %>% 
  synthetic_control(outcome = mocntr, # outcome
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
                     minority = mean(blgetmg, na.rm = T),
                     discrimination = mean(dscrgrp, na.rm = T),
                     disabled = mean(hlthhmp, na.rm = T),
                     citizen = mean(ctzcntr, na.rm = T),
                     pparty = mean(mmbprty, na.rm = T))%>%
  generate_weights(optimization_window = 2002:2005, # time to use in the optimization task
                   margin.ipop = .02, sigf.ipop = 7, bound.ipop = 6 # optimizer options
  ) %>%
  
  generate_control() 

synth33 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth33, time_window = 2002:2008)

synth33 %>% plot_weights()

synth33 %>% plot_placebos()

synth33 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth33 %>% grab_synthetic_control(placebo = TRUE)

mocntr <- synth33 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## gender ##

synth34 <- 
  df %>% 
  synthetic_control(outcome = gndr, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
                     age = mean(agea, na.rm = T),
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

synth34 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth34, time_window = 2002:2008)

synth34 %>% plot_weights()

synth34 %>% plot_placebos()

synth34 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth34 %>% grab_synthetic_control(placebo = TRUE)

gndr <- synth34 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## year born ##

synth35 <- 
  df %>% 
  synthetic_control(outcome = yrbrn, # outcome
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

synth35 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth35, time_window = 2002:2008)

synth35 %>% plot_weights()

synth35 %>% plot_placebos()

synth35 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth35 %>% grab_synthetic_control(placebo = TRUE)

yrbrn <- synth35 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## age ##

synth36 <- 
  df %>% 
  synthetic_control(outcome = agea, # outcome
                    unit = region, # unit index in the panel data
                    time = year, # time index in the panel data
                    i_unit = "London", # unit where the intervention occurred
                    i_time = 2005, # time period when the intervention occurred
                    generate_placebos=TRUE # generate placebo synthetic controls (for inference)
  ) %>%
  generate_predictor(time_window = 2002:2005,
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

synth36 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth36, time_window = 2002:2008)

synth36 %>% plot_weights()

synth36 %>% plot_placebos()

synth36 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth36 %>% grab_synthetic_control(placebo = TRUE)

agea <- synth36 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## ever had children living in the house ##

synth37 <- 
  df %>% 
  synthetic_control(outcome = chldhhe, # outcome
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

synth37 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth37, time_window = 2002:2008)

synth37 %>% plot_weights()

synth37 %>% plot_placebos()

synth37 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth37 %>% grab_synthetic_control(placebo = TRUE)

chldhhe <- synth37 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## children living in the house now ##

synth38 <- 
  df %>% 
  synthetic_control(outcome = chldhm, # outcome
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

synth38 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth38, time_window = 2002:2008)

synth38 %>% plot_weights()

synth38 %>% plot_placebos()

synth38 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth38 %>% grab_synthetic_control(placebo = TRUE)

chldhm <- synth38 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## education level ##

synth39 <- 
  df %>% 
  synthetic_control(outcome = edulvla, # outcome
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

synth39 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth39, time_window = 2002:2008)

synth39 %>% plot_weights()

synth39 %>% plot_placebos()

synth39 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth39 %>% grab_synthetic_control(placebo = TRUE)

edulvla <- synth39 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## fathers level of education ##

synth40 <- 
  df %>% 
  synthetic_control(outcome = edulvlfa, # outcome
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

synth40 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth40, time_window = 2002:2008)

synth40 %>% plot_weights()

synth40 %>% plot_placebos()

synth40 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth40 %>% grab_synthetic_control(placebo = TRUE)

edulvlfa <- synth40 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## mothers level of education ##

synth41 <- 
  df %>% 
  synthetic_control(outcome = edulvlma, # outcome
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

synth41 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth41, time_window = 2002:2008)

synth41 %>% plot_weights()

synth41 %>% plot_placebos()

synth41 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth41 %>% grab_synthetic_control(placebo = TRUE)

edulvlma <- synth41 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## partners level of education ##

synth42 <- 
  df %>% 
  synthetic_control(outcome = edulvlpa, # outcome
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

synth42 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth42, time_window = 2002:2008)

synth42 %>% plot_weights()

synth42 %>% plot_placebos()

synth42 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth42 %>% grab_synthetic_control(placebo = TRUE)

edulvlpa <- synth42 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## years of education ##

synth43 <- 
  df %>% 
  synthetic_control(outcome = eduyrs, # outcome
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

synth43 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth43, time_window = 2002:2008)

synth43 %>% plot_weights()

synth43 %>% plot_placebos()

synth43 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth43 %>% grab_synthetic_control(placebo = TRUE)

eduyrs <- synth43 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## HH income ##

synth44 <- 
  df %>% 
  synthetic_control(outcome = hinctnt, # outcome
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

synth44 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth44, time_window = 2002:2008)

synth44 %>% plot_weights()

synth44 %>% plot_placebos()

synth44 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth44 %>% grab_synthetic_control(placebo = TRUE)

hinctnt <- synth44 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## important to live in safe and secure surroundings ##

synth45 <- 
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


synth45 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth45, time_window = 2002:2008)

synth45 %>% plot_weights()

synth45 %>% plot_placebos()

synth45 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth45 %>% grab_synthetic_control(placebo = TRUE)

impsafe <- synth45 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## important to have a good time ##

synth46 <- 
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


synth46 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth46, time_window = 2002:2008)

synth46 %>% plot_weights()

synth46 %>% plot_placebos()

synth46 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth46 %>% grab_synthetic_control(placebo = TRUE)

ipgdtim <- synth46 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

## important that government is strong and ensures safety ##

synth47 <- 
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


synth47 %>% plot_trends(time_window = 2002:2008)

plot_differences(synth47, time_window = 2002:2008)

synth47 %>% plot_weights()

synth47 %>% plot_placebos()

synth47 %>% plot_mspe_ratio()

## grabbing the synthetic variable ##
synth47 %>% grab_synthetic_control(placebo = TRUE)

ipstrgv <- synth47 %>% grab_synthetic_control(placebo = TRUE) %>% as.data.frame()

