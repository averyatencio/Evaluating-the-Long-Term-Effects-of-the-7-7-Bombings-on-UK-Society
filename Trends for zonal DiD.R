library(panelView)
library(gsynth)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidysynth)
library(purrr)

################## Generating parallel trends for zonal DiD ###################

df <- read_excel("~/Documents/Term 3 /Social Economics/ess_avg.xlsx")

df$post <- ifelse(df$year > "2005", 1, 0)
df$treat <- ifelse(df$region == "London", 1, 0)
df$treatpost <- df$treat*df$post

## health ##
panelview(health ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## stflife ##
panelview(stflife ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## impsafe ##
panelview(impsafe ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## imueclt ##
panelview(imueclt ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## imwbcnt ##
panelview(imwbcnt ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## ipstrgv ##
panelview(ipstrgv ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## polintr ##
panelview(polintr ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## stfgov ##
panelview(stfgov ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## stfdem ##
panelview(stfdem ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## trstplt ##
panelview(trstplt ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## trstprl ##
panelview(trstprl ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## trstun ##
panelview(trstun ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")

## trstplc ##
panelview(trstplc ~ treatpost, data = df ,  index = c("region","year"), type = "outcome")





