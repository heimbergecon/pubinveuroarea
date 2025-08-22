rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table",
              "countrycode", "GGally", "car","lpirfs","openxlsx")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#load packages
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)
library(countrycode)
library(openxlsx) #export to excel
library(lpirfs)

#read data
data_select <- fread(here("data_pubinv_final.csv"), check.names = FALSE, header=TRUE)

#Real GDP RESPONSE
res_gdp <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "log_RGDP",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#Public investment ratio response
res_ratio <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PUBINVRATIO",       # percentage points of GDP (level, not growth)
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP","PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#function for cumulative response
to_cum <- function(irf_obj, scale = 100){
  irf <- as.numeric(irf_obj$irf_panel_mean) * scale
  lo  <- as.numeric(irf_obj$irf_panel_low)  * scale
  hi  <- as.numeric(irf_obj$irf_panel_up)   * scale
  data.frame(
    horizon = 1:length(irf),
    cum     = c(cumsum(irf)),
    cum_lo  = c(cumsum(lo)),
    cum_hi  = c(cumsum(hi))
  )
}

#calculate cumulative response
gdp_cum   <- to_cum(res_gdp)
ratio_cum  <- to_cum(res_ratio)
gdp_cum$horizon_true<-c(0,1,2,3)
ratio_cum$horizon_true<-c(0,1,2,3)

# Multiplier
mult_pp <- dplyr::left_join(gdp_cum, ratio_cum, by="horizon_true", suffix=c("_gdp","_inv")) |>
  dplyr::mutate(
    mult_point = 100 * (cum_gdp / cum_inv),
    mult_lo    = 100 * (cum_lo_gdp / cum_hi_inv),
    mult_hi    = 100 * (cum_hi_gdp / cum_lo_inv)
  )

#print multiplier
print(mult_pp[, c("horizon_true","mult_point","mult_lo","mult_hi")])

#Create multiplier chart
chart_multiplier<-ggplot(mult_pp, aes(x = horizon_true, y = mult_point)) +
  geom_ribbon(aes(ymin = mult_lo, ymax = mult_hi), fill = "grey", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = 2, color = '#E41A1C', linewidth = 0.8) +
  labs(
    title = "Real GDP (cumulative multiplier)",
    x = "Years after the shock",
    y = "multiplier"
  ) +
  theme_minimal(base_size = 12)
chart_multiplier

#response of private investment ratio

#calculate growth in private investment ratio
data_select <- ddply(data_select,"ccode", transform,
                     INVGDP_diff=c(NA,diff(INVGDP))) #calculate real growth

res_inv <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "INVGDP",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE","INVGDP_diff"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#function for cumulative response
to_cum <- function(irf_obj, scale = 1){
  irf <- as.numeric(irf_obj$irf_panel_mean) * scale
  lo  <- as.numeric(irf_obj$irf_panel_low)  * scale
  hi  <- as.numeric(irf_obj$irf_panel_up)   * scale
  data.frame(
    horizon = 1:length(irf),
    cum     = c(cumsum(irf)),
    cum_lo  = c(cumsum(lo)),
    cum_hi  = c(cumsum(hi))
  )
}

#calculate cumulative response
inv_cum   <- to_cum(res_inv)
inv_cum$horizon_true<-c(0,1,2,3)

#Create chart
chart_investment<-ggplot(inv_cum, aes(x = horizon_true, y = cum)) +
  geom_ribbon(aes(ymin = cum_lo, ymax = cum_hi), fill = "grey", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = 2, color = '#E41A1C', linewidth = 0.8) +
  labs(
    title = "Private investment ratio",
    x = "Years after the shock",
    y = "in %-points"
  ) +
  theme_minimal(base_size = 12)
chart_investment

#response of public debt ratio
res_pdebt <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PDEBT",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#calculate cumulative response
pdebt_cum   <- to_cum(res_pdebt)
pdebt_cum$horizon_true<-c(0,1,2,3)

#Create chart
chart_pdebt<-ggplot(pdebt_cum, aes(x = horizon_true, y = cum)) +
  geom_ribbon(aes(ymin = cum_lo, ymax = cum_hi), fill = "grey", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = 2, color = '#E41A1C', linewidth = 0.8) +
  labs(
    title = "Public debt ratio",
    x = "Years after the shock",
    y = "in %-points"
  ) +
  theme_minimal(base_size = 12)
chart_pdebt

#response of unemployment rate
res_unem <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "UNRATE",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE","UNRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#calculate cumulative response
unem_cum   <- to_cum(res_unem)
unem_cum$horizon_true<-c(0,1,2,3)

#Create chart
chart_unem<-ggplot(unem_cum, aes(x = horizon_true, y = cum)) +
  geom_ribbon(aes(ymin = cum_lo, ymax = cum_hi), fill = "grey", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = 2, color = '#E41A1C', linewidth = 0.8) +
  labs(
    title = "Unemployment rate",
    x = "Years after the shock",
    y = "in %-points"
  ) +
  theme_minimal(base_size = 12)
chart_unem

#### robustness checks ####
#add output gap measure

res_gdp_rob1 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "log_RGDP",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("OUTPUTGAP", "PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#Public investment ratio response
res_ratio_rob1 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PUBINVRATIO",       # percentage points of GDP (level, not growth)
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("OUTPUTGAP","PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#function for cumulative response
to_cum <- function(irf_obj, scale = 100){
  irf <- as.numeric(irf_obj$irf_panel_mean) * scale
  lo  <- as.numeric(irf_obj$irf_panel_low)  * scale
  hi  <- as.numeric(irf_obj$irf_panel_up)   * scale
  data.frame(
    horizon = 1:length(irf),
    cum     = c(cumsum(irf)),
    cum_lo  = c(cumsum(lo)),
    cum_hi  = c(cumsum(hi))
  )
}

#calculate cumulative response
gdp_cum_rob1   <- to_cum(res_gdp_rob1)
ratio_cum_rob1  <- to_cum(res_ratio_rob1)
gdp_cum_rob1$horizon_true<-c(0,1,2,3)
ratio_cum_rob1$horizon_true<-c(0,1,2,3)

# Multiplier
mult_pp_rob1 <- dplyr::left_join(gdp_cum_rob1, ratio_cum_rob1, by="horizon_true", suffix=c("_gdp","_inv")) |>
  dplyr::mutate(
    mult_point = 100 * (cum_gdp / cum_inv),
    mult_lo    = 100 * (cum_lo_gdp / cum_hi_inv),
    mult_hi    = 100 * (cum_hi_gdp / cum_lo_inv)
  )

#print multiplier
print(mult_pp_rob1[, c("horizon_true","mult_point","mult_lo","mult_hi")])

#vary lag length
res_gdp_rob2 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "log_RGDP",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 3,
  confint       = 1,
  hor           = 4
)

#Public investment ratio response
res_ratio_rob2 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PUBINVRATIO",       # percentage points of GDP (level, not growth)
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP","PDEBT","forecasterror","NOMLRATE"),
  lags_exog_data= 3,
  confint       = 1,
  hor           = 4
)

#calculate cumulative response
gdp_cum_rob2   <- to_cum(res_gdp_rob2)
ratio_cum_rob2  <- to_cum(res_ratio_rob2)
gdp_cum_rob2$horizon_true<-c(0,1,2,3)
ratio_cum_rob2$horizon_true<-c(0,1,2,3)

# Multiplier
mult_pp_rob2 <- dplyr::left_join(gdp_cum_rob2, ratio_cum_rob2, by="horizon_true", suffix=c("_gdp","_inv")) |>
  dplyr::mutate(
    mult_point = 100 * (cum_gdp / cum_inv),
    mult_lo    = 100 * (cum_lo_gdp / cum_hi_inv),
    mult_hi    = 100 * (cum_hi_gdp / cum_lo_inv)
  )

#print multiplier
print(mult_pp_rob2[, c("horizon_true","mult_point","mult_lo","mult_hi")])

#control for primary balance
res_gdp_rob3 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "log_RGDP",
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP", "PDEBT","forecasterror","NOMLRATE", "PRIMARYBAL"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#Public investment ratio response
res_ratio_rob3 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PUBINVRATIO",       # percentage points of GDP (level, not growth)
  shock         = "forecasterror",
  diff_shock    = FALSE,
  panel_model   = "within",
  panel_effect  = "twoways",
  robust_cov    = "vcovSCC",
  c_exog_data   = c("REER"),
  l_exog_data   = c("growth_RGDP","PDEBT","forecasterror","NOMLRATE", "PRIMARYBAL"),
  lags_exog_data= 2,
  confint       = 1,
  hor           = 4
)

#calculate cumulative response
gdp_cum_rob3   <- to_cum(res_gdp_rob3)
ratio_cum_rob3  <- to_cum(res_ratio_rob3)
gdp_cum_rob3$horizon_true<-c(0,1,2,3)
ratio_cum_rob3$horizon_true<-c(0,1,2,3)

# Multiplier
mult_pp_rob3 <- dplyr::left_join(gdp_cum_rob3, ratio_cum_rob3, by="horizon_true", suffix=c("_gdp","_inv")) |>
  dplyr::mutate(
    mult_point = 100 * (cum_gdp / cum_inv),
    mult_lo    = 100 * (cum_lo_gdp / cum_hi_inv),
    mult_hi    = 100 * (cum_hi_gdp / cum_lo_inv)
  )

#print multiplier
print(mult_pp_rob3[, c("horizon_true","mult_point","mult_lo","mult_hi")])