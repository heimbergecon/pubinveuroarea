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
  endog_data    = "PUBINVRATIO",      
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

#join cumulative responses
cum_df <- dplyr::left_join(
  gdp_cum  |> dplyr::rename(h = horizon_true, X = cum, X_lo = cum_lo, X_hi = cum_hi),
  ratio_cum|> dplyr::rename(h = horizon_true, Y = cum, Y_lo = cum_lo, Y_hi = cum_hi),
  by = "h"
)

#back out 1·SE for X and Y
se_X <- 0.5 * ((cum_df$X_hi - cum_df$X) + (cum_df$X - cum_df$X_lo))
se_Y <- 0.5 * ((cum_df$Y_hi - cum_df$Y) + (cum_df$Y - cum_df$Y_lo))

#point multiplier
R_point <- (cum_df$X / cum_df$Y) * 100

#delta-method SE for ratio R = X/Y, ignoring Cov(X,Y) (conservative if Cov > 0)
#Var(R) ≈ (1/Y^2) Var(X) + (X^2 / Y^4) Var(Y) - 2 X/(Y^3) Cov(X,Y) / Here we set Cov = 0 for a quick ±1·SE band.
eps <- 1e-10
Ysafe <- ifelse(abs(cum_df$Y) < eps, NA_real_, cum_df$Y)

se_R <- sqrt( (se_X^2) / (Ysafe^2) + ((cum_df$X^2) * (se_Y^2)) / (Ysafe^4) ) * 100

mult_pp <- data.frame(
  horizon_true = cum_df$h,
  mult_point   = R_point,
  mult_lo      = R_point - se_R,
  mult_hi      = R_point + se_R
)

print(mult_pp)

#create multiplier chart
chart_multiplier<-ggplot(mult_pp, aes(x = horizon_true, y = mult_point)) +
  geom_ribbon(aes(ymin = mult_lo, ymax = mult_hi), fill = "grey", alpha = 0.4) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0, linetype = 2, color = '#E41A1C', linewidth = 0.8) +
  labs(
    title = "Real GDP: cumulative investment multiplier",
    x = "Years after the shock",
    y = "multiplier"
  ) +
  theme_minimal(base_size = 12)
chart_multiplier

#response of private investment ratio
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
to_cum_delta <- function(irf_obj, scale = 1){
  mu  <- as.numeric(irf_obj$irf_panel_mean) * scale
  lo  <- as.numeric(irf_obj$irf_panel_low)  * scale
  hi  <- as.numeric(irf_obj$irf_panel_up)   * scale
  
  #per-horizon SE (average of upper/lower distances)
  se  <- 0.5 * ((hi - mu) + (mu - lo))
  
  #cumulative point response
  cum_mu <- cumsum(mu)
  
  #delta method for sum: Var(sum) = sum of variances
  cum_se <- sqrt(cumsum(se^2))
  
  #build data frame
  data.frame(
    horizon = 1:length(mu),
    cum          = c(cum_mu),
    cum_lo       = c(cum_mu - cum_se),
    cum_hi       = c(cum_mu + cum_se),
    se_cum       = c(cum_se)
  )
}

#cumulative response
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

#create chart
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
pdebt_cum <- to_cum(res_pdebt)
pdebt_cum$horizon_true<-c(0,1,2,3)

#create chart
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
unem_cum <- to_cum(res_unem, scale = 1)
unem_cum$horizon_true<-c(0,1,2,3)

#create chart
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

#public investment ratio response
res_ratio_rob1 <- lpirfs::lp_lin_panel(
  data_set      = data_select,
  endog_data    = "PUBINVRATIO",       
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

#join cumulative responses
cum_df_rob1 <- dplyr::left_join(
  gdp_cum_rob1  |> dplyr::rename(h = horizon_true, X = cum, X_lo = cum_lo, X_hi = cum_hi),
  ratio_cum_rob1|> dplyr::rename(h = horizon_true, Y = cum, Y_lo = cum_lo, Y_hi = cum_hi),
  by = "h"
)

#back out 1·SE for X and Y
se_X_rob1 <- 0.5 * ((cum_df_rob1$X_hi - cum_df_rob1$X) + (cum_df_rob1$X - cum_df_rob1$X_lo))
se_Y_rob1 <- 0.5 * ((cum_df_rob1$Y_hi - cum_df_rob1$Y) + (cum_df_rob1$Y - cum_df_rob1$Y_lo))

#point multiplier
R_point_rob1 <- (cum_df_rob1$X / cum_df_rob1$Y) * 100 

#delta-method SE
eps_rob1 <- 1e-10
Ysafe_rob1 <- ifelse(abs(cum_df_rob1$Y) < eps_rob1, NA_real_, cum_df_rob1$Y)

se_R_rob1 <- sqrt( (se_X_rob1^2) / (Ysafe_rob1^2) + ((cum_df_rob1$X^2) * (se_Y_rob1^2)) / (Ysafe_rob1^4) ) * 100

mult_pp_rob1 <- data.frame(
  horizon_true = cum_df_rob1$h,
  mult_point   = R_point_rob1,
  mult_lo      = R_point_rob1 - se_R_rob1,
  mult_hi      = R_point_rob1 + se_R_rob1
)

print(mult_pp_rob1)

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
  endog_data    = "PUBINVRATIO",       
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

#join cumulative responses
cum_df_rob2 <- dplyr::left_join(
  gdp_cum_rob2  |> dplyr::rename(h = horizon_true, X = cum, X_lo = cum_lo, X_hi = cum_hi),
  ratio_cum_rob2|> dplyr::rename(h = horizon_true, Y = cum, Y_lo = cum_lo, Y_hi = cum_hi),
  by = "h"
)

#back out 1·SE for X and Y
se_X_rob2 <- 0.5 * ((cum_df_rob2$X_hi - cum_df_rob2$X) + (cum_df_rob2$X - cum_df_rob2$X_lo))
se_Y_rob2 <- 0.5 * ((cum_df_rob2$Y_hi - cum_df_rob2$Y) + (cum_df_rob2$Y - cum_df_rob2$Y_lo))

#point multiplier
R_point_rob2 <- (cum_df_rob2$X / cum_df_rob2$Y) * 100

#delta-method SE
eps_rob2 <- 1e-10
Ysafe_rob2 <- ifelse(abs(cum_df_rob2$Y) < eps_rob2, NA_real_, cum_df_rob2$Y)

se_R_rob2 <- sqrt( (se_X_rob2^2) / (Ysafe_rob2^2) + ((cum_df_rob2$X^2) * (se_Y_rob2^2)) / (Ysafe_rob2^4) ) * 100

mult_pp_rob2 <- data.frame(
  horizon_true = cum_df_rob2$h,
  mult_point   = R_point_rob2,
  mult_lo      = R_point_rob2 - se_R_rob2,
  mult_hi      = R_point_rob2 + se_R_rob2
)

print(mult_pp_rob2)

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
  endog_data    = "PUBINVRATIO",      
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

#join cumulative responses
cum_df_rob3 <- dplyr::left_join(
  gdp_cum_rob3  |> dplyr::rename(h = horizon_true, X = cum, X_lo = cum_lo, X_hi = cum_hi),
  ratio_cum_rob3|> dplyr::rename(h = horizon_true, Y = cum, Y_lo = cum_lo, Y_hi = cum_hi),
  by = "h"
)

#back out 1·SE for X and Y
se_X_rob3 <- 0.5 * ((cum_df_rob3$X_hi - cum_df_rob3$X) + (cum_df_rob3$X - cum_df_rob3$X_lo))
se_Y_rob3 <- 0.5 * ((cum_df_rob3$Y_hi - cum_df_rob3$Y) + (cum_df_rob3$Y - cum_df_rob3$Y_lo))

#point multiplier
R_point_rob3 <- (cum_df_rob3$X / cum_df_rob3$Y) * 100

#delta-method SE
eps_rob3 <- 1e-10 
Ysafe_rob3 <- ifelse(abs(cum_df_rob3$Y) < eps_rob3, NA_real_, cum_df_rob3$Y)

se_R_rob3 <- sqrt( (se_X_rob3^2) / (Ysafe_rob3^2) + ((cum_df_rob3$X^2) * (se_Y_rob3^2)) / (Ysafe_rob3^4) ) * 100

mult_pp_rob3 <- data.frame(
  horizon_true = cum_df_rob3$h,
  mult_point   = R_point_rob3,
  mult_lo      = R_point_rob3 - se_R_rob3,
  mult_hi      = R_point_rob3 + se_R_rob3
)

print(mult_pp_rob3)
