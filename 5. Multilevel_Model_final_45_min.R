library(dplyr)
library(lme4)
library(gtable)
library(grid)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(interactions)
library(gridExtra)
library(reshape2)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(extrafont)
library(cowplot)
font_import()
loadfonts(device = "win")


####read data####
gro_45_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_2700_midday.csv")
gro_45_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_2700_morning.csv")

he_45_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_2700_midday_nu.csv")
he_45_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_2700_morning_nu.csv")

he_45_mid_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_2700_midday_u.csv")
he_45_morn_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_2700_morning_u.csv")




###################### Grocery ##########################

#### grocery 45 min midday (pre vs covid) ####
dat = gro_45_mid[gro_45_mid$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#dat1 = arrange(dat, p_pov_z)
#fitting multilevel model
gro_model1 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban + 
                      (1|City), 
                    dat, family=binomial ("logit"))
summary(gro_model1)

#### grocery 45 min midday (pre vs post) ####
dat = gro_45_mid[gro_45_mid$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
gro_model2 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                      (1|City), 
                    dat, family=binomial ("logit"))
summary(gro_model2)


##### grocery 45 min morning (pre vs covid) ####
dat = gro_45_morn[gro_45_morn$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
gro_model3 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                      (1|City), 
                    dat, family=binomial ("logit"))
summary(gro_model3)


#### grocery 45 min morning (pre vs post)####
dat = gro_45_morn[gro_45_morn$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
gro_model4 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                      (1|City), 
                    dat, family=binomial ("logit"))
summary(gro_model4)


#### grocery coefficients table #### 
Variables = c('Intercept','Poverty rate', 'People of colors (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'People of colors x suburban')

coeff = data.frame(Variables)
coeff$gro_mid_12_coef = summary(gro_model1)$coefficients[,1]
coeff$gro_mid_12_sd = summary(gro_model1)$coefficients[,2] 
coeff$gro_mid_12_pval = summary(gro_model1)$coefficients[,4]
coeff = coeff %>% mutate(gro_mid_12_sig = ifelse(gro_mid_12_pval<=0.05, 
                                                 "Significant", "Not significant"))
coeff$gro_mid_13_coef = summary(gro_model2)$coefficients[,1] 
coeff$gro_mid_13_sd = summary(gro_model2)$coefficients[,2] 
coeff$gro_mid_13_pval = summary(gro_model2)$coefficients[,4] 
coeff = coeff %>% mutate(gro_mid_13_sig = ifelse(gro_mid_13_pval<=0.05, 
                                                 "Significant", "Not significant"))
coeff$gro_morn_12_coef = summary(gro_model3)$coefficients[,1] 
coeff$gro_morn_12_sd = summary(gro_model3)$coefficients[,2] 
coeff$gro_morn_12_pval = summary(gro_model3)$coefficients[,4] 
coeff = coeff  %>% mutate(gro_morn_12_sig = ifelse(gro_morn_12_pval<=0.05, 
                                                   "Significant", "Not significant"))
coeff$gro_morn_13_coef = summary(gro_model4)$coefficients[,1] 
coeff$gro_morn_13_sd = summary(gro_model4)$coefficients[,2] 
coeff$gro_morn_13_pval = summary(gro_model4)$coefficients[,4] 
coeff = coeff %>% mutate(gro_morn_13_sig = ifelse(gro_morn_13_pval<=0.05, 
                                                  "Significant", "Not significant"))
coeff = coeff[-1,]

#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_grocery_45min.csv")

#### grocery predicted probability ####
pred1 = ggpredict(gro_model1, terms = c("City"), type="re")
pred2 = ggpredict(gro_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(gro_model3, terms = c("City"), type="re")
pred4 = ggpredict(gro_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

gro_prob = merge(pred12, pred34, by = "x")
#write.csv(gro_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_grocery_45min.csv")




################ Health_nonurgent #########################

#### Health 45 min midday (pre vs covid) ####
dat = he_45_mid[he_45_mid$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_model1 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_model1)

#### health 45 min midday (pre vs post)####
dat = he_45_mid[he_45_mid$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_model2 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_model2)

#### Health 45 min morning (pre vs covid)####
dat = he_45_morn[he_45_morn$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_model3 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_model3)


#### health 45 min morning (pre vs post) ####
dat = he_45_morn[he_45_morn$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_model4 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                     (1|City), 
                   dat, family=binomial ("logit"))

summary(he_model4)

#### health non-urgent coefficient table#### 
Variables = c('Intercept','Poverty rate', 'People of colors (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'People of colors x suburban' )

coeff = data.frame(Variables)
coeff$he_mid_12_coef = summary(he_model1)$coefficients[,1]
coeff$he_mid_12_sd = summary(he_model1)$coefficients[,2] 
coeff$he_mid_12_pval = summary(he_model1)$coefficients[,4]
coeff = coeff %>% mutate(he_mid_12_sig = ifelse(he_mid_12_pval<=0.05, 
                                                "Significant", "Not significant"))
coeff$he_mid_13_coef = summary(he_model2)$coefficients[,1] 
coeff$he_mid_13_sd = summary(he_model2)$coefficients[,2] 
coeff$he_mid_13_pval = summary(he_model2)$coefficients[,4] 
coeff = coeff %>% mutate(he_mid_13_sig = ifelse(he_mid_13_pval<=0.05, 
                                                "Significant", "Not significant"))
coeff$he_morn_12_coef = summary(he_model3)$coefficients[,1] 
coeff$he_morn_12_sd = summary(he_model3)$coefficients[,2] 
coeff$he_morn_12_pval = summary(he_model3)$coefficients[,4] 
coeff = coeff  %>% mutate(he_morn_12_sig = ifelse(he_morn_12_pval<=0.05, 
                                                  "Significant", "Not significant"))
coeff$he_morn_13_coef = summary(he_model4)$coefficients[,1] 
coeff$he_morn_13_sd = summary(he_model4)$coefficients[,2] 
coeff$he_morn_13_pval = summary(he_model4)$coefficients[,4] 
coeff = coeff %>% mutate(he_morn_13_sig = ifelse(he_morn_13_pval<=0.05, 
                                                 "Significant", "Not significant"))
coeff = coeff[-1,]

#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_nu_45min.csv")

#### health non-urgent predicted probability#### 
pred1 = ggpredict(he_model1, terms = c("City"), type="re")
pred2 = ggpredict(he_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(he_model3, terms = c("City"), type="re")
pred4 = ggpredict(he_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

health_prob = merge(pred12, pred34, by = "x")
#write.csv(health_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_health_45min_nu.csv")


################Health_urgent#############################################

#### Health 45 min midday (pre vs covid)####
dat = he_45_mid_u[he_45_mid_u$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_u_model1 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                       (1|City), 
                     dat, family=binomial ("logit"))
summary(he_u_model1)

#### health 45 min midday (pre vs post)####
dat = he_45_mid_u[he_45_mid_u$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_u_model2 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                       (1|City), 
                     dat, family=binomial ("logit"))
summary(he_u_model2)

#### Health 45 min morning (pre vs covid)####
dat = he_45_morn_u[he_45_morn_u$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_u_model3 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                       (1|City), 
                     dat, family=binomial ("logit"))
summary(he_u_model3)


#### health 45 min morning (pre vs post)####
dat = he_45_morn_u[he_45_morn_u$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_nwhi_z = (dat$p_nwhi - mean(dat$p_nwhi))/sd(dat$p_nwhi)
dat$med_inc_z = (dat$med_inc - mean(dat$med_inc))/sd(dat$med_inc)
dat$p_nv_z = (dat$p_nv - mean(dat$p_nv))/sd(dat$p_nv)
dat$p_own_z = (dat$p_own - mean(dat$p_own))/sd(dat$p_own)
dat$p_pov_z = (dat$p_pov - mean(dat$p_pov))/sd(dat$p_pov)
dat$p_noins_z = (dat$p_noins - mean(dat$p_noins))/sd(dat$p_noins)
dat$int_den_z = (dat$int_den - mean(dat$int_den, na.rm=TRUE))/sd(dat$int_den, na.rm=TRUE)
dat$mm_road_de_z = (dat$mm_road_de - mean(dat$mm_road_de, na.rm=TRUE))/sd(dat$mm_road_de, na.rm=TRUE)
dat$job_den_z = (dat$job_den - mean(dat$job_den, na.rm=TRUE))/sd(dat$job_den, na.rm=TRUE)
dat$p_low_inc_z = (dat$p_low_inc - mean(dat$p_low_inc, na.rm=TRUE))/sd(dat$p_low_inc, na.rm=TRUE)

#fitting multilevel model
he_u_model4 <- glmer(Cut ~ p_pov_z + p_nwhi_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_nwhi_z*suburban +
                       (1|City), 
                     dat, family=binomial ("logit"))

summary(he_u_model4)

#### health urgent coefficient table ####
Variables = c('Intercept','Poverty rate', 'People of colors (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'People of colors x suburban' )

coeff = data.frame(Variables)
coeff$he_mid_12_coef = summary(he_u_model1)$coefficients[,1]
coeff$he_mid_12_sd = summary(he_u_model1)$coefficients[,2] 
coeff$he_mid_12_pval = summary(he_u_model1)$coefficients[,4]
coeff = coeff %>% mutate(he_mid_12_sig = ifelse(he_mid_12_pval<=0.05, 
                                                "Significant", "Not significant"))
coeff$he_mid_13_coef = summary(he_u_model2)$coefficients[,1] 
coeff$he_mid_13_sd = summary(he_u_model2)$coefficients[,2] 
coeff$he_mid_13_pval = summary(he_u_model2)$coefficients[,4] 
coeff = coeff %>% mutate(he_mid_13_sig = ifelse(he_mid_13_pval<=0.05, 
                                                "Significant", "Not significant"))
coeff$he_morn_12_coef = summary(he_u_model3)$coefficients[,1] 
coeff$he_morn_12_sd = summary(he_u_model3)$coefficients[,2] 
coeff$he_morn_12_pval = summary(he_u_model3)$coefficients[,4] 
coeff = coeff  %>% mutate(he_morn_12_sig = ifelse(he_morn_12_pval<=0.05, 
                                                  "Significant", "Not significant"))
coeff$he_morn_13_coef = summary(he_u_model4)$coefficients[,1] 
coeff$he_morn_13_sd = summary(he_u_model4)$coefficients[,2] 
coeff$he_morn_13_pval = summary(he_u_model4)$coefficients[,4] 
coeff = coeff %>% mutate(he_morn_13_sig = ifelse(he_morn_13_pval<=0.05, 
                                                 "Significant", "Not significant"))
coeff = coeff[-1,]


#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_u_45min.csv")

#### health urgent predicted probability####
pred1 = ggpredict(he_u_model1, terms = c("City"), type="re")
pred2 = ggpredict(he_u_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(he_u_model3, terms = c("City"), type="re")
pred4 = ggpredict(he_u_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

health_u_prob = merge(pred12, pred34, by = "x")
#write.csv(health_u_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_health_45min_u.csv")



################### fixed effect plots #########################
coeff_gro = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_grocery_45min.csv",
                     stringsAsFactors = FALSE)
coeff_he_nu = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_nu_45min.csv",
                       stringsAsFactors = FALSE)
coeff_he_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_u_45min.csv",
                      stringsAsFactors = FALSE)

coeff_gro = coeff_gro[order(nrow(coeff_gro):1),]
coeff_he_nu = coeff_he_nu[order(nrow(coeff_he_nu):1),]
coeff_he_u = coeff_he_u[order(nrow(coeff_he_u):1),]

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}
coeff_gro$Variables <- factor(coeff_gro$Variables, levels = coeff_gro$Variables)
coeff_he_nu$Variables <- factor(coeff_he_nu$Variables, levels = coeff_he_nu$Variables)
coeff_he_u$Variables <- factor(coeff_he_u$Variables, levels = coeff_he_u$Variables)

#Function for plotting fixed effects
fixef_plot = function(data, var, sig, sd, title){
  figure = ggplot2::ggplot(data,aes(x = Variables,y = {{var}}, fill = {{sig}})) + 
    geom_bar(stat = "identity", width = 0.4, color = "white",
             position = position_dodge(width = 0.1)) +
    geom_errorbar( aes(x=Variables, ymin={{var}}-{{sd}}, ymax={{var}}+{{sd}}), 
                   width=0.2, colour="#99000d", alpha=1, size=8)+ 
    geom_hline(yintercept=0, color = "grey70") +
    coord_flip() +
    ggtitle(title) + ylab("Coefficients") +
    scale_fill_manual(values=c("Not significant" = "grey60", "Significant" = "#91bfdb")) +
    scale_x_discrete(labels=addline_format(data$Variables)) +
    theme(plot.title = element_blank(),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "grey70", fill=NA, size=1),
          panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted"))+
    geom_text(aes(label= ifelse({{sig}} == "Significant", paste(round({{var}},2)), ""), 
                  hjust= ifelse(({{var}} < 0),1.4,-0.4)), 
              color = "#636363", size=32, family = "Arial") + 
    ylim(-2.75, 2.75) +
    theme(axis.text =element_blank(),
          axis.title =element_blank(),
          legend.position = "none")
  return(figure)
}

plot1 = fixef_plot(coeff_gro, gro_morn_12_coef, gro_morn_12_sig, gro_morn_12_sd, "gro_morn_12")
plot2 = fixef_plot(coeff_gro, gro_morn_13_coef, gro_morn_13_sig, gro_morn_13_sd, "gro_morn_13")
plot3 = fixef_plot(coeff_gro, gro_mid_12_coef, gro_mid_12_sig, gro_mid_12_sd, "gro_mid_12")
plot4 = fixef_plot(coeff_gro, gro_mid_13_coef, gro_mid_13_sig, gro_mid_13_sd, "gro_mid_13")

plot5 = fixef_plot(coeff_he_nu, he_morn_12_coef, he_morn_12_sig, he_morn_12_sd, "he_nu_morn_12") 
plot6 = fixef_plot(coeff_he_nu, he_morn_13_coef, he_morn_13_sig, he_morn_13_sd, "he_nu_morn_13") 
plot7 = fixef_plot(coeff_he_nu, he_mid_12_coef, he_mid_12_sig, he_mid_12_sd, "he_nu_mid_12") 
plot8 = fixef_plot(coeff_he_nu, he_mid_13_coef, he_mid_13_sig, he_mid_13_sd, "he_nu_mid_13") 

plot9 = fixef_plot(coeff_he_u, he_morn_12_coef, he_morn_12_sig, he_morn_12_sd, "he_u_morn_12") 
plot10 = fixef_plot(coeff_he_u, he_morn_13_coef, he_morn_13_sig, he_morn_13_sd, "he_u_morn_13") 
plot11 = fixef_plot(coeff_he_u, he_mid_12_coef, he_mid_12_sig, he_mid_12_sd, "he_u_mid_12") 
plot12 = fixef_plot(coeff_he_u, he_mid_13_coef, he_mid_13_sig, he_mid_13_sd, "he_u_mid_13") 



#resolution 8000*6000
grid.arrange(plot1, plot2, plot3, plot4,
             plot5, plot6, plot7, plot8,
             plot9, plot10, plot11, plot12, nrow=3)


###############interaction plots#####################

#Function for interaction plots
interact_plot = function(mod, pred, modx, title){
  figure = interactions :: interact_plot(mod, pred = {{pred}}, modx = {{modx}}, 
                                         main.title = title,
                                         color.class = 'Blues', #legend.main = "No vehicle owner (%)",
                                         line.thickness = 2) + 
    theme(axis.text = element_text(size = 36, family = "Arial Narrow"), legend.position = 'none', 
          plot.title = element_blank(),
          axis.title = element_blank(), 
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(figure)
}

#### grocery interaction plots ####
gro_m1_p1 = interact_plot(gro_model1, p_low_inc_z, p_nv_z, "gro12_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m1_p2 = interact_plot(gro_model1, p_pov_z, suburban, "gro12_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m1_p3 = interact_plot(gro_model1, p_nwhi_z, suburban, "gro12_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m1_p4 = interact_plot(gro_model1, p_nv_z, suburban,"gro12_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

gro_m2_p1 = interact_plot(gro_model2, p_low_inc_z, p_nv_z, "gro13_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m2_p2 = interact_plot(gro_model2, p_pov_z, suburban, "gro13_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m2_p3 = interact_plot(gro_model2, p_nwhi_z, suburban, "gro13_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m2_p4 = interact_plot(gro_model2, p_nv_z, suburban, "gro13_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

gro_m3_p1 = interact_plot(gro_model3, p_low_inc_z, p_nv_z, "gro12_morn_45") + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m3_p2 = interact_plot(gro_model3, p_pov_z, suburban, "gro12_morn_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m3_p3 = interact_plot(gro_model3,  p_nwhi_z, suburban,"gro12_morn_45")  +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m3_p4 = interact_plot(gro_model3,  p_nv_z, suburban,"gro12_morn_45")  +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


gro_m4_p1 = interact_plot(gro_model4, p_low_inc_z, p_nv_z, "gro13_morn_45")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m4_p2 = interact_plot(gro_model4, p_pov_z, suburban, "gro13_morn_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m4_p3 = interact_plot(gro_model4,  p_nwhi_z, suburban,"gro13_morn_45")   +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m4_p4 = interact_plot(gro_model4,  p_nv_z, suburban,"gro13_morn_45")   +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

#### health nonurgent interaction plots ####
he_m1_p1 = interact_plot(he_model1, p_low_inc_z, p_nv_z, "he12_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p2 = interact_plot(he_model1, p_pov_z, suburban, "he12_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m1_p3 = interact_plot(he_model1, p_nwhi_z, suburban, "he12_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p4 = interact_plot(he_model1, p_nv_z,suburban, "he12_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m2_p1 = interact_plot(he_model2, p_low_inc_z, p_nv_z, "he13_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p2 = interact_plot(he_model2, p_pov_z, suburban, "he13_mid_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m2_p3 = interact_plot(he_model2, p_nwhi_z, suburban, "he13_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p4 = interact_plot(he_model2, p_nv_z,suburban, "he13_mid_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m3_p1 = interact_plot(he_model3, p_low_inc_z, p_nv_z, "he12_morn_45")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p2 = interact_plot(he_model3, p_pov_z, suburban, "he12_morn_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m3_p3 = interact_plot(he_model3, p_nwhi_z, suburban, "he12_morn_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p4 = interact_plot(he_model3, p_nv_z,suburban, "he12_morn_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


he_m4_p1 = interact_plot(he_model4, p_low_inc_z, p_nv_z, "he13_morn_45")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p2 = interact_plot(he_model4, p_pov_z, suburban, "he13_morn_45")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m4_p3 = interact_plot(he_model4, p_nwhi_z, suburban,"he13_morn_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p4 = interact_plot(he_model4, p_nv_z,suburban,"he13_morn_45") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

#### health urgent interaction plots ####
he_m1_p1_u = interact_plot(he_u_model1, p_low_inc_z, p_nv_z, "he12_mid_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p2_u = interact_plot(he_u_model1, p_pov_z, suburban, "he12_mid_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p3_u = interact_plot(he_u_model1, p_nwhi_z, suburban, "he12_mid_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p4_u = interact_plot(he_u_model1, p_nv_z,suburban, "he12_mid_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m2_p1_u = interact_plot(he_u_model2, p_low_inc_z, p_nv_z, "he13_mid_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p2_u = interact_plot(he_u_model2, p_pov_z, suburban, "he13_mid_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p3_u = interact_plot(he_u_model2, p_nwhi_z, suburban, "he13_mid_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p4_u = interact_plot(he_u_model2, p_nv_z,suburban, "he13_mid_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m3_p1_u = interact_plot(he_u_model3, p_low_inc_z, p_nv_z, "he12_morn_45_u")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p2_u = interact_plot(he_u_model3, p_pov_z, suburban, "he12_morn_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p3_u = interact_plot(he_u_model3, p_nwhi_z, suburban, "he12_morn_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p4_u = interact_plot(he_u_model3, p_nv_z,suburban, "he12_morn_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


he_m4_p1_u = interact_plot(he_u_model4, p_low_inc_z, p_nv_z, "he13_morn_45_u")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p2_u = interact_plot(he_u_model4, p_pov_z, suburban, "he13_morn_45_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p3_u = interact_plot(he_u_model4, p_nwhi_z, suburban,"he13_morn_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p4_u = interact_plot(he_u_model4, p_nv_z,suburban,"he13_morn_45_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


#plot resolution 4000*700
grid.arrange(gro_m3_p1, gro_m4_p1, gro_m3_p2, gro_m4_p2, gro_m3_p4, gro_m4_p4,  nrow = 1)
grid.arrange(he_m3_p1, he_m4_p1, he_m3_p2, he_m4_p2, he_m3_p3, he_m4_p3, nrow = 1)
grid.arrange(he_m3_p1_u, he_m4_p1_u, he_m3_p4_u, he_m4_p4_u, he_m3_p3_u, he_m4_p3_u, nrow = 1)


grid.arrange(gro_m1_p1, gro_m2_p1, gro_m1_p2, gro_m2_p2, gro_m1_p4, gro_m2_p4,  nrow = 1)
grid.arrange(he_m1_p1, he_m2_p1, he_m1_p2, he_m2_p2, he_m1_p3, he_m2_p3, nrow = 1)
grid.arrange(he_m1_p1_u, he_m2_p1_u, he_m1_p4_u, he_m2_p4_u, he_m1_p3_u, he_m2_p3_u, nrow = 1)



rm(list = ls())

