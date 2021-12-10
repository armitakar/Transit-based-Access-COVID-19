#https://stackoverflow.com/questions/41384075/r-calculate-and-interpret-odds-ratio-in-logistic-regression
library(dplyr)
library(lme4)
library(gtable)
library(grid)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(ggcorrplot)
library(interactions)
library(gridExtra)
library(reshape2)
library(sf)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(extrafont)
library(cowplot)
library(purrr)
font_import()
loadfonts(device = "win")


####read data####
gro_30_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_1800_midday.csv")
gro_30_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_1800_morning.csv")

he_30_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_midday_nu.csv")
he_30_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_morning_nu.csv")

he_30_mid_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_midday_u.csv")
he_30_morn_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_morning_u.csv")


###### correlation plot for the socioeconomic variables ######
study = st_read('D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/study_area_updated_10_24_21.shp')
study$p_nv = study$p_ow_nv + study$p_re_nv
study$geometry = NULL

###check correlations
dat_sub <- subset(study, select= c(p_pov, p_black, p_other, p_nv ,  
                                 p_low_inc))
ggcorrplot(cor(dat_sub), method = "square", type = "upper",
           outline.col = "white", 
           lab = TRUE)

## estimating total suburban areas in sqm
study$suburb = ifelse(study$act_den > 0.5 & study$act_den < 6 , "suburban", "other")
p_suburb = sum((study %>% filter(study$suburb == "suburban"))$Area_sqm)/sum(study$Area_sqm)
p_suburb

###################### Grocery ##########################

#### grocery 30 min midday (pre vs covid) ####
dat = gro_30_mid[gro_30_mid$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
gro_model1 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
             dat, family=binomial ("logit"))
summary(gro_model1)

#### grocery 30 min midday (pre vs post) ####
dat = gro_30_mid[gro_30_mid$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
gro_model2 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(gro_model2)


##### grocery 30 min morning (pre vs covid) ####
dat = gro_30_morn[gro_30_morn$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
gro_model3 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(gro_model3)


#### grocery 30 min morning (pre vs post)####
dat = gro_30_morn[gro_30_morn$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
gro_model4 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                      p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                      p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(gro_model4)


#### grocery coefficients table #### 
Variables = c('Intercept','Poverty rate', 'Black population (%)', 'Other races (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'Black population x suburban', 'Poverty rate x No vehicle')

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

#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_grocery_30min_modified1.csv")

#### grocery predicted probability ####
pred1 = ggpredict(gro_model1, terms = c("City"), type="re")
pred2 = ggpredict(gro_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(gro_model3, terms = c("City"), type="re")
pred4 = ggpredict(gro_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

gro_prob = merge(pred12, pred34, by = "x")
#write.csv(gro_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_grocery_30min_modified1.csv")




################ Health_nonurgent #########################

#### Health 30 min midday (pre vs covid) ####
dat = he_30_mid[he_30_mid$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_model1 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(he_model1)

#### health 30 min midday (pre vs post)####
dat = he_30_mid[he_30_mid$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_model2 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(he_model2)

#### Health 30 min morning (pre vs covid)####
dat = he_30_morn[he_30_morn$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_model3 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))
summary(he_model3)


#### health 30 min morning (pre vs post) ####
dat = he_30_morn[he_30_morn$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_model4 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                     p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                     p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                  (1|City), 
                dat, family=binomial ("logit"))

summary(he_model4)

#### health non-urgent coefficient table#### 
Variables = c('Intercept','Poverty rate', 'Black population (%)', 'Other races (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'Black population x suburban', 'Poverty rate x No vehicle' )

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

#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_nu_30min_modified1.csv")

#### health non-urgent predicted probability#### 
pred1 = ggpredict(he_model1, terms = c("City"), type="re")
pred2 = ggpredict(he_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(he_model3, terms = c("City"), type="re")
pred4 = ggpredict(he_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

health_prob = merge(pred12, pred34, by = "x")
#write.csv(health_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_health_30min_nu_modified1.csv")


################Health_urgent#############################################

#### Health 30 min midday (pre vs covid)####
dat = he_30_mid_u[he_30_mid_u$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_u_model1 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_u_model1)

#### health 30 min midday (pre vs post)####
dat = he_30_mid_u[he_30_mid_u$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_u_model2 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_u_model2)

#### Health 30 min morning (pre vs covid)####
dat = he_30_morn_u[he_30_morn_u$Diff == "Precovid vs covid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_u_model3 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                     (1|City), 
                   dat, family=binomial ("logit"))
summary(he_u_model3)


#### health 30 min morning (pre vs post)####
dat = he_30_morn_u[he_30_morn_u$Diff == "Precovid vs postcovid",]
dat$City = factor(dat$City)
dat$int_den = as.numeric(dat$int_den)
dat <- dat %>% mutate(Cut = ifelse(Access ==0, 1, 0))

#estimating z scores
dat$pop_den_z = (dat$pop_den - mean(dat$pop_den))/sd(dat$pop_den)
dat$p_black_z = (dat$p_black - mean(dat$p_black, na.rm=TRUE))/sd(dat$p_black, na.rm=TRUE)
dat$p_other_z = (dat$p_other - mean(dat$p_other, na.rm=TRUE))/sd(dat$p_other, na.rm=TRUE)
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
he_u_model4 <- glmer(Cut ~ p_pov_z + p_black_z + p_other_z + p_nv_z +  
                       p_low_inc_z + suburban + p_nv_z*p_low_inc_z + 
                       p_pov_z*suburban + p_nv_z*suburban + p_black_z*suburban + p_pov_z*p_nv_z +
                     (1|City), 
                   dat, family=binomial ("logit"))

summary(he_u_model4)

#### health urgent coefficient table ####
Variables = c('Intercept','Poverty rate', 'Black population (%)', 'Other races (%)', 'Owner of no vehicle (%)',  
              'Low income workers (%)', 'Suburban', 'No vehicle x low income workers', 
              'Poverty rate x suburban','No vehicle x suburban', 'Black population x suburban', 'Poverty rate x No vehicle')

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


#write.csv(coeff, "D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_u_30min_modified1.csv")

#### health urgent predicted probability####
pred1 = ggpredict(he_u_model1, terms = c("City"), type="re")
pred2 = ggpredict(he_u_model2, terms = c("City"), type="re") 
pred12 = merge(pred1, pred2, by = "x")

pred3 = ggpredict(he_u_model3, terms = c("City"), type="re")
pred4 = ggpredict(he_u_model4, terms = c("City"), type="re") 
pred34 = merge(pred3, pred4, by = "x")

health_u_prob = merge(pred12, pred34, by = "x")
#write.csv(health_u_prob, "D:/Accessibility_study/Final isochrones1/Summary/probability_health_30min_u_modified1.csv")



################### fixed effect plots #########################
coeff_gro = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_grocery_30min_modified1.csv",
                     stringsAsFactors = FALSE)
coeff_he_nu = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_nu_30min_modified1.csv",
                    stringsAsFactors = FALSE)
coeff_he_u = read.csv("D:/Accessibility_study/Final isochrones1/Summary/coefficients_health_u_30min_modified1.csv",
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
      #axis.text.y = element_text(size=40),
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
                                         main.title = title, vary.lty = FALSE,
                                         color.class = c("#b30000", "#31a354","#253494"), legend.main = "No vehicle owner (%)",
                                         types = c("solid", "solid", "solid"), line.thickness = 2)+
    #scale_fill_manual(values = c("#b30000", "#edf8b1", "#31a354")) +
    theme(axis.text = element_text(size = 36, family = "Arial Narrow"), legend.position = 'none', 
          plot.title = element_blank(),
          axis.title = element_blank(), 
          axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(figure)
}

#### grocery interaction plots ####
gro_m1_p1 = interact_plot(gro_model1, p_low_inc_z, p_nv_z, "gro12_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m1_p2 = interact_plot(gro_model1, p_pov_z, suburban, "gro12_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m1_p3 = interact_plot(gro_model1, p_black_z, suburban, "gro12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m1_p4 = interact_plot(gro_model1, p_nv_z, suburban,"gro12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m1_p5 = interact_plot(gro_model1, p_pov_z, p_nv_z,  "gro12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

gro_m2_p1 = interact_plot(gro_model2, p_low_inc_z, p_nv_z, "gro13_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m2_p2 = interact_plot(gro_model2, p_pov_z, suburban, "gro13_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m2_p3 = interact_plot(gro_model2, p_black_z, suburban, "gro13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m2_p4 = interact_plot(gro_model2, p_nv_z, suburban, "gro13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m2_p5 = interact_plot(gro_model2, p_pov_z, p_nv_z,  "gro13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

gro_m3_p1 = interact_plot(gro_model3, p_low_inc_z, p_nv_z, "gro12_morn_30") + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m3_p2 = interact_plot(gro_model3, p_pov_z, suburban, "gro12_morn_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m3_p3 = interact_plot(gro_model3, p_black_z, suburban,"gro12_morn_30")  +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m3_p4 = interact_plot(gro_model3,  p_nv_z, suburban,"gro12_morn_30")  +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m3_p5 = interact_plot(gro_model3, p_pov_z, p_nv_z, "gro12_morn_30")  +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


gro_m4_p1 = interact_plot(gro_model4, p_low_inc_z, p_nv_z, "gro13_morn_30")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m4_p2 = interact_plot(gro_model4, p_pov_z, suburban, "gro13_morn_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
gro_m4_p3 = interact_plot(gro_model4,  p_black_z, suburban,"gro13_morn_30")   +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m4_p4 = interact_plot(gro_model4,  p_nv_z, suburban,"gro13_morn_30")   +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
gro_m4_p5 = interact_plot(gro_model4,  p_pov_z, p_nv_z, "gro13_morn_30")   +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))




#### health nonurgent interaction plots ####
he_m1_p1 = interact_plot(he_model1, p_low_inc_z, p_nv_z, "he12_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p2 = interact_plot(he_model1, p_pov_z, suburban, "he12_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m1_p3 = interact_plot(he_model1, p_black_z, suburban, "he12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p4 = interact_plot(he_model1, p_nv_z,suburban, "he12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p5 = interact_plot(he_model1, p_pov_z, p_nv_z, "he12_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m2_p1 = interact_plot(he_model2, p_low_inc_z, p_nv_z, "he13_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p2 = interact_plot(he_model2, p_pov_z, suburban, "he13_mid_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m2_p3 = interact_plot(he_model2, p_black_z, suburban, "he13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p4 = interact_plot(he_model2, p_nv_z,suburban, "he13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p5 = interact_plot(he_model2, p_pov_z, p_nv_z, "he13_mid_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m3_p1 = interact_plot(he_model3, p_low_inc_z, p_nv_z, "he12_morn_30")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p2 = interact_plot(he_model3, p_pov_z, suburban, "he12_morn_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m3_p3 = interact_plot(he_model3, p_black_z, suburban, "he12_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p4 = interact_plot(he_model3, p_nv_z,suburban, "he12_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p5 = interact_plot(he_model3, p_pov_z, p_nv_z, "he12_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m4_p1 = interact_plot(he_model4, p_low_inc_z, p_nv_z, "he13_morn_30")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p2 = interact_plot(he_model4, p_pov_z, suburban, "he13_morn_30")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15)) 
he_m4_p3 = interact_plot(he_model4, p_black_z, suburban,"he13_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p4 = interact_plot(he_model4, p_nv_z,suburban,"he13_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p5 = interact_plot(he_model4, p_pov_z, p_nv_z, "he13_morn_30") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

#### health urgent interaction plots ####
he_m1_p1_u = interact_plot(he_u_model1, p_low_inc_z, p_nv_z, "he12_mid_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p2_u = interact_plot(he_u_model1, p_pov_z, suburban, "he12_mid_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p3_u = interact_plot(he_u_model1, p_black_z, suburban, "he12_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p4_u = interact_plot(he_u_model1, p_nv_z,suburban, "he12_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m1_p5_u = interact_plot(he_u_model1, p_pov_z, p_nv_z,  "he12_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))

he_m2_p1_u = interact_plot(he_u_model2, p_low_inc_z, p_nv_z, "he13_mid_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p2_u = interact_plot(he_u_model2, p_pov_z, suburban, "he13_mid_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p3_u = interact_plot(he_u_model2, p_black_z, suburban, "he13_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p4_u = interact_plot(he_u_model2, p_nv_z,suburban, "he13_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m2_p5_u = interact_plot(he_u_model2, p_pov_z, p_nv_z,  "he13_mid_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


he_m3_p1_u = interact_plot(he_u_model3, p_low_inc_z, p_nv_z, "he12_morn_30_u")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p2_u = interact_plot(he_u_model3, p_pov_z, suburban, "he12_morn_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p3_u = interact_plot(he_u_model3, p_black_z, suburban, "he12_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p4_u = interact_plot(he_u_model3, p_nv_z,suburban, "he12_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m3_p5_u = interact_plot(he_u_model3, p_pov_z, p_nv_z,  "he12_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


he_m4_p1_u = interact_plot(he_u_model4, p_low_inc_z, p_nv_z, "he13_morn_30_u")  + 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p2_u = interact_plot(he_u_model4, p_pov_z, suburban, "he13_morn_30_u")+ 
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p3_u = interact_plot(he_u_model4, p_black_z, suburban,"he13_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p4_u = interact_plot(he_u_model4, p_nv_z,suburban,"he13_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))
he_m4_p5_u = interact_plot(he_u_model4, p_pov_z, p_nv_z, "he13_morn_30_u") +   
  scale_y_continuous(breaks=seq(0.0, 0.15, 0.1), limits=c(0, 0.15))


#plot resolution 4000*700
grid.arrange(gro_m3_p5, gro_m4_p5,  gro_m3_p2, gro_m4_p2, gro_m3_p4, gro_m4_p4, nrow = 1)
grid.arrange(he_m3_p5, he_m4_p5, he_m3_p3, he_m4_p3, he_m3_p1, he_m4_p1, nrow = 1)
grid.arrange(he_m3_p5_u, he_m4_p5_u, he_m3_p4_u, he_m4_p4_u, he_m3_p3_u, he_m4_p3_u, nrow = 1)


grid.arrange(gro_m1_p5, gro_m2_p5, gro_m1_p2, gro_m2_p2, gro_m1_p4, gro_m2_p4,   nrow = 1)
grid.arrange(he_m1_p5, he_m2_p5, he_m1_p3, he_m2_p3, he_m1_p1, he_m2_p1, nrow = 1)
grid.arrange(he_m1_p5_u, he_m2_p5_u, he_m1_p4_u, he_m2_p4_u, he_m1_p1_u, he_m2_p1_u, nrow = 1)




####plots for predicted probability map####
shp = st_read("D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/probability_30min_modified.shp")
shp$weighted_i[4] = 0
shp$sprawl[4] = 0
shp$sprawl = as.numeric(shp$sprawl)
shp = st_transform(shp, 4326)
shp_cen = st_centroid(st_make_valid(shp))
shp_cen$x = unlist(map(shp_cen$geometry,1))
shp_cen$y = unlist(map(shp_cen$geometry,2))
st_geometry(shp_cen) <- NULL


#### predicted probability plot during peak hours
for (i in shp_cen$City){
  df = shp_cen[shp_cen$City==i,]
  df1 = df%>% select(City, gro_morn12, gro_morn13)
  df2 <- melt(df1, id.vars="City")
  df2$x = c(1,1)
  
  
  plot1 = ggplot(df2,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = .3)) +
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("gro_morn12" = "#99000d", "gro_morn13" = "#fd8d3c"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0.5,0),"cm"))
  
  df3 = df%>% select(City, nu_morn12, nu_morn13)
  df4 <- melt(df3, id.vars="City")
  df4$x = c(1,1)
  
  plot2 = ggplot(df4,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = .3)) +
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("nu_morn12" = "#7a0177", "nu_morn13" = "#c994c7"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0,0),"cm"))
  
  
  
  df5 = df%>% select(City, u_morn12, u_morn13)
  df6 <- melt(df5, id.vars="City")
  df6$x = c(1,1)
  plot3 = ggplot(df6,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = 0.3)) + 
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("u_morn12" = "#0c2c84", "u_morn13" = "#41b6c4"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0,0),"cm"))
  
  
  plot4 = ggplot(df,aes(x = City,y = (sprawl/0.009491309))) + 
    geom_bar(stat = "identity", width = 1.5, fill = "dark green",
             position = position_dodge(width = 0)) + ylim(0, 1) +
    theme(plot.title = element_blank(),
          panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted"),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0),"cm"))
  
  
  g = arrangeGrob(plot2, plot3, plot1, nrow=3)  
  g1 = arrangeGrob(plot4, g, nrow= 1, widths = c(0.08, 0.92), 
                   bottom=textGrob(i, x = 0.45, y= 0.5, gp = gpar(fontsize = 95)))
  #filepath = paste("D:/Accessibility_study/Final isochrones1/Images/Image sections/city_graphs/",i,"1.png",sep = "")
  filepath = paste("D:/Accessibility_study/Final isochrones1/Images/Image sections/New folder/",i,".png",sep = "")
  ggsave(filepath,g1, bg = "transparent", dpi = 700)
}

#### predicted probability plot during off-peakhours
for (i in shp_cen$City){
  df = shp_cen[shp_cen$City==i,]
  df1 = df%>% select(City, gro_mid12, gro_mid13)
  df2 <- melt(df1, id.vars="City")
  df2$x = c(1,1)
  
  
  plot1 = ggplot(df2,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = .3)) +
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("gro_mid12" = "#99000d", "gro_mid13" = "#fd8d3c"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0.5,0),"cm"))
  
  df3 = df%>% select(City, nu_mid12, nu_mid13)
  df4 <- melt(df3, id.vars="City")
  df4$x = c(1,1)
  
  plot2 = ggplot(df4,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = .3)) +
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("nu_mid12" = "#7a0177", "nu_mid13" = "#c994c7"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0,0),"cm"))
  
  
  
  df5 = df%>% select(City, u_mid12, u_mid13)
  df6 <- melt(df5, id.vars="City")
  df6$x = c(1,1)
  plot3 = ggplot(df6,aes(x = x,y = value,fill = variable)) + 
    geom_bar(stat = "identity", width = .3, 
             position = position_dodge(width = 0.3)) + 
    coord_flip() + ylim(0, 0.15) +
    scale_fill_manual(values=c("u_mid12" = "#0c2c84", "u_mid13" = "#41b6c4"))+
    theme(plot.title = element_blank(),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0.5,0,0,0),"cm"))
  
  
  plot4 = ggplot(df,aes(x = City,y = (sprawl/0.009491309))) + 
    geom_bar(stat = "identity", width = 1.5, fill = "dark green",
             position = position_dodge(width = 0)) + ylim(0, 1) +
    theme(plot.title = element_blank(),
          panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted"),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.box.background = element_rect(fill = "transparent"),
          axis.text =element_blank(),
          axis.title =element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin=unit(c(0,0,0,0),"cm"))
  
  
  g = arrangeGrob(plot2, plot3, plot1, nrow=3)  
  g1 = arrangeGrob(plot4, g, nrow= 1, widths = c(0.08, 0.92), 
                   bottom=textGrob(i, x = 0.45, y= 0.5, gp = gpar(fontsize = 95)))
  filepath = paste("D:/Accessibility_study/Final isochrones1/Images/Image sections/city_graphs/",i,"1_mid.png",sep = "")
  #filepath = paste("D:/Accessibility_study/Final isochrones1/Images/Image sections/New folder/",i,"1_mid.png",sep = "")
  ggsave(filepath,g1, bg = "transparent", dpi = 700)
}
