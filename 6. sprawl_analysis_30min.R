library(geojson)
library(geojsonR)
library(geojsonio)

library(sf)
library(mapdeck)
library(geojsonsf)

library(tidycensus)
library(tidyverse)
library(tigris)
library(magrittr)
library(dplyr)
library(car)
library(ggplot2)
library(directlabels)
library(ggpubr)
library(gridExtra)

#### Area weighted sprawl index for cities using county sprawl index####
#read data
sprawl = read_csv("D:/Accessibility_study/Final isochrones1/sprawl indices2010_counties.csv") #consists of composite compactness indices
study_area = st_read("D:/Accessibility_study/Final isochrones1/Shapefiles_new/Original shapefiles/study_area_edited.shp")

#join county-level sprawl data to study areas
dat = subset(study_area, select=c('GEOID','NAME', 'layer', 'Area_sqm')) 
dat$GEOID_county = ""
len = nrow(dat)
for (i in 1:len){
  if (nchar(dat$GEOID[i])==12){
    dat$GEOID_county[i] = substr(dat$GEOID[i], 1, 5)
  } else{
    dat$GEOID_county[i] = substr(dat$GEOID[i], 1, 4)
  }
}
dat$GEOID_county = as.numeric(dat$GEOID_county)
dat1 = merge(sprawl, dat, by.x = 'CountyFips', by.y = 'GEOID_county')

#estimate area-weighted composite index
city = subset(dat1, select = c('GEOID','layer', 'compositeindex2010', 'Area_sqm'))
city$ind_area = city$compositeindex2010 * city$Area_sqm
city1 = as.data.frame(city %>%
                        group_by(layer) %>% 
                        summarise(GEOID = n(), 
                                  Area_sqm = sum(Area_sqm),
                                  ind_area_sum  = sum(ind_area)) %>%
                        arrange(desc(GEOID)))
city1$weighted_index = city1$ind_area_sum/city1$Area_sqm
city1 = city1[order(city1$layer),]
city_name = c("Ann Arbor" , "Atlanta","Austin" , "Boston" ,
                 "Champaign", "Chicago" ,"Columbus", "Dallas" ,"Denver" ,
                 "Los Angeles","Louisville","Madison", "Miami", "Nashville" ,
                 "New York City" ,"Philadelphia" ,"Phoenix","Portland","San Francisco",
                 "San Jose","Seattle","Salt Lake City")  

city1$City = city_name
Index = subset(city1, select= c(City, weighted_index))

#Inverse of area-weighted composite index to estimate sprawl index
Index$Index = 1/Index$weighted_index

#### Estimating percentage of blockgroups that lost access ####

#read data
he_30_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_morning_u.csv")
he_30_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_midday_u.csv")
#he_30_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_morning_nu.csv")
#he_30_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/health_1800_midday_nu.csv")
gro_30_morn = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_1800_morning.csv")
gro_30_mid = read.csv("D:/Accessibility_study/Final isochrones1/Summary/grocery_1800_midday.csv")

#function to estimate cuts
est_cut = function(data, phase){
  dat = subset(data, select=c('GEOID','NAME', 'Access', 'City', 'Diff', 'shp_sqm'))
  dat1 = dat[dat$Diff == phase,]
  dat1 <- dat1 %>% mutate(Cut = ifelse(Access ==0, 1, 0))
  dat1$Cut = as.factor(dat1$Cut)
  
  ### using percentage of blockgroups
  dat2 = subset(dat1, select = c('City', 'Cut', 'GEOID'))
  dat2 = as.data.frame(dat2 %>%
                         group_by(City, Cut) %>% 
                         summarise(GEOID = n()) %>%
                         arrange(desc(GEOID)))
  
  dat2_cut = dat2[dat2$Cut == 1,]
  dat2_nocut = dat2[dat2$Cut == 0,]
  
  dat3 = merge(dat2_cut, dat2_nocut, by = 'City', all = TRUE)
  dat3$tot_num = dat3$GEOID.x +dat3$GEOID.y
  dat3$pct_cut = dat3$GEOID.x/dat3$tot_num *100
  dat3 = dat3[order(dat3$City),]
  
  final = subset(dat3, select = c('City', 'pct_cut'))
  return(as.data.frame(final))
}

#### Loss in access to health ####
dat_mid12 = est_cut(he_30_mid, "Precovid vs covid")
cut = merge(Index, dat_mid12, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'he_30_mid12'

dat_mid13 = est_cut(he_30_mid, "Precovid vs postcovid")
cut = merge(cut, dat_mid13, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'he_30_mid13'

dat_morn12 = est_cut(he_30_morn, "Precovid vs covid")
cut = merge(cut, dat_morn12, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'he_30_morn12'

dat_morn13 = est_cut(he_30_morn, "Precovid vs postcovid")
cut = merge(cut, dat_morn13, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'he_30_morn13'

cut$diff_mid = cut$he_30_mid13 /cut$he_30_mid12
cut$diff_morn = cut$he_30_morn13 /cut$he_30_morn12


#### Loss in access to grocery ####
dat_mid12 = est_cut(gro_30_mid, "Precovid vs covid")
cut = merge(cut, dat_mid12, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'gro_30_mid12'

dat_mid13 = est_cut(gro_30_mid, "Precovid vs postcovid")
cut = merge(cut, dat_mid13, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'gro_30_mid13'

dat_morn12 = est_cut(gro_30_morn, "Precovid vs covid")
cut = merge(cut, dat_morn12, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'gro_30_morn12'

dat_morn13 = est_cut(gro_30_morn, "Precovid vs postcovid")
cut = merge(cut, dat_morn13, by.x= "City", by.y = "City", all.y = TRUE)
colnames(cut)[colnames(cut) == 'pct_cut'] <- 'gro_30_morn13'

cut$diff_mid_gro = cut$gro_30_mid13 /cut$gro_30_mid12
cut$diff_morn_gro = cut$gro_30_morn13 /cut$gro_30_morn12


#### export file####
write.csv(cut, "D:/Accessibility_study/Final isochrones1/Summary/service_cut_pct_blg_u.csv")
#write.csv(cut, "D:/Accessibility_study/Final isochrones1/Summary/service_cut_pct_blg_nu.csv")

####Sprawl plot####

#function to plot the relationship between urban sprawl index and % of blg with lost accessibility
sprawl_plot = function(data, var, subtitle){
  data = data[data$City != "Boston",]
  figure = ggplot2::ggplot(data, aes(y={{var}}, x=Index)) +  
    geom_point() + ylim(0,35) +
    geom_smooth(method=lm, color="#3182bd", size = 3) + 
    ylab("Percentage of block groups with service cut") +
    xlab("Area-weighted sprawl index") +
    ggtitle(subtitle) + 
    #geom_dl(aes(label = City), method = list(cex = 0.8, rot = 90, hjust = -.2)) + 
    stat_cor(method = "spearman",digits = 2, color = "#000000", size = 9) + #Spearman's correlation and the p-values
    stat_summary(fun.data = count, geom = "text", fun.y = count) + 
    theme_bw() +
    theme(axis.text.y = element_blank(),
          legend.position = "none", axis.title = element_blank()) 
  
  
  return(figure)
}

serv_cut = read.csv('D:/Accessibility_study/Final isochrones1/Summary/service_cut_pct_blg_nu.csv')
cut = serv_cut

he_plot1 = sprawl_plot(cut, he_30_mid12, "he_30_mid12") 
he_plot2 = sprawl_plot(cut, he_30_mid13, "he_30_mid13")
he_plot3 = sprawl_plot(cut, he_30_morn12, "he_30_morn12")
he_plot4 = sprawl_plot(cut, he_30_morn13, "he_30_morn13")

gro_plot1 = sprawl_plot(cut, gro_30_mid12, "gro_30_mid12")
gro_plot2 = sprawl_plot(cut, gro_30_mid13, "gro_30_mid13")
gro_plot3 = sprawl_plot(cut, gro_30_morn12, "gro_30_morn12")
gro_plot4 = sprawl_plot(cut, gro_30_morn13, "gro_30_morn13")

#res 2000*350
grid.arrange(gro_plot3, gro_plot4, he_plot3, he_plot4, nrow=1)
grid.arrange(gro_plot1, gro_plot2, he_plot1, he_plot2, nrow=1)


##### correlation plots ######
dat = read.csv("D:/Accessibility_study/Final isochrones1/Summary/correlation.csv")
dat$hour = dat$ï..hour
dat = mutate(dat, hour_code = if_else(hour == "Peak Hours", 1, 2))
dat1 = dat[dat$time == "30_min" ,]

###plot res (4000*1000)
ggplot(dat1,aes(x = phase,y = -1*(rho), fill = sig)) + 
  geom_bar(stat = "identity", width = 0.4, color = "white",
           position = position_dodge(width = 0.1))+ 
  geom_hline(yintercept=0, color = "grey70") +
  ylab("Correlation coefficients") +
  scale_fill_manual(values=c("Not significant" = "grey60", "Significant" = "#91bfdb")) +
  scale_x_discrete(labels=c("Lockdown", "Post-lockdown")) +
  facet_wrap(type~hour_code, nrow = 1) + 
  theme(panel.background = element_blank(),
        panel.border = element_rect(colour = "grey70", fill=NA, size=1),
        panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted"),
        legend.position = "bottom",
        legend.margin = margin(1,1,1,1),
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 40),
        legend.title = element_blank(), 
        strip.text.x = element_blank(), 
        axis.text = element_text(size = 36), 
        axis.title = element_blank())

rm(list = ls())
