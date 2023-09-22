########################## All pacages ####
setwd("C:/Users/Administrator/Desktop/ѧ������/ռ�ѻ�-����-9.2/Supplementary informations")
library(metafor)
library(ggplot2)
library(ggpmisc)
library(ggthemes)
library(readxl)
library(dplyr)
library(tidyverse)
library(readr)
library(ggsci)
library(grid)
library(ggh4x)
library(performance)
library(patchwork)
library(readxl)

##### Input dataset ###
d1<-read_excel("data_orginal.xlsx",sheet=1)
#dataset information
nrow(unique(d1[c("latitude", "longitude")]))
length(d1$ID)
length(unique(d1$observations))

######################## Fig 1 World map ######
world<-map_data("world")
override.shape <- c(16, 17,17,17)
ggplot() +
  borders('world', colour = "gray50", #地图边界颜色
          fill = "gray95") + #地图边界填充颜色
  geom_polygon(fill = "gray70", colour = "gray50", linetype = "dashed") +
  geom_point(d1, 
             mapping = aes(x=longitude, y = latitude,colour = interaction(Biome1, ecosystem),shape=ecosystem),
             size=2)+
  scale_color_manual(values = c("#2878b5","#c82423","#FF9900","#9ac9db"))+
  labs(x = 'Longitude', y = 'Latitude')+
  
  scale_x_continuous(breaks = c(-200,-100,0,100,200), expand = c(0, 0), 
                     labels = c('200°W', '100°W', '0', '100°E',  '200°E')) +
  scale_y_continuous(breaks = c(-60,  0,  60), expand = c(0, 0), 
                     labels = c('60°S',  '0', '60°N'))+
  theme_bw() +  
  theme(panel.grid = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = 'black'))+ 
  theme(axis.text.x = element_text(size = 16,angle = 0,colour = "black", hjust = 0.5),#x轴坐标大小颜色#family="serif"
        axis.text.y = element_text(size = 16,angle = 0,colour = "black", hjust = 0.5),
        axis.title = element_text(face="bold",size = 18),#family="serif"
        plot.title = element_text(face="bold",size = 16,hjust = 0))+
  guides(colour = guide_legend(override.aes = list(shape = override.shape,size=5)))+
  labs(colour="Ecosystem")+guides(shape = "none")+
  theme(legend.position = c(0.11,0.4),legend.key=element_blank(),
        legend.text =element_text(size=12),legend.title =element_text(size=14))
ggsave("Fig1.pdf", width =8, height =4)



##### Input dataset ###
d2<-read_excel("data_orginal.xlsx",sheet=1)
######################## Data preprocessing ####
attach(d2)
exp_site<-as.factor(exp_site)
observations<-as.factor(observations)
ID<-as.factor(ID)
# climate
latitude2<- log(latitude+46)
longitude2<- log(longitude+156)
MAP2<-log1p(MAP1)
MAT2<-log(MAT1+10)
Alt2 <- log1p(Alt1)
# Initial litter quality
Plant_TOC1 <- log1p(Plant_TOC)
Plant_TN1 <- log1p(Plant_TN)
Plant_TP1 <- log1p(Plant_TP)
Plant_C.N1 <- log1p(Plant_C.N)
Plant_C.P1 <- log1p(Plant_C.P)
Plant_N.P1 <- log1p(Plant_N.P)
Plant_Lignin1 <- log1p(Plant_Lignin)
Lignin.N1 <- log1p(Lignin.N)

# physicochemical properties
PH1 <- log1p(PH)
SOC1 <- log1p(SOC)
STN1 <- log1p(STN)
SCN1 <- log1p(SCN)
Water_temperature1<- log1p(Water_temperature)
DO1<-log1p(DO)
Conductivity1 <- log1p(Conductivity)
NO3.1 <- log1p(NO3)
NH4.1 <- log1p(NH4)
SRP1 <- log1p(SRP)

# Assign a value to d2 #
# Experimental condition
Study_length1 <- log1p(Study_length)
initail_mass1<- log1p(initail_mass)
litterbags_size1<- log1p(litterbags_size)
harvesting_times1<-log1p(harvesting_times)

# climate
d2$MAP2 <- MAP2
d2$MAT2<-MAT2
d2$latitude2<- latitude2
d2$longitude2<- longitude2
d2$Alt2<-Alt2
# Initial litter quality
d2$Plant_TOC1<- Plant_TOC1
d2$Plant_TN1 <- Plant_TN1
d2$Plant_TP1 <- Plant_TP1
d2$Plant_C.N1<- Plant_C.N1
d2$Plant_C.P1<- Plant_C.P1
d2$Plant_N.P1<- Plant_N.P1
d2$Plant_Lignin1 <- Plant_Lignin1
d2$Lignin.N1<- Lignin.N1

# Experimental condition
d2$Study_length1 <- Study_length1
d2$initail_mass1 <- initail_mass1
d2$litterbags_size1 <-litterbags_size1
d2$harvesting_times1 <-harvesting_times1

# Physicochemical properties
d2$PH1 <- PH1
d2$SOC1 <- SOC1
d2$STN1 <- STN1
d2$SCN1 <- SCN1
d2$Water_temperature1<- Water_temperature1
d2$DO1 <- DO1
d2$Conductivity1 <- Conductivity1
d2$NO3.1 <- NO3.1 
d2$NH4.1 <- NH4.1
d2$SRP1 <- SRP1 
d2$exp_site<-exp_site
d2$observations<-observations
d2$ID<-ID
# data screening
dt.terrestrial<- subset(d2,Biome1!="aquatic")
dt.aquatic<- subset(d2,Biome1=="aquatic")
dt.grassland<- subset(d2,Biome1=="grassland")
dt.forest <- subset(d2,Biome1=="forest")
dt.cropland <- subset(d2,Biome1=="cropland")

######################## Effect sizes calculation ####
##overall effect 
r.aquatic1<-rma.mv(yi,vi,data=dt.aquatic,random=~1|ID/observations,method="REML")
summary(r.aquatic1)
r.terrestrial1<-rma.mv(yi,vi,data=dt.terrestrial,random=~1|ID/observations,method="REML")
summary(r.terrestrial1)
make_pct <- function(x) (exp(x) - 1) * 100
Ae.df <- coef(summary(r.aquatic1))
Te.df <- coef(summary(r.terrestrial1))

#### Effects among ecosystem types ###
r.ecosystem.type5 <- rma.mv(yi,vi,mods =~Biome1,random=~1|ID/observations,data=dt.terrestrial,method = "REML")
r.ecosystem.type6<- rma.mv(yi,vi,mods =~Biome1-1,random=~1|ID/observations,data=dt.terrestrial,method = "REML")
summary(r.ecosystem.type5);summary(r.ecosystem.type6)
ecosystem.type2<-coef(summary(r.ecosystem.type2));ecosystem.type6<-coef(summary(r.ecosystem.type6))

#### Effects among climate zoneS ###
r.climate.zones1 <- rma.mv(yi,vi,mods =~climate.zones,random=~1|ID/observations,data=d2,method = "REML")
r.climate.zones2<- rma.mv(yi,vi,mods =~climate.zones-1,random=~1|ID/observations,data=d2,method = "REML")
summary(r.climate.zones1);summary(r.climate.zones2)

r.climate.zones3 <- rma.mv(yi,vi,mods =~climate.zones,random=~1|ID/observations,data=dt.aquatic,method = "REML")
r.climate.zones4<- rma.mv(yi,vi,mods =~climate.zones-1,random=~1|ID/observations,data=dt.aquatic,method = "REML")
summary(r.climate.zones3);summary(r.climate.zones4)

r.climate.zones5 <- rma.mv(yi,vi,mods =~climate.zones,random=~1|ID/observations,data=dt.terrestrial,method = "REML")
r.climate.zones6<- rma.mv(yi,vi,mods =~climate.zones-1,random=~1|ID/observations,data=dt.terrestrial,method = "REML")
summary(r.climate.zones5);summary(r.climate.zones6)


r.climate.zones7 <- rma.mv(yi,vi,mods =~climate.zones,random=~1|ID/observations,data=dt.forest,method = "REML")
r.climate.zones8<- rma.mv(yi,vi,mods =~climate.zones-1,random=~1|ID/observations,data=dt.forest,method = "REML")
summary(r.climate.zones7);summary(r.climate.zones8)

r.climate.zones9 <- rma.mv(yi,vi,mods =~climate.zones,random=~1|ID/observations,data=dt.cropland,method = "REML")
r.climate.zones10<- rma.mv(yi,vi,mods =~climate.zones-1,random=~1|ID/observations,data=dt.cropland,method = "REML")
summary(r.climate.zones9);summary(r.climate.zones10)

#### Effects among study lengh levels ###
r.length_level3 <- rma.mv(yi,vi,mods =~Alength_level,data=dt.aquatic,random=~1|ID/observations,method = "REML")
r.length_level4<- rma.mv(yi,vi,mods =~Alength_level-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(r.length_level3);summary(r.length_level4)

r.length_level5 <- rma.mv(yi,vi,mods =~length_level,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
r.length_level6<- rma.mv(yi,vi,mods =~length_level-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(r.length_level5);summary(r.length_level6)

r.length_level7 <- rma.mv(yi,vi,mods =~length_level,data=dt.forest,random=~1|ID/observations,method = "REML")
r.length_level8<- rma.mv(yi,vi,mods =~length_level-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(r.length_level7);summary(r.length_level8)

r.length_level9 <- rma.mv(yi,vi,mods =~length_level,data=dt.cropland,random=~1|ID/observations,method = "REML")
r.length_level10<- rma.mv(yi,vi,mods =~length_level-1,data=dt.cropland,method = "REML")
summary(r.length_level9);summary(r.length_level10)

r.length_level11 <- rma.mv(yi,vi,mods =~length_level,data=dt.grassland,random=~1|ID/observations,method = "REML")
r.length_level12<- rma.mv(yi,vi,mods =~length_level-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(r.length_level11);summary(r.length_level12)

#### Effects among precipitation levels ###
r.pre1 <- rma.mv(yi,vi,mods =~pre_level,data=d2,random=~1|ID/observations,method = "REML")
r.pre2<- rma.mv(yi,vi,mods =~pre_level-1,data=d2,random=~1|ID/observations,method = "REML")
summary(r.pre1);summary(r.pre2)

r.pre3 <- rma.mv(yi,vi,mods =~pre_level,data=dt.aquatic,random=~1|ID/observations,method = "REML")
r.pre4<- rma.mv(yi,vi,mods =~pre_level-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(r.pre3);summary(r.pre4)

r.pre5 <- rma.mv(yi,vi,mods =~pre_level,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
r.pre6<- rma.mv(yi,vi,mods =~pre_level-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(r.pre5);summary(r.pre6)

r.pre_level7 <- rma.mv(yi,vi,mods =~pre_level,data=dt.forest,random=~1|ID/observations,method = "REML")
r.pre_level8<- rma.mv(yi,vi,mods =~pre_level-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(r.pre_level7);summary(r.pre_level8)

r.pre_level9 <- rma.mv(yi,vi,mods =~pre_level,data=dt.cropland,random=~1|ID/observations,method = "REML")
r.pre_level10<- rma.mv(yi,vi,mods =~pre_level-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(r.pre_level9);summary(r.pre_level10)

r.pre_level11 <- rma.mv(yi,vi,mods =~pre_level,data=dt.grassland,random=~1|ID/observations,method = "REML")
r.pre_level12<- rma.mv(yi,vi,mods =~pre_level-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(r.pre_level11);summary(r.pre_level12)

#### Effects among fauna exclusion types ###
fauna.exclusion.types1 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,random=~1|ID/observations,data=d2,method = "REML")
fauna.exclusion.types2<- rma.mv(yi,vi,mods =~fauna.exclusion.types-1,random=~1|ID/observations,data=d2,method = "REML")
summary(fauna.exclusion.types1);summary(fauna.exclusion.types2)

fauna.exclusion.types3 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,data=dt.aquatic,random=~1|ID/observations,method = "REML")
fauna.exclusion.types4<- rma.mv(yi,vi,mods =~fauna.exclusion.types-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(fauna.exclusion.types3);summary(fauna.exclusion.types4)

fauna.exclusion.types5 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
fauna.exclusion.types6<-rma.mv(yi,vi,mods =~fauna.exclusion.types-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(fauna.exclusion.types5);summary(fauna.exclusion.types6)

fauna.exclusion.types7 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,data=dt.forest,random=~1|ID/observations,method = "REML")
fauna.exclusion.types8<- rma.mv(yi,vi,mods =~fauna.exclusion.types-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(fauna.exclusion.types7);summary(fauna.exclusion.types8)

fauna.exclusion.types9 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,data=dt.cropland,random=~1|ID/observations,method = "REML")
fauna.exclusion.types10<- rma.mv(yi,vi,mods =~fauna.exclusion.types-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(fauna.exclusion.types9);summary(fauna.exclusion.types10)

fauna.exclusion.types11 <- rma.mv(yi,vi,mods =~fauna.exclusion.types,data=dt.grassland,random=~1|ID,method = "REML")
fauna.exclusion.types12<- rma.mv(yi,vi,mods =~fauna.exclusion.types-1,data=dt.grassland,random=~1|ID,method = "REML")
summary(fauna.exclusion.types11);summary(fauna.exclusion.types12)

## Extract coefficients and confidence intervals about fig 2 ###
climate.zones2<-coef(summary(r.climate.zones2));climate.zones4<-coef(summary(r.climate.zones4));climate.zones6<-coef(summary(r.climate.zones6));
climate.zones8<-coef(summary(r.climate.zones8));climate.zones10<-coef(summary(r.climate.zones10))
length_level4<-coef(summary(r.length_level4));length_level6<-coef(summary(r.length_level6));length_level8<-coef(summary(r.length_level8));
length_level10<-coef(summary(r.length_level10));length_level12<-coef(summary(r.length_level12))
pre_level2<-coef(summary(r.pre2));pre_level4<-coef(summary(r.pre4));pre_level6<-coef(summary(r.pre6));
pre_level8<-coef(summary(r.pre_level8));pre_level10<-coef(summary(r.pre_level10));pre_level12<-coef(summary(r.pre_level12))
df.fauna.exclusion.types2<-coef(summary(fauna.exclusion.types2));df.fauna.exclusion.types4<-coef(summary(fauna.exclusion.types4));df.fauna.exclusion.types6<-coef(summary(fauna.exclusion.types6));
df.fauna.exclusion.types8<-coef(summary(fauna.exclusion.types8));df.fauna.exclusion.types10<-coef(summary(fauna.exclusion.types10));df.fauna.exclusion.types12<-coef(summary(fauna.exclusion.types12))

forest1.df <- bind_rows(Global.df, Ae.df ,Te.df,
                        ecosystem.type2,ecosystem.type6,
                        climate.zones2,climate.zones4,climate.zones6,climate.zones8,climate.zones10,
                        length_level4,length_level6,length_level8,length_level10,length_level12,
                        pre_level2,pre_level4,pre_level6,pre_level8,pre_level10,pre_level12,
                        fauna.exclusion.types2,fauna.exclusion.types4,fauna.exclusion.types6,fauna.exclusion.types8,fauna.exclusion.types10,fauna.exclusion.types12)
openxlsx::write.xlsx(forest1.df,file = "D:/meta_analysis/aboutR/meta2023-07/data_forest1.xlsx")

#### Effects among litter types ###
litter_type1 <- rma.mv(yi,vi,mods =~litter_type,random=~1|ID/observations,data=d2,method = "REML")
litter_type2<- rma.mv(yi,vi,mods =~litter_type-1,random=~1|ID/observations,data=d2,method = "REML")
summary(litter_type1);summary(litter_type2)

litter_type3 <- rma.mv(yi,vi,mods =~litter_type,data=dt.aquatic,random=~1|ID/observations,method = "REML")
litter_type4<- rma.mv(yi,vi,mods =~litter_type-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(litter_type3);summary(litter_type4)

litter_type5 <- rma.mv(yi,vi,mods =~litter_type,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
litter_type6<-rma.mv(yi,vi,mods =~litter_type-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(litter_type5);summary(litter_type6)

litter_type7 <- rma.mv(yi,vi,mods =~litter_type,data=dt.forest,random=~1|ID/observations,method = "REML")
litter_type8<- rma.mv(yi,vi,mods =~litter_type-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(litter_type7);summary(litter_type8)

litter_type9 <- rma.mv(yi,vi,mods =~litter_type,data=dt.cropland,random=~1|ID/observations,method = "REML")
litter_type10<- rma.mv(yi,vi,mods =~litter_type-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(litter_type9);summary(litter_type10)

litter_type11 <- rma.mv(yi,vi,mods =~litter_type,data=dt.grassland,random=~1|ID/observations,method = "REML")
litter_type12<- rma.mv(yi,vi,mods =~litter_type-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(litter_type11);summary(litter_type12)

##### Effects among litter forms ###
litter_form1 <- rma.mv(yi,vi,mods =~litter_form1,random=~1|ID/observations,data=d2,method = "REML")
litter_form2<- rma.mv(yi,vi,mods =~litter_form1-1,random=~1|ID/observations,data=d2,method = "REML")
summary(litter_form1);summary(litter_form2)

litter_form3 <- rma.mv(yi,vi,mods =~litter_form1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
litter_form4<- rma.mv(yi,vi,mods =~litter_form1-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(litter_form3);summary(litter_form4)

litter_form5 <- rma.mv(yi,vi,mods =~litter_form1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
litter_form6<-rma.mv(yi,vi,mods =~litter_form1-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(litter_form5);summary(litter_form6)

litter_form7 <- rma.mv(yi,vi,mods =~litter_form,data=dt.forest,random=~1|ID/observations,method = "REML")
litter_form8<- rma.mv(yi,vi,mods =~litter_form-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(litter_form7);summary(litter_form8)

litter_form9 <- rma.mv(yi,vi,mods =~litter_form,data=dt.cropland,random=~1|ID/observations,method = "REML")
litter_form10<- rma.mv(yi,vi,mods =~litter_form-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(litter_form9);summary(litter_form10)

litter_form11 <- rma.mv(yi,vi,mods =~litter_form,data=dt.grassland,random=~1|ID/observations,method = "REML")
litter_form12<- rma.mv(yi,vi,mods =~litter_form-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(litter_form11);summary(litter_form12)

#### Effects among growth forms ###
growth_form1 <- rma.mv(yi,vi,mods =~growth_form,random=~1|ID/observations,data=d2,method = "REML")
growth_form2<- rma.mv(yi,vi,mods =~growth_form-1,random=~1|ID/observations,data=d2,method = "REML")
summary(growth_form1);summary(growth_form2)

growth_form3 <- rma.mv(yi,vi,mods =~growth_form,data=dt.aquatic,random=~1|ID/observations,method = "REML")
growth_form4<- rma.mv(yi,vi,mods =~growth_form-1,data=dt.aquatic,random=~1|ID/observations,method = "REML")
summary(growth_form3);summary(growth_form4)

growth_form5 <- rma.mv(yi,vi,mods =~growth_form,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
growth_form6<-rma.mv(yi,vi,mods =~growth_form-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(growth_form5);summary(growth_form6)

growth_form7 <- rma.mv(yi,vi,mods =~growth_form,data=dt.forest,random=~1|ID/observations,method = "REML")
growth_form8<- rma.mv(yi,vi,mods =~growth_form-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(growth_form7);summary(growth_form8)

growth_form9 <- rma.mv(yi,vi,mods =~growth_form,data=dt.cropland,random=~1|ID/observations,method = "REML")
growth_form10<- rma.mv(yi,vi,mods =~growth_form-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(growth_form9);summary(growth_form10)

growth_form11 <- rma.mv(yi,vi,mods =~growth_form,data=dt.grassland,random=~1|ID/observations,method = "REML")
growth_form12<- rma.mv(yi,vi,mods =~growth_form-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(growth_form11);summary(growth_form12)

##### Effects among placements ###
placement5 <- rma.mv(yi,vi,mods =~placement,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
placement6<-rma.mv(yi,vi,mods =~placement-1,data=dt.terrestrial,random=~1|ID/observations,method = "REML")
summary(placement5);summary(placement6)

placement7 <- rma.mv(yi,vi,mods =~placement,data=dt.forest,random=~1|ID/observations,method = "REML")
placement8<- rma.mv(yi,vi,mods =~placement-1,data=dt.forest,random=~1|ID/observations,method = "REML")
summary(placement7);summary(placement8)

placement9 <- rma.mv(yi,vi,mods =~placement,data=dt.cropland,random=~1|ID/observations,method = "REML")
placement10<- rma.mv(yi,vi,mods =~placement-1,data=dt.cropland,random=~1|ID/observations,method = "REML")
summary(placement9);summary(placement10)

placement11 <- rma.mv(yi,vi,mods =~placement,data=dt.grassland,random=~1|ID/observations,method = "REML")
placement12<- rma.mv(yi,vi,mods =~placement-1,data=dt.grassland,random=~1|ID/observations,method = "REML")
summary(placement11);summary(placement12)

## Extract coefficients and confidence intervals about fig 3 ###
df.litter_type2<-coef(summary(litter_type2));df.litter_type4<-coef(summary(litter_type4));df.litter_type6<-coef(summary(litter_type6));
df.litter_type8<-coef(summary(litter_type8));df.litter_type10<-coef(summary(litter_type10));df.litter_type12<-coef(summary(litter_type12))
df.litter_form2<-coef(summary(litter_form2));df.litter_form4<-coef(summary(litter_form4));df.litter_form6<-coef(summary(litter_form6));
df.litter_form8<-coef(summary(litter_form8));df.litter_form10<-coef(summary(litter_form10));df.litter_form12<-coef(summary(litter_form12))
df.growth_form2<-coef(summary(growth_form2));df.growth_form4<-coef(summary(growth_form4));df.growth_form6<-coef(summary(growth_form6));
df.growth_form8<-coef(summary(growth_form8));df.growth_form10<-coef(summary(growth_form10));df.growth_form12<-coef(summary(growth_form12))
df.placement6<-coef(summary(placement6));df.placement8<-coef(summary(placement8));df.placement10<-coef(summary(placement10));df.placement12<-coef(summary(placement12))

forest2.df <- bind_rows(df.litter_type2,df.litter_type4,df.litter_type6,df.litter_type8,df.litter_type10,df.litter_type12,
                        df.litter_form2,df.litter_form4,df.litter_form6,df.litter_form8,df.litter_form10,df.litter_form12,
                        df.growth_form2,df.growth_form4,df.growth_form6,df.growth_form8,df.growth_form10,df.growth_form12,
                        df.placement6,df.placement8,df.placement10,df.placement12)
openxlsx::write.xlsx(forest2.df,file = "D:/meta_analysis/aboutR/meta2023-07/data_forest2.xlsx")

######################## Fig 2 ####
##TE
dataset2.1<-read_excel("effectsize1.xlsx",sheet=2)
P2.1<- ggplot(dataset2.1, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height =0.0)+
  scale_y_discrete(limits=c(">12month","6-12month","<6month","Duration levels",
                            "Macrofauna and Mesofauna","Mesofauna","Macrofauna",
                            "Fauna exclusion types",">2000mm/year","1000-2000mm/year",
                            "<1000mm/year","Precipitation levels","Grassland","Forest",
                            "Cropland","Ecosystem types", "Tropical","Temperate","Cold",
                            "Climate zones","Overall"))+
  scale_color_manual(values = c("#c82423","#2878b5","#008000","#9ac9db","#FF9900","#3C5488FF"))+
  scale_x_continuous(limits= c(-0.75, 0.8))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 4),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 8),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 12),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 16),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 20),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.5,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=0.02,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  ggtitle("Terrestrial")+theme(plot.title = element_text(hjust = 0.5))
P2.1

##AE
dataset2.2<-read_excel("effectsize1.xlsx",sheet=3)
P2.2<- ggplot(dataset2.2, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c(">3month","1-3month","<1month","Duration levels",
                            "Macrofauna and Mesofauna","Mesofauna","Macrofauna",
                            "Fauna exclusion types",">2000mm/year","1000-2000mm/year",
                            "<1000mm/year","Precipitation levels", "Tropical","Temperate","Cold",
                            "Climate zones","Overall"))+
  scale_color_manual(values = c("#c82423","#2878b5","#9ac9db","#FF9900","#3C5488FF"))+
  scale_x_continuous(limits= c(-1.1, 0.8))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 4),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 8),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 12),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 16),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.5,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=0.02,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  theme(legend.position = 'none')+
  ggtitle("Aquatic")+theme(plot.title = element_text(hjust = 0.5))
P2.2 

pdf("Fig2a-b.pdf", width =14, height =6)   
grid.newpage()
pushViewport(viewport(layout =grid.layout(1, 2)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P2.1, vp = vplayout(1, 1))
print(P2.2, vp = vplayout(1, 2))
dev.off()

##### Forest;Cropland;Grassland ###
dataset2.3<-read_excel("effectsize1.xlsx",sheet=4)
P2.3 <- ggplot(dataset2.3, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c(">12month","6-12month","<6month","Duration level",
                            "Macrofauna and Mesofauna","Mesofauna","Macrofauna",
                            "Fauna exclusion types",">2000mm/year","1000-2000mm/year",
                            "<1000mm/year","Precipitation level", "Tropical","Temperate","Cold",
                            "Climate zones"))+
  scale_color_manual(values = c("#c82423","#2878b5","#FF9900","#3C5488FF"))+
  scale_x_continuous(limits= c(-0.85, 0.8))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 4),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 8),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 12),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 16),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.5,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=0.02,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  theme(legend.position = 'none')+
  ggtitle("Forest")+theme(plot.title = element_text(hjust = 0.5))
P2.3

dataset2.4<-read_excel("effectsize1.xlsx",sheet=5)
P2.4 <- ggplot(dataset2.4, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c(">12month","6-12month","<6month","Duration levels",
                            "Macrofauna and Mesofauna","Mesofauna","Macrofauna",
                            "Fauna exclusion types",">2000mm/year","1000-2000mm/year",
                            "<1000mm/year","Precipitation levels", "Tropical","Temperate","Cold",
                            "Climate zones"))+
  scale_color_manual(values = c("#c82423","#2878b5","#FF9900","#3C5488FF"))+
  scale_x_continuous(limits= c(-1.6, 1))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 4),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 8),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 12),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 16),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.5,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=0.02,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  theme(legend.position = 'none')+
  ggtitle("Cropland")+theme(plot.title = element_text(hjust = 0.5))
P2.4

dataset2.5<-read_excel("effectsize1.xlsx",sheet=6)
P2.5 <- ggplot(dataset2.5, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c(">12month","6-12month","<6month","Duration levels",
                            "Macrofauna and Mesofauna","Mesofauna","Macrofauna",
                            "Fauna exclusion types",">2000mm/year","1000-2000mm/year",
                            "<1000mm/year","Precipitation levels", "Tropical","Temperate","Cold",
                            "Climate zones"))+
  scale_color_manual(values = c("#c82423","#2878b5","#FF9900","#3C5488FF"))+
  scale_x_continuous(limits= c(-1.3, 0.8))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 4),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 8),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 12),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.4,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=0.02,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  theme(legend.position = 'none')+
  ggtitle("Grassland")+theme(plot.title = element_text(hjust = 0.5))
P2.5

pdf("Fig2c-e.pdf", width =13, height =5)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P2.3, vp = vplayout(1, 1))
print(P2.4, vp = vplayout(1, 2))
print(P2.5, vp = vplayout(1, 2))
dev.off()

######################## Fig 3 ####
##TE
dataset2.7<-read_excel("effectsize1.xlsx",sheet=8)
P2.7 <- ggplot(dataset2.7, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c("Surface","Buried","Placements","Woody","Herbaceous","Growth forms","Stem","Root","Leaf + Stem",
                            "Leaf","Litter forms","Single","Mix","Litter types"))+
  scale_color_manual(values = c("#c82423","#9ac9db","#FF9900","#2878b5"))+
  scale_x_continuous(limits= c(-0.9,0.9))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 3),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 6),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 11),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 14),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.4,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=-0.1,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  theme(legend.position = 'none')+
  ggtitle("Terrestrail")+theme(plot.title = element_text(hjust = 0.5))
P2.7

##AE
dataset2.8<-read_excel("effectsize1.xlsx",sheet=9)
P2.8 <- ggplot(dataset2.8, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c("Woody","Herbaceous","Growth formss","Stem","Root","Leaf + Stem",
                            "Leaf","Litter forms","Single","Mix","Litter types"))+
  scale_x_continuous(limits= c(-1.6, 2))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 3),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 6),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 11),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 14),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=1.,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=-0.13,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  scale_color_manual(values = c("#c82423","#9ac9db","#FF9900"))+
  theme(legend.position = 'none')+
  ggtitle("Aquatic")+theme(plot.title = element_text(hjust = 0.5))
P2.8

pdf("Fig3a-b.pdf", width =10, height =5)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P2.7, vp = vplayout(1, 1))
print(P2.8, vp = vplayout(1, 2))
dev.off()

######forest;cropland;grassland###
dataset2.9<-read_excel("effectsize1.xlsx",sheet=10)
P2.9 <- ggplot(dataset2.9, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c("Surface","Buried","Placements","Woody","Herbaceous","Growth forms","Stem","Root","Leaf + Stem",
                            "Leaf","Litter forms","Single","Mix","Litter types"))+
  scale_x_continuous(limits= c(-1.3,1.3))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 3),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 6),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 11),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 14),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.9,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=-0.13,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  scale_color_manual(values = c("#c82423","#9ac9db","#FF9900","#2878b5"))+
  theme(legend.position = 'none')+
  ggtitle("Forest")+theme(plot.title = element_text(hjust = 0.5))
P2.9

dataset2.10<-read_excel("effectsize1.xlsx",sheet=11)
P2.10 <- ggplot(dataset2.10, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c("Surface","Buried","Placements","Woody","Herbaceous","Growth forms","Stem","Root","Leaf + Stem",
                            "Leaf","Litter forms","Single","Mix","Litter types"))+
  scale_x_continuous(limits= c(-1.1,0.5))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 3),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 6),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 11),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 14),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.35,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=-0.13,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=14))+
  scale_color_manual(values = c("#c82423","#9ac9db","#FF9900","#2878b5"))+
  theme(legend.position = 'none')+
  ggtitle("Cropland")+theme(plot.title = element_text(hjust = 0.5))
P2.10

dataset2.11<-read_excel("effectsize1.xlsx",sheet=12)
P2.11 <- ggplot(dataset2.11, aes(x=Estimate, y=index,color=group))+
  geom_point( aes(color = group), size=4)+
  geom_errorbarh(aes(xmax = ci.ub, xmin =ci.lb, colour=group),size= 0.5,height = 0)+
  scale_y_discrete(limits=c("Surface","Buried","Placements","Woody","Herbaceous","Growth forms","Stem","Root","Leaf + Stem",
                            "Leaf","Litter forms","Single","Mix","Litter types"))+
  scale_x_continuous(limits= c(-1.55,1.2))+
  geom_vline(aes(xintercept = 0),color="gray",linetype="dashed", size = 1.5) +
  geom_hline(aes(yintercept = 3),color="gray",linetype="dashed", size = 0.5) +
  geom_hline(aes(yintercept = 6),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 11),color="gray",linetype="dashed", size = 0.5)+
  geom_hline(aes(yintercept = 14),color="gray",linetype="dashed", size = 0.5)+
  xlab('Effect size (lnRR)')+ 
  ylab(' ')+
  theme_few()+
  geom_text(aes(label=number,y=index,x=0.8,hjust=0),size=6,color = "black")+
  geom_text(aes(label=significance,y=index,x=-0.13,hjust=0),size=6,color = "black")+
  theme(axis.text.x = element_text(size = 18, color = "black"))+
  theme(axis.text.y = element_text(size = 18, color = "black"))+
  theme(title=element_text(size=18))+
  scale_color_manual(values = c("#c82423","#9ac9db","#FF9900","#2878b5"))+
  theme(legend.position = 'none')+
  ggtitle("Grassland")+theme(plot.title = element_text(hjust = 0.5))
P2.11

pdf("Fig3c-e.pdf", width =13, height =5)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P2.9, vp = vplayout(1, 1))
print(P2.10, vp = vplayout(1, 2))
print(P2.11, vp = vplayout(1, 3))
dev.off()

############## Meta regression analysis ####
################### Terrestrial ####
str(dt.terrestrial)
dtte_biome<-dt.terrestrial %>% 
  pivot_wider(names_from = Biome1,values_from = c("yi":"SCN1"))%>%
  dplyr::select(68:154)
################### Statistics (r and P values) for Fig 4 and TableS2 ####
# Climate
# MAP
lm.MAP<- lm(yi~MAP2,data=dt.terrestrial,na.action=na.omit) 
nlm.MAP<- lm(yi~poly(MAP2,2,raw=T),data=dt.terrestrial,na.action=na.omit)
AIC(lm.MAP)
AIC(nlm.MAP)
summary(lm.MAP)
summary(nlm.MAP)

lm.MAP<- lm(yi~MAP2,data=dt.forest,na.action=na.omit) 
nlm.MAP<- lm(yi~poly(MAP2,2,raw=T),data=dt.forest,na.action=na.omit)
AIC(lm.MAP)
AIC(nlm.MAP)
summary(lm.MAP)
summary(nlm.MAP)

lm.MAP<- lm(yi~MAP2,data=dt.cropland,na.action=na.omit) 
nlm.MAP<- lm(yi~poly(MAP2,2,raw=T),data=dt.cropland,na.action=na.omit)
AIC(lm.MAP)
AIC(nlm.MAP)
summary(lm.MAP)
summary(nlm.MAP)

lm.MAP<- lm(yi~MAP2,data=dt.grassland,na.action=na.omit) 
nlm.MAP<- lm(yi~poly(MAP2,2,raw=T),data=dt.grassland,na.action=na.omit)
AIC(lm.MAP)
AIC(nlm.MAP)
summary(lm.MAP)
summary(nlm.MAP)

# MAT
lm.MAT<- lm(yi~MAT2,data=dt.terrestrial,na.action=na.omit) 
nlm.MAT<- lm(yi~poly(MAT2,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.MAT)
AIC(nlm.MAT)
anova(lm.MAT,nlm.MAT)
summary(lm.MAT)
summary(nlm.MAT)

lm.MAT<- lm(yi~MAT2,data=dt.forest,na.action=na.omit) 
nlm.MAT<- lm(yi~poly(MAT2,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.MAT)
AIC(nlm.MAT)
anova(lm.MAT,nlm.MAT)
summary(lm.MAT)
summary(nlm.MAT)

lm.MAT<- lm(yi~MAT2,data=dt.cropland,na.action=na.omit) 
nlm.MAT<- lm(yi~poly(MAT2,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.MAT)
AIC(nlm.MAT)
anova(lm.MAT,nlm.MAT)
summary(lm.MAT)
summary(nlm.MAT)

lm.MAT<- lm(yi~MAT2,data=dt.grassland,na.action=na.omit) 
nlm.MAT<- lm(yi~poly(MAT2,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.MAT)
AIC(nlm.MAT)
summary(lm.MAT)
summary(nlm.MAT)

# Alt
lm.Alt<- lm(yi~Alt2,data=dt.terrestrial,na.action=na.omit) 
nlm.Alt<- lm(yi~poly(Alt2,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Alt)
AIC(nlm.Alt)
summary(lm.Alt)
summary(nlm.Alt)

lm.Alt<- lm(yi~Alt2,data=dt.forest,na.action=na.omit) 
nlm.Alt<- lm(yi~poly(Alt2,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Alt)
AIC(nlm.Alt)
summary(lm.Alt)
summary(nlm.Alt)

lm.Alt<- lm(yi~Alt2,data=dt.cropland,na.action=na.omit) 
nlm.Alt<- lm(yi~poly(Alt2,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Alt)
AIC(nlm.Alt)
summary(lm.Alt)
summary(nlm.Alt)

lm.Alt<- lm(yi~Alt2,data=dt.grassland,na.action=na.omit) 
nlm.Alt<- lm(yi~poly(Alt2,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Alt)
AIC(nlm.Alt)
anova(lm.Alt,nlm.Alt)
summary(lm.Alt)
summary(nlm.Alt)

# Latitude
lm.latitude<- lm(yi~latitude2,data=dt.terrestrial,na.action=na.omit) 
nlm.latitude<- lm(yi~poly(latitude2,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.latitude)
AIC(nlm.latitude)
anova(lm.latitude,nlm.latitude)
summary(lm.latitude)
summary(nlm.latitude)

lm.latitude<- lm(yi~latitude2,data=dt.forest,na.action=na.omit) 
nlm.latitude<- lm(yi~poly(latitude2,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.latitude)
AIC(nlm.latitude)
anova(lm.latitude,nlm.latitude)
summary(lm.latitude)
summary(nlm.latitude)

lm.latitude<- lm(yi~latitude2,data=dt.cropland,na.action=na.omit) 
nlm.latitude<- lm(yi~poly(latitude2,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.latitude)
AIC(nlm.latitude)
summary(lm.latitude)
summary(nlm.latitude)

lm.latitude<- lm(yi~latitude2,data=dt.grassland,na.action=na.omit) 
nlm.latitude<- lm(yi~poly(latitude2,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.latitude)
AIC(nlm.latitude)
summary(lm.latitude)
summary(nlm.latitude)

# Longitude
lm.longitude<- lm(yi~longitude2,data=dt.terrestrial,na.action=na.omit) 
nlm.longitude<- lm(yi~poly(longitude2,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.longitude)
AIC(nlm.longitude)
summary(lm.longitude)
summary(nlm.longitude)

lm.longitude<- lm(yi~longitude2,data=dt.forest,na.action=na.omit) 
nlm.longitude<- lm(yi~poly(longitude2,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.longitude)
AIC(nlm.longitude)
summary(lm.longitude)
summary(nlm.longitude)

lm.longitude<- lm(yi~longitude2,data=dt.cropland,na.action=na.omit) 
nlm.longitude<- lm(yi~poly(longitude2,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.longitude)
AIC(nlm.longitude)
anova(lm.longitude,nlm.longitude)
summary(lm.longitude)
summary(nlm.longitude)

lm.longitude<- lm(yi~longitude2,data=dt.grassland,na.action=na.omit) 
nlm.longitude<- lm(yi~poly(longitude2,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.longitude)
AIC(nlm.longitude)
summary(lm.longitude)
summary(nlm.longitude)

# Experimental condition
# Study length
lm.Study_length<- lm(yi~Study_length1,data=dt.terrestrial,na.action=na.omit) 
nlm.Study_length<- lm(yi~poly(Study_length1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Study_length)
AIC(nlm.Study_length)
anova(lm.Study_length,nlm.Study_length)
summary(lm.Study_length)
summary(nlm.Study_length)

lm.Study_length<- lm(yi~Study_length1,data=dt.forest,na.action=na.omit) 
nlm.Study_length<- lm(yi~poly(Study_length1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Study_length)
AIC(nlm.Study_length)
anova(lm.Study_length,nlm.Study_length)
summary(lm.Study_length)
summary(nlm.Study_length)

lm.Study_length<- lm(yi~Study_length1,data=dt.cropland,na.action=na.omit) 
nlm.Study_length<- lm(yi~poly(Study_length1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Study_length)
AIC(nlm.Study_length)
anova(lm.Study_length,nlm.Study_length)
summary(lm.Study_length)
summary(nlm.Study_length)

lm.Study_length<- lm(yi~Study_length1,data=dt.grassland,na.action=na.omit) 
nlm.Study_length<- lm(yi~poly(Study_length1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Study_length)
AIC(nlm.Study_length)
anova(lm.Study_length,nlm.Study_length)
summary(lm.Study_length)
summary(nlm.Study_length)

# harvesting times
lm.harvesting_times<- lm(yi~harvesting_times1,data=dt.terrestrial,na.action=na.omit) 
nlm.harvesting_times<- lm(yi~poly(harvesting_times1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.harvesting_times)
AIC(nlm.harvesting_times)
anova(lm.harvesting_times,nlm.harvesting_times)
summary(lm.harvesting_times)
summary(nlm.harvesting_times)

lm.harvesting_times<- lm(yi~harvesting_times1,data=dt.forest,na.action=na.omit) 
nlm.harvesting_times<- lm(yi~poly(harvesting_times1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.harvesting_times)
AIC(nlm.harvesting_times)
anova(lm.harvesting_times,nlm.harvesting_times)
summary(lm.harvesting_times)
summary(nlm.harvesting_times)

lm.harvesting_times<- lm(yi~harvesting_times1,data=dt.cropland,na.action=na.omit) 
nlm.harvesting_times<- lm(yi~poly(harvesting_times1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.harvesting_times)
AIC(nlm.harvesting_times)
summary(lm.harvesting_times)
summary(nlm.harvesting_times)

lm.harvesting_times<- lm(yi~harvesting_times1,data=dt.grassland,na.action=na.omit) 
nlm.harvesting_times<- lm(yi~poly(harvesting_times1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.harvesting_times)
AIC(nlm.harvesting_times)
summary(lm.harvesting_times)
summary(nlm.harvesting_times)

# initail mass
lm.initail_mass<- lm(yi~initail_mass1,data=dt.terrestrial,na.action=na.omit) 
nlm.initail_mass<- lm(yi~poly(initail_mass1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.initail_mass)
AIC(nlm.initail_mass)
anova(lm.initail_mass,nlm.initail_mass)
summary(lm.initail_mass)
summary(nlm.initail_mass)

lm.initail_mass<- lm(yi~initail_mass1,data=dt.forest,na.action=na.omit) 
nlm.initail_mass<- lm(yi~poly(initail_mass1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.initail_mass)
AIC(nlm.initail_mass)
summary(lm.initail_mass)
summary(nlm.initail_mass)

lm.initail_mass<- lm(yi~initail_mass1,data=dt.cropland,na.action=na.omit) 
nlm.initail_mass<- lm(yi~poly(initail_mass1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.initail_mass)
AIC(nlm.initail_mass)
anova(lm.initail_mass,nlm.initail_mass)
summary(lm.initail_mass)
summary(nlm.initail_mass)

lm.initail_mass<- lm(yi~initail_mass1,data=dt.grassland,na.action=na.omit) 
nlm.initail_mass<- lm(yi~poly(initail_mass1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.initail_mass)
AIC(nlm.initail_mass)
anova(lm.initail_mass,nlm.initail_mass)
summary(lm.initail_mass)
summary(nlm.initail_mass)

# litterbags size
lm.litterbags_size<- lm(yi~litterbags_size1,data=dt.terrestrial,na.action=na.omit) 
nlm.litterbags_size<- lm(yi~poly(litterbags_size1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.litterbags_size)
AIC(nlm.litterbags_size)
summary(lm.litterbags_size)
summary(nlm.litterbags_size)

lm.litterbags_size<- lm(yi~litterbags_size1,data=dt.forest,na.action=na.omit) 
nlm.litterbags_size<- lm(yi~poly(litterbags_size1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.litterbags_size)
AIC(nlm.litterbags_size)
anova(lm.litterbags_size,nlm.litterbags_size)
summary(lm.litterbags_size)
summary(nlm.litterbags_size)

lm.litterbags_size<- lm(yi~litterbags_size1,data=dt.cropland,na.action=na.omit) 
nlm.litterbags_size<- lm(yi~poly(litterbags_size1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.litterbags_size)
AIC(nlm.litterbags_size)
summary(lm.litterbags_size)
summary(nlm.litterbags_size)

lm.litterbags_size<- lm(yi~litterbags_size1,data=dt.grassland,na.action=na.omit) 
nlm.litterbags_size<- lm(yi~poly(litterbags_size1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.litterbags_size)
AIC(nlm.litterbags_size)
summary(lm.litterbags_size)
summary(nlm.litterbags_size)

# Physicochemical properties
# PH
lm.PH<- lm(yi~PH1,data=dt.terrestrial,na.action=na.omit) 
nlm.PH<- lm(yi~poly(PH1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.PH)
AIC(nlm.PH)
summary(lm.PH)
summary(nlm.PH)

lm.PH<- lm(yi~PH1,data=dt.forest,na.action=na.omit) 
nlm.PH<- lm(yi~poly(PH1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.PH)
AIC(nlm.PH)
summary(lm.PH)
summary(nlm.PH)

lm.PH<- lm(yi~PH1,data=dt.cropland,na.action=na.omit) 
nlm.PH<- lm(yi~poly(PH1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.PH)
AIC(nlm.PH)
summary(lm.PH)
summary(nlm.PH)

lm.PH<- lm(yi~PH1,data=dt.grassland,na.action=na.omit) 
nlm.PH<- lm(yi~poly(PH1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.PH)
AIC(nlm.PH)
anova(lm.PH,nlm.PH)
summary(lm.PH)
summary(nlm.PH)

#SOC
lm.SOC<- lm(yi~SOC1,data=dt.terrestrial,na.action=na.omit) 
nlm.SOC<- lm(yi~poly(SOC1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.SOC)
AIC(nlm.SOC)
anova(lm.SOC,nlm.SOC)
summary(lm.SOC)
summary(nlm.SOC)

lm.SOC<- lm(yi~SOC1,data=dt.forest,na.action=na.omit) 
nlm.SOC<- lm(yi~poly(SOC1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.SOC)
AIC(nlm.SOC)
anova(lm.SOC,nlm.SOC)
summary(lm.SOC)
summary(nlm.SOC)

lm.SOC<- lm(yi~SOC1,data=dt.cropland,na.action=na.omit) 
nlm.SOC<- lm(yi~poly(SOC1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.SOC)
AIC(nlm.SOC)
summary(lm.SOC)
summary(nlm.SOC)

lm.SOC<- lm(yi~SOC1,data=dt.grassland,na.action=na.omit) 
nlm.SOC<- lm(yi~poly(SOC1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.SOC)
AIC(nlm.SOC)
summary(lm.SOC)
summary(nlm.SOC)

# TN
lm.STN<- lm(yi~STN1,data=dt.terrestrial,na.action=na.omit) 
nlm.STN<- lm(yi~poly(STN1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.STN)
AIC(nlm.STN)
summary(lm.STN)
summary(nlm.STN)

lm.STN<- lm(yi~STN1,data=dt.forest,na.action=na.omit) 
nlm.STN<- lm(yi~poly(STN1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.STN)
AIC(nlm.STN)
anova(lm.STN,nlm.STN)
summary(lm.STN)
summary(nlm.STN)

lm.STN<- lm(yi~STN1,data=dt.cropland,na.action=na.omit) 
nlm.STN<- lm(yi~poly(STN1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.STN)
AIC(nlm.STN)
summary(lm.STN)
summary(nlm.STN)

lm.STN<- lm(yi~STN1,data=dt.grassland,na.action=na.omit) 
nlm.STN<- lm(yi~poly(STN1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.STN)
AIC(nlm.STN)
anova(lm.STN,nlm.STN)
summary(lm.STN)
summary(nlm.STN)

#C:N
lm.SCN<- lm(yi~SCN1,data=dt.terrestrial,na.action=na.omit) 
nlm.SCN<- lm(yi~poly(SCN1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.SCN)
AIC(nlm.SCN)
anova(lm.SCN,nlm.SCN)
summary(lm.SCN)
summary(nlm.SCN)

lm.SCN<- lm(yi~SCN1,data=dt.forest,na.action=na.omit) 
nlm.SCN<- lm(yi~poly(SCN1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.SCN)
AIC(nlm.SCN)
anova(lm.SCN,nlm.SCN)
summary(lm.SCN)
summary(nlm.SCN)

lm.SCN<- lm(yi~SCN1,data=dt.cropland,na.action=na.omit) 
nlm.SCN<- lm(yi~poly(SCN1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.SCN)
AIC(nlm.SCN)
summary(lm.SCN)
summary(nlm.SCN)

lm.SCN<- lm(yi~SCN1,data=dt.grassland,na.action=na.omit) 
nlm.SCN<- lm(yi~poly(SCN1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.SCN)
AIC(nlm.SCN)
summary(lm.SCN)
summary(nlm.SCN)

# Initial litter quality
# TOC
lm.Plant_TOC<- lm(yi~Plant_TOC1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_TOC<- lm(yi~poly(Plant_TOC1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_TOC)
AIC(nlm.Plant_TOC)
summary(lm.Plant_TOC)
summary(nlm.Plant_TOC)

lm.Plant_TOC<- lm(yi~Plant_TOC1,data=dt.forest,na.action=na.omit) 
nlm.Plant_TOC<- lm(yi~poly(Plant_TOC1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_TOC)
AIC(nlm.Plant_TOC)
summary(lm.Plant_TOC)
summary(nlm.Plant_TOC)

lm.Plant_TOC<- lm(yi~Plant_TOC1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_TOC<- lm(yi~poly(Plant_TOC1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_TOC)
AIC(nlm.Plant_TOC)
summary(lm.Plant_TOC)
summary(nlm.Plant_TOC)

lm.Plant_TOC<- lm(yi~Plant_TOC1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_TOC<- lm(yi~poly(Plant_TOC1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_TOC)
AIC(nlm.Plant_TOC)
summary(lm.Plant_TOC)
summary(nlm.Plant_TOC)

# TN
lm.Plant_TN<- lm(yi~Plant_TN1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_TN<- lm(yi~poly(Plant_TN1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_TN)
AIC(nlm.Plant_TN)
summary(lm.Plant_TN)
summary(nlm.Plant_TN)

lm.Plant_TN<- lm(yi~Plant_TN1,data=dt.forest,na.action=na.omit) 
nlm.Plant_TN<- lm(yi~poly(Plant_TN1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_TN)
AIC(nlm.Plant_TN)
anova(lm.Plant_TN,nlm.Plant_TN)
summary(lm.Plant_TN)
summary(nlm.Plant_TN)

lm.Plant_TN<- lm(yi~Plant_TN1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_TN<- lm(yi~poly(Plant_TN1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_TN)
AIC(nlm.Plant_TN)
anova(lm.Plant_TN,nlm.Plant_TN)
summary(lm.Plant_TN)
summary(nlm.Plant_TN)

lm.Plant_TN<- lm(yi~Plant_TN1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_TN<- lm(yi~poly(Plant_TN1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_TN)
AIC(nlm.Plant_TN)
summary(lm.Plant_TN)
summary(nlm.Plant_TN)

# TP
lm.Plant_TP<- lm(yi~Plant_TP1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_TP<- lm(yi~poly(Plant_TP1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_TP)
AIC(nlm.Plant_TP)
summary(lm.Plant_TP)
summary(nlm.Plant_TP)

lm.Plant_TP<- lm(yi~Plant_TP1,data=dt.forest,na.action=na.omit) 
nlm.Plant_TP<- lm(yi~poly(Plant_TP1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_TP)
AIC(nlm.Plant_TP)
summary(lm.Plant_TP)
summary(nlm.Plant_TP)

lm.Plant_TP<- lm(yi~Plant_TP1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_TP<- lm(yi~poly(Plant_TP1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_TP)
AIC(nlm.Plant_TP)
anova(lm.Plant_TP,nlm.Plant_TP)
summary(lm.Plant_TP)
summary(nlm.Plant_TP)

lm.Plant_TP<- lm(yi~Plant_TP1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_TP<- lm(yi~poly(Plant_TP1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_TP)
AIC(nlm.Plant_TP)
summary(lm.Plant_TP)
summary(nlm.Plant_TP)

# C:N
lm.Plant_C.N<- lm(yi~Plant_C.N1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_C.N<- lm(yi~poly(Plant_C.N1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
anova(lm.Plant_C.N,nlm.Plant_C.N)
AIC(lm.Plant_C.N)
AIC(nlm.Plant_C.N)
summary(lm.Plant_C.N)
summary(nlm.Plant_C.N)

lm.Plant_C.N<- lm(yi~Plant_C.N1,data=dt.forest,na.action=na.omit) 
nlm.Plant_C.N<- lm(yi~poly(Plant_C.N1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_C.N)
AIC(nlm.Plant_C.N)
anova(lm.Plant_C.N,nlm.Plant_C.N)
summary(lm.Plant_C.N)
summary(nlm.Plant_C.N)

lm.Plant_C.N<- lm(yi~Plant_C.N1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_C.N<- lm(yi~poly(Plant_C.N1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_C.N)
AIC(nlm.Plant_C.N)
anova(lm.Plant_C.N,nlm.Plant_C.N)
summary(lm.Plant_C.N)
summary(nlm.Plant_C.N)

lm.Plant_C.N<- lm(yi~Plant_C.N1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_C.N<- lm(yi~poly(Plant_C.N1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_C.N)
AIC(nlm.Plant_C.N)
summary(lm.Plant_C.N)
summary(nlm.Plant_C.N)

# C.P
lm.Plant_C.P<- lm(yi~Plant_C.P1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_C.P<- lm(yi~poly(Plant_C.P1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_C.P)
AIC(nlm.Plant_C.P)
anova(lm.Plant_C.P,nlm.Plant_C.P)
summary(lm.Plant_C.P)
summary(nlm.Plant_C.P)

lm.Plant_C.P<- lm(yi~Plant_C.P1,data=dt.forest,na.action=na.omit) 
nlm.Plant_C.P<- lm(yi~poly(Plant_C.P1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_C.P)
AIC(nlm.Plant_C.P)
anova(lm.Plant_C.P,nlm.Plant_C.P)
summary(lm.Plant_C.P)
summary(nlm.Plant_C.P)

lm.Plant_C.P<- lm(yi~Plant_C.P1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_C.P<- lm(yi~poly(Plant_C.P1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_C.P)
AIC(nlm.Plant_C.P)
anova(lm.Plant_C.P,nlm.Plant_C.P)
summary(lm.Plant_C.P)
summary(nlm.Plant_C.P)

lm.Plant_C.P<- lm(yi~Plant_C.P1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_C.P<- lm(yi~poly(Plant_C.P1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_C.P)
AIC(nlm.Plant_C.P)
anova(lm.Plant_C.P,nlm.Plant_C.P)
summary(lm.Plant_C.P)
summary(nlm.Plant_C.P)

# N:P
lm.Plant_N.P<- lm(yi~Plant_N.P1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_N.P<- lm(yi~poly(Plant_N.P1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_N.P)
AIC(nlm.Plant_N.P)
summary(lm.Plant_N.P)
summary(nlm.Plant_N.P)

lm.Plant_N.P<- lm(yi~Plant_N.P1,data=dt.forest,na.action=na.omit) 
nlm.Plant_N.P<- lm(yi~poly(Plant_N.P1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_N.P)
AIC(nlm.Plant_N.P)
summary(lm.Plant_N.P)
summary(nlm.Plant_N.P)

lm.Plant_N.P<- lm(yi~Plant_N.P1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_N.P<- lm(yi~poly(Plant_N.P1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_N.P)
AIC(nlm.Plant_N.P)
anova(lm.Plant_N.P,nlm.Plant_N.P)
summary(lm.Plant_N.P)
summary(nlm.Plant_N.P)

lm.Plant_N.P<- lm(yi~Plant_N.P1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_N.P<- lm(yi~poly(Plant_N.P1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_N.P)
AIC(nlm.Plant_N.P)
summary(lm.Plant_N.P)
summary(nlm.Plant_N.P)


# lignin
lm.Plant_Lignin<- lm(yi~Plant_Lignin1,data=dt.terrestrial,na.action=na.omit) 
nlm.Plant_Lignin<- lm(yi~poly(Plant_Lignin1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Plant_Lignin)
AIC(nlm.Plant_Lignin)
anova(lm.Plant_Lignin,nlm.Plant_Lignin)
summary(lm.Plant_Lignin)
summary(nlm.Plant_Lignin)

lm.Plant_Lignin<- lm(yi~Plant_Lignin1,data=dt.forest,na.action=na.omit) 
nlm.Plant_Lignin<- lm(yi~poly(Plant_Lignin1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Plant_Lignin)
AIC(nlm.Plant_Lignin)
anova(lm.Plant_Lignin,nlm.Plant_Lignin)
summary(lm.Plant_Lignin)
summary(nlm.Plant_Lignin)

lm.Plant_Lignin<- lm(yi~Plant_Lignin1,data=dt.cropland,na.action=na.omit) 
nlm.Plant_Lignin<- lm(yi~poly(Plant_Lignin1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Plant_Lignin)
AIC(nlm.Plant_Lignin)
anova(lm.Plant_Lignin,nlm.Plant_Lignin)
summary(lm.Plant_Lignin)
summary(nlm.Plant_Lignin)

lm.Plant_Lignin<- lm(yi~Plant_Lignin1,data=dt.grassland,na.action=na.omit) 
nlm.Plant_Lignin<- lm(yi~poly(Plant_Lignin1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Plant_Lignin)
AIC(nlm.Plant_Lignin)
summary(lm.Plant_Lignin)
summary(nlm.Plant_Lignin)

# Lignin:N
lm.Lignin.N<- lm(yi~Lignin.N1,data=dt.terrestrial,na.action=na.omit) 
nlm.Lignin.N<- lm(yi~poly(Lignin.N1,2,raw=T),data=dt.terrestrial,na.action=na.omit) 
AIC(lm.Lignin.N)
AIC(nlm.Lignin.N)
anova(lm.Lignin.N,nlm.Lignin.N)
summary(lm.Lignin.N)
summary(nlm.Lignin.N)

lm.Lignin.N<- lm(yi~Lignin.N1,data=dt.forest,na.action=na.omit) 
nlm.Lignin.N<- lm(yi~poly(Lignin.N1,2,raw=T),data=dt.forest,na.action=na.omit) 
AIC(lm.Lignin.N)
AIC(nlm.Lignin.N)
anova(lm.Lignin.N,nlm.Lignin.N)
summary(lm.Lignin.N)
summary(nlm.Lignin.N)

lm.Lignin.N<- lm(yi~Lignin.N1,data=dt.cropland,na.action=na.omit) 
nlm.Lignin.N<- lm(yi~poly(Lignin.N1,2,raw=T),data=dt.cropland,na.action=na.omit) 
AIC(lm.Lignin.N)
AIC(nlm.Lignin.N)
anova(lm.Lignin.N,nlm.Lignin.N)
summary(lm.Lignin.N)
summary(nlm.Lignin.N)

lm.Lignin.N<- lm(yi~Lignin.N1,data=dt.grassland,na.action=na.omit) 
nlm.Lignin.N<- lm(yi~poly(Lignin.N1,2,raw=T),data=dt.grassland,na.action=na.omit) 
AIC(lm.Lignin.N)
AIC(nlm.Lignin.N)
summary(lm.Lignin.N)
summary(nlm.Lignin.N)

################ Fig 4 ####
P4.1 <-  ggplot(dt.terrestrial, aes( x = MAT2, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(MAT )") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = MAT2_forest, y = yi_forest),method = 'lm', formula = y ~ poly(x,2),se= T,col = "#FF9900",size=1.5,fill="#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x = MAT2, y = yi),method = 'lm', formula = y ~ poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.077~~P<0.001",parse=T,x=2,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.062~~P<0.001",parse=T,x=2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.1

P4.2 <-  ggplot(dt.terrestrial, aes( x = MAP2, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(MAP mm)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = MAP2_forest, y = yi_forest),method = 'lm', formula = y ~ x,se= T,col = "#FF9900",size=1.5,fill="#FF9900")+
  geom_smooth(data = dtte_biome,aes(x = MAP2_grassland, y = yi_grassland,),method = 'lm', formula = y ~ x,se= T,col = "#2878b5",size=1.5,fill="#2878b5")+
  geom_smooth(data = dt.terrestrial,aes(x = MAP2, y = yi),method = 'lm', formula = y ~ x,se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 34))+
  theme(axis.text = element_text(size = 34))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 10)))+
  annotate("text",label="R^2==0.057~~P<0.001",parse=T,x=6.5,y=-2,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.046~~P==0.017",parse=T,x=6.5,y=-3,color="#2878b5",size=9)+
  annotate("text",label="R^2==0.024~~P<0.001",parse=T,x=6.5,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.2

P4.3 <-  ggplot(dt.terrestrial, aes( x =latitude2, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Latitude °)") +  ylab("ln(RR) of k")+
  scale_y_continuous(limits = c(-4,1))+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x =latitude2_forest, y = yi_forest),method = 'lm', formula = y ~ poly(x,2),se= T,col = "#FF9900",size=1.5,fill="#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x =latitude2, y = yi),method = 'lm', formula = y ~ poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.058~~P<0.001",parse=T,x=1.3,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.023~~P<0.001",parse=T,x=1.3,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.3

P4.4 <-  ggplot(dt.terrestrial, aes( x =longitude2, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Longitude °)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x =longitude2_forest, y = yi_forest),method = 'lm', formula = y ~ x,se= T,col = "#FF9900",size=1.5,fill="#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x =longitude2, y = yi),method = 'lm', formula = y ~ x,se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.019~~P<0.001",parse=T,x=2.2,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.124~~P<0.001",parse=T,x=2.2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.4

P4.5<-  ggplot(dt.terrestrial, aes( x = PH1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(pH)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = PH1_forest, y = yi_forest),method = 'lm', formula = y ~x,se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 9)))+
  annotate("text",label="R^2==0.008~~P==0.027",parse=T,x=1.7,y=-3.9,color="#FF9900",size=9)+
  theme(legend.position ="none")
P4.5

P4.6<-  ggplot(dt.terrestrial, aes( x = SOC1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(SOC g/kg)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = SOC1_forest, y = yi_forest),method = 'lm', formula =  y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x = SOC1, y = yi),method = 'lm', formula =  y ~poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.034~~P==0.027",parse=T,x=3,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.023~~P==0.027",parse=T,x=3,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.6

P4.7<-  ggplot(dt.terrestrial, aes( x = STN1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(TN g/kg)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = STN1_forest, y = yi_forest),method = 'lm', formula =y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x = STN1, y = yi),method = 'lm', formula =y ~x,se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.018~~P==0.001",parse=T,x=1.2,y=-3.6,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.017~~P<0.001",parse=T,x=1.2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.7

P4.8<-  ggplot(dt.terrestrial, aes( x = SCN1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(SoilC:N g/kg)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = SCN1_grassland, y = yi_grassland,),method = 'lm', formula = y ~x,se= T,col = "#2878b5",size=1.5,fill="#2878b5")+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.058~~P==0.007",parse=T,x=2.6,y=-3.9,color="#2878b5",size=9)+
  theme(legend.position ="none")
P4.8

P4.9<-  ggplot(dt.terrestrial, aes( x = Plant_TN1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(LitterTN %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Plant_TN1_forest, y = yi_forest),method = 'lm', formula =y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dtte_biome,aes(x = Plant_TN1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.014~~P==0.018",parse=T,x=1,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.121~~P==0.008",parse=T,x=1,y=-3.9,color="#c82423",size=9)+
  theme(legend.position ="none")
P4.9

P4.10<-  ggplot(dt.terrestrial, aes( x = Plant_C.N1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(LitterC:N %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Plant_C.N1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.220~~P<0.001",parse=T,x=2.9,y=-3.9,color="#c82423",size=9)+
  theme(legend.position ="none")
P4.10

P4.11<-  ggplot(dt.terrestrial, aes( x = Plant_C.P1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(LitterC:P %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Plant_C.P1_forest, y = yi_forest),method = 'lm', formula =y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dtte_biome,aes(x = Plant_C.P1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  geom_smooth(data = dt.terrestrial,aes(x = Plant_C.P1, y = yi),method = 'lm', formula =y ~x,se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.021~~P==0.004",parse=T,x=5.7,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.261~~P<0.001",parse=T,x=5.7,y=-3.5,color="#c82423",size=9)+
  annotate("text",label="R^2==0.007~~P==0.033",parse=T,x=5.7,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.11

P4.12<-  ggplot(dt.terrestrial, aes( x = Plant_Lignin1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Litter Lignin%)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Plant_Lignin1_forest, y = yi_forest),method = 'lm', formula =y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dtte_biome,aes(x =Plant_Lignin1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  geom_smooth(data = dt.terrestrial,aes(x = Plant_Lignin1, y = yi),method = 'lm', formula =y ~poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.025~~P==0.002",parse=T,x=1.7,y=-2,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.497~~P<0.001",parse=T,x=1.7,y=-3,color="#c82423",size=9)+
  annotate("text",label="R^2==0.085~~P<0.001",parse=T,x=1.7,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.12

P4.13<-  ggplot(dt.terrestrial, aes( x = Lignin.N1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Litter Lignin:N %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Lignin.N1_forest, y = yi_forest),method = 'lm', formula =y ~poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dtte_biome,aes(x = Lignin.N1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ x,se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  geom_smooth(data = dtte_biome,aes(x = Lignin.N1_grassland, y = yi_grassland,),method = 'lm', formula = y ~x,se= T,col = "#2878b5",size=1.5,fill="#2878b5")+
  geom_smooth(data = dt.terrestrial,aes(x = Lignin.N1, y = yi),method = 'lm', formula =y ~x,se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.032~~P<0.001",parse=T,x=1.8,y=-2,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.351~~P<0.001",parse=T,x=1.8,y=-2.5,color="#c82423",size=9)+
  annotate("text",label="R^2==0.083~~P==0.041",parse=T,x=1.8,y=-3.5,color="#2878b5",size=9)+
  annotate("text",label="R^2==0.049~~P<0.001",parse=T,x=1.8,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.13

P4.14<-  ggplot(dt.terrestrial, aes( x = Study_length1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Duration day)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = Study_length1_forest, y = yi_forest),method = 'lm', formula = y ~ x,se= T,col = "#FF9900",size=1.5,fill="#FF9900")+
  geom_smooth(data = dtte_biome,aes(x = Study_length1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill="#c82423")+
  geom_smooth(data = dtte_biome,aes(x = Study_length1_grassland, y = yi_grassland,),method = 'lm', formula = y ~ poly(x,2),se= T,col = "#2878b5",size=1.5,fill="#2878b5")+
  geom_smooth(data = dt.terrestrial,aes(x = Study_length1, y = yi),method = 'lm', formula = y ~ poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.028~~P<0.001",parse=T,x=5.2,y=-1.5,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.023~~P==0.023",parse=T,x=5.2,y=-2.5,color="#c82423",size=9)+
  annotate("text",label="R^2==0.049~~P==0.049",parse=T,x=5.2,y=-3.3,color="#2878b5",size=9)+
  annotate("text",label="R^2==0.032~~P<0.001",parse=T,x=5.2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.14

P4.15<-  ggplot(dt.terrestrial, aes( x = harvesting_times1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Harvesting times )") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = harvesting_times1_forest, y = yi_forest),method = 'lm', formula = y ~ poly(x,2),se= T,col = "#FF9900",size=1.5,fill= "#FF9900")+
  geom_smooth(data = dt.terrestrial,aes(x = harvesting_times1, y = yi),method = 'lm', formula = y ~ poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.059~~P<0.001",parse=T,x=2,y=-3,color="#FF9900",size=9)+
  annotate("text",label="R^2==0.037~~P<0.001",parse=T,x=2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.15

P4.16<-  ggplot(dt.terrestrial, aes( x = initail_mass1, y = yi,size=wi)) + 
  geom_point(aes(color=Biome1),alpha=0.5,show.legend =T)+
  scale_color_manual(values = c("#c82423","#FF9900","#2878b5"))+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Initail mass g)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(data = dtte_biome,aes(x = initail_mass1_cropland, y = yi_cropland,),method = 'lm', formula =  y ~ poly(x,2),se= T,col = "#c82423",size=1.5,fill= "#c82423")+
  geom_smooth(data = dtte_biome,aes(x = initail_mass1_grassland, y = yi_grassland,),method = 'lm', formula = y ~ poly(x,2),se= T,col = "#2878b5",size=1.5,fill="#2878b5")+
  geom_smooth(data = dt.terrestrial,aes(x = initail_mass1, y = yi),method = 'lm', formula = y ~ poly(x,2),se= T,col = "black",size=1.5)+
  theme(axis.title = element_text(size = 36))+
  theme(axis.text = element_text(size = 36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 8)))+
  annotate("text",label="R^2==0.269~~P<0.001",parse=T,x=2,y=-2,color="#c82423",size=9)+
  annotate("text",label="R^2==0.049~~P==0.048",parse=T,x=2,y=-3,color="#2878b5",size=9)+
  annotate("text",label="R^2==0.019~~P<0.001",parse=T,x=2,y=-3.9,color="black",size=9)+
  theme(legend.position ="none")
P4.16

pdf("Fig4.pdf", width =32, height =25)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 4)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P4.1, vp = vplayout(1, 1))
print(P4.2, vp = vplayout(1, 2))
print(P4.3, vp = vplayout(1, 3))
print(P4.4, vp = vplayout(1, 4))
print(P4.5, vp = vplayout(2, 1))
print(P4.6, vp = vplayout(2, 2))
print(P4.7, vp = vplayout(2, 3))
print(P4.8, vp = vplayout(2, 4))
print(P4.9, vp = vplayout(3, 1))
print(P4.10, vp = vplayout(3, 2))
print(P4.11, vp = vplayout(3, 3))
print(P4.12, vp = vplayout(3, 4))
print(P4.13, vp = vplayout(4, 1))
print(P4.14, vp = vplayout(4, 2))
print(P4.15, vp = vplayout(4, 3))
print(P4.16, vp = vplayout(4, 4))
dev.off()

################## Aquatic ####
dt.aquatic<- subset(d2,Biome1=="aquatic")
################### Statistics (r and P values) for Fig 5 TableS2 ####
# Climate
lm.MAP<- lm(yi~MAP2,data=dt.aquatic,na.action=na.omit) 
nlm.MAP<- lm(yi~poly(MAP2,2,raw=T),data=dt.aquatic,na.action=na.omit)
AIC(lm.MAP)
AIC(nlm.MAP)
summary(lm.MAP)
summary(nlm.MAP)

lm.MAT<- lm(yi~MAT2,data=dt.aquatic,na.action=na.omit) 
nlm.MAT<- lm(yi~poly(MAT2,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.MAT)
AIC(nlm.MAT)
summary(lm.MAT)

lm.Alt<- lm(yi~Alt2,data=dt.aquatic,na.action=na.omit) 
nlm.Alt<- lm(yi~poly(Alt2,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Alt)
AIC(nlm.Alt)
anova(lm.Alt,nlm.Alt)
summary(lm.Alt)
summary(nlm.Alt)

lm.latitude<- lm(yi~latitude2,data=dt.aquatic,na.action=na.omit) 
nlm.latitude<- lm(yi~poly(latitude2,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.latitude)
AIC(nlm.latitude)
summary(lm.latitude)
summary(nlm.latitude)

lm.longitude<- lm(yi~longitude2,data=dt.aquatic,na.action=na.omit) 
nlm.longitude<- lm(yi~poly(longitude2,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.longitude)
AIC(nlm.longitude)
summary(lm.longitude)
summary(nlm.longitude)

# Experimental condition
lm.Study_length<- lm(yi~Study_length1,data=dt.aquatic,na.action=na.omit) 
nlm.Study_length<- lm(yi~poly(Study_length1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Study_length)
AIC(nlm.Study_length)
summary(lm.Study_length)
summary(nlm.Study_length)

lm.harvesting_times<- lm(yi~harvesting_times1,data=dt.aquatic,na.action=na.omit) 
nlm.harvesting_times<- lm(yi~poly(harvesting_times1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.harvesting_times)
AIC(nlm.harvesting_times)
summary(lm.harvesting_times)
summary(nlm.harvesting_times)

lm.initail_mass<- lm(yi~initail_mass1,data=dt.aquatic,na.action=na.omit) 
nlm.initail_mass<- lm(yi~poly(initail_mass1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.initail_mass)
AIC(nlm.initail_mass)
anova(lm.initail_mass,nlm.initail_mass)
summary(lm.initail_mass)
summary(nlm.initail_mass)

lm.litterbags_size<- lm(yi~litterbags_size1,data=dt.aquatic,na.action=na.omit) 
nlm.litterbags_size<- lm(yi~poly(litterbags_size1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.litterbags_size)
AIC(nlm.litterbags_size)
anova(lm.litterbags_size,nlm.litterbags_size)
summary(lm.litterbags_size)
summary(nlm.litterbags_size)

# physicochemical properties 
lm.PH<- lm(yi~PH1,data=dt.aquatic,na.action=na.omit) 
nlm.PH<- lm(yi~poly(PH1,2,raw=T),data=dt.aquatic,na.action=na.omit)
AIC(lm.PH)
AIC(nlm.PH)
anova(lm.PH,nlm.PH)
summary(lm.PH)
summary(nlm.PH)

lm.Water_temperature<- lm(yi~Water_temperature1,data=dt.aquatic,na.action=na.omit) 
nlm.Water_temperature<- lm(yi~poly(Water_temperature1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Water_temperature)
AIC(nlm.Water_temperature)
summary(lm.Water_temperature)

lm.Conductivity<- lm(yi~Conductivity1,data=dt.aquatic,na.action=na.omit) 
nlm.Conductivity<- lm(yi~poly(Conductivity1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Conductivity)
AIC(nlm.Conductivity)
summary(lm.Conductivity)

lm.NO3<- lm(yi~NO3.1,data=dt.aquatic,na.action=na.omit) 
nlm.NO3<- lm(yi~poly(NO3.1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.NO3)
AIC(nlm.NO3)
anova(lm.NO3,nlm.NO3)
summary(lm.NO3)
summary(nlm.NO3)

lm.NH4<- lm(yi~NH4.1,data=dt.aquatic,na.action=na.omit) 
nlm.NH4<- lm(yi~poly(NH4.1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.NH4)
AIC(nlm.NH4)
anova(lm.NH4,nlm.NH4)
summary(lm.NH4)
summary(nlm.NH4)

lm.SRP<- lm(yi~SRP1,data=dt.aquatic,na.action=na.omit) 
nlm.SRP<- lm(yi~poly(SRP1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.SRP)
AIC(nlm.SRP)
anova(lm.SRP,nlm.SRP)
summary(lm.SRP)
summary(nlm.SRP)

# Initial litter quality
lm.Plant_TOC<- lm(yi~Plant_TOC1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_TOC<- lm(yi~poly(Plant_TOC1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_TOC)
AIC(nlm.Plant_TOC)
summary(lm.Plant_TOC)
summary(nlm.Plant_TOC)

lm.Plant_TN<- lm(yi~Plant_TN1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_TN<- lm(yi~poly(Plant_TN1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_TN)
AIC(nlm.Plant_TN)
summary(lm.Plant_TN)
summary(nlm.Plant_TN)


lm.Plant_TP<- lm(yi~Plant_TP1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_TP<- lm(yi~poly(Plant_TP1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_TP)
AIC(nlm.Plant_TP)
summary(lm.Plant_TP)
summary(nlm.Plant_TP)

lm.Plant_C.N<- lm(yi~Plant_C.N1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_C.N<- lm(yi~poly(Plant_C.N1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_C.N)
AIC(nlm.Plant_C.N)
anova(lm.Plant_C.N,nlm.Plant_C.N)
summary(lm.Plant_C.N)
summary(nlm.Plant_C.N)

lm.Plant_C.P<- lm(yi~Plant_C.P1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_C.P<- lm(yi~poly(Plant_C.P1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_C.P)
AIC(nlm.Plant_C.P)
summary(lm.Plant_C.P)
summary(nlm.Plant_C.P)

lm.Plant_N.P<- lm(yi~Plant_N.P1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_N.P<- lm(yi~poly(Plant_N.P1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_N.P)
AIC(nlm.Plant_N.P)
summary(lm.Plant_N.P)
summary(nlm.Plant_N.P)

lm.Plant_Lignin<- lm(yi~Plant_Lignin1,data=dt.aquatic,na.action=na.omit) 
nlm.Plant_Lignin<- lm(yi~poly(Plant_Lignin1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Plant_Lignin)
AIC(nlm.Plant_Lignin)
summary(lm.Plant_Lignin)
summary(nlm.Plant_Lignin)

lm.Lignin.N<- lm(yi~Lignin.N1,data=dt.aquatic,na.action=na.omit) 
nlm.Lignin.N<- lm(yi~poly(Lignin.N1,2,raw=T),data=dt.aquatic,na.action=na.omit) 
AIC(lm.Lignin.N)
AIC(nlm.Lignin.N)
summary(lm.Lignin.N)
summary(nlm.Lignin.N)

################ Fig 5 ####
P5.1 <-  ggplot(dt.aquatic, aes( x = latitude2,   y = yi,size=wi)) + 
  geom_point(color="#2878b5",alpha=0.5,show.legend =T)+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Latitude2 °)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(method = 'lm', formula =y~x, se = T,color="#2878b5",size=1.5) + 
  stat_poly_eq(aes(label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=6,rr.digits = 3)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 20))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 6)))
P5.1

P5.2 <-  ggplot(dt.aquatic, aes( x = Water_temperature1, y = yi,size=wi)) + 
  geom_point(color="#2878b5",alpha=0.5,show.legend =T)+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(Water temperature ℃ )") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(method = 'lm', formula = y ~ x, se = T,color="#2878b5",size=1.5) + 
  stat_poly_eq(aes(label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=6,rr.digits = 3)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 20))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 6)))
P5.2

P5.3<-  ggplot(dt.aquatic, aes( x =NH4.1, y = yi,size=wi)) + 
  geom_point(color="#2878b5",alpha=0.5,show.legend =T)+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(NH4 mg/l)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = T,color="#2878b5",size=1.5) + 
  stat_poly_eq(aes(label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),formula = y~poly(x,2),parse=T,size=6,rr.digits = 3)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 20))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 6)))
P5.3

P5.4 <-  ggplot(dt.aquatic, aes( x = Plant_TN1,   y = yi,size=wi)) + 
  geom_point(color="#2878b5",alpha=0.5,show.legend =T)+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(LitterTN %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(method = 'lm', formula =y~x, se = T,color="#2878b5",size=1.5) + 
  stat_poly_eq(aes(label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=6,rr.digits = 3)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 20))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 6)))
P5.4

P5.5<-  ggplot(dt.aquatic, aes( x = Plant_C.N1,   y = yi,size=wi)) + 
  geom_point(color="#2878b5",alpha=0.5,show.legend =T)+
  geom_hline(yintercept=0,linetype="dashed",colour="black",size=1)+
  xlab("ln(LitterC:N %)") +  ylab("ln(RR) of k")+
  theme_few()+
  geom_smooth(method = 'lm', formula =y~x, se = T,color="#2878b5",size=1.5) + 
  stat_poly_eq(aes(label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),formula = y~x,parse=T,size=6,rr.digits = 3)+
  theme(axis.title = element_text(size = 20))+
  theme(axis.text = element_text(size = 20))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(panel.border = element_rect(fill=NA,color="black", size=2, linetype="solid"))+
  labs(size="Weight",color="Ecosystem")+guides(color = guide_legend(override.aes = list(size = 6)))
P5.5

pdf("Fig5.pdf", width =13, height =8)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 3)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(P5.1, vp = vplayout(1, 1))
print(P5.2, vp = vplayout(1, 2))
print(P5.3, vp = vplayout(2, 1))
print(P5.4, vp = vplayout(2, 2))
print(P5.5, vp = vplayout(2, 3))
dev.off()


########### Multiple regression #######
### Terrestrial ####
##### Input dataset ###
data<-read_excel("data.xlsx",sheet=1)%>%
  mutate(ID=as.factor(ID),observations=as.factor(observations),
         exp_site=as.factor(exp_site))
dat.te<- subset(data,Eco!="aquatic")%>%dplyr::select(1:13,17:19,21:28)
dat.te1<-na.omit(dat.te)

# data standardization (0-1) ###
dat.te1$MAP2<-(dat.te1$MAP2-min(dat.te1$MAP2))/(max(dat.te1$MAP2)-min(dat.te1$MAP2))
dat.te1$MAT2<-(dat.te1$MAT2-min(dat.te1$MAT2))/(max(dat.te1$MAT2)-min(dat.te1$MAT2))
dat.te1$latitude<-(dat.te1$latitude-min(dat.te1$latitude))/(max(dat.te1$latitude)-min(dat.te1$latitude))
dat.te1$longitude<-(dat.te1$longitude-min(dat.te1$longitude))/(max(dat.te1$longitude)-min(dat.te1$longitude))
dat.te1$Lat<-(dat.te1$Lat-min(dat.te1$Lat))/(max(dat.te1$Lat)-min(dat.te1$Lat))
dat.te1$Long<-(dat.te1$Long-min(dat.te1$Long))/(max(dat.te1$Long)-min(dat.te1$Long))
dat.te1$Alt2<-(dat.te1$Alt2-min(dat.te1$Alt2))/(max(dat.te1$Alt2)-min(dat.te1$Alt2))
dat.te1$Plant_C.N1<-(dat.te1$Plant_C.N1-min(dat.te1$Plant_C.N1))/(max(dat.te1$Plant_C.N1)-min(dat.te1$Plant_C.N1))
dat.te1$Plant_C.P1<-(dat.te1$Plant_C.P1-min(dat.te1$Plant_C.P1))/(max(dat.te1$Plant_C.P1)-min(dat.te1$Plant_C.P1))
dat.te1$Plant_N.P1<-(dat.te1$Plant_N.P1-min(dat.te1$Plant_N.P1))/(max(dat.te1$Plant_N.P1)-min(dat.te1$Plant_N.P1))
dat.te1$Lignin.N1<-(dat.te1$Lignin.N1-min(dat.te1$Lignin.N1))/(max(dat.te1$Lignin.N1)-min(dat.te1$Lignin.N1))
dat.te1$Study_length1<-(dat.te1$Study_length1-min(dat.te1$Study_length1))/(max(dat.te1$Study_length1)-min(dat.te1$Study_length1))
dat.te1$initail_mass1<-(dat.te1$initail_mass1-min(dat.te1$initail_mass1))/(max(dat.te1$initail_mass1)-min(dat.te1$initail_mass1))
dat.te1$harvesting_times1<-(dat.te1$harvesting_times1-min(dat.te1$harvesting_times1))/(max(dat.te1$harvesting_times1)-min(dat.te1$harvesting_times1))
dat.te1$PH1<-(dat.te1$PH1-min(dat.te1$PH1))/(max(dat.te1$PH1)-min(dat.te1$PH1))
dat.te1$SOC1<-(dat.te1$SOC1-min(dat.te1$SOC1))/(max(dat.te1$SOC1)-min(dat.te1$SOC1))
dat.te1$SCN1<-(dat.te1$SCN1-min(dat.te1$SCN1))/(max(dat.te1$SCN1)-min(dat.te1$SCN1))
dat.te1$STN1<-(dat.te1$STN1-min(dat.te1$STN1))/(max(dat.te1$STN1)-min(dat.te1$STN1))
dat.te1$yi<-(dat.te1$yi-min(dat.te1$yi))/(max(dat.te1$yi)-min(dat.te1$yi))
dat.te1$vi<-(dat.te1$vi-min(dat.te1$vi))/(max(dat.te1$vi)-min(dat.te1$vi))

model1<-lm(yi~latitude+longitude+Alt2+MAP2+MAT2+Plant_C.N1+Plant_C.P1+Lignin.N1+Study_length1+harvesting_times1+
             initail_mass1+PH1+SOC1+SCN1,data =dat.te1 )
summary(model1)

### Aquatic ####
dat.ae<- subset(data,Eco=="aquatic")%>%dplyr::select(1:13,17:19,21:25,29:30)
dat.ae1<-na.omit(dat.ae)
# data standardization (0-1) ###
dat.ae1$MAP2<-(dat.ae1$MAP2-min(dat.ae1$MAP2))/(max(dat.ae1$MAP2)-min(dat.ae1$MAP2))
dat.ae1$MAT2<-(dat.ae1$MAT2-min(dat.ae1$MAT2))/(max(dat.ae1$MAT2)-min(dat.ae1$MAT2))
dat.ae1$latitude<-(dat.ae1$latitude-min(dat.ae1$latitude))/(max(dat.ae1$latitude)-min(dat.ae1$latitude))
dat.ae1$Long<-(dat.ae1$Long-min(dat.ae1$Long))/(max(dat.ae1$Long)-min(dat.ae1$Long))
dat.ae1$Lat<-(dat.ae1$Lat-min(dat.ae1$Lat))/(max(dat.ae1$Lat)-min(dat.ae1$Lat))
dat.ae1$longitude<-(dat.ae1$longitude-min(dat.ae1$longitude))/(max(dat.ae1$longitude)-min(dat.ae1$longitude))
dat.ae1$Alt2<-(dat.ae1$Alt2-min(dat.ae1$Alt2))/(max(dat.ae1$Alt2)-min(dat.ae1$Alt2))
dat.ae1$Plant_C.N1<-(dat.ae1$Plant_C.N1-min(dat.ae1$Plant_C.N1))/(max(dat.ae1$Plant_C.N1)-min(dat.ae1$Plant_C.N1))
dat.ae1$Plant_C.P1<-(dat.ae1$Plant_C.P1-min(dat.ae1$Plant_C.P1))/(max(dat.ae1$Plant_C.P1)-min(dat.ae1$Plant_C.P1))
dat.ae1$Plant_N.P1<-(dat.ae1$Plant_N.P1-min(dat.ae1$Plant_N.P1))/(max(dat.ae1$Plant_N.P1)-min(dat.ae1$Plant_N.P1))
dat.ae1$Lignin.N1<-(dat.ae1$Lignin.N1-min(dat.ae1$Lignin.N1))/(max(dat.ae1$Lignin.N1)-min(dat.ae1$Lignin.N1))
dat.ae1$Study_length1<-(dat.ae1$Study_length1-min(dat.ae1$Study_length1))/(max(dat.ae1$Study_length1)-min(dat.ae1$Study_length1))
dat.ae1$initail_mass1<-(dat.ae1$initail_mass1-min(dat.ae1$initail_mass1))/(max(dat.ae1$initail_mass1)-min(dat.ae1$initail_mass1))
dat.ae1$harvesting_times1<-(dat.ae1$harvesting_times1-min(dat.ae1$harvesting_times1))/(max(dat.ae1$harvesting_times1)-min(dat.ae1$harvesting_times1))
dat.ae1$PH1<-(dat.ae1$PH1-min(dat.ae1$PH1))/(max(dat.ae1$PH1)-min(dat.ae1$PH1))
dat.ae1$Water_temperature1<-(dat.ae1$Water_temperature1-min(dat.ae1$Water_temperature1))/(max(dat.ae1$Water_temperature1)-min(dat.ae1$Water_temperature1))
dat.ae1$Conductivity1<-(dat.ae1$Conductivity1-min(dat.ae1$Conductivity1))/(max(dat.ae1$Conductivity1)-min(dat.ae1$Conductivity1))
dat.ae1$yi<-(dat.ae1$yi-min(dat.ae1$yi))/(max(dat.ae1$yi)-min(dat.ae1$yi))

model2<-lm(yi~MAP2+MAT2+longitude+latitude+Alt2+Plant_C.N1+Plant_C.P1+Lignin.N1+Study_length1+harvesting_times1+
             initail_mass1+PH1+Water_temperature1+Conductivity1,data =dat.ae1 )
summary(model2)

### Forest ####
dat.forest<-subset(dat.te,Eco=="forest")
dat.forest1<-na.omit(dat.forest)
# data standardization (0-1) ###
dat.forest1$MAP2<-(dat.forest1$MAP2-min(dat.forest1$MAP2))/(max(dat.forest1$MAP2)-min(dat.forest1$MAP2))
dat.forest1$MAT2<-(dat.forest1$MAT2-min(dat.forest1$MAT2))/(max(dat.forest1$MAT2)-min(dat.forest1$MAT2))
dat.forest1$latitude<-(dat.forest1$latitude-min(dat.forest1$latitude))/(max(dat.forest1$latitude)-min(dat.forest1$latitude))
dat.forest1$longitude<-(dat.forest1$longitude-min(dat.forest1$longitude))/(max(dat.forest1$longitude)-min(dat.forest1$longitude))
dat.forest1$Lat<-(dat.forest1$Lat-min(dat.forest1$Lat))/(max(dat.forest1$Lat)-min(dat.forest1$Lat))
dat.forest1$Long<-(dat.forest1$Long-min(dat.forest1$Long))/(max(dat.forest1$Long)-min(dat.forest1$Long))
dat.forest1$Alt2<-(dat.forest1$Alt2-min(dat.forest1$Alt2))/(max(dat.forest1$Alt2)-min(dat.forest1$Alt2))
dat.forest1$Plant_C.N1<-(dat.forest1$Plant_C.N1-min(dat.forest1$Plant_C.N1))/(max(dat.forest1$Plant_C.N1)-min(dat.forest1$Plant_C.N1))
dat.forest1$Plant_C.P1<-(dat.forest1$Plant_C.P1-min(dat.forest1$Plant_C.P1))/(max(dat.forest1$Plant_C.P1)-min(dat.forest1$Plant_C.P1))
dat.forest1$Plant_N.P1<-(dat.forest1$Plant_N.P1-min(dat.forest1$Plant_N.P1))/(max(dat.forest1$Plant_N.P1)-min(dat.forest1$Plant_N.P1))
dat.forest1$Lignin.N1<-(dat.forest1$Lignin.N1-min(dat.forest1$Lignin.N1))/(max(dat.forest1$Lignin.N1)-min(dat.forest1$Lignin.N1))
dat.forest1$Study_length1<-(dat.forest1$Study_length1-min(dat.forest1$Study_length1))/(max(dat.forest1$Study_length1)-min(dat.forest1$Study_length1))
dat.forest1$initail_mass1<-(dat.forest1$initail_mass1-min(dat.forest1$initail_mass1))/(max(dat.forest1$initail_mass1)-min(dat.forest1$initail_mass1))
dat.forest1$harvesting_times1<-(dat.forest1$harvesting_times1-min(dat.forest1$harvesting_times1))/(max(dat.forest1$harvesting_times1)-min(dat.forest1$harvesting_times1))
dat.forest1$PH1<-(dat.forest1$PH1-min(dat.forest1$PH1))/(max(dat.forest1$PH1)-min(dat.forest1$PH1))
dat.forest1$SOC1<-(dat.forest1$SOC1-min(dat.forest1$SOC1))/(max(dat.forest1$SOC1)-min(dat.forest1$SOC1))
dat.forest1$SCN1<-(dat.forest1$SCN1-min(dat.forest1$SCN1))/(max(dat.forest1$SCN1)-min(dat.forest1$SCN1))
dat.forest1$yi<-(dat.forest1$yi-min(dat.forest1$yi))/(max(dat.forest1$yi)-min(dat.forest1$yi))
dat.forest1$vi<-(dat.forest1$vi-min(dat.forest1$vi))/(max(dat.forest1$vi)-min(dat.forest1$vi))

model3<-lm(yi~latitude+longitude+Alt2+MAP2+MAT2+Plant_C.N1+Plant_C.P1+Lignin.N1+Study_length1+harvesting_times1+
             initail_mass1+PH1+SOC1+SCN1,data =dat.forest1 )
summary(model3)

### Cropland ####
dat.cropland<- subset(dat.te,Eco=="cropland")
dat.cropland1<-na.omit(dat.cropland)
# data standardization (0-1) ###
dat.cropland1$MAP2<-(dat.cropland1$MAP2-min(dat.cropland1$MAP2))/(max(dat.cropland1$MAP2)-min(dat.cropland1$MAP2))
dat.cropland1$MAT2<-(dat.cropland1$MAT2-min(dat.cropland1$MAT2))/(max(dat.cropland1$MAT2)-min(dat.cropland1$MAT2))
dat.cropland1$latitude<-(dat.cropland1$latitude-min(dat.cropland1$latitude))/(max(dat.cropland1$latitude)-min(dat.cropland1$latitude))
dat.cropland1$longitude<-(dat.cropland1$longitude-min(dat.cropland1$longitude))/(max(dat.cropland1$longitude)-min(dat.cropland1$longitude))
dat.cropland1$Lat<-(dat.cropland1$Lat-min(dat.cropland1$Lat))/(max(dat.cropland1$Lat)-min(dat.cropland1$Lat))
dat.cropland1$Long<-(dat.cropland1$Long-min(dat.cropland1$Long))/(max(dat.cropland1$Long)-min(dat.cropland1$Long))
dat.cropland1$Alt2<-(dat.cropland1$Alt2-min(dat.cropland1$Alt2))/(max(dat.cropland1$Alt2)-min(dat.cropland1$Alt2))
dat.cropland1$Plant_C.N1<-(dat.cropland1$Plant_C.N1-min(dat.cropland1$Plant_C.N1))/(max(dat.cropland1$Plant_C.N1)-min(dat.cropland1$Plant_C.N1))
dat.cropland1$Plant_C.P1<-(dat.cropland1$Plant_C.P1-min(dat.cropland1$Plant_C.P1))/(max(dat.cropland1$Plant_C.P1)-min(dat.cropland1$Plant_C.P1))
dat.cropland1$Plant_N.P1<-(dat.cropland1$Plant_N.P1-min(dat.cropland1$Plant_N.P1))/(max(dat.cropland1$Plant_N.P1)-min(dat.cropland1$Plant_N.P1))
dat.cropland1$Lignin.N1<-(dat.cropland1$Lignin.N1-min(dat.cropland1$Lignin.N1))/(max(dat.cropland1$Lignin.N1)-min(dat.cropland1$Lignin.N1))
dat.cropland1$Study_length1<-(dat.cropland1$Study_length1-min(dat.cropland1$Study_length1))/(max(dat.cropland1$Study_length1)-min(dat.cropland1$Study_length1))
dat.cropland1$initail_mass1<-(dat.cropland1$initail_mass1-min(dat.cropland1$initail_mass1))/(max(dat.cropland1$initail_mass1)-min(dat.cropland1$initail_mass1))
dat.cropland1$harvesting_times1<-(dat.cropland1$harvesting_times1-min(dat.cropland1$harvesting_times1))/(max(dat.cropland1$harvesting_times1)-min(dat.cropland1$harvesting_times1))
dat.cropland1$PH1<-(dat.cropland1$PH1-min(dat.cropland1$PH1))/(max(dat.cropland1$PH1)-min(dat.cropland1$PH1))
dat.cropland1$SOC1<-(dat.cropland1$SOC1-min(dat.cropland1$SOC1))/(max(dat.cropland1$SOC1)-min(dat.cropland1$SOC1))
dat.cropland1$SCN1<-(dat.cropland1$SCN1-min(dat.cropland1$SCN1))/(max(dat.cropland1$SCN1)-min(dat.cropland1$SCN1))
dat.cropland1$yi<-(dat.cropland1$yi-min(dat.cropland1$yi))/(max(dat.cropland1$yi)-min(dat.cropland1$yi))
dat.cropland1$vi<-(dat.cropland1$vi-min(dat.cropland1$vi))/(max(dat.cropland1$vi)-min(dat.cropland1$vi))

model4<-lm(yi~latitude+longitude+Alt2+MAP2+MAT2+Plant_C.N1+Plant_C.P1+Lignin.N1+Study_length1+harvesting_times1+
             initail_mass1+PH1+SOC1+SCN1,data =dat.cropland1 )

summary(model4)

### Grassland ####
dat.grassland<- subset(dat.te,Eco=="grassland")
dat.grassland1<-na.omit(dat.grassland)
# data standardization (0-1) ###
dat.grassland1$MAP2<-(dat.grassland1$MAP2-min(dat.grassland1$MAP2))/(max(dat.grassland1$MAP2)-min(dat.grassland1$MAP2))
dat.grassland1$MAT2<-(dat.grassland1$MAT2-min(dat.grassland1$MAT2))/(max(dat.grassland1$MAT2)-min(dat.grassland1$MAT2))
dat.grassland1$latitude<-(dat.grassland1$latitude-min(dat.grassland1$latitude))/(max(dat.grassland1$latitude)-min(dat.grassland1$latitude))
dat.grassland1$longitude<-(dat.grassland1$longitude-min(dat.grassland1$longitude))/(max(dat.grassland1$longitude)-min(dat.grassland1$longitude))
dat.grassland1$Lat<-(dat.grassland1$Lat-min(dat.grassland1$Lat))/(max(dat.grassland1$Lat)-min(dat.grassland1$Lat))
dat.grassland1$Long<-(dat.grassland1$Long-min(dat.grassland1$Long))/(max(dat.grassland1$Long)-min(dat.grassland1$Long))
dat.grassland1$Alt2<-(dat.grassland1$Alt2-min(dat.grassland1$Alt2))/(max(dat.grassland1$Alt2)-min(dat.grassland1$Alt2))
dat.grassland1$Plant_C.N1<-(dat.grassland1$Plant_C.N1-min(dat.grassland1$Plant_C.N1))/(max(dat.grassland1$Plant_C.N1)-min(dat.grassland1$Plant_C.N1))
dat.grassland1$Plant_C.P1<-(dat.grassland1$Plant_C.P1-min(dat.grassland1$Plant_C.P1))/(max(dat.grassland1$Plant_C.P1)-min(dat.grassland1$Plant_C.P1))
dat.grassland1$Plant_N.P1<-(dat.grassland1$Plant_N.P1-min(dat.grassland1$Plant_N.P1))/(max(dat.grassland1$Plant_N.P1)-min(dat.grassland1$Plant_N.P1))
dat.grassland1$Lignin.N1<-(dat.grassland1$Lignin.N1-min(dat.grassland1$Lignin.N1))/(max(dat.grassland1$Lignin.N1)-min(dat.grassland1$Lignin.N1))
dat.grassland1$Study_length1<-(dat.grassland1$Study_length1-min(dat.grassland1$Study_length1))/(max(dat.grassland1$Study_length1)-min(dat.grassland1$Study_length1))
dat.grassland1$initail_mass1<-(dat.grassland1$initail_mass1-min(dat.grassland1$initail_mass1))/(max(dat.grassland1$initail_mass1)-min(dat.grassland1$initail_mass1))
dat.grassland1$harvesting_times1<-(dat.grassland1$harvesting_times1-min(dat.grassland1$harvesting_times1))/(max(dat.grassland1$harvesting_times1)-min(dat.grassland1$harvesting_times1))
dat.grassland1$PH1<-(dat.grassland1$PH1-min(dat.grassland1$PH1))/(max(dat.grassland1$PH1)-min(dat.grassland1$PH1))
dat.grassland1$SOC1<-(dat.grassland1$SOC1-min(dat.grassland1$SOC1))/(max(dat.grassland1$SOC1)-min(dat.grassland1$SOC1))
dat.grassland1$SCN1<-(dat.grassland1$SCN1-min(dat.grassland1$SCN1))/(max(dat.grassland1$SCN1)-min(dat.grassland1$SCN1))
dat.grassland1$yi<-(dat.grassland1$yi-min(dat.grassland1$yi))/(max(dat.grassland1$yi)-min(dat.grassland1$yi))
dat.grassland1$vi<-(dat.grassland1$vi-min(dat.grassland1$vi))/(max(dat.grassland1$vi)-min(dat.grassland1$vi))

model5<-lm(yi~latitude+longitude+Alt2+MAP2+MAT2+Plant_C.N1+Plant_N.P1+Plant_C.P1+Lignin.N1+Study_length1+harvesting_times1+
             initail_mass1+PH1+SOC1+SCN1,data =dat.grassland1 )
summary(model5)

## Extract coefficients and confidence intervals about fig 6 ###
r2(model1)
d1<-data.frame(coef(summary(model1)))
d1$env <- rownames(d1)
openxlsx::write.xlsx(d1,file = "D:/meta_analysis/aboutR/meta2023-07/te.xlsx")

r2(model2)
d2<-data.frame(coef(summary(model2)))
d2$env <- rownames(d2)
openxlsx::write.xlsx(d2,file = "D:/meta_analysis/aboutR/meta2023-07/ae.xlsx")

r2(model3)
d3<-data.frame(coef(summary(model3)))
d3$env <- rownames(d3)
openxlsx::write.xlsx(d3,file = "D:/meta_analysis/aboutR/meta2023-07/forest.xlsx")

r2(model4)
d4<-data.frame(coef(summary(model4)))
d4$env <- rownames(d4)
openxlsx::write.xlsx(d4,file = "D:/meta_analysis/aboutR/meta2023-07/cropland.xlsx")

r2(model5)
d5<-data.frame(coef(summary(model5)))
d5$env <- rownames(d5)
openxlsx::write.xlsx(d5,file = "D:/meta_analysis/aboutR/meta2023-07/grass.xlsx")

################### Fig 6 ####
## TE
df<-read_excel("mydf.xlsx",sheet=6)
df
cols<-c("#9832cb","#6a8d23","#fc0000","#4776fd","#f2a260")
p1<-df %>% 
  mutate(group=fct_relevel(group,
                           c("Soil","Climate","Litter","Exp","Geo"))) %>% 
  group_by(group) %>% 
  arrange(Estimate,.by_group = T) %>% 
  mutate(var=fct_relevel(var,var)) %>% 
  mutate(signi=case_when(
    `Pr(>|z|)` < 0.1 & `Pr(>|z|)` >= 0.05 ~ '#',
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01 ~ '*',
    `Pr(>|z|)` < 0.01 & `Pr(>|z|)` >= 0.001 ~ '**',
    `Pr(>|z|)` < 0.001 ~ '***'
  )) %>% 
  ggplot(aes(x=Estimate,y=var))+
  geom_linerange(aes(xmin=Estimate-1.96*`Std.Error`,
                     xmax=Estimate+1.96*`Std.Error`,
                     color=group))+
  geom_point(aes(color=group),size=3)+
  theme_minimal()+
  labs(x="Parameter Estimate",y=NULL)+
  scale_x_continuous(limits = c(-0.25,0.32),)+
  guides(x=guide_axis_truncated(trunc_lower = -0.2,
                                trunc_upper = 0.32))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none")+
  scale_color_manual(values = rev(cols))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_text(aes(x=0.1,label=var),
            hjust=0,size=6)+
  geom_text(aes(x=-0.1,label=signi),size=6)+
  theme(axis.title = element_text(size = 20,color = "black"))+
  theme(axis.text = element_text(size = 20,color = "black"))
p1

p2<-df %>% 
  mutate(group=fct_relevel(group,
                           rev(c("Soil","Climate","Litter","Exp","Geo")))) %>% 
  group_by(group) %>% 
  summarise(sum_value=sum(abs(Estimate))) %>% 
  mutate(new_col=sum_value/sum(sum_value)) %>% 
  ggplot(aes(x=1,y=new_col,label=group))+
  geom_col(aes(fill=group),
           show.legend = F)+
  scale_fill_manual(values = cols)+
  scale_y_continuous( expand = c(0, 0))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        plot.title = element_text(hjust=0.5))+
  labs(y="Relative efect of estimates (%)",
       title = expression(Adj.R^2==0.17))+
  theme(axis.title = element_text(size = 18,color = "black"))+
  theme(axis.text = element_text(size = 18,color = "black"))+
  theme(axis.text.y = element_text(angle = 90))
p2
Fig6.1<-p2+p1+plot_layout(widths = c(0.5,3))
Fig6.1

## AE
df<-read_excel("mydf.xlsx",sheet=7)
df
cols<-c("#9832cb","#6a8d23","#fc0000","#4776fd","#f2a260")
p1<-df %>% 
  mutate(group=fct_relevel(group,
                           c("Water","Climate","Litter","Exp","Geo"))) %>% 
  group_by(group) %>% 
  arrange(Estimate,.by_group = T) %>% 
  mutate(var=fct_relevel(var,var)) %>% 
  mutate(signi=case_when(
    `Pr(>|z|)` < 0.1 & `Pr(>|z|)` >= 0.05 ~ '#',
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01 ~ '*',
    `Pr(>|z|)` < 0.01 & `Pr(>|z|)` >= 0.001 ~ '**',
    `Pr(>|z|)` < 0.001 ~ '***'
  )) %>% 
  ggplot(aes(x=Estimate,y=var))+
  geom_linerange(aes(xmin=Estimate-1.96*`Std.Error`,
                     xmax=Estimate+1.96*`Std.Error`,
                     color=group))+
  geom_point(aes(color=group),size=3)+
  theme_minimal()+
  labs(x="Parameter Estimate",y=NULL)+
  scale_x_continuous(limits = c(-0.6,0.8)
  )+
  guides(x=guide_axis_truncated(trunc_lower = -0.6,
                                trunc_upper = 0.8))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none")+
  scale_color_manual(values = rev(cols))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_text(aes(x=0.3,label=var),
            hjust=0,size=5)+
  geom_text(aes(x=-0.5,label=signi),size=5)+
  theme(axis.title = element_text(size = 20,color = "black"))+
  theme(axis.text = element_text(size = 20,color = "black"))
p1

p2<-df %>% 
  mutate(group=fct_relevel(group,
                           rev(c("Water","Climate","Litter","Exp","Geo")))) %>% 
  group_by(group) %>% 
  summarise(sum_value=sum(abs(Estimate))) %>% 
  mutate(new_col=sum_value/sum(sum_value)) %>% 
  ggplot(aes(x=1,y=new_col,label=group))+
  geom_col(aes(fill=group),
           show.legend = F)+
  scale_fill_manual(values = cols)+
  scale_y_continuous(limits = c(0,1),expand = c(0,0))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        plot.title = element_text(hjust=0.5))+
  labs(y="Relative efect of estimates (%)",
       title = expression(Adj.R^2==0.27))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(axis.title = element_text(size = 18,color = "black"))+
  theme(axis.text = element_text(size = 18,color = "black"))+
  theme(axis.text.y = element_text(angle = 90))
p2
Fig6.2<-p2+p1+plot_layout(widths = c(0.5,3))
Fig6.2

pdf("Fig6a-b.pdf", width =15, height =6)  
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(Fig6.1, vp = vplayout(1, 1))
print(Fig6.2, vp = vplayout(1, 2))
dev.off()

## forest
df<-read_excel("mydf.xlsx",sheet=8)
df
cols<-c("#9832cb","#6a8d23","#fc0000","#4776fd","#f2a260")
p1<-df %>% 
  mutate(group=fct_relevel(group,
                           c("Soil","Climate","Litter","Exp","Geo"))) %>% 
  group_by(group) %>% 
  arrange(Estimate,.by_group = T) %>% 
  mutate(var=fct_relevel(var,var)) %>% 
  mutate(signi=case_when(
    `Pr(>|z|)` < 0.1 & `Pr(>|z|)` >= 0.05 ~ '#',
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01 ~ '*',
    `Pr(>|z|)` < 0.01 & `Pr(>|z|)` >= 0.001 ~ '**',
    `Pr(>|z|)` < 0.001 ~ '***'
  )) %>% 
  ggplot(aes(x=Estimate,y=var))+
  geom_linerange(aes(xmin=Estimate-1.96*`Std.Error`,
                     xmax=Estimate+1.96*`Std.Error`,
                     color=group))+
  geom_point(aes(color=group),size=3)+
  theme_minimal()+
  labs(x="Parameter Estimate",y=NULL)+
  scale_x_continuous(limits = c(-0.3,0.4))+
  guides(x=guide_axis_truncated(trunc_lower = -0.3,
                                trunc_upper = 0.4))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none")+
  scale_color_manual(values = rev(cols))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_text(aes(x=0.2,label=var),
            hjust=0,size=5)+
  geom_text(aes(x=-0.2,label=signi),size=5)+
  theme(axis.title = element_text(size = 22,color = "black"))+
  theme(axis.text = element_text(size = 22,color = "black"))
p1

p2<-df %>% 
  mutate(group=fct_relevel(group,
                           rev(c("Soil","Climate","Litter","Exp","Geo")))) %>% 
  group_by(group) %>% 
  summarise(sum_value=sum(abs(Estimate))) %>% 
  mutate(new_col=sum_value/sum(sum_value)) %>% 
  ggplot(aes(x=1,y=new_col,label=group))+
  geom_col(aes(fill=group),
           show.legend = F)+
  scale_fill_manual(values = cols)+
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        plot.title = element_text(hjust=0.5))+
  labs(y="Relative efect of estimates (%)",
       title = expression(Adj.R^2==0.15))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(axis.title = element_text(size = 18,color = "black"))+
  theme(axis.text = element_text(size = 18,color = "black"))+
  theme(axis.text.y = element_text(angle = 90))
p2
Fig6.3<-p2+p1+plot_layout(widths = c(0.5,3))
Fig6.3

# Cropland
df<-read_excel("mydf.xlsx",sheet=9)
df
cols<-c("#9832cb","#6a8d23","#fc0000","#4776fd","#f2a260")
p1<-df %>% 
  mutate(group=fct_relevel(group,
                           c("Soil","Climate","Litter","Exp","Geo"))) %>% 
  group_by(group) %>% 
  arrange(Estimate,.by_group = T) %>% 
  mutate(var=fct_relevel(var,var)) %>% 
  mutate(signi=case_when(
    `Pr(>|z|)` < 0.1 & `Pr(>|z|)` >= 0.05 ~ '#',
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01 ~ '*',
    `Pr(>|z|)` < 0.01 & `Pr(>|z|)` >= 0.001 ~ '**',
    `Pr(>|z|)` < 0.001 ~ '***'
  )) %>% 
  ggplot(aes(x=Estimate,y=var))+
  geom_linerange(aes(xmin=Estimate-1.96*`Std.Error`,
                     xmax=Estimate+1.96*`Std.Error`,
                     color=group))+
  geom_point(aes(color=group),size=3)+
  theme_minimal()+
  labs(x="Parameter Estimate",y=NULL)+
  scale_x_continuous(limits = c(-180,200))+
  guides(x=guide_axis_truncated(trunc_lower = -140,
                                trunc_upper = 200))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none")+
  scale_color_manual(values = rev(cols))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_text(aes(x=40,label=var),
            hjust=0,size=5)+
  geom_text(aes(x=-50,label=signi),size=5)+
  theme(axis.title = element_text(size = 22,color = "black"))+
  theme(axis.text = element_text(size = 22,color = "black"))
p1

p2<-df %>% 
  mutate(group=fct_relevel(group,
                           rev(c("Soil","Climate","Litter","Exp","Geo")))) %>% 
  group_by(group) %>% 
  summarise(sum_value=sum(abs(Estimate))) %>% 
  mutate(new_col=sum_value/sum(sum_value)) %>% 
  ggplot(aes(x=1,y=new_col,label=group))+
  geom_col(aes(fill=group),
           show.legend = F)+
  scale_fill_manual(values = cols)+
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        plot.title = element_text(hjust=0.5))+
  labs(y="Relative efect of estimates (%)",
       title = expression(Adj.R^2==0.36))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(axis.title = element_text(size = 18,color = "black"))+
  theme(axis.text = element_text(size = 18,color = "black"))+
  theme(axis.text.y = element_text(angle = 90))
p2
Fig6.4<-p2+p1+plot_layout(widths = c(0.5,3))
Fig6.4

# Grassland
df<-read_excel("mydf.xlsx",sheet=10)
df
cols<-c("#9832cb","#6a8d23","#fc0000","#4776fd","#f2a260")
p1<-df %>% 
  mutate(group=fct_relevel(group,
                           c("Soil","Climate","Litter","Exp","Geo"))) %>% 
  group_by(group) %>% 
  arrange(Estimate,.by_group = T) %>% 
  mutate(var=fct_relevel(var,var)) %>% 
  mutate(signi=case_when(
    `Pr(>|z|)` < 0.1 & `Pr(>|z|)` >= 0.05 ~ '#',
    `Pr(>|z|)` < 0.05 & `Pr(>|z|)` >= 0.01 ~ '*',
    `Pr(>|z|)` < 0.01 & `Pr(>|z|)` >= 0.001 ~ '**',
    `Pr(>|z|)` < 0.001 ~ '***'
  )) %>% 
  ggplot(aes(x=Estimate,y=var))+
  geom_linerange(aes(xmin=Estimate-1.96*`Std.Error`,
                     xmax=Estimate+1.96*`Std.Error`,
                     color=group))+
  geom_point(aes(color=group),size=3)+
  theme_minimal()+
  labs(x="Parameter Estimate",y=NULL)+
  scale_x_continuous(limits = c(-30,45))+
  guides(x=guide_axis_truncated(trunc_lower = -30,
                                trunc_upper = 45))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(),
        axis.ticks.x = element_line(),
        legend.position = "none")+
  scale_color_manual(values = rev(cols))+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_text(aes(x=20,label=var),
            hjust=0,size=5)+
  geom_text(aes(x=-20,label=signi),size=5)+
  theme(axis.title = element_text(size = 22,color = "black"))+
  theme(axis.text = element_text(size = 22,color = "black"))
p1

p2<-df %>% 
  mutate(group=fct_relevel(group,
                           rev(c("Soil","Climate","Litter","Exp","Geo")))) %>% 
  group_by(group) %>% 
  summarise(sum_value=sum(abs(Estimate))) %>% 
  mutate(new_col=sum_value/sum(sum_value)) %>% 
  ggplot(aes(x=1,y=new_col,label=group))+
  geom_col(aes(fill=group),
           show.legend = F)+
  scale_fill_manual(values = cols)+
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0))+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        plot.title = element_text(hjust=0.5))+
  labs(y="Relative efect of estimates (%)",
       title = expression(Adj.R^2==0.333))+
  theme(axis.text.y = element_text(angle = 90))+
  theme(axis.title = element_text(size = 18,color = "black"))+
  theme(axis.text = element_text(size = 18,color = "black"))+
  theme(axis.text.y = element_text(angle = 90))
p2
Fig6.5<-p2+p1+plot_layout(widths = c(0.5,3))
Fig6.5

pdf("Fig6c-e.pdf", width =6, height =14)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(Fig6.3, vp = vplayout(1, 1))
print(Fig6.4, vp = vplayout(2, 1))
print(Fig6.5, vp = vplayout(3, 1))
dev.off()
###########################################################################
############ Funnel plot ####
res<-rma.mv(yi,vi,data=d2,random=~1|ID/observations,method="REML")
funnel(res, main="Funnel Plot")
############ Fail-safe N computations ####
fsn(yi, vi, data=d2,type="Rosenberg")