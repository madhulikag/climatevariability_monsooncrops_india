# Fig1 Varying trends in production and harvested area of Rice, Maize and traditional grains despite increases in yields

library(dplyr)
library(tidyverse)
library(sp)
library(sf)
library(ggpubr)
library(maptools)
library(classInt)
library(RColorBrewer)
library(raster)
library(terra)
library(pracma)
library(reshape2)
library(nlme)
library(gridExtra)
library(grid)
library(cowplot)
library(ppcor)
library(data.table)
library(foreign)
library(car)
library(gplots)
library(plm)
library(lme4)
library(arm)
library(lmerTest)
library(performance)
library(nortest)
library(sjPlot)
library(remotes)
library(Sleuth3)
library(purrr)
library(broom)
library(broom.mixed)

# Rearranging and selecting the data required

VDSA <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/ICRISAT-District Level Data.csv")
VDSA <- VDSA %>% relocate(Year, State.Code, Dist.Code, .before = State.Name) %>% arrange(Year, State.Code, Dist.Code)
write.csv(VDSA,"C:\\Users\\Madhulika\\Desktop\\Work\\CVSOP\\WSUV\\Research\\Simultaneous_extremes_impacts_on_yields\\Data\\OneDrive_1_18-03-2021\\ICRISAT_VDSA_1.csv", row.names = FALSE)
VDSA <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/ICRISAT_VDSA_1.csv")
VDSA$ID <- VDSA %>% group_by(State.Code, Dist.Code) %>% group_indices()
VDSA_Crops <- dplyr::select(VDSA, c(`Year`, `State.Code`, `Dist.Code`, `State.Name`, `Dist.Name`, `ID`, `RICE.YIELD..Kg.per.ha.`, `MAIZE.YIELD..Kg.per.ha.`, `KHARIF.SORGHUM.YIELD..Kg.per.ha.`, `PEARL.MILLET.YIELD..Kg.per.ha.`, `FINGER.MILLET.YIELD..Kg.per.ha.`, `RICE.PRODUCTION..1000.tons.`, `MAIZE.PRODUCTION..1000.tons.`, `KHARIF.SORGHUM.PRODUCTION..1000.tons.`, `PEARL.MILLET.PRODUCTION..1000.tons.`, `FINGER.MILLET.PRODUCTION..1000.tons.`, `RICE.AREA..1000.ha.`, `MAIZE.AREA..1000.ha.`, `KHARIF.SORGHUM.AREA..1000.ha.`, `PEARL.MILLET.AREA..1000.ha.`, `FINGER.MILLET.AREA..1000.ha.`))
cols_to_replace <- c(
  "RICE.YIELD..Kg.per.ha.",
  "MAIZE.YIELD..Kg.per.ha.",
  "KHARIF.SORGHUM.YIELD..Kg.per.ha.",
  "PEARL.MILLET.YIELD..Kg.per.ha.",
  "FINGER.MILLET.YIELD..Kg.per.ha.",
  "RICE.PRODUCTION..1000.tons.",
  "MAIZE.PRODUCTION..1000.tons.",
  "KHARIF.SORGHUM.PRODUCTION..1000.tons.",
  "PEARL.MILLET.PRODUCTION..1000.tons.",
  "FINGER.MILLET.PRODUCTION..1000.tons.",
  "RICE.AREA..1000.ha.",
  "MAIZE.AREA..1000.ha.",
  "KHARIF.SORGHUM.AREA..1000.ha.",
  "PEARL.MILLET.AREA..1000.ha.",
  "FINGER.MILLET.AREA..1000.ha."
)
VDSA_Crops <- VDSA_Crops %>%
  mutate_at(.vars = cols_to_replace, .funs = list(~ ifelse(. == -1, NA, .)))

# Yield, Production, and Harvested area plots for each crop

Yield <- dplyr::select(VDSA_Crops, c(1:11))
Yield_RY <- aggregate(Yield$RICE.YIELD..Kg.per.ha., by=list(Yield$Year), FUN=mean, na.rm=TRUE)
Yield_RY <- Yield_RY %>% 
  rename(Year=Group.1, RiceYield=x)
Yield_MZ <- aggregate(Yield$MAIZE.YIELD..Kg.per.ha., by=list(Yield$Year), FUN=mean, na.rm=TRUE)
Yield_MZ <- Yield_MZ %>% 
  rename(Year=Group.1, MaizeYield=x)
Yield_SGK <- aggregate(Yield$KHARIF.SORGHUM.YIELD..Kg.per.ha., by=list(Yield$Year), FUN=mean, na.rm=TRUE)
Yield_SGK <- Yield_SGK %>% 
  rename(Year=Group.1, SorghumKharifYield=x)
Yield_PM <- aggregate(Yield$PEARL.MILLET.YIELD..Kg.per.ha., by=list(Yield$Year), FUN=mean, na.rm=TRUE)
Yield_PM <- Yield_PM %>% 
  rename(Year=Group.1, PearlMilletYield=x)
Yield_FM <- aggregate(Yield$FINGER.MILLET.YIELD..Kg.per.ha., by=list(Yield$Year), FUN=mean, na.rm=TRUE)
Yield_FM <- Yield_FM %>% 
  rename(Year=Group.1, FingerMilletYield=x)
data_yield<-data.frame(Yield_RY,Yield_MZ,Yield_SGK,Yield_PM,Yield_FM)
data_yield$Year.1<-NULL
data_yield$Year.2<-NULL
data_yield$Year.3<-NULL
data_yield$Year.4<-NULL

yield <- ggplot(data_yield)+
  geom_line(aes(x = Year, y = RiceYield/1000, color="Rice"), size = 1)+
  geom_line(aes(x = Year, y = MaizeYield/1000, color="Maize"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = SorghumKharifYield/1000, color="Sorghum"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = PearlMilletYield/1000, color="Pearl Millet"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = FingerMilletYield/1000, color="Finger Millet"), size = 1, na.rm = TRUE)+
  ylab("Yield (in tons/ha)")+
  theme_classic()+
  xlab("")+
  theme(legend.title = element_blank(), text = element_text(size = 20))+
  scale_color_manual(breaks = c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"),
                     values=c("Rice" = "black", "Maize" = "orange","Sorghum" = "darkgreen","Pearl Millet" = "blue","Finger Millet" = "red"),
                     labels=c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"), name="")

Prod <- dplyr::select(VDSA_Crops, c(1:6,12:16))
Prod_RY <- aggregate(Prod$RICE.PRODUCTION..1000.tons., by=list(Prod$Year), FUN=sum, na.rm=TRUE)
Prod_RY <- Prod_RY %>%
  rename(Year=Group.1, RiceProd=x)
Prod_MZ <- aggregate(Prod$MAIZE.PRODUCTION..1000.tons., by=list(Prod$Year), FUN=sum, na.rm=TRUE)
Prod_MZ <- Prod_MZ %>%
  rename(Year=Group.1, MaizeProd=x)
Prod_SGK <- aggregate(Prod$KHARIF.SORGHUM.PRODUCTION..1000.tons., by=list(Prod$Year), FUN=sum, na.rm=TRUE)
Prod_SGK <- Prod_SGK %>%
  rename(Year=Group.1, SorghumProd=x)
Prod_PM <- aggregate(Prod$PEARL.MILLET.PRODUCTION..1000.tons., by=list(Prod$Year), FUN=sum, na.rm=TRUE)
Prod_PM <- Prod_PM %>%
  rename(Year=Group.1, PearlMilletProd=x)
Prod_FM <- aggregate(Prod$FINGER.MILLET.PRODUCTION..1000.tons., by=list(Prod$Year), FUN=sum, na.rm=TRUE)
Prod_FM <- Prod_FM %>%
  rename(Year=Group.1, FingerMilletProd=x)
data_prod<-data.frame(Prod_RY,Prod_MZ,Prod_SGK,Prod_PM,Prod_FM)
data_prod$Year.1<-NULL
data_prod$Year.2<-NULL
data_prod$Year.3<-NULL
data_prod$Year.4<-NULL

# 2015 Total kharif cereal production: 132355.07

prod <- ggplot(data_prod)+
  geom_line(aes(x = Year, y = RiceProd/1000, color="Rice"), size = 1)+
  geom_line(aes(x = Year, y = MaizeProd/1000, color="Maize"), size = 1)+
  geom_line(aes(x = Year, y = SorghumProd/1000, color="Sorghum"), size = 1)+
  geom_line(aes(x = Year, y = PearlMilletProd/1000, color="Pearl Millet"), size = 1)+
  geom_line(aes(x = Year, y = FingerMilletProd/1000, color="Finger Millet"), size = 1)+
  ylab("Production (in million tons)")+
  theme_classic()+
  xlab("")+
  theme(legend.title = element_blank(), text = element_text(size = 20))+
  scale_color_manual(breaks = c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"),
                     values=c("Rice" = "black", "Maize" = "orange","Sorghum" = "darkgreen","Pearl Millet" = "blue","Finger Millet" = "red"),
                     labels=c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"), name="")

Area <- dplyr::select(VDSA_Crops, c(1:6,17:21))
Area_RY <- aggregate(Area$RICE.AREA..1000.ha., by=list(Area$Year), FUN=sum, na.rm=TRUE)
Area_RY <- Area_RY %>% 
  rename(Year=Group.1, RiceArea=x)
Area_MZ <- aggregate(Area$MAIZE.AREA..1000.ha., by=list(Area$Year), FUN=sum, na.rm=TRUE)
Area_MZ <- Area_MZ %>% 
  rename(Year=Group.1, MaizeArea=x)
Area_SGK <- aggregate(Area$KHARIF.SORGHUM.AREA..1000.ha., by=list(Area$Year), FUN=sum, na.rm=TRUE)
Area_SGK <- Area_SGK %>% 
  rename(Year=Group.1, SorghumArea=x)
Area_PM <- aggregate(Area$PEARL.MILLET.AREA..1000.ha., by=list(Area$Year), FUN=sum, na.rm=TRUE)
Area_PM <- Area_PM %>% 
  rename(Year=Group.1, PearlMilletArea=x)
Area_FM <- aggregate(Area$FINGER.MILLET.AREA..1000.ha., by=list(Area$Year), FUN=sum, na.rm=TRUE)
Area_FM <- Area_FM %>% 
  rename(Year=Group.1, FingerMilletArea=x)
data_area<-data.frame(Area_RY,Area_MZ,Area_SGK,Area_PM,Area_FM)
data_area$Year.1<-NULL
data_area$Year.2<-NULL
data_area$Year.3<-NULL
data_area$Year.4<-NULL

area <- ggplot(data_area)+
  geom_line(aes(x = Year, y = RiceArea/1000, color="Rice"), size = 1)+
  geom_line(aes(x = Year, y = MaizeArea/1000, color="Maize"), size = 1)+
  geom_line(aes(x = Year, y = SorghumArea/1000, color="Sorghum"), size = 1)+
  geom_line(aes(x = Year, y = PearlMilletArea/1000, color="Pearl Millet"), size = 1)+
  geom_line(aes(x = Year, y = FingerMilletArea/1000, color="Finger Millet"), size = 1)+
  ylab("Harvested Area (in million ha)")+
  theme_classic()+
  xlab("")+
  theme(legend.title = element_blank(), text = element_text(size = 20))+
  scale_color_manual(breaks = c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"),
                     values=c("Rice" = "black", "Maize" = "orange","Sorghum" = "darkgreen","Pearl Millet" = "blue","Finger Millet" = "red"),
                     labels=c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"), name="")

# Loading Irrigated Areas, reorganizing the data, and calculating proportion of irrigated to total

VDSA_IrrigatedArea <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/ICRISAT-Irrigated_area.csv")
VDSA_IrrigatedArea <- VDSA_IrrigatedArea %>% relocate(Year, State.Code, Dist.Code, .before = State.Name) %>% arrange(Year, State.Code, Dist.Code)
VDSA_IrrigatedArea <- VDSA_IrrigatedArea %>% relocate(MAIZE.IRRIGATED.AREA..1000.ha., .before = KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.)
VDSA_IrrigatedArea$ID <- VDSA_IrrigatedArea %>% group_by(State.Code, Dist.Code) %>% group_indices()
VDSA_IrrigatedArea <- VDSA_IrrigatedArea %>% relocate(ID, .before = RICE.IRRIGATED.AREA..1000.ha.)
cols_to_replace <- c(
  "RICE.IRRIGATED.AREA..1000.ha.",
  "MAIZE.IRRIGATED.AREA..1000.ha.",
  "KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.",
  "PEARL.MILLET.IRRIGATED.AREA..1000.ha.",
  "FINGER.MILLET.IRRIGATED.AREA..1000.ha."
)
VDSA_IrrigatedArea <- VDSA_IrrigatedArea %>%
  mutate_at(.vars = cols_to_replace, .funs = list(~ ifelse(. == -1, NA, .)))
write.csv(VDSA_IrrigatedArea,"C:\\Users\\Madhulika\\Desktop\\Work\\CVSOP\\WSUV\\Research\\Simultaneous_extremes_impacts_on_yields\\Data\\OneDrive_1_18-03-2021\\ICRISAT_VDSA_IrrigatedArea.csv", row.names = FALSE)
VDSA_IrrigatedArea <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/ICRISAT_VDSA_IrrigatedArea_1.csv")

Harvested_IrrigatedArea <- merge(Area, VDSA_IrrigatedArea, by=c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))
Harv_Irr <- Harvested_IrrigatedArea

Harv_Irr$RIprop_irr_har <- ifelse(Harv_Irr$RICE.AREA..1000.ha. != 0, Harv_Irr$RICE.IRRIGATED.AREA..1000.ha. / Harv_Irr$RICE.AREA..1000.ha., NA)
Harv_Irr$MZprop_irr_har <- ifelse(Harv_Irr$MAIZE.AREA..1000.ha. != 0, Harv_Irr$MAIZE.IRRIGATED.AREA..1000.ha. / Harv_Irr$MAIZE.AREA..1000.ha., NA)
Harv_Irr$SGprop_irr_har <- ifelse(Harv_Irr$KHARIF.SORGHUM.AREA..1000.ha. != 0, Harv_Irr$KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. / Harv_Irr$KHARIF.SORGHUM.AREA..1000.ha., NA)
Harv_Irr$PMprop_irr_har <- ifelse(Harv_Irr$PEARL.MILLET.AREA..1000.ha. != 0, Harv_Irr$PEARL.MILLET.IRRIGATED.AREA..1000.ha. / Harv_Irr$PEARL.MILLET.AREA..1000.ha., NA)
Harv_Irr$FMprop_irr_har <- ifelse(Harv_Irr$FINGER.MILLET.AREA..1000.ha. != 0, Harv_Irr$FINGER.MILLET.IRRIGATED.AREA..1000.ha. / Harv_Irr$FINGER.MILLET.AREA..1000.ha., NA)

# Harvested areas > Irrigated areas can be explained by the fact that in some districts, the planted areas can be larger than harvested areas

# Weight yields by harvested area

# 1. Calculation of Total Harvested Areas

Yield_Area <- merge(Yield, Harv_Irr, by=c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))
RI_tot_har_area <- Yield_Area %>%
  group_by(Year) %>%
  summarize(RI_tot_har_area = sum(RICE.AREA..1000.ha., na.rm = TRUE))
MZ_tot_har_area <- Yield_Area %>%
  group_by(Year) %>%
  summarize(MZ_tot_har_area = sum(MAIZE.AREA..1000.ha., na.rm = TRUE))
SG_tot_har_area <- Yield_Area %>%
  group_by(Year) %>%
  summarize(SG_tot_har_area = sum(KHARIF.SORGHUM.AREA..1000.ha., na.rm = TRUE))
PM_tot_har_area <- Yield_Area %>%
  group_by(Year) %>%
  summarize(PM_tot_har_area = sum(PEARL.MILLET.AREA..1000.ha., na.rm = TRUE))
FM_tot_har_area <- Yield_Area %>%
  group_by(Year) %>%
  summarize(FM_tot_har_area = sum(FINGER.MILLET.AREA..1000.ha., na.rm = TRUE))

crop_tot_har_area <- list(RI_tot_har_area, MZ_tot_har_area, SG_tot_har_area, PM_tot_har_area, FM_tot_har_area)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Tot_har_area <- Reduce(merge_fun, crop_tot_har_area)
Yield_Area <- merge(Yield_Area, Tot_har_area, by = "Year")

# 2. Multiply Yields with Area

Yield_Area <- Yield_Area %>%
  mutate(WEIGHTED.RICE.YIELD..Kg.per.ha. = RICE.YIELD..Kg.per.ha. * RICE.AREA..1000.ha.,
         WEIGHTED.MAIZE.YIELD..Kg.per.ha. = MAIZE.YIELD..Kg.per.ha. * MAIZE.AREA..1000.ha.,
         WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha. = KHARIF.SORGHUM.YIELD..Kg.per.ha. * KHARIF.SORGHUM.AREA..1000.ha.,
         WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha. = PEARL.MILLET.YIELD..Kg.per.ha. * PEARL.MILLET.AREA..1000.ha.,
         WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha. = FINGER.MILLET.YIELD..Kg.per.ha. * FINGER.MILLET.AREA..1000.ha.)

# 3. Calculate the weighted average yields using the formula: https://www.worldpop.org/methods/pwd/

Yield_Area_Rice <- subset(Yield_Area, select = c('Year', 'Dist.Code', 'RICE.YIELD..Kg.per.ha.', 'RICE.AREA..1000.ha.', 'RI_tot_har_area', 'WEIGHTED.RICE.YIELD..Kg.per.ha.'))
Yield_Area_Maize <- subset(Yield_Area, select = c('Year', 'Dist.Code', 'MAIZE.YIELD..Kg.per.ha.', 'MAIZE.AREA..1000.ha.', 'MZ_tot_har_area', 'WEIGHTED.MAIZE.YIELD..Kg.per.ha.'))
Yield_Area_Sorghum <- subset(Yield_Area, select = c('Year', 'Dist.Code', 'KHARIF.SORGHUM.YIELD..Kg.per.ha.', 'KHARIF.SORGHUM.AREA..1000.ha.', 'SG_tot_har_area', 'WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha.'))
Yield_Area_PearlMillet <- subset(Yield_Area, select = c('Year', 'Dist.Code', 'PEARL.MILLET.YIELD..Kg.per.ha.', 'PEARL.MILLET.AREA..1000.ha.', 'PM_tot_har_area', 'WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha.'))
Yield_Area_FingerMillet <- subset(Yield_Area, select = c('Year', 'Dist.Code', 'FINGER.MILLET.YIELD..Kg.per.ha.', 'FINGER.MILLET.AREA..1000.ha.', 'FM_tot_har_area', 'WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha.'))

Rice_aggregated_data <- Yield_Area_Rice %>%
  group_by(Year) %>%
  summarize(Average_RICE.YIELD = mean(RICE.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_RICE_YIELD_Kg.per.ha = sum(WEIGHTED.RICE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(RI_tot_har_area))

Maize_aggregated_data <- Yield_Area_Maize %>%
  group_by(Year) %>%
  summarize(Average_MAIZE.YIELD = mean(MAIZE.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_MAIZE_YIELD_Kg.per.ha = sum(WEIGHTED.MAIZE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(MZ_tot_har_area))

Sorghum_aggregated_data <- Yield_Area_Sorghum %>%
  group_by(Year) %>%
  summarize(Average_SORGHUM.YIELD = mean(KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_SORGHUM_YIELD_Kg.per.ha = sum(WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm=TRUE)/mean(SG_tot_har_area))

PearlMillet_aggregated_data <- Yield_Area_PearlMillet %>%
  group_by(Year) %>%
  summarize(Average_PEARLMILLET.YIELD = mean(PEARL.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_PEARLMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(PM_tot_har_area))

FingerMillet_aggregated_data <- Yield_Area_FingerMillet %>%
  group_by(Year) %>%
  summarize(Average_FINGERMILLET.YIELD = mean(FINGER.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_FINGERMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(FM_tot_har_area))

crop_aggregated_data <- list(Rice_aggregated_data, Maize_aggregated_data, Sorghum_aggregated_data, PearlMillet_aggregated_data, FingerMillet_aggregated_data)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Crop_yield_aggregated_data <- Reduce(merge_fun, crop_aggregated_data)
Crop_yield_aggregated_data

weighted_yield <- ggplot(Crop_yield_aggregated_data)+
  geom_line(aes(x = Year, y = Total_WEIGHTED_RICE_YIELD_Kg.per.ha/1000, color="Rice"), size = 1)+
  geom_line(aes(x = Year, y = Total_WEIGHTED_MAIZE_YIELD_Kg.per.ha/1000, color="Maize"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = Total_WEIGHTED_SORGHUM_YIELD_Kg.per.ha/1000, color="Sorghum"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = Total_WEIGHTED_PEARLMILLET_YIELD_Kg.per.ha/1000, color="Pearl Millet"), size = 1, na.rm = TRUE)+
  geom_line(aes(x = Year, y = Total_WEIGHTED_FINGERMILLET_YIELD_Kg.per.ha/1000, color="Finger Millet"), size = 1, na.rm = TRUE)+
  ylab("Yields (in tons/ha)")+
  theme_classic()+
  xlab("")+
  theme(legend.title = element_blank(), text = element_text(size = 20))+
  scale_color_manual(breaks = c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"),
                     values=c("Rice" = "black", "Maize" = "orange","Sorghum" = "darkgreen","Pearl Millet" = "blue","Finger Millet" = "red"),
                     labels=c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"), name="")

# Absolute Harvested Area Anomalies

# 1. Calculate annual irrigated areas

Area_RY <- aggregate(Harv_Irr$RICE.IRRIGATED.AREA..1000.ha., by=list(Harv_Irr$Year), FUN=sum, na.rm=TRUE)
Area_RY <- Area_RY %>% 
  rename(Year=Group.1, RiceIrrigatedArea=x)
Area_MZ <- aggregate(Harv_Irr$MAIZE.IRRIGATED.AREA..1000.ha., by=list(Harv_Irr$Year), FUN=sum, na.rm=TRUE)
Area_MZ <- Area_MZ %>% 
  rename(Year=Group.1, MaizeIrrigatedArea=x)
Area_SGK <- aggregate(Harv_Irr$KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., by=list(Harv_Irr$Year), FUN=sum, na.rm=TRUE)
Area_SGK <- Area_SGK %>% 
  rename(Year=Group.1, SorghumIrrigatedArea=x)
Area_PM <- aggregate(Harv_Irr$PEARL.MILLET.IRRIGATED.AREA..1000.ha., by=list(Harv_Irr$Year), FUN=sum, na.rm=TRUE)
Area_PM <- Area_PM %>% 
  rename(Year=Group.1, PearlMilletIrrigatedArea=x)
Area_FM <- aggregate(Harv_Irr$FINGER.MILLET.IRRIGATED.AREA..1000.ha., by=list(Harv_Irr$Year), FUN=sum, na.rm=TRUE)
Area_FM <- Area_FM %>% 
  rename(Year=Group.1, FingerMilletIrrigatedArea=x)
data_irrigated_area<-data.frame(Area_RY,Area_MZ,Area_SGK,Area_PM,Area_FM)
data_irrigated_area$Year.1<-NULL
data_irrigated_area$Year.2<-NULL
data_irrigated_area$Year.3<-NULL
data_irrigated_area$Year.4<-NULL

# Replace 2017 Rice Irrigated Area value 15967.19 which is very low with the rolling average 2015 value 21898.29

data_irrigated_area[data_irrigated_area$Year == 2017, "RiceIrrigatedArea"] <- 21898.29	
irr_area <- ggplot(data_irrigated_area)+
  geom_line(aes(x = Year, y = RiceIrrigatedArea/1000, color="Rice"), size = 1)+
  geom_line(aes(x = Year, y = MaizeIrrigatedArea/1000, color="Maize"), size = 1)+
  geom_line(aes(x = Year, y = SorghumIrrigatedArea/1000, color="Sorghum"), size = 1)+
  geom_line(aes(x = Year, y = PearlMilletIrrigatedArea/1000, color="Pearl Millet"), size = 1)+
  geom_line(aes(x = Year, y = FingerMilletIrrigatedArea/1000, color="Finger Millet"), size = 1)+
  ylab("Irrigated Area (in million ha)")+
  theme_classic()+
  xlab("")+
  theme(legend.title = element_blank(), text = element_text(size = 20))+
  scale_color_manual(breaks = c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"),
                     values=c("Rice" = "black", "Maize" = "orange","Sorghum" = "darkgreen","Pearl Millet" = "blue","Finger Millet" = "red"),
                     labels=c("Rice", "Maize", "Sorghum", "Pearl Millet", "Finger Millet"), name="")
write.csv(data_irrigated_area, "/Users/madhulika/Desktop/data_irrigated_area.csv", row.names = FALSE)


