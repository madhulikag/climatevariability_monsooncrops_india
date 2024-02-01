#FigS10 Linear mixed effects model - actual yields and actual rainfall and temperature - rainfed yields


actual_tmax <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/CRU/tmax_cropped1.csv")
colnames(actual_tmax)[27:74] <- 1968:2015
actual_tmax1 <- melt(actual_tmax, id=c(1:26))
colnames(actual_tmax1)[27:28] <- c("Year", "mean_actual_tmax")

actual_rain <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/IMD_data/rain_clipped1_2.csv")
colnames(actual_rain)[27:74] <- 1968:2015
actual_rain1 <- melt(actual_rain, id=c(1:26))
colnames(actual_rain1)[27:28] <- c("Year", "total_actual_rain")

actual_tmax_rain <- merge(actual_tmax1, actual_rain1, by=c("Year", "JDist.Code"))
actual_tmax_rain1 <- actual_tmax_rain[-c(29:53)]
colnames(actual_tmax_rain1)[2] <- c("Dist.Code")
yield_climate <- merge(Harv_Irr_Yield1, actual_tmax_rain1, by=c("Year", "Dist.Code"))
yield_climate_nino34_iod <- merge(yield_climate, nino34_iod_phase, by="Year")

yield_climate_nino34_iod$Rice <- yield_climate_nino34_iod$RICE.YIELD..Kg.per.ha.
yield_climate_nino34_iod$Maize <- yield_climate_nino34_iod$MAIZE.YIELD..Kg.per.ha.
yield_climate_nino34_iod$Sorghum <- yield_climate_nino34_iod$KHARIF.SORGHUM.YIELD..Kg.per.ha.
yield_climate_nino34_iod$PearlMillet <- yield_climate_nino34_iod$PEARL.MILLET.YIELD..Kg.per.ha.
yield_climate_nino34_iod$FingerMillet <- yield_climate_nino34_iod$FINGER.MILLET.YIELD..Kg.per.ha.

yield_climate_nino34_iod$rain <- yield_climate_nino34_iod$total_actual_rain
yield_climate_nino34_iod$tmax <- yield_climate_nino34_iod$mean_actual_tmax


Maize_Rice_complete <- subset(yield_climate_nino34_iod, RI.MZ==1 & RiceIrrigation_Type=="Rainfed_Rice" & MaizeIrrigation_Type=="Rainfed_Maize")
Sorghum_Rice_complete <- subset(yield_climate_nino34_iod, RI.SGK==1 & RiceIrrigation_Type=="Rainfed_Rice" & SorghumIrrigation_Type=="Rainfed_Sorghum")
PearlMillet_Rice_complete <- subset(yield_climate_nino34_iod, RI.PM==1 & RiceIrrigation_Type=="Rainfed_Rice" & PearlMilletIrrigation_Type=="Rainfed_PearlMillet")
FingerMillet_Rice_complete <- subset(yield_climate_nino34_iod, RI.FM==1 & RiceIrrigation_Type=="Rainfed_Rice" & FingerMilletIrrigation_Type=="Rainfed_FingerMillet")

###No. of districts

length(unique(Maize_Rice_complete$Dist.Code))
length(unique(Sorghum_Rice_complete$Dist.Code))
length(unique(PearlMillet_Rice_complete$Dist.Code))
length(unique(FingerMillet_Rice_complete$Dist.Code))

Soil_data <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/ICRISAT-District Level Soil Data.csv")
# Assuming your data frame is named Soil_data
Soil_data <- separate(Soil_data, SOIL.TYPE.PERCENT..Percent., into = c("Soil_Type1", "Soil_Type2", "Soil_Type3"), sep = ";", extra = "merge")

# Separate the column into two new columns
Soil_data <- separate(Soil_data, Soil_Type1, into = c("Soil_Type1", "Percent_Soil1"), sep = " -")
Soil_data <- separate(Soil_data, Soil_Type2, into = c("Soil_Type2", "Percent_Soil2"), sep = "-")
Soil_data <- separate(Soil_data, Soil_Type3, into = c("Soil_Type3", "Percent_Soil3"), sep = " -")
Soil_data <- Soil_data %>%
  filter(!is.na(Soil_Type1))
Soil_data <- Soil_data %>%
  mutate(Soil_Type1 = ifelse(Soil_Type1 == "LOAMY ALFISOL", "LOAMY ALFISOLS", ifelse(Soil_Type1 == "VERTIC SOLS", "VERTIC SOILS",ifelse(Soil_Type1 == "VRTIC SOILS", "VERTIC SOILS", ifelse(Soil_Type1 == "PSSAMNETS", "PSSAMENTS", ifelse(Soil_Type1 == "SANDY ALFISOL", "SANDY ALFISOLS", ifelse(Soil_Type1 == "INNCEPTISOLS", "INCEPTISOLS", Soil_Type1)))))))
Soil_data <- Soil_data %>%
  mutate(Soil_Type2 = ifelse(Soil_Type2 == " VERTIC OSILS ", "VERTIC SOILS", ifelse(Soil_Type2 == " VERTI SOLS ", "VERTIC SOILS",ifelse(Soil_Type2 == " VERTIC SOILS ", "VERTIC SOILS", ifelse(Soil_Type2 == " PSSAMENTS ", "PSSAMENTS", ifelse(Soil_Type2 == " PSSAMNETS ", "PSSAMENTS", ifelse(Soil_Type2 == " VERRTISOLS ", "VERTISOLS", ifelse(Soil_Type2 == " VERTISOLS ", "VERTISOLS",ifelse(Soil_Type2 == " SANDY ALFISOL ", "SANDY ALFISOLS", ifelse(Soil_Type2 == " SANDY ALFISOLS ", "SANDY ALFISOLS",ifelse(Soil_Type2 == " USTALF/USTOLLS ", "USTALF/USTOLLS", ifelse(Soil_Type2 == " UDOLLS/UDALFS ", "UDOLLS/UDALFS", ifelse(Soil_Type2 == " UDUPTS/UDALFS ", "UDUPTS/UDALFS", ifelse(Soil_Type2 == " UDOLLS/UDALFS ", "UDOLLS/UDALFS", ifelse(Soil_Type2 == " ORTHIDS ", "ORTHIDS", ifelse(Soil_Type2 == " LOAMY ALFISOLS ", "LOAMY ALFISOLS", ifelse(Soil_Type2 == " INCEPTISOLS ", "INCEPTISOLS", ifelse(Soil_Type2 == " USTALF/USTOLLS", "USTALF/USTOLLS", Soil_Type2))))))))))))))))))
Soil_data <- Soil_data %>%
  mutate(Soil_Type3 = ifelse(Soil_Type3 == " VERTIC SOLS", "VERTIC SOILS", ifelse(Soil_Type3 == " VERTIC SOILS", "VERTIC SOILS",    ifelse(Soil_Type3 == " VERTISOLS", "VERTISOLS",ifelse(Soil_Type3 == " SANDY ALFISOL", "SANDY ALFISOLS", ifelse(Soil_Type3 == " USTALF/USTOLLS", "USTALF/USTOLLS", ifelse(Soil_Type3 == " UDUPTS/UDALFS", "UDUPTS/UDALFS", ifelse(Soil_Type3 == " ORTHIDS", "ORTHIDS", Soil_Type3))))))))
# Assuming your data frame is named Soil_data
Soil_data <- Soil_data %>%
  mutate(SoilType1 = case_when(
    Soil_Type1 %in% c("LOAMY ALFISOLS", "SANDY ALFISOLS", "UDALFS") ~ "Alfisols",
    Soil_Type1 %in% c("PSAMMENTS-OCHREPTS", "DYSTROPEPTS-ORTHENTS", "FLUVENTS-OCHREPTS", "ORTHENTS-TROPEPTS") ~ "Entisols-Inceptisols-Mix",
    Soil_Type1 %in% c("FLUVENTS-ORTHENTS", "PSAMMENTS-ORTHENTS", "PSSAMENTS") ~ "Entisols",
    Soil_Type1 == "INCEPTISOLS" ~ "Inceptisols",
    Soil_Type1 %in% c("VERTISOLS", "VERTIC SOILS") ~ "Vertisols",
    Soil_Type1 == "ORTHIDS" ~ "Aridisols",
    Soil_Type1 %in% c("USTALFS-OCHREPTS", "USTALFS-TROPEPTS", "UDALFS-OCHREPTS", "UDUPTS/UDALFS") ~ "Alfisols-Inceptisols-Mix",
    Soil_Type1 %in% c("USTALF/USTOLLS", "UDOLLS/UDALFS") ~ "Alfisols-Mollisols-Mix",
    TRUE ~ Soil_Type1
  ))
Soil_data <- Soil_data %>%
  mutate(SoilType2 = case_when(
    Soil_Type2 %in% c("USTALF/USTOLLS", "UDOLLS/UDALFS") ~ "Alfisols-Mollisols-Mix",
    Soil_Type2 %in% c("LOAMY ALFISOLS", "SANDY ALFISOLS") ~ "Alfisols",
    Soil_Type2 %in% c("VERTISOLS", "VERTIC SOILS") ~ "Vertisols",
    Soil_Type2 == "INCEPTISOLS" ~ "Inceptisols",
    Soil_Type2 == "ORTHIDS" ~ "Aridisols",
    Soil_Type2 == "PSSAMENTS" ~ "Entisols",
    Soil_Type2 == "UDUPTS/UDALFS" ~ "Alfisols-Inceptisols-Mix",
    TRUE ~ Soil_Type2
  ))
Soil_data <- Soil_data %>%
  mutate(SoilType3 = case_when(
    Soil_Type3 == "USTALF/USTOLLS" ~ "Alfisols-Mollisols-Mix",
    Soil_Type3 == "SANDY ALFISOLS" ~ "Alfisols",
    Soil_Type3 %in% c("VERTISOLS", "VERTIC SOILS") ~ "Vertisols",
    Soil_Type3 == "ORTHIDS" ~ "Aridisols",
    Soil_Type3 == "UDUPTS/UDALFS" ~ "Alfisols-Inceptisols-Mix",
    TRUE ~ Soil_Type3
  ))

Soil_data1 <- Soil_data
Soil_data1$Soil_Type1 <- NULL
Soil_data1$Soil_Type2 <- NULL
Soil_data1$Soil_Type3 <- NULL

Soil_data1$Percent_Soil1 <- as.numeric(gsub("%", "", Soil_data1$Percent_Soil1))
Soil_data1$Percent_Soil2 <- as.numeric(gsub("%", "", Soil_data1$Percent_Soil2))
Soil_data1$Percent_Soil3 <- as.numeric(gsub("%", "", Soil_data1$Percent_Soil3))

Soil_data1 <- Soil_data1 %>% 
  mutate(Percent_Soil1 = replace(Percent_Soil1, Percent_Soil1 == 1005, 100),
         Percent_Soil2 = replace(Percent_Soil2, Percent_Soil2 == 505, 50)
  )

Soil_data1$SoilType_1 <- Soil_data1$SoilType1
Soil_data1 <- Soil_data1 %>% 
  mutate(
    SoilType_2 = ifelse(SoilType1 != SoilType2, SoilType2, NA),
    SoilType_3 = ifelse(SoilType1 != SoilType3 & SoilType2 != SoilType3, SoilType3, NA)
  )
Soil_data1 <- Soil_data1 %>%
  mutate(
    Soil_per1 = ifelse(SoilType1 == SoilType2, Percent_Soil1+Percent_Soil2, Percent_Soil1),
    Soil_per1 = ifelse(Percent_Soil1 == 100, 100, Soil_per1),
    Soil_percent1 = ifelse(SoilType1 == SoilType3, Soil_per1+Percent_Soil3, Soil_per1),
    Soil_percent1 = ifelse(is.na(Soil_percent1), Soil_per1, Soil_percent1)
  )
Soil_data1$Soil_per1 <- NULL

Soil_data1 <- Soil_data1 %>%
  mutate(
    Soil_per2 = ifelse(SoilType1 == SoilType2, NA, Percent_Soil2),
    Soil_percent2 = ifelse(SoilType2 == SoilType3, Soil_per2+Percent_Soil3, Soil_per2),
    Soil_percent2 = ifelse(is.na(Soil_percent2), Soil_per2, Soil_percent2)
  )
Soil_data1$Soil_per2 <- NULL

Soil_data1 <- Soil_data1 %>%
  mutate(
    Soil_percent3 = ifelse(is.na(SoilType_3), NA, Percent_Soil3)
  )

Soil_data2 <- Soil_data1
Soil_data2$Percent_Soil1 <- NULL
Soil_data2$Percent_Soil2 <- NULL
Soil_data2$Percent_Soil3 <- NULL
Soil_data2$SoilType1 <- NULL
Soil_data2$SoilType2 <- NULL
Soil_data2$SoilType3 <- NULL

Soil_data2 <- Soil_data2 %>%
  mutate(
    SoilType_2 = ifelse(SoilType_1 == "Vertisols" & is.na(SoilType_2) & SoilType_3 == "Alfisols", SoilType_3, SoilType_2),
    SoilType_3 = ifelse(SoilType_1 == "Vertisols" & SoilType_2 == "Alfisols" & SoilType_3 == "Alfisols", NA, SoilType_3),
    Soil_percent2 = ifelse(Dist.Name == "Mahabubnagar" & SoilType_1 == "Vertisols" & SoilType_2 == "Alfisols" & is.na(SoilType_3), Soil_percent3, Soil_percent2),
    Soil_percent3 = ifelse(Dist.Name == "Mahabubnagar" & SoilType_1 == "Vertisols" & SoilType_2 == "Alfisols" & is.na(SoilType_3), NA, Soil_percent3)
  )

Soil_data2$Total <- rowSums(Soil_data2[, c("Soil_percent1", "Soil_percent2", "Soil_percent3")], na.rm = TRUE)
Soil_data2 <- Soil_data2 %>%
  mutate(Soil_percent2 = ifelse(Total == 110, 40, Soil_percent2))
Soil_data2$Total <- NULL
Soil_data2$Total <- rowSums(Soil_data2[, c("Soil_percent1", "Soil_percent2", "Soil_percent3")], na.rm = TRUE)
Soil_data2$Total <- NULL

Soil_data3 <- Soil_data2
Soil_data3$SoilType1 <- Soil_data3$SoilType_1
Soil_data3$SoilType2 <- Soil_data3$SoilType_2
Soil_data3$SoilType3 <- Soil_data3$SoilType_3
Soil_data3$SoilType_1 <- NULL
Soil_data3$SoilType_2 <- NULL
Soil_data3$SoilType_3 <- NULL

Soil_data3_wide1 <- Soil_data3 %>%
  pivot_wider(
    id_cols = c(Dist.Code, State.Code, State.Name, Dist.Name),
    names_from = SoilType1, 
    values_from = Soil_percent1,
    values_fill = 0    # Replace NA with 0 if there are missing values
  )
colnames(Soil_data3_wide1)[-c(1:4)] <- paste0(colnames(Soil_data3_wide1)[-c(1:4)], "1")

Soil_data3_wide2 <- Soil_data3 %>%
  pivot_wider(
    id_cols = c(Dist.Code, State.Code, State.Name, Dist.Name),
    names_from = SoilType2, 
    values_from = Soil_percent2,
    values_fill = 0    # Replace NA with 0 if there are missing values
  )
Soil_data3_wide2$`NA` <- NULL
colnames(Soil_data3_wide2)[-c(1:4)] <- paste0(colnames(Soil_data3_wide2)[-c(1:4)], "2")

Soil_data3_wide3 <- Soil_data3 %>%
  pivot_wider(
    id_cols = c(Dist.Code, State.Code, State.Name, Dist.Name),
    names_from = SoilType3, 
    values_from = Soil_percent3,
    values_fill = 0    # Replace NA with 0 if there are missing values
  )
Soil_data3_wide3$`NA` <- NULL
colnames(Soil_data3_wide3)[-c(1:4)] <- paste0(colnames(Soil_data3_wide3)[-c(1:4)], "3")

Soil_data3_wide12 <- merge(Soil_data3_wide1, Soil_data3_wide2, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))
Soil_data3_wide123 <- merge(Soil_data3_wide12, Soil_data3_wide3, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))
Soil_data3_wide123_1 <- Soil_data3_wide123 %>%
  mutate(across(-c(Dist.Code, State.Code, State.Name, Dist.Name), ~./100))
Soil_data3_wide123_1$`Entisols-Inceptisols-Mix1` <- NULL

Soil_data3_wide123_1 <- Soil_data3_wide123_1 %>%
  mutate(Alfisols = Alfisols1 + Alfisols2 + Alfisols3,
         `Alfisols-Mollisols-Mix` = `Alfisols-Mollisols-Mix1` + `Alfisols-Mollisols-Mix2` + `Alfisols-Mollisols-Mix3`,
         Vertisols = Vertisols1 + Vertisols2 + Vertisols3,
         Aridisols = Aridisols1 + Aridisols2 + Aridisols3,
         `Alfisols-Inceptisols-Mix` = `Alfisols-Inceptisols-Mix1` + `Alfisols-Inceptisols-Mix2` + `Alfisols-Inceptisols-Mix3`,
         Inceptisols = Inceptisols1 + Inceptisols2,
         Entisols = Entisols1 + Entisols2)

Soil_data3_wide123_2 <- Soil_data3_wide123_1 %>%
  dplyr::select(Dist.Code, State.Code, State.Name, Dist.Name, Alfisols, `Alfisols-Mollisols-Mix`, `Alfisols-Inceptisols-Mix`, Vertisols, Aridisols, Inceptisols, Entisols)
Soil_data3_wide123_2
}
Maize_Rice_wide_soil1 <- merge(Maize_Rice_complete, Soil_data3_wide123_2, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))
Sorghum_Rice_wide_soil1 <- merge(Sorghum_Rice_complete, Soil_data3_wide123_2, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))
PearlMillet_Rice_wide_soil1 <- merge(PearlMillet_Rice_complete, Soil_data3_wide123_2, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))
FingerMillet_Rice_wide_soil1 <- merge(FingerMillet_Rice_complete, Soil_data3_wide123_2, by = c("Dist.Code", "State.Code", "State.Name", "Dist.Name"))

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}



# Fit the model with standardized predictors
m1.1 <- lmer(Rice ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data = Maize_Rice_wide_soil1)
# Summary of the model
summary(m1.1)
# Standardized coefficients
stdCoef.merMod(m1.1)
# R-squared
r2_nakagawa(m1.1)

m1.2 <- lmer(Maize ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data = Maize_Rice_wide_soil1)
summary(m1.2)
stdCoef.merMod(m1.2)
r2_nakagawa(m1.2)

m2.1 <- lmer(Rice ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data=Sorghum_Rice_wide_soil1)
summary(m2.1)
stdCoef.merMod(m2.1)
r2_nakagawa(m2.1)

m2.2 <- lmer(Sorghum ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data=Sorghum_Rice_wide_soil1)
summary(m2.2)
stdCoef.merMod(m2.2)
r2_nakagawa(m2.2)

m3.1 <- lmer(Rice ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data=PearlMillet_Rice_wide_soil1)
summary(m3.1)
stdCoef.merMod(m3.1)
r2_nakagawa(m3.1)

m3.2 <- lmer(PearlMillet ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + Entisols + (1|Dist.Code) + (1|Year) - 1, data=PearlMillet_Rice_wide_soil1)
summary(m3.2)
stdCoef.merMod(m3.2)
r2_nakagawa(m3.2)

m4.1 <- lmer(Rice ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + (1|Dist.Code) + (1|Year) - 1, data=FingerMillet_Rice_wide_soil1)
summary(m4.1)
stdCoef.merMod(m4.1)
r2_nakagawa(m4.1)

m4.2 <- lmer(FingerMillet ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + Alfisols + `Alfisols-Mollisols-Mix` + `Alfisols-Inceptisols-Mix` + Vertisols + Aridisols + Inceptisols + (1|Dist.Code) + (1|Year) - 1, data=FingerMillet_Rice_wide_soil1)
summary(m4.2)
stdCoef.merMod(m4.2)
r2_nakagawa(m4.2)

tab_model(m1.1, m1.2, m2.1, m2.2)

tab_model(m3.1, m3.2, m4.1, m4.2)

Soil_data3_long <- Soil_data3 %>%
  pivot_longer(
    cols = starts_with("SoilType") | starts_with("Soil_percent"),
    names_to = c(".value", "index"),
    names_pattern = "([^0-9]+)([0-9]+)"
  )

Soil_data3_long_1 = Soil_data3_long %>%
  dplyr::filter(index == 1)
Soil_data3_long_2 = Soil_data3_long %>%
  dplyr::filter(index == 2)
Soil_data3_long_3 = Soil_data3_long %>%
  dplyr::filter(index == 3)

Maize_Rice_complete_soil1 <- merge(Maize_Rice_complete, Soil_data3_long_1, by = "Dist.Code")
colnames(Maize_Rice_complete_soil1)[2] <- "Year"
Maize_Rice_complete_soil2 <- merge(Maize_Rice_complete, Soil_data3_long_2, by = "Dist.Code")
colnames(Maize_Rice_complete_soil2)[2] <- "Year"
Maize_Rice_complete_soil3 <- merge(Maize_Rice_complete, Soil_data3_long_3, by = "Dist.Code")
colnames(Maize_Rice_complete_soil3)[2] <- "Year"

Sorghum_Rice_complete_soil1 <- merge(Sorghum_Rice_complete, Soil_data3_long_1, by = "Dist.Code")
colnames(Sorghum_Rice_complete_soil1)[2] <- "Year"
Sorghum_Rice_complete_soil2 <- merge(Sorghum_Rice_complete, Soil_data3_long_2, by = "Dist.Code")
colnames(Sorghum_Rice_complete_soil2)[2] <- "Year"
Sorghum_Rice_complete_soil3 <- merge(Sorghum_Rice_complete, Soil_data3_long_3, by = "Dist.Code")
colnames(Sorghum_Rice_complete_soil3)[2] <- "Year"

PearlMillet_Rice_complete_soil1 <- merge(PearlMillet_Rice_complete, Soil_data3_long_1, by = "Dist.Code")
colnames(PearlMillet_Rice_complete_soil1)[2] <- "Year"
PearlMillet_Rice_complete_soil2 <- merge(PearlMillet_Rice_complete, Soil_data3_long_2, by = "Dist.Code")
colnames(PearlMillet_Rice_complete_soil2)[2] <- "Year"
PearlMillet_Rice_complete_soil3 <- merge(PearlMillet_Rice_complete, Soil_data3_long_3, by = "Dist.Code")
colnames(PearlMillet_Rice_complete_soil3)[2] <- "Year"

FingerMillet_Rice_complete_soil1 <- merge(FingerMillet_Rice_complete, Soil_data3_long_1, by = "Dist.Code")
colnames(FingerMillet_Rice_complete_soil1)[2] <- "Year"
FingerMillet_Rice_complete_soil2 <- merge(FingerMillet_Rice_complete, Soil_data3_long_2, by = "Dist.Code")
colnames(FingerMillet_Rice_complete_soil2)[2] <- "Year"
FingerMillet_Rice_complete_soil3 <- merge(FingerMillet_Rice_complete, Soil_data3_long_3, by = "Dist.Code")
colnames(FingerMillet_Rice_complete_soil3)[2] <- "Year"

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

m1.1 <- lmer(Rice ~ poly(total_actual_rain, 2) + poly(mean_actual_tmax, 2) + SoilType + (1|Dist.Code) + (1|Year) - 1, data = Maize_Rice_complete_soil1)
summary(m1.1)
stdCoef.merMod(m1.1)
r2_nakagawa(m1.1)

m1.2 <- lmer(Maize ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=Maize_Rice_complete_soil1)
summary(m1.2)
stdCoef.merMod(m1.2)
r2_nakagawa(m1.2)

m2.1 <- lmer(Rice ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=Sorghum_Rice_complete_soil3)
summary(m2.1)
stdCoef.merMod(m2.1)
r2_nakagawa(m2.1)

m2.2 <- lmer(Sorghum ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=Sorghum_Rice_complete_soil3)
summary(m2.2)
stdCoef.merMod(m2.2)
r2_nakagawa(m2.2)

m3.1 <- lmer(Rice ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=PearlMillet_Rice_complete_soil3)
summary(m3.1)
stdCoef.merMod(m3.1)
r2_nakagawa(m3.1)

m3.2 <- lmer(PearlMillet ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=PearlMillet_Rice_complete_soil3)
summary(m3.2)
stdCoef.merMod(m3.2)
r2_nakagawa(m3.2)

m4.1 <- lmer(Rice ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=FingerMillet_Rice_complete_soil3)
summary(m4.1)
stdCoef.merMod(m4.1)
r2_nakagawa(m4.1)

m4.2 <- lmer(FingerMillet ~ poly(total_actual_rain,2) + poly(mean_actual_tmax,2) + SoilType + (1|Dist.Code) + (1|Year) -1, data=FingerMillet_Rice_complete_soil3)
summary(m4.2)
stdCoef.merMod(m4.2)
r2_nakagawa(m4.2)

tab_model(m1.1, m1.2, m2.1, m2.2)
tab_model(m3.1, m3.2, m4.1, m4.2)
