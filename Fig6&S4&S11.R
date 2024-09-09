# Fig6&S4&S11

# S4 Metric Anomalies across 5 cereals for Irrigated and Rainfed districts during ENSO and IOD
# 6 & S11 Comparison of the sensitivity of alternative grains to climate variability relative to rice

phase <- melt(area_nino34_iod[,c(1,28:29)], id=c("Year", "nino34phase", "iodphase"))
phase$phase1 <- ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="Neutral", "El Nino", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD+", "IOD+", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="Neutral", "La Nina", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD-", "IOD-", ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="IOD+", "El Nino", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="IOD-", "La Nina", ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="IOD-", "El Nino", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="IOD+", "La Nina", NA))))))))
phase$phase2 <- ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="Neutral", "El Nino", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD+", "IOD+", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="Neutral", "La Nina", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD-", "IOD-", ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="IOD+", "IOD+", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="IOD-", "IOD-", ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="IOD-", "IOD-", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="IOD+", "IOD+", NA))))))))
phase$phase <- ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="Neutral", "El Nino", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD+", "IOD+", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="Neutral", "La Nina", ifelse(phase$nino34phase=="Neutral" & phase$iodphase=="IOD-", "IOD-", ifelse(phase$nino34phase=="El Nino" & phase$iodphase=="IOD+", "ElNino&IOD+", ifelse(phase$nino34phase=="La Nina" & phase$iodphase=="IOD-", "LaNina&IOD-", NA))))))
nino34_iod_phase <- merge(nino34_iod, phase, by="Year")

# Sensitivity of absolute and relative yield anomalies to irrigated vs rainfed districts and within irrigated/rainfed districts, sensitivity of absolute and relative yield anomalies to ENSO and IOD

Yield1 <- Yield[,c(-91)]
Harv_Irr1 <- Harv_Irr[,-c(7,10,13,16,19,89)]
Harv_Irr_Yield <- merge(Yield1,Harv_Irr1,by=c('Year','State.Code', 'Dist.Code', 'State.Name','Dist.Name','ID'))
Harv_Irr_Yield$prop_irr_har <- ifelse(Harv_Irr_Yield$Overall_area != 0, Harv_Irr_Yield$IrrArea_total / Harv_Irr_Yield$Overall_area, NA)

Harv_Irr_Yield <- Harv_Irr_Yield %>%
  mutate(RiceIrrigation_Type = case_when(
    RIprop_irr_har >= 0.5 ~ "Irrigated_Rice",
    RIprop_irr_har < 0.5 ~ "Rainfed_Rice",
    TRUE ~ "NA"),
    MaizeIrrigation_Type = case_when(
      MZprop_irr_har >= 0.5 ~ "Irrigated_Maize",
      MZprop_irr_har < 0.5 ~ "Rainfed_Maize",
      TRUE ~ "NA"),
    SorghumIrrigation_Type = case_when(
      SGprop_irr_har >= 0.5 ~ "Irrigated_Sorghum",
      SGprop_irr_har < 0.5 ~ "Rainfed_Sorghum",
      TRUE ~ "NA"),
    PearlMilletIrrigation_Type = case_when(
      PMprop_irr_har >= 0.5 ~ "Irrigated_PearlMillet",
      PMprop_irr_har < 0.5 ~ "Rainfed_PearlMillet",
      TRUE ~ "NA"),
    FingerMilletIrrigation_Type = case_when(
      FMprop_irr_har >= 0.5 ~ "Irrigated_FingerMillet",
      FMprop_irr_har < 0.5 ~ "Rainfed_FingerMillet",
      TRUE ~ "NA"),
    YieldIrrigation_Type = case_when(
      prop_irr_har >= 0.5 ~ "Irrigated_Yield",
      prop_irr_har < 0.5 ~ "Rainfed_Yield",
      TRUE ~ "NA"))

Rice_Dist <- Harv_Irr_Yield %>%
  filter(RiceIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, RICE.YIELD..Kg.per.ha., RiceAnom, RicePerAnom, wRiceIrrAnom, wRiceIrrPerAnom, RICE.AREA..1000.ha., RICE.IRRIGATED.AREA..1000.ha., RIprop_irr_har, RI_tot_har_area, RI_tot_irr_area, WEIGHTED.RICE.YIELD..Kg.per.ha., RiceIrrigation_Type) %>%
  mutate(wRiceAnom = RiceAnom * RICE.AREA..1000.ha.,
         wRicePerAnom = RicePerAnom * RICE.AREA..1000.ha.)

Maize_Dist <- Harv_Irr_Yield %>%
  filter(MaizeIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, MAIZE.YIELD..Kg.per.ha., MaizeAnom, MaizePerAnom, wMaizeIrrAnom, wMaizeIrrPerAnom, MAIZE.AREA..1000.ha., MAIZE.IRRIGATED.AREA..1000.ha., MZprop_irr_har, MZ_tot_har_area, MZ_tot_irr_area, WEIGHTED.MAIZE.YIELD..Kg.per.ha., MaizeIrrigation_Type) %>%
  mutate(wMaizeAnom = MaizeAnom * MAIZE.AREA..1000.ha.,
         wMaizePerAnom = MaizePerAnom * MAIZE.AREA..1000.ha.)

Sorghum_Dist <- Harv_Irr_Yield %>%
  filter(SorghumIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, KHARIF.SORGHUM.YIELD..Kg.per.ha., SorghumKharifAnom, SorghumPerAnom, wSorghumIrrAnom, wSorghumIrrPerAnom, KHARIF.SORGHUM.AREA..1000.ha., KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., SGprop_irr_har, SG_tot_har_area, SG_tot_irr_area, WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha., SorghumIrrigation_Type) %>%
  mutate(wSorghumAnom = SorghumKharifAnom * KHARIF.SORGHUM.AREA..1000.ha.,
         wSorghumPerAnom = SorghumPerAnom * KHARIF.SORGHUM.AREA..1000.ha.)

PearlMillet_Dist <- Harv_Irr_Yield %>%
  filter(PearlMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, PEARL.MILLET.YIELD..Kg.per.ha., PearlMilletAnom, PearlMilletPerAnom, wPearlMilletIrrAnom, wPearlMilletIrrPerAnom, PEARL.MILLET.AREA..1000.ha., PEARL.MILLET.IRRIGATED.AREA..1000.ha., PMprop_irr_har, PM_tot_har_area, PM_tot_irr_area, WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha., PearlMilletIrrigation_Type) %>%
  mutate(wPearlMilletAnom = PearlMilletAnom * PEARL.MILLET.AREA..1000.ha.,
         wPearlMilletPerAnom = PearlMilletPerAnom * PEARL.MILLET.AREA..1000.ha.)

FingerMillet_Dist <- Harv_Irr_Yield %>%
  filter(FingerMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, FINGER.MILLET.YIELD..Kg.per.ha., FingerMilletAnom, FingerMilletPerAnom, wFingerMilletIrrAnom, wFingerMilletIrrPerAnom, FINGER.MILLET.AREA..1000.ha., FINGER.MILLET.IRRIGATED.AREA..1000.ha., FMprop_irr_har, FM_tot_har_area, FM_tot_irr_area, WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha., FingerMilletIrrigation_Type) %>%
  mutate(wFingerMilletAnom = FingerMilletAnom * FINGER.MILLET.AREA..1000.ha.,
         wFingerMilletPerAnom = FingerMilletPerAnom * FINGER.MILLET.AREA..1000.ha.)

Yield_Dist <- Harv_Irr_Yield %>%
  filter(YieldIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, Yield_avg, YieldAnom, YieldPerAnom, wIrrAnom, wIrrPerAnom, Overall_area, IrrArea_total, prop_irr_har, tot_har_area, tot_irr_area, WEIGHTED.YIELD..Kg.per.ha., YieldIrrigation_Type) %>%
  mutate(wYieldAnom = YieldAnom * Overall_area,
         wYieldPerAnom = YieldPerAnom * Overall_area)

Rice_dist_summary <- Rice_Dist %>%
  group_by(Year, RiceIrrigation_Type) %>%
  summarize(Average_RICE.YIELD = mean(RICE.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_RICE_YIELD_Kg.per.ha = sum(WEIGHTED.RICE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(RI_tot_har_area), wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = RiceIrrigation_Type, Average_yield = Average_RICE.YIELD, Weighted_yield = Total_WEIGHTED_RICE_YIELD_Kg.per.ha, wYieldAnom = wRiceAnom, wYieldPerAnom = wRicePerAnom, wIrrAnom = wRiceIrrAnom, wIrrPerAnom = wRiceIrrPerAnom) %>%
  mutate(crop = "Rice", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Rice", "Irrigated", "Rainfed"))

Maize_dist_summary <- Maize_Dist %>%
  group_by(Year, MaizeIrrigation_Type) %>%
  summarize(Average_MAIZE.YIELD = mean(MAIZE.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_MAIZE_YIELD_Kg.per.ha = sum(WEIGHTED.MAIZE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(MZ_tot_har_area), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizeIrrAnom = sum(wMaizeIrrAnom, na.rm=TRUE)/mean(MZ_tot_irr_area), wMaizeIrrPerAnom = sum(wMaizeIrrPerAnom, na.rm=TRUE)/mean(MZ_tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = MaizeIrrigation_Type, Average_yield = Average_MAIZE.YIELD, Weighted_yield = Total_WEIGHTED_MAIZE_YIELD_Kg.per.ha, wYieldAnom = wMaizeAnom, wYieldPerAnom = wMaizePerAnom, wIrrAnom = wMaizeIrrAnom, wIrrPerAnom = wMaizeIrrPerAnom) %>%
  mutate(crop = "Maize", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Maize", "Irrigated", "Rainfed"))

Sorghum_dist_summary <- Sorghum_Dist %>%
  group_by(Year, SorghumIrrigation_Type) %>%
  summarize(Average_SORGHUM.YIELD = mean(KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_SORGHUM_YIELD_Kg.per.ha = sum(WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm=TRUE)/mean(SG_tot_har_area), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumIrrAnom = sum(wSorghumIrrAnom, na.rm=TRUE)/mean(SG_tot_irr_area), wSorghumIrrPerAnom = sum(wSorghumIrrPerAnom, na.rm=TRUE)/mean(SG_tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = SorghumIrrigation_Type, Average_yield = Average_SORGHUM.YIELD, Weighted_yield = Total_WEIGHTED_SORGHUM_YIELD_Kg.per.ha, wYieldAnom = wSorghumAnom, wYieldPerAnom = wSorghumPerAnom, wIrrAnom = wSorghumIrrAnom, wIrrPerAnom = wSorghumIrrPerAnom) %>%
  mutate(crop = "Sorghum", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Sorghum", "Irrigated", "Rainfed"))

PearlMillet_dist_summary <- PearlMillet_Dist %>%
  group_by(Year, PearlMilletIrrigation_Type) %>%
  summarize(Average_PEARLMILLET.YIELD = mean(PEARL.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_PEARLMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletIrrAnom = sum(wPearlMilletIrrAnom, na.rm=TRUE)/mean(PM_tot_irr_area), wPearlMilletIrrPerAnom = sum(wPearlMilletIrrPerAnom, na.rm=TRUE)/mean(PM_tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = PearlMilletIrrigation_Type, Average_yield = Average_PEARLMILLET.YIELD, Weighted_yield = Total_WEIGHTED_PEARLMILLET_YIELD_Kg.per.ha, wYieldAnom = wPearlMilletAnom, wYieldPerAnom = wPearlMilletPerAnom, wIrrAnom = wPearlMilletIrrAnom, wIrrPerAnom = wPearlMilletIrrPerAnom) %>%
  mutate(crop = "PearlMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_PearlMillet", "Irrigated", "Rainfed"))

FingerMillet_dist_summary <- FingerMillet_Dist %>%
  group_by(Year, FingerMilletIrrigation_Type) %>%
  summarize(Average_FINGERMILLET.YIELD = mean(FINGER.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), Total_WEIGHTED_FINGERMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletIrrAnom = sum(wFingerMilletIrrAnom, na.rm=TRUE)/mean(FM_tot_irr_area), wFingerMilletIrrPerAnom = sum(wFingerMilletIrrPerAnom, na.rm=TRUE)/mean(FM_tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = FingerMilletIrrigation_Type, Average_yield = Average_FINGERMILLET.YIELD, Weighted_yield = Total_WEIGHTED_FINGERMILLET_YIELD_Kg.per.ha, wYieldAnom = wFingerMilletAnom, wYieldPerAnom = wFingerMilletPerAnom, wIrrAnom = wFingerMilletIrrAnom, wIrrPerAnom = wFingerMilletIrrPerAnom) %>%
  mutate(crop = "FingerMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_FingerMillet", "Irrigated", "Rainfed"))

dist_summary <- Yield_Dist %>%
  group_by(Year, YieldIrrigation_Type) %>%
  summarize(Average_YIELD = mean(Yield_avg, na.rm = TRUE), Total_WEIGHTED_YIELD_Kg.per.ha = sum(WEIGHTED.YIELD..Kg.per.ha., na.rm=TRUE)/mean(tot_har_area), wYieldAnom = sum(wYieldAnom, na.rm=TRUE)/mean(tot_har_area), wYieldPerAnom = sum(wYieldPerAnom, na.rm=TRUE)/mean(tot_har_area), wIrrAnom = sum(wIrrAnom, na.rm=TRUE)/mean(tot_irr_area), wIrrPerAnom = sum(wIrrPerAnom, na.rm=TRUE)/mean(tot_irr_area), .groups = "drop") %>%
  rename(Irrigation_Type = YieldIrrigation_Type, Average_yield = Average_YIELD, Weighted_yield = Total_WEIGHTED_YIELD_Kg.per.ha, wYieldAnom = wYieldAnom, wYieldPerAnom = wYieldPerAnom, wIrrAnom = wIrrAnom, wIrrPerAnom = wIrrPerAnom) %>%
  mutate(crop = "All", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Yield", "Irrigated", "Rainfed"))

Yields_Irr_Rain <- rbind(Rice_dist_summary[5:100,], Maize_dist_summary[5:100,], Sorghum_dist_summary[5:100,], PearlMillet_dist_summary[5:100,], FingerMillet_dist_summary[5:100,], dist_summary[5:100,])
Yields_Irr_Rain$crop <- factor(Yields_Irr_Rain$crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet", "All"))
Yields_Irr_Rain_phase <- merge(Yields_Irr_Rain, nino34_iod_phase, by="Year")

yields_irr_phase <- subset(Yields_Irr_Rain_phase, Irrigation_Type=="Irrigated")
yields_rain_phase <- subset(Yields_Irr_Rain_phase, Irrigation_Type=="Rainfed")
yields_irr_nphase <- subset(yields_irr_phase, nino34phase != "Neutral")
yields_rain_nphase <- subset(yields_rain_phase, nino34phase != "Neutral")
yields_irr_iphase <- subset(yields_irr_phase, iodphase != "Neutral")
yields_rain_iphase <- subset(yields_rain_phase, iodphase != "Neutral")

Yields_Irr_Rain <- subset(Yields_Irr_Rain, crop != "All")

# Absolute Anomalies

y_ir <- ggplot()+
  geom_boxplot(data = Yields_Irr_Rain, aes(x=crop, y=wYieldAnom, fill=Irrigation_Type), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yield anomalies")+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-350,350))

yields_irr_nphase <- subset(yields_irr_nphase, crop != "All")

y_ir <- ggplot()+
  geom_boxplot(data = yields_irr_nphase, aes(x=crop, y=wYieldAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yieldanom across yrs for all irr dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-350,350))

yields_rain_nphase <- subset(yields_rain_nphase, crop != "All")

y_ir <- ggplot()+
  geom_boxplot(data = yields_rain_nphase, aes(x=crop, y=wYieldAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yieldanom across yrs for all rain dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-350,350))

yields_irr_iphase <- subset(yields_irr_iphase, crop != "All")

y_ir <- ggplot()+
  geom_boxplot(data = yields_irr_iphase, aes(x=crop, y=wYieldAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yieldanom across yrs for all irr dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-350,350))

yields_rain_iphase <- subset(yields_rain_iphase, crop != "All")

y_ir <- ggplot()+
  geom_boxplot(data = yields_rain_iphase, aes(x=crop, y=wYieldAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yieldanom across yrs for all rain dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-350,350))

# Relative Anomalies

y_ir <- ggplot()+
  geom_boxplot(data = Yields_Irr_Rain, aes(x=crop, y=wYieldPerAnom, fill=Irrigation_Type), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yield anomalies")+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-100, 100))

y_ir <- ggplot()+
  geom_boxplot(data = yields_irr_nphase, aes(x=crop, y=wYieldPerAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yieldanom across yrs for all irr dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

y_ir <- ggplot()+
  geom_boxplot(data = yields_rain_nphase, aes(x=crop, y=wYieldPerAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yieldanom across yrs for all rain dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

y_ir <- ggplot()+
  geom_boxplot(data = yields_irr_iphase, aes(x=crop, y=wYieldPerAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yieldanom across yrs for all irr dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

y_ir <- ggplot()+
  geom_boxplot(data = yields_rain_iphase, aes(x=crop, y=wYieldPerAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yieldanom across yrs for all rain dist")+
  theme_classic()+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=45))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Sensitivity of absolute and relative yield anomalies to rainfed districts, sensitivity of absolute and relative yield anomalies to ENSO and IOD - overlapping districts

# Absolute Yield Anomalies

Harv_Irr_Yield$RI.RI <- ifelse(Harv_Irr_Yield$RICE.YIELD..Kg.per.ha. %in% c(-1, NA), NA, 1)
Harv_Irr_Yield_RI <- subset(Harv_Irr_Yield, RI.RI==1)

Harv_Irr_Yield_RI$RI.MZ <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$MAIZE.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI1 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'wMaizeIrrAnom', 'wMaizeIrrPerAnom', 'MZ_tot_har_area', 'MZ_tot_irr_area', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Yield_RI.MZ <- subset(Harv_Irr_Yield_RI1, RI.MZ==1)
Harv_Irr_Yield_RI.MZ1 <- subset(Harv_Irr_Yield_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Yield_RI.MZ2 <- Harv_Irr_Yield_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizeIrrAnom = sum(wMaizeIrrAnom, na.rm=TRUE)/mean(MZ_tot_irr_area), wMaizeIrrPerAnom = sum(wMaizeIrrPerAnom, na.rm=TRUE)/mean(MZ_tot_irr_area))
Harv_Irr_Yield_RI.MZ3 <- merge(Harv_Irr_Yield_RI.MZ2, nino34_iod_phase, by="Year")

yieldanom_avg <- pivot_longer(Harv_Irr_Yield_RI.MZ3, c(2,6), names_to = "Crops", values_to = "YieldAnom")
yieldanom_avg$Crop <- ifelse(yieldanom_avg$Crops=="wRiceAnom", "Rice", "Maize")
yieldanom_avg$Crops <- NULL
yieldanom_avg$Crop <- factor(yieldanom_avg$Crop, levels = c("Rice", "Maize"))
yieldanom_avg$phase <- factor(yieldanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldanom_avg$phase1 <- factor(yieldanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldanom_avg$phase2 <- factor(yieldanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Yield_RI$RI.SGK <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI2 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom', 'wSorghumIrrAnom', 'wSorghumIrrPerAnom', 'SG_tot_har_area', 'SG_tot_irr_area', 'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Yield_RI.SGK <- subset(Harv_Irr_Yield_RI2, RI.SGK==1)
Harv_Irr_Yield_RI.SGK1 <- subset(Harv_Irr_Yield_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Yield_RI.SGK2 <- Harv_Irr_Yield_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumIrrAnom = sum(wSorghumIrrAnom, na.rm=TRUE)/mean(SG_tot_irr_area), wSorghumIrrPerAnom = sum(wSorghumIrrPerAnom, na.rm=TRUE)/mean(SG_tot_irr_area))
Harv_Irr_Yield_RI.SGK3 <- merge(Harv_Irr_Yield_RI.SGK2, nino34_iod_phase, by="Year")

yieldanom_avg <- pivot_longer(Harv_Irr_Yield_RI.SGK3, c(2,6), names_to = "Crops", values_to = "YieldAnom")
yieldanom_avg$Crop <- ifelse(yieldanom_avg$Crops=="wRiceAnom", "Rice", "Sorghum")
yieldanom_avg$Crops <- NULL
yieldanom_avg$Crop <- factor(yieldanom_avg$Crop, levels = c("Rice", "Sorghum"))
yieldanom_avg$phase <- factor(yieldanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldanom_avg$phase1 <- factor(yieldanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldanom_avg$phase2 <- factor(yieldanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Yield_RI$RI.PM <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$PEARL.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI3 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'wPearlMilletIrrAnom', 'wPearlMilletIrrPerAnom', 'PM_tot_har_area', 'PM_tot_irr_area', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Yield_RI.PM <- subset(Harv_Irr_Yield_RI3, RI.PM==1)
Harv_Irr_Yield_RI.PM1 <- subset(Harv_Irr_Yield_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Yield_RI.PM2 <- Harv_Irr_Yield_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletIrrAnom = sum(wPearlMilletIrrAnom, na.rm=TRUE)/mean(PM_tot_irr_area), wPearlMilletIrrPerAnom = sum(wPearlMilletIrrPerAnom, na.rm=TRUE)/mean(PM_tot_irr_area))
Harv_Irr_Yield_RI.PM3 <- merge(Harv_Irr_Yield_RI.PM2, nino34_iod_phase, by="Year")

yieldanom_avg <- pivot_longer(Harv_Irr_Yield_RI.PM3, c(2,6), names_to = "Crops", values_to = "YieldAnom")
yieldanom_avg$Crop <- ifelse(yieldanom_avg$Crops=="wRiceAnom", "Rice", "PearlMillet")
yieldanom_avg$Crops <- NULL
yieldanom_avg$Crop <- factor(yieldanom_avg$Crop, levels = c("Rice", "PearlMillet"))
yieldanom_avg$phase <- factor(yieldanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldanom_avg$phase1 <- factor(yieldanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Yield_RI$RI.FM <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$FINGER.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI4 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'wFingerMilletIrrAnom', 'wFingerMilletIrrPerAnom', 'FM_tot_har_area', 'FM_tot_irr_area', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Yield_RI.FM <- subset(Harv_Irr_Yield_RI4, RI.FM==1)
Harv_Irr_Yield_RI.FM1 <- subset(Harv_Irr_Yield_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Yield_RI.FM2 <- Harv_Irr_Yield_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletIrrAnom = sum(wFingerMilletIrrAnom, na.rm=TRUE)/mean(FM_tot_irr_area), wFingerMilletIrrPerAnom = sum(wFingerMilletIrrPerAnom, na.rm=TRUE)/mean(FM_tot_irr_area))
Harv_Irr_Yield_RI.FM3 <- merge(Harv_Irr_Yield_RI.FM2, nino34_iod_phase, by="Year")

yieldanom_avg <- pivot_longer(Harv_Irr_Yield_RI.FM3, c(2,6), names_to = "Crops", values_to = "YieldAnom")
yieldanom_avg$Crop <- ifelse(yieldanom_avg$Crops=="wRiceAnom", "Rice", "FingerMillet")
yieldanom_avg$Crops <- NULL
yieldanom_avg$Crop <- factor(yieldanom_avg$Crop, levels = c("Rice", "FingerMillet"))
yieldanom_avg$phase <- factor(yieldanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldanom_avg$phase1 <- factor(yieldanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldanom_avg$phase2 <- factor(yieldanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$YieldAnom, Maize_E$YieldAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$YieldAnom, Sorghum_E$YieldAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$YieldAnom, PearlMillet_E$YieldAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$YieldAnom, FingerMillet_E$YieldAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$YieldAnom, Maize_L$YieldAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$YieldAnom, Sorghum_L$YieldAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$YieldAnom, PearlMillet_L$YieldAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$YieldAnom, FingerMillet_L$YieldAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$YieldAnom, Maize_P$YieldAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$YieldAnom, Sorghum_P$YieldAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$YieldAnom, PearlMillet_P$YieldAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$YieldAnom, FingerMillet_P$YieldAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$YieldAnom, Maize_N$YieldAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$YieldAnom, Sorghum_N$YieldAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$YieldAnom, PearlMillet_N$YieldAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$YieldAnom, FingerMillet_N$YieldAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$YieldAnom, Maize_E$YieldAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$YieldAnom, Sorghum_E$YieldAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$YieldAnom, PearlMillet_E$YieldAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$YieldAnom, FingerMillet_E$YieldAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$YieldAnom, Maize_L$YieldAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$YieldAnom, Sorghum_L$YieldAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$YieldAnom, PearlMillet_L$YieldAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$YieldAnom, FingerMillet_L$YieldAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$YieldAnom, Maize_P$YieldAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$YieldAnom, Sorghum_P$YieldAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$YieldAnom, PearlMillet_P$YieldAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$YieldAnom, FingerMillet_P$YieldAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$YieldAnom, Maize_N$YieldAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$YieldAnom, Sorghum_N$YieldAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$YieldAnom, PearlMillet_N$YieldAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$YieldAnom, FingerMillet_N$YieldAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations

Overlapping_yield <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=YieldAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("Yield Anomalies (kg/ha)")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=YieldAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-150,150))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")

# Relative Yield Anomalies

Harv_Irr_Yield$RI.RI <- ifelse(Harv_Irr_Yield$RICE.YIELD..Kg.per.ha. %in% c(-1, NA), NA, 1)
Harv_Irr_Yield_RI <- subset(Harv_Irr_Yield, RI.RI==1)

Harv_Irr_Yield_RI$RI.MZ <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$MAIZE.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI1 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'wMaizeIrrAnom', 'wMaizeIrrPerAnom', 'MZ_tot_har_area', 'MZ_tot_irr_area', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Yield_RI.MZ <- subset(Harv_Irr_Yield_RI1, RI.MZ==1)
Harv_Irr_Yield_RI.MZ1 <- subset(Harv_Irr_Yield_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Yield_RI.MZ2 <- Harv_Irr_Yield_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizeIrrAnom = sum(wMaizeIrrAnom, na.rm=TRUE)/mean(MZ_tot_irr_area), wMaizeIrrPerAnom = sum(wMaizeIrrPerAnom, na.rm=TRUE)/mean(MZ_tot_irr_area))
Harv_Irr_Yield_RI.MZ3 <- merge(Harv_Irr_Yield_RI.MZ2, nino34_iod_phase, by="Year")

yieldperanom_avg <- pivot_longer(Harv_Irr_Yield_RI.MZ3, c(3,7), names_to = "Crops", values_to = "YieldPerAnom")
yieldperanom_avg$Crop <- ifelse(yieldperanom_avg$Crops=="wRicePerAnom", "Rice", "Maize")
yieldperanom_avg$Crops <- NULL
yieldperanom_avg$Crop <- factor(yieldperanom_avg$Crop, levels = c("Rice", "Maize"))
yieldperanom_avg$phase <- factor(yieldperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldperanom_avg$phase1 <- factor(yieldperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldperanom_avg$phase2 <- factor(yieldperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldperanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldperanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldperanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Yield_RI$RI.SGK <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI2 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom', 'wSorghumIrrAnom', 'wSorghumIrrPerAnom', 'SG_tot_har_area', 'SG_tot_irr_area', 'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Yield_RI.SGK <- subset(Harv_Irr_Yield_RI2, RI.SGK==1)
Harv_Irr_Yield_RI.SGK1 <- subset(Harv_Irr_Yield_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Yield_RI.SGK2 <- Harv_Irr_Yield_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumIrrAnom = sum(wSorghumIrrAnom, na.rm=TRUE)/mean(SG_tot_irr_area), wSorghumIrrPerAnom = sum(wSorghumIrrPerAnom, na.rm=TRUE)/mean(SG_tot_irr_area))
Harv_Irr_Yield_RI.SGK3 <- merge(Harv_Irr_Yield_RI.SGK2, nino34_iod_phase, by="Year")

yieldperanom_avg <- pivot_longer(Harv_Irr_Yield_RI.SGK3, c(3,7), names_to = "Crops", values_to = "YieldPerAnom")
yieldperanom_avg$Crop <- ifelse(yieldperanom_avg$Crops=="wRicePerAnom", "Rice", "Sorghum")
yieldperanom_avg$Crops <- NULL
yieldperanom_avg$Crop <- factor(yieldperanom_avg$Crop, levels = c("Rice", "Sorghum"))
yieldperanom_avg$phase <- factor(yieldperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldperanom_avg$phase1 <- factor(yieldperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldperanom_avg$phase2 <- factor(yieldperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldperanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldperanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldperanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Yield_RI$RI.PM <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$PEARL.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI3 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'wPearlMilletIrrAnom', 'wPearlMilletIrrPerAnom', 'PM_tot_har_area', 'PM_tot_irr_area', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Yield_RI.PM <- subset(Harv_Irr_Yield_RI3, RI.PM==1)
Harv_Irr_Yield_RI.PM1 <- subset(Harv_Irr_Yield_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Yield_RI.PM2 <- Harv_Irr_Yield_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletIrrAnom = sum(wPearlMilletIrrAnom, na.rm=TRUE)/mean(PM_tot_irr_area), wPearlMilletIrrPerAnom = sum(wPearlMilletIrrPerAnom, na.rm=TRUE)/mean(PM_tot_irr_area))
Harv_Irr_Yield_RI.PM3 <- merge(Harv_Irr_Yield_RI.PM2, nino34_iod_phase, by="Year")

yieldperanom_avg <- pivot_longer(Harv_Irr_Yield_RI.PM3, c(3,7), names_to = "Crops", values_to = "YieldPerAnom")
yieldperanom_avg$Crop <- ifelse(yieldperanom_avg$Crops=="wRicePerAnom", "Rice", "PearlMillet")
yieldperanom_avg$Crops <- NULL
yieldperanom_avg$Crop <- factor(yieldperanom_avg$Crop, levels = c("Rice", "PearlMillet"))
yieldperanom_avg$phase <- factor(yieldperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldperanom_avg$phase1 <- factor(yieldperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldperanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldperanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldperanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Yield_RI$RI.FM <- ifelse((Harv_Irr_Yield_RI$RICE.YIELD..Kg.per.ha. %in% c(-1, NA)) | (Harv_Irr_Yield_RI$FINGER.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Yield_RI4 <- Harv_Irr_Yield_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'wRiceIrrAnom', 'wRiceIrrPerAnom', 'RI_tot_har_area', 'RI_tot_irr_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'wFingerMilletIrrAnom', 'wFingerMilletIrrPerAnom', 'FM_tot_har_area', 'FM_tot_irr_area', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Yield_RI.FM <- subset(Harv_Irr_Yield_RI4, RI.FM==1)
Harv_Irr_Yield_RI.FM1 <- subset(Harv_Irr_Yield_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Yield_RI.FM2 <- Harv_Irr_Yield_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletIrrAnom = sum(wFingerMilletIrrAnom, na.rm=TRUE)/mean(FM_tot_irr_area), wFingerMilletIrrPerAnom = sum(wFingerMilletIrrPerAnom, na.rm=TRUE)/mean(FM_tot_irr_area))
Harv_Irr_Yield_RI.FM3 <- merge(Harv_Irr_Yield_RI.FM2, nino34_iod_phase, by="Year")

yieldperanom_avg <- pivot_longer(Harv_Irr_Yield_RI.FM3, c(3,7), names_to = "Crops", values_to = "YieldPerAnom")
yieldperanom_avg$Crop <- ifelse(yieldperanom_avg$Crops=="wRicePerAnom", "Rice", "FingerMillet")
yieldperanom_avg$Crops <- NULL
yieldperanom_avg$Crop <- factor(yieldperanom_avg$Crop, levels = c("Rice", "FingerMillet"))
yieldperanom_avg$phase <- factor(yieldperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldperanom_avg$phase1 <- factor(yieldperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldperanom_avg$phase2 <- factor(yieldperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- yieldperanom_avg[,c(1,8:12,15:16)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- yieldperanom_avg[,c(1,8:11,13,15:16)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- yieldperanom_avg[,c(1,8:11,14:16)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$YieldPerAnom, Maize_E$YieldPerAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$YieldPerAnom, Sorghum_E$YieldPerAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$YieldPerAnom, PearlMillet_E$YieldPerAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$YieldPerAnom, FingerMillet_E$YieldPerAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$YieldPerAnom, Maize_L$YieldPerAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$YieldPerAnom, Sorghum_L$YieldPerAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$YieldPerAnom, PearlMillet_L$YieldPerAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$YieldPerAnom, FingerMillet_L$YieldPerAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$YieldPerAnom, Maize_P$YieldPerAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$YieldPerAnom, Sorghum_P$YieldPerAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$YieldPerAnom, PearlMillet_P$YieldPerAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$YieldPerAnom, FingerMillet_P$YieldPerAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$YieldPerAnom, Maize_N$YieldPerAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$YieldPerAnom, Sorghum_N$YieldPerAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$YieldPerAnom, PearlMillet_N$YieldPerAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$YieldPerAnom, FingerMillet_N$YieldPerAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$YieldPerAnom, Maize_E$YieldPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$YieldPerAnom, Sorghum_E$YieldPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$YieldPerAnom, PearlMillet_E$YieldPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$YieldPerAnom, FingerMillet_E$YieldPerAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$YieldPerAnom, Maize_L$YieldPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$YieldPerAnom, Sorghum_L$YieldPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$YieldPerAnom, PearlMillet_L$YieldPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$YieldPerAnom, FingerMillet_L$YieldPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$YieldPerAnom, Maize_P$YieldPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$YieldPerAnom, Sorghum_P$YieldPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$YieldPerAnom, PearlMillet_P$YieldPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$YieldPerAnom, FingerMillet_P$YieldPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$YieldPerAnom, Maize_N$YieldPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$YieldPerAnom, Sorghum_N$YieldPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$YieldPerAnom, PearlMillet_N$YieldPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$YieldPerAnom, FingerMillet_N$YieldPerAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations

Overlapping_yield <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=YieldPerAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("%Yield Anomalies")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=YieldPerAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-15,15))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")

# Sensitivity of absolute and relative production anomalies to irrigated vs rainfed districts and within irrigated/rainfed districts, sensitivity of absolute and relative production anomalies to ENSO and IOD

Prod1 <- Prod[,c(-79)]
Harv_Irr_Yield1 <- Harv_Irr_Yield[,-c(7:60, 79:90, 169:175)]
Harv_Irr_Prod <- merge(Prod1,Harv_Irr_Yield1,by=c('Year','State.Code', 'Dist.Code', 'State.Name','Dist.Name','ID'))
Harv_Irr_Prod$prop_irr_har <- ifelse(Harv_Irr_Prod$Overall_area != 0, Harv_Irr_Prod$IrrArea_total / Harv_Irr_Prod$Overall_area, NA)

Harv_Irr_Prod <- Harv_Irr_Prod %>%
  mutate(RiceIrrigation_Type = case_when(
    RIprop_irr_har >= 0.5 ~ "Irrigated_Rice",
    RIprop_irr_har < 0.5 ~ "Rainfed_Rice",
    TRUE ~ "NA"),
    MaizeIrrigation_Type = case_when(
      MZprop_irr_har >= 0.5 ~ "Irrigated_Maize",
      MZprop_irr_har < 0.5 ~ "Rainfed_Maize",
      TRUE ~ "NA"),
    SorghumIrrigation_Type = case_when(
      SGprop_irr_har >= 0.5 ~ "Irrigated_Sorghum",
      SGprop_irr_har < 0.5 ~ "Rainfed_Sorghum",
      TRUE ~ "NA"),
    PearlMilletIrrigation_Type = case_when(
      PMprop_irr_har >= 0.5 ~ "Irrigated_PearlMillet",
      PMprop_irr_har < 0.5 ~ "Rainfed_PearlMillet",
      TRUE ~ "NA"),
    FingerMilletIrrigation_Type = case_when(
      FMprop_irr_har >= 0.5 ~ "Irrigated_FingerMillet",
      FMprop_irr_har < 0.5 ~ "Rainfed_FingerMillet",
      TRUE ~ "NA"),
    ProdIrrigation_Type = case_when(
      prop_irr_har >= 0.5 ~ "Irrigated_Prod",
      prop_irr_har < 0.5 ~ "Rainfed_Prod",
      TRUE ~ "NA"))

Rice_Dist <- Harv_Irr_Prod %>%
  filter(RiceIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, RICE.PRODUCTION..1000.tons., RiceAnom, RicePerAnom, RIprop_irr_har, RI_tot_prod, wRiceAnom, wRicePerAnom, RiceIrrigation_Type) 

Maize_Dist <- Harv_Irr_Prod %>%
  filter(MaizeIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, MAIZE.PRODUCTION..1000.tons., MaizeAnom, MaizePerAnom, MZprop_irr_har, MZ_tot_prod, wMaizeAnom, wMaizePerAnom, MaizeIrrigation_Type) 

Sorghum_Dist <- Harv_Irr_Prod %>%
  filter(SorghumIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, KHARIF.SORGHUM.PRODUCTION..1000.tons., SorghumKharifAnom, SorghumPerAnom, SGprop_irr_har, SG_tot_prod, wSorghumAnom, wSorghumPerAnom, SorghumIrrigation_Type)

PearlMillet_Dist <- Harv_Irr_Prod %>%
  filter(PearlMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, PEARL.MILLET.PRODUCTION..1000.tons., PearlMilletAnom, PearlMilletPerAnom, PMprop_irr_har, PM_tot_prod, wPearlMilletAnom, wPearlMilletPerAnom, PearlMilletIrrigation_Type)

FingerMillet_Dist <- Harv_Irr_Prod %>%
  filter(FingerMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, FINGER.MILLET.PRODUCTION..1000.tons., FingerMilletAnom, FingerMilletPerAnom, FMprop_irr_har, FM_tot_prod, wFingerMilletAnom, wFingerMilletPerAnom, FingerMilletIrrigation_Type)

Prod_Dist <- Harv_Irr_Prod %>%
  filter(ProdIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, Prod_total, ProdAnom, ProdPerAnom, prop_irr_har, tot_prod, wProdAnom, wProdPerAnom, ProdIrrigation_Type)

Rice_dist_summary <- Rice_Dist %>%
  group_by(Year, RiceIrrigation_Type) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = RiceIrrigation_Type, wProdAnom = wRiceAnom, wProdPerAnom = wRicePerAnom) %>%
  mutate(crop = "Rice", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Rice", "Irrigated", "Rainfed"))

Maize_dist_summary <- Maize_Dist %>%
  group_by(Year, MaizeIrrigation_Type) %>%
  summarize(wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_prod), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = MaizeIrrigation_Type, wProdAnom = wMaizeAnom, wProdPerAnom = wMaizePerAnom) %>%
  mutate(crop = "Maize", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Maize", "Irrigated", "Rainfed"))

Sorghum_dist_summary <- Sorghum_Dist %>%
  group_by(Year, SorghumIrrigation_Type) %>%
  summarize(wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_prod), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = SorghumIrrigation_Type, wProdAnom = wSorghumAnom, wProdPerAnom = wSorghumPerAnom) %>%
  mutate(crop = "Sorghum", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Sorghum", "Irrigated", "Rainfed"))

PearlMillet_dist_summary <- PearlMillet_Dist %>%
  group_by(Year, PearlMilletIrrigation_Type) %>%
  summarize(wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_prod), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = PearlMilletIrrigation_Type, wProdAnom = wPearlMilletAnom, wProdPerAnom = wPearlMilletPerAnom) %>%
  mutate(crop = "PearlMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_PearlMillet", "Irrigated", "Rainfed"))

FingerMillet_dist_summary <- FingerMillet_Dist %>%
  group_by(Year, FingerMilletIrrigation_Type) %>%
  summarize(wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_prod), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = FingerMilletIrrigation_Type, wProdAnom = wFingerMilletAnom, wProdPerAnom = wFingerMilletPerAnom) %>%
  mutate(crop = "FingerMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_FingerMillet", "Irrigated", "Rainfed"))

dist_summary <- Prod_Dist %>%
  group_by(Year, ProdIrrigation_Type) %>%
  summarize(wProdAnom = sum(wProdAnom, na.rm=TRUE)/mean(tot_prod), wProdPerAnom = sum(wProdPerAnom, na.rm=TRUE)/mean(tot_prod), .groups = "drop") %>%
  rename(Irrigation_Type = ProdIrrigation_Type, wProdAnom = wProdAnom, wProdPerAnom = wProdPerAnom) %>%
  mutate(crop = "All", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Prod", "Irrigated", "Rainfed"))

Prod_Irr_Rain <- rbind(Rice_dist_summary[5:100,], Maize_dist_summary[5:100,], Sorghum_dist_summary[5:100,], PearlMillet_dist_summary[5:100,], FingerMillet_dist_summary[5:100,], dist_summary[5:100,])
Prod_Irr_Rain$crop <- factor(Prod_Irr_Rain$crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet", "All"))
Prod_Irr_Rain_phase <- merge(Prod_Irr_Rain, nino34_iod_phase, by="Year")

prod_irr_phase <- subset(Prod_Irr_Rain_phase, Irrigation_Type=="Irrigated")
prod_rain_phase <- subset(Prod_Irr_Rain_phase, Irrigation_Type=="Rainfed")
prod_irr_nphase <- subset(prod_irr_phase, nino34phase != "Neutral")
prod_rain_nphase <- subset(prod_rain_phase, nino34phase != "Neutral")
prod_irr_iphase <- subset(prod_irr_phase, iodphase != "Neutral")
prod_rain_iphase <- subset(prod_rain_phase, iodphase != "Neutral")

# Absolute Production Anomalies

Harv_Irr_Prod$RI.RI <- ifelse(Harv_Irr_Prod$RICE.PRODUCTION..1000.tons. %in% c(-1, NA), NA, 1)
Harv_Irr_Prod_RI <- subset(Harv_Irr_Prod, RI.RI==1)

Harv_Irr_Prod_RI$RI.MZ <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$MAIZE.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI1 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'MZ_tot_prod', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Prod_RI.MZ <- subset(Harv_Irr_Prod_RI1, RI.MZ==1)
Harv_Irr_Prod_RI.MZ1 <- subset(Harv_Irr_Prod_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Prod_RI.MZ2 <- Harv_Irr_Prod_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_prod), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_prod))
Harv_Irr_Prod_RI.MZ3 <- merge(Harv_Irr_Prod_RI.MZ2, nino34_iod_phase, by="Year")

prodanom_avg <- pivot_longer(Harv_Irr_Prod_RI.MZ3, c(2,4), names_to = "Crops", values_to = "ProdAnom")
prodanom_avg$Crop <- ifelse(prodanom_avg$Crops=="wRiceAnom", "Rice", "Maize")
prodanom_avg$Crops <- NULL
prodanom_avg$Crop <- factor(prodanom_avg$Crop, levels = c("Rice", "Maize"))
prodanom_avg$phase <- factor(prodanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodanom_avg$phase1 <- factor(prodanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodanom_avg$phase2 <- factor(prodanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Prod_RI$RI.SGK <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI2 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom','SG_tot_prod',  'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Prod_RI.SGK <- subset(Harv_Irr_Prod_RI2, RI.SGK==1)
Harv_Irr_Prod_RI.SGK1 <- subset(Harv_Irr_Prod_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Prod_RI.SGK2 <- Harv_Irr_Prod_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_prod), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_prod))
Harv_Irr_Prod_RI.SGK3 <- merge(Harv_Irr_Prod_RI.SGK2, nino34_iod_phase, by="Year")

prodanom_avg <- pivot_longer(Harv_Irr_Prod_RI.SGK3, c(2,4), names_to = "Crops", values_to = "ProdAnom")
prodanom_avg$Crop <- ifelse(prodanom_avg$Crops=="wRiceAnom", "Rice", "Sorghum")
prodanom_avg$Crops <- NULL
prodanom_avg$Crop <- factor(prodanom_avg$Crop, levels = c("Rice", "Sorghum"))
prodanom_avg$phase <- factor(prodanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodanom_avg$phase1 <- factor(prodanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodanom_avg$phase2 <- factor(prodanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Prod_RI$RI.PM <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$PEARL.MILLET.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI3 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'PM_tot_prod', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Prod_RI.PM <- subset(Harv_Irr_Prod_RI3, RI.PM==1)
Harv_Irr_Prod_RI.PM1 <- subset(Harv_Irr_Prod_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Prod_RI.PM2 <- Harv_Irr_Prod_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_prod), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_prod))
Harv_Irr_Prod_RI.PM3 <- merge(Harv_Irr_Prod_RI.PM2, nino34_iod_phase, by="Year")

prodanom_avg <- pivot_longer(Harv_Irr_Prod_RI.PM3, c(2,4), names_to = "Crops", values_to = "ProdAnom")
prodanom_avg$Crop <- ifelse(prodanom_avg$Crops=="wRiceAnom", "Rice", "PearlMillet")
prodanom_avg$Crops <- NULL
prodanom_avg$Crop <- factor(prodanom_avg$Crop, levels = c("Rice", "PearlMillet"))
prodanom_avg$phase <- factor(prodanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodanom_avg$phase1 <- factor(prodanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Prod_RI$RI.FM <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$FINGER.MILLET.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI4 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'FM_tot_prod', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Prod_RI.FM <- subset(Harv_Irr_Prod_RI4, RI.FM==1)
Harv_Irr_Prod_RI.FM1 <- subset(Harv_Irr_Prod_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Prod_RI.FM2 <- Harv_Irr_Prod_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_prod), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_prod))
Harv_Irr_Prod_RI.FM3 <- merge(Harv_Irr_Prod_RI.FM2, nino34_iod_phase, by="Year")

prodanom_avg <- pivot_longer(Harv_Irr_Prod_RI.FM3, c(2,4), names_to = "Crops", values_to = "ProdAnom")
prodanom_avg$Crop <- ifelse(prodanom_avg$Crops=="wRiceAnom", "Rice", "FingerMillet")
prodanom_avg$Crops <- NULL
prodanom_avg$Crop <- factor(prodanom_avg$Crop, levels = c("Rice", "FingerMillet"))
prodanom_avg$phase <- factor(prodanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodanom_avg$phase1 <- factor(prodanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodanom_avg$phase2 <- factor(prodanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$ProdAnom, Maize_E$ProdAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$ProdAnom, Sorghum_E$ProdAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$ProdAnom, PearlMillet_E$ProdAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$ProdAnom, FingerMillet_E$ProdAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$ProdAnom, Maize_L$ProdAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$ProdAnom, Sorghum_L$ProdAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$ProdAnom, PearlMillet_L$ProdAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$ProdAnom, FingerMillet_L$ProdAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$ProdAnom, Maize_P$ProdAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$ProdAnom, Sorghum_P$ProdAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$ProdAnom, PearlMillet_P$ProdAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$ProdAnom, FingerMillet_P$ProdAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$ProdAnom, Maize_N$ProdAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$ProdAnom, Sorghum_N$ProdAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$ProdAnom, PearlMillet_N$ProdAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$ProdAnom, FingerMillet_N$ProdAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$ProdAnom, Maize_E$ProdAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$ProdAnom, Sorghum_E$ProdAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$ProdAnom, PearlMillet_E$ProdAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$ProdAnom, FingerMillet_E$ProdAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$ProdAnom, Maize_L$ProdAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$ProdAnom, Sorghum_L$ProdAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$ProdAnom, PearlMillet_L$ProdAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$ProdAnom, FingerMillet_L$ProdAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$ProdAnom, Maize_P$ProdAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$ProdAnom, Sorghum_P$ProdAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$ProdAnom, PearlMillet_P$ProdAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$ProdAnom, FingerMillet_P$ProdAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$ProdAnom, Maize_N$ProdAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$ProdAnom, Sorghum_N$ProdAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$ProdAnom, PearlMillet_N$ProdAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$ProdAnom, FingerMillet_N$ProdAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations

Overlapping_prod <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=ProdAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("Production Anomalies (1000 tons)")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=ProdAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-35,35))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")
# annotate("text", x = 7.3, y = 35, label = "*", size = 8, color ="black")+
# annotate("text", x = 8.3, y = 35, label = "*", size = 8, color ="black")

# Relative Production Anomalies

Harv_Irr_Prod$RI.RI <- ifelse(Harv_Irr_Prod$RICE.PRODUCTION..1000.tons. %in% c(-1, NA), NA, 1)
Harv_Irr_Prod_RI <- subset(Harv_Irr_Prod, RI.RI==1)

Harv_Irr_Prod_RI$RI.MZ <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$MAIZE.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI1 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'MZ_tot_prod', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Prod_RI.MZ <- subset(Harv_Irr_Prod_RI1, RI.MZ==1)
Harv_Irr_Prod_RI.MZ1 <- subset(Harv_Irr_Prod_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Prod_RI.MZ2 <- Harv_Irr_Prod_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_prod), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_prod))
Harv_Irr_Prod_RI.MZ3 <- merge(Harv_Irr_Prod_RI.MZ2, nino34_iod_phase, by="Year")

prodperanom_avg <- pivot_longer(Harv_Irr_Prod_RI.MZ3, c(3,5), names_to = "Crops", values_to = "ProdPerAnom")
prodperanom_avg$Crop <- ifelse(prodperanom_avg$Crops=="wRicePerAnom", "Rice", "Maize")
prodperanom_avg$Crops <- NULL
prodperanom_avg$Crop <- factor(prodperanom_avg$Crop, levels = c("Rice", "Maize"))
prodperanom_avg$phase <- factor(prodperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodperanom_avg$phase1 <- factor(prodperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodperanom_avg$phase2 <- factor(prodperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Prod_RI$RI.SGK <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI2 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom','SG_tot_prod',  'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Prod_RI.SGK <- subset(Harv_Irr_Prod_RI2, RI.SGK==1)
Harv_Irr_Prod_RI.SGK1 <- subset(Harv_Irr_Prod_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Prod_RI.SGK2 <- Harv_Irr_Prod_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_prod), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_prod))
Harv_Irr_Prod_RI.SGK3 <- merge(Harv_Irr_Prod_RI.SGK2, nino34_iod_phase, by="Year")

prodperanom_avg <- pivot_longer(Harv_Irr_Prod_RI.SGK3, c(3,5), names_to = "Crops", values_to = "ProdPerAnom")
prodperanom_avg$Crop <- ifelse(prodperanom_avg$Crops=="wRicePerAnom", "Rice", "Sorghum")
prodperanom_avg$Crops <- NULL
prodperanom_avg$Crop <- factor(prodperanom_avg$Crop, levels = c("Rice", "Sorghum"))
prodperanom_avg$phase <- factor(prodperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodperanom_avg$phase1 <- factor(prodperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodperanom_avg$phase2 <- factor(prodperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Prod_RI$RI.PM <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$PEARL.MILLET.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI3 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'PM_tot_prod', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Prod_RI.PM <- subset(Harv_Irr_Prod_RI3, RI.PM==1)
Harv_Irr_Prod_RI.PM1 <- subset(Harv_Irr_Prod_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Prod_RI.PM2 <- Harv_Irr_Prod_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_prod), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_prod))
Harv_Irr_Prod_RI.PM3 <- merge(Harv_Irr_Prod_RI.PM2, nino34_iod_phase, by="Year")

prodperanom_avg <- pivot_longer(Harv_Irr_Prod_RI.PM3, c(3,5), names_to = "Crops", values_to = "ProdPerAnom")
prodperanom_avg$Crop <- ifelse(prodperanom_avg$Crops=="wRicePerAnom", "Rice", "PearlMillet")
prodperanom_avg$Crops <- NULL
prodperanom_avg$Crop <- factor(prodperanom_avg$Crop, levels = c("Rice", "PearlMillet"))
prodperanom_avg$phase <- factor(prodperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodperanom_avg$phase1 <- factor(prodperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Prod_RI$RI.FM <- ifelse((Harv_Irr_Prod_RI$RICE.PRODUCTION..1000.tons. %in% c(-1, NA)) | (Harv_Irr_Prod_RI$FINGER.MILLET.PRODUCTION..1000.tons. %in% c(-1, NA)), NA, 1)
Harv_Irr_Prod_RI4 <- Harv_Irr_Prod_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_prod', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'FM_tot_prod', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Prod_RI.FM <- subset(Harv_Irr_Prod_RI4, RI.FM==1)
Harv_Irr_Prod_RI.FM1 <- subset(Harv_Irr_Prod_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Prod_RI.FM2 <- Harv_Irr_Prod_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_prod), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_prod))
Harv_Irr_Prod_RI.FM3 <- merge(Harv_Irr_Prod_RI.FM2, nino34_iod_phase, by="Year")

prodperanom_avg <- pivot_longer(Harv_Irr_Prod_RI.FM3, c(3,5), names_to = "Crops", values_to = "ProdPerAnom")
prodperanom_avg$Crop <- ifelse(prodperanom_avg$Crops=="wRicePerAnom", "Rice", "FingerMillet")
prodperanom_avg$Crops <- NULL
prodperanom_avg$Crop <- factor(prodperanom_avg$Crop, levels = c("Rice", "FingerMillet"))
prodperanom_avg$phase <- factor(prodperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodperanom_avg$phase1 <- factor(prodperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodperanom_avg$phase2 <- factor(prodperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- prodperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- prodperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- prodperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$ProdPerAnom, Maize_E$ProdPerAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$ProdPerAnom, Sorghum_E$ProdPerAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$ProdPerAnom, PearlMillet_E$ProdPerAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$ProdPerAnom, FingerMillet_E$ProdPerAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$ProdPerAnom, Maize_L$ProdPerAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$ProdPerAnom, Sorghum_L$ProdPerAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$ProdPerAnom, PearlMillet_L$ProdPerAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$ProdPerAnom, FingerMillet_L$ProdPerAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$ProdPerAnom, Maize_P$ProdPerAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$ProdPerAnom, Sorghum_P$ProdPerAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$ProdPerAnom, PearlMillet_P$ProdPerAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$ProdPerAnom, FingerMillet_P$ProdPerAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$ProdPerAnom, Maize_N$ProdPerAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$ProdPerAnom, Sorghum_N$ProdPerAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$ProdPerAnom, PearlMillet_N$ProdPerAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$ProdPerAnom, FingerMillet_N$ProdPerAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$ProdPerAnom, Maize_E$ProdPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$ProdPerAnom, Sorghum_E$ProdPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$ProdPerAnom, PearlMillet_E$ProdPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$ProdPerAnom, FingerMillet_E$ProdPerAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$ProdPerAnom, Maize_L$ProdPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$ProdPerAnom, Sorghum_L$ProdPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$ProdPerAnom, PearlMillet_L$ProdPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$ProdPerAnom, FingerMillet_L$ProdPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$ProdPerAnom, Maize_P$ProdPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$ProdPerAnom, Sorghum_P$ProdPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$ProdPerAnom, PearlMillet_P$ProdPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$ProdPerAnom, FingerMillet_P$ProdPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$ProdPerAnom, Maize_N$ProdPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$ProdPerAnom, Sorghum_N$ProdPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$ProdPerAnom, PearlMillet_N$ProdPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$ProdPerAnom, FingerMillet_N$ProdPerAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations

Overlapping_prod <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=ProdPerAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("%Production Anomalies")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=ProdPerAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-15,15))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")

# Sensitivity of absolute and relative area anomalies to irrigated vs rainfed districts and within irrigated/rainfed districts, sensitivity of absolute and relative production anomalies to ENSO and IOD

Area1 <- Area[,c(-79)]
Harv_Irr_Yield1 <- Harv_Irr_Yield[,-c(7:65,67:72, 79:90, 169:175)]
Harv_Irr_Area <- merge(Area1,Harv_Irr_Yield1,by=c('Year','State.Code', 'Dist.Code', 'State.Name','Dist.Name','ID'))
Harv_Irr_Area$prop_irr_har <- ifelse(Harv_Irr_Area$Overall_area != 0, Harv_Irr_Area$IrrArea_total / Harv_Irr_Area$Overall_area, NA)

Harv_Irr_Area <- Harv_Irr_Area %>%
  mutate(RiceIrrigation_Type = case_when(
    RIprop_irr_har >= 0.5 ~ "Irrigated_Rice",
    RIprop_irr_har < 0.5 ~ "Rainfed_Rice",
    TRUE ~ "NA"),
    MaizeIrrigation_Type = case_when(
      MZprop_irr_har >= 0.5 ~ "Irrigated_Maize",
      MZprop_irr_har < 0.5 ~ "Rainfed_Maize",
      TRUE ~ "NA"),
    SorghumIrrigation_Type = case_when(
      SGprop_irr_har >= 0.5 ~ "Irrigated_Sorghum",
      SGprop_irr_har < 0.5 ~ "Rainfed_Sorghum",
      TRUE ~ "NA"),
    PearlMilletIrrigation_Type = case_when(
      PMprop_irr_har >= 0.5 ~ "Irrigated_PearlMillet",
      PMprop_irr_har < 0.5 ~ "Rainfed_PearlMillet",
      TRUE ~ "NA"),
    FingerMilletIrrigation_Type = case_when(
      FMprop_irr_har >= 0.5 ~ "Irrigated_FingerMillet",
      FMprop_irr_har < 0.5 ~ "Rainfed_FingerMillet",
      TRUE ~ "NA"),
    AreaIrrigation_Type = case_when(
      prop_irr_har >= 0.5 ~ "Irrigated_Area",
      prop_irr_har < 0.5 ~ "Rainfed_Area",
      TRUE ~ "NA"))

Rice_Dist <- Harv_Irr_Area %>%
  filter(RiceIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, RICE.AREA..1000.ha., RiceAnom, RicePerAnom, RIprop_irr_har, RI_tot_har_area, wRiceAnom, wRicePerAnom, RiceIrrigation_Type) 

Maize_Dist <- Harv_Irr_Area %>%
  filter(MaizeIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, MAIZE.AREA..1000.ha., MaizeAnom, MaizePerAnom, MZprop_irr_har, MZ_tot_har_area, wMaizeAnom, wMaizePerAnom, MaizeIrrigation_Type) 

Sorghum_Dist <- Harv_Irr_Area %>%
  filter(SorghumIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, KHARIF.SORGHUM.AREA..1000.ha., SorghumKharifAnom, SorghumPerAnom, SGprop_irr_har, SG_tot_har_area, wSorghumAnom, wSorghumPerAnom, SorghumIrrigation_Type)

PearlMillet_Dist <- Harv_Irr_Area %>%
  filter(PearlMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, PEARL.MILLET.AREA..1000.ha., PearlMilletAnom, PearlMilletPerAnom, PMprop_irr_har, PM_tot_har_area, wPearlMilletAnom, wPearlMilletPerAnom, PearlMilletIrrigation_Type)

FingerMillet_Dist <- Harv_Irr_Area %>%
  filter(FingerMilletIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, FINGER.MILLET.AREA..1000.ha., FingerMilletAnom, FingerMilletPerAnom, FMprop_irr_har, FM_tot_har_area, wFingerMilletAnom, wFingerMilletPerAnom, FingerMilletIrrigation_Type)

Area_Dist <- Harv_Irr_Area %>%
  filter(AreaIrrigation_Type != "NA") %>%
  dplyr::select(Year, Dist.Code, Dist.Name, State.Code, State.Name, ID, Area_total, AreaAnom, AreaPerAnom, prop_irr_har, tot_har_area, wAreaAnom, wAreaPerAnom, AreaIrrigation_Type)

Rice_dist_summary <- Rice_Dist %>%
  group_by(Year, RiceIrrigation_Type) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = RiceIrrigation_Type, wAreaAnom = wRiceAnom, wAreaPerAnom = wRicePerAnom) %>%
  mutate(crop = "Rice", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Rice", "Irrigated", "Rainfed"))

Maize_dist_summary <- Maize_Dist %>%
  group_by(Year, MaizeIrrigation_Type) %>%
  summarize(wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = MaizeIrrigation_Type, wAreaAnom = wMaizeAnom, wAreaPerAnom = wMaizePerAnom) %>%
  mutate(crop = "Maize", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Maize", "Irrigated", "Rainfed"))

Sorghum_dist_summary <- Sorghum_Dist %>%
  group_by(Year, SorghumIrrigation_Type) %>%
  summarize(wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = SorghumIrrigation_Type, wAreaAnom = wSorghumAnom, wAreaPerAnom = wSorghumPerAnom) %>%
  mutate(crop = "Sorghum", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Sorghum", "Irrigated", "Rainfed"))

PearlMillet_dist_summary <- PearlMillet_Dist %>%
  group_by(Year, PearlMilletIrrigation_Type) %>%
  summarize(wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = PearlMilletIrrigation_Type, wAreaAnom = wPearlMilletAnom, wAreaPerAnom = wPearlMilletPerAnom) %>%
  mutate(crop = "PearlMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_PearlMillet", "Irrigated", "Rainfed"))

FingerMillet_dist_summary <- FingerMillet_Dist %>%
  group_by(Year, FingerMilletIrrigation_Type) %>%
  summarize(wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = FingerMilletIrrigation_Type, wAreaAnom = wFingerMilletAnom, wAreaPerAnom = wFingerMilletPerAnom) %>%
  mutate(crop = "FingerMillet", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_FingerMillet", "Irrigated", "Rainfed"))

dist_summary <- Area_Dist %>%
  group_by(Year, AreaIrrigation_Type) %>%
  summarize(wAreaAnom = sum(wAreaAnom, na.rm=TRUE)/mean(tot_har_area), wAreaPerAnom = sum(wAreaPerAnom, na.rm=TRUE)/mean(tot_har_area), .groups = "drop") %>%
  rename(Irrigation_Type = AreaIrrigation_Type, wAreaAnom = wAreaAnom, wAreaPerAnom = wAreaPerAnom) %>%
  mutate(crop = "All", Irrigation_Type = ifelse(Irrigation_Type == "Irrigated_Area", "Irrigated", "Rainfed"))

Area_Irr_Rain <- rbind(Rice_dist_summary[5:100,], Maize_dist_summary[5:100,], Sorghum_dist_summary[5:100,], PearlMillet_dist_summary[5:100,], FingerMillet_dist_summary[5:100,], dist_summary[5:100,])
Area_Irr_Rain$crop <- factor(Area_Irr_Rain$crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet", "All"))
Area_Irr_Rain_phase <- merge(Area_Irr_Rain, nino34_iod_phase, by="Year")

area_irr_phase <- subset(Area_Irr_Rain_phase, Irrigation_Type=="Irrigated")
area_rain_phase <- subset(Area_Irr_Rain_phase, Irrigation_Type=="Rainfed")
area_irr_nphase <- subset(area_irr_phase, nino34phase != "Neutral")
area_rain_nphase <- subset(area_rain_phase, nino34phase != "Neutral")
area_irr_iphase <- subset(area_irr_phase, iodphase != "Neutral")
area_rain_iphase <- subset(area_rain_phase, iodphase != "Neutral")

# Absolute Harvested Area Anomalies

Harv_Irr_Area$RI.RI <- ifelse(Harv_Irr_Area$RICE.AREA..1000.ha. %in% c(-1, NA), NA, 1)
Harv_Irr_Area_RI <- subset(Harv_Irr_Area, RI.RI==1)

Harv_Irr_Area_RI$RI.MZ <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$MAIZE.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI1 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'MZ_tot_har_area', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Area_RI.MZ <- subset(Harv_Irr_Area_RI1, RI.MZ==1)
Harv_Irr_Area_RI.MZ1 <- subset(Harv_Irr_Area_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Area_RI.MZ2 <- Harv_Irr_Area_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area))
Harv_Irr_Area_RI.MZ3 <- merge(Harv_Irr_Area_RI.MZ2, nino34_iod_phase, by="Year")

areaanom_avg <- pivot_longer(Harv_Irr_Area_RI.MZ3, c(2,4), names_to = "Crops", values_to = "AreaAnom")
areaanom_avg$Crop <- ifelse(areaanom_avg$Crops=="wRiceAnom", "Rice", "Maize")
areaanom_avg$Crops <- NULL
areaanom_avg$Crop <- factor(areaanom_avg$Crop, levels = c("Rice", "Maize"))
areaanom_avg$phase <- factor(areaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaanom_avg$phase1 <- factor(areaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaanom_avg$phase2 <- factor(areaanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Area_RI$RI.SGK <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$KHARIF.SORGHUM.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI2 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom','SG_tot_har_area',  'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Area_RI.SGK <- subset(Harv_Irr_Area_RI2, RI.SGK==1)
Harv_Irr_Area_RI.SGK1 <- subset(Harv_Irr_Area_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Area_RI.SGK2 <- Harv_Irr_Area_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area))
Harv_Irr_Area_RI.SGK3 <- merge(Harv_Irr_Area_RI.SGK2, nino34_iod_phase, by="Year")

areaanom_avg <- pivot_longer(Harv_Irr_Area_RI.SGK3, c(2,4), names_to = "Crops", values_to = "AreaAnom")
areaanom_avg$Crop <- ifelse(areaanom_avg$Crops=="wRiceAnom", "Rice", "Sorghum")
areaanom_avg$Crops <- NULL
areaanom_avg$Crop <- factor(areaanom_avg$Crop, levels = c("Rice", "Sorghum"))
areaanom_avg$phase <- factor(areaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaanom_avg$phase1 <- factor(areaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaanom_avg$phase2 <- factor(areaanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Area_RI$RI.PM <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$PEARL.MILLET.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI3 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'PM_tot_har_area', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Area_RI.PM <- subset(Harv_Irr_Area_RI3, RI.PM==1)
Harv_Irr_Area_RI.PM1 <- subset(Harv_Irr_Area_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Area_RI.PM2 <- Harv_Irr_Area_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area))
Harv_Irr_Area_RI.PM3 <- merge(Harv_Irr_Area_RI.PM2, nino34_iod_phase, by="Year")

areaanom_avg <- pivot_longer(Harv_Irr_Area_RI.PM3, c(2,4), names_to = "Crops", values_to = "AreaAnom")
areaanom_avg$Crop <- ifelse(areaanom_avg$Crops=="wRiceAnom", "Rice", "PearlMillet")
areaanom_avg$Crops <- NULL
areaanom_avg$Crop <- factor(areaanom_avg$Crop, levels = c("Rice", "PearlMillet"))
areaanom_avg$phase <- factor(areaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaanom_avg$phase1 <- factor(areaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Area_RI$RI.FM <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$FINGER.MILLET.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI4 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'FM_tot_har_area', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Area_RI.FM <- subset(Harv_Irr_Area_RI4, RI.FM==1)
Harv_Irr_Area_RI.FM1 <- subset(Harv_Irr_Area_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Area_RI.FM2 <- Harv_Irr_Area_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area))
Harv_Irr_Area_RI.FM3 <- merge(Harv_Irr_Area_RI.FM2, nino34_iod_phase, by="Year")

areaanom_avg <- pivot_longer(Harv_Irr_Area_RI.FM3, c(2,4), names_to = "Crops", values_to = "AreaAnom")
areaanom_avg$Crop <- ifelse(areaanom_avg$Crops=="wRiceAnom", "Rice", "FingerMillet")
areaanom_avg$Crops <- NULL
areaanom_avg$Crop <- factor(areaanom_avg$Crop, levels = c("Rice", "FingerMillet"))
areaanom_avg$phase <- factor(areaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaanom_avg$phase1 <- factor(areaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaanom_avg$phase2 <- factor(areaanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$AreaAnom, Maize_E$AreaAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$AreaAnom, Sorghum_E$AreaAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$AreaAnom, PearlMillet_E$AreaAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$AreaAnom, FingerMillet_E$AreaAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$AreaAnom, Maize_L$AreaAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$AreaAnom, Sorghum_L$AreaAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$AreaAnom, PearlMillet_L$AreaAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$AreaAnom, FingerMillet_L$AreaAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$AreaAnom, Maize_P$AreaAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$AreaAnom, Sorghum_P$AreaAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$AreaAnom, PearlMillet_P$AreaAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$AreaAnom, FingerMillet_P$AreaAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$AreaAnom, Maize_N$AreaAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$AreaAnom, Sorghum_N$AreaAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$AreaAnom, PearlMillet_N$AreaAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$AreaAnom, FingerMillet_N$AreaAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$AreaAnom, Maize_E$AreaAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$AreaAnom, Sorghum_E$AreaAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$AreaAnom, PearlMillet_E$AreaAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$AreaAnom, FingerMillet_E$AreaAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$AreaAnom, Maize_L$AreaAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$AreaAnom, Sorghum_L$AreaAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$AreaAnom, PearlMillet_L$AreaAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$AreaAnom, FingerMillet_L$AreaAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$AreaAnom, Maize_P$AreaAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$AreaAnom, Sorghum_P$AreaAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$AreaAnom, PearlMillet_P$AreaAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$AreaAnom, FingerMillet_P$AreaAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$AreaAnom, Maize_N$AreaAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$AreaAnom, Sorghum_N$AreaAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$AreaAnom, PearlMillet_N$AreaAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$AreaAnom, FingerMillet_N$AreaAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations

Overlapping_area <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=AreaAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("Harvested Area Anomalies (1000 ha)")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=AreaAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-7.5,7.5))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")
# annotate("text", x = 7.1, y = 7.5, label = "*", size = 8, color ="black")+
# annotate("text", x = 8.1, y = 7.5, label = "*", size = 8, color ="black")

# Relative Harvested Area Anomalies

Harv_Irr_Area$RI.RI <- ifelse(Harv_Irr_Area$RICE.AREA..1000.ha. %in% c(-1, NA), NA, 1)
Harv_Irr_Area_RI <- subset(Harv_Irr_Area, RI.RI==1)

Harv_Irr_Area_RI$RI.MZ <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$MAIZE.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI1 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wMaizeAnom', 'wMaizePerAnom', 'MZ_tot_har_area', 'MZprop_irr_har',  'MaizeIrrigation_Type', 'RI.RI', 'RI.MZ')]
Harv_Irr_Area_RI.MZ <- subset(Harv_Irr_Area_RI1, RI.MZ==1)
Harv_Irr_Area_RI.MZ1 <- subset(Harv_Irr_Area_RI.MZ, RiceIrrigation_Type=='Rainfed_Rice' &  MaizeIrrigation_Type=='Rainfed_Maize')
Harv_Irr_Area_RI.MZ2 <- Harv_Irr_Area_RI.MZ1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area))
Harv_Irr_Area_RI.MZ3 <- merge(Harv_Irr_Area_RI.MZ2, nino34_iod_phase, by="Year")

areaperanom_avg <- pivot_longer(Harv_Irr_Area_RI.MZ3, c(3,5), names_to = "Crops", values_to = "AreaPerAnom")
areaperanom_avg$Crop <- ifelse(areaperanom_avg$Crops=="wRicePerAnom", "Rice", "Maize")
areaperanom_avg$Crops <- NULL
areaperanom_avg$Crop <- factor(areaperanom_avg$Crop, levels = c("Rice", "Maize"))
areaperanom_avg$phase <- factor(areaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaperanom_avg$phase1 <- factor(areaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaperanom_avg$phase2 <- factor(areaperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_MZ <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_MZ <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_MZ$phase)

Harv_Irr_Area_RI$RI.SGK <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$KHARIF.SORGHUM.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI2 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wSorghumAnom', 'wSorghumPerAnom','SG_tot_har_area',  'SGprop_irr_har',  'SorghumIrrigation_Type', 'RI.RI', 'RI.SGK')]
Harv_Irr_Area_RI.SGK <- subset(Harv_Irr_Area_RI2, RI.SGK==1)
Harv_Irr_Area_RI.SGK1 <- subset(Harv_Irr_Area_RI.SGK, RiceIrrigation_Type=='Rainfed_Rice' &  SorghumIrrigation_Type=='Rainfed_Sorghum')
Harv_Irr_Area_RI.SGK2 <- Harv_Irr_Area_RI.SGK1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area))
Harv_Irr_Area_RI.SGK3 <- merge(Harv_Irr_Area_RI.SGK2, nino34_iod_phase, by="Year")

areaperanom_avg <- pivot_longer(Harv_Irr_Area_RI.SGK3, c(3,5), names_to = "Crops", values_to = "AreaPerAnom")
areaperanom_avg$Crop <- ifelse(areaperanom_avg$Crops=="wRicePerAnom", "Rice", "Sorghum")
areaperanom_avg$Crops <- NULL
areaperanom_avg$Crop <- factor(areaperanom_avg$Crop, levels = c("Rice", "Sorghum"))
areaperanom_avg$phase <- factor(areaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaperanom_avg$phase1 <- factor(areaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaperanom_avg$phase2 <- factor(areaperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_SGK <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_SGK <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_SGK$phase)

Harv_Irr_Area_RI$RI.PM <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$PEARL.MILLET.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI3 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wPearlMilletAnom', 'wPearlMilletPerAnom', 'PM_tot_har_area', 'PMprop_irr_har',  'PearlMilletIrrigation_Type', 'RI.RI', 'RI.PM')]
Harv_Irr_Area_RI.PM <- subset(Harv_Irr_Area_RI3, RI.PM==1)
Harv_Irr_Area_RI.PM1 <- subset(Harv_Irr_Area_RI.PM, RiceIrrigation_Type=='Rainfed_Rice' &  PearlMilletIrrigation_Type=='Rainfed_PearlMillet')
Harv_Irr_Area_RI.PM2 <- Harv_Irr_Area_RI.PM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area))
Harv_Irr_Area_RI.PM3 <- merge(Harv_Irr_Area_RI.PM2, nino34_iod_phase, by="Year")

areaperanom_avg <- pivot_longer(Harv_Irr_Area_RI.PM3, c(3,5), names_to = "Crops", values_to = "AreaPerAnom")
areaperanom_avg$Crop <- ifelse(areaperanom_avg$Crops=="wRicePerAnom", "Rice", "PearlMillet")
areaperanom_avg$Crops <- NULL
areaperanom_avg$Crop <- factor(areaperanom_avg$Crop, levels = c("Rice", "PearlMillet"))
areaperanom_avg$phase <- factor(areaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaperanom_avg$phase1 <- factor(areaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_PM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_PM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_PM$phase)

Harv_Irr_Area_RI$RI.FM <- ifelse((Harv_Irr_Area_RI$RICE.AREA..1000.ha. %in% c(-1, NA)) | (Harv_Irr_Area_RI$FINGER.MILLET.AREA..1000.ha. %in% c(-1, NA)), NA, 1)
Harv_Irr_Area_RI4 <- Harv_Irr_Area_RI[,c('Year','State.Code', 'Dist.Code', 'State.Name', 'Dist.Name', 'ID', 'wRiceAnom', 'wRicePerAnom', 'RI_tot_har_area', 'RIprop_irr_har', 'RiceIrrigation_Type', 'wFingerMilletAnom', 'wFingerMilletPerAnom', 'FM_tot_har_area', 'FMprop_irr_har',  'FingerMilletIrrigation_Type', 'RI.RI', 'RI.FM')]
Harv_Irr_Area_RI.FM <- subset(Harv_Irr_Area_RI4, RI.FM==1)
Harv_Irr_Area_RI.FM1 <- subset(Harv_Irr_Area_RI.FM, RiceIrrigation_Type=='Rainfed_Rice' &  FingerMilletIrrigation_Type=='Rainfed_FingerMillet')
Harv_Irr_Area_RI.FM2 <- Harv_Irr_Area_RI.FM1 %>%
  group_by(Year) %>%
  summarize(wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area))
Harv_Irr_Area_RI.FM3 <- merge(Harv_Irr_Area_RI.FM2, nino34_iod_phase, by="Year")

areaperanom_avg <- pivot_longer(Harv_Irr_Area_RI.FM3, c(3,5), names_to = "Crops", values_to = "AreaPerAnom")
areaperanom_avg$Crop <- ifelse(areaperanom_avg$Crops=="wRicePerAnom", "Rice", "FingerMillet")
areaperanom_avg$Crops <- NULL
areaperanom_avg$Crop <- factor(areaperanom_avg$Crop, levels = c("Rice", "FingerMillet"))
areaperanom_avg$phase <- factor(areaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaperanom_avg$phase1 <- factor(areaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaperanom_avg$phase2 <- factor(areaperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
phaseENSO <- areaperanom_avg[,c(1,4:8,11:12)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[6] <- "phase"
phaseIOD <- areaperanom_avg[,c(1,4:7,9,11:12)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[6] <- "phase"
phasesENSOIOD <- areaperanom_avg[,c(1,4:7,10:12)]
phaseENSOIOD_FM <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD_FM <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD_FM$phase)

All_ENSO_IOD_MZ$Crops <- ifelse(All_ENSO_IOD_MZ$Crop=="Rice","Rice_MZ","Maize")
All_ENSO_IOD_SGK$Crops <- ifelse(All_ENSO_IOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
All_ENSO_IOD_PM$Crops <- ifelse(All_ENSO_IOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
All_ENSO_IOD_FM$Crops <- ifelse(All_ENSO_IOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
All_ENSO_IOD1 <- rbind(All_ENSO_IOD_MZ,All_ENSO_IOD_SGK,All_ENSO_IOD_PM,All_ENSO_IOD_FM)
All_ENSO_IOD1$Crops <- factor(All_ENSO_IOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

phaseENSOIOD_MZ$Crops <- ifelse(phaseENSOIOD_MZ$Crop=="Rice","Rice_MZ","Maize")
phaseENSOIOD_SGK$Crops <- ifelse(phaseENSOIOD_SGK$Crop=="Rice","Rice_SGK","Sorghum")
phaseENSOIOD_PM$Crops <- ifelse(phaseENSOIOD_PM$Crop=="Rice","Rice_PM","PearlMillet")
phaseENSOIOD_FM$Crops <- ifelse(phaseENSOIOD_FM$Crop=="Rice","Rice_FM","FingerMillet")
phaseENSOIOD1 <- rbind(phaseENSOIOD_MZ,phaseENSOIOD_SGK,phaseENSOIOD_PM,phaseENSOIOD_FM)
phaseENSOIOD1$Crops <- factor(phaseENSOIOD1$Crops, levels = c("Rice_MZ","Maize","Rice_SGK","Sorghum","Rice_PM","PearlMillet","Rice_FM","FingerMillet"))

Rice_MZ_E <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="El Nino")
Maize_E <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="El Nino")
wilcox.test(Rice_MZ_E$AreaPerAnom, Maize_E$AreaPerAnom)

Rice_SGK_E <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="El Nino")
Sorghum_E <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="El Nino")
wilcox.test(Rice_SGK_E$AreaPerAnom, Sorghum_E$AreaPerAnom)

Rice_PM_E <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="El Nino")
PearlMillet_E <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="El Nino")
wilcox.test(Rice_PM_E$AreaPerAnom, PearlMillet_E$AreaPerAnom)

Rice_FM_E <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="El Nino")
FingerMillet_E <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="El Nino")
wilcox.test(Rice_FM_E$AreaPerAnom, FingerMillet_E$AreaPerAnom)

#

Rice_MZ_L <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="La Nina")
Maize_L <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="La Nina")
wilcox.test(Rice_MZ_L$AreaPerAnom, Maize_L$AreaPerAnom)

Rice_SGK_L <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="La Nina")
Sorghum_L <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="La Nina")
wilcox.test(Rice_SGK_L$AreaPerAnom, Sorghum_L$AreaPerAnom)

Rice_PM_L <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="La Nina")
PearlMillet_L <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="La Nina")
wilcox.test(Rice_PM_L$AreaPerAnom, PearlMillet_L$AreaPerAnom)

Rice_FM_L <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="La Nina")
FingerMillet_L <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="La Nina")
wilcox.test(Rice_FM_L$AreaPerAnom, FingerMillet_L$AreaPerAnom)

#

Rice_MZ_P <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD+")
Maize_P <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD+")
wilcox.test(Rice_MZ_P$AreaPerAnom, Maize_P$AreaPerAnom)

Rice_SGK_P <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD+")
Sorghum_P <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD+")
wilcox.test(Rice_SGK_P$AreaPerAnom, Sorghum_P$AreaPerAnom)

Rice_PM_P <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD+")
PearlMillet_P <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD+")
wilcox.test(Rice_PM_P$AreaPerAnom, PearlMillet_P$AreaPerAnom)

Rice_FM_P <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD+")
FingerMillet_P <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD+")
wilcox.test(Rice_FM_P$AreaPerAnom, FingerMillet_P$AreaPerAnom)

#

Rice_MZ_N <- subset(All_ENSO_IOD1, Crops=="Rice_MZ" & phase=="IOD-")
Maize_N <- subset(All_ENSO_IOD1, Crops=="Maize" & phase=="IOD-")
wilcox.test(Rice_MZ_N$AreaPerAnom, Maize_N$AreaPerAnom)

Rice_SGK_N <- subset(All_ENSO_IOD1, Crops=="Rice_SGK" & phase=="IOD-")
Sorghum_N <- subset(All_ENSO_IOD1, Crops=="Sorghum" & phase=="IOD-")
wilcox.test(Rice_SGK_N$AreaPerAnom, Sorghum_N$AreaPerAnom)

Rice_PM_N <- subset(All_ENSO_IOD1, Crops=="Rice_PM" & phase=="IOD-")
PearlMillet_N <- subset(All_ENSO_IOD1, Crops=="PearlMillet" & phase=="IOD-")
wilcox.test(Rice_PM_N$AreaPerAnom, PearlMillet_N$AreaPerAnom)

Rice_FM_N <- subset(All_ENSO_IOD1, Crops=="Rice_FM" & phase=="IOD-")
FingerMillet_N <- subset(All_ENSO_IOD1, Crops=="FingerMillet" & phase=="IOD-")
wilcox.test(Rice_FM_N$AreaPerAnom, FingerMillet_N$AreaPerAnom)

# Perform the Wilcoxon tests
wilcox_tests <- list(
  "Rice_MZ vs. Maize (El Nino)" = wilcox.test(Rice_MZ_E$AreaPerAnom, Maize_E$AreaPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (El Nino)" = wilcox.test(Rice_SGK_E$AreaPerAnom, Sorghum_E$AreaPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (El Nino)" = wilcox.test(Rice_PM_E$AreaPerAnom, PearlMillet_E$AreaPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (El Nino)" = wilcox.test(Rice_FM_E$AreaPerAnom, FingerMillet_E$AreaPerAnom)$p.value,
  "Rice_MZ vs. Maize (La Nina)" = wilcox.test(Rice_MZ_L$AreaPerAnom, Maize_L$AreaPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (La Nina)" = wilcox.test(Rice_SGK_L$AreaPerAnom, Sorghum_L$AreaPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (La Nina)" = wilcox.test(Rice_PM_L$AreaPerAnom, PearlMillet_L$AreaPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (La Nina)" = wilcox.test(Rice_FM_L$AreaPerAnom, FingerMillet_L$AreaPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD+)" = wilcox.test(Rice_MZ_P$AreaPerAnom, Maize_P$AreaPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD+)" = wilcox.test(Rice_SGK_P$AreaPerAnom, Sorghum_P$AreaPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD+)" = wilcox.test(Rice_PM_P$AreaPerAnom, PearlMillet_P$AreaPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD+)" = wilcox.test(Rice_FM_P$AreaPerAnom, FingerMillet_P$AreaPerAnom)$p.value,
  "Rice_MZ vs. Maize (IOD-)" = wilcox.test(Rice_MZ_N$AreaPerAnom, Maize_N$AreaPerAnom)$p.value,
  "Rice_SGK vs. Sorghum (IOD-)" = wilcox.test(Rice_SGK_N$AreaPerAnom, Sorghum_N$AreaPerAnom)$p.value,
  "Rice_PM vs. PearlMillet (IOD-)" = wilcox.test(Rice_PM_N$AreaPerAnom, PearlMillet_N$AreaPerAnom)$p.value,
  "Rice_FM vs. FingerMillet (IOD-)" = wilcox.test(Rice_FM_N$AreaPerAnom, FingerMillet_N$AreaPerAnom)$p.value
)

# Filter significant combinations (p-value < 0.05)
significant_combinations <- names(wilcox_tests)[sapply(wilcox_tests, `<`, 0.05)]
significant_combinations


Overlapping_area <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD1, aes(x=Crops, y=AreaPerAnom, fill=phase), outlier.size = 0.5)+
  theme_classic()+
  xlab("")+
  ylab("% Harvested Area Anomalies")+
  # geom_point(data = phaseENSOIOD1, aes(x=Crops, y=AreaPerAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.text.x = element_text(vjust=0.6, angle=90))+
  theme(legend.text=element_text(size=14))+
  geom_hline(yintercept=0, linetype="dashed", color = "dimgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-4,4))+
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "dimgrey")+
  geom_vline(xintercept = 6.5, linetype = "dashed", color = "dimgrey")
# annotate("text", x = 6.9, y = 4, label = "*", size = 8, color ="black")+
# annotate("text", x = 7.9, y = 4, label = "*", size = 8, color ="black")+
# annotate("text", x = 7.3, y = 4, label = "*", size = 8, color ="black")+
# annotate("text", x = 8.3, y = 4, label = "*", size = 8, color ="black")



