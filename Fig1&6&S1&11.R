# Fig1&6&S1&11
# District maps of proportion of irrigated to harvested areas for each monsoon crop averaged from 1966-2017.
# District maps of average crop yields from 1966-2017
# Comparison of the sensitivity of alternative grains to climate variability relative to rice (inset maps)


# Maps of proportion of irrigated to harvested areas

VDSA_shp <- st_read(dsn = "/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/1966_Districts/", layer = "1966_districts_correct_india")
crs(VDSA_shp)
length(VDSA_shp["NAME_2"])
VDSA_shape <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/VDSA_shapefile.csv")
mymap <- st_read("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/1966_Districts/1966_districts_correct_india.shp", stringsAsFactors=FALSE)
colnames(mymap)[25] <- "JDist.Code"

Harv_Irr_year <- Harv_Irr %>%
  group_by(Dist.Code) %>%
  summarize(irr_mean_rice = mean(RICE.IRRIGATED.AREA..1000.ha., na.rm = TRUE),
            irr_mean_maize = mean(MAIZE.IRRIGATED.AREA..1000.ha., na.rm = TRUE),
            irr_mean_sorghum = mean(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., na.rm = TRUE),
            irr_mean_pearl_millet = mean(PEARL.MILLET.IRRIGATED.AREA..1000.ha., na.rm = TRUE),
            irr_mean_finger_millet = mean(FINGER.MILLET.IRRIGATED.AREA..1000.ha., na.rm = TRUE),
            har_mean_rice = mean(RICE.AREA..1000.ha., na.rm = TRUE),
            har_mean_maize = mean(MAIZE.AREA..1000.ha., na.rm = TRUE),
            har_mean_sorghum = mean(KHARIF.SORGHUM.AREA..1000.ha., na.rm = TRUE),
            har_mean_pearl_millet = mean(PEARL.MILLET.AREA..1000.ha., na.rm = TRUE),
            har_mean_finger_millet = mean(FINGER.MILLET.AREA..1000.ha., na.rm = TRUE),
            RIprop_irr_har = irr_mean_rice/har_mean_rice,
            MZprop_irr_har = irr_mean_maize/har_mean_maize,
            SGprop_irr_har = irr_mean_sorghum/har_mean_sorghum,
            PMprop_irr_har = irr_mean_pearl_millet/har_mean_pearl_millet,
            FMprop_irr_har = irr_mean_finger_millet/har_mean_finger_millet
  )

Harv_Irr_year$Dist.Code <- as.character(Harv_Irr_year$Dist.Code)
map_and_data_year <- merge(VDSA_shape, Harv_Irr_year, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))

map_and_data_year$RIprop_irr_har[which(map_and_data_year$RIprop_irr_har > 1)] <- 1
map_and_data_year$RIprop_irr_har[which(map_and_data_year$RIprop_irr_har < 0)] <- 0

ggplot(map_and_data_year) +
  geom_sf(aes(fill = RIprop_irr_har)) +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "blue") +
  ggtitle(paste("Rice Proportion of Irrigated to Harvested Areas")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_void()

map_and_data_year$MZprop_irr_har[which(map_and_data_year$MZprop_irr_har > 1)] <- 1
map_and_data_year$MZprop_irr_har[which(map_and_data_year$MZprop_irr_har < 0)] <- 0

ggplot(map_and_data_year) +
  geom_sf(aes(fill = MZprop_irr_har)) +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "blue") +
  ggtitle(paste("Maize Proportion of Irrigated to Harvested Areas")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_void()

map_and_data_year$SGprop_irr_har[which(map_and_data_year$SGprop_irr_har > 1)] <- 1
map_and_data_year$SGprop_irr_har[which(map_and_data_year$SGprop_irr_har < 0)] <- 0

ggplot(map_and_data_year) +
  geom_sf(aes(fill = SGprop_irr_har)) +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "blue") +
  ggtitle(paste("Sorghum Proportion of Irrigated to Harvested Areas")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_void()

map_and_data_year$PMprop_irr_har[which(map_and_data_year$PMprop_irr_har > 1)] <- 1
map_and_data_year$PMprop_irr_har[which(map_and_data_year$PMprop_irr_har < 0)] <- 0

ggplot(map_and_data_year) +
  geom_sf(aes(fill = PMprop_irr_har)) +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "blue") +
  ggtitle(paste("Pearl Millet Proportion of Irrigated to Harvested Areas")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_void()

map_and_data_year$FMprop_irr_har[which(map_and_data_year$FMprop_irr_har > 1)] <- 1
map_and_data_year$FMprop_irr_har[which(map_and_data_year$FMprop_irr_har < 0)] <- 0

ggplot(map_and_data_year) +
  geom_sf(aes(fill = FMprop_irr_har)) +
  scale_fill_continuous(limits = c(0, 1), low = "white", high = "blue") +
  ggtitle(paste("Finger Millet Proportion of Irrigated to Harvested Areas")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_void()

# Maps of average crop yields

Harv_Irr_Yield_R <- subset(Harv_Irr_Yield, !(RICE.YIELD..Kg.per.ha. %in% c(-1, NA)))
Harv_Irr_Yield_M <- subset(Harv_Irr_Yield, !(MAIZE.YIELD..Kg.per.ha. %in% c(-1, NA)))
Harv_Irr_Yield_S <- subset(Harv_Irr_Yield, !(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(-1, NA)))
Harv_Irr_Yield_P <- subset(Harv_Irr_Yield, !(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)))
Harv_Irr_Yield_F <- subset(Harv_Irr_Yield, !(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(-1, NA)))

mean_rice_yield_by_district <- Harv_Irr_Yield_R %>%
  group_by(Dist.Code) %>%
  summarize(mean_rice = mean(RICE.YIELD..Kg.per.ha., na.rm = TRUE))
mean_maize_yield_by_district <- Harv_Irr_Yield_M %>%
  group_by(Dist.Code) %>%
  summarize(mean_maize = mean(MAIZE.YIELD..Kg.per.ha., na.rm = TRUE))
mean_sorghum_yield_by_district <- Harv_Irr_Yield_S %>%
  group_by(Dist.Code) %>%
  summarize(mean_sorghum = mean(KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm = TRUE))
mean_pearlmillet_yield_by_district <- Harv_Irr_Yield_P %>%
  group_by(Dist.Code) %>%
  summarize(mean_pearl_millet = mean(PEARL.MILLET.YIELD..Kg.per.ha., na.rm = TRUE))
mean_fingermillet_yield_by_district <- Harv_Irr_Yield_F %>%
  group_by(Dist.Code) %>%
  summarize(mean_finger_millet = mean(FINGER.MILLET.YIELD..Kg.per.ha., na.rm = TRUE))

mean_rice_yield_by_district$Dist.Code <- as.character(mean_rice_yield_by_district$Dist.Code)
map_and_data_year <- merge(VDSA_shape, mean_rice_yield_by_district, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))
hist(map_and_data_year$mean_rice/1000)

ggplot(map_and_data_year) +
  geom_sf(aes(fill = mean_rice/1000)) +
  scale_fill_gradient(low = "white", high = "saddlebrown", na.value = "grey50") +
  ggtitle(paste("Mean Rice Yields")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1, label.vjust = 0.5, keywidth = 1, keyheight = 6, label.theme = element_text(size = 20))) +
  theme_void()

mean_maize_yield_by_district$Dist.Code <- as.character(mean_maize_yield_by_district$Dist.Code)
map_and_data_year <- merge(VDSA_shape, mean_maize_yield_by_district, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))
hist(map_and_data_year$mean_maize/1000)

ggplot(map_and_data_year) +
  geom_sf(aes(fill = mean_maize/1000)) +
  scale_fill_gradient(low = "white", high = "darkgoldenrod1", na.value = "grey50") +
  ggtitle(paste("Mean Maize Yields")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1, label.vjust = 0.5, keywidth = 1, keyheight = 6, label.theme = element_text(size = 20))) +
  theme_void()

mean_sorghum_yield_by_district$Dist.Code <- as.character(mean_sorghum_yield_by_district$Dist.Code)
map_and_data_year <- merge(VDSA_shape, mean_sorghum_yield_by_district, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))
hist(map_and_data_year$mean_sorghum/1000)

ggplot(map_and_data_year) +
  geom_sf(aes(fill = mean_sorghum/1000)) +
  scale_fill_gradient(low = "white", high = "darkgreen", na.value = "grey50") +
  ggtitle(paste("Mean Sorghum Yields")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1, label.vjust = 0.5, keywidth = 1, keyheight = 6, label.theme = element_text(size = 20))) +
  theme_void()

mean_pearlmillet_yield_by_district$Dist.Code <- as.character(mean_pearlmillet_yield_by_district$Dist.Code)
map_and_data_year <- merge(VDSA_shape, mean_pearlmillet_yield_by_district, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))
hist(map_and_data_year$mean_pearl_millet/1000)

ggplot(map_and_data_year) +
  geom_sf(aes(fill = mean_pearl_millet/1000)) +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey50") +
  ggtitle(paste("Mean Pearl Millet Yields")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1, label.vjust = 0.5, keywidth = 1, keyheight = 6, label.theme = element_text(size = 20))) +
  theme_void()

mean_fingermillet_yield_by_district$Dist.Code <- as.character(mean_fingermillet_yield_by_district$Dist.Code)
map_and_data_year <- merge(VDSA_shape, mean_fingermillet_yield_by_district, by = "Dist.Code")
map_and_data_year$Dist.Code <- as.character(map_and_data_year$Dist.Code)
map_and_data_year <- full_join(mymap, map_and_data_year, by = c("JDist.Code" = "Dist.Code"))
hist(map_and_data_year$mean_finger_millet/1000)

ggplot(map_and_data_year) +
  geom_sf(aes(fill = mean_finger_millet/1000)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey50") +
  ggtitle(paste("Mean Finger Millet Yields")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top", title.vjust = 1, label.vjust = 0.5, keywidth = 1, keyheight = 6, label.theme = element_text(size = 20))) +
  theme_void()

# Inset maps

# Production - Rainfed areas

Harv_Irr_Prod1_rainfed_RI.MZ <- subset(Harv_Irr_Prod1, RiceIrrigation_Type == "Rainfed_Rice" & MaizeIrrigation_Type == "Rainfed_Maize")
Harv_Irr_Prod1_rainfed_RI.SGK <- subset(Harv_Irr_Prod1, RiceIrrigation_Type == "Rainfed_Rice" & SorghumIrrigation_Type == "Rainfed_Sorghum")
Harv_Irr_Prod1_rainfed_RI.PM <- subset(Harv_Irr_Prod1, RiceIrrigation_Type == "Rainfed_Rice" & PearlMilletIrrigation_Type == "Rainfed_PearlMillet")
Harv_Irr_Prod1_rainfed_RI.FM <- subset(Harv_Irr_Prod1, RiceIrrigation_Type == "Rainfed_Rice" & FingerMilletIrrigation_Type == "Rainfed_FingerMillet")


average_production_RI.MZ <- Harv_Irr_Prod1_rainfed_RI.MZ %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.MZ_Avg = mean(RI.MZ))

average_production_RI.SGK <- Harv_Irr_Prod1_rainfed_RI.SGK %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.SGK_Avg = mean(RI.SGK))

average_production_RI.PM <- Harv_Irr_Prod1_rainfed_RI.PM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.PM_Avg = mean(RI.PM))

average_production_RI.FM <- Harv_Irr_Prod1_rainfed_RI.FM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.FM_Avg = mean(RI.FM))

merge(VDSA_shape, average_production_RI.MZ, by="Dist.Code")
average_production_RI.MZ$Dist.Code <- as.character(average_production_RI.MZ$Dist.Code)
map_and_data_average_production_RI.MZ <- full_join(mymap,  average_production_RI.MZ, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.MZ, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.MZ, !is.na(RI.RI_Avg)), aes(fill=factor(RI.MZ_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_production_RI.SGK, by="Dist.Code")
average_production_RI.SGK$Dist.Code <- as.character(average_production_RI.SGK$Dist.Code)
map_and_data_average_production_RI.SGK <- full_join(mymap,  average_production_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_production_RI.PM, by="Dist.Code")
average_production_RI.PM$Dist.Code <- as.character(average_production_RI.PM$Dist.Code)
map_and_data_average_production_RI.PM <- full_join(mymap,  average_production_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_production_RI.FM, by="Dist.Code")
average_production_RI.FM$Dist.Code <- as.character(average_production_RI.FM$Dist.Code)
map_and_data_average_production_RI.FM <- full_join(mymap,  average_production_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

# Harvested Areas - Rainfed areas

Harv_Irr_Area1_rainfed_RI.MZ <- subset(Harv_Irr_Area1, RiceIrrigation_Type == "Rainfed_Rice" & MaizeIrrigation_Type == "Rainfed_Maize")
Harv_Irr_Area1_rainfed_RI.SGK <- subset(Harv_Irr_Area1, RiceIrrigation_Type == "Rainfed_Rice" & SorghumIrrigation_Type == "Rainfed_Sorghum")
Harv_Irr_Area1_rainfed_RI.PM <- subset(Harv_Irr_Area1, RiceIrrigation_Type == "Rainfed_Rice" & PearlMilletIrrigation_Type == "Rainfed_PearlMillet")
Harv_Irr_Area1_rainfed_RI.FM <- subset(Harv_Irr_Area1, RiceIrrigation_Type == "Rainfed_Rice" & FingerMilletIrrigation_Type == "Rainfed_FingerMillet")

average_area_RI.MZ <- Harv_Irr_Area1_rainfed_RI.MZ %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.MZ_Avg = mean(RI.MZ))

average_area_RI.SGK <- Harv_Irr_Area1_rainfed_RI.SGK %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.SGK_Avg = mean(RI.SGK))

average_area_RI.PM <- Harv_Irr_Area1_rainfed_RI.PM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.PM_Avg = mean(RI.PM))

average_area_RI.FM <- Harv_Irr_Area1_rainfed_RI.FM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.FM_Avg = mean(RI.FM))

merge(VDSA_shape, average_area_RI.MZ, by="Dist.Code")
average_area_RI.MZ$Dist.Code <- as.character(average_area_RI.MZ$Dist.Code)
map_and_data_average_area_RI.MZ <- full_join(mymap,  average_area_RI.MZ, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.MZ, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.MZ, !is.na(RI.RI_Avg)), aes(fill=factor(RI.MZ_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_area_RI.SGK, by="Dist.Code")
average_area_RI.SGK$Dist.Code <- as.character(average_area_RI.SGK$Dist.Code)
map_and_data_average_area_RI.SGK <- full_join(mymap,  average_area_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_area_RI.PM, by="Dist.Code")
average_area_RI.PM$Dist.Code <- as.character(average_area_RI.PM$Dist.Code)
map_and_data_average_area_RI.PM <- full_join(mymap,  average_area_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_area_RI.FM, by="Dist.Code")
average_area_RI.FM$Dist.Code <- as.character(average_area_RI.FM$Dist.Code)
map_and_data_average_area_RI.FM <- full_join(mymap,  average_area_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

# Yield - Rainfed areas

Harv_Irr_Yield1_rainfed_RI.MZ <- subset(Harv_Irr_Yield1, RiceIrrigation_Type == "Rainfed_Rice" & MaizeIrrigation_Type == "Rainfed_Maize")
Harv_Irr_Yield1_rainfed_RI.SGK <- subset(Harv_Irr_Yield1, RiceIrrigation_Type == "Rainfed_Rice" & SorghumIrrigation_Type == "Rainfed_Sorghum")
Harv_Irr_Yield1_rainfed_RI.PM <- subset(Harv_Irr_Yield1, RiceIrrigation_Type == "Rainfed_Rice" & PearlMilletIrrigation_Type == "Rainfed_PearlMillet")
Harv_Irr_Yield1_rainfed_RI.FM <- subset(Harv_Irr_Yield1, RiceIrrigation_Type == "Rainfed_Rice" & FingerMilletIrrigation_Type == "Rainfed_FingerMillet")

average_yield_RI.MZ <- Harv_Irr_Yield1_rainfed_RI.MZ %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.MZ_Avg = mean(RI.MZ))

average_yield_RI.SGK <- Harv_Irr_Yield1_rainfed_RI.SGK %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.SGK_Avg = mean(RI.SGK))

average_yield_RI.PM <- Harv_Irr_Yield1_rainfed_RI.PM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.PM_Avg = mean(RI.PM))

average_yield_RI.FM <- Harv_Irr_Yield1_rainfed_RI.FM %>%
  group_by(Dist.Code) %>%
  summarize(RI.RI_Avg = mean(RI.RI),
            RI.FM_Avg = mean(RI.FM))

merge(VDSA_shape, average_yield_RI.MZ, by="Dist.Code")
average_yield_RI.MZ$Dist.Code <- as.character(average_yield_RI.MZ$Dist.Code)
map_and_data_average_yield_RI.MZ <- full_join(mymap,  average_yield_RI.MZ, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.MZ, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.MZ, !is.na(RI.RI_Avg)), aes(fill=factor(RI.MZ_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_yield_RI.SGK, by="Dist.Code")
average_yield_RI.SGK$Dist.Code <- as.character(average_yield_RI.SGK$Dist.Code)
map_and_data_average_yield_RI.SGK <- full_join(mymap,  average_yield_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_yield_RI.PM, by="Dist.Code")
average_yield_RI.PM$Dist.Code <- as.character(average_yield_RI.PM$Dist.Code)
map_and_data_average_yield_RI.PM <- full_join(mymap,  average_yield_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()

merge(VDSA_shape, average_yield_RI.FM, by="Dist.Code")
average_yield_RI.FM$Dist.Code <- as.character(average_yield_RI.FM$Dist.Code)
map_and_data_average_yield_RI.FM <- full_join(mymap,  average_yield_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
