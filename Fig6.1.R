#Fig6.1 Maps of overlapping rainfed districts growing rice and alternative grains

Harv_Irr_Prod1 <- Harv_Irr_Prod_RI
Harv_Irr_Area1 <- Harv_Irr_Area_RI
Harv_Irr_Yield1 <- Harv_Irr_Yield_RI
Harv_Irr1 <- Harv_Irr_RI

average_production <- Harv_Irr_Prod1 %>%
  group_by(Dist.Code) %>%
  summarize(
    RI.RI_Avg = mean(RI.RI),
    RI.MZ_Avg = mean(RI.MZ),
    RI.SGK_Avg = mean(RI.SGK),
    RI.PM_Avg = mean(RI.PM),
    RI.FM_Avg = mean(RI.FM)
  )

average_area <- Harv_Irr_Area1 %>%
  group_by(Dist.Code) %>%
  summarize(
    RI.RI_Avg = mean(RI.RI),
    RI.MZ_Avg = mean(RI.MZ),
    RI.SGK_Avg = mean(RI.SGK),
    RI.PM_Avg = mean(RI.PM),
    RI.FM_Avg = mean(RI.FM)
  )

average_yield <- Harv_Irr_Yield1 %>%
  group_by(Dist.Code) %>%
  summarize(
    RI.RI_Avg = mean(RI.RI),
    RI.MZ_Avg = mean(RI.MZ),
    RI.SGK_Avg = mean(RI.SGK),
    RI.PM_Avg = mean(RI.PM),
    RI.FM_Avg = mean(RI.FM)
  )

average_irrigated <- Harv_Irr1 %>%
  group_by(Dist.Code) %>%
  summarize(
    RI.RI_Avg = mean(RI.RI),
    RI.MZ_Avg = mean(RI.MZ),
    RI.SGK_Avg = mean(RI.SGK),
    RI.PM_Avg = mean(RI.PM),
    RI.FM_Avg = mean(RI.FM)
  )

###Production - Rainfed areas

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
gg


merge(VDSA_shape, average_production_RI.SGK, by="Dist.Code")
average_production_RI.SGK$Dist.Code <- as.character(average_production_RI.SGK$Dist.Code)
map_and_data_average_production_RI.SGK <- full_join(mymap,  average_production_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


merge(VDSA_shape, average_production_RI.PM, by="Dist.Code")
average_production_RI.PM$Dist.Code <- as.character(average_production_RI.PM$Dist.Code)
map_and_data_average_production_RI.PM <- full_join(mymap,  average_production_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


merge(VDSA_shape, average_production_RI.FM, by="Dist.Code")
average_production_RI.FM$Dist.Code <- as.character(average_production_RI.FM$Dist.Code)
map_and_data_average_production_RI.FM <- full_join(mymap,  average_production_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_production_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_production_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

###Harvested Areas - Rainfed areas

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
gg


merge(VDSA_shape, average_area_RI.SGK, by="Dist.Code")
average_area_RI.SGK$Dist.Code <- as.character(average_area_RI.SGK$Dist.Code)
map_and_data_average_area_RI.SGK <- full_join(mymap,  average_area_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


merge(VDSA_shape, average_area_RI.PM, by="Dist.Code")
average_area_RI.PM$Dist.Code <- as.character(average_area_RI.PM$Dist.Code)
map_and_data_average_area_RI.PM <- full_join(mymap,  average_area_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


merge(VDSA_shape, average_area_RI.FM, by="Dist.Code")
average_area_RI.FM$Dist.Code <- as.character(average_area_RI.FM$Dist.Code)
map_and_data_average_area_RI.FM <- full_join(mymap,  average_area_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_area_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_area_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

###Yield - Rainfed areas

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
gg

merge(VDSA_shape, average_yield_RI.SGK, by="Dist.Code")
average_yield_RI.SGK$Dist.Code <- as.character(average_yield_RI.SGK$Dist.Code)
map_and_data_average_yield_RI.SGK <- full_join(mymap,  average_yield_RI.SGK, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.SGK, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.SGK, !is.na(RI.RI_Avg)), aes(fill=factor(RI.SGK_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


merge(VDSA_shape, average_yield_RI.PM, by="Dist.Code")
average_yield_RI.PM$Dist.Code <- as.character(average_yield_RI.PM$Dist.Code)
map_and_data_average_yield_RI.PM <- full_join(mymap,  average_yield_RI.PM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.PM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.PM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.PM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

merge(VDSA_shape, average_yield_RI.FM, by="Dist.Code")
average_yield_RI.FM$Dist.Code <- as.character(average_yield_RI.FM$Dist.Code)
map_and_data_average_yield_RI.FM <- full_join(mymap,  average_yield_RI.FM, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot() +
  geom_sf(data = map_and_data_average_yield_RI.FM, color = "black", fill = "white") +
  geom_sf(data = subset(map_and_data_average_yield_RI.FM, !is.na(RI.RI_Avg)), aes(fill=factor(RI.FM_Avg)))+
  scale_fill_manual(values = c("plum", "white")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

