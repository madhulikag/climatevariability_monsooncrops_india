# Fig2 El Niño decreases national-average crop yields

# Calculate the number of years with yields > 0 for each crop

yield_summary <- Yield %>%
  group_by(Dist.Code) %>%
  summarize(
    rice_years = sum(RICE.YIELD..Kg.per.ha. > 0),
    maize_years = sum(MAIZE.YIELD..Kg.per.ha. > 0),
    sorghum_years = sum(KHARIF.SORGHUM.YIELD..Kg.per.ha. > 0),
    pearl_millet_years = sum(PEARL.MILLET.YIELD..Kg.per.ha. > 0),
    finger_millet_years = sum(FINGER.MILLET.YIELD..Kg.per.ha. > 0)
  )

# Select Dist.Code for each crop where the number of years with yields > 40
dist_codes_rice <- yield_summary %>% filter(rice_years > 40) %>% dplyr::select(Dist.Code)
dist_codes_maize <- yield_summary %>% filter(maize_years > 40) %>% dplyr::select(Dist.Code)
dist_codes_sorghum <- yield_summary %>% filter(sorghum_years > 40) %>% dplyr::select(Dist.Code)
dist_codes_pearl_millet <- yield_summary %>% filter(pearl_millet_years > 40) %>% dplyr::select(Dist.Code)
dist_codes_finger_millet <- yield_summary %>% filter(finger_millet_years > 40) %>% dplyr::select(Dist.Code)

# Subset RICE YIELDS based on dist_codes_rice
rice_yields_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, RICE.YIELD..Kg.per.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_rice$Dist.Code)

# Subset MAIZE YIELDS based on dist_codes_maize
maize_yields_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, MAIZE.YIELD..Kg.per.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_maize$Dist.Code)

# Subset SORGHUM YIELDS based on dist_codes_sorghum
sorghum_yields_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, KHARIF.SORGHUM.YIELD..Kg.per.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_sorghum$Dist.Code)

# Subset PEARL MILLET YIELDS based on dist_codes_pearl_millet
pearl_millet_yields_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, PEARL.MILLET.YIELD..Kg.per.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_pearl_millet$Dist.Code)

# Subset FINGER MILLET YIELDS based on dist_codes_finger_millet
finger_millet_yields_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, FINGER.MILLET.YIELD..Kg.per.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_finger_millet$Dist.Code)

Yield_subset <- Yield %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID)


# Merge Yield_subset with rice_yields_subset
merged_data <- full_join(Yield_subset, rice_yields_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with maize_yields_subset
merged_data <- full_join(merged_data, maize_yields_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with sorghum_yields_subset
merged_data <- full_join(merged_data, sorghum_yields_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with pearl_millet_yields_subset
merged_data <- full_join(merged_data, pearl_millet_yields_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with finger_millet_yields_subset
merged_data <- full_join(merged_data, finger_millet_yields_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

Yield <- merged_data

# Subset RICE YIELDS based on dist_codes_rice
rice_prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, RICE.PRODUCTION..1000.tons.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_rice$Dist.Code)

# Subset MAIZE YIELDS based on dist_codes_maize
maize_prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, MAIZE.PRODUCTION..1000.tons.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_maize$Dist.Code)

# Subset SORGHUM YIELDS based on dist_codes_sorghum
sorghum_prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, KHARIF.SORGHUM.PRODUCTION..1000.tons.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_sorghum$Dist.Code)

# Subset PEARL MILLET YIELDS based on dist_codes_pearl_millet
pearl_millet_prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, PEARL.MILLET.PRODUCTION..1000.tons.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_pearl_millet$Dist.Code)

# Subset FINGER MILLET YIELDS based on dist_codes_finger_millet
finger_millet_prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, FINGER.MILLET.PRODUCTION..1000.tons.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_finger_millet$Dist.Code)

Prod_subset <- Prod %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID)

# Merge Yield_subset with rice_yields_subset
merged_data <- full_join(Prod_subset, rice_prod_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with maize_yields_subset
merged_data <- full_join(merged_data, maize_prod_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with sorghum_yields_subset
merged_data <- full_join(merged_data, sorghum_prod_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with pearl_millet_yields_subset
merged_data <- full_join(merged_data, pearl_millet_prod_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with finger_millet_yields_subset
merged_data <- full_join(merged_data, finger_millet_prod_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

Prod <- merged_data

# Subset RICE YIELDS based on dist_codes_rice
rice_area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, RICE.AREA..1000.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_rice$Dist.Code)

# Subset MAIZE YIELDS based on dist_codes_maize
maize_area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, MAIZE.AREA..1000.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_maize$Dist.Code)

# Subset SORGHUM YIELDS based on dist_codes_sorghum
sorghum_area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, KHARIF.SORGHUM.AREA..1000.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_sorghum$Dist.Code)

# Subset PEARL MILLET YIELDS based on dist_codes_pearl_millet
pearl_millet_area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, PEARL.MILLET.AREA..1000.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_pearl_millet$Dist.Code)

# Subset FINGER MILLET YIELDS based on dist_codes_finger_millet
finger_millet_area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, FINGER.MILLET.AREA..1000.ha.) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_finger_millet$Dist.Code)

Area_subset <- Area %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID)

# Merge Yield_subset with rice_yields_subset
merged_data <- full_join(Area_subset, rice_area_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with maize_yields_subset
merged_data <- full_join(merged_data, maize_area_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with sorghum_yields_subset
merged_data <- full_join(merged_data, sorghum_area_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with pearl_millet_yields_subset
merged_data <- full_join(merged_data, pearl_millet_area_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with finger_millet_yields_subset
merged_data <- full_join(merged_data, finger_millet_area_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

Area <- merged_data

# Subset RICE YIELDS based on dist_codes_rice
rice_irrarea_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, RICE.AREA..1000.ha., RICE.IRRIGATED.AREA..1000.ha., RIprop_irr_har) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_rice$Dist.Code)

# Subset MAIZE YIELDS based on dist_codes_maize
maize_irrarea_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, MAIZE.AREA..1000.ha., MAIZE.IRRIGATED.AREA..1000.ha., MZprop_irr_har) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_maize$Dist.Code)

# Subset SORGHUM YIELDS based on dist_codes_sorghum
sorghum_irrarea_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, KHARIF.SORGHUM.AREA..1000.ha., KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., SGprop_irr_har) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_sorghum$Dist.Code)

# Subset PEARL MILLET YIELDS based on dist_codes_pearl_millet
pearl_millet_irrarea_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, PEARL.MILLET.AREA..1000.ha., PEARL.MILLET.IRRIGATED.AREA..1000.ha., PMprop_irr_har) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_pearl_millet$Dist.Code)

# Subset FINGER MILLET YIELDS based on dist_codes_finger_millet
finger_millet_irrarea_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID, FINGER.MILLET.AREA..1000.ha., FINGER.MILLET.IRRIGATED.AREA..1000.ha., FMprop_irr_har) %>% 
  dplyr::filter(Dist.Code %in% dist_codes_finger_millet$Dist.Code)

Harv_Irr_subset <- Harv_Irr %>% 
  dplyr::select(Year, State.Code, Dist.Code, State.Name, Dist.Name, ID)

# Merge Yield_subset with rice_yields_subset
merged_data <- full_join(Harv_Irr_subset, rice_irrarea_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with maize_yields_subset
merged_data <- full_join(merged_data, maize_irrarea_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with sorghum_yields_subset
merged_data <- full_join(merged_data, sorghum_irrarea_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with pearl_millet_yields_subset
merged_data <- full_join(merged_data, pearl_millet_irrarea_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

# Merge with finger_millet_yields_subset
merged_data <- full_join(merged_data, finger_millet_irrarea_subset,
                         by = c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))

Harv_Irr <- merged_data

# Calculate the 5-year moving average and absolute and relative crop anomalies at the district level

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(RY_lead2 = dplyr::lead(RICE.YIELD..Kg.per.ha., n=2, order_by = Year),
         RY_lead1 = dplyr::lead(RICE.YIELD..Kg.per.ha., n=1, order_by = Year),
         RY_lag0 = RICE.YIELD..Kg.per.ha.,
         RY_lag1 = dplyr::lag(RICE.YIELD..Kg.per.ha., n=1, order_by = Year),
         RY_lag2 = dplyr::lag(RICE.YIELD..Kg.per.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average1 = (RY_lead2+RY_lead1+RY_lag0+RY_lag1+RY_lag2)/5) %>%
  ungroup()
Yield$RiceAnom <- Yield$RICE.YIELD..Kg.per.ha. - Yield$rolling_average1
Yield$RicePerAnom <- (Yield$RiceAnom*100)/Yield$rolling_average1

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(MZ_lead2 = dplyr::lead(MAIZE.YIELD..Kg.per.ha., n=2, order_by = Year),
         MZ_lead1 = dplyr::lead(MAIZE.YIELD..Kg.per.ha., n=1, order_by = Year),
         MZ_lag0 = MAIZE.YIELD..Kg.per.ha.,
         MZ_lag1 = dplyr::lag(MAIZE.YIELD..Kg.per.ha., n=1, order_by = Year),
         MZ_lag2 = dplyr::lag(MAIZE.YIELD..Kg.per.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average2 = (MZ_lead2+MZ_lead1+MZ_lag0+MZ_lag1+MZ_lag2)/5) %>%
  ungroup()
Yield$MaizeAnom <- Yield$MAIZE.YIELD..Kg.per.ha. - Yield$rolling_average2
Yield$MaizePerAnom <- (Yield$MaizeAnom*100)/Yield$rolling_average2

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(SGK_lead2 = dplyr::lead(KHARIF.SORGHUM.YIELD..Kg.per.ha., n=2, order_by = Year),
         SGK_lead1 = dplyr::lead(KHARIF.SORGHUM.YIELD..Kg.per.ha., n=1, order_by = Year),
         SGK_lag0 = KHARIF.SORGHUM.YIELD..Kg.per.ha.,
         SGK_lag1 = dplyr::lag(KHARIF.SORGHUM.YIELD..Kg.per.ha., n=1, order_by = Year),
         SGK_lag2 = dplyr::lag(KHARIF.SORGHUM.YIELD..Kg.per.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average3 = (SGK_lead2+SGK_lead1+SGK_lag0+SGK_lag1+SGK_lag2)/5) %>%
  ungroup()
Yield$SorghumKharifAnom <- Yield$KHARIF.SORGHUM.YIELD..Kg.per.ha. - Yield$rolling_average3
Yield$SorghumPerAnom <- (Yield$SorghumKharifAnom*100)/Yield$rolling_average3

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(PM_lead2 = dplyr::lead(PEARL.MILLET.YIELD..Kg.per.ha., n=2, order_by = Year),
         PM_lead1 = dplyr::lead(PEARL.MILLET.YIELD..Kg.per.ha., n=1, order_by = Year),
         PM_lag0 = PEARL.MILLET.YIELD..Kg.per.ha.,
         PM_lag1 = dplyr::lag(PEARL.MILLET.YIELD..Kg.per.ha., n=1, order_by = Year),
         PM_lag2 = dplyr::lag(PEARL.MILLET.YIELD..Kg.per.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average4 = (PM_lead2+PM_lead1+PM_lag0+PM_lag1+PM_lag2)/5) %>%
  ungroup()
Yield$PearlMilletAnom <- Yield$PEARL.MILLET.YIELD..Kg.per.ha. - Yield$rolling_average4
Yield$PearlMilletPerAnom <- (Yield$PearlMilletAnom*100)/Yield$rolling_average4

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(FM_lead2 = dplyr::lead(FINGER.MILLET.YIELD..Kg.per.ha., n=2, order_by = Year),
         FM_lead1 = dplyr::lead(FINGER.MILLET.YIELD..Kg.per.ha., n=1, order_by = Year),
         FM_lag0 = FINGER.MILLET.YIELD..Kg.per.ha.,
         FM_lag1 = dplyr::lag(FINGER.MILLET.YIELD..Kg.per.ha., n=1, order_by = Year),
         FM_lag2 = dplyr::lag(FINGER.MILLET.YIELD..Kg.per.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average5 = (FM_lead2+FM_lead1+FM_lag0+FM_lag1+FM_lag2)/5) %>%
  ungroup()
Yield$FingerMilletAnom <- Yield$FINGER.MILLET.YIELD..Kg.per.ha. - Yield$rolling_average5
Yield$FingerMilletPerAnom <- (Yield$FingerMilletAnom*100)/Yield$rolling_average5

Yield$Yield_avg <- (Yield$RICE.YIELD..Kg.per.ha.+Yield$MAIZE.YIELD..Kg.per.ha.+Yield$KHARIF.SORGHUM.YIELD..Kg.per.ha.+   Yield$PEARL.MILLET.YIELD..Kg.per.ha.+Yield$FINGER.MILLET.YIELD..Kg.per.ha.)/5

Yield <- Yield %>%
  group_by(ID) %>%
  mutate(TY_lead2 = dplyr::lead(Yield_avg, n=2, order_by = Year),
         TY_lead1 = dplyr::lead(Yield_avg, n=1, order_by = Year),
         TY_lag0 = Yield_avg,
         TY_lag1 = dplyr::lag(Yield_avg, n=1, order_by = Year),
         TY_lag2 = dplyr::lag(Yield_avg, n=2, order_by = Year)) %>%
  mutate(rolling_average6 = (TY_lead2+TY_lead1+TY_lag0+TY_lag1+TY_lag2)/5) %>%
  ungroup()
Yield$YieldAnom <- Yield$Yield_avg - Yield$rolling_average6
Yield$YieldPerAnom <- (Yield$YieldAnom*100)/Yield$rolling_average6

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(RY_lead2 = dplyr::lead(RICE.PRODUCTION..1000.tons., n=2, order_by = Year),
         RY_lead1 = dplyr::lead(RICE.PRODUCTION..1000.tons., n=1, order_by = Year),
         RY_lag0 = RICE.PRODUCTION..1000.tons.,
         RY_lag1 = dplyr::lag(RICE.PRODUCTION..1000.tons., n=1, order_by = Year),
         RY_lag2 = dplyr::lag(RICE.PRODUCTION..1000.tons., n=2, order_by = Year)) %>%
  mutate(rolling_average1 = (RY_lead2+RY_lead1+RY_lag0+RY_lag1+RY_lag2)/5) %>%
  ungroup()
Prod$RiceAnom <- Prod$RICE.PRODUCTION..1000.tons. - Prod$rolling_average1
Prod$RicePerAnom <- (Prod$RiceAnom*100)/Prod$rolling_average1

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(MZ_lead2 = dplyr::lead(MAIZE.PRODUCTION..1000.tons., n=2, order_by = Year),
         MZ_lead1 = dplyr::lead(MAIZE.PRODUCTION..1000.tons., n=1, order_by = Year),
         MZ_lag0 = MAIZE.PRODUCTION..1000.tons.,
         MZ_lag1 = dplyr::lag(MAIZE.PRODUCTION..1000.tons., n=1, order_by = Year),
         MZ_lag2 = dplyr::lag(MAIZE.PRODUCTION..1000.tons., n=2, order_by = Year)) %>%
  mutate(rolling_average2 = (MZ_lead2+MZ_lead1+MZ_lag0+MZ_lag1+MZ_lag2)/5) %>%
  ungroup()
Prod$MaizeAnom <- Prod$MAIZE.PRODUCTION..1000.tons. - Prod$rolling_average2
Prod$MaizePerAnom <- (Prod$MaizeAnom*100)/Prod$rolling_average2

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(SGK_lead2 = dplyr::lead(KHARIF.SORGHUM.PRODUCTION..1000.tons., n=2, order_by = Year),
         SGK_lead1 = dplyr::lead(KHARIF.SORGHUM.PRODUCTION..1000.tons., n=1, order_by = Year),
         SGK_lag0 = KHARIF.SORGHUM.PRODUCTION..1000.tons.,
         SGK_lag1 = dplyr::lag(KHARIF.SORGHUM.PRODUCTION..1000.tons., n=1, order_by = Year),
         SGK_lag2 = dplyr::lag(KHARIF.SORGHUM.PRODUCTION..1000.tons., n=2, order_by = Year)) %>%
  mutate(rolling_average3 = (SGK_lead2+SGK_lead1+SGK_lag0+SGK_lag1+SGK_lag2)/5) %>%
  ungroup()
Prod$SorghumKharifAnom <- Prod$KHARIF.SORGHUM.PRODUCTION..1000.tons. - Prod$rolling_average3
Prod$SorghumPerAnom <- (Prod$SorghumKharifAnom*100)/Prod$rolling_average3

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(PM_lead2 = dplyr::lead(PEARL.MILLET.PRODUCTION..1000.tons., n=2, order_by = Year),
         PM_lead1 = dplyr::lead(PEARL.MILLET.PRODUCTION..1000.tons., n=1, order_by = Year),
         PM_lag0 = PEARL.MILLET.PRODUCTION..1000.tons.,
         PM_lag1 = dplyr::lag(PEARL.MILLET.PRODUCTION..1000.tons., n=1, order_by = Year),
         PM_lag2 = dplyr::lag(PEARL.MILLET.PRODUCTION..1000.tons., n=2, order_by = Year)) %>%
  mutate(rolling_average4 = (PM_lead2+PM_lead1+PM_lag0+PM_lag1+PM_lag2)/5) %>%
  ungroup()
Prod$PearlMilletAnom <- Prod$PEARL.MILLET.PRODUCTION..1000.tons. - Prod$rolling_average4
Prod$PearlMilletPerAnom <- (Prod$PearlMilletAnom*100)/Prod$rolling_average4

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(FM_lead2 = dplyr::lead(FINGER.MILLET.PRODUCTION..1000.tons., n=2, order_by = Year),
         FM_lead1 = dplyr::lead(FINGER.MILLET.PRODUCTION..1000.tons., n=1, order_by = Year),
         FM_lag0 = FINGER.MILLET.PRODUCTION..1000.tons.,
         FM_lag1 = dplyr::lag(FINGER.MILLET.PRODUCTION..1000.tons., n=1, order_by = Year),
         FM_lag2 = dplyr::lag(FINGER.MILLET.PRODUCTION..1000.tons., n=2, order_by = Year)) %>%
  mutate(rolling_average5 = (FM_lead2+FM_lead1+FM_lag0+FM_lag1+FM_lag2)/5) %>%
  ungroup()
Prod$FingerMilletAnom <- Prod$FINGER.MILLET.PRODUCTION..1000.tons. - Prod$rolling_average5
Prod$FingerMilletPerAnom <- (Prod$FingerMilletAnom*100)/Prod$rolling_average5

Prod$Prod_total <- Prod$RICE.PRODUCTION..1000.tons. + Prod$MAIZE.PRODUCTION..1000.tons. + Prod$KHARIF.SORGHUM.PRODUCTION..1000.tons. + Prod$PEARL.MILLET.PRODUCTION..1000.tons. + Prod$FINGER.MILLET.PRODUCTION..1000.tons.

Prod <- Prod %>%
  group_by(ID) %>%
  mutate(TP_lead2 = dplyr::lead(Prod_total, n=2, order_by = Year),
         TP_lead1 = dplyr::lead(Prod_total, n=1, order_by = Year),
         TP_lag0 = Prod_total,
         TP_lag1 = dplyr::lag(Prod_total, n=1, order_by = Year),
         TP_lag2 = dplyr::lag(Prod_total, n=2, order_by = Year)) %>%
  mutate(rolling_average6 = (TP_lead2+TP_lead1+TP_lag0+TP_lag1+TP_lag2)/5) %>%
  ungroup()
Prod$ProdAnom <- Prod$Prod_total - Prod$rolling_average6
Prod$ProdPerAnom <- (Prod$ProdAnom*100)/Prod$rolling_average6

Area <- Area %>%
  group_by(ID) %>%
  mutate(RY_lead2 = dplyr::lead(RICE.AREA..1000.ha., n=2, order_by = Year),
         RY_lead1 = dplyr::lead(RICE.AREA..1000.ha., n=1, order_by = Year),
         RY_lag0 = RICE.AREA..1000.ha.,
         RY_lag1 = dplyr::lag(RICE.AREA..1000.ha., n=1, order_by = Year),
         RY_lag2 = dplyr::lag(RICE.AREA..1000.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average1 = (RY_lead2+RY_lead1+RY_lag0+RY_lag1+RY_lag2)/5) %>%
  ungroup()
Area$RiceAnom <- Area$RICE.AREA..1000.ha. - Area$rolling_average1
Area$RicePerAnom <- (Area$RiceAnom*100)/Area$rolling_average1

Area <- Area %>%
  group_by(ID) %>%
  mutate(MZ_lead2 = dplyr::lead(MAIZE.AREA..1000.ha., n=2, order_by = Year),
         MZ_lead1 = dplyr::lead(MAIZE.AREA..1000.ha., n=1, order_by = Year),
         MZ_lag0 = MAIZE.AREA..1000.ha.,
         MZ_lag1 = dplyr::lag(MAIZE.AREA..1000.ha., n=1, order_by = Year),
         MZ_lag2 = dplyr::lag(MAIZE.AREA..1000.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average2 = (MZ_lead2+MZ_lead1+MZ_lag0+MZ_lag1+MZ_lag2)/5) %>%
  ungroup()
Area$MaizeAnom <- Area$MAIZE.AREA..1000.ha. - Area$rolling_average2
Area$MaizePerAnom <- (Area$MaizeAnom*100)/Area$rolling_average2

Area <- Area %>%
  group_by(ID) %>%
  mutate(SGK_lead2 = dplyr::lead(KHARIF.SORGHUM.AREA..1000.ha., n=2, order_by = Year),
         SGK_lead1 = dplyr::lead(KHARIF.SORGHUM.AREA..1000.ha., n=1, order_by = Year),
         SGK_lag0 = KHARIF.SORGHUM.AREA..1000.ha.,
         SGK_lag1 = dplyr::lag(KHARIF.SORGHUM.AREA..1000.ha., n=1, order_by = Year),
         SGK_lag2 = dplyr::lag(KHARIF.SORGHUM.AREA..1000.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average3 = (SGK_lead2+SGK_lead1+SGK_lag0+SGK_lag1+SGK_lag2)/5) %>%
  ungroup()
Area$SorghumKharifAnom <- Area$KHARIF.SORGHUM.AREA..1000.ha. - Area$rolling_average3
Area$SorghumPerAnom <- (Area$SorghumKharifAnom*100)/Area$rolling_average3

Area <- Area %>%
  group_by(ID) %>%
  mutate(PM_lead2 = dplyr::lead(PEARL.MILLET.AREA..1000.ha., n=2, order_by = Year),
         PM_lead1 = dplyr::lead(PEARL.MILLET.AREA..1000.ha., n=1, order_by = Year),
         PM_lag0 = PEARL.MILLET.AREA..1000.ha.,
         PM_lag1 = dplyr::lag(PEARL.MILLET.AREA..1000.ha., n=1, order_by = Year),
         PM_lag2 = dplyr::lag(PEARL.MILLET.AREA..1000.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average4 = (PM_lead2+PM_lead1+PM_lag0+PM_lag1+PM_lag2)/5) %>%
  ungroup()
Area$PearlMilletAnom <- Area$PEARL.MILLET.AREA..1000.ha. - Area$rolling_average4
Area$PearlMilletPerAnom <- (Area$PearlMilletAnom*100)/Area$rolling_average4

Area <- Area %>%
  group_by(ID) %>%
  mutate(FM_lead2 = dplyr::lead(FINGER.MILLET.AREA..1000.ha., n=2, order_by = Year),
         FM_lead1 = dplyr::lead(FINGER.MILLET.AREA..1000.ha., n=1, order_by = Year),
         FM_lag0 = FINGER.MILLET.AREA..1000.ha.,
         FM_lag1 = dplyr::lag(FINGER.MILLET.AREA..1000.ha., n=1, order_by = Year),
         FM_lag2 = dplyr::lag(FINGER.MILLET.AREA..1000.ha., n=2, order_by = Year)) %>%
  mutate(rolling_average5 = (FM_lead2+FM_lead1+FM_lag0+FM_lag1+FM_lag2)/5) %>%
  ungroup()
Area$FingerMilletAnom <- Area$FINGER.MILLET.AREA..1000.ha. - Area$rolling_average5
Area$FingerMilletPerAnom <- (Area$FingerMilletAnom*100)/Area$rolling_average5

Area$Area_total <- Area$RICE.AREA..1000.ha. + Area$MAIZE.AREA..1000.ha. + Area$KHARIF.SORGHUM.AREA..1000.ha. + Area$PEARL.MILLET.AREA..1000.ha. + Area$FINGER.MILLET.AREA..1000.ha.

Area <- Area %>%
  group_by(ID) %>%
  mutate(TA_lead2 = dplyr::lead(Area_total, n=2, order_by = Year),
         TA_lead1 = dplyr::lead(Area_total, n=1, order_by = Year),
         TA_lag0 = Area_total,
         TA_lag1 = dplyr::lag(Area_total, n=1, order_by = Year),
         TA_lag2 = dplyr::lag(Area_total, n=2, order_by = Year)) %>%
  mutate(rolling_average6 = (TA_lead2+TA_lead1+TA_lag0+TA_lag1+TA_lag2)/5) %>%
  ungroup()
Area$AreaAnom <- Area$Area_total - Area$rolling_average6
Area$AreaPerAnom <- (Area$AreaAnom*100)/Area$rolling_average6

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(RY_lead2 = dplyr::lead(RICE.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year),
         RY_lead1 = dplyr::lead(RICE.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         RY_lag0 = RICE.IRRIGATED.AREA..1000.ha.,
         RY_lag1 = dplyr::lag(RICE.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         RY_lag2 = dplyr::lag(RICE.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year)) %>%
  mutate(rolling_average1 = (RY_lead2 + RY_lead1 + RY_lag0 + RY_lag1 + RY_lag2) / 5) %>%
  ungroup()
Harv_Irr$RiceIrrAnom <- Harv_Irr$RICE.IRRIGATED.AREA..1000.ha. - Harv_Irr$rolling_average1
Harv_Irr$RiceIrrPerAnom <- (Harv_Irr$RiceIrrAnom * 100) / Harv_Irr$rolling_average1
Harv_Irr$RiceIrrPerAnom <- ifelse(Harv_Irr$rolling_average1 != 0, (Harv_Irr$RiceIrrAnom * 100) / Harv_Irr$rolling_average1, NA)

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(MZ_lead2 = dplyr::lead(MAIZE.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year),
         MZ_lead1 = dplyr::lead(MAIZE.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         MZ_lag0 = MAIZE.IRRIGATED.AREA..1000.ha.,
         MZ_lag1 = dplyr::lag(MAIZE.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         MZ_lag2 = dplyr::lag(MAIZE.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year)) %>%
  mutate(rolling_average2 = (MZ_lead2 + MZ_lead1 + MZ_lag0 + MZ_lag1 + MZ_lag2) / 5) %>%
  ungroup()
Harv_Irr$MaizeIrrAnom <- Harv_Irr$MAIZE.IRRIGATED.AREA..1000.ha. - Harv_Irr$rolling_average2
Harv_Irr$MaizeIrrPerAnom <- (Harv_Irr$MaizeIrrAnom * 100) / Harv_Irr$rolling_average2
Harv_Irr$MaizeIrrPerAnom <- ifelse(Harv_Irr$rolling_average2 != 0, (Harv_Irr$MaizeIrrAnom * 100) / Harv_Irr$rolling_average2, NA)

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(SGK_lead2 = dplyr::lead(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year),
         SGK_lead1 = dplyr::lead(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         SGK_lag0 = KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.,
         SGK_lag1 = dplyr::lag(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         SGK_lag2 = dplyr::lag(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year)) %>%
  mutate(rolling_average3 = (SGK_lead2 + SGK_lead1 + SGK_lag0 + SGK_lag1 + SGK_lag2) / 5) %>%
  ungroup()
Harv_Irr$SorghumIrrAnom <- Harv_Irr$KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. - Harv_Irr$rolling_average3
Harv_Irr$SorghumIrrPerAnom <- (Harv_Irr$SorghumIrrAnom * 100) / Harv_Irr$rolling_average3
Harv_Irr$SorghumIrrPerAnom <- ifelse(Harv_Irr$rolling_average3 != 0, (Harv_Irr$SorghumIrrAnom * 100) / Harv_Irr$rolling_average3, NA)

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(PM_lead2 = dplyr::lead(PEARL.MILLET.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year),
         PM_lead1 = dplyr::lead(PEARL.MILLET.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         PM_lag0 = PEARL.MILLET.IRRIGATED.AREA..1000.ha.,
         PM_lag1 = dplyr::lag(PEARL.MILLET.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         PM_lag2 = dplyr::lag(PEARL.MILLET.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year)) %>%
  mutate(rolling_average4 = (PM_lead2 + PM_lead1 + PM_lag0 + PM_lag1 + PM_lag2) / 5) %>%
  ungroup()
Harv_Irr$PearlMilletIrrAnom <- Harv_Irr$PEARL.MILLET.IRRIGATED.AREA..1000.ha. - Harv_Irr$rolling_average4
Harv_Irr$PearlMilletIrrPerAnom <- (Harv_Irr$PearlMilletIrrAnom * 100) / Harv_Irr$rolling_average4
Harv_Irr$PearlMilletIrrPerAnom <- ifelse(Harv_Irr$rolling_average4 != 0, (Harv_Irr$PearlMilletIrrAnom * 100) / Harv_Irr$rolling_average4, NA)

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(FM_lead2 = dplyr::lead(FINGER.MILLET.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year),
         FM_lead1 = dplyr::lead(FINGER.MILLET.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         FM_lag0 = FINGER.MILLET.IRRIGATED.AREA..1000.ha.,
         FM_lag1 = dplyr::lag(FINGER.MILLET.IRRIGATED.AREA..1000.ha., n = 1, order_by = Year),
         FM_lag2 = dplyr::lag(FINGER.MILLET.IRRIGATED.AREA..1000.ha., n = 2, order_by = Year)) %>%
  mutate(rolling_average5 = (FM_lead2 + FM_lead1 + FM_lag0 + FM_lag1 + FM_lag2) / 5) %>%
  ungroup()
Harv_Irr$FingerMilletIrrAnom <- Harv_Irr$FINGER.MILLET.IRRIGATED.AREA..1000.ha. - Harv_Irr$rolling_average5
Harv_Irr$FingerMilletIrrPerAnom <- (Harv_Irr$FingerMilletIrrAnom * 100) / Harv_Irr$rolling_average5
Harv_Irr$FingerMilletIrrPerAnom <- ifelse(Harv_Irr$rolling_average5 != 0, (Harv_Irr$FingerMilletIrrAnom * 100) / Harv_Irr$rolling_average5, NA)

Harv_Irr$IrrArea_total <- Harv_Irr$RICE.IRRIGATED.AREA..1000.ha. + Harv_Irr$MAIZE.IRRIGATED.AREA..1000.ha. + Harv_Irr$KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. + Harv_Irr$PEARL.MILLET.IRRIGATED.AREA..1000.ha. + Harv_Irr$FINGER.MILLET.IRRIGATED.AREA..1000.ha.

Harv_Irr <- Harv_Irr %>%
  group_by(ID) %>%
  mutate(TA_lead2 = dplyr::lead(IrrArea_total, n=2, order_by = Year),
         TA_lead1 = dplyr::lead(IrrArea_total, n=1, order_by = Year),
         TA_lag0 = IrrArea_total,
         TA_lag1 = dplyr::lag(IrrArea_total, n=1, order_by = Year),
         TA_lag2 = dplyr::lag(IrrArea_total, n=2, order_by = Year)) %>%
  mutate(rolling_average6 = (TA_lead2+TA_lead1+TA_lag0+TA_lag1+TA_lag2)/5) %>%
  ungroup()
Harv_Irr$IrrAreaAnom <- Harv_Irr$IrrArea_total - Harv_Irr$rolling_average6
Harv_Irr$IrrAreaPerAnom <- (Harv_Irr$IrrAreaAnom*100)/Harv_Irr$rolling_average6

Area1 <- Area %>% dplyr::select(1:11)

# Weight the absolute and relative anomalies and aggregate it at the national level

Yield <- merge(Yield, Area1, by=c("Year", "State.Code", "Dist.Code", "State.Name", "Dist.Name", "ID"))
Yield$Overall_area <- Yield$RICE.AREA..1000.ha.+Yield$MAIZE.AREA..1000.ha.+Yield$KHARIF.SORGHUM.AREA..1000.ha.+Yield$PEARL.MILLET.AREA..1000.ha.+Yield$FINGER.MILLET.AREA..1000.ha.

# Yield 

# 1. Calculation of Total Harvested Areas

RI_tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(RI_tot_har_area = sum(RICE.AREA..1000.ha., na.rm = TRUE))
MZ_tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(MZ_tot_har_area = sum(MAIZE.AREA..1000.ha., na.rm = TRUE))
SG_tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(SG_tot_har_area = sum(KHARIF.SORGHUM.AREA..1000.ha., na.rm = TRUE))
PM_tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(PM_tot_har_area = sum(PEARL.MILLET.AREA..1000.ha., na.rm = TRUE))
FM_tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(FM_tot_har_area = sum(FINGER.MILLET.AREA..1000.ha., na.rm = TRUE))
tot_har_area <- Yield %>%
  group_by(Year) %>%
  summarize(tot_har_area = sum(Overall_area, na.rm = TRUE))

crop_tot_har_area <- list(RI_tot_har_area, MZ_tot_har_area, SG_tot_har_area, PM_tot_har_area, FM_tot_har_area, tot_har_area)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}
Tot_har_area <- Reduce(merge_fun, crop_tot_har_area)
Yield <- merge(Yield, Tot_har_area, by = "Year")

# 2. Multiply Absolute and Relative Anomalies with Area

Yield <- Yield %>%
  dplyr::mutate(WEIGHTED.RICE.YIELD..Kg.per.ha. = RICE.YIELD..Kg.per.ha. * RICE.AREA..1000.ha.,
                WEIGHTED.MAIZE.YIELD..Kg.per.ha. = MAIZE.YIELD..Kg.per.ha. * MAIZE.AREA..1000.ha.,
                WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha. = KHARIF.SORGHUM.YIELD..Kg.per.ha. * KHARIF.SORGHUM.AREA..1000.ha.,
                WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha. = PEARL.MILLET.YIELD..Kg.per.ha. * PEARL.MILLET.AREA..1000.ha.,
                WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha. = FINGER.MILLET.YIELD..Kg.per.ha. * FINGER.MILLET.AREA..1000.ha.,
                WEIGHTED.YIELD..Kg.per.ha. = Yield_avg * Overall_area,
                wRiceAnom = RiceAnom * RICE.AREA..1000.ha.,
                wRicePerAnom = RicePerAnom * RICE.AREA..1000.ha.,
                wMaizeAnom = MaizeAnom * MAIZE.AREA..1000.ha.,
                wMaizePerAnom = MaizePerAnom * MAIZE.AREA..1000.ha.,
                wSorghumAnom = SorghumKharifAnom * KHARIF.SORGHUM.AREA..1000.ha.,
                wSorghumPerAnom = SorghumPerAnom * KHARIF.SORGHUM.AREA..1000.ha.,
                wPearlMilletAnom = PearlMilletAnom * PEARL.MILLET.AREA..1000.ha.,
                wPearlMilletPerAnom = PearlMilletPerAnom * PEARL.MILLET.AREA..1000.ha.,
                wFingerMilletAnom = FingerMilletAnom * FINGER.MILLET.AREA..1000.ha.,
                wFingerMilletPerAnom = FingerMilletPerAnom * FINGER.MILLET.AREA..1000.ha.,
                wYieldAnom = YieldAnom * Overall_area,
                wYieldPerAnom = YieldPerAnom * Overall_area)

# 3. Calculate the weighted average absolute and relative anomalies using the formula: https://www.worldpop.org/methods/pwd/

Yield_Rice <- subset(Yield, select = c('Year', 'Dist.Code', 'RICE.YIELD..Kg.per.ha.', 'RICE.AREA..1000.ha.', 'RI_tot_har_area', 'WEIGHTED.RICE.YIELD..Kg.per.ha.', 'RiceAnom', 'wRiceAnom', 'RicePerAnom', 'wRicePerAnom'))
Yield_Maize <- subset(Yield, select = c('Year', 'Dist.Code', 'MAIZE.YIELD..Kg.per.ha.', 'MAIZE.AREA..1000.ha.', 'MZ_tot_har_area', 'WEIGHTED.MAIZE.YIELD..Kg.per.ha.', 'MaizeAnom', 'wMaizeAnom', 'MaizePerAnom', 'wMaizePerAnom'))
Yield_Sorghum <- subset(Yield, select = c('Year', 'Dist.Code', 'KHARIF.SORGHUM.YIELD..Kg.per.ha.', 'KHARIF.SORGHUM.AREA..1000.ha.', 'SG_tot_har_area', 'WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha.', 'SorghumKharifAnom', 'wSorghumAnom', 'SorghumPerAnom', 'wSorghumPerAnom'))
Yield_PearlMillet <- subset(Yield, select = c('Year', 'Dist.Code', 'PEARL.MILLET.YIELD..Kg.per.ha.', 'PEARL.MILLET.AREA..1000.ha.', 'PM_tot_har_area', 'WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha.', 'PearlMilletAnom', 'wPearlMilletAnom', 'PearlMilletPerAnom', 'wPearlMilletPerAnom'))
Yield_FingerMillet <- subset(Yield, select = c('Year', 'Dist.Code', 'FINGER.MILLET.YIELD..Kg.per.ha.', 'FINGER.MILLET.AREA..1000.ha.', 'FM_tot_har_area', 'WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha.', 'FingerMilletAnom', 'wFingerMilletAnom', 'FingerMilletPerAnom', 'wFingerMilletPerAnom'))
Yield_Avg <- subset(Yield, select = c('Year', 'Dist.Code', 'Yield_avg', 'Overall_area', 'tot_har_area', 'WEIGHTED.YIELD..Kg.per.ha.', 'YieldAnom', 'wYieldAnom', 'YieldPerAnom', 'wYieldPerAnom'))

Rice_aggregated_data <- Yield_Rice %>%
  group_by(Year) %>%
  summarize(Average_RICE.YIELD = mean(RICE.YIELD..Kg.per.ha., na.rm = TRUE), WEIGHTED_RICE_YIELD_Kg.per.ha = sum(WEIGHTED.RICE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(RI_tot_har_area), Avg_RiceAnom = mean(RiceAnom, na.rm = TRUE), Avg_RicePerAnom = mean(RicePerAnom, na.rm = TRUE), wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area))

Maize_aggregated_data <- Yield_Maize %>%
  group_by(Year) %>%
  summarize(Average_MAIZE.YIELD = mean(MAIZE.YIELD..Kg.per.ha., na.rm = TRUE), WEIGHTED_MAIZE_YIELD_Kg.per.ha = sum(WEIGHTED.MAIZE.YIELD..Kg.per.ha., na.rm=TRUE)/mean(MZ_tot_har_area), Avg_MaizeAnom = mean(MaizeAnom, na.rm = TRUE), Avg_MaizePerAnom = mean(MaizePerAnom, na.rm = TRUE), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area))

Sorghum_aggregated_data <- Yield_Sorghum %>%
  group_by(Year) %>%
  summarize(Average_SORGHUM.YIELD = mean(KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm = TRUE), WEIGHTED_SORGHUM_YIELD_Kg.per.ha = sum(WEIGHTED.KHARIF.SORGHUM.YIELD..Kg.per.ha., na.rm=TRUE)/mean(SG_tot_har_area), Avg_SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE), Avg_SorghumPerAnom = mean(SorghumPerAnom, na.rm = TRUE), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area))

PearlMillet_aggregated_data <- Yield_PearlMillet %>%
  group_by(Year) %>%
  summarize(Average_PEARLMILLET.YIELD = mean(PEARL.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), WEIGHTED_PEARLMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.PEARL.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(PM_tot_har_area), Avg_PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE), Avg_PearlMilletPerAnom = mean(PearlMilletPerAnom, na.rm = TRUE), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area))

FingerMillet_aggregated_data <- Yield_FingerMillet %>%
  group_by(Year) %>%
  summarize(Average_FINGERMILLET.YIELD = mean(FINGER.MILLET.YIELD..Kg.per.ha., na.rm = TRUE), WEIGHTED_FINGERMILLET_YIELD_Kg.per.ha = sum(WEIGHTED.FINGER.MILLET.YIELD..Kg.per.ha., na.rm=TRUE)/mean(FM_tot_har_area), Avg_FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE), Avg_FingerMilletPerAnom = mean(FingerMilletPerAnom, na.rm = TRUE), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area))

aggregated_data <- Yield_Avg %>%
  group_by(Year) %>%
  summarize(Average.YIELD = mean(Yield_avg, na.rm = TRUE), WEIGHTED.YIELD..Kg.per.ha. = sum(WEIGHTED.YIELD..Kg.per.ha., na.rm=TRUE)/mean(tot_har_area), Avg_YieldAnom = mean(YieldAnom, na.rm = TRUE), Avg_YieldPerAnom = mean(YieldPerAnom, na.rm = TRUE), wYieldAnom = sum(wYieldAnom, na.rm=TRUE)/mean(tot_har_area), wYieldPerAnom = sum(wYieldPerAnom, na.rm=TRUE)/mean(tot_har_area))

crop_aggregated_data <- list(Rice_aggregated_data, Maize_aggregated_data, Sorghum_aggregated_data, PearlMillet_aggregated_data, FingerMillet_aggregated_data, aggregated_data)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Crop_yield_aggregated_data <- Reduce(merge_fun, crop_aggregated_data)

# Production

# 1. Calculation of Total Harvested Areas

RI_tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(RI_tot_prod = sum(RICE.PRODUCTION..1000.tons., na.rm = TRUE))
MZ_tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(MZ_tot_prod = sum(MAIZE.PRODUCTION..1000.tons., na.rm = TRUE))
SG_tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(SG_tot_prod = sum(KHARIF.SORGHUM.PRODUCTION..1000.tons., na.rm = TRUE))
PM_tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(PM_tot_prod = sum(PEARL.MILLET.PRODUCTION..1000.tons., na.rm = TRUE))
FM_tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(FM_tot_prod = sum(FINGER.MILLET.PRODUCTION..1000.tons., na.rm = TRUE))
tot_prod <- Prod %>%
  group_by(Year) %>%
  summarize(tot_prod = sum(Prod_total, na.rm = TRUE))

crop_tot_prod <- list(RI_tot_prod, MZ_tot_prod, SG_tot_prod, PM_tot_prod, FM_tot_prod, tot_prod)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Tot_prod <- Reduce(merge_fun, crop_tot_prod)
Prod <- merge(Prod, Tot_prod, by = "Year")

# 2. Multiply Absolute and Relative Anomalies with Area

Prod <- Prod %>%
  mutate(wRiceAnom = RiceAnom * RICE.PRODUCTION..1000.tons.,
         wRicePerAnom = RicePerAnom * RICE.PRODUCTION..1000.tons.,
         wMaizeAnom = MaizeAnom * MAIZE.PRODUCTION..1000.tons.,
         wMaizePerAnom = MaizePerAnom * MAIZE.PRODUCTION..1000.tons.,
         wSorghumAnom = SorghumKharifAnom * KHARIF.SORGHUM.PRODUCTION..1000.tons.,
         wSorghumPerAnom = SorghumPerAnom * KHARIF.SORGHUM.PRODUCTION..1000.tons.,
         wPearlMilletAnom = PearlMilletAnom * PEARL.MILLET.PRODUCTION..1000.tons.,
         wPearlMilletPerAnom = PearlMilletPerAnom * PEARL.MILLET.PRODUCTION..1000.tons.,
         wFingerMilletAnom = FingerMilletAnom * FINGER.MILLET.PRODUCTION..1000.tons.,
         wFingerMilletPerAnom = FingerMilletPerAnom * FINGER.MILLET.PRODUCTION..1000.tons.,
         wProdAnom = ProdAnom * Prod_total,
         wProdPerAnom = ProdPerAnom * Prod_total)

# 3. Calculate the weighted average absolute and relative anomalies using the formula: https://www.worldpop.org/methods/pwd/
  
Prod_Rice <- subset(Prod, select = c('Year', 'Dist.Code', 'RICE.PRODUCTION..1000.tons.', 'RI_tot_prod', 'RiceAnom', 'wRiceAnom', 'RicePerAnom', 'wRicePerAnom'))
Prod_Maize <- subset(Prod, select = c('Year', 'Dist.Code', 'MAIZE.PRODUCTION..1000.tons.', 'MZ_tot_prod', 'MaizeAnom', 'wMaizeAnom', 'MaizePerAnom', 'wMaizePerAnom'))
Prod_Sorghum <- subset(Prod, select = c('Year', 'Dist.Code', 'KHARIF.SORGHUM.PRODUCTION..1000.tons.', 'SG_tot_prod', 'SorghumKharifAnom', 'wSorghumAnom', 'SorghumPerAnom', 'wSorghumPerAnom'))
Prod_PearlMillet <- subset(Prod, select = c('Year', 'Dist.Code', 'PEARL.MILLET.PRODUCTION..1000.tons.', 'PM_tot_prod', 'PearlMilletAnom', 'wPearlMilletAnom', 'PearlMilletPerAnom', 'wPearlMilletPerAnom'))
Prod_FingerMillet <- subset(Prod, select = c('Year', 'Dist.Code', 'FINGER.MILLET.PRODUCTION..1000.tons.', 'FM_tot_prod', 'FingerMilletAnom', 'wFingerMilletAnom', 'FingerMilletPerAnom', 'wFingerMilletPerAnom'))
Prod_Total <- subset(Prod, select = c('Year', 'Dist.Code', 'Prod_total', 'tot_prod', 'ProdAnom', 'wProdAnom', 'ProdPerAnom', 'wProdPerAnom'))

Rice_aggregated_data <- Prod_Rice %>%
  group_by(Year) %>%
  summarize(Avg_RiceAnom = mean(RiceAnom, na.rm = TRUE), Avg_RicePerAnom = mean(RicePerAnom, na.rm = TRUE), wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_prod), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_prod))

Maize_aggregated_data <- Prod_Maize %>%
  group_by(Year) %>%
  summarize(Avg_MaizeAnom = mean(MaizeAnom, na.rm = TRUE), Avg_MaizePerAnom = mean(MaizePerAnom, na.rm = TRUE), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_prod), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_prod))

Sorghum_aggregated_data <- Prod_Sorghum %>%
  group_by(Year) %>%
  summarize(Avg_SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE), Avg_SorghumPerAnom = mean(SorghumPerAnom, na.rm = TRUE), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_prod), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_prod))

PearlMillet_aggregated_data <- Prod_PearlMillet %>%
  group_by(Year) %>%
  summarize(Avg_PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE), Avg_PearlMilletPerAnom = mean(PearlMilletPerAnom, na.rm = TRUE), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_prod), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_prod))

FingerMillet_aggregated_data <- Prod_FingerMillet %>%
  group_by(Year) %>%
  summarize(Avg_FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE), Avg_FingerMilletPerAnom = mean(FingerMilletPerAnom, na.rm = TRUE), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_prod), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_prod))

aggregated_data <- Prod_Total %>%
  group_by(Year) %>%
  summarize(Avg_ProdAnom = mean(ProdAnom, na.rm = TRUE), Avg_ProdPerAnom = mean(ProdPerAnom, na.rm = TRUE), wProdAnom = sum(wProdAnom, na.rm=TRUE)/mean(tot_prod), wProdPerAnom = sum(wProdPerAnom, na.rm=TRUE)/mean(tot_prod))

crop_aggregated_data <- list(Rice_aggregated_data, Maize_aggregated_data, Sorghum_aggregated_data, PearlMillet_aggregated_data, FingerMillet_aggregated_data, aggregated_data)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Crop_prod_aggregated_data <- Reduce(merge_fun, crop_aggregated_data)

# Area

# 1. Calculation of Total Harvested Areas

RI_tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(RI_tot_har_area = sum(RICE.AREA..1000.ha., na.rm = TRUE))
MZ_tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(MZ_tot_har_area = sum(MAIZE.AREA..1000.ha., na.rm = TRUE))
SG_tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(SG_tot_har_area = sum(KHARIF.SORGHUM.AREA..1000.ha., na.rm = TRUE))
PM_tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(PM_tot_har_area = sum(PEARL.MILLET.AREA..1000.ha., na.rm = TRUE))
FM_tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(FM_tot_har_area = sum(FINGER.MILLET.AREA..1000.ha., na.rm = TRUE))
tot_har_area <- Area %>%
  group_by(Year) %>%
  summarize(tot_har_area = sum(Area_total, na.rm = TRUE))

crop_tot_har_area <- list(RI_tot_har_area, MZ_tot_har_area, SG_tot_har_area, PM_tot_har_area, FM_tot_har_area, tot_har_area)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Tot_har_area <- Reduce(merge_fun, crop_tot_har_area)
Area <- merge(Area, Tot_har_area, by = "Year")

# 2. Multiply Absolute and Relative Anomalies with Area

Area <- Area %>%
  mutate(wRiceAnom = RiceAnom * RICE.AREA..1000.ha.,
         wRicePerAnom = RicePerAnom * RICE.AREA..1000.ha.,
         wMaizeAnom = MaizeAnom * MAIZE.AREA..1000.ha.,
         wMaizePerAnom = MaizePerAnom * MAIZE.AREA..1000.ha.,
         wSorghumAnom = SorghumKharifAnom * KHARIF.SORGHUM.AREA..1000.ha.,
         wSorghumPerAnom = SorghumPerAnom * KHARIF.SORGHUM.AREA..1000.ha.,
         wPearlMilletAnom = PearlMilletAnom * PEARL.MILLET.AREA..1000.ha.,
         wPearlMilletPerAnom = PearlMilletPerAnom * PEARL.MILLET.AREA..1000.ha.,
         wFingerMilletAnom = FingerMilletAnom * FINGER.MILLET.AREA..1000.ha.,
         wFingerMilletPerAnom = FingerMilletPerAnom * FINGER.MILLET.AREA..1000.ha.,
         wAreaAnom = AreaAnom * Area_total,
         wAreaPerAnom = AreaPerAnom * Area_total)

# 3. Calculate the weighted average absolute and relative anomalies using the formula: https://www.worldpop.org/methods/pwd/
  
Area_Rice <- subset(Area, select = c('Year', 'Dist.Code', 'RICE.AREA..1000.ha.', 'RI_tot_har_area', 'RiceAnom', 'wRiceAnom', 'RicePerAnom', 'wRicePerAnom'))
Area_Maize <- subset(Area, select = c('Year', 'Dist.Code', 'MAIZE.AREA..1000.ha.', 'MZ_tot_har_area', 'MaizeAnom', 'wMaizeAnom', 'MaizePerAnom', 'wMaizePerAnom'))
Area_Sorghum <- subset(Area, select = c('Year', 'Dist.Code', 'KHARIF.SORGHUM.AREA..1000.ha.', 'SG_tot_har_area', 'SorghumKharifAnom', 'wSorghumAnom', 'SorghumPerAnom', 'wSorghumPerAnom'))
Area_PearlMillet <- subset(Area, select = c('Year', 'Dist.Code', 'PEARL.MILLET.AREA..1000.ha.', 'PM_tot_har_area', 'PearlMilletAnom', 'wPearlMilletAnom', 'PearlMilletPerAnom', 'wPearlMilletPerAnom'))
Area_FingerMillet <- subset(Area, select = c('Year', 'Dist.Code', 'FINGER.MILLET.AREA..1000.ha.', 'FM_tot_har_area', 'FingerMilletAnom', 'wFingerMilletAnom', 'FingerMilletPerAnom', 'wFingerMilletPerAnom'))
Area_Total <- subset(Area, select = c('Year', 'Dist.Code', 'Area_total', 'tot_har_area', 'AreaAnom', 'wAreaAnom', 'AreaPerAnom', 'wAreaPerAnom'))

Rice_aggregated_data <- Area_Rice %>%
  group_by(Year) %>%
  summarize(Avg_RiceAnom = mean(RiceAnom, na.rm = TRUE), Avg_RicePerAnom = mean(RicePerAnom, na.rm = TRUE), wRiceAnom = sum(wRiceAnom, na.rm=TRUE)/mean(RI_tot_har_area), wRicePerAnom = sum(wRicePerAnom, na.rm=TRUE)/mean(RI_tot_har_area))

Maize_aggregated_data <- Area_Maize %>%
  group_by(Year) %>%
  summarize(Avg_MaizeAnom = mean(MaizeAnom, na.rm = TRUE), Avg_MaizePerAnom = mean(MaizePerAnom, na.rm = TRUE), wMaizeAnom = sum(wMaizeAnom, na.rm=TRUE)/mean(MZ_tot_har_area), wMaizePerAnom = sum(wMaizePerAnom, na.rm=TRUE)/mean(MZ_tot_har_area))

Sorghum_aggregated_data <- Area_Sorghum %>%
  group_by(Year) %>%
  summarize(Avg_SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE), Avg_SorghumPerAnom = mean(SorghumPerAnom, na.rm = TRUE), wSorghumAnom = sum(wSorghumAnom, na.rm=TRUE)/mean(SG_tot_har_area), wSorghumPerAnom = sum(wSorghumPerAnom, na.rm=TRUE)/mean(SG_tot_har_area))

PearlMillet_aggregated_data <- Area_PearlMillet %>%
  group_by(Year) %>%
  summarize(Avg_PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE), Avg_PearlMilletPerAnom = mean(PearlMilletPerAnom, na.rm = TRUE), wPearlMilletAnom = sum(wPearlMilletAnom, na.rm=TRUE)/mean(PM_tot_har_area), wPearlMilletPerAnom = sum(wPearlMilletPerAnom, na.rm=TRUE)/mean(PM_tot_har_area))

FingerMillet_aggregated_data <- Area_FingerMillet %>%
  group_by(Year) %>%
  summarize(Avg_FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE), Avg_FingerMilletPerAnom = mean(FingerMilletPerAnom, na.rm = TRUE), wFingerMilletAnom = sum(wFingerMilletAnom, na.rm=TRUE)/mean(FM_tot_har_area), wFingerMilletPerAnom = sum(wFingerMilletPerAnom, na.rm=TRUE)/mean(FM_tot_har_area))

aggregated_data <- Area_Total %>%
  group_by(Year) %>%
  summarize(Avg_AreaAnom = mean(AreaAnom, na.rm = TRUE), Avg_AreaPerAnom = mean(AreaPerAnom, na.rm = TRUE), wAreaAnom = sum(wAreaAnom, na.rm=TRUE)/mean(tot_har_area), wAreaPerAnom = sum(wAreaPerAnom, na.rm=TRUE)/mean(tot_har_area))

crop_aggregated_data <- list(Rice_aggregated_data, Maize_aggregated_data, Sorghum_aggregated_data, PearlMillet_aggregated_data, FingerMillet_aggregated_data, aggregated_data)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Crop_area_aggregated_data <- Reduce(merge_fun, crop_aggregated_data)

# Irrigated Area

# 1. Calculation of Total Harvested Areas

RI_tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(RI_tot_irr_area = sum(RICE.IRRIGATED.AREA..1000.ha., na.rm = TRUE))
MZ_tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(MZ_tot_irr_area = sum(MAIZE.IRRIGATED.AREA..1000.ha., na.rm = TRUE))
SG_tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(SG_tot_irr_area = sum(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha., na.rm = TRUE))
PM_tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(PM_tot_irr_area = sum(PEARL.MILLET.IRRIGATED.AREA..1000.ha., na.rm = TRUE))
FM_tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(FM_tot_irr_area = sum(FINGER.MILLET.IRRIGATED.AREA..1000.ha., na.rm = TRUE))
tot_irr_area <- Harv_Irr %>%
  group_by(Year) %>%
  summarize(tot_irr_area = sum(IrrArea_total, na.rm = TRUE))


crop_tot_irr_area <- list(RI_tot_irr_area, MZ_tot_irr_area, SG_tot_irr_area, PM_tot_irr_area, FM_tot_irr_area, tot_irr_area)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Tot_irr_area <- Reduce(merge_fun, crop_tot_irr_area)
Harv_Irr <- merge(Harv_Irr, Tot_irr_area, by = "Year")

# 2. Multiply Absolute and Relative Anomalies with Area

Harv_Irr <- Harv_Irr %>%
  mutate(wRiceIrrAnom = RiceIrrAnom * RICE.IRRIGATED.AREA..1000.ha.,
         wRiceIrrPerAnom = RiceIrrPerAnom * RICE.IRRIGATED.AREA..1000.ha.,
         wMaizeIrrAnom = MaizeIrrAnom * MAIZE.IRRIGATED.AREA..1000.ha.,
         wMaizeIrrPerAnom = MaizeIrrPerAnom * MAIZE.IRRIGATED.AREA..1000.ha.,
         wSorghumIrrAnom = SorghumIrrAnom * KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.,
         wSorghumIrrPerAnom = SorghumIrrPerAnom * KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.,
         wPearlMilletIrrAnom = PearlMilletIrrAnom * PEARL.MILLET.IRRIGATED.AREA..1000.ha.,
         wPearlMilletIrrPerAnom = PearlMilletIrrPerAnom * PEARL.MILLET.IRRIGATED.AREA..1000.ha.,
         wFingerMilletIrrAnom = FingerMilletIrrAnom * FINGER.MILLET.IRRIGATED.AREA..1000.ha.,
         wFingerMilletIrrPerAnom = FingerMilletIrrPerAnom * FINGER.MILLET.IRRIGATED.AREA..1000.ha.,
         wIrrAnom = IrrAreaAnom * IrrArea_total,
         wIrrPerAnom = IrrAreaPerAnom * IrrArea_total)

# 3. Calculate the weighted average absolute and relative anomalies using the formula: https://www.worldpop.org/methods/pwd/

Harv_Irr_Rice <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'RICE.IRRIGATED.AREA..1000.ha.', 'RI_tot_irr_area', 'RiceIrrAnom', 'wRiceIrrAnom', 'RiceIrrPerAnom', 'wRiceIrrPerAnom'))
Harv_Irr_Maize <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'MAIZE.IRRIGATED.AREA..1000.ha.', 'MZ_tot_irr_area', 'MaizeIrrAnom', 'wMaizeIrrAnom', 'MaizeIrrPerAnom', 'wMaizeIrrPerAnom'))
Harv_Irr_Sorghum <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha.', 'SG_tot_irr_area', 'SorghumIrrAnom', 'wSorghumIrrAnom', 'SorghumIrrPerAnom', 'wSorghumIrrPerAnom'))
Harv_Irr_PearlMillet <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'PEARL.MILLET.IRRIGATED.AREA..1000.ha.', 'PM_tot_irr_area', 'PearlMilletIrrAnom', 'wPearlMilletIrrAnom', 'PearlMilletIrrPerAnom', 'wPearlMilletIrrPerAnom'))
Harv_Irr_FingerMillet <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'FINGER.MILLET.IRRIGATED.AREA..1000.ha.', 'FM_tot_irr_area', 'FingerMilletIrrAnom', 'wFingerMilletIrrAnom', 'FingerMilletIrrPerAnom', 'wFingerMilletIrrPerAnom'))
Harv_Irr_Total <- subset(Harv_Irr, select = c('Year', 'Dist.Code', 'IrrArea_total', 'tot_irr_area', 'IrrAreaAnom', 'wIrrAnom', 'IrrAreaPerAnom', 'wIrrPerAnom'))

Rice_aggregated_data <- Harv_Irr_Rice %>%
  group_by(Year) %>%
  summarize(Avg_RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE), Avg_RiceIrrPerAnom = mean(RiceIrrPerAnom, na.rm = TRUE), wRiceIrrAnom = sum(wRiceIrrAnom, na.rm=TRUE)/mean(RI_tot_irr_area), wRiceIrrPerAnom = sum(wRiceIrrPerAnom, na.rm=TRUE)/mean(RI_tot_irr_area))

Maize_aggregated_data <- Harv_Irr_Maize %>%
  group_by(Year) %>%
  summarize(Avg_MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE), Avg_MaizeIrrPerAnom = mean(MaizeIrrPerAnom, na.rm = TRUE), wMaizeIrrAnom = sum(wMaizeIrrAnom, na.rm=TRUE)/mean(MZ_tot_irr_area), wMaizeIrrPerAnom = sum(wMaizeIrrPerAnom, na.rm=TRUE)/mean(MZ_tot_irr_area))

Sorghum_aggregated_data <- Harv_Irr_Sorghum %>%
  group_by(Year) %>%
  summarize(Avg_SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE), Avg_SorghumIrrPerAnom = mean(SorghumIrrPerAnom, na.rm = TRUE), wSorghumIrrAnom = sum(wSorghumIrrAnom, na.rm=TRUE)/mean(SG_tot_irr_area), wSorghumIrrPerAnom = sum(wSorghumIrrPerAnom, na.rm=TRUE)/mean(SG_tot_irr_area))

PearlMillet_aggregated_data <- Harv_Irr_PearlMillet %>%
  group_by(Year) %>%
  summarize(Avg_PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE), Avg_PearlMilletIrrPerAnom = mean(PearlMilletIrrPerAnom, na.rm = TRUE), wPearlMilletIrrAnom = sum(wPearlMilletIrrAnom, na.rm=TRUE)/mean(PM_tot_irr_area), wPearlMilletIrrPerAnom = sum(wPearlMilletIrrPerAnom, na.rm=TRUE)/mean(PM_tot_irr_area))

FingerMillet_aggregated_data <- Harv_Irr_FingerMillet %>%
  group_by(Year) %>%
  summarize(Avg_FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE), Avg_FingerMilletIrrPerAnom = mean(FingerMilletIrrPerAnom, na.rm = TRUE), wFingerMilletIrrAnom = sum(wFingerMilletIrrAnom, na.rm=TRUE)/mean(FM_tot_irr_area), wFingerMilletIrrPerAnom = sum(wFingerMilletIrrPerAnom, na.rm=TRUE)/mean(FM_tot_irr_area))

aggregated_data <- Harv_Irr_Total %>%
  group_by(Year) %>%
  summarize(Avg_IrrAnom = mean(IrrAreaAnom, na.rm = TRUE), Avg_IrrPerAnom = mean(IrrAreaPerAnom, na.rm = TRUE), wIrrAnom = sum(wIrrAnom, na.rm=TRUE)/mean(tot_irr_area), wIrrPerAnom = sum(wIrrPerAnom, na.rm=TRUE)/mean(tot_irr_area))

crop_aggregated_data <- list(Rice_aggregated_data, Maize_aggregated_data, Sorghum_aggregated_data, PearlMillet_aggregated_data, FingerMillet_aggregated_data, aggregated_data)
merge_fun <- function(x, y) {
  merge(x, y, by = "Year", all = TRUE)
}

Crop_irrarea_aggregated_data <- Reduce(merge_fun, crop_aggregated_data)


# ENSO

nino34 <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/nino3.4_SST.csv")
nino34 <- data.frame(cbind(nino34$YR, nino34$MON, nino34$NINO3.4))
nino34_1 <- nino34[(which(nino34[,1]==1968)[1]):(which(nino34[,1]==2015)[12]),]
colnames(nino34_1)<-c('Y', 'MON', 'Nino34')
nino34_jjas <- subset(nino34_1 , MON >= 6 & MON <= 9)
nino34_jjas1 <- aggregate(cbind(nino34_jjas$Nino34), by=list(nino34_jjas$Y), FUN=mean, na.rm=T)
nino34_jjas_mean <- subset(nino34_jjas1 , Group.1 >= 1981 & Group.1 <= 2010)
jjas_nino34_anom<-(nino34_jjas1$V1-mean(nino34_jjas_mean$V1))/sd(nino34_jjas_mean$V1)
detrend_jjas_nino34<-detrend(jjas_nino34_anom, tt='linear')
plot(c(1968:2015),detrend_jjas_nino34, type="l", ylab="Detrend-Standardized Nino34 anomalies")
abline(h=0.5, col='grey', lty=2, xpd=FALSE)
abline(h=-0.5, col='grey', lty=2, xpd=FALSE)
print(c(1968:2015)[which(detrend_jjas_nino34>=0.5)])
print(c(1968:2015)[which(detrend_jjas_nino34<=-0.5)])
#1968 1969 1972 1977 1982 1987 1991 1993 1994 1997 2002 2004 2009 2015 0.5SD El Nino
#1970 1971 1973 1974 1975 1978 1985 1988 1998 1999 2000 2007 2008 2010 2011 2013 -0.5SD La Nina

# IOD

iod <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/DMI HadISST1.1.csv")
iod_long <- melt(data=iod, id.vars="Year", variable.name="Month", value.name="iod")
iod_long <- iod_long[order(iod_long$Year),]
iod_jjas <- subset(iod_long, Month=="Jun" | Month=="Jul" | Month=="Aug" | Month=="Sep")
iod_jjas1 <- aggregate(cbind(iod_jjas$iod), by=list(iod_jjas$Year), FUN=mean, na.rm=T)
colnames(iod_jjas1) <- c('Year', 'iod')
iod_jjas2 <- subset(iod_jjas1, Year >=1968 & Year<=2015)
iod_jjas3 <- subset(iod_jjas2, Year >= 1981 & Year <= 2010)
jjas_iod_anom <- iod_jjas2$iod/sd(iod_jjas3$iod)
detrend_jjas_iod <- detrend(jjas_iod_anom, tt='linear')
plot(c(1968:2015),detrend_jjas_iod, type="l", ylab="Detrend-Standardized IOD anomalies")
abline(h=0.5, col='grey', lty=2, xpd=FALSE)
abline(h=-0.5, col='grey', lty=2, xpd=FALSE)
print(c(1968:2015)[which(detrend_jjas_iod>=0.5)])
print(c(1968:2015)[which(detrend_jjas_iod<=-0.5)])

#1972 1976 1982 1983 1987 1991 1994 1997 2006 2008 2011 2012 2015 0.5SD IOD+
#1970 1971 1973 1980 1981 1984 1985 1986 1989 1990 1992 1996 1998 2004 2005 2013 2014 -0.5SD IOD-

# ENSO and IOD

nino34_iod <- data.frame(c(1968:2015), detrend_jjas_nino34, detrend_jjas_iod)
colnames(nino34_iod) <- c('Year', 'detrend_jjas_nino34', 'detrend_jjas_iod')
yield_nino34_iod <- merge(x = Crop_yield_aggregated_data, y = nino34_iod, by = "Year", all.x = TRUE)
prod_nino34_iod <- merge(x = Crop_prod_aggregated_data, y = nino34_iod, by = "Year", all.x = TRUE)
area_nino34_iod <- merge(x = Crop_area_aggregated_data, y = nino34_iod, by = "Year", all.x = TRUE)
irrarea_nino34_iod <- merge(x = Crop_irrarea_aggregated_data, y = nino34_iod, by = "Year", all.x = TRUE)

yield_nino34_iod$nino34phase <- ifelse(yield_nino34_iod$detrend_jjas_nino34 <= -0.5, "La Nina", ifelse((yield_nino34_iod$detrend_jjas_nino34 >= 0.5), "El Nino", "Neutral"))
yield_nino34_iod$iodphase <- ifelse(yield_nino34_iod$detrend_jjas_iod <= -0.5, "IOD-", ifelse((yield_nino34_iod$detrend_jjas_iod >= 0.5), "IOD+", "Neutral"))

prod_nino34_iod$nino34phase <- ifelse(prod_nino34_iod$detrend_jjas_nino34 <= -0.5, "La Nina", ifelse((prod_nino34_iod$detrend_jjas_nino34 >= 0.5), "El Nino", "Neutral"))
prod_nino34_iod$iodphase <- ifelse(prod_nino34_iod$detrend_jjas_iod <= -0.5, "IOD-", ifelse((prod_nino34_iod$detrend_jjas_iod >= 0.5), "IOD+", "Neutral"))

area_nino34_iod$nino34phase <- ifelse(area_nino34_iod$detrend_jjas_nino34 <= -0.5, "La Nina", ifelse((area_nino34_iod$detrend_jjas_nino34 >= 0.5), "El Nino", "Neutral"))
area_nino34_iod$iodphase <- ifelse(area_nino34_iod$detrend_jjas_iod <= -0.5, "IOD-", ifelse((area_nino34_iod$detrend_jjas_iod >= 0.5), "IOD+", "Neutral"))

irrarea_nino34_iod$nino34phase <- ifelse(irrarea_nino34_iod$detrend_jjas_nino34 <= -0.5, "La Nina", ifelse((irrarea_nino34_iod$detrend_jjas_nino34 >= 0.5), "El Nino", "Neutral"))
irrarea_nino34_iod$iodphase <- ifelse(irrarea_nino34_iod$detrend_jjas_iod <= -0.5, "IOD-", ifelse((irrarea_nino34_iod$detrend_jjas_iod >= 0.5), "IOD+", "Neutral"))

yield_nino34_iod_1 <- yield_nino34_iod[3:50, c('Year', 'wYieldAnom', 'wYieldPerAnom', 'nino34phase', 'iodphase')]
prod_nino34_iod_1 <- prod_nino34_iod[3:50, c('Year', 'wProdAnom', 'wProdPerAnom', 'nino34phase', 'iodphase')]
area_nino34_iod_1 <- area_nino34_iod[3:50, c('Year', 'wAreaAnom', 'wAreaPerAnom', 'nino34phase', 'iodphase')]
irrarea_nino34_iod_1 <- irrarea_nino34_iod[3:50, c('Year', 'wIrrAnom', 'wIrrPerAnom', 'nino34phase', 'iodphase')]

p1 <- ggplot(yield_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = yield_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wYieldAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200, 200)+
  theme(axis.text = element_text(size = 15))
yield_nino34_iod_2 <- yield_nino34_iod[3:50, c('Year', 'wYieldAnom', 'nino34phase', 'iodphase')]
p1.1 <- p1+
  geom_point(data = yield_nino34_iod_2, aes(x=Year, y=wYieldAnom), size=1.5, color=ifelse(yield_nino34_iod_2$wYieldAnom >= mean(yield_nino34_iod_2$wYieldAnom)+0.5*sd(yield_nino34_iod_2$wYieldAnom), "blue", ifelse(yield_nino34_iod_2$wYieldAnom <= mean(yield_nino34_iod_2$wYieldAnom)-0.5*sd(yield_nino34_iod_2$wYieldAnom), "red", "NA")))
p2 <- ggplot(yield_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = yield_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wYieldAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200, 200)+
  theme(axis.text = element_text(size = 15))
p2.1 <- p2+
  geom_point(data = yield_nino34_iod_2, aes(x=Year, y=wYieldAnom), size=1.5, color=ifelse(yield_nino34_iod_2$wYieldAnom >= mean(yield_nino34_iod_2$wYieldAnom)+0.5*sd(yield_nino34_iod_2$wYieldAnom), "blue", ifelse(yield_nino34_iod_2$wYieldAnom <= mean(yield_nino34_iod_2$wYieldAnom)-0.5*sd(yield_nino34_iod_2$wYieldAnom), "red", "NA")))

p <- cowplot::plot_grid(p1.1, p2.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("Yield Anomaly (kg/ha)", fontface='bold')
cowplot::plot_grid(title, p, ncol=1, rel_heights = c(0.1,1))

p3 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200,200)+
  theme(axis.text = element_text(size = 15))
prod_nino34_iod_2 <- prod_nino34_iod[3:50, c('Year', 'wProdAnom', 'nino34phase', 'iodphase')]
p3.1 <- p3+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))
p4 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200,200)+
  theme(axis.text = element_text(size = 15))
p4.1 <- p4+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))

p <- cowplot::plot_grid(p3.1, p4.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("Production Anomaly (1000 tons)", fontface='bold')
cowplot::plot_grid(title, p, ncol=1, rel_heights = c(0.1,1))

p5 <- ggplot(area_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = area_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wAreaAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-25, 25)+
  theme(axis.text = element_text(size = 15))
area_nino34_iod_2 <- area_nino34_iod[3:50, c('Year', 'wAreaAnom', 'nino34phase', 'iodphase')]
p5.1 <- p5+
  geom_point(data = area_nino34_iod_2, aes(x=Year, y=wAreaAnom), size=1.5, color=ifelse(area_nino34_iod_2$wAreaAnom >= mean(area_nino34_iod_2$wAreaAnom)+0.5*sd(area_nino34_iod_2$wAreaAnom), "blue", ifelse(area_nino34_iod_2$wAreaAnom <= mean(area_nino34_iod_2$wAreaAnom)-0.5*sd(area_nino34_iod_2$wAreaAnom), "red", "NA")))
p6 <- ggplot(area_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = area_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wAreaAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-25, 25)+
  theme(axis.text = element_text(size = 15))
p6.1 <- p6+
  geom_point(data = area_nino34_iod_2, aes(x=Year, y=wAreaAnom), size=1.5, color=ifelse(area_nino34_iod_2$wAreaAnom >= mean(area_nino34_iod_2$wAreaAnom)+0.5*sd(area_nino34_iod_2$wAreaAnom), "blue", ifelse(area_nino34_iod_2$wAreaAnom <= mean(area_nino34_iod_2$wAreaAnom)-0.5*sd(area_nino34_iod_2$wAreaAnom), "red", "NA")))

p<- cowplot::plot_grid(p5.1, p6.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("Harvested Area Anomaly (1000 ha)", fontface='bold')
cowplot::plot_grid(title, p, ncol=1, rel_heights = c(0.1,1))

p3 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200,200)+
  theme(axis.text = element_text(size = 15))
prod_nino34_iod_2 <- prod_nino34_iod[3:50, c('Year', 'wProdAnom', 'nino34phase', 'iodphase')]
p3.1 <- p3+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))

p5 <- ggplot(area_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = area_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wAreaAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-25, 25)+
  theme(axis.text = element_text(size = 15))
area_nino34_iod_2 <- area_nino34_iod[3:50, c('Year', 'wAreaAnom', 'nino34phase', 'iodphase')]
p5.1 <- p5+
  geom_point(data = area_nino34_iod_2, aes(x=Year, y=wAreaAnom), size=1.5, color=ifelse(area_nino34_iod_2$wAreaAnom >= mean(area_nino34_iod_2$wAreaAnom)+0.5*sd(area_nino34_iod_2$wAreaAnom), "blue", ifelse(area_nino34_iod_2$wAreaAnom <= mean(area_nino34_iod_2$wAreaAnom)-0.5*sd(area_nino34_iod_2$wAreaAnom), "red", "NA")))

p_enso <- cowplot::plot_grid(p3.1, p5.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("ENSO phases", fontface='bold')
p_enso1 <- cowplot::plot_grid(title, p_enso, ncol=1, rel_heights = c(0.1,1))
p_enso1

p4 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-200,200)+
  theme(axis.text = element_text(size = 15))
p4.1 <- p4+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))

p6 <- ggplot(area_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = area_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wAreaAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-25, 25)+
  theme(axis.text = element_text(size = 15))
p6.1 <- p6+
  geom_point(data = area_nino34_iod_2, aes(x=Year, y=wAreaAnom), size=1.5, color=ifelse(area_nino34_iod_2$wAreaAnom >= mean(area_nino34_iod_2$wAreaAnom)+0.5*sd(area_nino34_iod_2$wAreaAnom), "blue", ifelse(area_nino34_iod_2$wAreaAnom <= mean(area_nino34_iod_2$wAreaAnom)-0.5*sd(area_nino34_iod_2$wAreaAnom), "red", "NA")))

p_iod <- cowplot::plot_grid(p4.1, p6.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("IOD phases", fontface='bold')
p_iod1 <- cowplot::plot_grid(title, p_iod, ncol=1, rel_heights = c(0.1,1))
p_iod1

p3 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  theme(legend.key.size = unit(1, 'cm'))+
  theme(legend.justification = "top")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=15))+
  ylab("")+
  theme(axis.title.x = element_blank())
prod_nino34_iod_2 <- prod_nino34_iod[3:50, c('Year', 'wProdAnom', 'nino34phase', 'iodphase')]
p3.1 <- p3+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))

p4 <- ggplot(prod_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = prod_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wProdAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  theme(legend.key.size = unit(1, 'cm'))+
  theme(legend.justification = "top")+
  theme(legend.position="bottom")+
  theme(legend.text=element_text(size=15))+
  ylab("")+
  theme(axis.title.x = element_blank())
p4.1 <- p4+
  geom_point(data = prod_nino34_iod_2, aes(x=Year, y=wProdAnom), size=1.5, color=ifelse(prod_nino34_iod_2$wProdAnom >= mean(prod_nino34_iod_2$wProdAnom)+0.5*sd(prod_nino34_iod_2$wProdAnom), "blue", ifelse(prod_nino34_iod_2$wProdAnom <= mean(prod_nino34_iod_2$wProdAnom)-0.5*sd(prod_nino34_iod_2$wProdAnom), "red", "NA")))

p <- cowplot::plot_grid(p3.1, p4.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("Production Anomaly (1000 tons)", fontface='bold')
cowplot::plot_grid(title, p, ncol=1, rel_heights = c(0.1,1))

p7 <- ggplot(irrarea_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = nino34phase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = irrarea_nino34_iod_1)+
  scale_fill_manual(values = c("red", "blue", "white"))+
  geom_line(aes(x = Year, y = wIrrAnom), size = 0.7, show.legend = FALSE)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-35, 35)+
  theme(axis.text = element_text(size = 15))
irrarea_nino34_iod_2 <- irrarea_nino34_iod[3:50, c('Year', 'wIrrAnom', 'nino34phase', 'iodphase')]
p7.1 <- p7+
  geom_point(data = irrarea_nino34_iod_2, aes(x=Year, y=wIrrAnom), size=1.5, color=ifelse(irrarea_nino34_iod_2$wIrrAnom >= mean(irrarea_nino34_iod_2$wIrrAnom)+0.5*sd(irrarea_nino34_iod_2$wIrrAnom), "blue", ifelse(irrarea_nino34_iod_2$wIrrAnom <= mean(irrarea_nino34_iod_2$wIrrAnom)-0.5*sd(irrarea_nino34_iod_2$wIrrAnom), "red", "NA")))
p8 <- ggplot(irrarea_nino34_iod_1)+
  geom_rect(aes(xmin = Year-0.5, xmax = Year+0.5, fill = iodphase), ymin = -Inf, ymax = Inf, alpha = 0.1, data = irrarea_nino34_iod_1)+
  scale_fill_manual(values = c("forestgreen", "magenta", "white"))+
  geom_line(aes(x = Year, y = wIrrAnom), size = 0.7)+
  geom_hline(yintercept = 0, lty=2)+
  theme_minimal()+
  ylab("")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")+
  ylim(-35, 35)+
  theme(axis.text = element_text(size = 15))
p8.1 <- p8+
  geom_point(data = irrarea_nino34_iod_2, aes(x=Year, y=wIrrAnom), size=1.5, color=ifelse(irrarea_nino34_iod_2$wIrrAnom >= mean(irrarea_nino34_iod_2$wIrrAnom)+0.5*sd(irrarea_nino34_iod_2$wIrrAnom), "blue", ifelse(irrarea_nino34_iod_2$wIrrAnom <= mean(irrarea_nino34_iod_2$wIrrAnom)-0.5*sd(irrarea_nino34_iod_2$wIrrAnom), "red", "NA")))

p<- cowplot::plot_grid(p7.1, p8.1, align = "v", nrow = 2, rel_heights = c(1/2, 1/2))
title <- ggdraw() + draw_label("Irrigated Area Anomaly (1000 ha)", fontface='bold')
cowplot::plot_grid(title, p, ncol=1, rel_heights = c(0.1,1))

