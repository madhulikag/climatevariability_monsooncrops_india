#Fig4&S3-6 
#Spatial differences in yield impacts during El Niño and IOD+ events
#Spatial differences in yield impacts during La Niña and IOD- events
#Spatial differences in production impacts during ENSO and IOD
#Spatial differences in harvested area impacts during ENSO and IOD
#Spatial differences in irrigated area impacts during ENSO and IOD

###Composite Maps of yield, production, harvested areas, irrigated areas - absolute


Harv_Irr_Yield


###Yield

# Define the years for each phase
el_nino_years <- c(1968, 1969, 1972, 1977, 1982, 1987, 1991, 1993, 1994, 1997, 2002, 2004, 2009, 2015)
la_nina_years <- c(1970, 1971, 1973, 1974, 1975, 1978, 1985, 1988, 1998, 1999, 2000, 2007, 2008, 2010, 2011, 2013)
iod_plus_years <- c(1972, 1976, 1982, 1983, 1987, 1991, 1994, 1997, 2006, 2008, 2011, 2012, 2015)
iod_minus_years <- c(1970, 1971, 1973, 1980, 1981, 1984, 1985, 1986, 1989, 1990, 1992, 1996, 1998, 2004, 2005, 2013, 2014)
el_nino_iod_plus_years <- c(1972, 1982, 1987, 1991, 1994, 1997, 2015)
la_nina_iod_minus_years <- c(1970, 1971, 1973, 1985, 1998, 2013)

# Subset the data for each phase
el_nino_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_plus_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_plus_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_plus_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_plus_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_plus_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_minus_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_minus_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_minus_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_minus_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_minus_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_iod_plus_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_iod_plus_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_iod_plus_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_rice <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(RICE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_iod_minus_data_maize <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(MAIZE.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_iod_minus_data_sorghum <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_iod_minus_data_pearl_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_finger_millet <- Harv_Irr_Yield %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.YIELD..Kg.per.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

hist(el_nino_data_rice$RiceAnom, main = "El Nino - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_rice$RiceAnom, main = "La Nina - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_rice$RiceAnom, main = "IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_rice$RiceAnom, main = "IOD- - Rice Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_rice$RiceAnom, main = "El Nino & IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_rice$RiceAnom, main = "La Nina & IOD- - Rice Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_maize$MaizeAnom, main = "El Nino - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_maize$MaizeAnom, main = "La Nina - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_maize$MaizeAnom, main = "IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_maize$MaizeAnom, main = "IOD- - Maize Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_maize$MaizeAnom, main = "El Nino & IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_maize$MaizeAnom, main = "La Nina & IOD- - Maize Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_sorghum$SorghumKharifAnom, main = "El Nino - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_sorghum$SorghumKharifAnom, main = "La Nina - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_sorghum$SorghumKharifAnom, main = "IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_sorghum$SorghumKharifAnom, main = "IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_sorghum$SorghumKharifAnom, main = "El Nino & IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_sorghum$SorghumKharifAnom, main = "La Nina & IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_pearl_millet$PearlMilletAnom, main = "El Nino - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_pearl_millet$PearlMilletAnom, main = "La Nina - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_pearl_millet$PearlMilletAnom, main = "IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_pearl_millet$PearlMilletAnom, main = "IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom, main = "El Nino & IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom, main = "La Nina & IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_finger_millet$FingerMilletAnom, main = "El Nino - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_finger_millet$FingerMilletAnom, main = "La Nina - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_finger_millet$FingerMilletAnom, main = "IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_finger_millet$FingerMilletAnom, main = "IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_finger_millet$FingerMilletAnom, main = "El Nino & IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_finger_millet$FingerMilletAnom, main = "La Nina & IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")

#el_nino_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

#la_nina_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# el_nino_iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# la_nina_iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom > 400)] <- 400
el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom < -400)] <- -400

el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom > 400)] <- 400
el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom < -400)] <- -400

el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom > 400)] <- 400
el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom < -400)] <- -400

el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom > 400)] <- 400
el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom < -400)] <- -400

el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom > 400)] <- 400
el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom < -400)] <- -400

la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom > 400)] <- 400
la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom < -400)] <- -400

la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom > 400)] <- 400
la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom < -400)] <- -400

la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom > 400)] <- 400
la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom < -400)] <- -400

la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom > 400)] <- 400
la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom < -400)] <- -400

la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom > 400)] <- 400
la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom < -400)] <- -400

iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom > 400)] <- 400
iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom < -400)] <- -400

iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom > 400)] <- 400
iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom < -400)] <- -400

iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom > 400)] <- 400
iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom < -400)] <- -400

iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom > 400)] <- 400
iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom < -400)] <- -400

iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom > 400)] <- 400
iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom < -400)] <- -400

iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom > 400)] <- 400
iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom < -400)] <- -400

iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom > 400)] <- 400
iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom < -400)] <- -400

iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom > 400)] <- 400
iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom < -400)] <- -400

iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom > 400)] <- 400
iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom < -400)] <- -400

iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom > 400)] <- 400
iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom < -400)] <- -400

el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom > 400)] <- 400
el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom < -400)] <- -400

el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom > 400)] <- 400
el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom < -400)] <- -400

el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 400)] <- 400
el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < -400)] <- -400

el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 400)] <- 400
el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < -400)] <- -400

el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 400)] <- 400
el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < -400)] <- -400

la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom > 400)] <- 400
la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom < -400)] <- -400

la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom > 400)] <- 400
la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom < -400)] <- -400

la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 400)] <- 400
la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < -400)] <- -400

la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 400)] <- 400
la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < -400)] <- -400

la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 400)] <- 400
la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < -400)] <- -400

# Merge the composite means with the shapefile
merge(VDSA_shape, el_nino_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, el_nino_iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_finger_millet, by = "Dist.Code")

el_nino_data_rice$Dist.Code <- as.character(el_nino_data_rice$Dist.Code)
el_nino_data_maize$Dist.Code <- as.character(el_nino_data_maize$Dist.Code)
el_nino_data_sorghum$Dist.Code <- as.character(el_nino_data_sorghum$Dist.Code)
el_nino_data_pearl_millet$Dist.Code <- as.character(el_nino_data_pearl_millet$Dist.Code)
el_nino_data_finger_millet$Dist.Code <- as.character(el_nino_data_finger_millet$Dist.Code)

la_nina_data_rice$Dist.Code <- as.character(la_nina_data_rice$Dist.Code)
la_nina_data_maize$Dist.Code <- as.character(la_nina_data_maize$Dist.Code)
la_nina_data_sorghum$Dist.Code <- as.character(la_nina_data_sorghum$Dist.Code)
la_nina_data_pearl_millet$Dist.Code <- as.character(la_nina_data_pearl_millet$Dist.Code)
la_nina_data_finger_millet$Dist.Code <- as.character(la_nina_data_finger_millet$Dist.Code)

iod_plus_data_rice$Dist.Code <- as.character(iod_plus_data_rice$Dist.Code)
iod_plus_data_maize$Dist.Code <- as.character(iod_plus_data_maize$Dist.Code)
iod_plus_data_sorghum$Dist.Code <- as.character(iod_plus_data_sorghum$Dist.Code)
iod_plus_data_pearl_millet$Dist.Code <- as.character(iod_plus_data_pearl_millet$Dist.Code)
iod_plus_data_finger_millet$Dist.Code <- as.character(iod_plus_data_finger_millet$Dist.Code)

iod_minus_data_rice$Dist.Code <- as.character(iod_minus_data_rice$Dist.Code)
iod_minus_data_maize$Dist.Code <- as.character(iod_minus_data_maize$Dist.Code)
iod_minus_data_sorghum$Dist.Code <- as.character(iod_minus_data_sorghum$Dist.Code)
iod_minus_data_pearl_millet$Dist.Code <- as.character(iod_minus_data_pearl_millet$Dist.Code)
iod_minus_data_finger_millet$Dist.Code <- as.character(iod_minus_data_finger_millet$Dist.Code)

el_nino_iod_plus_data_rice$Dist.Code <- as.character(el_nino_iod_plus_data_rice$Dist.Code)
el_nino_iod_plus_data_maize$Dist.Code <- as.character(el_nino_iod_plus_data_maize$Dist.Code)
el_nino_iod_plus_data_sorghum$Dist.Code <- as.character(el_nino_iod_plus_data_sorghum$Dist.Code)
el_nino_iod_plus_data_pearl_millet$Dist.Code <- as.character(el_nino_iod_plus_data_pearl_millet$Dist.Code)
el_nino_iod_plus_data_finger_millet$Dist.Code <- as.character(el_nino_iod_plus_data_finger_millet$Dist.Code)

la_nina_iod_minus_data_rice$Dist.Code <- as.character(la_nina_iod_minus_data_rice$Dist.Code)
la_nina_iod_minus_data_maize$Dist.Code <- as.character(la_nina_iod_minus_data_maize$Dist.Code)
la_nina_iod_minus_data_sorghum$Dist.Code <- as.character(la_nina_iod_minus_data_sorghum$Dist.Code)
la_nina_iod_minus_data_pearl_millet$Dist.Code <- as.character(la_nina_iod_minus_data_pearl_millet$Dist.Code)
la_nina_iod_minus_data_finger_millet$Dist.Code <- as.character(la_nina_iod_minus_data_finger_millet$Dist.Code)

el_nino_map_data_rice <- full_join(mymap,  el_nino_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_maize <- full_join(mymap,  el_nino_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_sorghum <- full_join(mymap,  el_nino_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_pearl_millet <- full_join(mymap,  el_nino_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_finger_millet <- full_join(mymap,  el_nino_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_map_data_rice <- full_join(mymap,  la_nina_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_maize <- full_join(mymap,  la_nina_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_sorghum <- full_join(mymap,  la_nina_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_pearl_millet <- full_join(mymap,  la_nina_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_finger_millet <- full_join(mymap,  la_nina_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_plus_map_data_rice <- full_join(mymap,  iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_maize <- full_join(mymap,  iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_sorghum <- full_join(mymap,  iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_pearl_millet <- full_join(mymap,  iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_finger_millet <- full_join(mymap,  iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_minus_map_data_rice <- full_join(mymap,  iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_maize <- full_join(mymap,  iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_sorghum <- full_join(mymap,  iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_pearl_millet <- full_join(mymap,  iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_finger_millet <- full_join(mymap,  iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

el_nino_iod_plus_map_data_rice <- full_join(mymap,  el_nino_iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_maize <- full_join(mymap,  el_nino_iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_sorghum <- full_join(mymap,  el_nino_iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_pearl_millet <- full_join(mymap,  el_nino_iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_finger_millet <- full_join(mymap,  el_nino_iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_iod_minus_map_data_rice <- full_join(mymap,  la_nina_iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_maize <- full_join(mymap,  la_nina_iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_sorghum <- full_join(mymap,  la_nina_iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_pearl_millet <- full_join(mymap,  la_nina_iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_finger_millet <- full_join(mymap,  la_nina_iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))


gg <- ggplot(el_nino_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD+ Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD+ Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD+ Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD+ Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD+ Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD- Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD- Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD- Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD- Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("IOD- Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino & IOD+ Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino & IOD+ Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino & IOD+ Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino & IOD+ Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("El Nino & IOD+ Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina & IOD- Rice Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina & IOD- Maize Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina & IOD- Sorghum Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina & IOD- Pearl Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-400,400), breaks = c(-400,-300,-200,-100,0,100,200,300,400), space = "Lab")+
  ggtitle("La Nina & IOD- Finger Millet Yield Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


###Production

# Subset the data for each phase
el_nino_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_plus_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_plus_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_plus_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_plus_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_plus_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_minus_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_minus_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_minus_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_minus_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_minus_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_iod_plus_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_iod_plus_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_iod_plus_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_rice <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(RICE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_iod_minus_data_maize <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(MAIZE.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_iod_minus_data_sorghum <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_iod_minus_data_pearl_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_finger_millet <- Harv_Irr_Prod %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.PRODUCTION..1000.tons. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

hist(el_nino_data_rice$RiceAnom, main = "El Nino - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_rice$RiceAnom, main = "La Nina - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_rice$RiceAnom, main = "IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_rice$RiceAnom, main = "IOD- - Rice Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_rice$RiceAnom, main = "El Nino & IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_rice$RiceAnom, main = "La Nina & IOD- - Rice Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_maize$MaizeAnom, main = "El Nino - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_maize$MaizeAnom, main = "La Nina - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_maize$MaizeAnom, main = "IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_maize$MaizeAnom, main = "IOD- - Maize Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_maize$MaizeAnom, main = "El Nino & IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_maize$MaizeAnom, main = "La Nina & IOD- - Maize Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_sorghum$SorghumKharifAnom, main = "El Nino - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_sorghum$SorghumKharifAnom, main = "La Nina - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_sorghum$SorghumKharifAnom, main = "IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_sorghum$SorghumKharifAnom, main = "IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_sorghum$SorghumKharifAnom, main = "El Nino & IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_sorghum$SorghumKharifAnom, main = "La Nina & IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_pearl_millet$PearlMilletAnom, main = "El Nino - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_pearl_millet$PearlMilletAnom, main = "La Nina - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_pearl_millet$PearlMilletAnom, main = "IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_pearl_millet$PearlMilletAnom, main = "IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom, main = "El Nino & IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom, main = "La Nina & IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_finger_millet$FingerMilletAnom, main = "El Nino - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_finger_millet$FingerMilletAnom, main = "La Nina - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_finger_millet$FingerMilletAnom, main = "IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_finger_millet$FingerMilletAnom, main = "IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_finger_millet$FingerMilletAnom, main = "El Nino & IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_finger_millet$FingerMilletAnom, main = "La Nina & IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")

#el_nino_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

#la_nina_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# el_nino_iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# la_nina_iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom > 100)] <- 100
el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom < -100)] <- -100

el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom > 100)] <- 100
el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom < -100)] <- -100

el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom > 100)] <- 100
el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom < -100)] <- -100

el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom > 100)] <- 100
el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom < -100)] <- -100

el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom > 100)] <- 100
el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom < -100)] <- -100

la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom > 100)] <- 100
la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom < -100)] <- -100

la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom > 100)] <- 100
la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom < -100)] <- -100

la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom > 100)] <- 100
la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom < -100)] <- -100

la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom > 100)] <- 100
la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom < -100)] <- -100

la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom > 100)] <- 100
la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom < -100)] <- -100

iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom > 100)] <- 100
iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom < -100)] <- -100

iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom > 100)] <- 100
iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom < -100)] <- -100

iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom > 100)] <- 100
iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom < -100)] <- -100

iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom > 100)] <- 100
iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom < -100)] <- -100

iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom > 100)] <- 100
iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom < -100)] <- -100

iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom > 100)] <- 100
iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom < -100)] <- -100

iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom > 100)] <- 100
iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom < -100)] <- -100

iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom > 100)] <- 100
iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom < -100)] <- -100

iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom > 100)] <- 100
iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom < -100)] <- -100

iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom > 100)] <- 100
iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom < -100)] <- -100

el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom > 100)] <- 100
el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom < -100)] <- -100

el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom > 100)] <- 100
el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom < -100)] <- -100

el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 100)] <- 100
el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < -100)] <- -100

el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 100)] <- 100
el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < -100)] <- -100

el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 100)] <- 100
el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < -100)] <- -100

la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom > 100)] <- 100
la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom < -100)] <- -100

la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom > 100)] <- 100
la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom < -100)] <- -100

la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 100)] <- 100
la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < -100)] <- -100

la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 100)] <- 100
la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < -100)] <- -100

la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 100)] <- 100
la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < -100)] <- -100

# Merge the composite means with the shapefile
merge(VDSA_shape, el_nino_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, el_nino_iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_finger_millet, by = "Dist.Code")


el_nino_data_rice$Dist.Code <- as.character(el_nino_data_rice$Dist.Code)
el_nino_data_maize$Dist.Code <- as.character(el_nino_data_maize$Dist.Code)
el_nino_data_sorghum$Dist.Code <- as.character(el_nino_data_sorghum$Dist.Code)
el_nino_data_pearl_millet$Dist.Code <- as.character(el_nino_data_pearl_millet$Dist.Code)
el_nino_data_finger_millet$Dist.Code <- as.character(el_nino_data_finger_millet$Dist.Code)

la_nina_data_rice$Dist.Code <- as.character(la_nina_data_rice$Dist.Code)
la_nina_data_maize$Dist.Code <- as.character(la_nina_data_maize$Dist.Code)
la_nina_data_sorghum$Dist.Code <- as.character(la_nina_data_sorghum$Dist.Code)
la_nina_data_pearl_millet$Dist.Code <- as.character(la_nina_data_pearl_millet$Dist.Code)
la_nina_data_finger_millet$Dist.Code <- as.character(la_nina_data_finger_millet$Dist.Code)

iod_plus_data_rice$Dist.Code <- as.character(iod_plus_data_rice$Dist.Code)
iod_plus_data_maize$Dist.Code <- as.character(iod_plus_data_maize$Dist.Code)
iod_plus_data_sorghum$Dist.Code <- as.character(iod_plus_data_sorghum$Dist.Code)
iod_plus_data_pearl_millet$Dist.Code <- as.character(iod_plus_data_pearl_millet$Dist.Code)
iod_plus_data_finger_millet$Dist.Code <- as.character(iod_plus_data_finger_millet$Dist.Code)

iod_minus_data_rice$Dist.Code <- as.character(iod_minus_data_rice$Dist.Code)
iod_minus_data_maize$Dist.Code <- as.character(iod_minus_data_maize$Dist.Code)
iod_minus_data_sorghum$Dist.Code <- as.character(iod_minus_data_sorghum$Dist.Code)
iod_minus_data_pearl_millet$Dist.Code <- as.character(iod_minus_data_pearl_millet$Dist.Code)
iod_minus_data_finger_millet$Dist.Code <- as.character(iod_minus_data_finger_millet$Dist.Code)

el_nino_iod_plus_data_rice$Dist.Code <- as.character(el_nino_iod_plus_data_rice$Dist.Code)
el_nino_iod_plus_data_maize$Dist.Code <- as.character(el_nino_iod_plus_data_maize$Dist.Code)
el_nino_iod_plus_data_sorghum$Dist.Code <- as.character(el_nino_iod_plus_data_sorghum$Dist.Code)
el_nino_iod_plus_data_pearl_millet$Dist.Code <- as.character(el_nino_iod_plus_data_pearl_millet$Dist.Code)
el_nino_iod_plus_data_finger_millet$Dist.Code <- as.character(el_nino_iod_plus_data_finger_millet$Dist.Code)

la_nina_iod_minus_data_rice$Dist.Code <- as.character(la_nina_iod_minus_data_rice$Dist.Code)
la_nina_iod_minus_data_maize$Dist.Code <- as.character(la_nina_iod_minus_data_maize$Dist.Code)
la_nina_iod_minus_data_sorghum$Dist.Code <- as.character(la_nina_iod_minus_data_sorghum$Dist.Code)
la_nina_iod_minus_data_pearl_millet$Dist.Code <- as.character(la_nina_iod_minus_data_pearl_millet$Dist.Code)
la_nina_iod_minus_data_finger_millet$Dist.Code <- as.character(la_nina_iod_minus_data_finger_millet$Dist.Code)

el_nino_map_data_rice <- full_join(mymap,  el_nino_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_maize <- full_join(mymap,  el_nino_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_sorghum <- full_join(mymap,  el_nino_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_pearl_millet <- full_join(mymap,  el_nino_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_finger_millet <- full_join(mymap,  el_nino_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_map_data_rice <- full_join(mymap,  la_nina_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_maize <- full_join(mymap,  la_nina_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_sorghum <- full_join(mymap,  la_nina_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_pearl_millet <- full_join(mymap,  la_nina_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_finger_millet <- full_join(mymap,  la_nina_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_plus_map_data_rice <- full_join(mymap,  iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_maize <- full_join(mymap,  iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_sorghum <- full_join(mymap,  iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_pearl_millet <- full_join(mymap,  iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_finger_millet <- full_join(mymap,  iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_minus_map_data_rice <- full_join(mymap,  iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_maize <- full_join(mymap,  iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_sorghum <- full_join(mymap,  iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_pearl_millet <- full_join(mymap,  iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_finger_millet <- full_join(mymap,  iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

el_nino_iod_plus_map_data_rice <- full_join(mymap,  el_nino_iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_maize <- full_join(mymap,  el_nino_iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_sorghum <- full_join(mymap,  el_nino_iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_pearl_millet <- full_join(mymap,  el_nino_iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_finger_millet <- full_join(mymap,  el_nino_iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_iod_minus_map_data_rice <- full_join(mymap,  la_nina_iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_maize <- full_join(mymap,  la_nina_iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_sorghum <- full_join(mymap,  la_nina_iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_pearl_millet <- full_join(mymap,  la_nina_iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_finger_millet <- full_join(mymap,  la_nina_iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot(el_nino_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD+ Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD+ Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD+ Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD+ Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD+ Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD- Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD- Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD- Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD- Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("IOD- Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino & IOD+ Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino & IOD+ Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino & IOD+ Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino & IOD+ Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("El Nino & IOD+ Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina & IOD- Rice Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina & IOD- Maize Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina & IOD- Sorghum Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina & IOD- Pearl Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-100,100), breaks = c(-100,-75,-50,-25,0,25,50,75,100), space = "Lab")+
  ggtitle("La Nina & IOD- Finger Millet Prod Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


###Harvested Area


# Subset the data for each phase
el_nino_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_plus_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_plus_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_plus_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_plus_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_plus_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

iod_minus_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

iod_minus_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

iod_minus_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

iod_minus_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

iod_minus_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

el_nino_iod_plus_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

el_nino_iod_plus_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

el_nino_iod_plus_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

el_nino_iod_plus_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_rice <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(RICE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceAnom = mean(RiceAnom, na.rm = TRUE))

la_nina_iod_minus_data_maize <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(MAIZE.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeAnom = mean(MaizeAnom, na.rm = TRUE))

la_nina_iod_minus_data_sorghum <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumKharifAnom = mean(SorghumKharifAnom, na.rm = TRUE))

la_nina_iod_minus_data_pearl_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletAnom = mean(PearlMilletAnom, na.rm = TRUE))

la_nina_iod_minus_data_finger_millet <- Harv_Irr_Area %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletAnom = mean(FingerMilletAnom, na.rm = TRUE))

hist(el_nino_data_rice$RiceAnom, main = "El Nino - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_rice$RiceAnom, main = "La Nina - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_rice$RiceAnom, main = "IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_rice$RiceAnom, main = "IOD- - Rice Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_rice$RiceAnom, main = "El Nino & IOD+ - Rice Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_rice$RiceAnom, main = "La Nina & IOD- - Rice Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_maize$MaizeAnom, main = "El Nino - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_maize$MaizeAnom, main = "La Nina - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_maize$MaizeAnom, main = "IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_maize$MaizeAnom, main = "IOD- - Maize Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_maize$MaizeAnom, main = "El Nino & IOD+ - Maize Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_maize$MaizeAnom, main = "La Nina & IOD- - Maize Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_sorghum$SorghumKharifAnom, main = "El Nino - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_sorghum$SorghumKharifAnom, main = "La Nina - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_sorghum$SorghumKharifAnom, main = "IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_sorghum$SorghumKharifAnom, main = "IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_sorghum$SorghumKharifAnom, main = "El Nino & IOD+ - SorghumKharif Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_sorghum$SorghumKharifAnom, main = "La Nina & IOD- - SorghumKharif Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_pearl_millet$PearlMilletAnom, main = "El Nino - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_pearl_millet$PearlMilletAnom, main = "La Nina - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_pearl_millet$PearlMilletAnom, main = "IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_pearl_millet$PearlMilletAnom, main = "IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom, main = "El Nino & IOD+ - PearlMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom, main = "La Nina & IOD- - PearlMillet Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_finger_millet$FingerMilletAnom, main = "El Nino - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_finger_millet$FingerMilletAnom, main = "La Nina - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_finger_millet$FingerMilletAnom, main = "IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_finger_millet$FingerMilletAnom, main = "IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_finger_millet$FingerMilletAnom, main = "El Nino & IOD+ - FingerMillet Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_finger_millet$FingerMilletAnom, main = "La Nina & IOD- - FingerMillet Anomaly", xlab = "Anomaly Value")

#el_nino_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

#la_nina_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# el_nino_iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_iod_plus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_iod_plus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

# la_nina_iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_iod_minus_data_rice$RiceAnom > 0, na.rm = TRUE)
cat("Rice Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_iod_minus_data_maize$MaizeAnom > 0, na.rm = TRUE)
cat("Maize Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 0, na.rm = TRUE)
cat("SorghumKharif Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 0, na.rm = TRUE)
cat("PearlMillet Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 0, na.rm = TRUE)
cat("FingerMillet Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Anomaly > 0:", finger_millet_gt_0, "\n")

el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom > 30)] <- 30
el_nino_data_rice$RiceAnom[which(el_nino_data_rice$RiceAnom < -30)] <- -30

el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom > 30)] <- 30
el_nino_data_maize$MaizeAnom[which(el_nino_data_maize$MaizeAnom < -30)] <- -30

el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom > 30)] <- 30
el_nino_data_sorghum$SorghumKharifAnom[which(el_nino_data_sorghum$SorghumKharifAnom < -30)] <- -30

el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom > 30)] <- 30
el_nino_data_pearl_millet$PearlMilletAnom[which(el_nino_data_pearl_millet$PearlMilletAnom < -30)] <- -30

el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom > 30)] <- 30
el_nino_data_finger_millet$FingerMilletAnom[which(el_nino_data_finger_millet$FingerMilletAnom < -30)] <- -30

la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom > 30)] <- 30
la_nina_data_rice$RiceAnom[which(la_nina_data_rice$RiceAnom < -30)] <- -30

la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom > 30)] <- 30
la_nina_data_maize$MaizeAnom[which(la_nina_data_maize$MaizeAnom < -30)] <- -30

la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom > 30)] <- 30
la_nina_data_sorghum$SorghumKharifAnom[which(la_nina_data_sorghum$SorghumKharifAnom < -30)] <- -30

la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom > 30)] <- 30
la_nina_data_pearl_millet$PearlMilletAnom[which(la_nina_data_pearl_millet$PearlMilletAnom < -30)] <- -30

la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom > 30)] <- 30
la_nina_data_finger_millet$FingerMilletAnom[which(la_nina_data_finger_millet$FingerMilletAnom < -30)] <- -30

iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom > 30)] <- 30
iod_plus_data_rice$RiceAnom[which(iod_plus_data_rice$RiceAnom < -30)] <- -30

iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom > 30)] <- 30
iod_plus_data_maize$MaizeAnom[which(iod_plus_data_maize$MaizeAnom < -30)] <- -30

iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom > 30)] <- 30
iod_plus_data_sorghum$SorghumKharifAnom[which(iod_plus_data_sorghum$SorghumKharifAnom < -30)] <- -30

iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom > 30)] <- 30
iod_plus_data_pearl_millet$PearlMilletAnom[which(iod_plus_data_pearl_millet$PearlMilletAnom < -30)] <- -30

iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom > 30)] <- 30
iod_plus_data_finger_millet$FingerMilletAnom[which(iod_plus_data_finger_millet$FingerMilletAnom < -30)] <- -30

iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom > 30)] <- 30
iod_minus_data_rice$RiceAnom[which(iod_minus_data_rice$RiceAnom < -30)] <- -30

iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom > 30)] <- 30
iod_minus_data_maize$MaizeAnom[which(iod_minus_data_maize$MaizeAnom < -30)] <- -30

iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom > 30)] <- 30
iod_minus_data_sorghum$SorghumKharifAnom[which(iod_minus_data_sorghum$SorghumKharifAnom < -30)] <- -30

iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom > 30)] <- 30
iod_minus_data_pearl_millet$PearlMilletAnom[which(iod_minus_data_pearl_millet$PearlMilletAnom < -30)] <- -30

iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom > 30)] <- 30
iod_minus_data_finger_millet$FingerMilletAnom[which(iod_minus_data_finger_millet$FingerMilletAnom < -30)] <- -30

el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom > 30)] <- 30
el_nino_iod_plus_data_rice$RiceAnom[which(el_nino_iod_plus_data_rice$RiceAnom < -30)] <- -30

el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom > 30)] <- 30
el_nino_iod_plus_data_maize$MaizeAnom[which(el_nino_iod_plus_data_maize$MaizeAnom < -30)] <- -30

el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom > 30)] <- 30
el_nino_iod_plus_data_sorghum$SorghumKharifAnom[which(el_nino_iod_plus_data_sorghum$SorghumKharifAnom < -30)] <- -30

el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom > 30)] <- 30
el_nino_iod_plus_data_pearl_millet$PearlMilletAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletAnom < -30)] <- -30

el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom > 30)] <- 30
el_nino_iod_plus_data_finger_millet$FingerMilletAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletAnom < -30)] <- -30

la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom > 30)] <- 30
la_nina_iod_minus_data_rice$RiceAnom[which(la_nina_iod_minus_data_rice$RiceAnom < -30)] <- -30

la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom > 30)] <- 30
la_nina_iod_minus_data_maize$MaizeAnom[which(la_nina_iod_minus_data_maize$MaizeAnom < -30)] <- -30

la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom > 30)] <- 30
la_nina_iod_minus_data_sorghum$SorghumKharifAnom[which(la_nina_iod_minus_data_sorghum$SorghumKharifAnom < -30)] <- -30

la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom > 30)] <- 30
la_nina_iod_minus_data_pearl_millet$PearlMilletAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletAnom < -30)] <- -30

la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom > 30)] <- 30
la_nina_iod_minus_data_finger_millet$FingerMilletAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletAnom < -30)] <- -30

# Merge the composite means with the shapefile
merge(VDSA_shape, el_nino_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, el_nino_iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_finger_millet, by = "Dist.Code")

el_nino_data_rice$Dist.Code <- as.character(el_nino_data_rice$Dist.Code)
el_nino_data_maize$Dist.Code <- as.character(el_nino_data_maize$Dist.Code)
el_nino_data_sorghum$Dist.Code <- as.character(el_nino_data_sorghum$Dist.Code)
el_nino_data_pearl_millet$Dist.Code <- as.character(el_nino_data_pearl_millet$Dist.Code)
el_nino_data_finger_millet$Dist.Code <- as.character(el_nino_data_finger_millet$Dist.Code)

la_nina_data_rice$Dist.Code <- as.character(la_nina_data_rice$Dist.Code)
la_nina_data_maize$Dist.Code <- as.character(la_nina_data_maize$Dist.Code)
la_nina_data_sorghum$Dist.Code <- as.character(la_nina_data_sorghum$Dist.Code)
la_nina_data_pearl_millet$Dist.Code <- as.character(la_nina_data_pearl_millet$Dist.Code)
la_nina_data_finger_millet$Dist.Code <- as.character(la_nina_data_finger_millet$Dist.Code)

iod_plus_data_rice$Dist.Code <- as.character(iod_plus_data_rice$Dist.Code)
iod_plus_data_maize$Dist.Code <- as.character(iod_plus_data_maize$Dist.Code)
iod_plus_data_sorghum$Dist.Code <- as.character(iod_plus_data_sorghum$Dist.Code)
iod_plus_data_pearl_millet$Dist.Code <- as.character(iod_plus_data_pearl_millet$Dist.Code)
iod_plus_data_finger_millet$Dist.Code <- as.character(iod_plus_data_finger_millet$Dist.Code)

iod_minus_data_rice$Dist.Code <- as.character(iod_minus_data_rice$Dist.Code)
iod_minus_data_maize$Dist.Code <- as.character(iod_minus_data_maize$Dist.Code)
iod_minus_data_sorghum$Dist.Code <- as.character(iod_minus_data_sorghum$Dist.Code)
iod_minus_data_pearl_millet$Dist.Code <- as.character(iod_minus_data_pearl_millet$Dist.Code)
iod_minus_data_finger_millet$Dist.Code <- as.character(iod_minus_data_finger_millet$Dist.Code)

el_nino_iod_plus_data_rice$Dist.Code <- as.character(el_nino_iod_plus_data_rice$Dist.Code)
el_nino_iod_plus_data_maize$Dist.Code <- as.character(el_nino_iod_plus_data_maize$Dist.Code)
el_nino_iod_plus_data_sorghum$Dist.Code <- as.character(el_nino_iod_plus_data_sorghum$Dist.Code)
el_nino_iod_plus_data_pearl_millet$Dist.Code <- as.character(el_nino_iod_plus_data_pearl_millet$Dist.Code)
el_nino_iod_plus_data_finger_millet$Dist.Code <- as.character(el_nino_iod_plus_data_finger_millet$Dist.Code)

la_nina_iod_minus_data_rice$Dist.Code <- as.character(la_nina_iod_minus_data_rice$Dist.Code)
la_nina_iod_minus_data_maize$Dist.Code <- as.character(la_nina_iod_minus_data_maize$Dist.Code)
la_nina_iod_minus_data_sorghum$Dist.Code <- as.character(la_nina_iod_minus_data_sorghum$Dist.Code)
la_nina_iod_minus_data_pearl_millet$Dist.Code <- as.character(la_nina_iod_minus_data_pearl_millet$Dist.Code)
la_nina_iod_minus_data_finger_millet$Dist.Code <- as.character(la_nina_iod_minus_data_finger_millet$Dist.Code)

el_nino_map_data_rice <- full_join(mymap,  el_nino_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_maize <- full_join(mymap,  el_nino_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_sorghum <- full_join(mymap,  el_nino_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_pearl_millet <- full_join(mymap,  el_nino_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_finger_millet <- full_join(mymap,  el_nino_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_map_data_rice <- full_join(mymap,  la_nina_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_maize <- full_join(mymap,  la_nina_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_sorghum <- full_join(mymap,  la_nina_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_pearl_millet <- full_join(mymap,  la_nina_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_finger_millet <- full_join(mymap,  la_nina_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_plus_map_data_rice <- full_join(mymap,  iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_maize <- full_join(mymap,  iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_sorghum <- full_join(mymap,  iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_pearl_millet <- full_join(mymap,  iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_finger_millet <- full_join(mymap,  iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_minus_map_data_rice <- full_join(mymap,  iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_maize <- full_join(mymap,  iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_sorghum <- full_join(mymap,  iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_pearl_millet <- full_join(mymap,  iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_finger_millet <- full_join(mymap,  iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

el_nino_iod_plus_map_data_rice <- full_join(mymap,  el_nino_iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_maize <- full_join(mymap,  el_nino_iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_sorghum <- full_join(mymap,  el_nino_iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_pearl_millet <- full_join(mymap,  el_nino_iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_finger_millet <- full_join(mymap,  el_nino_iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_iod_minus_map_data_rice <- full_join(mymap,  la_nina_iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_maize <- full_join(mymap,  la_nina_iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_sorghum <- full_join(mymap,  la_nina_iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_pearl_millet <- full_join(mymap,  la_nina_iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_finger_millet <- full_join(mymap,  la_nina_iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot(el_nino_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumKharifAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg


###Irrigated Area


# Subset the data for each phase
el_nino_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

el_nino_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

el_nino_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

el_nino_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

el_nino_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

la_nina_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

la_nina_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

la_nina_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

la_nina_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

la_nina_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

iod_plus_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

iod_plus_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

iod_plus_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

iod_plus_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

iod_plus_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

iod_minus_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

iod_minus_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

iod_minus_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

iod_minus_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

iod_minus_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

el_nino_iod_plus_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

el_nino_iod_plus_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

el_nino_iod_plus_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

el_nino_iod_plus_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

el_nino_iod_plus_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% el_nino_iod_plus_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

la_nina_iod_minus_data_rice <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(RICE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(RiceIrrAnom = mean(RiceIrrAnom, na.rm = TRUE))

la_nina_iod_minus_data_maize <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(MAIZE.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(MaizeIrrAnom = mean(MaizeIrrAnom, na.rm = TRUE))

la_nina_iod_minus_data_sorghum <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(KHARIF.SORGHUM.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(SorghumIrrAnom = mean(SorghumIrrAnom, na.rm = TRUE))

la_nina_iod_minus_data_pearl_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(PEARL.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(PearlMilletIrrAnom = mean(PearlMilletIrrAnom, na.rm = TRUE))

la_nina_iod_minus_data_finger_millet <- Harv_Irr %>%
  dplyr::filter(Year %in% la_nina_iod_minus_years) %>%
  dplyr::filter(!(FINGER.MILLET.IRRIGATED.AREA..1000.ha. %in% c(0, 0.00, NA, -1))) %>%
  dplyr::group_by(Dist.Code) %>%
  dplyr::summarise(FingerMilletIrrAnom = mean(FingerMilletIrrAnom, na.rm = TRUE))

hist(el_nino_data_rice$RiceIrrAnom, main = "El Nino - Rice Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_rice$RiceIrrAnom, main = "La Nina - Rice Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_rice$RiceIrrAnom, main = "IOD+ - Rice Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_rice$RiceIrrAnom, main = "IOD- - Rice Irrigation Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_rice$RiceIrrAnom, main = "El Nino & IOD+ - Rice Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_rice$RiceIrrAnom, main = "La Nina & IOD- - Rice Irrigation Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_maize$MaizeIrrAnom, main = "El Nino - Maize Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_maize$MaizeIrrAnom, main = "La Nina - Maize Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_maize$MaizeIrrAnom, main = "IOD+ - Maize Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_maize$MaizeIrrAnom, main = "IOD- - Maize Irrigation Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_maize$MaizeIrrAnom, main = "El Nino & IOD+ - Maize Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_maize$MaizeIrrAnom, main = "La Nina & IOD- - Maize Irrigation Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_sorghum$SorghumIrrAnom, main = "El Nino - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_sorghum$SorghumIrrAnom, main = "La Nina - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_sorghum$SorghumIrrAnom, main = "IOD+ - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_sorghum$SorghumIrrAnom, main = "IOD- - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_sorghum$SorghumIrrAnom, main = "El Nino & IOD+ - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_sorghum$SorghumIrrAnom, main = "La Nina & IOD- - SorghumKharif Irrigation Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_pearl_millet$PearlMilletIrrAnom, main = "El Nino - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_pearl_millet$PearlMilletIrrAnom, main = "La Nina - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_pearl_millet$PearlMilletIrrAnom, main = "IOD+ - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_pearl_millet$PearlMilletIrrAnom, main = "IOD- - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom, main = "El Nino & IOD+ - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom, main = "La Nina & IOD- - PearlMillet Irrigation Anomaly", xlab = "Anomaly Value")

hist(el_nino_data_finger_millet$FingerMilletIrrAnom, main = "El Nino - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_data_finger_millet$FingerMilletIrrAnom, main = "La Nina - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_plus_data_finger_millet$FingerMilletIrrAnom, main = "IOD+ - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(iod_minus_data_finger_millet$FingerMilletIrrAnom, main = "IOD- - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom, main = "El Nino & IOD+ - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")
hist(la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom, main = "La Nina & IOD- - FingerMillet Irrigation Anomaly", xlab = "Anomaly Value")

#el_nino_mean

# For Rice Crop Irrigation Anomaly
rice_lt_0 <- sum(el_nino_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Irrigation Anomaly
maize_lt_0 <- sum(el_nino_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Irrigation Anomaly
sorghum_lt_0 <- sum(el_nino_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Irrigation Anomaly
pearl_millet_lt_0 <- sum(el_nino_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Irrigation Anomaly
finger_millet_lt_0 <- sum(el_nino_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

#la_nina_mean

# For Rice Crop Irrigation Anomaly
rice_lt_0 <- sum(la_nina_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Irrigation Anomaly
maize_lt_0 <- sum(la_nina_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Irrigation Anomaly
sorghum_lt_0 <- sum(la_nina_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Irrigation Anomaly
pearl_millet_lt_0 <- sum(la_nina_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Irrigation Anomaly
finger_millet_lt_0 <- sum(la_nina_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_plus_mean

# For Rice Crop Irrigation Anomaly
rice_lt_0 <- sum(iod_plus_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_plus_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Irrigation Anomaly
maize_lt_0 <- sum(iod_plus_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_plus_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Irrigation Anomaly
sorghum_lt_0 <- sum(iod_plus_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_plus_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Irrigation Anomaly
pearl_millet_lt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_plus_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Irrigation Anomaly
finger_millet_lt_0 <- sum(iod_plus_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_plus_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

# iod_minus_mean

# For Rice Crop Irrigation Anomaly
rice_lt_0 <- sum(iod_minus_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(iod_minus_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Irrigation Anomaly
maize_lt_0 <- sum(iod_minus_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(iod_minus_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Irrigation Anomaly
sorghum_lt_0 <- sum(iod_minus_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(iod_minus_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Irrigation Anomaly
pearl_millet_lt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(iod_minus_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Irrigation Anomaly
finger_millet_lt_0 <- sum(iod_minus_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(iod_minus_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

# el_nino_iod_plus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(el_nino_iod_plus_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(el_nino_iod_plus_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(el_nino_iod_plus_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(el_nino_iod_plus_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(el_nino_iod_plus_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

# la_nina_iod_minus_mean

# For Rice Crop Anomaly
rice_lt_0 <- sum(la_nina_iod_minus_data_rice$RiceIrrAnom < 0, na.rm = TRUE)
rice_gt_0 <- sum(la_nina_iod_minus_data_rice$RiceIrrAnom > 0, na.rm = TRUE)
cat("Rice Irrigation Anomaly < 0:", rice_lt_0, "\n")
cat("Rice Irrigation Anomaly > 0:", rice_gt_0, "\n")

# For Maize Crop Anomaly
maize_lt_0 <- sum(la_nina_iod_minus_data_maize$MaizeIrrAnom < 0, na.rm = TRUE)
maize_gt_0 <- sum(la_nina_iod_minus_data_maize$MaizeIrrAnom > 0, na.rm = TRUE)
cat("Maize Irrigation Anomaly < 0:", maize_lt_0, "\n")
cat("Maize Irrigation Anomaly > 0:", maize_gt_0, "\n")

# For SorghumKharif Crop Anomaly
sorghum_lt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumIrrAnom < 0, na.rm = TRUE)
sorghum_gt_0 <- sum(la_nina_iod_minus_data_sorghum$SorghumIrrAnom > 0, na.rm = TRUE)
cat("SorghumKharif Irrigation Anomaly < 0:", sorghum_lt_0, "\n")
cat("SorghumKharif Irrigation Anomaly > 0:", sorghum_gt_0, "\n")

# For PearlMillet Crop Anomaly
pearl_millet_lt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom < 0, na.rm = TRUE)
pearl_millet_gt_0 <- sum(la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom > 0, na.rm = TRUE)
cat("PearlMillet Irrigation Anomaly < 0:", pearl_millet_lt_0, "\n")
cat("PearlMillet Irrigation Anomaly > 0:", pearl_millet_gt_0, "\n")

# For FingerMillet Crop Anomaly
finger_millet_lt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom < 0, na.rm = TRUE)
finger_millet_gt_0 <- sum(la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom > 0, na.rm = TRUE)
cat("FingerMillet Irrigation Anomaly < 0:", finger_millet_lt_0, "\n")
cat("FingerMillet Irrigation Anomaly > 0:", finger_millet_gt_0, "\n")

el_nino_data_rice$RiceIrrAnom[which(el_nino_data_rice$RiceIrrAnom > 30)] <- 30
el_nino_data_rice$RiceIrrAnom[which(el_nino_data_rice$RiceIrrAnom < -30)] <- -30

el_nino_data_maize$MaizeIrrAnom[which(el_nino_data_maize$MaizeIrrAnom > 30)] <- 30
el_nino_data_maize$MaizeIrrAnom[which(el_nino_data_maize$MaizeIrrAnom < -30)] <- -30

el_nino_data_sorghum$SorghumIrrAnom[which(el_nino_data_sorghum$SorghumIrrAnom > 30)] <- 30
el_nino_data_sorghum$SorghumIrrAnom[which(el_nino_data_sorghum$SorghumIrrAnom < -30)] <- -30

el_nino_data_pearl_millet$PearlMilletIrrAnom[which(el_nino_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
el_nino_data_pearl_millet$PearlMilletIrrAnom[which(el_nino_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

el_nino_data_finger_millet$FingerMilletIrrAnom[which(el_nino_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
el_nino_data_finger_millet$FingerMilletIrrAnom[which(el_nino_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

la_nina_data_rice$RiceIrrAnom[which(la_nina_data_rice$RiceIrrAnom > 30)] <- 30
la_nina_data_rice$RiceIrrAnom[which(la_nina_data_rice$RiceIrrAnom < -30)] <- -30

la_nina_data_maize$MaizeIrrAnom[which(la_nina_data_maize$MaizeIrrAnom > 30)] <- 30
la_nina_data_maize$MaizeIrrAnom[which(la_nina_data_maize$MaizeIrrAnom < -30)] <- -30

la_nina_data_sorghum$SorghumIrrAnom[which(la_nina_data_sorghum$SorghumIrrAnom > 30)] <- 30
la_nina_data_sorghum$SorghumIrrAnom[which(la_nina_data_sorghum$SorghumIrrAnom < -30)] <- -30

la_nina_data_pearl_millet$PearlMilletIrrAnom[which(la_nina_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
la_nina_data_pearl_millet$PearlMilletIrrAnom[which(la_nina_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

la_nina_data_finger_millet$FingerMilletIrrAnom[which(la_nina_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
la_nina_data_finger_millet$FingerMilletIrrAnom[which(la_nina_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

iod_plus_data_rice$RiceIrrAnom[which(iod_plus_data_rice$RiceIrrAnom > 30)] <- 30
iod_plus_data_rice$RiceIrrAnom[which(iod_plus_data_rice$RiceIrrAnom < -30)] <- -30

iod_plus_data_maize$MaizeIrrAnom[which(iod_plus_data_maize$MaizeIrrAnom > 30)] <- 30
iod_plus_data_maize$MaizeIrrAnom[which(iod_plus_data_maize$MaizeIrrAnom < -30)] <- -30

iod_plus_data_sorghum$SorghumIrrAnom[which(iod_plus_data_sorghum$SorghumIrrAnom > 30)] <- 30
iod_plus_data_sorghum$SorghumIrrAnom[which(iod_plus_data_sorghum$SorghumIrrAnom < -30)] <- -30

iod_plus_data_pearl_millet$PearlMilletIrrAnom[which(iod_plus_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
iod_plus_data_pearl_millet$PearlMilletIrrAnom[which(iod_plus_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

iod_plus_data_finger_millet$FingerMilletIrrAnom[which(iod_plus_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
iod_plus_data_finger_millet$FingerMilletIrrAnom[which(iod_plus_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

iod_minus_data_rice$RiceIrrAnom[which(iod_minus_data_rice$RiceIrrAnom > 30)] <- 30
iod_minus_data_rice$RiceIrrAnom[which(iod_minus_data_rice$RiceIrrAnom < -30)] <- -30

iod_minus_data_maize$MaizeIrrAnom[which(iod_minus_data_maize$MaizeIrrAnom > 30)] <- 30
iod_minus_data_maize$MaizeIrrAnom[which(iod_minus_data_maize$MaizeIrrAnom < -30)] <- -30

iod_minus_data_sorghum$SorghumIrrAnom[which(iod_minus_data_sorghum$SorghumIrrAnom > 30)] <- 30
iod_minus_data_sorghum$SorghumIrrAnom[which(iod_minus_data_sorghum$SorghumIrrAnom < -30)] <- -30

iod_minus_data_pearl_millet$PearlMilletIrrAnom[which(iod_minus_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
iod_minus_data_pearl_millet$PearlMilletIrrAnom[which(iod_minus_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

iod_minus_data_finger_millet$FingerMilletIrrAnom[which(iod_minus_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
iod_minus_data_finger_millet$FingerMilletIrrAnom[which(iod_minus_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

el_nino_iod_plus_data_rice$RiceIrrAnom[which(el_nino_iod_plus_data_rice$RiceIrrAnom > 30)] <- 30
el_nino_iod_plus_data_rice$RiceIrrAnom[which(el_nino_iod_plus_data_rice$RiceIrrAnom < -30)] <- -30

el_nino_iod_plus_data_maize$MaizeIrrAnom[which(el_nino_iod_plus_data_maize$MaizeIrrAnom > 30)] <- 30
el_nino_iod_plus_data_maize$MaizeIrrAnom[which(el_nino_iod_plus_data_maize$MaizeIrrAnom < -30)] <- -30

el_nino_iod_plus_data_sorghum$SorghumIrrAnom[which(el_nino_iod_plus_data_sorghum$SorghumIrrAnom > 30)] <- 30
el_nino_iod_plus_data_sorghum$SorghumIrrAnom[which(el_nino_iod_plus_data_sorghum$SorghumIrrAnom < -30)] <- -30

el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom[which(el_nino_iod_plus_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom[which(el_nino_iod_plus_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

la_nina_iod_minus_data_rice$RiceIrrAnom[which(la_nina_iod_minus_data_rice$RiceIrrAnom > 30)] <- 30
la_nina_iod_minus_data_rice$RiceIrrAnom[which(la_nina_iod_minus_data_rice$RiceIrrAnom < -30)] <- -30

la_nina_iod_minus_data_maize$MaizeIrrAnom[which(la_nina_iod_minus_data_maize$MaizeIrrAnom > 30)] <- 30
la_nina_iod_minus_data_maize$MaizeIrrAnom[which(la_nina_iod_minus_data_maize$MaizeIrrAnom < -30)] <- -30

la_nina_iod_minus_data_sorghum$SorghumIrrAnom[which(la_nina_iod_minus_data_sorghum$SorghumIrrAnom > 30)] <- 30
la_nina_iod_minus_data_sorghum$SorghumIrrAnom[which(la_nina_iod_minus_data_sorghum$SorghumIrrAnom < -30)] <- -30

la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom > 30)] <- 30
la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom[which(la_nina_iod_minus_data_pearl_millet$PearlMilletIrrAnom < -30)] <- -30

la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom > 30)] <- 30
la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom[which(la_nina_iod_minus_data_finger_millet$FingerMilletIrrAnom < -30)] <- -30

# Merge the composite means with the shapefile
merge(VDSA_shape, el_nino_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, iod_minus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, el_nino_iod_plus_data_rice, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_maize, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, el_nino_iod_plus_data_finger_millet, by = "Dist.Code")

merge(VDSA_shape, la_nina_iod_minus_data_rice, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_maize, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_sorghum, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_pearl_millet, by = "Dist.Code")
merge(VDSA_shape, la_nina_iod_minus_data_finger_millet, by = "Dist.Code")

el_nino_data_rice$Dist.Code <- as.character(el_nino_data_rice$Dist.Code)
el_nino_data_maize$Dist.Code <- as.character(el_nino_data_maize$Dist.Code)
el_nino_data_sorghum$Dist.Code <- as.character(el_nino_data_sorghum$Dist.Code)
el_nino_data_pearl_millet$Dist.Code <- as.character(el_nino_data_pearl_millet$Dist.Code)
el_nino_data_finger_millet$Dist.Code <- as.character(el_nino_data_finger_millet$Dist.Code)

la_nina_data_rice$Dist.Code <- as.character(la_nina_data_rice$Dist.Code)
la_nina_data_maize$Dist.Code <- as.character(la_nina_data_maize$Dist.Code)
la_nina_data_sorghum$Dist.Code <- as.character(la_nina_data_sorghum$Dist.Code)
la_nina_data_pearl_millet$Dist.Code <- as.character(la_nina_data_pearl_millet$Dist.Code)
la_nina_data_finger_millet$Dist.Code <- as.character(la_nina_data_finger_millet$Dist.Code)

iod_plus_data_rice$Dist.Code <- as.character(iod_plus_data_rice$Dist.Code)
iod_plus_data_maize$Dist.Code <- as.character(iod_plus_data_maize$Dist.Code)
iod_plus_data_sorghum$Dist.Code <- as.character(iod_plus_data_sorghum$Dist.Code)
iod_plus_data_pearl_millet$Dist.Code <- as.character(iod_plus_data_pearl_millet$Dist.Code)
iod_plus_data_finger_millet$Dist.Code <- as.character(iod_plus_data_finger_millet$Dist.Code)

iod_minus_data_rice$Dist.Code <- as.character(iod_minus_data_rice$Dist.Code)
iod_minus_data_maize$Dist.Code <- as.character(iod_minus_data_maize$Dist.Code)
iod_minus_data_sorghum$Dist.Code <- as.character(iod_minus_data_sorghum$Dist.Code)
iod_minus_data_pearl_millet$Dist.Code <- as.character(iod_minus_data_pearl_millet$Dist.Code)
iod_minus_data_finger_millet$Dist.Code <- as.character(iod_minus_data_finger_millet$Dist.Code)

el_nino_iod_plus_data_rice$Dist.Code <- as.character(el_nino_iod_plus_data_rice$Dist.Code)
el_nino_iod_plus_data_maize$Dist.Code <- as.character(el_nino_iod_plus_data_maize$Dist.Code)
el_nino_iod_plus_data_sorghum$Dist.Code <- as.character(el_nino_iod_plus_data_sorghum$Dist.Code)
el_nino_iod_plus_data_pearl_millet$Dist.Code <- as.character(el_nino_iod_plus_data_pearl_millet$Dist.Code)
el_nino_iod_plus_data_finger_millet$Dist.Code <- as.character(el_nino_iod_plus_data_finger_millet$Dist.Code)

la_nina_iod_minus_data_rice$Dist.Code <- as.character(la_nina_iod_minus_data_rice$Dist.Code)
la_nina_iod_minus_data_maize$Dist.Code <- as.character(la_nina_iod_minus_data_maize$Dist.Code)
la_nina_iod_minus_data_sorghum$Dist.Code <- as.character(la_nina_iod_minus_data_sorghum$Dist.Code)
la_nina_iod_minus_data_pearl_millet$Dist.Code <- as.character(la_nina_iod_minus_data_pearl_millet$Dist.Code)
la_nina_iod_minus_data_finger_millet$Dist.Code <- as.character(la_nina_iod_minus_data_finger_millet$Dist.Code)

el_nino_map_data_rice <- full_join(mymap,  el_nino_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_maize <- full_join(mymap,  el_nino_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_sorghum <- full_join(mymap,  el_nino_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_pearl_millet <- full_join(mymap,  el_nino_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_map_data_finger_millet <- full_join(mymap,  el_nino_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_map_data_rice <- full_join(mymap,  la_nina_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_maize <- full_join(mymap,  la_nina_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_sorghum <- full_join(mymap,  la_nina_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_pearl_millet <- full_join(mymap,  la_nina_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_map_data_finger_millet <- full_join(mymap,  la_nina_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_plus_map_data_rice <- full_join(mymap,  iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_maize <- full_join(mymap,  iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_sorghum <- full_join(mymap,  iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_pearl_millet <- full_join(mymap,  iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_plus_map_data_finger_millet <- full_join(mymap,  iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

iod_minus_map_data_rice <- full_join(mymap,  iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_maize <- full_join(mymap,  iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_sorghum <- full_join(mymap,  iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_pearl_millet <- full_join(mymap,  iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
iod_minus_map_data_finger_millet <- full_join(mymap,  iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

el_nino_iod_plus_map_data_rice <- full_join(mymap,  el_nino_iod_plus_data_rice, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_maize <- full_join(mymap,  el_nino_iod_plus_data_maize, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_sorghum <- full_join(mymap,  el_nino_iod_plus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_pearl_millet <- full_join(mymap,  el_nino_iod_plus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
el_nino_iod_plus_map_data_finger_millet <- full_join(mymap,  el_nino_iod_plus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

la_nina_iod_minus_map_data_rice <- full_join(mymap,  la_nina_iod_minus_data_rice, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_maize <- full_join(mymap,  la_nina_iod_minus_data_maize, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_sorghum <- full_join(mymap,  la_nina_iod_minus_data_sorghum, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_pearl_millet <- full_join(mymap,  la_nina_iod_minus_data_pearl_millet, by = c("JDist.Code" = "Dist.Code"))
la_nina_iod_minus_map_data_finger_millet <- full_join(mymap,  la_nina_iod_minus_data_finger_millet, by = c("JDist.Code" = "Dist.Code"))

gg <- ggplot(el_nino_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD+ Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("IOD- Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(el_nino_iod_plus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("El Nino & IOD+ Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_rice) +
  geom_sf(aes(fill=RiceIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Rice Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_maize) +
  geom_sf(aes(fill=MaizeIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Maize Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_sorghum) +
  geom_sf(aes(fill=SorghumIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Sorghum Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_pearl_millet) +
  geom_sf(aes(fill=PearlMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Pearl Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

gg <- ggplot(la_nina_iod_minus_map_data_finger_millet) +
  geom_sf(aes(fill=FingerMilletIrrAnom)) +
  scale_fill_gradient2(low = "red", mid = "white", high = "navyblue", midpoint = 0, limit = c(-30,30), breaks = c(-30,-20,-10,0,10,20,30), space = "Lab")+
  ggtitle("La Nina & IOD- Finger Millet Area Anom")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme_void()
gg

