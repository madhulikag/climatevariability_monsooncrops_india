#Fig5&S7 Composite maps of temperature and rainfall


###Attribute table that has mean rainfall and mean maximum temperature data for all years at the district level

CRU_tmax <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/CRU/Tmax_detrend/tmax_anomalies_detrend_xr.csv")
colnames(CRU_tmax)[27:74] <- c(1968:2015)
CRU_tmax1 <- melt(CRU_tmax, id=c(1:26))
colnames(CRU_tmax1)[27:28] <- c("Year", "mean_tmax_anomaly")
CRU_tmax1

IMD_rain <- read.csv("/Users/madhulika/Desktop/Research/Simultaneous_extremes_impacts_on_yields/Data/OneDrive_1_18-03-2021/IMD_data/Rain_detrend/rain_anomalies_detrend_xr_2.csv")
colnames(IMD_rain)[27:74] <- c(1968:2015)
IMD_rain1 <- melt(IMD_rain, id=c(1:26))
colnames(IMD_rain1)[27:28] <- c("Year", "total_rain_anomaly")
IMD_rain1

tmax_rain <- merge(CRU_tmax1, IMD_rain1, by=c("GID_0", "NAME_0", "ID_0", "ISO", "ID_1", "NAME_1", "ID_2", "NAME_2",  "TYPE_2", "ENGTYPE_2", "NL_NAME_2", "VARNAME_2", "NAME_1a", "Ratio2", "Unsust_cur", "Cur_Total", "Frxn_Unsus", "layer", "path", "Dist_Name", "JNAME_1", "JVARNAME_2", "JNAME_1a", "JNAME_2", "JDist.Name", "JDist.Code", "Year", "JDist.Code"))
colnames(tmax_rain)[c(7,25,26)] <- c("ID", "Dist.Name", "Dist.Code")
tmax_rain

###El Nino & Tmax

forloop<-c(1968, 1969, 1972, 1977, 1982, 1987, 1991, 1993, 1994, 1997, 2002, 2004, 2009, 2015)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1968, Tmax_1969, Tmax_1972, Tmax_1977, Tmax_1982, Tmax_1987, Tmax_1991, Tmax_1993, Tmax_1994, Tmax_1997, Tmax_2002, Tmax_2004, Tmax_2009, Tmax_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1), space = "Lab")+
#     ggtitle("El Nino Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax0.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###La Nina & Tmax 

forloop<-c(1970, 1971, 1973, 1974, 1975, 1978, 1985, 1988, 1998, 1999, 2000, 2007, 2008, 2010, 2011, 2013)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1970, Tmax_1971, Tmax_1973, Tmax_1974, Tmax_1975, Tmax_1978, Tmax_1985, Tmax_1988, Tmax_1998, Tmax_1999, Tmax_2000, Tmax_2007, Tmax_2008, Tmax_2010, Tmax_2011, Tmax_2013))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("La Nina Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax1.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###IOD+ & Tmax

forloop<-c(1972, 1976, 1982, 1983, 1987, 1991, 1994, 1997, 2006, 2008, 2011, 2012, 2015)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1972, Tmax_1976, Tmax_1982, Tmax_1983, Tmax_1987, Tmax_1991, Tmax_1994, Tmax_1997, Tmax_2006, Tmax_2008, Tmax_2011, Tmax_2012, Tmax_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("IOD+ Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax2.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###IOD- & Tmax 

forloop<-c(1970, 1971, 1973, 1980, 1981, 1984, 1985, 1986, 1989, 1990, 1992, 1996, 1998, 2004, 2005, 2013, 2014)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1970, Tmax_1971, Tmax_1973, Tmax_1980, Tmax_1981, Tmax_1984, Tmax_1985, Tmax_1986, Tmax_1989, Tmax_1990, Tmax_1992, Tmax_1996,  Tmax_1998, Tmax_2004, Tmax_2005, Tmax_2013, Tmax_2014))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("IOD- Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax3.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###El Nino+IOD+ & Tmax

forloop<-c(1972, 1982, 1987, 1991, 1994, 1997, 2015)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1972, Tmax_1982, Tmax_1987, Tmax_1991, Tmax_1994, Tmax_1997, Tmax_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("El Nino+IOD+ Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax4.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###La Nina+IOD- & Tmax

forloop<-c(1970, 1971, 1973, 1985, 1998, 2013)

for(year in forloop){
  Tmax_year <- paste("Tmax_", year, sep="")
  
  Tmax_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, mean_tmax_anomaly)
  
  assign(Tmax_year, Tmax_anom)
  #Tmax_year_$Dist.Code <- as.character(Tmax_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Tmax_1970, Tmax_1971, Tmax_1973, Tmax_1985, Tmax_1998, Tmax_2013))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$mean_tmax_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$mean_tmax_anomaly<0,na.rm=TRUE)
sum(forplot$mean_tmax_anomaly>0,na.rm=TRUE)

forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly > 1)] <- 1
forplot$mean_tmax_anomaly[which(forplot$mean_tmax_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`mean_tmax_anomaly`)) +
#     scale_fill_gradient2(low = "dodgerblue", mid = "white", high = "coral", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("La Nina+IOD- Tmax Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Tmax5.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###El Nino & Rain

forloop<-c(1968, 1969, 1972, 1977, 1982, 1987, 1991, 1993, 1994, 1997, 2002, 2004, 2009, 2015)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1968, Rain_1969, Rain_1972, Rain_1977, Rain_1982, Rain_1987, Rain_1991, Rain_1993, Rain_1994, Rain_1997, Rain_2002, Rain_2004, Rain_2009, Rain_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), breaks = c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1), space = "Lab")+
#     ggtitle("El Nino Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain0.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###La Nina & Rain 

forloop<-c(1970, 1971, 1973, 1974, 1975, 1978, 1985, 1988, 1998, 1999, 2000, 2007, 2008, 2010, 2011, 2013)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1970, Rain_1971, Rain_1973, Rain_1974, Rain_1975, Rain_1978, Rain_1985, Rain_1988, Rain_1998, Rain_1999, Rain_2000, Rain_2007, Rain_2008, Rain_2010, Rain_2011, Rain_2013))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("La Nina Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain1.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)



###IOD+ & Rain

forloop<-c(1972, 1976, 1982, 1983, 1987, 1991, 1994, 1997, 2006, 2008, 2011, 2012, 2015)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1972, Rain_1976, Rain_1982, Rain_1983, Rain_1987, Rain_1991, Rain_1994, Rain_1997, Rain_2006, Rain_2008, Rain_2011, Rain_2012, Rain_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("IOD+ Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain2.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###IOD- & Rain 

forloop<-c(1970, 1971, 1973, 1980, 1981, 1984, 1985, 1986, 1989, 1990, 1992, 1996, 1998, 2004, 2005, 2013, 2014)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1970, Rain_1971, Rain_1973, Rain_1980, Rain_1981, Rain_1984, Rain_1985, Rain_1986, Rain_1989, Rain_1990, Rain_1992, Rain_1996,  Rain_1998, Rain_2004, Rain_2005, Rain_2013, Rain_2014))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("IOD- Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain3.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###El Nino+IOD+ & Rain

forloop<-c(1972, 1982, 1987, 1991, 1994, 1997, 2015)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1972, Rain_1982, Rain_1987, Rain_1991, Rain_1994, Rain_1997, Rain_2015))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("El Nino+IOD+ Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain4.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)


###La Nina+IOD- & Rain

forloop<-c(1970, 1971, 1973, 1985, 1998, 2013)

for(year in forloop){
  Rain_year <- paste("Rain_", year, sep="")
  
  Rain_anom <- tmax_rain %>%
    dplyr::filter(Year==year) %>%
    dplyr::select(Dist.Code, Dist.Name, ID, total_rain_anomaly)
  
  assign(Rain_year, Rain_anom)
  #Rain_year_$Dist.Code <- as.character(Rain_year_$Dist.Code)
  
}

forplot<-rbindlist(list(Rain_1970, Rain_1971, Rain_1973, Rain_1985, Rain_1998, Rain_2013))[,lapply(.SD,mean), list(Dist.Name)]
par(mfrow = c(2.5,2.5), oma = c(0, 4, 5, 0), mar = c(5, 6, 3, 1), cex.main = 2)
hist(forplot$total_rain_anomaly, xlab = "σ", ylab = "#Districts", cex.axis = 1.6, cex.lab = 1.6, breaks = seq(-2, 2, by = 0.1), main = "")
sum(forplot$total_rain_anomaly<0,na.rm=TRUE)
sum(forplot$total_rain_anomaly>0,na.rm=TRUE)

forplot$total_rain_anomaly[which(forplot$total_rain_anomaly > 1)] <- 1
forplot$total_rain_anomaly[which(forplot$total_rain_anomaly < -1)] <- -1

merge(VDSA_shape, forplot, by="Dist.Code")
forplot$Dist.Code <- as.character(forplot$Dist.Code)
map_and_data_forplot <- full_join(mymap,  forplot, by = c("JDist.Code" = "Dist.Code"))

# gg <- ggplot(map_and_data_forplot) +
#     geom_sf(aes(fill=`total_rain_anomaly`)) +
#     scale_fill_gradient2(low = "sienna", mid = "white", high = "darkgreen", midpoint = 0, limit = c(-1,1), space = "Lab")+
#     ggtitle("La Nina+IOD- Rain Anom")+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#     theme_void()
# gg
# ggsave(plot=gg, file=paste0("Rain5.pdf"), bg = "white", width = 44.45, height = 27.78, units = "cm", dpi=300)



climate_yield <- merge(Harv_Irr_Yield, tmax_rain, by=c("Year", "Dist.Code", "Dist.Name"))

climate_yield_phase <- merge(climate_yield, nino34_iod_phase, by='Year')

climate_yield_phase_rice <- climate_yield_phase[climate_yield_phase$RICE.YIELD..Kg.per.ha. != c(0,0.00,NA,-1), ]
climate_yield_phase_maize <- climate_yield_phase[climate_yield_phase$MAIZE.YIELD..Kg.per.ha. != c(0,0.00,NA,-1), ]
climate_yield_phase_sorghum <- climate_yield_phase[climate_yield_phase$KHARIF.SORGHUM.YIELD..Kg.per.ha. != c(0,0.00,NA,-1), ]
climate_yield_phase_pearl_millet <- climate_yield_phase[climate_yield_phase$PEARL.MILLET.YIELD..Kg.per.ha. != c(0,0.00,NA,-1), ]
climate_yield_phase_finger_millet <- climate_yield_phase[climate_yield_phase$FINGER.MILLET.YIELD..Kg.per.ha. != c(0,0.00,NA,-1), ]


columns_to_mean <- c('mean_tmax_anomaly', 'total_rain_anomaly')
climate_yield_phase_means <- climate_yield_phase %>%
  group_by(Dist.Code, phase) %>%
  summarise(across(all_of(columns_to_mean), mean, na.rm = TRUE))
climate_yield_phase_means


# Count the number of districts meeting the specified conditions
districts_count <- climate_yield_phase_means %>%
  filter(total_rain_anomaly < 0) %>%
  group_by(phase) %>%
  summarise(distinct_districts_count = n_distinct(Dist.Code))

cat("\nNumber of districts meeting the specified conditions:\n")
print(districts_count)



#'RiceAnom', 'MaizeAnom', 'SorghumKharifAnom', 'PearlMilletAnom', 'FingerMilletAnom',
columns_to_mean <- c('FingerMilletAnom',  'mean_tmax_anomaly', 'total_rain_anomaly')
climate_yield_means <- climate_yield 
climate_yield_phase_means <- climate_yield_phase_finger_millet %>%
  group_by(Dist.Code, phase) %>%
  summarise(across(all_of(columns_to_mean), mean, na.rm = TRUE))
climate_yield_phase_means


# Count the number of districts meeting the specified conditions
districts_count <- climate_yield_phase_means %>%
  filter(FingerMilletAnom < 0 & mean_tmax_anomaly < 0) %>%
  group_by(phase) %>%
  summarise(distinct_districts_count = n_distinct(Dist.Code))

cat("\nNumber of districts meeting the specified conditions:\n")
print(districts_count)


