# Fig3 Simultaneous declines in multiple crops and grains likely to reduce during El Niño and positive IOD events

# Absolute Yield Anomalies

yield_nino34_iod_phase <- dplyr::select(yield_nino34_iod, c(Year, wRiceAnom, wMaizeAnom, wSorghumAnom, wPearlMilletAnom, wFingerMilletAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
yieldanom_avg <- pivot_longer(yield_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "YieldAnom")
yieldanom_avg$Crop <- ifelse(yieldanom_avg$Crops=="wRiceAnom", "Rice", ifelse(yieldanom_avg$Crops=="wMaizeAnom", "Maize", ifelse(yieldanom_avg$Crops=="wSorghumAnom", "Sorghum", ifelse(yieldanom_avg$Crops=="wPearlMilletAnom", "PearlMillet", "FingerMillet"))))
yieldanom_avg$Crops <- NULL
yieldanom_avg$Crop <- factor(yieldanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
yieldanom_avg$phase <- ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
yieldanom_avg$phase1 <- ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="IOD+", "El Nino", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="IOD-", "La Nina", ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="IOD-", "El Nino", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
yieldanom_avg$phase2 <- ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldanom_avg$nino34phase=="Neutral" & yieldanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldanom_avg$nino34phase=="El Nino" & yieldanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldanom_avg$nino34phase=="La Nina" & yieldanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
yieldanom_avg$phase <- factor(yieldanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldanom_avg$phase1 <- factor(yieldanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldanom_avg$phase2 <- factor(yieldanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- yieldanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- yieldanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- yieldanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

yieldanom_avg$np <- ifelse(yieldanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(yieldanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
yieldanom_avg$np <- factor(yieldanom_avg$np, levels = c(".4El Nino",".3La Nina"))
yieldanom_avg$ip <- ifelse(yieldanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(yieldanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
yieldanom_avg$ip <- factor(yieldanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
yieldanom_avg$Crop1 <- ifelse(yieldanom_avg$Crop=="Rice", "5Rice", ifelse(yieldanom_avg$Crop=="Maize", "4Maize", ifelse(yieldanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(yieldanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
yieldanom_avg$YieldAnom <- ifelse(yieldanom_avg$YieldAnom <= -200, -200, ifelse(yieldanom_avg$YieldAnom >= 200, 200, yieldanom_avg$YieldAnom))
plot1 <- ggplot(yieldanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=YieldAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-200,200), breaks = seq(-200, 200, by = 50))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="right", legend.direction = "vertical", legend.key.height = unit(0.6, "cm"),  # Adjust the key height
        legend.key.width = unit(0.5, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8))  # Adjust the text size in the legend
plot1 +
  geom_point(data = yieldanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(yieldanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = yieldanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(yieldanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = yieldanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(yieldanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = yieldanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(yieldanom_avg$ip == ".1IOD-", "blue", "NA"))

yieldanom_avg1 <- yieldanom_avg
np <- subset(yieldanom_avg1, np!="NA")
ip <- subset(yieldanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$YieldAnom<0, 1, ifelse(np$np==".3La Nina" & np$YieldAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$YieldAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$YieldAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$YieldAnom>0, 1, ifelse(np$np==".3La Nina" & np$YieldAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$YieldAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$YieldAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "El Niño", ifelse(combined_data$phases==".3La Nina", "La Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("El Niño", "La Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("") +
  ggtitle("Yield") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=YieldAnom, fill=nino34phase), outlier.size = 0.0)+
  theme_classic()+
  xlab("")+
  ylab("Yield Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=YieldAnom, fill=iodphase), outlier.size = 0.0)+
  theme_classic()+
  xlab("")+
  ylab("Yield Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=YieldAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Yield Anomalies (kg/ha)")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=YieldAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Relative Yield Anomalies

yield_nino34_iod_phase <- dplyr::select(yield_nino34_iod, c(Year, wRicePerAnom, wMaizePerAnom, wSorghumPerAnom, wPearlMilletPerAnom, wFingerMilletPerAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
yieldperanom_avg <- pivot_longer(yield_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "PercentYieldAnom")
yieldperanom_avg$Crop <- ifelse(yieldperanom_avg$Crops=="wRicePerAnom", "Rice", ifelse(yieldperanom_avg$Crops=="wMaizePerAnom", "Maize", ifelse(yieldperanom_avg$Crops=="wSorghumPerAnom", "Sorghum", ifelse(yieldperanom_avg$Crops=="wPearlMilletPerAnom", "PearlMillet", "FingerMillet"))))
yieldperanom_avg$Crops <- NULL
yieldperanom_avg$Crop <- factor(yieldperanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
yieldperanom_avg$phase <- ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
yieldperanom_avg$phase1 <- ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="IOD+", "El Nino", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="IOD-", "La Nina", ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="IOD-", "El Nino", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
yieldperanom_avg$phase2 <- ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="Neutral", "El Nino", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="Neutral", "La Nina", ifelse(yieldperanom_avg$nino34phase=="Neutral" & yieldperanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="IOD+", "IOD+", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldperanom_avg$nino34phase=="El Nino" & yieldperanom_avg$iodphase=="IOD-", "IOD-", ifelse(yieldperanom_avg$nino34phase=="La Nina" & yieldperanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
yieldperanom_avg$phase <- factor(yieldperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
yieldperanom_avg$phase1 <- factor(yieldperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
yieldperanom_avg$phase2 <- factor(yieldperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- yieldperanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- yieldperanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- yieldperanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

yieldperanom_avg$np <- ifelse(yieldperanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(yieldperanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
yieldperanom_avg$np <- factor(yieldperanom_avg$np, levels = c(".4El Nino",".3La Nina"))
yieldperanom_avg$ip <- ifelse(yieldperanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(yieldperanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
yieldperanom_avg$ip <- factor(yieldperanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
yieldperanom_avg$Crop1 <- ifelse(yieldperanom_avg$Crop=="Rice", "5Rice", ifelse(yieldperanom_avg$Crop=="Maize", "4Maize", ifelse(yieldperanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(yieldperanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
yieldperanom_avg$PercentYieldAnom <- ifelse(yieldperanom_avg$PercentYieldAnom <= -20, -20, ifelse(yieldperanom_avg$PercentYieldAnom >= 20, 20, yieldperanom_avg$PercentYieldAnom))
plot1 <- ggplot(yieldperanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=PercentYieldAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-20,20), breaks = seq(-20, 20, by = 5))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.height = unit(0.5, "cm"),  # Adjust the key height
        legend.key.width = unit(1, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10))+  # Adjust the text size in the legend
  labs(fill = "%Yield Anomaly")
plot1 + 
  geom_point(data = yieldperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(yieldperanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = yieldperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(yieldperanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = yieldperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(yieldperanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = yieldperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(yieldperanom_avg$ip == ".1IOD-", "blue", "NA"))

yieldperanom_avg1 <- yieldperanom_avg
np <- subset(yieldperanom_avg1, np!="NA")
ip <- subset(yieldperanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentYieldAnom<0, 1, ifelse(np$np==".3La Nina" & np$PercentYieldAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentYieldAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentYieldAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentYieldAnom>0, 1, ifelse(np$np==".3La Nina" & np$PercentYieldAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentYieldAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentYieldAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "Niño", ifelse(combined_data$phases==".3La Nina", "Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("Niño", "Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("% Yield") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines")) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=PercentYieldAnom, fill=nino34phase), outlier.size = 0.0)+
  theme_classic()+
  xlab("")+
  ylab("Percent Yield Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=PercentYieldAnom, fill=iodphase), outlier.size = 0.0)+
  theme_classic()+
  xlab("")+
  ylab("Percent Yield Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

a <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=PercentYieldAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Yield Anomalies")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=PercentYieldAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Absolute Production Anomalies

prod_nino34_iod_phase <- dplyr::select(prod_nino34_iod, c(Year, wRiceAnom, wMaizeAnom, wSorghumAnom, wPearlMilletAnom, wFingerMilletAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
prodanom_avg <- pivot_longer(prod_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "ProdAnom")
prodanom_avg$Crop <- ifelse(prodanom_avg$Crops=="wRiceAnom", "Rice", ifelse(prodanom_avg$Crops=="wMaizeAnom", "Maize", ifelse(prodanom_avg$Crops=="wSorghumAnom", "Sorghum", ifelse(prodanom_avg$Crops=="wPearlMilletAnom", "PearlMillet", "FingerMillet"))))
prodanom_avg$Crops <- NULL
prodanom_avg$Crop <- factor(prodanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
prodanom_avg$phase <- ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
prodanom_avg$phase1 <- ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="IOD+", "El Nino", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="IOD-", "La Nina", ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="IOD-", "El Nino", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
prodanom_avg$phase2 <- ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodanom_avg$nino34phase=="Neutral" & prodanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodanom_avg$nino34phase=="El Nino" & prodanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodanom_avg$nino34phase=="La Nina" & prodanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
prodanom_avg$phase <- factor(prodanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodanom_avg$phase1 <- factor(prodanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodanom_avg$phase2 <- factor(prodanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- prodanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- prodanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- prodanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

prodanom_avg$np <- ifelse(prodanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(prodanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
prodanom_avg$np <- factor(prodanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
prodanom_avg$ip <- ifelse(prodanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(prodanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
prodanom_avg$ip <- factor(prodanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
prodanom_avg$Crop1 <- ifelse(prodanom_avg$Crop=="Rice", "5Rice", ifelse(prodanom_avg$Crop=="Maize", "4Maize", ifelse(prodanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(prodanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
prodanom_avg$ProdAnom <- ifelse(prodanom_avg$ProdAnom <= -80, -80, ifelse(prodanom_avg$ProdAnom >= 80, 80, prodanom_avg$ProdAnom))
# prodanom_avg <- subset(prodanom_avg, Year >= 1968 & Year <= 2015)
plot2 <- ggplot(prodanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=ProdAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-80,80), breaks = seq(-80, 80, by = 20))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="right", legend.direction = "vertical", legend.key.height = unit(0.6, "cm"),  # Adjust the key height
        legend.key.width = unit(0.5, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8))  # Adjust the text size in the legend
plot2 + 
  geom_point(data = prodanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(prodanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = prodanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(prodanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = prodanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(prodanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = prodanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(prodanom_avg$ip == ".1IOD-", "blue", "NA"))

prodanom_avg1 <- prodanom_avg
np <- subset(prodanom_avg1, np!="NA")
ip <- subset(prodanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$ProdAnom<0, 1, ifelse(np$np==".3La Nina" & np$ProdAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$ProdAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$ProdAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$ProdAnom>0, 1, ifelse(np$np==".3La Nina" & np$ProdAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$ProdAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$ProdAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "El Niño", ifelse(combined_data$phases==".3La Nina", "La Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("El Niño", "La Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("") +
  ggtitle("Production") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=ProdAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Production Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=ProdAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Production Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=ProdAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Production Anomalies (1000 tons)")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=ProdAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Relative Production Anomalies

prod_nino34_iod_phase <- dplyr::select(prod_nino34_iod, c(Year, wRicePerAnom, wMaizePerAnom, wSorghumPerAnom, wPearlMilletPerAnom, wFingerMilletPerAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
prodperanom_avg <- pivot_longer(prod_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "PercentProdAnom")
prodperanom_avg$Crop <- ifelse(prodperanom_avg$Crops=="wRicePerAnom", "Rice", ifelse(prodperanom_avg$Crops=="wMaizePerAnom", "Maize", ifelse(prodperanom_avg$Crops=="wSorghumPerAnom", "Sorghum", ifelse(prodperanom_avg$Crops=="wPearlMilletPerAnom", "PearlMillet", "FingerMillet"))))
prodperanom_avg$Crops <- NULL
prodperanom_avg$Crop <- factor(prodperanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
prodperanom_avg$phase <- ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
prodperanom_avg$phase1 <- ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="IOD+", "El Nino", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="IOD-", "La Nina", ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="IOD-", "El Nino", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
prodperanom_avg$phase2 <- ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="Neutral", "El Nino", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="Neutral", "La Nina", ifelse(prodperanom_avg$nino34phase=="Neutral" & prodperanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="IOD+", "IOD+", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodperanom_avg$nino34phase=="El Nino" & prodperanom_avg$iodphase=="IOD-", "IOD-", ifelse(prodperanom_avg$nino34phase=="La Nina" & prodperanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
prodperanom_avg$phase <- factor(prodperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
prodperanom_avg$phase1 <- factor(prodperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
prodperanom_avg$phase2 <- factor(prodperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- prodperanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- prodperanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- prodperanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

prodperanom_avg$np <- ifelse(prodperanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(prodperanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
prodperanom_avg$np <- factor(prodperanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
prodperanom_avg$ip <- ifelse(prodperanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(prodperanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
prodperanom_avg$ip <- factor(prodperanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
prodperanom_avg$Crop1 <- ifelse(prodperanom_avg$Crop=="Rice", "5Rice", ifelse(prodperanom_avg$Crop=="Maize", "4Maize", ifelse(prodperanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(prodperanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
prodperanom_avg$PercentProdAnom <- ifelse(prodperanom_avg$PercentProdAnom <= -20, -20, ifelse(prodperanom_avg$PercentProdAnom >= 20, 20, prodperanom_avg$PercentProdAnom))
plot2 <- ggplot(prodperanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=PercentProdAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-20,20), breaks = seq(-20, 20, by = 5))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.height = unit(0.5, "cm"),  # Adjust the key height
        legend.key.width = unit(1, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10))+  # Adjust the text size in the legend
  labs(fill = "%Production Anomaly")
plot2 + 
  geom_point(data = prodperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(prodperanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = prodperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(prodperanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = prodperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(prodperanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = prodperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(prodperanom_avg$ip == ".1IOD-", "blue", "NA"))

prodperanom_avg1 <- prodperanom_avg
np <- subset(prodperanom_avg1, np!="NA")
ip <- subset(prodperanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentProdAnom<0, 1, ifelse(np$np==".3La Nina" & np$PercentProdAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentProdAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentProdAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentProdAnom>0, 1, ifelse(np$np==".3La Nina" & np$PercentProdAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentProdAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentProdAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "Niño", ifelse(combined_data$phases==".3La Nina", "Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("Niño", "Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("% Production") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines")) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=PercentProdAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Percent Production Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=PercentProdAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Percent Production Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

b <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=PercentProdAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Production Anomalies")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=PercentProdAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Absolute Harvested Area Anomalies 

area_nino34_iod_phase <- dplyr::select(area_nino34_iod, c(Year, wRiceAnom, wMaizeAnom, wSorghumAnom, wPearlMilletAnom, wFingerMilletAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
areaanom_avg <- pivot_longer(area_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "AreaAnom")
areaanom_avg$Crop <- ifelse(areaanom_avg$Crops=="wRiceAnom", "Rice", ifelse(areaanom_avg$Crops=="wMaizeAnom", "Maize", ifelse(areaanom_avg$Crops=="wSorghumAnom", "Sorghum", ifelse(areaanom_avg$Crops=="wPearlMilletAnom", "PearlMillet", "FingerMillet"))))
areaanom_avg$Crops <- NULL
areaanom_avg$Crop <- factor(areaanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
areaanom_avg$phase <- ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="Neutral", "El Nino", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD+", "IOD+", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="Neutral", "La Nina", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
areaanom_avg$phase1 <- ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="Neutral", "El Nino", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD+", "IOD+", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="Neutral", "La Nina", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="IOD+", "El Nino", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="IOD-", "La Nina", ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="IOD-", "El Nino", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
areaanom_avg$phase2 <- ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="Neutral", "El Nino", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD+", "IOD+", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="Neutral", "La Nina", ifelse(areaanom_avg$nino34phase=="Neutral" & areaanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="IOD+", "IOD+", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaanom_avg$nino34phase=="El Nino" & areaanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaanom_avg$nino34phase=="La Nina" & areaanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
areaanom_avg$phase <- factor(areaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaanom_avg$phase1 <- factor(areaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaanom_avg$phase2 <- factor(areaanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- areaanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- areaanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- areaanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

areaanom_avg$np <- ifelse(areaanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(areaanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
areaanom_avg$np <- factor(areaanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
areaanom_avg$ip <- ifelse(areaanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(areaanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
areaanom_avg$ip <- factor(areaanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
areaanom_avg$Crop1 <- ifelse(areaanom_avg$Crop=="Rice", "5Rice", ifelse(areaanom_avg$Crop=="Maize", "4Maize", ifelse(areaanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(areaanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
areaanom_avg$AreaAnom <- ifelse(areaanom_avg$AreaAnom <= -20, -20, ifelse(areaanom_avg$AreaAnom >= 20, 20, areaanom_avg$AreaAnom))
plot2 <- ggplot(areaanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=AreaAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-20,20), breaks = seq(-20, 20, by = 5))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="right", legend.direction = "vertical", legend.key.height = unit(0.6, "cm"),  # Adjust the key height
        legend.key.width = unit(0.5, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8))  # Adjust the text size in the legend
plot2 + 
  geom_point(data = areaanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(areaanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = areaanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(areaanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = areaanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(areaanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = areaanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(areaanom_avg$ip == ".1IOD-", "blue", "NA"))

areaanom_avg1 <- areaanom_avg
np <- subset(areaanom_avg1, np!="NA")
ip <- subset(areaanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$AreaAnom<0, 1, ifelse(np$np==".3La Nina" & np$AreaAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$AreaAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$AreaAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$AreaAnom>0, 1, ifelse(np$np==".3La Nina" & np$AreaAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$AreaAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$AreaAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "El Niño", ifelse(combined_data$phases==".3La Nina", "La Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("El Niño", "La Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("") +
  ggtitle("Harvested Area") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=AreaAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Harvested Area Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=AreaAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Harvested Area Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=AreaAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Harvested Area Anomalies (1000 ha)")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=AreaAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-35,35))

# Relative Area Anomalies

area_nino34_iod_phase <- dplyr::select(area_nino34_iod, c(Year, wRicePerAnom, wMaizePerAnom, wSorghumPerAnom, wPearlMilletPerAnom, wFingerMilletPerAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))

areaperanom_avg <- pivot_longer(area_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "PercentAreaAnom")
areaperanom_avg$Crop <- ifelse(areaperanom_avg$Crops=="wRicePerAnom", "Rice", ifelse(areaperanom_avg$Crops=="wMaizePerAnom", "Maize", ifelse(areaperanom_avg$Crops=="wSorghumPerAnom", "Sorghum", ifelse(areaperanom_avg$Crops=="wPearlMilletPerAnom", "PearlMillet", "FingerMillet"))))
areaperanom_avg$Crops <- NULL
areaperanom_avg$Crop <- factor(areaperanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
areaperanom_avg$phase <- ifelse(areaperanom_avg$nino34phase=="El Nino" & areaperanom_avg$iodphase=="Neutral", "El Nino", ifelse(areaperanom_avg$nino34phase=="Neutral" & areaperanom_avg$iodphase=="IOD+", "IOD+", ifelse(areaperanom_avg$nino34phase=="La Nina" & areaperanom_avg$iodphase=="Neutral", "La Nina", ifelse(areaperanom_avg$nino34phase=="Neutral" & areaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(areaperanom_avg$nino34phase=="El Nino" & areaperanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(areaperanom_avg$nino34phase=="La Nina" & areaperanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
areaperanom_avg$phase1 <- ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "Neutral", "El Nino", ifelse(areaperanom_avg$nino34phase == "Neutral" & areaperanom_avg$iodphase == "IOD+", "IOD+", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "Neutral", "La Nina", ifelse(areaperanom_avg$nino34phase == "Neutral" & areaperanom_avg$iodphase == "IOD-", "IOD-",ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "IOD+", "El Nino", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "IOD-", "La Nina", ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "IOD-", "El Nino", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "IOD+", "La Nina", NA))))))))
areaperanom_avg$phase2 <- ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "Neutral", "El Nino", ifelse(areaperanom_avg$nino34phase == "Neutral" & areaperanom_avg$iodphase == "IOD+", "IOD+", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "Neutral", "La Nina", ifelse(areaperanom_avg$nino34phase == "Neutral" & areaperanom_avg$iodphase == "IOD-", "IOD-", ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "IOD+", "IOD+", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "IOD-", "IOD-", ifelse(areaperanom_avg$nino34phase == "El Nino" & areaperanom_avg$iodphase == "IOD-", "IOD-", ifelse(areaperanom_avg$nino34phase == "La Nina" & areaperanom_avg$iodphase == "IOD+", "IOD+", NA))))))))

areaperanom_avg$phase <- factor(areaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
areaperanom_avg$phase1 <- factor(areaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
areaperanom_avg$phase2 <- factor(areaperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- areaperanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- areaperanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- areaperanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

areaperanom_avg$np <- ifelse(areaperanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(areaperanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
areaperanom_avg$np <- factor(areaperanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
areaperanom_avg$ip <- ifelse(areaperanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(areaperanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
areaperanom_avg$ip <- factor(areaperanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
areaperanom_avg$Crop1 <- ifelse(areaperanom_avg$Crop=="Rice", "5Rice", ifelse(areaperanom_avg$Crop=="Maize", "4Maize", ifelse(areaperanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(areaperanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
areaperanom_avg$PercentAreaAnom <- ifelse(areaperanom_avg$PercentAreaAnom <= -20, -20, ifelse(areaperanom_avg$PercentAreaAnom >= 20, 20, areaperanom_avg$PercentAreaAnom))
plot2 <- ggplot(areaperanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=PercentAreaAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-20,20), breaks = seq(-20, 20, by = 5))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.height = unit(0.5, "cm"),  # Adjust the key height
        legend.key.width = unit(1, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10))+  # Adjust the text size in the legend
  labs(fill = "%Harvested Area Anomaly")
plot2 + 
  geom_point(data = areaperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(areaperanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = areaperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(areaperanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = areaperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(areaperanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = areaperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(areaperanom_avg$ip == ".1IOD-", "blue", "NA"))

areaperanom_avg1 <- areaperanom_avg
np <- subset(areaperanom_avg1, np!="NA")
ip <- subset(areaperanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentAreaAnom<0, 1, ifelse(np$np==".3La Nina" & np$PercentAreaAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentAreaAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentAreaAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentAreaAnom>0, 1, ifelse(np$np==".3La Nina" & np$PercentAreaAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentAreaAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentAreaAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "Niño", ifelse(combined_data$phases==".3La Nina", "Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("Niño", "Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("% Harvested Area") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines")) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=PercentAreaAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Percent Harvested Area Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=PercentAreaAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Percent Harvested Area Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

c <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=PercentAreaAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Harvested Area Anomalies")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=PercentAreaAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

# Absolute Irrigated Area Anomalies

irrarea_nino34_iod_phase <- dplyr::select(irrarea_nino34_iod, c(Year, wRiceIrrAnom, wMaizeIrrAnom, wSorghumIrrAnom, wPearlMilletIrrAnom, wFingerMilletIrrAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
irrareaanom_avg <- pivot_longer(irrarea_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "IrrAreaAnom")
irrareaanom_avg$Crop <- ifelse(irrareaanom_avg$Crops=="wRiceIrrAnom", "Rice", ifelse(irrareaanom_avg$Crops=="wMaizeIrrAnom", "Maize", ifelse(irrareaanom_avg$Crops=="wSorghumIrrAnom", "Sorghum", ifelse(irrareaanom_avg$Crops=="wPearlMilletIrrAnom", "PearlMillet", "FingerMillet"))))
irrareaanom_avg$Crops <- NULL
irrareaanom_avg$Crop <- factor(irrareaanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
irrareaanom_avg$phase <- ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
irrareaanom_avg$phase1 <- ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="IOD+", "El Nino", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="IOD-", "La Nina", ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="IOD-", "El Nino", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
irrareaanom_avg$phase2 <- ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaanom_avg$nino34phase=="Neutral" & irrareaanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaanom_avg$nino34phase=="El Nino" & irrareaanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaanom_avg$nino34phase=="La Nina" & irrareaanom_avg$iodphase=="IOD+", "IOD+", NA))))))))
irrareaanom_avg$phase <- factor(irrareaanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
irrareaanom_avg$phase1 <- factor(irrareaanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
irrareaanom_avg$phase2 <- factor(irrareaanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- irrareaanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- irrareaanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- irrareaanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

irrareaanom_avg$np <- ifelse(irrareaanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(irrareaanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
irrareaanom_avg$np <- factor(irrareaanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
irrareaanom_avg$ip <- ifelse(irrareaanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(irrareaanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
irrareaanom_avg$ip <- factor(irrareaanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
irrareaanom_avg$Crop1 <- ifelse(irrareaanom_avg$Crop=="Rice", "5Rice", ifelse(irrareaanom_avg$Crop=="Maize", "4Maize", ifelse(irrareaanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(irrareaanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
irrareaanom_avg$IrrAreaAnom <- ifelse(irrareaanom_avg$IrrAreaAnom <= -10, -10, ifelse(irrareaanom_avg$IrrAreaAnom >= 10, 10, irrareaanom_avg$IrrAreaAnom))
plot2 <- ggplot(irrareaanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=IrrAreaAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-10,10), breaks = seq(-10, 10, by = 2))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="right", legend.direction = "vertical", legend.key.height = unit(0.6, "cm"),  # Adjust the key height
        legend.key.width = unit(0.5, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 8))  # Adjust the text size in the legend
plot2 + 
  geom_point(data = irrareaanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(irrareaanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = irrareaanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(irrareaanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = irrareaanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(irrareaanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = irrareaanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(irrareaanom_avg$ip == ".1IOD-", "blue", "NA"))

irrareaanom_avg1 <- irrareaanom_avg
np <- subset(irrareaanom_avg1, np!="NA")
ip <- subset(irrareaanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$IrrAreaAnom<0, 1, ifelse(np$np==".3La Nina" & np$IrrAreaAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$IrrAreaAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$IrrAreaAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$IrrAreaAnom>0, 1, ifelse(np$np==".3La Nina" & np$IrrAreaAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$IrrAreaAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$IrrAreaAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "El Niño", ifelse(combined_data$phases==".3La Nina", "La Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("El Niño", "La Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("") +
  ggtitle("Irrigated Area") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=IrrAreaAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Irrigated Area Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=IrrAreaAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Irrigated Area Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=IrrAreaAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("Irrigated Area Anomalies (1000 ha)")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=IrrAreaAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits=c(-10,15))

# Relative Irrigated Area Anomalies

irrarea_nino34_iod_phase <- dplyr::select(irrarea_nino34_iod, c(Year, wRiceIrrPerAnom, wMaizeIrrPerAnom, wSorghumIrrPerAnom, wPearlMilletIrrPerAnom, wFingerMilletIrrPerAnom, detrend_jjas_nino34, detrend_jjas_iod, nino34phase, iodphase))
irrareaperanom_avg <- pivot_longer(irrarea_nino34_iod_phase[3:50,], 2:6, names_to = "Crops", values_to = "PercentIrrAreaAnom")
irrareaperanom_avg$Crop <- ifelse(irrareaperanom_avg$Crops=="wRiceIrrPerAnom", "Rice", ifelse(irrareaperanom_avg$Crops=="wMaizeIrrPerAnom", "Maize", ifelse(irrareaperanom_avg$Crops=="wSorghumIrrPerAnom", "Sorghum", ifelse(irrareaperanom_avg$Crops=="wPearlMilletIrrPerAnom", "PearlMillet", "FingerMillet"))))
irrareaperanom_avg$Crops <- NULL
irrareaperanom_avg$Crop <- factor(irrareaperanom_avg$Crop, levels = c("Rice", "Maize", "Sorghum", "PearlMillet", "FingerMillet"))
irrareaperanom_avg$phase <- ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="IOD+", "ElNino&IOD+", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="IOD-", "LaNina&IOD-", NA))))))
irrareaperanom_avg$phase1 <- ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="IOD+", "El Nino", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="IOD-", "La Nina", ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="IOD-", "El Nino", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="IOD+", "La Nina", NA))))))))
irrareaperanom_avg$phase2 <- ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="Neutral", "El Nino", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="Neutral", "La Nina", ifelse(irrareaperanom_avg$nino34phase=="Neutral" & irrareaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="IOD+", "IOD+", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaperanom_avg$nino34phase=="El Nino" & irrareaperanom_avg$iodphase=="IOD-", "IOD-", ifelse(irrareaperanom_avg$nino34phase=="La Nina" & irrareaperanom_avg$iodphase=="IOD+", "IOD+", NA))))))))

irrareaperanom_avg$phase <- factor(irrareaperanom_avg$phase, levels = c("ElNino&IOD+", "El Nino", "IOD+", "IOD-", "La Nina", "LaNina&IOD-"))
irrareaperanom_avg$phase1 <- factor(irrareaperanom_avg$phase1, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))
irrareaperanom_avg$phase2 <- factor(irrareaperanom_avg$phase2, levels = c("El Nino", "IOD+", "IOD-", "La Nina"))

phaseENSO <- irrareaperanom_avg[,c(1:7,9)]
phase1ENSO <- subset(phaseENSO, phase1 == "El Nino" | phase1 == "La Nina")
colnames(phase1ENSO)[8] <- "phase"
phaseIOD <- irrareaperanom_avg[,c(1:7,10)]
phase2IOD <- subset(phaseIOD, phase2 == "IOD+" | phase2 =="IOD-")
colnames(phase2IOD)[8] <- "phase"
phasesENSOIOD <- irrareaperanom_avg[,c(1:8)]
phaseENSOIOD <- subset(phasesENSOIOD, phase == "ElNino&IOD+" | phase == "LaNina&IOD-")
All_ENSO_IOD <- rbind(phase1ENSO, phase2IOD)
summary(All_ENSO_IOD$phase)

irrareaperanom_avg$np <- ifelse(irrareaperanom_avg$nino34phase=="El Nino", ".4El Nino", ifelse(irrareaperanom_avg$nino34phase=="La Nina", ".3La Nina", "NA"))
irrareaperanom_avg$np <- factor(irrareaperanom_avg$np, levels = c(".4El Nino", ".3La Nina"))
irrareaperanom_avg$ip <- ifelse(irrareaperanom_avg$iodphase=="IOD+", ".2IOD+", ifelse(irrareaperanom_avg$iodphase=="IOD-", ".1IOD-", "NA"))
irrareaperanom_avg$ip <- factor(irrareaperanom_avg$ip, levels = c(".2IOD+", ".1IOD-"))
irrareaperanom_avg$Crop1 <- ifelse(irrareaperanom_avg$Crop=="Rice", "5Rice", ifelse(irrareaperanom_avg$Crop=="Maize", "4Maize", ifelse(irrareaperanom_avg$Crop=="Sorghum", "3Sorghum", ifelse(irrareaperanom_avg$Crop=="PearlMillet", "2PearlMillet", "1FingerMillet"))))
irrareaperanom_avg$PercentIrrAreaAnom <- ifelse(irrareaperanom_avg$PercentIrrAreaAnom <= -20, -20, ifelse(irrareaperanom_avg$PercentIrrAreaAnom >= 20, 20, irrareaperanom_avg$PercentIrrAreaAnom))
plot2 <- ggplot(irrareaperanom_avg, aes(Year,Crop1))+
  geom_tile(aes(fill=PercentIrrAreaAnom))+
  coord_equal(ratio=2)+
  scale_fill_gradient2(low = "indianred4", mid = "white", high = "darkgreen", midpoint=0, limits=c(-20,20), breaks = seq(-20, 20, by = 5))+
  scale_x_continuous(breaks=seq(1970,2015,5))+
  scale_y_discrete(name = "") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank())+
  theme(legend.title = element_text(face = "bold", size = 10))+
  theme(legend.position="bottom", legend.direction = "horizontal", legend.key.height = unit(0.5, "cm"),  # Adjust the key height
        legend.key.width = unit(1, "cm"),  # Adjust the key width
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 10))+  # Adjust the text size in the legend
  labs(fill = "%Irrigated Area Anomaly")
plot2 + 
  geom_point(data = irrareaperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(irrareaperanom_avg$np == ".4El Nino", "darkred", "NA"))+
  geom_point(data = irrareaperanom_avg, aes(x=Year, y=np), size=0.5, color=ifelse(irrareaperanom_avg$np == ".3La Nina", "navy", "NA"))+
  geom_point(data = irrareaperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(irrareaperanom_avg$ip == ".2IOD+", "red", "NA"))+
  geom_point(data = irrareaperanom_avg, aes(x=Year, y=ip), size=0.5, color=ifelse(irrareaperanom_avg$ip == ".1IOD-", "blue", "NA"))

irrareaperanom_avg1 <- irrareaperanom_avg
np <- subset(irrareaperanom_avg1, np!="NA")
ip <- subset(irrareaperanom_avg1, ip!="NA")
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentIrrAreaAnom<0, 1, ifelse(np$np==".3La Nina" & np$PercentIrrAreaAnom<0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentIrrAreaAnom<0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentIrrAreaAnom<0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip1 <- rbind(np1, ip1)
np_ip1$condition <- "Simultaneous declines"
np$cropsaffected <- ifelse(np$np==".4El Nino" & np$PercentIrrAreaAnom>0, 1, ifelse(np$np==".3La Nina" & np$PercentIrrAreaAnom>0, 1, 0))
ip$cropsaffected <- ifelse(ip$ip==".2IOD+" & ip$PercentIrrAreaAnom>0, 1, ifelse(ip$ip==".1IOD-" & ip$PercentIrrAreaAnom>0, 1, 0))
np1 <- aggregate(np$cropsaffected, list(np$Year, np$np), sum)
ip1 <- aggregate(ip$cropsaffected, list(ip$Year, ip$ip), sum)
colnames(np1) <- c("Year", "phases", "no_crops_affected")
colnames(ip1) <- c("Year", "phases", "no_crops_affected")
np_ip2 <- rbind(np1, ip1)
np_ip2$condition <- "Simultaneous increases"

# Combine the data frames
combined_data <- rbind(np_ip1, np_ip2)
combined_data$phase <- ifelse(combined_data$phases==".4El Nino" , "Niño", ifelse(combined_data$phases==".3La Nina", "Niña", ifelse(combined_data$phases==".2IOD+", "IOD+", "IOD-")))
# Reorder levels for 'phases'
combined_data$phase <- factor(combined_data$phase, levels = c("Niño", "Niña", "IOD+", "IOD-"))
condition_colors <- c("indianred4", "darkgreen")
plot_combined <- ggplot(combined_data, aes(x = phase, y = no_crops_affected, fill = condition)) +
  geom_boxplot(position = "dodge", alpha = 0.75) +
  scale_fill_manual(values = condition_colors) +
  theme_classic() +
  ylab("Number of grains") +
  xlab("% Irrigated Area") +
  theme(text = element_text(size = 17),
        panel.spacing = unit(0.2, "lines")) +
  facet_grid(. ~ condition)
plot_combined

All_ENSO_IOD_Nino <- subset(All_ENSO_IOD, nino34phase!="Neutral")
All_ENSO_IOD_IOD <- subset(All_ENSO_IOD, iodphase!="Neutral")
d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_Nino, aes(x=Crop, y=PercentIrrAreaAnom, fill=nino34phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Irrigated Area Anomalies")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD_IOD, aes(x=Crop, y=PercentIrrAreaAnom, fill=iodphase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Irrigated Area Anomalies")+
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14, face="bold"))+
  theme(legend.text=element_text(size=10))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))

All_ENSO_IOD$Crop <- ifelse(All_ENSO_IOD$Crop=="Rice", "RI", ifelse(All_ENSO_IOD$Crop=="Maize", "MZ", ifelse(All_ENSO_IOD$Crop=="Sorghum", "SG", ifelse(All_ENSO_IOD$Crop=="PearlMillet", "PM", "FM"))))
All_ENSO_IOD$Crop <- factor(All_ENSO_IOD$Crop, levels = c("RI","MZ", "SG", "PM", "FM"))

d <- ggplot()+
  geom_boxplot(data = All_ENSO_IOD, aes(x=Crop, y=PercentIrrAreaAnom, fill=phase), outlier.size = 0.9)+
  theme_classic()+
  xlab("")+
  ylab("%Irrigated Area Anomalies")+
  #geom_point(data = phaseENSOIOD, aes(x=Crop, y=PercentIrrAreaAnom, shape=phase, color=phase), position=position_dodge(0.8), alpha=1, size=2)+
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.text=element_text(size=17), axis.title=element_text(size=17, face="bold"))+
  theme(legend.text=element_text(size=17))+
  geom_hline(yintercept=0, linetype="dashed", color = "darkgrey")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(-20,60))
