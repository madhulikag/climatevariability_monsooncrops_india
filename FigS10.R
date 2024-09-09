# FigS10 Scatterplots of grain characteristics anomalies to ENSO and IOD

# Quadrant plots with absolute anomalies

xvar<-as.vector(na.omit(yield_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(yield_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(yield_nino34_iod$wYieldAnom[3:50]))
year<-as.vector(yield_nino34_iod$Year[3:50])

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>60) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>40 & crop[i]<=60){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>20 & crop[i]<=40){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=20){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-20) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -20 & crop[i]>-40) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -40 & crop[i]>-60) {
    col_array[i]<-'#8b6f19'
  } else if (crop[i]<= -60 & crop[i]>-80) {
    col_array[i]<-'#725600'
  } else {
    col_array[i]<-'#584300'
  }
}

size_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>60) {
    size_array[i]<- 1.4
  } else if (crop[i]>40 & crop[i]<=60){
    size_array[i]<-1.2
  } else if (crop[i]>20 & crop[i]<=40){
    size_array[i]<- 1
  } else if (crop[i]>0 & crop[i]<=20){
    size_array[i]<- 0.8
  } else if (crop[i]<=0 & crop[i]>-20) {
    size_array[i]<- 0.8
  } else if (crop[i]<= -20 & crop[i]>-40) {
    size_array[i]<-1
  } else if (crop[i]<= -40 & crop[i]>-60) {
    size_array[i]<-1.2
  } else if (crop[i]<= -60 & crop[i]>-80) {
    size_array[i]<- 1.4
  } else {
    size_array[i]<- 1.6
  }
}

par(mar=c(3,3,1,7))
for (i in 1:49) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>60 kg/ha', '40 to 60', '20 to 40', '0 to 20', '-20 to 0', '-20 to -40', '-40 to -60','-60 to -80', '<-80' ), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600','#584300'),  pch=c(19,19,19,19,19,19,19,19,19), pt.cex=c(1.4,1.2,1,0.8,0.8,1,1.2,1.4,1.6))

xvar<-as.vector(na.omit(prod_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(prod_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(prod_nino34_iod$wProdAnom[3:50]))
year<-as.vector(na.omit(prod_nino34_iod$Year[3:50]))

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  if(crop[i]>8) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>10 & crop[i]<=15){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>5 & crop[i]<=10){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=5){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-5) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -5 & crop[i]>-10) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -10 & crop[i]>-15) {
    col_array[i]<-'#8b6f19'
  } else {
    col_array[i]<-'#725600'
  } 
}

size_array<-c()
for (i  in 1:48) {
  if(crop[i]>8) {
    size_array[i]<-1.4
  } else if (crop[i]>10 & crop[i]<=15){
    size_array[i]<-1.2
  } else if (crop[i]>5 & crop[i]<=10){
    size_array[i]<- 1
  } else if (crop[i]>0 & crop[i]<=5){
    size_array[i]<- 0.8
  } else if (crop[i]<=0 & crop[i]>-5) {
    size_array[i]<-0.8
  } else if (crop[i]<= -5 & crop[i]>-10) {
    size_array[i]<-1
  } else if (crop[i]<= -10 & crop[i]>-15) {
    size_array[i]<-1.2
  } else {
    size_array[i]<- 1.4
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>15 thousand tons', '10 to 15', '5 to 10', '0 to 5', '-5 to 0', '-5 to -10', '-10 to -15', '<-15' ), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600'),  pch=c(19,19,19,19,19,19,19,19), pt.cex=c(1.4,1.2,1,0.8,0.8,1,1.2,1.4))

xvar<-as.vector(na.omit(area_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(area_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(area_nino34_iod$wAreaAnom[3:50]))
year<-as.vector(na.omit(area_nino34_iod$Year[3:50]))

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>7.5) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>5 & crop[i]<=7.5){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>2.5 & crop[i]<=5){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=2.5){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-2.5) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -2.5 & crop[i]>-5) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -5 & crop[i]>-7.5) {
    col_array[i]<-'#8b6f19'
  } else if (crop[i]<= -7.5 & crop[i]>-10) {
    col_array[i]<-'#725600'
  } else {
    col_array[i]<- '#584300'
  }
}

size_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>7.5) {
    size_array[i]<-1.4
  } else if (crop[i]>5 & crop[i]<=7.5){
    size_array[i]<-1.2
  } else if (crop[i]>2.5 & crop[i]<=5){
    size_array[i]<- 1
  } else if (crop[i]>0 & crop[i]<=2.5){
    size_array[i]<- 0.8
  } else if (crop[i]<=0 & crop[i]>-2.5) {
    size_array[i]<-0.8
  } else if (crop[i]<= -2.5 & crop[i]>-5) {
    size_array[i]<-1
  } else if (crop[i]<= -5 & crop[i]>-7.5) {
    size_array[i]<-1.2
  } else if (crop[i]<= -7.5 & crop[i]>-10) {
    size_array[i]<- 1.4
  } else {
    size_array[i]<- 1.6
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>7.5 thousand ha', '5 to 7.5', '2.5 to 5', '0 to 2.5', '-2.5 to 0', '-2.5 to -5', '-5 to -7.5','-7.5 to -10', '<-10' ), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600','#584300'),  pch=c(19,19,19,19,19,19,19,19,19), pt.cex=c(1.4,1.2,1,0.8,0.8,1,1.2,1.4,1.6))

xvar<-as.vector(na.omit(irrarea_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(irrarea_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(irrarea_nino34_iod$wIrrAnom[3:50]))
year<-as.vector(na.omit(irrarea_nino34_iod$Year[3:50]))

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>7.5) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>5 & crop[i]<=7.5){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>2.5 & crop[i]<=5){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=2.5){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-2.5) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -2.5 & crop[i]>-5) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -5 & crop[i]>-7.5) {
    col_array[i]<-'#8b6f19'
  } else if (crop[i]<= -7.5 & crop[i]>-10) {
    col_array[i]<-'#725600'
  } else {
    col_array[i]<- '#584300'
  }
}

size_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>7.5) {
    size_array[i]<-1.4
  } else if (crop[i]>5 & crop[i]<=7.5){
    size_array[i]<-1.2
  } else if (crop[i]>2.5 & crop[i]<=5){
    size_array[i]<- 1
  } else if (crop[i]>0 & crop[i]<=2.5){
    size_array[i]<- 0.8
  } else if (crop[i]<=0 & crop[i]>-2.5) {
    size_array[i]<-0.8
  } else if (crop[i]<= -2.5 & crop[i]>-5) {
    size_array[i]<-1
  } else if (crop[i]<= -5 & crop[i]>-7.5) {
    size_array[i]<-1.2
  } else if (crop[i]<= -7.5 & crop[i]>-10) {
    size_array[i]<- 1.4
  } else {
    size_array[i]<- 1.6
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>7.5 thousand ha', '5 to 7.5', '2.5 to 5', '0 to 2.5', '-2.5 to 0', '-2.5 to -5', '-5 to -7.5','-7.5 to -10', '<-10' ), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600','#584300'),  pch=c(19,19,19,19,19,19,19,19,19), pt.cex=c(1.4,1.2,1,0.8,0.8,1,1.2,1.4,1.6))

# Quadrant plots with relative anomalies 

xvar<-as.vector(na.omit(yield_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(yield_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(yield_nino34_iod$wYieldPerAnom[3:50]))
year<-as.vector(yield_nino34_iod$Year[3:50])

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>13) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>10 & crop[i]<=13){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>5 & crop[i]<=10){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=5){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-5) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -5 & crop[i]>-10) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -10 & crop[i]>-13) {
    col_array[i]<-'#8b6f19'
  } else {
    col_array[i]<- '#725600'
  }
}

size_array<-c()
for (i  in 1:48) {
  
  if(crop[i]>13) {
    size_array[i] <- 1.2
  } else if (crop[i]>10 & crop[i]<=13){
    size_array[i] <- 1
  } else if (crop[i]>5 & crop[i]<=10){
    size_array[i] <- 0.8
  } else if (crop[i]>0 & crop[i]<=5){
    size_array[i] <- 0.6
  } else if (crop[i]<=0 & crop[i]>-5) {
    size_array[i] <- 0.6
  } else if (crop[i]<= -5 & crop[i]>-10) {
    size_array[i] <- 0.8
  } else if (crop[i]<= -10 & crop[i]>-13) {
    size_array[i] <- 1
  } else {
    size_array[i] <- 1.2
  }
}

par(mar=c(3,3,1,7))
for (i in 1:49) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>13%YieldAnom', '10 to 13', '5 to 10', '0 to 5', '-5 to 0', '-10 to -5', '-13 to -10','< -13'), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600'),  pch=c(19,19,19,19,19,19,19,19), pt.cex=c(1.2,1,0.8,0.6,0.6,0.8,1,1.2))

xvar<-as.vector(na.omit(prod_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(prod_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(prod_nino34_iod$wProdPerAnom[3:50]))
year<-as.vector(na.omit(prod_nino34_iod$Year[3:50]))

#hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i  in 1:48) {
  if(crop[i]>13) {
    col_array[i]<-'#4a7537'
  } else if (crop[i]>10 & crop[i]<=13){
    col_array[i]<- '#5f9747'
  } else if (crop[i]>5 & crop[i]<=10){
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=5){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-5) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -5 & crop[i]>-10) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -10 & crop[i]>-13) {
    col_array[i]<-'#8b6f19'
  } else {
    col_array[i]<-'#725600'
  }
}

size_array<-c()
for (i  in 1:48) {
  if(crop[i]>13) {
    size_array[i]<-1.2
  } else if (crop[i]>10 & crop[i]<=13){
    size_array[i]<-1
  } else if (crop[i]>5 & crop[i]<=10){
    size_array[i]<- 0.8
  } else if (crop[i]>0 & crop[i]<=5){
    size_array[i]<- 0.6
  } else if (crop[i]<=0 & crop[i]>-5) {
    size_array[i]<-0.6
  } else if (crop[i]<= -5 & crop[i]>-10) {
    size_array[i]<-0.8
  } else if (crop[i]<= -10 & crop[i]>-13) {
    size_array[i]<-1
  } else {
    size_array[i]<- 1.2
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>13%ProdAnom', '10 to 13', '5 to 10', '0 to 5', '-5 to 0', '-10 to -5', '-13 to -10', '< -13' ), col=c('#4a7537', '#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600'),  pch=c(19,19,19,19,19,19,19,19), pt.cex=c(1.2,1,0.8,0.6,0.6,0.8,1,1.2))

xvar<-as.vector(na.omit(area_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(area_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(area_nino34_iod$wAreaPerAnom[3:50]))
year<-as.vector(na.omit(area_nino34_iod$Year[3:50]))

# hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i in 1:48) {
  if(crop[i]>3) {
    col_array[i]<-'#5f9747'
  } else if (crop[i]>1 & crop[i]<=3) {
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=1){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-1) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -1 & crop[i]>-3) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -3 & crop[i]>-5) {
    col_array[i]<-'#8b6f19'
  } else if (crop[i]<= -5 & crop[i]>-7) {
    col_array[i]<-'#725600'
  } else {
    col_array[i]<- '#584300'
  }
}

size_array<-c()
for (i in 1:48) {
  
  if(crop[i]>3) {
    size_array[i]<-1
  } else if (crop[i]>1 & crop[i]<=3){
    size_array[i]<-0.8
  } else if (crop[i]>0 & crop[i]<=1){
    size_array[i]<- 0.6
  } else if (crop[i]<=0 & crop[i]>-1) {
    size_array[i]<-0.6
  } else if (crop[i]<= -1 & crop[i]>-3) {
    size_array[i]<-0.8
  } else if (crop[i]<= -3 & crop[i]>-5) {
    size_array[i]<-1
  } else if (crop[i]<= -5 & crop[i]>-7) {
    size_array[i]<- 1.2
  } else {
    size_array[i]<- 1.4
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>3%AreaAnom', '1 to 3', '0 to 1', '-1 to 0', '-3 to -1', '-5 to -3','-7 to -5', '< -7' ), col=c('#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600','#584300'),  pch=c(19,19,19,19,19,19,19,19), pt.cex=c(1,0.8,0.6,0.6,0.8,1,1.2,1.4))

xvar<-as.vector(na.omit(irrarea_nino34_iod$detrend_jjas_iod[3:50]))
yvar<-as.vector(na.omit(irrarea_nino34_iod$detrend_jjas_nino34[3:50]))
crop<-as.vector(na.omit(irrarea_nino34_iod$wIrrPerAnom[3:50]))
year<-as.vector(na.omit(irrarea_nino34_iod$Year[3:50]))

# hist(crop)
pcor.test(crop,xvar,yvar)
col_array<-c()
for (i in 1:48) {
  if(crop[i]>3) {
    col_array[i]<-'#5f9747'
  } else if (crop[i]>1 & crop[i]<=3) {
    col_array[i]<- '#78b060'
  } else if (crop[i]>0 & crop[i]<=1){
    col_array[i]<- '#96c283'
  } else if (crop[i]<=0 & crop[i]>-1) {
    col_array[i]<-'#bfaf7f'
  } else if (crop[i]<= -1 & crop[i]>-3) {
    col_array[i]<-'#a58f4c'
  } else if (crop[i]<= -3 & crop[i]>-5) {
    col_array[i]<-'#8b6f19'
  } else if (crop[i]<= -5 & crop[i]>-7) {
    col_array[i]<-'#725600'
  } else {
    col_array[i]<- '#584300'
  }
}

size_array<-c()
for (i in 1:48) {
  
  if(crop[i]>3) {
    size_array[i]<-1
  } else if (crop[i]>1 & crop[i]<=3){
    size_array[i]<-0.8
  } else if (crop[i]>0 & crop[i]<=1){
    size_array[i]<- 0.6
  } else if (crop[i]<=0 & crop[i]>-1) {
    size_array[i]<-0.6
  } else if (crop[i]<= -1 & crop[i]>-3) {
    size_array[i]<-0.8
  } else if (crop[i]<= -3 & crop[i]>-5) {
    size_array[i]<-1
  } else if (crop[i]<= -5 & crop[i]>-7) {
    size_array[i]<- 1.2
  } else {
    size_array[i]<- 1.4
  }
}

par(mar=c(3,3,1,8))
for (i in 1:48) {
  if(i==1){
    
    plot(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], ylim=c(-2,2.5), xlim=c(-2,2.5), xaxt='n', yaxt='n', xlab='DMI', ylab='Nino34', pch=19)
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
    
  } else {
    
    points(xvar[i], yvar[i], col=col_array[i], cex=size_array[i], pch=19)
    text(xvar[i], yvar[i], col=col_array[i], labels=year[i], cex=0.7, font=2, pos=2)
  }
  
  
}
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(h=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(v=0.5, col = 'coral2', lwd = 1, lty=2, xpd=FALSE)
abline(h=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
abline(v=-0.5, col = 'blue', lwd = 1, lty=2, xpd=FALSE)
axis(1, at=c(-2,-1,0,1,2))
axis(2, at=c(-2,-1,0,1,2))
mtext('Detrended & Standarized IOD Anomaly', side=1, cex = 1, line=2)
mtext('Detrended & Standarized Nino3.4 Anomaly', side=2, cex = 1, line=2)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c('>3%AreaAnom', '1 to 3', '0 to 1', '-1 to 0', '-3 to -1', '-5 to -3','-7 to -5', '< -7' ), col=c('#5f9747', '#78b060', '#96c283','#bfaf7f','#a58f4c','#8b6f19','#725600','#584300'),  pch=c(19,19,19,19,19,19,19,19), pt.cex=c(1,0.8,0.6,0.6,0.8,1,1.2,1.4))
