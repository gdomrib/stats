libraries <- c("ggplot2","ggfortify","readxl","ggfortify","corrplot")

lapply(libraries, library, character.only=TRUE)

"
EXPLORATORY DATA ANALYSIS
"

summary(smaUtil)

# Density plot
ggplot(smaUtil, aes(dVoBa, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus, ncol = 1, scales = 'free')

ggplot(sma, aes(dMinMax, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus)

ggplot(sma, aes(grMin, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus)

ggplot(sma, aes(grMig, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus)

ggplot(sma, aes(grMax, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus)

ggplot(sma, aes(alçada, fill=tipus)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~tipus)


# Boxplot

ggplot(smaUtil, aes(tipus, dVoBa, fill=tipus)) +  
  geom_boxplot(alpha=0.5)

ggplot(sma, aes(tipus, dMinMax, fill=tipus)) +  
  geom_boxplot(alpha=0.5)

ggplot(sma, aes(tipus, grMin, fill=tipus)) +  
  geom_boxplot(alpha=0.5)

ggplot(sma, aes(tipus, grMig, fill=tipus)) +  
  geom_boxplot(alpha=0.5)

ggplot(sma, aes(tipus, grMax, fill=tipus)) +  
  geom_boxplot(alpha=0.5)

ggplot(sma, aes(tipus, alçada, fill=tipus)) +  
  geom_boxplot(alpha=0.5) +  
  




"
PRINCIPAL COMPONENT ANALYSIS
"

# select varriables
measuresSm <- sma[,10:14]
measuresSm$bcyxd <- sma$bcyxd

# compute PCA
pcs <- prcomp(measuresSm)
pcs
plot(pcs)

pcSma <- sma
pcSma$pc1 <- pcs$x[,1]
pcSma$pc2 <- pcs$x[,2]

jpeg('plots/PCA/PCA_noLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs, data=sma, colour='tipus', shape='tipus', size=2)
dev.off()

jpeg('plots/PCA/PCA_withLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs, data=sma, colour='tipus', shape='tipus', size=2, loadgins=TRUE, loadings.label=TRUE)
dev.off()  


# Without bcyxd

measuresSm2 <- sma[,8:13]

pcs2 <- prcomp(measuresSm2)
pcs2
plot(pcs2)

pcSma2 <- sma
pcSma2$pc1 <- pcs2$x[,1]
pcSma2$pc2 <- pcs2$x[,2]

jpeg('plots/PCA/PCA_noLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs2, data=sma, colour='tipus', shape='tipus', size=2)
dev.off()

jpeg('plots/PCA/PCA_withLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs2, data=sma, colour='tipus', shape='ue', size=2, loadgins=TRUE, loadings.label=TRUE)
dev.off()  


# PCA with log transformation data

logTrans = log(measuresSm)
plot(logTrans)




pcsLog <- prcomp(logTrans)
pcsLog
plot(pcsLog)

pcSmaLog <- sma
pcSmaLog$pc1 <- pcsLog$x[,1]
pcSmaLog$pc2 <- pcsLog$x[,2]

autoplot(pcsLog, data=sma, colour='tipus', shape='tipus', size=2)

autoplot(pcsLog, data=sma, colour='tipus', size=2, loadgins=TRUE, loadings.label=TRUE)




"
CORRELATION MATRIX
"
