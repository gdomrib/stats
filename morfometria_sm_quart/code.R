libraries <- c("ggplot2","ggfortify","readxl","ggfortify","corrplot","devEMF")

lapply(libraries, library, character.only=TRUE)

"
ANÁLISIS EXPLORATORIO DE DATOS
"

summary(potteryMeasures)

# gráfico de densidad y boxplot para todas las variables

    # diámetro borde y base / borde
    
ggplot(potteryMeasures, aes(diamBo, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type, scales = 'free')

ggplot(potteryMeasures, aes(type, diamBo, fill=type)) +  
  geom_boxplot(alpha=0.5)
  
    # diámetro mínimo y máximo

ggplot(potteryMeasures, aes(diamMin, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)

ggplot(potteryMeasures, aes(diamMax, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)

ggplot(potteryMeasures, aes(type, diamMin, fill=type)) +  
  geom_boxplot(alpha=0.5)  

ggplot(potteryMeasures, aes(type, diamMax, fill=type)) +  
  geom_boxplot(alpha=0.5)  
  
  # grosor mínimo y máximo
  
ggplot(potteryMeasures, aes(grMin, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)

ggplot(potteryMeasures, aes(grMax, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)

ggplot(potteryMeasures, aes(type, grMin, fill=type)) +  
  geom_boxplot(alpha=0.5) 

ggplot(potteryMeasures, aes(type, grMax, fill=type)) +  
  geom_boxplot(alpha=0.5)   
  
  # altura
  
ggplot(potteryMeasures, aes(altura, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)
  
ggplot(potteryMeasures, aes(type, altura, fill=type)) +  
  geom_boxplot(alpha=0.5)   
  
  # perfil BCYXD
  
ggplot(potteryMeasures, aes(bcyxd, fill=type)) +  
  geom_density(alpha=0.5) +  
  facet_wrap(~type)

ggplot(potteryMeasures, aes(type, bcyxd, fill=type)) +  
  geom_boxplot(alpha=0.5)    
  
"
TRANSFORMACIÓN LOGARÍTMICA
"
  
logPotMeasures <- log(potteryMeasures)

 
"
MATRIZ DE CORRELACIÓN
"  

corMeasures <- cor(potteryMeasures, method = "pearson")
corLogMeasures <- cor(logPotMeasures, method = "pearson")


emf(file = "plots/correlation_matrix/corrplot_potteryMeasures.emf", emfPlus = FALSE)

corrplot.mixed(corMeasures, lower="number", lower.col = "gray30", number.cex= .8,    
               upper="circle", tl.srt = 45, order="hclust", insig="p-value")

               dev.off()


emf(file = "plots/correlation_matrix/corrplot_logPotteryMeasures.emf", emfPlus = FALSE)

corrplot.mixed(logPotMeasures, lower="number", lower.col = "gray30", number.cex= .8,    
               upper="circle", tl.srt = 45, order="hclust", insig="p-value")

dev.off()


"
ANÁLISIS DE COMPONENTES PRINCIPALES
"
# si es necesario, seleccionar variables numéricas
    # potMeasPCA <- smaMeasures[,X:X]


pcs <- prcomp(potteryMeasures)
pcs
plot(pcs)

pcaMeasures <- potteryMeasures
pcaMeasures$pc1 <- pcs$x[,1]
pcaMeasures$pc2 <- pcs$x[,2]

jpeg('plots/PCA/PCA_noLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs, data=potteryMeasures, colour='type', shape='type', size=2)
dev.off()

jpeg('plots/PCA/PCA_withLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcs, data=potteryMeasures, colour='type', shape='type', size=2, loadgins=TRUE, loadings.label=TRUE)
dev.off()  


# PCA - log

pcsLog <- prcomp(logPotMeasures)
pcsLog
plot(pcsLog)

pcaLogMeasures <- logPotMeasures
pcaLogMeasures$pc1 <- pcsLog$x[,1]
pcaLogMeasures$pc2 <- pcsLog$x[,2]

jpeg('plots/PCA/PCA_log_noLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcsLog, data=logPotMeasures, colour='type', shape='type', size=2)
dev.off()

jpeg('plots/PCA/PCA_log_withLoadings.jpeg', width = 20, height = 15, units = 'in', res = 300)
autoplot(pcsLog, data=logPotMeasures, colour='type', shape='type', size=2, loadgins=TRUE, loadings.label=TRUE)
dev.off()  
