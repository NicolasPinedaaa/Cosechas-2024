#------------------------------------------------------------------------------#
#------------------------- COSECHAS - 2024/01 ---------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
#dev.off()
cat("\014")

#setwd('C:\\Users\\nicol\\OneDrive\\Documents\\Cosechas-Banrep')
#setwd('~/Archivos/Cosechas/LaTeX/QREF/R')

# Paquetes --------------------------------------------------------------------
library(readxl)
library(lubridate)
library(seasonal)
library(dplyr)
library(gridExtra)
library(bsts)
library(data.table)
library(vars)
library(ggplot2)
library(lpirfs)
#SLP
library(splines)
library(sandwich)
library(lmtest)
library(Matrix)

source("Smooth_lproj_functions.R")

# Parametros del modelo VAR (IRF y lproj) --------------------------------------
#lags = 5   # Rezagos del modelo VAR (se define dentro de cada modelo VAR)
hor  = 24  # Horizonte del IRF    
h1   = 0   # Periodo donde se realiza el choque, puede ser <0> o <1> 
           # Solo se usa en Smooth-lproj
plot.transformaciones = FALSE
graficas_individuales = TRUE
# Data -------------------------------------------------------------------------
Elastic.Network = c('Ridge','Lasso','EN')[1]
if(Elastic.Network=='Ridge') load('SG_APC-Ridge_Coefficients.RData') # Componentes APC-Ridge (periodo y cosecha)
if(Elastic.Network=='Lasso') load('APC-EN_Coefficients.RData')       # Componentes APC-Lasso (periodo y cosecha)
if(Elastic.Network=='EN')    load('SG_APC-Ridge_Coefficients.RData') # Componentes APC-EN con alpha=0.5 (periodo y cosecha) 

Data <- read_excel("Info_Camilo_Clean_2024Ene31.xlsx", sheet = "Info_Macro_M") # Datos macro
#Data <- read_excel("Info_Camilo_Clean.xlsx", sheet = "Info_Macro_M") # Datos macro
Data$Fecha = as.Date(Data$Fecha)
mes        = month(Data$Fecha[1])
year       = year(Data$Fecha[1])
Fechas     = Data$Fecha
Var.Macro  = data.frame(Fechas)

# Vintage Component (outliers) ------------------------------------------------
Fecha    <- LastDayInMonth(as.Date(paste0(as.numeric(substring(rownames(Coh.p),1,2))+2000,'-',as.numeric(substring(rownames(Coh.p),4,5)),'-01')))
Data_Coh <- data.table(Date=Fecha,Component=Coh.p[,1]*100)
setnames(Data_Coh, "Date", "Fechas")

outlier_vars  <- c('outlier1U','outlier1D','outlier2U','outlier2D')
outlier_dates <- as.Date(c('2010-03-31','2010-04-30','2014-06-30','2014-07-31'))
names(outlier_dates) <- outlier_vars

for (o in seq_along(outlier_vars)) {
  Data_Coh[,eval(parse(text=paste0(outlier_vars[o],':=0')))]
  Data_Coh[eval(parse(text=paste0('Fechas==\'',outlier_dates[o],'\''))),eval(parse(text=paste0(outlier_vars[o],':=1')))]
}

setorder(Data_Coh,Fechas)

Data_Coh[,dm_Component:=Component-shift(Component,1)]

# Period Component (outliers) ------------------------------------------------
dates.Per <- LastDayInMonth(as.Date(paste0(as.numeric(substring(rownames(Per.p),1,2))+2000,'-',as.numeric(substring(rownames(Per.p),4,5)),'-01')))
Data_Per <- data.table(Date=dates.Per,Component=Per.p[,1]*100)
setorder(Data_Per,Date)
setnames(Data_Per, "Date", "Fechas")

Data_Per[,dm_Component:=Component-shift(Component,1)]

# Variables Macro (transformaciones) -------------------------------------------
# Real GDP (<ISE>)
if(1){
ISE1   = Data$ISE
ISE    = ts(data  = ISE1, start = c(year,mes), frequency = 12)
ISE    = log(ISE)
ISE    = seas(ISE, na.action = na.omit) # descom. aditiva????
ISE    = as.numeric(ISE[["data"]][,'final'])
ISE    = (ISE-shift(ISE,1))*100
#ISE    = ts(data  = ISE, start = c(year,mes), frequency = 12)
Var.Macro$ISE=ISE

if(plot.transformaciones){
plot(ts(data  = ISE1, start = c(year,mes), frequency = 12), type= "l", 
     ylab="ISE", xlab="Tiempo", main= "ISE antes de transformación")
plot(ts(data  = ISE, start = c(year,mes), frequency = 12), type= "l", ylab="ISE", xlab="Tiempo", 
     main= "ISE después de transformación")
}
}
# Unemployment (<U>)
if(1){
  U1     = Data$Desempleo
  U      = ts(data  = U1, start = c(year,mes), frequency = 12)
  U      = U*100
  U      = seas(U, na.action = na.omit)
  U      = as.numeric(U[["data"]][,'final'])
  dU     = (U-shift(U,1)) 
#  dU      = ts(data  = dU, start = c(year,mes), frequency = 12)
  Var.Macro$U = dU
if(plot.transformaciones){ 
  plot(ts(data  = U1, start = c(year,mes), frequency = 12), type= "l", 
       ylab="Desempleo", xlab="Tiempo", main= "Desempleo antes de transformación")
  plot(ts(data  = U, start = c(year,mes), frequency = 12), type= "l", ylab="Desempleo", xlab="Tiempo", 
       main= "Desempleo después de transformación")
}
}
# Tasa Interbancaria (<tib>)
if(1){
  tib1     = Data$TIB
  tib      = ts(data  = tib1, start = c(year,mes), frequency = 12)
  tib      = tib*100
  tib_real = tib-100*Data$Inflacion
  #tib      = (tib-shift(tib,1))*100 # Otra vez por <100> ???
  tib      = (tib-shift(tib,1))
  dtib_real = (tib_real-shift(tib_real,1))
  dtib_real      = as.numeric(dtib_real)
  Var.Macro$tib = dtib_real
if(plot.transformaciones){
  plot(ts(data  = tib1, start = c(year,mes), frequency = 12), type= "l", 
       ylab="Tasa Interbancaria", xlab="Tiempo", 
       main= "Tasa Interbancaria antes de transformación")
  plot(ts(data  = tib_real, start = c(year,mes), frequency = 12), type= "l", ylab="Tasa Interbancaria", 
       xlab="Tiempo", main= "Tasa Interbancaria después de transformación")
}
}
# Spread (<spread>)
if(1){
  spread1    = (Data$tasa_con-Data$TIB)*100 # Verif que ambas esten con las mismas unidades!!!
  spread     = ts(data  = spread1, start = c(year,mes), frequency = 12)
  spread     = (spread-shift(spread,1))
  spread     = as.numeric(spread)
  Var.Macro$spread = spread
if(plot.transformaciones){
  plot(ts(data  = spread1, start = c(year,mes), frequency = 12), type= "l", 
       ylab="Spread", xlab="Tiempo", main= "Spread antes de transformación")
  plot(ts(data  = spread, start = c(year,mes), frequency = 12), type= "l", ylab="Spread", xlab="Tiempo", 
       main= "Spread después de transformación")
}
}
# Credito Agregado (<crag>)
if(1){
  crag1 = Data$Cartera_con/Data$IPC
  crag  = ts(data  = crag1, start = c(year,mes), frequency = 12)
  crag  = log(crag)
  #crag  = seas(crag, na.action = na.omit)
  #crag  = as.numeric(crag[["data"]][,'final'])
  crag  = (crag-shift(crag,1))*100
  crag  = as.numeric(crag)
  #crag  = ts(data  = crag, start = c(year,mes), frequency = 12)
  Var.Macro$crag = crag
  
  if(plot.transformaciones){
   layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
   plot(ts(data  = crag1, start = c(year,mes), frequency = 12), type= "l", 
       ylab="Credito Agregado", xlab="Tiempo", main= "Credito Agregado antes de transformación")
   plot(ts(data  = crag, start = c(year,mes), frequency = 12), type= "l", 
       ylab="Credito Agregado", xlab="Tiempo", 
       main= "Credito Agregado después de transformación")
  }
}
# Inflacion (<inf>)
if(1){
  inf1   = (Data$Inflacion)*100
  inf    = ts(data  = inf1, start = c(year,mes), frequency = 12)
  inf    = (inf-shift(inf,1))
  Var.Macro$inf = inf
  
  if(plot.transformaciones){
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = inf1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="Inflacion", xlab="Tiempo", main= "Inflacion antes de transformación")
    plot(ts(data  = inf, start = c(year,mes), frequency = 12), type= "l", ylab="Inflacion", xlab="Tiempo", 
         main= "Inflacion después de transformación")
  }
}
# Deuda Soberana (<def.fis>)
if(1){
  #Se usa como variable Proxy el deficit fiscal
  def.fis1 = Data$Deficit_Fiscal
  def.fis  = ts(data=def.fis1, start=c(year,mes), frequency = 12)
  def.fis  = seas(def.fis, na.action=na.omit) # descom. aditiva????
  def.fis  = as.numeric(def.fis[["data"]][,'final'])
  def.fis  = (def.fis-shift(def.fis,1))
  Var.Macro$def.fis = def.fis1   #debido a los valores faltantes (solo para guardar los NA)
  Var.Macro$def.fis[!is.na(def.fis1)] = def.fis #debido a los valores faltantes
  
  if(plot.transformaciones){ 
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = def.fis1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="Deuda Soberana", xlab="Tiempo", main= "Deuda Soberana antes de transformación")
    plot(ts(data  = Var.Macro$def.fis, start = c(year,mes), frequency = 12), type= "l", ylab="Deuda Soberana", xlab="Tiempo", 
         main= "Deuda Soberana después de transformación")
  }
}
# Aversión al riesgo (vix)
if(1){
  vix1     = Data$VIX
  vix      = ts(data  = vix1, start = c(year,mes), frequency = 12)
  vix    = (vix-shift(vix,1))
  Var.Macro$vix = vix
  if(plot.transformaciones){  
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = vix1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="Aversión al riesgo", xlab="Tiempo", main= "Aversión al riesgo antes de transformación")
    plot(ts(data  = vix, start = c(year,mes), frequency = 12), type= "l", ylab="Aversión al riesgo", xlab="Tiempo", 
         main= "Aversión al riesgo después de transformación")
  }
  
}
# Stock Market Returns (<colcap>)
if(1){
  colcap1 = Data$COLCAP
  colcap  = ts(data  = colcap1, start = c(year,mes), frequency = 12)
  #colcap  = log(colcap)
  colcap  = (colcap-shift(colcap,1))
  Var.Macro$colcap = colcap
  
  if(plot.transformaciones){ 
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = colcap1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="Stock Market Returns", xlab="Tiempo", main= "Stock Market Returns antes de transformación")
    plot(ts(data  = colcap, start = c(year,mes), frequency = 12), type= "l", ylab="Stock Market Returns", xlab="Tiempo", 
         main= "Stock Market Returns después de transformación")
  }
}
# Retail Sales (<rsal>)
if(1){
  rsal1 = Data$Ventas
  rsal  = ts(data  = rsal1, start = c(year,mes), frequency = 12)
  #rsal  = seas(rsal, na.action = na.omit) # descom. aditiva????
  #rsal  = as.numeric(rsal[["data"]][,'final'])
  #Rsal  = rsal1             #debido a los valores faltantes (solo para guardar los NA)
  #Rsal[!is.na(rsal1)] = rsal #debido a los valores faltantes
  #rsal   = Rsal
  rsal   = (rsal-shift(rsal,1))*100
  Var.Macro$rsal = rsal
  
  if(plot.transformaciones){ 
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = rsal1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="Retail Sales", xlab="Tiempo", main= "Retail Sales antes de transformación")
    plot(ts(data  = rsal, start = c(year,mes), frequency = 12), type= "l", ylab="Retail Sales", xlab="Tiempo", 
         main= "Retail Sales después de transformación")
  }
}
# NPL (<ICM_con>)
if(1){
  npl1  = Data$ICM_con
  npl   = ts(data  = npl1, start = c(year,mes), frequency = 12)
  npl   = (npl-shift(npl,1))
  Var.Macro$npl = npl
  
  if(plot.transformaciones){ 
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = npl1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="NPL", xlab="Tiempo", main= "NPL antes de transformación")
    plot(ts(data  = npl, start = c(year,mes), frequency = 12), type= "l", ylab="NPL", xlab="Tiempo", 
         main= "NPL después de transformación")
  }
}
# Private Consumption (<ICC> Indice de Confinza del Consumidor) 
if(1){
  ICC1  = Data$ICC
  ICC   = ts(data  = ICC1, start = c(year,mes), frequency = 12)
  #ICC    = seas(ICC, na.action = na.omit) # descom. aditiva????
  #ICC    = as.numeric(ICC[["data"]][,'final'])
  ICC   = (ICC-shift(ICC,1))
  Var.Macro$ICC = ICC
  
  if(plot.transformaciones){ 
    layout(matrix(c(1,1,2,2),nrow=2,byrow=TRUE))
    plot(ts(data  = ICC1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="ICC", xlab="Tiempo", main= "ICC antes de transformación")
    plot(ts(data  = ICC, start = c(year,mes), frequency = 12), type= "l", ylab="NPL", xlab="Tiempo", 
         main= "ICC después de transformación")
  }
}
# Exchange Rate Depreciation (<ExRat>) 
if(1){
  ExRat1   = Data$COP_returns*100
  ExRat    = ts(data=ExRat1, start=c(year,mes), frequency=12)
  ExRat    = (ExRat-shift(ExRat,1))
  Var.Macro$ExRat=ExRat
  
  if(plot.transformaciones){
    plot(ts(data=ExRat1, start=c(year,mes), frequency=12), type="l", 
         ylab="ExRat", xlab="Tiempo", main= "ExRat antes de transformación")
    plot(ts(data=ExRat, start=c(year,mes), frequency=12), type="l", ylab="ExRat", xlab="Tiempo", 
         main="ExRat después de transformación")
  }
}
# Housing Market Returns (<ipvn>)
if(1){
  ipvn1   = Data$IPVN_real
  ipvn    = ts(data  = ipvn1, start = c(year,mes), frequency = 12)
  ipvn    = (ipvn-shift(ipvn,1))*100
  Var.Macro$ipvn=ipvn
  
  if(plot.transformaciones){
    plot(ts(data  = ipvn1, start = c(year,mes), frequency = 12), type= "l", 
         ylab="IPVN", xlab="Tiempo", main= "IPVN antes de transformación")
    plot(ts(data  = ipvn, start = c(year,mes), frequency = 12), type= "l", ylab="IPVN", xlab="Tiempo", 
         main= "IPVN después de transformación")
  }
}
letras <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M") #Usadas en las graficas (Ej: Panel A)

#Merge df
if(1){
Data_Coh <- merge(Data_Coh,Var.Macro,all.x=T,by='Fechas')
Data_Per <- merge(Data_Per,Var.Macro,all.x=T,by='Fechas')
#Quitar primera observacion de los datos para no tener el NA
Data_Per <- Data_Per[2:nrow(Data_Per),]
Data_Coh <- Data_Coh[2:nrow(Data_Coh),]
}

#---variables de los modelos VAR  ------- En que modelo se incluye <ICI> proxy de <PMI manufacturero>????
var.names.model.1  = c("ISE", "U", "tib", "spread", "crag", 'dm_Component')
var.names.model.2  = c("ISE", "U",  "def.fis", "inf", "tib", "spread", "crag",  'dm_Component')
var.names.model.3  = c("vix", "ISE", "U", "def.fis", "inf", "tib", "spread", "crag", "colcap", 'ipvn' ,'dm_Component') 
var.names.model.4  = c("ISE", 'ICC', "U", "tib", "spread", "crag",  'dm_Component')
var.names.model.5  = c("vix","ISE","rsal", "U", "tib", "spread", "crag",  'ExRat' , 'dm_Component')
var.names.model.6  = c("vix", "ISE", 'ICC', "rsal", "U", "def.fis", "inf", "tib", "spread", "crag", 'ExRat', 'dm_Component') 
  
# Graficas Paper Variables Macro y NPL -----------------------------------------
if(0){
df_plot  = data.frame(Fecha = Data$Fecha)
#Grafica ISE
ISE_a    = ts(data  = ISE1, start = c(year,mes), frequency = 12)
ISE_a    = log(ISE_a)
ISE_a    = seas(ISE_a, na.action = na.omit)
ISE_a    = as.numeric(ISE_a[["data"]][,'final'])
ISE_a    = (ISE_a-shift(ISE_a,12))*100
df_plot$ISE = ISE_a
p_ISE_a <- ggplot(df_plot) + geom_line(aes(x=Fecha,y=ISE_a),size=1.3) +
  labs(y='Percentage Points',x='',title='ISE_Dif12.SeasAdj.Log') + 
  scale_y_continuous(limits = c(-2,10), breaks=seq(-2,10,2)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#Grafica Desempleo
df_plot$U   = U
plot_U <- ggplot(df_plot) + geom_line(aes(x=Fecha,y=U),size=1.3) +
  labs(y='Percentage Points',x='',title='U_Niv') + 
  scale_y_continuous(limits = c(8,14), breaks=seq(8,14,1)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#Grafica TIB
TIB = tib_real
df_plot$TIB = TIB
p_TIB <- ggplot(df_plot) + geom_line(aes(x=Fecha,y=TIB),size=1.3) +
  labs(y='Percentage Points',x='',title='Tib_Niv') + 
  scale_y_continuous(limits = c(-2,5), breaks=seq(-2,5,1)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#Grafica Spread
spread      = (Data$tasa_con-Data$TIB)*100
df_plot$spread = spread
p_spread    = ggplot(df_plot) + geom_line(aes(x=Fecha,y=spread),size=1.3) +
  labs(y='Percentage Points',x='',title='Spread_Niv') + 
  scale_y_continuous(limits = c(10,20), breaks=seq(10,20,1)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#Grafica Credito Agregado
#En este caso se grafican las diferencias anuales, no mensuales que son las que
#se usan en el modelo
  crag_a    = ts(data  = crag1, start = c(year,mes), frequency = 12)
  crag_a    = log(crag_a)
  crag_a    = seas(crag_a, na.action = na.omit)
  crag_a    = as.numeric(crag_a[["data"]][,'final'])
  crag_a    = (crag_a-shift(crag_a,12))*100
df_plot$crag_a = crag_a
p_crag_a    = ggplot(df_plot) + geom_line(aes(x=Fecha,y=crag_a),size=1.3) +
  labs(y='Percentage Points',x='',title='Crag_Dif12.SeaAdj.Log') + 
  scale_y_continuous(limits = c(-4,38), breaks=seq(-4,38,4)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#NPL
df_plot$npl = npl1
p_NPL <- ggplot(df_plot) + geom_line(aes(x=Fecha,y=npl),size=1.3) +
  labs(y='Percentage Points',x='', title =" NPL_Niv") + 
  scale_y_continuous(limits = c(3,9), breaks=seq(3,9,1)) + 
  scale_x_date(date_breaks = "2 years" , date_labels = "%Y")
#Imprimirlos
grid.arrange(p_ISE_a, plot_U, p_TIB, p_spread, p_crag_a, p_NPL,nrow = 3)
}

# Modelo 1---------------------------------------------------------------------
# VAR - IFR
if(0){
 #--------------  VAR con <Periodo> ---------------# 
 # Definicion del modelo
 lags = 5   # Rezagos del modelo VAR
 data_mod1   = Data_Per[,.(ISE,U,tib,spread,crag,dm_Component)]
 Per.pos_mod = rowSums(is.na(data_mod1))==0
 Per.model1  = vars::VAR(data_mod1[Per.pos_mod],p=lags)
 var.names.plots1=c("Economic Activity Index","Unemployment Rate",
                    "Monetary Policy Interest Rate",
                    "Interest Rate Spread","Aggregate Credit","Component")
 
 #Definicion de las restricciones de cero 
 Per.N          <- ncol(Per.model1$y)
 Per.RESVAR.mat <- matrix(1,Per.N,Per.N)
 Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
 Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*lags), 1)
 for (i in 1:Per.N) {
  for (j in 1:Per.N) {
    if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*lags-(Per.N-j), Per.N)] <- 1
  }
 }
 #Modelo restringido
 Per.res.model <- restrict(Per.model1, method='man', resmat=Per.rest.N.L)
 #summary(Per.res.model)

 # Res-tests
 if(1){
   #Residual's hypothesis tests------------
   VAR_dif   = Per.res.model
   VAR_htest = matrix(NA, nrow=1, ncol=3)
   colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
   #----Normality
   VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
   #----Homoskedasticity
   VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
   #-----Autocorrelation
   VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                    type = 'PT.asymptotic')$serial$p.value, 4)
   print(VAR_htest)
 }
 
 Per.vars  = colnames(Per.res.model$y)
 Per.lags  = max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
 Per.comp  = Per.vars[grep('Component',Per.vars)]
 Per.N     = length(Per.vars)
 Per.TT    = nrow(Per.res.model$y)
 Per.dates = Data_Per[Per.pos_mod]$Fechas

 # IRF del modelo restringido
 Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
 #------ Prueba de grafica del IRF totat------#
 if(0){
x11()
par(mfrow=c(6,6), mar=c(5-3, 4-3, 4-3, 2-1) + 0.1)
VARS.names =  names(Per.irf$irf)
for(ss in VARS.names)
 for (rr in VARS.names){
  plot.ts(Per.irf$irf[[ss]][,rr],main=paste0('Ch en ',ss,'Rta en ',rr),
          cex.main=0.9, col='tomato', lwd=2) #IRF donde se choca <ISE>
  abline(h=0, col='black', lwd=2)
 }
}
 #Este ejercicio indica que se chocan las var.macro y se mira rta en el componente 
#--------------------------------------------#

 #IRF del modelo restringido chocando las var macro y rta en el componente  
 IRFS = list()
 for (i in seq_along(Per.vars)) {
  mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
  upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
  lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
  dist  <- (upper-lower)/2
  upper <- mean + dist
  lower <- mean - dist
  
  Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
  
  p_irf <- ggplot(Per.data_irf) +
    geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
    geom_line(aes(x=H,y=mean),size=1.3) +
    geom_hline(yintercept=0) +
    labs(y='Percentage Points',x='Months', 
         title=paste0('Panel ', letras[i]," ", var.names.plots1[i])) +
    scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))
  #print(p_irf)
  IRFS[[i]] = p_irf
}
 names(IRFS) = Per.vars
 grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
              nrow=3, top=paste0('Impulse-Response on Period Component for model 1'),
              layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))

 #--------------  VAR con <Cohort> ---------------# 
 
 # Definicion del modelo 
 Coh.VAR.lag     = 8  
 VAR.names.model = var.names.model.1  
 Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
 Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
 Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
 outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
 Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
 Data_Coh[,dm_Component_ori:=dm_Component]
 Data_Coh[,dm_Component:=dm_Component-outlier_effect]
 
 #plot(Data_Coh[,.(Fecha,dm_Component_ori)],t='l')
 #lines(Data_Coh[,.(Fecha,dm_Component)],col=2)
 
 Coh.data_mod = Data_Coh[, ..VAR.names.model]
 Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
 Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
 
 #Definicion de las restricciones de cero 
 Coh.N          = ncol(Coh.model$y)
 Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
 Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
 Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
 for (i in 1:Coh.N) {
   for (j in 1:Coh.N) {
     if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
   }
 }
 Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
 #summary(Coh.res.model)
 
 # Res-tests
 if(1){
   #Residual's hypothesis tests------------
   VAR_dif   = Coh.res.model
   VAR_htest = matrix(NA, nrow=1, ncol=3)
   colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
   #----Normality
   VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
   #----Homoskedasticity
   VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
   #-----Autocorrelation
   VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                    type = 'PT.asymptotic')$serial$p.value, 4)
   print(VAR_htest)
 }
 
 Coh.vars  <- colnames(Coh.res.model$y)
 Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
 Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
 Coh.N     <- length(Coh.vars)
 Coh.TT    <- nrow(Coh.res.model$y)
 Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
 
 # IRF del modelo restringido
 IRFS = list()
 Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
 for (i in seq_along(Coh.vars)) {
   mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
   upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
   lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
   dist  <- (upper-lower)/2
   upper <- mean + dist
   lower <- mean - dist
   
   Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
   
   c_irf <- ggplot(Coh.data_irf) +
     geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
     geom_line(aes(x=H,y=mean),size=1.3) +
     geom_hline(yintercept=0) +
     labs(y='Percentage Points',x='Months', 
          title=paste0('Panel ', letras[i]," ", var.names.plots1[i])) +
     scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))
   #print(p_irf)
   IRFS[[i]] = c_irf
 }
 names(IRFS) = Coh.vars
 grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]],
              nrow=3, top=paste0('Impulse-Response on Vintage Component for model 1'),
              layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))
}

#-----  Loc.Proj Ada Acumulada (Definitivo)-----# 
if(1){
  data_mod1 <- Data_Coh[,.(ISE, U, tib, spread, crag, dm_Component)]
  names(data_mod1) <- c("Economic Activity Index","Unemployment Rate",
                       "Monetary Policy Interest Rate",
                       "Interest Rate Spread","Aggregate Credit","Component")
  lags    = 8
  Vintage = lp_lin(endog_data=data_mod1, lags_endog_lin=lags, trend=0, shock_type=0,
              confint=1.96, hor=hor)
  #plot(Vintage)
  #grid.arrange(plot_lin(Vintage)[[31]],  plot_lin(Vintage)[[32]], plot_lin(Vintage)[[33]],
  #            plot_lin(Vintage)[[34]], plot_lin(Vintage)[[35]], ncol=2,
  #           top=paste0('Local Projection for Impulse-Response for model 1'),
  #           layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))
  #grid.arrange(plot_lin(lp)[[31]],  plot_lin(lp)[[32]], plot_lin(lp)[[33]],
  #             plot_lin(lp)[[34]], plot_lin(lp)[[35]], plot_lin(lp)[[36]], ncol=2,
  #             top=paste0('Local Projection for Impulse-Response for model 1'))
  grid.arrange(plot_lin(Vintage)[[31]],  plot_lin(Vintage)[[32]], plot_lin(Vintage)[[33]],
               plot_lin(Vintage)[[34]], plot_lin(Vintage)[[35]], ncol=2,
               layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))
  h1 = hor+1
  Local.Proj.LFM2(obj.Addaemer=Vintage, data=data_mod1, h=h1,  
                  plot.loc.proj.acum=TRUE, sin.CI=TRUE, EN= )

#Period
  data_mod1 <- Data_Per[,.(ISE, U, tib, spread, crag, dm_Component)]
  names(data_mod1) <- c("Economic Activity Index","Unemployment Rate",
                      "Monetary Policy Interest Rate",
                      "Interest Rate Spread","Aggregate Credit","Component")
lags   = 5
Period = lp_lin(endog_data=data_mod1, lags_endog_lin=lags, trend=0, shock_type=0,
            confint=1.96, hor=hor)
#plot(Period)
grid.arrange(plot_lin(Period)[[31]],  plot_lin(Period)[[32]], plot_lin(Period)[[33]],
             plot_lin(Period)[[34]], plot_lin(Period)[[35]], ncol=2,
             layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))
#grid.arrange(plot_lin(Period)[[31]],  plot_lin(Period)[[32]], plot_lin(Period)[[33]],
#            plot_lin(Period)[[34]], plot_lin(Period)[[35]], ncol=2,
#            top=paste0('Local Projection for Impulse-Response for model 1',
#                       layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)))
#grid.arrange(plot_lin(lp)[[31]],  plot_lin(lp)[[32]], plot_lin(lp)[[33]],
#             plot_lin(lp)[[34]], plot_lin(lp)[[35]], plot_lin(lp)[[36]], ncol=2,
#             top=paste0('Local Projection for Impulse-Response for model 1'))
h1 = hor+1
Local.Proj.LFM2(obj.Addaemer=Period, data=data_mod1, h=h1,  plot.loc.proj.acum=TRUE, sin.CI=TRUE, EN=Elastic.Network)
}

#----- Local Projections a mano Acumulada (no dio igual)-------#
if(0){
 Local.Proj.LFM(data=data_mod1, p=lags ,shock_type=0, h=hor, confint=qnorm(1-((1-0.95)/2)), plot.loc.proj.acum=FALSE )  
}

# Smooth.Loc.Proj (no dio igual)
if(0){

#Parametros
  P <- lags # number of lags of rhs variables
  h1<- 0 # Set to 0 to have the shock hit in period 0, 1 to have the shock hit in period 1
  H <- hor # Number of horizons

#Variables
  y  <- Data_Per$dm_Component # Response variable
  x  <- Data_Per$ISE # Endogenous variable related to the shock
  # Other RHS variables (used to instrument for the impulse variable)
  #w  <- cbind( Data_Per[,c("ISE","U","tib","spread","crag")] ,
  #             lagmatrix( Data_Per[,c("dm_Component", "ISE","U","tib","spread","crag")] , P ) )
  #w  <- as.matrix(w)
  w  <- cbind(lagmatrix( Data_Per[,c("dm_Component", "ISE","U","tib","spread","crag")] , P ) )
  w  <- as.matrix(w)

# Create lambda options
lambda <- c(0.0001,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,1.0,9,10)/1000

# Remove NA rows due to lags
LPdata <- cbind(y,x,w)
LPdata <- na.omit(LPdata)
colnames(LPdata)[1] <- "component"
colnames(LPdata)[2] <- "ISE"
w <- LPdata[,3:ncol(LPdata)]
y <- LPdata[,1]
x <- LPdata[,2]


# Run a regular local projection (Jorda 2005).
ir.regular  <- lproj( y=y , x=x , w=w , const=TRUE , type='reg' , H=H , h1=h1 )
# Run a smooth local projection
ir.smooth   <- lproj( y=y , x=x , w=w , const=TRUE , type='smooth' , H=H , h1=h1 , r=2 , lambda=lambda )
ir.smooth   <- lproj.cv( ir.smooth , 5 ) # Pick the optimal coefficients as result of minimum RSS from choice of lambda


# Plot the regular and smooth local projection coefficients as impulse response functions to compare them
ir.smooth <- lproj.conf(ir.smooth, ir.smooth$idx.opt)

x11()
plot(ir.regular$ir, type='l', col='blue', xlab='time', ylab='response')
lines(ir.smooth$ir.opt, type='l', col='red')
lines(ir.smooth$irc[,1], type='l', col='red', lty='dashed')
lines(ir.smooth$irc[,2], type='l', col='red', lty='dashed')
abline(h=0)
legend('bottomright', inset=.05, legend=c("regular", "smooth"), col=c('blue','red'), lty=1:2)


# Add confidence intervals to the smooth local projections and plot the impulse response functions
ir.smooth <- lproj.conf(ir.smooth, ir.smooth$idx.opt)

x11()
plot(ir.smooth$ir.opt, type='l', col='black', xlab='time', ylab='response', ylim=range(min(na.omit(ir.smooth$irc[,1])), max(na.omit(ir.smooth$irc[,2]))))
lines(ir.smooth$irc[,1], type='l', col='red', lty='dashed')
lines(ir.smooth$irc[,2], type='l', col='red', lty='dashed')
abline(h=0)

}

# Modelo 2---------------------------------------------------------------------
# VAR
if(0){
  #--------------  VAR con <Periodo> ---------------# 
  # Definicion del modelo 
  Per.VAR.lag     = 3
  Model.number    = '2'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  data_mod        = Data_Per[, ..VAR.names.model] #Sup: la ult. var es el componente
  Per.pos_mod     = rowSums(is.na(data_mod))==0
  Per.model       = vars::VAR(data_mod[Per.pos_mod], p=Per.VAR.lag)
  var.names.plots2=c("Economic Activity Index","Unemployment Rate","Sovereign debt",
                     "Inflation","Monetary Policy Interest Rate",
                     "Interest Rate Spread","Aggregate Credit")
  cat("\n modelo #", Model.number, "\n")
  #Definicion de las restricciones de cero 
  Per.N          = ncol(Per.model$y)
  Per.RESVAR.mat = matrix(1,Per.N,Per.N)
  Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
  Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*Per.model$p), 1)
  for (i in 1:Per.N) {
    for (j in 1:Per.N) {
      if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*Per.model$p-(Per.N-j), Per.N)] <- 1
    }
  }
  Per.res.model <- restrict(Per.model, method='man', resmat=Per.rest.N.L)
  #summary(Per.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Periodo",Per.VAR.lag, "\n")
    VAR_dif   = Per.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }  
  Per.vars  <- colnames(Per.res.model$y)
  Per.lags  <- max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
  Per.comp  <- Per.vars[grep('Component',Per.vars)]
  Per.N     <- length(Per.vars)
  Per.TT    <- nrow(Per.res.model$y)
  Per.dates <- Data_Per[Per.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
  for (i in seq_along(Per.vars)) {
    mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
    upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
    lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    p_irf <- ggplot(Per.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots2[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = p_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Per',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  
  names(IRFS) = Per.vars
  Per_plots2 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
                            IRFS[[6]],IRFS[[7]],
                            ncol=2,
                            layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(5,5,6,6),c(NA,7,7,NA)))
  #Per_plots2 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
  #                          IRFS[[6]],IRFS[[7]],
  #                          ncol=2, top=paste0('Impulse-Response on Period Component for model ',
  #                                             Model.number),
  #                          layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(5,5,6,6),c(NA,7,7,NA)))
  #ggsave(paste0('PerIRF_Model',Model.number,'.pdf'), get(paste0('Per_plots',Model.number)), device = "pdf")
  #--------------  VAR con <Cohort> ---------------# 
  # Definicion del modelo
  Coh.VAR.lag     = 3
  cat("Rezago para Cohorte",Coh.VAR.lag, "\n")
  Model.number    = '2'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
  Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
  Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
  outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
  Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
  Data_Coh[,dm_Component_ori:=dm_Component]
  Data_Coh[,dm_Component:=dm_Component-outlier_effect]
  
  Coh.data_mod = Data_Coh[, ..VAR.names.model]
  Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
  Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
  
  #Definicion de las restricciones de cero 
  Coh.N          = ncol(Coh.model$y)
  Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
  Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
  Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
  for (i in 1:Coh.N) {
    for (j in 1:Coh.N) {
      if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
    }
  }
  Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
  #summary(Coh.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    VAR_dif   = Coh.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }

  Coh.vars  <- colnames(Coh.res.model$y)
  Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
  Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
  Coh.N     <- length(Coh.vars)
  Coh.TT    <- nrow(Coh.res.model$y)
  Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
  for (i in seq_along(Coh.vars)) {
    mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
    upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
    lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    c_irf <- ggplot(Coh.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots2[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3)) +
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = c_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Coh',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Coh.vars
  Coh_plots2 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
               IRFS[[7]], nrow=3,  
               layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(5,5,6,6),c(NA,7,7,NA)))
#  Coh_plots2 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
#                            IRFS[[7]], nrow=3,  top=paste0('Impulse-Response on Vintage Component for model ',
#                                                           Model.number),
#                           layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(5,5,6,6),c(NA,7,7,NA)))
#  ggsave(paste0('CohIRF_Model',Model.number,'.pdf'), get(paste0('Coh_plots',Model.number)), device = "pdf")
}
# Modelo 3---------------------------------------------------------------------
# VAR
if(0){
  #--------------  VAR con <Periodo> ---------------# 
  # Definicion del modelo 
  Per.VAR.lag     = 1
  Model.number    = '3'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  data_mod        = Data_Per[, ..VAR.names.model] #Sup: la ult. var es el componente
  Per.pos_mod     = rowSums(is.na(data_mod))==0
  Per.model       = vars::VAR(data_mod[Per.pos_mod], p=Per.VAR.lag)
  var.names.plots3=c("VIX","Economic Activity Index","Unemployment Rate",
                     "Sovereign debt","Inflation","Monetary Policy Interest Rate",
                     "Interest Rate Spread","Aggregate Credit",
                     "Stock market returns", "Housing market returns")
  cat("\n modelo #", Model.number, "\n")
  
  #Definicion de las restricciones de cero 
  Per.N          = ncol(Per.model$y)
  Per.RESVAR.mat = matrix(1,Per.N,Per.N)
  Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
  Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*Per.model$p), 1)
  for (i in 1:Per.N) {
    for (j in 1:Per.N) {
      if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*Per.model$p-(Per.N-j), Per.N)] <- 1
    }
  }
  Per.res.model <- restrict(Per.model, method='man', resmat=Per.rest.N.L)
  #summary(Per.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Periodo:",Per.VAR.lag, "\n")
    VAR_dif   = Per.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }  
  Per.vars  <- colnames(Per.res.model$y)
  Per.lags  <- max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
  Per.comp  <- Per.vars[grep('Component',Per.vars)]
  Per.N     <- length(Per.vars)
  Per.TT    <- nrow(Per.res.model$y)
  Per.dates <- Data_Per[Per.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
  for (i in seq_along(Per.vars)) {
    mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
    upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
    lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    p_irf <- ggplot(Per.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots3[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = p_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Per',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Per.vars
  Per_plots3 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
                              IRFS[[6]],IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],
                              ncol=2,
                            layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10)))
  
  #Per_plots3 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
  #                          IRFS[[6]],IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],
  #                          nrow=3,
  #                          layout_matrix=rbind(c(1,2,3,4),c(5,6,7,8),c(NA,9,10,NA)),
  #                          top=paste0('Impulse-Response on Period Component for model ',
  #                                     Model.number))
  #Per_plots3.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
  #                          IRFS[[6]],ncol=2)
  #Per_plots3.2 = grid.arrange(IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],ncol=2)
  #ggsave(paste0('PerIRF_Model',Model.number,'.pdf'), get(paste0('Per_plots',Model.number)), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_1.pdf'), get(paste0('Per_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_2.pdf'), get(paste0('Per_plots',Model.number,'.2')), device = "pdf")
  #--------------  VAR con <Cohort> ---------------# 
  # Definicion del modelo
  Coh.VAR.lag     = 1
  cat("Rezago para Cohorte:",Coh.VAR.lag, "\n")
  Model.number    = '3'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
  Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
  Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
  outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
  Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
  Data_Coh[,dm_Component_ori:=dm_Component]
  Data_Coh[,dm_Component:=dm_Component-outlier_effect]
  
  Coh.data_mod = Data_Coh[, ..VAR.names.model]
  Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
  Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
  
  #Definicion de las restricciones de cero 
  Coh.N          = ncol(Coh.model$y)
  Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
  Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
  Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
  for (i in 1:Coh.N) {
    for (j in 1:Coh.N) {
      if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
    }
  }
  Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
  #summary(Coh.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    VAR_dif   = Coh.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }
  
  Coh.vars  <- colnames(Coh.res.model$y)
  Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
  Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
  Coh.N     <- length(Coh.vars)
  Coh.TT    <- nrow(Coh.res.model$y)
  Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
  for (i in seq_along(Coh.vars)) {
    mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
    upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
    lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    c_irf <- ggplot(Coh.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots3[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = c_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Coh',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Coh.vars
  #Coh_plots3 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
  #                          IRFS[[6]],IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],
  #                          layout_matrix=rbind(c(1,2,3,4),c(5,6,7,8),c(NA,9,10,NA)),
  #                          nrow=3,
  #                          top=paste0('Impulse-Response on Vintage Component for model ',
  #                                     Model.number))
  Coh_plots3 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
                            IRFS[[6]],IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],
                            layout_matrix=rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10)),
                            ncol=2)
  #Coh_plots3.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], 
  #                            IRFS[[6]],ncol=2)
  #Coh_plots3.2 = grid.arrange(IRFS[[7]],IRFS[[8]],IRFS[[9]],IRFS[[10]],ncol=2)
  #ggsave(paste0('CohIRF_Model',Model.number,'.pdf'), get(paste0('Coh_plots',Model.number)), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_1.pdf'), get(paste0('Coh_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_2.pdf'), get(paste0('Coh_plots',Model.number,'.2')), device = "pdf")
  
}
# Modelo 4---------------------------------------------------------------------
# VAR (Este modelo corresponde al <Modelo R1> del archivo de Excel <referencias>)
if(0){
  #--------------  VAR con <Periodo> ---------------# 
  # Definicion del modelo 
  Per.VAR.lag     = 4
  Model.number    = '4'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  data_mod        = Data_Per[, ..VAR.names.model] #Sup: la ult. var es el componente
  Per.pos_mod     = rowSums(is.na(data_mod))==0
  Per.model       = vars::VAR(data_mod[Per.pos_mod], p=Per.VAR.lag)
  var.names.plots4= c("Economic Activity Index","Private consumption",
                      "Unemployment Rate","Monetary Policy Interest Rate",
                      "Interest Rate Spread","Aggregate Credit")
  
  #Definicion de las restricciones de cero 
  Per.N          = ncol(Per.model$y)
  Per.RESVAR.mat = matrix(1,Per.N,Per.N)
  Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
  Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*Per.model$p), 1)
  for (i in 1:Per.N) {
    for (j in 1:Per.N) {
      if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*Per.model$p-(Per.N-j), Per.N)] <- 1
    }
  }
  Per.res.model <- restrict(Per.model, method='man', resmat=Per.rest.N.L)
  #summary(Per.res.model)
  
  # Res-tests
  if(1){
    cat("Rezago para Periodo",Per.VAR.lag, "\n")
    #Residual's hypothesis tests------------
    VAR_dif   = Per.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }  
  Per.vars  <- colnames(Per.res.model$y)
  Per.lags  <- max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
  Per.comp  <- Per.vars[grep('Component',Per.vars)]
  Per.N     <- length(Per.vars)
  Per.TT    <- nrow(Per.res.model$y)
  Per.dates <- Data_Per[Per.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
  for (i in seq_along(Per.vars)) {
    mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
    upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
    lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    p_irf <- ggplot(Per.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots4[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = p_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Per',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Per.vars
  Per_plots4 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
                            ncol=2)
  #Per_plots4 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
  #                         ncol=2, top=paste0('Impulse-Response on Period Component for model ',
  #                                            Model.number))
  #ggsave(paste0('PerIRF_Model',Model.number,'.pdf'), get(paste0('Per_plots',Model.number)), device = "pdf")
  
  
  
  #--------------  VAR con <Cohort> ---------------# 
  # Definicion del modelo 
  Coh.VAR.lag     = 3
  cat("Rezago para Cohorte",Coh.VAR.lag, "\n")
  Model.number    = '4'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
  Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
  Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
  outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
  Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
  Data_Coh[,dm_Component_ori:=dm_Component]
  Data_Coh[,dm_Component:=dm_Component-outlier_effect]
  
  Coh.data_mod = Data_Coh[, ..VAR.names.model]
  Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
  Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
  
  #Definicion de las restricciones de cero 
  Coh.N          = ncol(Coh.model$y)
  Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
  Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
  Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
  for (i in 1:Coh.N) {
    for (j in 1:Coh.N) {
      if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
    }
  }
  Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
  #summary(Coh.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    VAR_dif   = Coh.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }
  
  Coh.vars  <- colnames(Coh.res.model$y)
  Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
  Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
  Coh.N     <- length(Coh.vars)
  Coh.TT    <- nrow(Coh.res.model$y)
  Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
  for (i in seq_along(Coh.vars)) {
    mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
    upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
    lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    c_irf <- ggplot(Coh.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots4[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = c_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Coh',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Coh.vars
#  Coh_plots4 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
#               ncol=2, top=paste0('Impulse-Response on Vintage Component for model ',
#                                              Model.number))
# Coh_plots4 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
#                          ncol=2)
#ggsave(paste0('CohIRF_Model',Model.number,'.pdf'), get(paste0('Coh_plots',Model.number)), device = "pdf")
  
  
}

# Modelo 5---------------------------------------------------------------------
# VAR (Este modelo corresponde al <Modelo R3> del archivo de Excel <referencias>)
if(0){
  #--------------  VAR con <Periodo> ---------------# 
  # Definicion del modelo 
  Per.VAR.lag     = 3
  Model.number    = '5'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  data_mod        = Data_Per[, ..VAR.names.model] #Sup: la ult. var es el componente
  Per.pos_mod     = rowSums(is.na(data_mod))==0
  Per.model       = vars::VAR(data_mod[Per.pos_mod], p=Per.VAR.lag)
  var.names.plots5=c("VIX","Economic Activity Index",
                     "Retail sales", "Unemployment Rate",
                     "Monetary Policy Interest Rate",
                     "Interest Rate Spread","Aggregate Credit", 
                     'Exchange rate depreciation')
  
  #Definicion de las restricciones de cero 
  Per.N          = ncol(Per.model$y)
  Per.RESVAR.mat = matrix(1,Per.N,Per.N)
  Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
  Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*Per.model$p), 1)
  for (i in 1:Per.N) {
    for (j in 1:Per.N) {
      if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*Per.model$p-(Per.N-j), Per.N)] <- 1
    }
  }
  Per.res.model <- restrict(Per.model, method='man', resmat=Per.rest.N.L)
  #summary(Per.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Periodo",Per.VAR.lag, "\n")
    VAR_dif   = Per.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }  
  Per.vars  <- colnames(Per.res.model$y)
  Per.lags  <- max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
  Per.comp  <- Per.vars[grep('Component',Per.vars)]
  Per.N     <- length(Per.vars)
  Per.TT    <- nrow(Per.res.model$y)
  Per.dates <- Data_Per[Per.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
  for (i in seq_along(Per.vars)) {
    mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
    upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
    lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    p_irf <- ggplot(Per.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months',y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots5[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = p_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Per',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Per.vars
#  Per_plots5 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
#                            IRFS[[7]], IRFS[[8]], ncol=3,
#                            layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(NA,7,7,8,8,NA)),
#                            top=paste0('Impulse-Response on Period Component for model ',
#                                       Model.number))
  #Per_plots5 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
  #                          IRFS[[7]], IRFS[[8]], ncol=2)
  #Per_plots5.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]],
  #                            ncol=2)
  #Per_plots5.2 = grid.arrange( IRFS[[5]], IRFS[[6]],IRFS[[7]], IRFS[[8]],  ncol=2)
  #ggsave(paste0('PerIRF_Model',Model.number,'.pdf'), get(paste0('Per_plots',Model.number)), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_1.pdf'), get(paste0('Per_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_2.pdf'), get(paste0('Per_plots',Model.number,'.2')), device = "pdf")
  
  
  
  #--------------  VAR con <Cohort> ---------------# 
  # Definicion del modelo 
  Coh.VAR.lag     = 3
  Model.number    = '5'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
  Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
  Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
  outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
  Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
  Data_Coh[,dm_Component_ori:=dm_Component]
  Data_Coh[,dm_Component:=dm_Component-outlier_effect]
  
  Coh.data_mod = Data_Coh[, ..VAR.names.model]
  Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
  Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
  
  #Definicion de las restricciones de cero 
  Coh.N          = ncol(Coh.model$y)
  Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
  Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
  Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
  for (i in 1:Coh.N) {
    for (j in 1:Coh.N) {
      if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
    }
  }
  Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
  #summary(Coh.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Cohorte",Coh.VAR.lag, "\n")
    VAR_dif   = Coh.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }
  
  Coh.vars  <- colnames(Coh.res.model$y)
  Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
  Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
  Coh.N     <- length(Coh.vars)
  Coh.TT    <- nrow(Coh.res.model$y)
  Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
  for (i in seq_along(Coh.vars)) {
    mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
    upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
    lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    c_irf <- ggplot(Coh.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots5[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = c_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Coh',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Coh.vars
  #Coh_plots5 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
  #                         IRFS[[7]], IRFS[[8]], ncol=3, 
  #                          top=paste0('Impulse-Response on Vintage Component for model ',
  #                                     Model.number),
  #                         layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(NA,7,7,8,8,NA)))
  #Coh_plots5 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
  #                          IRFS[[7]], IRFS[[8]], ncol=3,
  #                          layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(NA,7,7,8,8,NA)))
  #Coh_plots5.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], 
  #                            ncol=2)
  #Coh_plots5.2 = grid.arrange(IRFS[[5]], IRFS[[6]],IRFS[[7]], IRFS[[8]], ncol=2)
  #ggsave(paste0('CohIRF_Model',Model.number,'.pdf'), get(paste0('Coh_plots',Model.number)), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_1.pdf'), get(paste0('Coh_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_2.pdf'), get(paste0('Coh_plots',Model.number,'.2')), device = "pdf")
  
  
}

# Modelo 6---------------------------------------------------------------------
# VAR (Este modelo corresponde al <Modelo 4> del archivo de Excel <referencias>)
if(0){
  #--------------  VAR con <Periodo> ---------------# 
  # Definicion del modelo 
  Per.VAR.lag     = 1
  Model.number    = '6'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  data_mod        = Data_Per[, ..VAR.names.model] #Sup: la ult. var es el componente
  Per.pos_mod     = rowSums(is.na(data_mod))==0
  Per.model       = vars::VAR(data_mod[Per.pos_mod], p=Per.VAR.lag)
  var.names.plots6= c("VIX","Economic Activity Index",
                      "Private consumption","Retail sales", "Unemployment Rate",
                      "Sovereign debt", "Inflation", 
                      "Monetary Policy Interest Rate",
                      "Interest Rate Spread","Aggregate Credit", 
                      'Exchange rate depreciation')
  
  #Definicion de las restricciones de cero 
  Per.N          = ncol(Per.model$y)
  Per.RESVAR.mat = matrix(1,Per.N,Per.N)
  Per.RESVAR.mat[1:(Per.N-1),Per.N] <- 0
  Per.rest.N.L <- cbind(matrix(0, Per.N, Per.N*Per.model$p), 1)
  for (i in 1:Per.N) {
    for (j in 1:Per.N) {
      if (Per.RESVAR.mat[i,j]==1) Per.rest.N.L[i, seq(j, Per.N*Per.model$p-(Per.N-j), Per.N)] <- 1
    }
  }
  Per.res.model <- restrict(Per.model, method='man', resmat=Per.rest.N.L)
  #summary(Per.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Periodo",Per.VAR.lag, "\n")
    VAR_dif   = Per.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }  
  Per.vars  <- colnames(Per.res.model$y)
  Per.lags  <- max(as.numeric(substring(colnames(Per.res.model$datamat)[grep(Per.vars[1],colnames(Per.res.model$datamat))],nchar(Per.vars[1])+3,nchar(Per.vars[1])+10)),na.rm=T)
  Per.comp  <- Per.vars[grep('Component',Per.vars)]
  Per.N     <- length(Per.vars)
  Per.TT    <- nrow(Per.res.model$y)
  Per.dates <- Data_Per[Per.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Per.irf  <- irf(Per.res.model,cumulative=T,n.ahead=hor,ci=0.9)
  for (i in seq_along(Per.vars)) {
    mean  <- as.numeric(Per.irf$irf[[Per.vars[i]]][,Per.comp])
    upper <- as.numeric(Per.irf$Upper[[Per.vars[i]]][,Per.comp])
    lower <- as.numeric(Per.irf$Lower[[Per.vars[i]]][,Per.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Per.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    p_irf <- ggplot(Per.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots6[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = p_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Per',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Per.vars
  Per_plots6 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
               IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]],
               layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(7,7,8,8,9,9),c(NA,10,10,11,11,NA)),
               nrow=3, top=paste0('Impulse-Response on Period Component for model ',
                                  Model.number))
  Per_plots6 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
                            IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]],
                            layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(7,7,8,8,9,9),c(NA,10,10,11,11,NA)),
                            nrow=3)
  Per_plots6.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
                              ncol=2)
  Per_plots6.2 = grid.arrange(IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]],
                              layout_matrix=rbind(c(7,7,8,8),c(9,9,10,10),c(NA,11,11,NA)),
                            ncol=2)
  #ggsave(paste0('PerIRF_Model',Model.number,'.pdf'), get(paste0('Per_plots',Model.number)), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_1.pdf'), get(paste0('Per_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('PerIRF_Model',Model.number,'_2.pdf'), get(paste0('Per_plots',Model.number,'.2')), device = "pdf")
  
  
  #--------------  VAR con <Cohort> ---------------# 
  # Definicion del modelo 
  Coh.VAR.lag     = 1
  Model.number    = '6'
  VAR.names.model = get(paste0('var.names.model.',Model.number))
  Coh.data_mod    = Data_Coh[, ..VAR.names.model] #Sup: la ult. var es el componente
  Coh.pos_mod     = rowSums(is.na(Coh.data_mod))==0
  Coh.model       = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag,exogen=Data_Coh[Coh.pos_mod,outlier_vars,with=F])
  outlier_effects = coefficients(Coh.model)$dm_Component[outlier_vars,'Estimate']
  Data_Coh[,eval(parse(text=paste0('outlier_effect:=',paste(paste0(outlier_vars,'*',outlier_effects),collapse='+'))))]
  Data_Coh[,dm_Component_ori:=dm_Component]
  Data_Coh[,dm_Component:=dm_Component-outlier_effect]
  
  Coh.data_mod = Data_Coh[, ..VAR.names.model]
  Coh.pos_mod  = rowSums(is.na(Coh.data_mod))==0
  Coh.model    = vars::VAR(Coh.data_mod[Coh.pos_mod],p=Coh.VAR.lag)
  
  #Definicion de las restricciones de cero 
  Coh.N          = ncol(Coh.model$y)
  Coh.RESVAR.mat = matrix(1,Coh.N,Coh.N)
  Coh.RESVAR.mat[1:(Coh.N-1),Coh.N] <- 0
  Coh.rest.N.L <- cbind(matrix(0, Coh.N, Coh.N*Coh.model$p), 1)
  for (i in 1:Coh.N) {
    for (j in 1:Coh.N) {
      if (Coh.RESVAR.mat[i,j]==1) Coh.rest.N.L[i, seq(j, Coh.N*Coh.model$p-(Coh.N-j), Coh.N)] <- 1
    }
  }
  Coh.res.model <- restrict(Coh.model, method='man', resmat=Coh.rest.N.L)
  #summary(Coh.res.model)
  
  # Res-tests
  if(1){
    #Residual's hypothesis tests------------
    cat("Rezago para Cohorte",Coh.VAR.lag, "\n")
    VAR_dif   = Coh.res.model
    VAR_htest = matrix(NA, nrow=1, ncol=3)
    colnames(VAR_htest) <- c('norm', 'Arch','autocorr')
    #----Normality
    VAR_htest[1] <- round(normality.test(VAR_dif)$jb.mul$JB$p.value, 4)
    #----Homoskedasticity
    VAR_htest[2] <- round(arch.test(VAR_dif, lags.multi = VAR_dif$p+1)$arch.mul$p.value, 4)
    #-----Autocorrelation
    VAR_htest[3] <-round(serial.test(VAR_dif, lags.pt = trunc(VAR_dif$obs/4+1),
                                     type = 'PT.asymptotic')$serial$p.value, 4)
    print(VAR_htest)
  }
  
  Coh.vars  <- colnames(Coh.res.model$y)
  Coh.lags  <- max(as.numeric(substring(colnames(Coh.res.model$datamat)[grep(Coh.vars[1],colnames(Coh.res.model$datamat))],nchar(Coh.vars[1])+3,nchar(Coh.vars[1])+10)),na.rm=T)
  Coh.comp  <- Coh.vars[grep('Component',Coh.vars)]
  Coh.N     <- length(Coh.vars)
  Coh.TT    <- nrow(Coh.res.model$y)
  Coh.dates <- Data_Coh[Coh.pos_mod]$Fechas
  
  # IRF del modelo restringido
  IRFS = list()
  Coh.irf  <- irf(Coh.res.model, cumulative=T, n.ahead=hor, ci=0.9)
  for (i in seq_along(Coh.vars)) {
    mean  <- as.numeric(Coh.irf$irf[[Coh.vars[i]]][,Coh.comp])
    upper <- as.numeric(Coh.irf$Upper[[Coh.vars[i]]][,Coh.comp])
    lower <- as.numeric(Coh.irf$Lower[[Coh.vars[i]]][,Coh.comp])
    dist  <- (upper-lower)/2
    upper <- mean + dist
    lower <- mean - dist
    
    Coh.data_irf <- data.table(H=0:hor,mean,lower,upper)
    
    c_irf <- ggplot(Coh.data_irf) +
      geom_ribbon(aes(x=H,ymin=lower,ymax=upper),fill="skyblue",alpha=.5) +
      geom_line(aes(x=H,y=mean),size=1.3) +
      geom_hline(yintercept=0) +
      labs(x='Months', y=" ", title=paste0('Panel ', letras[i]," ", var.names.plots6[i])) +
      scale_x_continuous(limits = c(0,hor), breaks=seq(0,hor,3))+
      theme(axis.title = element_text(size = 6),axis.text = element_text(size = 6),
            plot.title = element_text(size = 9))
    #print(p_irf)
    IRFS[[i]] = c_irf
    if(graficas_individuales){
      ggsave(paste0('Modelo',Model.number,'Coh',i,'.pdf'),plot=IRFS[[i]])
    }
  }
  names(IRFS) = Coh.vars
  Coh_plots6 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
                            IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]], 
                            layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(7,7,8,8,9,9),c(NA,10,10,11,11,NA)),
                            nrow=3)
  #Coh_plots6 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
  #                         IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]], 
  #                         layout_matrix=rbind(c(1,1,2,2,3,3),c(4,4,5,5,6,6),c(7,7,8,8,9,9),c(NA,10,10,11,11,NA)),
  #                         nrow=3, top=paste0('Impulse-Response on Vintage Component for model ',
  #                                          Model.number))
  
  Coh_plots6.1 = grid.arrange(IRFS[[1]], IRFS[[2]], IRFS[[3]], IRFS[[4]], IRFS[[5]], IRFS[[6]],
                              ncol=2)
  Coh_plots6.2 = grid.arrange(IRFS[[7]], IRFS[[8]], IRFS[[9]], IRFS[[10]], IRFS[[11]], 
                              layout_matrix=rbind(c(1,1,2,2),c(3,3,4,4),c(NA,5,5,NA)),
                              ncol=2)
  #ggsave(paste0('CohIRF_Model',Model.number,'.pdf'), get(paste0('Coh_plots',Model.number)), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_1.pdf'), get(paste0('Coh_plots',Model.number,'.1')), device = "pdf")
  #ggsave(paste0('CohIRF_Model',Model.number,'_2.pdf'), get(paste0('Coh_plots',Model.number,'.2')), device = "pdf")
  
  
  
}
