### Cargas librer?as a utilizar
suppressMessages(library(plotly)) # Graficas interactivas
suppressMessages(library(Quandl)) # Descargar Precios
suppressMessages(library(PortfolioAnalytics)) # Teor?a Moderna de Portafolios
suppressMessages(library(ROI)) # Optimizacion para portafolio

suppressMessages(library(knitr))  # Opciones de documentaci?n + c?digo
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

Quandl.api_key("PU3GsQWU2zsn6N8_eq71")


funcion_pesos<-function(i){
  f<-paste("IBB_holdings_",i,".csv",sep="")
  data_iak<- read.csv(f,row.names = NULL,skip = 10)
  #primero<-which(x = data_iak[,1]=="Ticker")
  #ultimo<-length(data_iak[,1])
  #delete.na(data_iak,n=0)
  tk <- as.character(data_iak[,1])
  pesos <-as.integer(data_iak[,4]*1000)
  num <- as.numeric(data_iak[,6])
  p<-data_iak[,5]
  return(data.frame(tk,pesos,num,p))
}  
  
Bajar_Precios<-function(c,tk,f1,f2){
  Datos<-Quandl.datatable("WIKI/PRICES",qopts.columns=c,ticker=tk,date.gte=f1,date.lte=f2,paginate=TRUE)
  return (data.frame(Datos))
}
  
cs<-c("adj_close","ticker","date")


data<-list()

for (i in 1:12){
 data[[i]]<-funcion_pesos(i)
}

#Escenario 1
tk_1<-(data[[1]]$tk)
fechas<-c("2017-02-28","2017-03-31","2017-04-28","2017-05-31","2017-06-30","2017-07-31","2017-08-31","2017-09-29","2017-10-31","2017-11-30","2017-12-29","2018-01-31","2018-02-28")

fecha<-as.Date("2017-02-28")
fecha2<-as.Date("2017-03-28")
precios4<-Bajar_Precios(cs,tk_1,fecha,fecha)
fecha<-fecha+1
while ((fecha<fecha2)) {
  precios3<-Bajar_Precios(cs,tk_1,fecha,fecha)
  precios4<-rbind(precios4,precios3)
  fecha<-fecha+1
}





precios<-Bajar_Precios(cs,tk_1,fechas[1],fechas[1])

for (i in 2:length(fechas)){
  precios2<-Bajar_Precios(cs,tk_1,fechas[i],fechas[i+1])
  precios<-rbind(precios,precios2)
}
rm(precios2)

tk_existentes<-unique(precios[,2])


precios_<-matrix(nrow = length(tk_existentes),ncol = length(fechas))
x<-1
y<-1
for (f in fechas){
  for (i in 1:nrow(precios)){
    if (precios[i,3]==f){
      precios_[x,y]=precios[i,1]
      x<-x+1
    }
  }
  y<-y+1
  x<-1
}
rownames(precios_)<-tk_existentes
colnames(precios_)<-fechas

x<-1
vector<-matrix(nrow = length(tk_existentes),ncol = 2)
for (i in 1:length(data[[1]]$num)){
  if (data[[1]]$tk[i] %in% tk_existentes )
  {
   vector[x,1]<-data[[1]]$tk[i]
   vector[x,2]<-data[[1]]$num[i]
   x<-x+1
  }
}

