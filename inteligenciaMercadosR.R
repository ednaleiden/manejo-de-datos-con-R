#muestra variable de cabecera
head(Boston,9)

#resumen 
summary(Boston)

#media -- signo dolar el nombre de la variable
mean(Boston$crim)

#desviacion
sd(Boston$crim)

min(Boston$crim)

max(Boston$crim)

median(Boston$crim)
#longitud
length(Boston$crim)


#CONSULTAR DATOS NULLOS

sum(is.na(Boston$crim))

#tabla
#table()
#sapli aplica una caracteristica de la tabla

#funciones para iterar

data.frame(mean=sapply(Boston,mean),
           sd=sapply(Boston,sd),
           miss_value=sapply(Boston,function(x)sum(length(which(is.na(x)))))
           
           )

#redondeo

round(cor(Boston),2)

#tabla -  conteo ceros y unos o f y m
table(Boston$chas)

#variable nueva

#.bincode() elimina decimales
Boston$RM.bin=.bincode(Boston$rm,c(2,9))
  
#ver como funciona una funcion en otra haciendo agregacion
#variable que me aparecera en lo ultimo agrupar en ceros y unos
aggregate(Boston$medv,by=list(CHAS=Boston$chas,RM=Boston$rm),FUN=mean)

#funcion de grafico de calor derretir

mtl=melt(Boston,id=("chas"),measure=c("medv"))


cast(mlt,rm-chas,subset =="medv",margins = T,mean)

#conteo
A=table(Boston$rad)

#conteos se convierte en porcentaje
B=prop.table(A)

#todo manipular pasar a dataframe vamos a pasar de tabel a dataframe
C=as.data.frame(B)

ggplot(C,aes(x="",y=Freq,fill=Var1))+
  geom_bar(stat = "identity",color="gray")+
    coord_polar(theta = "y")



# otros Graficos
plot(Boston$medv~Boston$lstat,xlab="MEDV",ylab="LSTAT")

ggplot(Boston)+
  geom_point(aes(x=lstat,y=medv),colour="green")+
  theme(panel.background = element_rect(fill = "transparent"))


## graficos  barras CHAS  VS MEAN  MEDV

#comparacion de la  meda dev por cada chas

datos_para_plot = aggregate(Boston$medv,by = list(Boston$chas),
                            FUN=mean)
#c vectores
names(datos_para_plot)=c("CHAS","MeanMEDV")


barplot(datos_para_plot$MeanMEDV,names.arg = datos_para_plot$CHAS,
        xlab = "CHAS",ylab="meanMEDV")

ggplot(datos_para_plot)+
  geom_bar(aes(x=CHAS,y=MeanMEDV,fill="red"),stat = "identity")

#Histogramas

hist(Boston$medv,xlab="MEDV",main = "grafico",breaks = 50)

ggplot(Boston)+geom_histogram(aes(x=medv),binwidth = 1,fill="green",colour="blue")+
  labs(x="MEDV",y="Frecuencia")+
  theme_light()





  
