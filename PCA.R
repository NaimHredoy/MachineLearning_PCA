#MD SHAH NAIM HREDOY
#hridhowlader@gmail.com

#setting up working directory
#Importing the data set

Data=read.csv("Data.csv",header=TRUE)
summary(Data)

#checking correlation

library(gclus)
my.abs<- abs(cor(Data))
my.colors<- dmat.color(my.abs)
my.ordered<- order.single(cor(Data))
cpairs(Data, my.ordered, 
       panel.colors=my.colors, 
       gap=0.5)
#or
pairs.panels(Data,
             gap=0,
             pch=21)
#PCA

pca<-prcomp(Data, 
            scale. = TRUE)
summary(pca)
pca$rotation

#orthogonality of PC

pairs.panels(pca$x,
             gap=1,
             pch = 21)


#getting eigen value, scree plot, and biplot

pca$sdev^2

plot(pca, type="l", 
     main=("Scree Plot"))

#pca socres

str(pca)
pcscore<- cbind(Data, 
                pca$x[,1:2]) #extracted pc by kaiser criteria
head(pcscore)

#for better visualization of PCA

library(ggplot2)
ggplot(pcscore, aes(PC1,PC2))+
  stat_ellipse(geom = "polygon", 
               col="black", 
               alpha=.4)+
  geom_point(shape=10, 
             col="blue")

#correlation among pc and variables

corrplot (cor(Data, 
              pcscore[, 17:18]), 
          method = "ellips")
