library(ggplot2)
library(gridExtra)
library(grid)
library(ggcorrplot)

## Graficados con GGPLOT

## Datos IRIS

data("iris")

str(iris)

## points with a small correlation Sepal.Length~Petal.Length

lm1 <- lm(iris$Petal.Length[iris$Species==unique(iris$Species)[1]]~iris$Sepal.Length[iris$Species==unique(iris$Species)[1]])
lm2 <- lm(iris$Petal.Length[iris$Species==unique(iris$Species)[2]]~iris$Sepal.Length[iris$Species==unique(iris$Species)[2]])
lm3 <- lm(iris$Petal.Length[iris$Species==unique(iris$Species)[3]]~iris$Sepal.Length[iris$Species==unique(iris$Species)[3]])

ggplot()+
  geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[1]],
                 y=iris$Petal.Length[iris$Species==unique(iris$Species)[1]],
                 color=unique(iris$Species)[1]))+
  geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[2]],
                 y=iris$Petal.Length[iris$Species==unique(iris$Species)[2]],
                 color=unique(iris$Species)[2]))+
  geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[3]],
                 y=iris$Petal.Length[iris$Species==unique(iris$Species)[3]],
                 color=unique(iris$Species)[3]))+
  geom_abline(color="#4C4C6D",intercept = lm1$coefficients[1],slope = lm1$coefficients[2])+
  geom_abline(color="#1B9C85",intercept = lm2$coefficients[1],slope = lm2$coefficients[2])+
  geom_abline(color="#99627A",intercept = lm3$coefficients[1],slope = lm3$coefficients[2])+
  xlab("Sepal Length")+ylab("Width Length")+
  scale_color_manual(name="Species", values=c(setosa="#4C4C6D",versicolor="#1B9C85",virginica="#99627A"))+
  theme(legend.position = "bottom")

##############################################################################################
## 
# Detalied

p1 <- ggplot()+
      geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[1]],
                     y=iris$Petal.Length[iris$Species==unique(iris$Species)[1]]),
                     color="#4C4C6D")+
      geom_smooth(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[1]],
                      y=iris$Petal.Length[iris$Species==unique(iris$Species)[1]]),
                      color="#4C4C6D", method = "lm")+
      xlab("Sepal Length")+ylab("Petal Length")+
      ggtitle("setosa")+
      theme(title = element_text(size=18,face = c("bold.italic")),
            axis.title = element_text(size=12, face ="plain" )); p1

p2 <- ggplot()+
      geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[2]],
                 y=iris$Petal.Length[iris$Species==unique(iris$Species)[2]]),
                 color="#1B9C85")+
      geom_smooth(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[2]],
                  y=iris$Petal.Length[iris$Species==unique(iris$Species)[2]]),
                  color="#1B9C85", method = "lm")+
      xlab("Sepal Length")+ylab("Petal Length")+
      ggtitle("versicolor")+
      theme(title = element_text(size=18,face = c("bold.italic")),
            axis.title = element_text(size=12, face ="plain" )); p2

p3 <- ggplot()+
      geom_point(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[3]],
                 y=iris$Petal.Length[iris$Species==unique(iris$Species)[3]]),
                 color="#99627A")+
      geom_smooth(aes(x=iris$Sepal.Length[iris$Species==unique(iris$Species)[3]],
                  y=iris$Petal.Length[iris$Species==unique(iris$Species)[3]]),
              color="#99627A", method = "lm")+
      xlab("Sepal Length")+ylab("Petal Length")+
      ggtitle("virginica")+
      theme(title = element_text(size=18,face = c("bold.italic")),
            axis.title = element_text(size=12, face ="plain" )); p3

p4 <- ggcorrplot(cor(iris[,c(1:4)]))+
      ggtitle("General Correlations")+
      theme(title = element_text(size=18,face = c("bold.italic")),
            axis.title = element_text(size=12, face ="plain" )); p4
      

grid.arrange(p1,p2,p3,p4, nrow=2, ncol=2)
