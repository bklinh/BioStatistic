#### data Iris
data("iris")
head(iris)
table(iris$Species)

### histogram
p = ggplot(iris, aes(x=Sepal.Length,
                     fill=Species))
p1<- p + geom_histogram(aes(y=..density..), binwidth=0.1, color="white", fill="blue") + 
  geom_density(alpha=0.5, col="red") + facet_grid(Species ~.)

p = ggplot(iris, aes(x=Petal.Length,
                     fill=Species))
p2 <- p + geom_histogram(aes(y=..density..), binwidth=0.1, color="white", fill="blue") + 
  geom_density(alpha=0.5, col="red") + facet_grid(Species ~.)

library(gridExtra)
grid.arrange(p1, p2, ncol=2)

#### boxplot

p = ggplot(iris, aes(x=Species, y=Sepal.Length))
p + geom_boxplot(fill="grey", color="black")

### Correlation 

p = ggplot(iris, aes(x=Sepal.Length,y=Petal.Length),fill=Species)
p + geom_point (aes(col=Species)) + geom_smooth(aes(col=Species))
p + geom_point (aes(col=Species)) 

### matrix

library(corrplot)
library(RColorBrewer)
#Remove Rows/Columns with Missing Values:
mcor <- cor(iris[,c(1:4)])

corrplot(mcor, type = "lower", order="hclust", mar=c(0,0,2,0), tl.col="black", tl.srt = 45, title = "Coleration between panicle traits of Grain_traitcan rice")
corrplot(mcor, method="number", type = "lower")
corrplot(mcor,type="lower", method="color", addCoef.col= "black",tl.col="black", tl.srt=45, col=brewer.pal(n=9, name="RdYlGn"))
corrplot(mcor,type="lower",addCoef.col= "black",tl.col="black", tl.srt=45, col=brewer.pal(n=9, name="Spectral"))




