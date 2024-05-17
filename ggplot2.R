pisa <- read.table("C:\\Users\\Admin\\Downloads\\PISA DATA.txt", sep = "\t", header = T)
str(pisa)
library(ggplot2)
p = ggplot(pisa, aes(x=PV1MATH))
p + geom_histogram(color="white", fill="blue")

# Add density
# os <- read.table("C:\\Users\\Admin\\Downloads\\osteo data.txt", header = T)
# # create a subset with a group of patients with age 60 or older, name it osteo
# osteo = subset(os, age>=60)
p = ggplot(pisa, aes(x=PV1MATH, after_stat(density)))
p + geom_histogram(binwidth = 20, color="white", fill="blue") 
  + geom_density(alpha=0.3, col="red")

# distribution by group (REGION)
p = ggplot(pisa, aes(x=PV1MATH, fill=REGION))
p1= p+ geom_histogram(binwidth=10, position="dodge", col="white") + geom_density(alpha=0.5, col="red")
  theme(legend.position = "top") + facet_grid(REGION~.) + theme_bw() + xlim(200,800)
p2= p+geom_density(alpha=0.4, col="red") + theme() + facet_grid(REGION~.) 
p3= p+ geom_histogram(aes(y=..density..), binwidth = 10, color="white", fill="blue") + geom_density(alpha=0.5, col="red")+ facet_grid(REGION~.) + xlim(200,800)

library(gridExtra)
grid.arrange(p1, p2, ncol=2)
grid.arrange(p1, p2, nrow=2)
sdat = aggregate(pisa$PV1MATH, by=list(pisa$REGION), FUN=function(x) c(mean = mean(x), sd = sd(x)))
colnames(sdat) = c("Group", "Mean")

# BARPLOT
# Create new variable 
# tạo 1 biến mới tên là group trong pisa, nếu điều kiện ... thì sẽ hiển thị trong cột là...
pisa$Group[pisa$REGION=="NORTH"&pisa$AREA=="URBAN"] = "North/Urban"
pisa$Group[pisa$REGION=="NORTH"&pisa$AREA=="RURAL"] = "North/Rural"
pisa$Group[pisa$REGION=="NORTH"&pisa$AREA=="REMOTE"] = "North/Remote"
pisa$Group[pisa$REGION=="SOUTH"&pisa$AREA=="URBAN"] = "South/Urban"
pisa$Group[pisa$REGION=="SOUTH"&pisa$AREA=="RURAL"] = "South/Rural"
pisa$Group[pisa$REGION=="SOUTH"&pisa$AREA=="REMOTE"] = "South/Remote"
pisa$Group[pisa$REGION=="CENTRAL"&pisa$AREA=="URBAN"] = "Central/Urban"
pisa$Group[pisa$REGION=="CENTRAL"&pisa$AREA=="RURAL"] = "Central/Rural"
pisa$Group[pisa$REGION=="CENTRAL"&pisa$AREA=="REMOTE"] = "Central/Remote"
p = ggplot(pisa, aes(x=Group, y=PV1MATH, fill=Group)) 
p + stat_summary(fun.y="mean", geom="bar") + stat_summary(fun.data = "mean_cl_normal", geom="errorbar") + coord_flip() +theme(legend.position = "none")


# BOXPLOT
p = ggplot(pisa, aes(x=REGION, y=PV1MATH))
p + geom_boxplot()
# split into groups
p = ggplot(pisa, aes(x=REGION, y=PV1MATH, fill=AREA))
p + geom_boxplot(notch=T)

library(corrplot)
mcor_os <- cor(na.omit(osteo[,c(3:7)]))
corrplot(mcor_os, method="pie")
corrplot(mcor_os, method="color")
corrplot(mcor_os, method="number",type="lower", order="hclust", mar=c(0,0,2,0), tl.col="black", tl.srt=45, title="Colleration")

data("iris") 
p = ggplot(iris, aes(x=Sepal.Length), fill= Species)
p + geom_histogram(aes(y=..density..), color="white", fill="black") + geom_density(alpha=0.5,aes(fill=Species)) 
pp = ggplot(iris, aes(x=Sepal.Length, y=Petal.Length))
pp + geom_boxplot(aes(fill=Species), notch=T)
p1 = ggplot(iris, aes(x=Petal.Length))
p1 + geom_histogram(fill="blue", color="white")
# tương quan hai biến là scatterplot, nhiều biến là matrix
library("car")
scatterplot(iris$Sepal.Length~iris$Petal.Length|iris$Species,pch=c(1,1,1), col=c("red", "black","green"), smooth=T)
p = ggplot(iris, aes(x=Sepal.Length, y=petal.Length))

mcor_os <- cor(iris[,c(1:4)])
corrplot(mcor_os, type = "lower")
