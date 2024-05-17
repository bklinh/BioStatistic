pisa <- read.table("C:\\Users\\Admin\\Downloads\\PISA DATA.txt", sep = "\t", header = T)
# mô tả theo khu vực
library(psych)
attach(pisa)
describeBy(cbind(PV1MATH), group = ST04Q01, skew=F, range=F)
t.test(PV1MATH~ST04Q01)

A <- c(8,9,11,4,7,8,5)
B <- c(7,17,10,14,12,24,11,22)
C <- c(28,21,26,11,24,19)
D <- c(26,16,13,12,9,10,11,17,15)
x = c(A,B,C,D)
group = c(rep('A',7), rep('B',8), rep('C',6), rep('D',9))
data = data.frame(x, group)
summary(aov(x~group))

ma = mean(A)
sswa = 0
for (i in A){
  sswa = (i-ma)^2 + sswa
}
  
mb = mean(B)
sswb = 0
for (i in B){
  sswb = (i-mb)^2 + sswb
}

mc = mean(C)
sswc = 0
for (i in C){
  sswc = (i-mc)^2 + sswc
}

md = mean(D)
sswd = 0
for (i in D){
  sswd = (i-md)^2 + sswd
}


# Practice
pg <- read.table("S:\\R PROGRAMMING\\BioStatistics\\penguins_R.txt", header=T, sep=",")
?read.table
library(ggplot2)
p = ggplot(pg, aes(x=species,y=flipper_length_mm, fill=species))
p + geom_boxplot() 
attach(pg)
describeBy(cbind(flipper_length_mm), group = species, skew=F, range=F)

list_of_species <- split(pg, pg$species)
# Accessing the individual tables
Adelie <- list_of_species[[1]]
Chinstrap  <- list_of_species[[2]]
Gentoo <- list_of_species[[3]]
res_aov<- aov(flipper_length_mm~species,data=pg)
summary(res_aov)
av <- aov(c(Adelie$flipper_length_mm,Chinstrap$flipper_length_mm,Gentoo$flipper_length_mm)~pg$flipper_length_mm)
summary(av)

library(report)
report(av)

# 13/05/2024
A1_data <- c(72,78,80,76,74,79,77,75,73,81)
t.test(A1_data, mu=75)
# H0 accepted

before <- c(65, 70, 72, 68, 71, 65, 63, 70)
after <- c(75, 80, 78,82, 79, 76, 80, 74, 76, 70)
t.test(before, after, paired = T) #same group
t.test(before, after, paired = F) #independent t-test, default

pg <- read.table("S:\\R PROGRAMMING\\BioStatistics\\penguins_R.txt", header=T, sep=",")
?read.table
library(ggplot2)
p = ggplot(pg, aes(x=species,y=flipper_length_mm, fill=species))
p + geom_boxplot() 
attach(pg)
describeBy(cbind(flipper_length_mm), group = species, skew=F, range=F)

# Penguin Data
pg <- read.table("S:\\R PROGRAMMING\\BioStatistics\\penguins_R.txt", header=T, sep=",")
?read.table
library(ggplot2)
p = ggplot(pg, aes(x=species,y=flipper_length_mm, fill=species))
p + geom_boxplot() 
attach(pg)
describeBy(cbind(flipper_length_mm), group = species, skew=F, range=F)

list_of_species <- split(pg, pg$species)
# Accessing the individual tables
Adelie <- list_of_species[[1]]
Chinstrap  <- list_of_species[[2]]
Gentoo <- list_of_species[[3]]
res_aov<- aov(flipper_length_mm~species,data=pg)
summary(res_aov)
av <- aov(c(Adelie$flipper_length_mm,Chinstrap$flipper_length_mm,Gentoo$flipper_length_mm)~pg$flipper_length_mm)
summary(av)

library(report)
report(av)

post3 <- glht(res_aov, linfct = mcp(group = "species"))

# Galactose in 3 patient groups
crohn = c(1.343, 1.393, 1.420, 1.641, 1.897, 2.160, 2.169, 2.279, 2.890) 
colitis = c(1.264, 1.314, 1.399, 1.605, 2.385, 2.511, 2.514, 2.767, 2.827, 2.895, 3.011) 
control = c(1.809, 1.926, 2.283, 2.447, 2.479, 2.495, 2.525, 2.541, 2.769, 2.850, 2.964, 2.973, 3.171, 3.257, 3.271, 3.288, 3.358, 3.643, 3.657) 
gal = c(crohn, colitis, control)
group = c(rep('crohn',9), rep('colitis',11), rep('control',19))
data = data.frame(gal, group)
p = ggplot(data, aes(x=group, y=gal)
p + geom_boxplot(notch=T)) 
model <- aov(gal~group)
summary(aov(gal~group))

# Kết luận ???
t.test(crohn, colitis)
t.test(colitis, control)
t.test(crohn, control)
TukeyHSD(model)
plot(TukeyHSD(model), order=T)
library(multcomp)
data$group = as.factor(data$group)
post_test <- glht(model, linfct = mcp(group = "Tukey"))


data$group <- relevel(data$group, ref="control")
levels(data$group)
post_test1 <- glht(model, linfct = mcp(group = "Dunnet"))

pairwise.t.test(gal, group, p.adjust="bonferroni",pool.sd=T)
pairwise.t.test(gal, group, p.adjust="BH",pool.sd=T)
