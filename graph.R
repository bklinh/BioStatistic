os <- read.table("C:\\Users\\Admin\\Downloads\\osteo data.txt", header = T)
# See first 6 lines
head(os)
# See last 6 lines
tail(os)
# view variable names
names(os)
# view variable format
str(os)
# convert to numeric (continuous variable)
os$wt <- as.numeric(os$wt)
os$fnbmd <- as.numeric(os$fnbmd)
# check the format of variables
typeof(os$wt)
typeof(os$fnbmd)
# check the data how many men, how many women
table(os$gender)
# create a subset with a group of patients with age 60 or older, name it osteo
osteo = subset(os, age>=60)

boxplot(os$wt)
hist(os$wt)
boxplot(os$wt, main = "Weight(kg)", xlab = "Weight", ylab = "Kg", col = "red", nod = T)
boxplot(os$wt~os$gender, main = "Weight(kg)", xlab = "Weight", ylab = "Kg", col = c("red","blue"), names=c("women", "men"))
plot(osteo$bmi~osteo$age, pch=2, col="blue")
abline(lm(osteo$bmi~osteo$age), col="red", lwd=3)
library("car")
scatterplot(osteo$bmi~osteo$age|osteo$gender,pch=c(1,1), col=c("red", "black"), xlab="age", ylab="BMI", smooth=F)
library("psych")
pairs.panels(osteo[,c(3:7)]) #lấy tất cả hàng, cột từ 3 đến 7
library("PerformanceAnalytics")
chart.Correlation(osteo[,c(3:7)])

school <- c("A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "C","C", "D", 
          "E", "E", "E", "E", "E", "E", "E", "F","F", "G", "G", "G", "H", "H")
freq <- table(school)
barplot(freq)
colors = c("red", "yellow", "green", "blue", "pink", "purple", "turquoise", "orange")
# Add colors
barplot(freq, col = colors)
# Horizontal Chart
barplot(sort(freq), col = colors, horiz = T)

female = subset(osteo, gender=="Female")
sd1 <- sd(female$wt, na.rm = T)
mean(female$wt, na.rm = T)
se1 <- sd1/sqrt(length(na.omit(female$wt)))

male = subset(osteo, gender=="Male")
sd2 <- sd(male$wt, na.rm = T)
mean(male$wt, na.rm = T)
se2 <- sd2/sqrt(length(na.omit(male$wt)))


barplot(female$wt, breaks = seq(min(female$wt), max(female$wt), length.out=10), labels=F)
?barplot

shapiro.test(osteo$wt)

pisa <- read.table("C:\\Users\\Admin\\Downloads\\PISA DATA.txt", sep = "\t", header = T)
str(pisa)
# How many male and female in the data
table(pisa$ST04Q01)
hist(pisa$PV1MATH, probability = T)
# Describe the math score PV1MATH by boxplot with info of REGION, TYPE, AREA
boxplot(pisa$PV1MATH~pisa$REGION, col=c("red", "blue", "yellow"))
boxplot(pisa$PV1MATH~pisa$TYPE, , col=c("red", "blue"))
boxplot(pisa$PV1MATH~pisa$AREA, col=c("red", "blue", "yellow"))
# Make the correlation plot between PV1MATH and PV2MATH with the info of gender
library("car")
scatterplot(pisa$PV1MATH~pisa$PV2MATH|pisa$ST04Q01,pch=c(1,1), col=c("red", "black"), xlab="age", ylab="BMI", smooth=T)

