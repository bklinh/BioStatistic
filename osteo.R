data <- read.table("C:\\Users\\Admin\\Downloads\\osteo data.txt", header = T)
str(data)
data$gender <- as.factor(data$gender)
data$wt <- as.numeric(data$wt)
data$bmi <- as.numeric(data$bmi)
data$fnbmd <- as.numeric(data$fnbmd)
str(data)
names(data)
head(data)
tail(data)
dim(data) #row - column
edit(data$id)

data$Group[data$gender=="Female"] <- 1
data$Group[data$gender=="Male"] <- 2

data$Type[data$wt>50] <- "A"
data$Type[data$wt<50] <- "B"
table(data$Type)

object1 <- subset(data, data$gender=="Female")
object2 <- subset(data, data$gender=="Male" & data$wt>62)

newdata <- data[order(data$age), ]
newdata <- data[order(data$age), c(1:4)]

id <- c(1:4)
gender <- c("M", "F","M", "F")
group <- c(1,1,2,2)
day1 <- c(15,16,21,31)
day2 <- c(17,15,23,25)
day3 <- c(19,20,19,33)
dat <- data.frame(id, gender, group, day1, day2, day3)
dat
require(reshape2)
# giữ lại biến gender và group thay đổi theo id
dat1 <- melt(dat, id<-c("id", "gender", "group"), measure.vars = c("day1", "day2", "day3"))

dat1 <- melt(dat, id<-c(1,3), measure.vars = c("day1", "day2", "day3"))


