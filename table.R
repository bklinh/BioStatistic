#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
pisa <- read.table("C:\\Users\\Admin\\Downloads\\PISA DATA.txt", sep = "\t", header = T)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

##Summarize the data :
df_pisa <- data_summary(pisa, varname="PV1MATH", 
                    groupnames=c("REGION", "AREA"))

# Convert to a factor variable
df_pisa$REGION=as.factor(df_pisa$REGION)
head(df_pisa)
df_pisa$AREA=as.factor(df_pisa$AREA)
str(df_pisa)

library(ggplot2)
# Standard deviation of the mean as error bar
p <- ggplot(df_pisa, aes(x=REGION, y=PV1MATH, fill=AREA)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=PV1MATH-sd, ymax=PV1MATH+sd), width=.2,
                position=position_dodge(.9))

pop = c(130, 189, 200, 156, 154, 160, 162, 170, 145, 140)
sample1 = sample(pop,5)
sample2 = sample(pop,5)
sample3 = sample(pop,5)

height <- rnorm(500, 160, 6.2)
hist(height, breaks=50)
pnorm(150, 160, 6.5)

pisa <- read.table("C:\\Users\\Admin\\Downloads\\PISA DATA.txt", sep = "\t", header = T)
library(psych)
summary(pisa)
describe(pisa)
attach(pisa)
describe(cbind(PV1MATH, PV1SCIE, PV1READ), skew=F)
# mô tả theo khu vực
describeBy(cbind(PV1MATH, PV1SCIE, PV1READ), group = REGION, skew=F, range=F)
library(tables)
pisa$REGION=as.factor(pisa$REGION)
pisa$AREA=as.factor(pisa$AREA)
pisa$TYPE=as.factor(pisa$TYPE)
tabular(REGION ~ 1*(n=1), data=df_pisa)
tabular((REGION) ~ 1 * (n=1 + Percent("col")), data=pisa)
tabular((REGION) ~ AREA * (n=1 + Percent("col")), data=pisa)
tabular(REGION*AREA ~ TYPE, data=pisa)
tabular((REGION)*AREA ~ TYPE*(n=1 + Percent("row")), data=pisa)
tabular(REGION ~ PV1MATH * (n=1 + mean + sd), data=pisa)
tabular(REGION ~ (PV1MATH+PV1READ) * (n=1 + mean + sd), data=pisa)
pisa$SCHOOLID=as.factor(pisa$SCHOOLID)
tabular(SCHOOLID  ~  PV1MATH*(n=1 + mean + sd), data=pisa)
t = tabular(REGION*AREA ~ PV1MATH * (n=1 + mean + sd), data=pisa)
setwd("S:\\R PROGRAMMING\\BioStatistics")
write.csv.tabular(t, "tabular_pisa.csv")
write.csv.tabular(t, "tabular_pisa.txt", sep="\t")
