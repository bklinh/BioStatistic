#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

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
df3 <- data_summary(ToothGrowth, varname="len", 
                    groupnames=c("supp", "dose"))
# Add: kiểm tra dữ liệu với 2 câu lệnh này
str(ToothGrowth) 
table(ToothGrowth$dose)
# Convert dose to a factor variable
df3$dose=as.factor(df3$dose)
head(df3)

library(ggplot2)
# Standard deviation of the mean as error bar
p <- ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Paired") + theme_minimal() 

# Change color by groups
# Add error bars
p + labs(title="Plot of length  per dose", 
         x="Dose (mg)", y = "Length")+
  scale_fill_manual(values=c('black','lightgray'))+
  theme_classic()

# Greens
p + scale_fill_brewer(palette="Greens") + theme_minimal()
# Reds
p + scale_fill_brewer(palette="Reds") + theme_minimal()


### barplot with multiple groups
ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")
# Use position=position_dodge()
ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())

# Create the barplot with label 
df3$lenlab <- cumsum(df3$len)
ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=len, label=len), vjust=1.6, 
            color="white", size=4)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

ggplot(data=df3, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=1.6, color="white",
            position = position_dodge(0.9), size=4)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()
