########## Q1
data1m <- rnorm(1e6, 165, 6.72)
data1f <- rnorm(1e6, 154, 5.35)

# Combine the data into a data frame
df1 <- data.frame(value = c(data1m, data1f),
                 gender = rep(c("Male", "Female"), each = 1e6))

ggplot(df1, aes(x = gender, y = value, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Height", title = "Height Distribution by Gender")

pnorm(150, 165, 6.72)
pnorm(150, 154, 5.35)

########## Q2
library(dplyr)
orange <- read.table("C:\\Users\\Admin\\Downloads\\Orange_tree.txt", header = T)
str(orange$circumference)

orange<- orange[order(orange$circumference), ]
orange$Tree <- factor(orange$Tree)

# Calculate the mean circumference for each tree
tree_means <- orange %>% 
  group_by(Tree) %>% 
  summarise(mean_circumference = mean(circumference))

# Sort the factor levels based on the mean circumference
orange$Tree <- factor(orange$Tree, levels = tree_means$Tree[order(tree_means$mean_circumference)])

# Boxplot
ggplot(orange, aes(x = Tree, y = circumference)) +
  geom_boxplot() +
  labs(title = "Box Plot of Circumference of Orange Trees",
       y = "Circumference")

# Create the bar plot
ggplot(orange, aes(x = Tree, y = circumference)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Circumference of Orange Trees",
       x = "Tree ID",
       y = "Circumference")

# Correlation
p = ggplot(orange, aes(x=age,y=circumference, color=Tree))
p + geom_point() + geom_line() 

library(psych)
attach(orange)
describeBy(circumference, group=age, skew=F, range=F)

library(tables)
orange$age = as.factor(orange$age)
tabular((Tree + age) ~ circumference * (n=1 + mean + sd), data=orange)

########## Q3
easy_count <- 39
medium_count <- 19
hard_count <- 19

num_exams <- 15

num_easy <- 3
num_medium <- 1
num_hard <- 1

get_questions <- function(count, num) {
  sample(1:count, num, replace = FALSE)
}

exams <- matrix(NA, nrow = num_exams, ncol = num_easy + num_medium + num_hard + 1)

colnames(exams) <- c("De thi", "Toan_de1", "Toan_de2", "Toan_de3", "Toan_TB", "Toan_kho")

exams[, "De thi"] <- 1:num_exams

# Fill questions to each exam
for (i in 1:num_exams) {
  easy <- get_questions(easy_count, num_easy)
  medium <- get_questions(medium_count, num_medium)
  hard <- get_questions(hard_count, num_hard)
  exams[i, 2:(num_easy + 1)] <- easy
  exams[i, (num_easy + 2):(num_easy + num_medium + 1)] <- medium
  exams[i, (num_easy + num_medium + 2):(num_easy + num_medium + num_hard + 1)] <- hard
}

# CSV
setwd("S:\\R PROGRAMMING\\BioStatistics")
write.csv(exams, file = "de_thi1.csv", row.names = FALSE)

