####Q1. Input data in R
Weight_year1 <- c(56,76,65,61,49)
Weight_year2 <- c(58,77,65,62,51)
Weight_year3 <- c(55,77,64,65,53)
BMI_year1 <- c(1.16,1.1, 0.97,0.78,1.07)
BMI_year2 <- c(1.15, 1.06, 0.96, 0.77, 1.05)
BMI_year3 <- c(1.11, 1.07, 0.96, 0.75, 1.05)
DIS_year1 <- c(1,1,0,1,0)
DIS_year2 <- c(0,1,0,1,0)
DIS_year3 <- c(0,0,1,1,1)
ID <- c(1:5)

library(tidyverse)

# Create a data frame with the given data
df <- data.frame(ID = 1:5,
                 Weight_year1 = c(56,76,65,61,49),
                 Weight_year2 = c(58,77,65,62,51),
                 Weight_year3 = c(55,77,64,65,53),
                 BMI_year1 = c(1.16,1.1, 0.97,0.78,1.07),
                 BMI_year2 = c(1.15, 1.06, 0.96, 0.77, 1.05),
                 BMI_year3 = c(1.11, 1.07, 0.96, 0.75, 1.05),
                 DIS_year1 = c(1,1,0,1,0),
                 DIS_year2 = c(0,1,0,1,0),
                 DIS_year3 = c(0,0,1,1,1)
)

# Reshape the data into a long format
df_long <- df %>%
  pivot_longer(cols = starts_with(c("Weight_", "BMI_", "DIS_")),
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\w+)",
               values_drop_na = TRUE)

# Print the reshaped data
print(df_long)

#Q2. 
ISI <- c(9313,31231,42464,54336,134125,237730,247670)
Country <- c("Vietnam","Thailand","MMalaysia","Singapore","Taiwan","Korea","Australia")
dat4 <- data.frame(Country, ISI)
str(dat4)
#Create the bar chart
barplot(ISI, names.arg = Country, main = "Bar Chart", xlab = "Country", ylab = "ISI")

# Load the ggplot2 library
library(ggplot2)
# Create the bar chart using ggplot2
ggplot(dat4, aes(x = Country, y = ISI)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Bar Chart", x = "Country", y = "ISI")

# Sort the data frame by values in descending order
dat4 <- dat4[order(-dat4$ISI), ]

# Create the bar chart using ggplot2
ggplot(dat4, aes(x = reorder(Country, ISI), y = ISI)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "International publication (ISI) data for some Asian countries from 2010-2014", x = "Country", y = "ISI")

#Q3:
# Load the ggplot2 library
library(ggplot2)

# Sample data
years <- c(2010, 2011, 2012, 2013, 2014)
vietnam_articles <- c(1331,1444,1809,2236,2493)
thailand_articles <- c(5451,5931,6232,6733,6884)
malaysia_articles <- c(6039,7992,8311,9450,10672)

# Create the data frame
data <- data.frame(years, vietnam_articles, thailand_articles, malaysia_articles)

# Convert data to long format
data_long <- tidyr::gather(data, country, articles, -years)

# Create the line graph using ggplot2
ggplot(data_long, aes(x = years, y = articles, color = country)) +
  geom_line() +
  labs(title = "Number of Scientific Articles (ISI)",
       x = "Year", y = "Number of Articles",
       color = "Country")
       
#Q4
# Load the iris dataset
data(iris)
# Stripchart
stripchart(iris$Sepal.Length, method = "stack", xlab = "Species", ylab = "Sepal Length")

# Create a stripchart using ggplot2
ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_jitter(width = 0.2, height = 0, color = "steelblue", alpha = 0.7) +
  labs(x = "Species", y = "Sepal Length") +
  ggtitle("Stripchart of Sepal Length by Species")

# Boxplot
boxplot(Sepal.Length ~ Species, data = iris, xlab = "Species", ylab = "Sepal Length")

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(fill = "lightblue", color = "steelblue") +
  labs(x = "Species", y = "Sepal Length") +
  ggtitle("Boxplot of Sepal Length by Species")
  
# Calculate average and sd by species group
library(tables)
str(iris)
tabular(Species~Sepal.Length * (n=1 + mean + sd), data= iris)

###Q5
set.seed(123)  # Set a seed for reproducibility
sample_size <- 1000  # Number of samples to generate

mean_iq <- 105  # Mean IQ
sd_iq <- 15  # Standard deviation of IQ

sample_iq <- rnorm(sample_size, mean_iq, sd_iq)  # Generate the sample

# Print the first 10 IQ values for demonstration
print(sample_iq[1:10])

hist(sample_iq, breaks = "FD", main = "Histogram of IQ Values",
     xlab = "IQ", ylab = "Frequency")


