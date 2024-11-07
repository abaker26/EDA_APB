#Import libraries 
library(tidyverse)
library(MASS)
library(datasets)
data(package = .packages(all.available = TRUE))

#Load the data set
data<-longley
head(data)

#Structure of the data
str(data)

#Summary of the data
summary(data)


# Calculate the count and standard deviation for all numeric variables
summary_stats <- longley %>%
  summarise(across(where(is.numeric), list(count = ~length(.), stdev = ~sd(., na.rm = TRUE))))

print(summary_stats)


#Check for any missing values present or not 
sapply(data, function(x) sum(is.na(x)))

#Univariate Analysis
#Histogram plot of population 
ggplot(data, aes(x=Population))+
  geom_histogram(bins=8, fill="lightblue", color="black")+
  labs(title="Histogram Plot of Population", x="Population (thousands)", y="Count")

#Histogram plot of GNP
ggplot(data, aes(x=GNP))+
  geom_histogram(bins=8, fill="lightblue", color="black")+
  labs(title="Histogram Plot of GNP", x="GNP (millions)", y="Count")

#Bivariate Analysis
#Scatter Plot of Population vs. Year
ggplot(data, aes(x=Year, y=Population))+
  geom_point(color="blue")+
  labs(title="Population vs. Year", x="Population", y="Year")

#creating a variable that calculates the percentage of unemployed people
data$UnemploymentRate <- data$Unemployed / data$Population

#Bivariate Analysis 
#Scatter Plot of Unemployment Rate vs. Population 
ggplot(data, aes(x=Population, y=UnemploymentRate)) +
  geom_point(color="blue")+
  labs(title="Unemployment Rate vs. Population", x="Population (thousand)", y="Unemployment (rate/%)")

#creating a variable that calculates the percentage of armed forces
data$Armed.ForcesRate <- data$Armed.Forces / data$Population

#Bivariate Analysis 
#Looking at the relationship between Population vs. Armed Forces and Year vs. Armed Forces 
ggplot(data, aes(x=Population, y=Armed.ForcesRate))+
  geom_point(color="blue")+
  labs(title="Armed Forces Rate vs. Population", x="Population (thousand)", y="Armed Forces (rate/%)")
ggplot(data, aes(x=Year, y=Armed.ForcesRate))+
  geom_point(color="blue")+
  labs(title="Armed Forces Rate vs. Year", x="Year", y="Armed Forces (rate/%)")

#Scatter Plot of GNP vs. Year 
ggplot(data, aes(x=Year, y=GNP))+
  geom_point(color="blue")+
  labs(title="GNP vs. Year", x="Year", y="GNP (millions)")

#Correlation Analysis
cor_matrix <- cor(data %>% select_if(is.numeric))
print(cor_matrix)
as.table(cor_matrix)
as.data.frame(as.table(cor_matrix))
ggplot(data=as.data.frame(as.table(cor_matrix)), aes(Var1, Var2, fill=Freq))+
  geom_tile(color="white")+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, limit=c(-1,1))+
  geom_text(aes(label=round(Freq,2)), color="black", size=3)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
