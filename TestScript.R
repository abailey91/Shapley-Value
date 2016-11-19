library(dplyr)
library(tidyr)
library(ggplot2)

source("CalculateShapley.R")
source("SimulationData.R")

#load in a data set
#test_data <- read.csv("test_data.csv",header=T,sep=",") 


test_data <- GenerateSimulatedExposurePaths(
  num_rows = 500000,
  unique_users = 20000,
  channel_names = c("Paid.Social","Paid.Search","Affiliates","Display","Direct","Natural.Search"))

#compute the shapley value contributions in % form
model.fit <- CalculateShapley(test_data) 

#calculate conversions


#plot weights
weights <- data.frame(Weights=model.fit$Weights,Channels=rownames(model.fit$Weights))
ggplot(data=weights,aes(x=Channels,y=Weights,fill=Channels)) + 
  geom_bar(stat="identity") + 
  labs(title="Attribution Weights",x = "Channel", y="Weight")

#plot conversions - bar and pie charts
attr_conversions <- data.frame(Conversions=model.fit$attributedConversions,Channels=names(model.fit$attributedConversions))
ggplot(data = attr_conversions,aes(x=Channels,y=Conversions,fill=Channels)) + 
  geom_bar(stat="identity") +
  labs(title="Attributed Conversions",x="Channel",y="Conversions")

ggplot(data = attr_conversions,aes(x="",y=Conversions,fill=Channels)) + 
    geom_bar(stat="identity") +
  coord_polar(theta = "y") +
  labs(title="Attributed Conversions",x=NULL,y="Conversions")
  
  