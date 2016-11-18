library(dplyr)
library(tidyr)


#function to compute conditional probabilities for each channel 
ComputeSumProbabilities <- function(initialChannel,conversionRates){
  rows_with_channel <- grepl(initialChannel,rownames(conversionRates),fixed=T)
  
  useable_rows <- as.data.frame(conversionRates[rows_with_channel,])
  rownames(useable_rows) <- rownames(conversionRates)[rows_with_channel]
  
  #loop through each channel taking combined probability minus individual probability of each channel in isolation
  probabilities <- lapply(rownames(useable_rows)[-1],function(x){
    
    channels_used <- strsplit(x,"^",fixed = T)
    
    conversionRates[x,]-sum(conversionRates[unlist(channels_used),])
  })
  
  sum(do.call("rbind",probabilities))
}

###############################################################

#load in a data set
test_data <- read.csv("test_data.csv",header=T,sep=",") 

#initial a conversion rate data frame
final_rates <- data.frame()

#loop through all available variables
for(i in 1:ncol(test_data[,-c(1,ncol(test_data))])){

  #calculate potential combinations for each level i
  combinations <- combn(colnames(test_data[,-c(1,ncol(test_data))]),i)
  
  #generate filters to split the data based on unique combinations
  dots <- apply(combinations,2,function(x){
    
    temp_names <- colnames(test_data)[-c(1,ncol(test_data))]
    
    missing_vars <- temp_names[!(temp_names %in% x)] 
    
    if(length(missing_vars)>0)
      missing_vars <- paste0(paste0(missing_vars,collapse = "==0 &"),"==0")
    
    selected_vars <- paste0(paste0(x,collapse = "==1&"),"==1")
    
    if(substr(selected_vars,1,nchar(selected_vars))=="&")
      selected_vars <- selected_vars[-nchar(selected_vars)]
    
    if(length(missing_vars)>0)
    paste0(selected_vars,"&",missing_vars)
    else
      selected_vars
  })
  
  #calculate conversion rates of each path type
  conversion_rates <- do.call("rbind",lapply(dots,function(x){
    filtered_data <- filter_(test_data,x)
    
    nrow(filter(filtered_data,Converted==1))/nrow(filtered_data)
    
  }))
  
  
  #change rownames to unique combinations of channels
  rownames(conversion_rates) <- apply(combinations,2,function(x){
    paste0(x,collapse = "^")
  })
  
  #store all the conversion rates with names
  final_rates <- rbind(final_rates,conversion_rates)
  
}

#rename column on conversion rates
colnames(final_rates) <- "probability"

#store names of individual channels
channel_names <- colnames(test_data)[-c(1,ncol(test_data))]

#compute contributions
#white paper - http://eprints.soton.ac.uk/380534/1/GHLEFMG_FGMJHM_VJ1QM9QF.pdf - formula on page 3

contributions <- lapply(channel_names,function(x){
    final_rates[x,] + (1/(2*(length(channel_names)-1))) * ComputeSumProbabilities(x,final_rates)
})

#bind to a data frame
contributions <- do.call("rbind",contributions)

#calculate weights to apply based on contribution data frame
final_contributions <- contributions/sum(contributions)




