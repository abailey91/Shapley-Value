



# generate a fake exposure path data set

GenerateSimulatedExposurePaths <- function(num_rows,unique_users,channel_names){

set.seed(265)



exposure_data <- data.frame(User.ID = sample(c(1:unique_users), num_rows, replace = TRUE),
                  Date = sample(c(1:30), num_rows, replace = TRUE),
                  Channel = sample(c(1:length(channel_names)), num_rows, replace = TRUE,
                                   prob = c(0.1, 0.2, 0.05, 0.25, 0.07, 0.33)))                  




exposure_data$Date <- as.Date(exposure_data$Date, origin = "2016-01-01")
exposure_data$Channel <- channel_names[exposure_data$Channel]


last_event_channel <- lapply(unique(exposure_data$User.ID),function(x){
  user_data <- filter(exposure_data,User.ID==x)
  
  user_data[user_data$Date==max(user_data$Date),]$Channel[1]
})

last_event_channel <- do.call("rbind",last_event_channel)

#convert raw data to modelling format

#generate binary variables for each channel
binary_vars <- as.data.frame(model.matrix(~Channel-1,data=exposure_data))
colnames(binary_vars) <- gsub("Channel","",colnames(binary_vars))


#bind User ID to dummy variables and delete
modelling_data <- cbind(User.ID=exposure_data$User.ID,binary_vars)
rm(binary_vars)

#create a path data set for use in shapley calculation
paths <- modelling_data %>% group_by(User.ID) %>% summarise_each(funs(sum))
paths <- as.matrix(paths[,-1]/paths[,-1])
paths[is.nan(paths)] <-0
paths <-as.data.frame(paths)

#add user ID and create a conversion data column
modelling_data <- cbind(
  summarise(group_by(modelling_data,User.ID)),
  paths,
  Converted = sample(c(0:1),nrow(paths),replace = TRUE,
                     prob = c(0.88,0.12)))

#look at ways of making time stamp data
Conversion_Date <- dplyr::if_else(modelling_data$Converted==1,as.character(as.Date(sample(1:32,1), origin = "2016-01-01")),"NA")
Conversion_Date[Conversion_Date=="NA"]<-NA

modelling_data_colnames <- colnames(modelling_data)[-1]

modelling_data <- cbind(modelling_data,Conversion_Date=as.Date(Conversion_Date),Last_Event_Channel=last_event_channel)

modelling_data <- modelling_data[,c(colnames(modelling_data)[1],"Conversion_Date","Last_Event_Channel",modelling_data_colnames)]
#remove paths variable
rm(paths)

return(modelling_data)
}

