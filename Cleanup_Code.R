library(tidyverse)
library(corrplot)


#load data
data <- read.csv(file.choose(), sep=",", header = TRUE )
d2 <- data
# Set Correct Data Types
data$id <- as.character(data$id)
data$yr_renovated <- as.character(data$yr_renovated)
data$yr_built <- as.character(data$yr_built)
data$zipcode <- as.character(data$zipcode)
data$waterfront <- as.factor(data$waterfront)
data$view <- as.factor(data$view)
data$condition<- as.factor(data$condition)
data$grade<- as.factor(data$grade)



# Clean Date
data$date = substr(data$date,1,8)
data$date <- as.Date(data$date,format = "%Y%m%d")

# Get Summary Stats
summary(data)

# Find NAs
new_DF <- data[rowSums(is.na(data)) > 0,]
# No NAs present

####### Check Validity of Data Relationships #######

# Check to make sure sqft above is not greater than sqft living
new_DF <- data[data$sqft_above > data$sqft_living,]
data$sqft_above[data$sqft_above > data$sqft_living] <- -1

# Investigate Entries where sqft living is greater than the Sqft lot
# This could be possible if there are multiple floors and 
# a basement so check will take that into account
data$sqft_lot[data$sqft_living > ((data$sqft_lot * data$floors)+data$sqft_basement)] <- -1
# Due to the small number of entries meeting that condition deleted entries (4 entries)
data<- data[!data$sqft_lot == -1, ] # deleted entries


###### Outlier Detection and Replacement ##############

# Clean Price
plot(data$id,data$price) # by listing
mean_price <- mean(data$price)
std_price <- sd(data$price)
two_sd_price <- (2*std_price) + mean_price
two_sd_price_below <- (-2*std_price) + mean_price
data$price[data$price > two_sd_price] <- two_sd_price
data$price[data$price < two_sd_price_below] <- two_sd_price_below
plot(data$id,data$price) # by listing

# Clean Bedrooms
plot(data$id,data$bedrooms) # by listing
# Investigate entry that has 33 bedrooms 
# 1620 SQFT for 33 bedrooms seems impossible and only 1.75 baths
# Delete Entry
data<- data[!data$bedrooms == 33, ] 

#CONTINUE CLEANING
mean_bedrooms <- mean(data$bedrooms)
std_bedrooms <- sd(data$bedrooms)
two_sd_bedrooms <- (2*std_bedrooms) + mean_bedrooms
data$bedrooms[data$bedrooms > two_sd_bedrooms] <- two_sd_bedrooms
plot(data$id,data$bedrooms) # by listing

# Clean Bathrooms
plot(data$id,data$bathrooms) # by listing
# Some entries have no bathrooms
# That seems impossible - due to small number decided to delete entries
#data<- data[!data$bathrooms == 0, ] # deleted entries
# Continue Cleaning
mean_bathrooms <- mean(data$bathrooms)
std_bathrooms <- sd(data$bathrooms)
two_sd_bathrooms <- (2*std_bathrooms) + mean_bathrooms
data$bathrooms[data$bathrooms > two_sd_bathrooms] <- two_sd_bathrooms
plot(data$id,data$bathrooms) # by listing

# Clean Sqft Living
plot(data$id,data$sqft_living) # by listing
mean_sqftliving <- mean(data$sqft_living)
std_sqftliving  <- sd(data$sqft_living)
two_sd_sqftliving  <- (2*std_sqftliving) + mean_sqftliving 
two_sd_sqftliving_below <- (-2*std_sqftliving ) + mean_sqftliving 
data$sqft_living[data$sqft_living > two_sd_sqftliving ] <- two_sd_sqftliving 
data$sqft_living[data$sqft_living < two_sd_sqftliving_below] <- two_sd_sqftliving_below
plot(data$id,data$sqft_living) # by listing

# Clean Sqft Lot
plot(data$id,data$sqft_lot) # by listing
mean_sqft_lot<- mean(data$sqft_lot)
std_sqft_lot <- sd(data$sqft_lot)
two_sd_sqft_lot  <- (2*std_sqft_lot) + mean_sqft_lot
two_sd_sqft_lot_below <- (-2*std_sqft_lot ) + mean_sqft_lot
data$sqft_lot[data$sqft_lot > two_sd_sqft_lot ] <- two_sd_sqft_lot
data$sqft_lot[data$sqft_lot < two_sd_sqft_lot_below] <- two_sd_sqft_lot_below
plot(data$id,data$sqft_lot) # by listing

# Clean Sqft Above
plot(data$id,data$sqft_above) # by listing
mean_sqft_above<- mean(data$sqft_above)
std_sqft_above <- sd(data$sqft_above)
two_sd_sqft_above <- (2*std_sqft_above) + mean_sqft_above
two_sd_sqft_above_below <- (-2*std_sqft_above ) + mean_sqft_above
data$sqft_above[data$sqft_above > two_sd_sqft_above] <- two_sd_sqft_above
data$sqft_above[data$sqft_above < two_sd_sqft_above_below] <- two_sd_sqft_above_below
plot(data$id,data$sqft_above) # by listing

# Clean Sqft Basement
plot(data$id,data$sqft_basement) # by listing
mean_sqft_basement<- mean(data$sqft_basement)
std_sqft_basement<- sd(data$sqft_basement)
two_sd_sqft_basement <- (2*std_sqft_basement) + mean_sqft_basement
two_sd_sqft_basement_below <- (-2*std_sqft_basement) + mean_sqft_basement
data$sqft_basement[data$sqft_basement> two_sd_sqft_basement ] <- two_sd_sqft_basement
data$sqft_basement[data$sqft_basement < two_sd_sqft_basement_below] <- two_sd_sqft_basement_below
plot(data$id,data$sqft_basement) # by listing

# Clean Year Renovated
# Not sure what to do with 0 values here - do we keep in same format?

# Clean Sqft Living 15
plot(data$id,data$sqft_living15) # by listing
mean_sqft_living15<- mean(data$sqft_living15)
std_sqft_living15<- sd(data$sqft_living15)
two_sd_sqft_living15 <- (2*std_sqft_living15) + mean_sqft_living15
two_sd_sqft_living15_below <- (-2*std_sqft_living15) + mean_sqft_living15
data$sqft_living15[data$sqft_living15> two_sd_sqft_living15 ] <- two_sd_sqft_living15
data$sqft_living15[data$sqft_living15 < two_sd_sqft_living15_below] <- two_sd_sqft_living15_below
plot(data$id,data$sqft_living15) # by listing

# Clean Sqft Lot 15
plot(data$id,data$sqft_lot15) # by listing
mean_sqft_lot15<- mean(data$sqft_lot15)
std_sqft_lot15<- sd(data$sqft_lot15)
two_sd_sqft_lot15 <- (2*std_sqft_lot15) + mean_sqft_lot15
two_sd_sqft_lot15_below <- (-2*std_sqft_lot15) + mean_sqft_lot15
data$sqft_lot15[data$sqft_lot15 > two_sd_sqft_lot15] <- two_sd_sqft_lot15
plot(data$id,data$sqft_lot15) # by listing


##### Correlation Data #########

# Code for correlation plot
cordata <- data[,c(3:14,18:20)] #correlation plot (numerical only)
res <- cor(cordata) 
round(res, 2)

corrplot(res, method="color")


###### Plot Box Plots, Scatter Plots, Histograms #########

# Distribution of Price
# Based on matrix - Grade, Sqft living, and sqftliving15 have the strongest
# positive correlation with price - lets visualize these distributions

boxplot(data$price~data$grade) # boxplot by overall grade of house
hist(data$price) # histogram of price
hist(data$sqft_living15) # histogram of sqft_living15
hist(data$sqft_living) # histogram of sqft_living
hist(data$grade) # histogram of grade

#Identify Waterfront Homes

sp <- ggplot(data, aes(long, lat, colour = waterfront)) + 
  geom_point(shape =18)
sp + scale_color_manual(values=c("#999999", "blue"))

# Show relationship between price and location
ggplot(data, aes(long, lat, colour = price)) + 
  geom_point(shape =18)


# Scatter Plot Matrix (TOP Positive Correlating Variables)
pairs(data[,c(3,6,12,20)], pch = 1, lower.panel = NULL, col = "light blue")

write.csv(data, file="data.csv", row.names=FALSE)
### Archived Code Not Used ###

# Ten entries that have no bedrooms
# That seems impossible - due to small number decided to delete entries
# data<- data[!data$bedrooms == 0, ] # deleted entries

