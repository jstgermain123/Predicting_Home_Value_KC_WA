install.packages("leaps")
install.packages("MASS")
install.packages("randomForest")
install.packages("caTools")
library(tidyverse)
library(corrplot)
library(ggplot2)
library(leaps)
library(MASS)
library(caret)
library(randomForest)
require(caTools)


#load data
data <- read.csv(file.choose(), sep=",", header = TRUE )
d_Forest <- data
# Set Correct Data Types
data$id <- as.character(data$id)
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

# Create Number of Rooms Variable
data['num_Rooms'] <- data$bedrooms + data$bathrooms
plot(data$id,data$num_Rooms)

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

##### Modeling Code Week 2###########

# Model Creation

model <- lm(price ~ . - id - num_Rooms, data = data)
summary(model)
alias(model)
model$coefficients
plot(residuals(model))
abline(0,0,col="blue")
plot(model)
plot(predict(model),data$price,
     xlab="predicted",ylab="actual", col="light gray")
abline(a=0,b=1,col="blue")

mse <- mean(model$residuals^2) #mean squared error = 9297484160.66227 or RMSE of 96423.46
aic <- AIC(model) #AIC - 557499.480819643

# Model Optimized 1 - Feature Engineering 2 Variables 

d_original <- data
# Use latitude and longitude to determine distance to Seattle
# 47.608013, -122.335167 lat and long for Seattle
d_original['distance_to_Seattle'] <- abs(d_original$lat - 47.608013) + abs(d_original$long - -122.335167)


# Create basement variable
d_original['basement'] <- ""
d_original$basement[data$sqft_basement == 0 ] <- 0
d_original$basement[data$sqft_basement != 0 ] <- 1

# Create Season Variable
d_original['season'] <- ""
d_original$season[(as.numeric(substr(d_original$date,6,7)) >= 9) & (as.numeric(substr(d_original$date,6,7)) <= 11)] <- "Fall"
d_original$season[(as.numeric(substr(d_original$date,6,7)) == 12) | (as.numeric(substr(d_original$date,6,7)) == 1) | (as.numeric(substr(d_original$date,6,7)) == 2)] <- "Winter"
d_original$season[(as.numeric(substr(d_original$date,6,7)) >= 6) & (as.numeric(substr(d_original$date,6,7)) <= 8)] <- "Summer"
d_original$season[(as.numeric(substr(d_original$date,6,7)) >= 3) & (as.numeric(substr(d_original$date,6,7)) <= 5)] <- "Spring"

# Modify year renovate column
# Create Renovated Column that is a 0 for unrenovated and 1 for renovated

d_original['Renovated'] <- ""
number_rows <- NROW(data)
list_1 <- c(1:number_rows)

for (k in list_1){
  if (d_original$yr_renovated[k] == 0){
    d_original$Renovated[k] <- 0
  }
  if (d_original$yr_renovated[k] != "0"){
    d_original$Renovated[k] <- 1
  }
}
d_original$Renovated <- as.factor(d_original$Renovated)

# Make renovation year entries that are 0 the year the house was built
for (k in list_1){
  if (d_original$yr_renovated[k] == 0){
    d_original$yr_renovated[k] <- d_original$yr_built[k]
  }
}

d_original$basement<- as.factor(d_original$basement)

# Grade reclassification
d_original$construction[as.numeric(d_original$grade) > 7] <- "Above Average"
d_original$construction[as.numeric(d_original$grade) == 7]  <- "Average"
d_original$construction[as.numeric(d_original$grade) < 7] <- "Below Average"
#d_original <- d_original[,-12]

# Check Model

model2 <- lm(price ~ . - id, data = d_original)
summary(model2)

mse2<- mean(model2$residuals^2) #mean squared error
aic2<- AIC(model2)
# mse = 9144147281.25
# RMSE = 95,625.03

# Model Optimized 2 - Feature Engineering + Feature Selection

# Fit the full model 
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(price ~ . - id, data = d_original,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:109),
                    trControl = train.control
)
#RMSE = 96,246.61 # 109

# Fit the full model 
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model2 <- train(price ~ . - id, data = d_original,
                     method = "leapForward", 
                     tuneGrid = data.frame(nvmax = 1:109),
                     trControl = train.control
)
step.model2$results
step.model2$bestTune
#RMSE = 96,235.01 #109



# Model Optimized 3 - Feature Selection
# Fit the full model 
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model3 <- train(price ~ . - id, data = data,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:104),
                    trControl = train.control
)
step.model3$results
step.model3$bestTune
# RMSE = 96,942.68 #99

set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model4 <- train(price ~ . - id, data = data,
                     method = "leapForward", 
                     tuneGrid = data.frame(nvmax = 1:104),
                     trControl = train.control
)
step.model4$results
step.model4$bestTune
# RMSE = 96,944.31 #102


# Model Optimized 4 - Random Forrest

set.seed(123)

# default RF model
rf.matrix = randomForest(y = d_original[,3], x = d_original[, c(2,4:27)], ntree = 2000, sampsize=10000)
m1<- rf.matrix
# number of trees with lowest MSE
which.min(m1$mse)

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])
# 84,738.43

m1$importance
AIC(m1)
