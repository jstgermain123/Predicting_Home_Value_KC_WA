#load data
data <- read.csv(file.choose(), sep=",", header = TRUE )

# Set Correct Data Types
data$id <- as.character(data$id)
data$yr_renovated <- as.character(data$yr_renovated)
data$yr_built <- as.character(data$yr_built)
data$zipcode <- as.character(data$zipcode)

# Get Summary Stats
summary(data)

# Find NAs
new_DF <- data[rowSums(is.na(data)) > 0,]
# No NAs present

# Classify Houses based on livable square footage to aid in further analysis
data['sqft_cat'] <- ""
data$sqft_cat[data$sqft_living <= 1490] <- "Small"
data$sqft_cat[(data$sqft_living > 1490) & (data$sqft_living <= 2360)] <- "Average"
data$sqft_cat[data$sqft_living  > 2360] <- "Large"

# Plot Box Plots and scatter plots

# Distribution of Price
boxplot(data$price ~ data$grade) # boxplot by overall grade of house
boxplot(data$price ~ data$sqft_cat) # boxplot by sqft category

# Clean Date
data$date = substr(data$date,1,8)
data$date <- as.Date(data$date,format = "%Y%m%d")

# Clean Price
plot(data$id,data$price) # by listing
mean_price <- mean(data$price)
std_price <- sd(data$price)
two_sd_price <- (2*std_price) + mean_price
data$price[data$price > two_sd_price] <- two_sd_price
plot(data$id,data$price) # by listing

# Clean Bedrooms
plot(data$id,data$bedrooms) # by listing
mean_bedrooms <- mean(data$bedrooms)
std_bedrooms <- sd(data$bedrooms)
two_sd_bedrooms <- (2*std_bedrooms) + mean_bedrooms
data$bedrooms[data$bedrooms > two_sd_bedrooms] <- two_sd_bedrooms
plot(data$id,data$bedrooms) # by listing

# Clean Bathrooms
plot(data$id,data$bathrooms) # by listing
mean_bathrooms <- mean(data$bathrooms)
std_bathrooms <- sd(data$bathrooms)
two_sd_bathrooms <- (2*std_bathrooms) + mean_bathrooms
data$bathrooms[data$bathrooms > two_sd_bathrooms] <- two_sd_bathrooms
plot(data$id,data$bathrooms) # by listing

# Clean Sqft Living


# Clean Sqft Loft
# Clean Sqft Above
# Clean Sqft Basement
# Clean Year Rennovated
# Clean Sqft Living 15
# Clean Sqft Loft 15

# Code for correlation plot
cordata <- data[,c(3:14,19,20)] #correlation plot (numerical only)
res <- cor(cordata) 
round(res, 2)





# Code that cane be used to replace N/A's
# use for variables as needed
data$Deposit.Amount[(is.na(data$Completed) == TRUE)] <- 100

# Code for correlation plot
cordata <- data[,c(4,6,11)] # use this to select columns you need for correlation plot (numerical only)
res <- cor(cordata) 
round(res, 2)