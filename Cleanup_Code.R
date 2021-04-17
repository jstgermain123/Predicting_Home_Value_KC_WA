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




# Code that cane be used to replace N/A's
# use for variables as needed
data$Deposit.Amount[(is.na(data$Completed) == TRUE)] <- 100

# Code for correlation plot
cordata <- data[,c(4,6,11)] # use this to select columns you need for correlation plot (numerical only)
res <- cor(cordata) 
round(res, 2)