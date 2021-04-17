#load data
data <- read.csv(file.choose(), sep=",", header = TRUE )

# Code that cane be used to replace N/A's
# use for variables as needed
data$Deposit.Amount[(is.na(data$Completed) == TRUE)] <- 100

# Code for correlation plot
cordata <- data[,c(4,6,11)] # use this to select columns you need for correlation plot (numerical onlu)
res <- cor(cordata) 
round(res, 2)