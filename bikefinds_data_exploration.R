# "Exploration of two-stroke dirt-bike listings"
# "Kevin Garder"
# "8/10/2017"

library(plotly)
library(dplyr)


# Load cleaned data
df <- read.csv("2-stroke_dirt-bikes_cleaned.csv")


# Number of listings by Model
table(df$Bike)[order(-table(df$Bike))]
# Number of listings by Year
table(df$Year)[order(-table(df$Year))]
# Number of listings by State
table(df$State)[order(-table(df$State))]
# Number of listings by Source
table(df$Source)[order(-table(df$Source))]


# Histogram of Asking price (All two-strokes)
hist(df$Price,
     xlab = "Price (USD)",
     main = "Histogram of Asking Price")

# Histogram of Asking price (YZ125 and YZ250)
x1 <- df$Price[df$Bike=="YZ125" & df$Year >= 2005 & df$Year <= 2016]
x2 <- df$Price[df$Bike=="YZ250" & df$Year >= 2005 & df$Year <= 2016]

par(mfrow=c(1,2))
hist(x1, 
     xlab = "Price (USD)",
     main = "Asking price for YZ125\n2005-2016 models")
hist(x2, 
     xlab = "Price (USD)",
     main = "Asking price for YZ250\n2005-2016 models")

# Add column Freq for frequency of occurance
frequencyTable <- data.frame(table(df$Bike))
names(frequencyTable)[1] <- "Bike"

df <- left_join(df,frequencyTable)

df_subset <- df[df$Freq >= 10,]

plot_ly(df_subset, 
        x     = ~Year, 
        y     = ~Price, 
        type  = 'scatter', 
        mode  = 'markers',
        text  = ~paste('Location: ',paste(Location,State,sep=", ")), 
        color = ~as.factor(Bike))