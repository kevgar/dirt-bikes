# "Exploration of two-stroke dirt-bike listings"
# "Kevin Garder"
# "8/10/2017"
options(stringsAsFactors = F)
library(plotly)
library(dplyr)
library(stringr)
library(data.table)


# Load cleaned data
df <- data.table(read.csv("two-strokes_cleaned.csv"))

usd2num <- function(x) as.integer(
    str_replace(sub('\\$','',sub(',','',x))," \\(auction\\)","")
    )



df[, Price:=usd2num(Price)]


# # Number of listings by Model
# table(df$Bike)[order(-table(df$Bike))]
# # Number of listings by Year
# table(df$Year)[order(-table(df$Year))]
# # Number of listings by State
# table(df$State)[order(-table(df$State))]
# # Number of listings by Source
# table(df$Source)[order(-table(df$Source))]
# 
# 
# # Histogram of Asking price (All two-strokes)
# hist(df$Price,
#      xlab = "Price (USD)",
#      main = "Histogram of Asking Price")
# 
# # Histogram of Asking price (YZ125 and YZ250)
# x1 <- df$Price[df$Bike=="YZ125" & df$Year >= 2005 & df$Year <= 2016]
# x2 <- df$Price[df$Bike=="YZ250" & df$Year >= 2005 & df$Year <= 2016]
# 
# par(mfrow=c(1,2))
# hist(x1, 
#      xlab = "Price (USD)",
#      main = "Asking price for YZ125\n2005-2016 models")
# hist(x2, 
#      xlab = "Price (USD)",
#      main = "Asking price for YZ250\n2005-2016 models")

# Add column Freq for frequency of occurance
frequencyTable <- data.table(table(df$Bike))
names(frequencyTable)[1] <- "Bike"

df <- data.table(left_join(df,frequencyTable))

df_subset <- df[N >= 10 &
                    !is.na(Year),]

# fit <- lm(Price ~ Year, data = df_subset)

plot_ly(df_subset, 
        x     = ~Year, 
        y     = ~Price, 
        type  = 'scatter', 
        mode  = 'markers',
        text  = ~paste('Location: ',paste(Location,State,sep=", ")), 
        color = ~as.factor(Bike))
   
library(plotly)
library(broom)

bikes <-  unique(df_subset$Bike)
colors <- c('(255, 0, 0', '(0, 255, 0', '(0, 0, 255')

p <- plot_ly()

for (i in 1:length(bikes)) {
    
    browser()
    
    p <- add_lines(p, data = df_subset[which(df_subset$Bike==bikes[[i]]), ],
                   y = fitted(lm(data = df_subset[which(df_subset$Bike==bikes[[i]]), ], Price ~ Year)),
                   x = ~Year,
                   line = list(color = paste('rgb', colors[[i]], ')')),
                   name = bikes[[i]])
    p <- add_ribbons(p, data = augment(lm(data = df_subset[which(df_subset$Bike==bikes[[i]]), ], Price ~ Year)),
                     y = ~Price,
                     x = ~Year,
                     ymin = ~.fitted - 1.96 * .se.fit,
                     ymax = ~.fitted + 1.96 * .se.fit,
                     line = list(color = paste('rgba', colors[[i]], ', 0.05)')), 
                     fillcolor = paste('rgba', colors[[i]], ', 0.1)'),
                     showlegend = FALSE)
    p <- add_markers(p, data = df_subset[which(df_subset$Bike==bikes[[i]]), ], 
                     x = ~Year, 
                     y = ~Price,
                     # symbol = ~bikes,
                     # symbol = bikes[[i]],
                     # symbol = ~Bike,
                     marker=list(color=paste('rgb', colors[[i]])))
}
p <- layout(p, xaxis = list(title = "Year"), yaxis = list(title = "Price"))

p



