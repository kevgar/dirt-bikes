# "Exploration of two-stroke dirt-bike listings"
# "Kevin Garder"
# "8/10/2017"
options(stringsAsFactors = F)
library(plotly)
library(dplyr)
library(stringr)
library(data.table)
library(broom)


# Load cleaned data
dt <- data.table(read.csv("two-strokes_cleaned.csv"))

usd2num <- function(x) as.integer(
    str_replace(sub('\\$','',sub(',','',x))," \\(auction\\)","")
    )
# convert currency to integer
dt[, Price:=usd2num(Price)]
# add column for bike count
dt[, Count:=.N, by=Bike]
# convert Bike column to factor
dt[, Bike:=as.factor(Bike)]

# Top 5 most common Bikes
# unique(dt$Bike[(order(-dt$Count))])[1:5]

# create subset for plotting
dt_subset <- dt[Count>=10 & 
             !is.na(Year) & 
             Bike %in% unique(Bike[(order(-Count))])[1:5],]

# # Number of listings by Model
# table(df$Bike)[order(-table(df$Bike))]
# # Number of listings by Year
# table(df$Year)[order(-table(df$Year))]
# # Number of listings by State
# table(df$State)[order(-table(df$State))]
# # Number of listings by Source
# table(df$Source)[order(-table(df$Source))]

# Fit a linear model
m <- lm(Price ~ Year*Bike, dt_subset)

# Plot the data and interactions
p <- broom::augment(m) %>%
    
    plot_ly(x = ~Year, showlegend = TRUE) %>%
    
    add_markers(y = ~Price,
                color = ~Bike,
                text = ~paste('Location: ',paste(dt_subset$Location,dt_subset$State,sep=", "))) %>%
    
    add_ribbons(ymin = ~.fitted - 1.96 * .se.fit,
                ymax = ~.fitted + 1.96 * .se.fit,
                color = ~Bike) %>%
    
    add_lines(y = ~.fitted, color = ~Bike) %>% 
    
    layout(title = 'Asking Price vs Year of Common Two-strokes')

p
# chart_link = api_create(p, filename="text/mode")
# chart_link


kerns <- c("gaussian", "epanechnikov", "rectangular", 
           "triangular", "biweight", "cosine", "optcosine")
p <- plot_ly()
for (k in kerns) {
    d <- density(dt_subset$Price, kernel = k, na.rm = TRUE)
    p <- add_lines(p, x = d$x, y = d$y, name = k)
}
layout(p, title = 'Asking Price vs Year of Common Two-strokes',xaxis = list(title = "Price"))


