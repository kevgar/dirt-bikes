library(tidyverse)

options(stringsAsFactors = FALSE)

setwd("~/GitHub/dirt-bikes")

# Print the first several lines of html
system("head -15 '2-stroke\ dirt-bikes\ HTML\ listings-table.txt'")

# character vector with one element
html_text <- scan(file="2-stroke dirt-bikes HTML listings-table.txt", what=character(), sep='\n') %>% 
    paste(.,sep="", collapse="") %>% 
    as.vector()

# create string for each table row 
rows <- html_text %>% strsplit(.,'</tr>')

# put the rows in a list
rows_list <- as.list(rows[1][[1]])

# remove string contents before the "<tr>"
rows_list <- lapply(X=rows_list,FUN=function(i) str_replace(i, ".*<tr>", "<tr>"))

# rows_list[[1]]   # header
# rows_list[[2]]   # listing 1
# rows_list[[3]]   # listing 2
# rows_list[[724]] # ...
# rows_list[[725]] # listing 724
# rows_list[[726]] # close table

# remove invalid rows
rows_list[[1]] <- rows_list[[length(rows_list)]] <- NULL 

cleanup <- function(my.list, i, j, k=1){
    my.list[[i]] %>% 
        # split string into columns
        str_split(.,'</td>') %>% .[[1]] %>% 
        # get jth column
        .[j] %>%
        # remove leading characters
        str_replace(".*\">", "") %>% 
        str_replace("<td>", "") %>%
        str_replace("<tr>", "") %>%
        # remove trailing characters
        str_replace("</a>","") %>%
        # get the kth element (1 for Summary, 2 for Full)
        str_split("<span>") %>% .[[1]] %>% .[k] %>%
        # remove trailing characters
        str_split(" BACKNEXT") %>% .[[1]] %>% .[1] %>%
        str_replace("</span>","") %>% 
        str_replace(" \\(auction\\)","")
    
}

# cleanup(rows_list, 1, 1)
# cleanup(rows_list, 1, 2, 1)
# cleanup(rows_list, 1, 2, 2)

get.url <- function(my.list, i, j){
    my.list[[i]] %>% 
        # split string into columns
        str_split(.,'</td>') %>% .[[1]] %>% 
        # get jth column
        .[j] %>% 
        # remove leading characters
        str_replace(".*a id=\"", "") %>% 
        # remove trailing characters
        str_split('\"') %>% .[[1]] %>% .[1] %>% 
        # append prefix
        paste0("http://www.bikefinds.com/for-sale/", .)
}

usd2num <- function(x) as.numeric(sub('\\$','',sub(',','',x)))

# get.url(rows_list, 1, 2)

rows_list_cleaned <- lapply(X=1:length(rows_list), FUN=function(i){
    
    # browser() # use for debugging
    
    data.frame(
        Bike         = cleanup(rows_list, i, 1),
        Description1 = cleanup(rows_list, i, 2, 1),
        Description2 = cleanup(rows_list, i, 2, 2),
        Price        = usd2num(cleanup(rows_list, i, 3)),
        Year         = cleanup(rows_list, i, 4),
        Location     = cleanup(rows_list, i, 5),
        State        = cleanup(rows_list, i, 6),
        Listed       = cleanup(rows_list, i, 7),
        Source       = cleanup(rows_list, i, 8),
        url          = get.url(rows_list, i, 2))
    })

# rbind list into single data frame
df <- do.call("rbind",rows_list_cleaned)

df$Price

df$Price %>% sub('\\$','',.) %>% sub(',','',.) %>% as.numeric()




# df[df$url=="http://www.bikefinds.com/for-sale/e152654787680", "Description2"]

# lapply(df, class)
# df <- lapply(df, as.character)

# # Frequency table of Bike
# table(df$Bike)[order(-table(df$Bike))]
# # Frequency table of Year
# table(df$Year)[order(-table(df$Year))]
# # Frequency table of State
# table(df$State)[order(-table(df$State))]
# # Frequency table of Source
# table(df$Source)[order(-table(df$Source))]

hist(df$Price[df$Bike=="YZ125" & 
                  as.numeric(df$Year) >= 2005 & 
                  as.numeric(df$Year) <= 2016], 
     xlab = "Price (USD)",
     main = "Asking price for YZ125\n2005-2016 models")

hist(df$Price[df$Bike=="YZ250" & 
                  as.numeric(df$Year) >= 2005 & 
                  as.numeric(df$Year) <= 2016], 
     xlab = "Price (USD)",
     main = "Asking price for YZ250\n2005-2016 models")

# Write cleaned data frame to csv
write_csv(x=df, path="2-stroke_dirt-bikes_cleaned.csv")
