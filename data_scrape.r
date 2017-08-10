library(tidyverse)
library(data.table)

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

rows_list[[1]]   # header
rows_list[[2]]   # listing 1
rows_list[[3]]   # listing 2
rows_list[[724]] # ...
rows_list[[725]] # listing 724
rows_list[[726]] # close table

rows_list[[1]] <- rows_list[[726]] <- NULL # remove invalid rows

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
        # split string into Summary and Full (for description column)
        str_split("<span>") %>% .[[1]] %>% 
        # get the kth element (1 for Summary, 2 for Full)
        .[k] %>% 
        # remove trailing characters
        str_replace("</span>","")
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

# get.url(rows_list, 1, 2)

rows_list_cleaned <- lapply(X=1:length(rows_list), FUN=function(i){
    
    # browser() # use for debugging
    
    data.frame(
        Bike         = cleanup(rows_list, i, 1),
        Description1 = cleanup(rows_list, i, 2, 1),
        Description2 = cleanup(rows_list, i, 2, 2),
        Price        = cleanup(rows_list, i, 3),
        Year         = cleanup(rows_list, i, 4),
        Location     = cleanup(rows_list, i, 5),
        State        = cleanup(rows_list, i, 6),
        Listed       = cleanup(rows_list, i, 7),
        Source       = cleanup(rows_list, i, 8),
        url          = get.url(rows_list, i, 2))
    })

# rbind list into single data frame
df <- rows_list_cleaned %>% do.call("rbind",.)

problem <- df[df$url=="http://www.bikefinds.com/for-sale/73dab565d760b69da5215106bbf2ec46","Description2"]
notproblem <- df[df$url=="http://www.bikefinds.com/for-sale/efc285764f1dfc3eeff3fb38009546c0","Description2"]
lapply(df, class)
# df <- lapply(df, as.character)


# # Frequency table of Bike
# table(df$Bike)[order(-table(df$Bike))]
# # Frequency table of Year
# table(df$Year)[order(-table(df$Year))]
# # Frequency table of State
# table(df$State)[order(-table(df$State))]
# # Frequency table of Source
# table(df$Source)[order(-table(df$Source))]

# Write cleaned data frame to csv
write.table(
    x       = df,
    file    = "2-stroke_dirt-bikes_cleaned.csv",
    # file    = stdout(),
    qmethod = "escape",
    quote   = TRUE,
    row.names = FALSE
    )

length(write.table(
    x       = problem,
    # file    = "2-stroke_dirt-bikes_cleaned.csv",
    file    = stdout(),
    qmethod = "escape",
    quote   = TRUE, 
    row.names = FALSE
))
write.table(
    x       = notproblem,
    # file    = "2-stroke_dirt-bikes_cleaned.csv",
    file    = stdout(),
    qmethod = "escape",
    quote   = TRUE, 
    row.names = FALSE
)


write.table(df, file="2-stroke_dirt-bikes_cleaned.csv", row.names = FALSE, col.names = TRUE, sep=",")


write_csv(x=df, path="2-stroke_dirt-bikes_cleaned.csv", 