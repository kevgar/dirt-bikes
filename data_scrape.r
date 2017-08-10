library(tidyverse)

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

rows_list[[1]] <- rows_list[[726]] <- NULL # invalid rows


rows_list_cleaned <- lapply(X=rows_list, FUN=function(i){
    
    # browser() # use for debugging
    
    data.frame(
        Bike = i %>% 
            # Get the first column
            str_split(.,'</td>') %>% .[[1]] %>% .[1] %>% 
            str_replace(".*\">", "") %>% 
            str_replace("</a>",""),
        
        Description = i %>% 
            # Get the second column
            str_split(.,'</td>') %>% .[[1]] %>% .[2] %>% 
            str_replace(".*\">", "") %>%
            str_split("</a>") %>% .[[1]] %>% .[1],
        
        # Description = i %>% 
        #     # Get the second column (Full Description)
        #     str_split(.,'</td>') %>% .[[1]] %>% .[2] %>%
        #     str_replace(".*\">", "") %>%
        #     str_split("</a>") %>% .[[1]] %>% .[2] %>%
        #     str_replace("<span>","") %>%
        #     str_replace("</span>",""),
        
        Price = i %>% 
            # get the third column
            str_split(.,'</td>') %>% .[[1]] %>% .[3] %>%
            str_replace("<td>",""),
        
        Year = i %>%
            # get the 4th column
            str_split(.,'</td>') %>% .[[1]] %>% .[4] %>%
            str_replace("<td>",""),
        
        Location = i %>% 
            # get the 5th column
            str_split(.,'</td>') %>% .[[1]] %>% .[5] %>% 
            str_replace("<td>",""),
        
        State = i %>% 
            # get the 6th column
            str_split(.,'</td>') %>% .[[1]] %>% .[6] %>% 
            str_replace(".*\">", "") %>% 
            str_replace("</a>", ""),
        
        Listed = i %>%
            # get the 7th column
            str_split(.,'</td>') %>% .[[1]] %>% .[7] %>% 
            str_replace(".*\">", ""),
        
        Source = i %>% 
            # get the 8th column
            str_split(.,'</td>') %>% .[[1]] %>% .[8] %>% 
            str_replace("<td>", "")
        
        )
    
    })

# rbind list into single data frame
df <- rows_list_cleaned %>% do.call("rbind",.)

# Frequency table of Bike
table(df$Bike)[order(-table(df$Bike))]
# Frequency table of Year
table(df$Year)[order(-table(df$Year))]
# Frequency table of State
table(df$State)[order(-table(df$State))]
# Frequency table of Source
table(df$Source)[order(-table(df$Source))]

# Write cleaned data frame to csv
write.table(df, file="2-stroke_dirt-bikes_cleaned.csv", row.names = FALSE, col.names = TRUE, sep=",")
