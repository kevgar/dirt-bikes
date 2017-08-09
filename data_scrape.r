library(magrittr)
require(stringr)

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
rows_list[[4]]   # listing 3
                 # ...
rows_list[[725]] # listing 724