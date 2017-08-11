library(rvest)
library(stringr)
library(data.table)
library(dplyr)
library(readr)


harvest_table <- function(Url) {
    # Download the table url
    read_html(Url) %>% 
        # Find the html table
        html_node("table") %>% 
        # Convert to dataframe
        html_table()
    }

get_links <- function(Url){
    id <- read_html(Url) %>%
        html_node("tbody") %>%
        html_nodes(xpath = 'tr') %>% 
        html_nodes(xpath = 'td') %>%
        html_nodes(xpath = 'a') %>%
        html_attr("id") %>%
        .[!is.na(.)]
    # Add prefix and create hyperlink
    paste0(paste0('[Link](http://www.bikefinds.com/for-sale/',id,')'))
    }

get_category <- function(Url){
    Url %>%
        str_replace(".*.com/", "") %>% 
        str_replace("-for-sale", "") %>% 
        str_replace("-", " ")
    }

categories <- harvest_table("http://www.bikefinds.com/bikes-for-sale")

mini <- data.table(harvest_table("http://www.bikefinds.com/mini-bikes-for-sale"))
mini[, Link:=get_links("http://www.bikefinds.com/mini-bikes-for-sale")]
mini[, Category:=get_category("http://www.bikefinds.com/mini-bikes-for-sale")]

motocross <- data.table(harvest_table("http://www.bikefinds.com/motocross-bikes-for-sale"))
motocross[, Link:=get_links("http://www.bikefinds.com/motocross-bikes-for-sale")]
motocross[, Category:=get_category("http://www.bikefinds.com/motocross-bikes-for-sale")]

trail <- data.table(harvest_table("http://www.bikefinds.com/trail-bikes-for-sale"))
trail[, Link:=get_links("http://www.bikefinds.com/trail-bikes-for-sale")]
trail[, Category:=get_category("http://www.bikefinds.com/trail-bikes-for-sale")]

enduro <- data.table(harvest_table("http://www.bikefinds.com/enduro-bikes-for-sale"))
enduro[, Link:=get_links("http://www.bikefinds.com/enduro-bikes-for-sale")]
enduro[, Category:=get_category("http://www.bikefinds.com/enduro-bikes-for-sale")]

dual <- data.table(harvest_table("http://www.bikefinds.com/dual-sport-for-sale"))
dual[, Link:=get_links("http://www.bikefinds.com/dual-sport-for-sale")]
dual[, Category:=get_category("http://www.bikefinds.com/dual-sport-for-sale")]

df <- do.call("rbind", list(mini, motocross, trail, enduro, dual))
df.dummies <- cbind(df,dummy(df[,"Category",drop=FALSE], int = TRUE))

cols <- names(df.dummies)[11:15]
df.sum <- df.dummies[, lapply(.SD, sum), by=Link, .SDcols=cols]

df_final <- left_join(df.sum, df[,-10]) %>% .[, c(7:14, 1:6)]
write_csv(x=df_final, path="dirt-bikes_cleaned.csv")

