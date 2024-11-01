library(jsonlite)
library(dplyr)

top_stories <- fromJSON("https://api.nytimes.com/svc/topstories/v2/us.json?api-key=Scz8UJ7pKtRfGDpP7Gvg27VMmCC74GlA", flatten = TRUE) %>% 
  data.frame()


head(top_stories)


library("selectr")
library("xml2")
library("rvest")

url <- "https://nytimes.com"
webpage <- read_html(url)

# Just sample code. 
titles <- html_nodes(webpage, "div h2")
without_tags <- gsub("<.*?>", "", titles) 
print(without_tags)

# Assignment 15 Task 1: write scripts that extract "titles" and "dates" of articles out of the scrapped data. 
# Then, print them out using "print()" statement. 



# Assignment 15 Task 2: write scripts that oraganize your data as dataframe with column names, "title" and "date", respectively.
# Then, save this dataframe as a CSV file. Name it as "NYT_titles.csv". 



# Assignment 15 Task 3: once you save the CSV file, commit and push it back to your repository (no R scripts involved for Task 3). 