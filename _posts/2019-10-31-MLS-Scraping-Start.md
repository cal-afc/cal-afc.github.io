---
layout: post
title: "The Start of the MLS Scraping Project"
date: 2019-10-31
---

 This is some insight into the start of my MLS scraping project. 
 
 This portion only contains information about the part of the project dealing with power rankings.
 
 This project started with an in class assignment, where I navigated to [MLS Soccer](https://www.mlssoccer.com) and looked at their power rankings news posts.
 
 Using the rvest and stringr libraries in R, I read the power rankings specific HTML.
 The next step was to dive through the nodes and then grab the text (which included the specific titles of each post) and links associated with that text.
 After grabbing the titles, all that needed to be done was to write a for loop to iterate through the links grabbed in the previous step.
 In this for loop, each iteration followed the link and pulled all the text from each individual power rankings post.
 
 All of this text was appended to a list, resulting in a large list with all of the power rankings text.
 
 This list was formatted and visualized using the R libraries tm, SnowballC, wordcloud, and RColorBrewer.
 
 All numbers, punctuation and stopwords (using the stopwords english dictionary) were removed, along with overly common words in the text itself. All text was pushed to lowercase. Extra white space was also removed.
 
 The text was converted to a document term matrix, with each term having a frequency next to it, allowing for easy analysis.
 
 The generated wordcloud from this analysis is included below:
 
 ![Image](https://github.com/cal-afc/cal-afc.github.io/blob/master/images/MLSwordcloud.png?raw=true)
 
 The next step in this project involves taking the text for specific teams, and creating individual files for specific teams, where I can use R's sentiment analysis packages in order to see which team has the most positive or negative news associated with it.
 Along with team specific text, I also plan to look at the text for MLS Soccer's analysts, and see if some have more negative writing styles than others.
 
 Code for this project is included below:

library(rvest)
library(stringr)

mlspowerrankings <- read_html("https://www.mlssoccer.com/series/power-rankings")

urls <- mlspowerrankings %>%
  html_node('div.ct_wrapper') %>%
  html_nodes('a') %>%
  html_text()

urls

mls_list <- c()

for (i in urls){
  mls_session <- html_session("https://www.mlssoccer.com/series/power-rankings")
  
  mls_link <- mls_session %>%
    follow_link(i)
  
  mls_text <- mls_link %>%
    html_nodes('div.power-rankings-container') %>%
    html_text()
  
  mls_list <- append(mls_list, mls_text)
  
  Sys.sleep(5)
}

mls_list

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

mls <- Corpus(VectorSource(mls_list))

inspect(mls)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
mls <- tm_map(mls, toSpace, "/")
mls <- tm_map(mls, toSpace, "@")
mls <- tm_map(mls, toSpace, "\\|")

inspect(mls)

# Convert the text to lower case
mls <- tm_map(mls, content_transformer(tolower))

# Remove numbers
mls <- tm_map(mls, removeNumbers)

# Remove english common stopwords
mls <- tm_map(mls, removeWords, stopwords("english"))

# Remove other stop words
# specify stopwords as a character vector
mls <- tm_map(mls, removeWords, c("week", "last", "next", "high", "low", "previous"))

# Remove punctuation
mls <- tm_map(mls, removePunctuation)

# Eliminate extra white spaces
mls <- tm_map(mls, stripWhitespace)

# Text stemming
# Takes words down to common roots
# mls <- tm_map(mls, stemDocument)

dtm <- TermDocumentMatrix(mls)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
mls_df <- data.frame(word = names(v), freq=v)

head(mls_df, 10)
inspect(mls)

set.seed(1)
wordcloud(words = mls_df$word, 
          freq = mls_df$freq, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2")
          )
