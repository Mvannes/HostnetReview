library(rvest)
library(stringr)
library(dplyr)
library(scales)

# If this limit ever stops working, scrape differently. Also, well done Hostnet on that many reviews.
url <- "https://www.kiyoh.com/reviews/1047322/hostnet?limit=1000000"

star_count <- 5
page <- read_html(url)

date <- html_nodes(page, ".review .meta-data .created-date") %>% html_attr('data-date') %>% as.Date()
review <- html_nodes(page, ".review .rating-description") %>% html_text() %>% trimws()

reviewer <- html_nodes(page, '.review .meta-data .name-city') %>%
  html_text() %>%
  tolower() %>% 
  str_match('.+( ,)?')
reviewer <- reviewer[,1]

score <- html_nodes(page, ".review .review-content .left .overall-rating-stars .rating-stars") 

agg <- html_nodes(score, 'span') %>% 
  html_attr(., "class") %>%
  data.frame(Stars=.)
agg$Group <- rep(1:(nrow(agg)/star_count), each=star_count)
agg$StarAsNumber <- -1
if(nrow(agg[agg$Stars == "icon-star", ]) > 0) {
  agg[agg$Stars == "icon-star", ]$StarAsNumber <- 1
}
if(nrow(agg[agg$Stars == "icon-star-half-full", ]) > 0) {
  agg[agg$Stars == "icon-star-half-full", ]$StarAsNumber <- 0.5
}
if(nrow(agg[agg$Stars == "icon-star-o", ]) > 0) {
  agg[agg$Stars == "icon-star-o", ]$StarAsNumber <- 0
}

score_values_stars <- aggregate(StarAsNumber ~ Group, agg, sum)$StarAsNumber

score <- html_nodes(page, ".review .rating-number") %>% 
  html_text() %>% 
  str_match('\\d+') %>% 
  as.integer()

all_reviews <- data.frame(
  Date=date,
  Review=review,
  Reviewer=reviewer,
  AverageScoreStars=score_values_stars,
  AverageScore=score
)

# Removing the variable also properly closes the connection.
rm(page)

all_reviews$AverageScore <- rescale(all_reviews$AverageScore, to=c(0,100))
all_reviews$AverageScoreStars <- rescale(all_reviews$AverageScoreStars, to=c(0,100))
all_reviews <- all_reviews %>% arrange(Date)
all_reviews$cumulativeAverageScore <- cummean(all_reviews$AverageScore)

all_reviews$Origin <- 'Kiyoh'
