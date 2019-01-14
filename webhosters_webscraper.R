library(rvest)
library(stringr)
library(dplyr)
library(scales)

base_url <- "https://www.webhosters.nl/hosting-bedrijven/hostnet/page/"

first_page <- "1"

page <- read_html(paste(base_url, first_page, sep=""))

total_pages <- html_nodes(page, "#read-experiences") %>%
  html_text() %>%
  str_match(., "\\d* pagina") %>%
  str_match(., '\\d*') %>% 
  as.numeric()

# Close the connection.
rm(page)

per_page <- 10
star_count <- 5

# Always round up because there is no such thing as half a page.
pages_to_fetch <- 1:ceiling(total_pages / per_page)

all_reviews <- data.frame(Date=c(), Package=c(), Review=c())

for(i in pages_to_fetch) {
  page <- read_html(paste(base_url, i, sep=""))
  
  date <- html_nodes(page, "#ervaringen .meta-date") %>% 
    html_text() %>% 
    trimws() %>% 
    str_match(., "\\d* [a-z]* \\d*")
  
  review <- html_nodes(page, "#ervaringen .widget-content .row .l-col-8") %>% html_text()
  
  reviewer <- html_nodes(page, '#ervaringen .comment-meta') %>%
    html_text() %>%
    tolower() %>% 
    gsub("[a-z]* \\d* [a-z]* \\d*", "", .) %>%
    trimws()
  
  score <- html_nodes(page, "#ervaringen .widget-content .star_rating_container .star_rating_group") 
  
  
  
  price_elements <- seq(1, length(score), by = 3)
  price_score <- score[price_elements]
  agg <- html_nodes(price_score, '.fa') %>% 
    html_attr(., "class") %>%
    data.frame(Stars=.)
  agg$Group <- rep(1:(nrow(agg)/star_count), each=star_count)
  agg$StarAsNumber <- -1
  agg[agg$Stars == "fa fa-star", ]$StarAsNumber <- 1
  agg[agg$Stars == "fa fa-star-half-o", ]$StarAsNumber <- 0.5
  agg[agg$Stars == "fa fa-star-o", ]$StarAsNumber <- 0
  price_score <- aggregate(StarAsNumber ~ Group, agg, sum)$StarAsNumber
  
  quality_elements <- seq(2, length(score), by = 3)
  quality_score <- score[quality_elements]
  agg <- html_nodes(quality_score, '.fa') %>% 
    html_attr(., "class") %>%
    data.frame(Stars=.)
  agg$Group <- rep(1:(nrow(agg)/star_count), each=star_count)
  agg$StarAsNumber <- -1
  agg[agg$Stars == "fa fa-star", ]$StarAsNumber <- 1
  agg[agg$Stars == "fa fa-star-half-o", ]$StarAsNumber <- 0.5
  agg[agg$Stars == "fa fa-star-o", ]$StarAsNumber <- 0
  quality_score <- aggregate(StarAsNumber ~ Group, agg, sum)$StarAsNumber
  
  support_elements <- seq(3, length(score), by = 3)
  support_score <-score[support_elements]
  agg <- html_nodes(support_score, '.fa') %>% 
    html_attr(., "class") %>%
    data.frame(Stars=.)
  agg$Group <- rep(1:(nrow(agg)/star_count), each=star_count)
  agg$StarAsNumber <- -1
  agg[agg$Stars == "fa fa-star", ]$StarAsNumber <- 1
  agg[agg$Stars == "fa fa-star-half-o", ]$StarAsNumber <- 0.5
  agg[agg$Stars == "fa fa-star-o", ]$StarAsNumber <- 0
  support_score <- aggregate(StarAsNumber ~ Group, agg, sum)$StarAsNumber

  page_reviews <- data.frame(
    Date=date,
    Review=review,
    SupportScore=rescale(support_score, to=c(0,100)),
    QualityScore=rescale(quality_score, to=c(0,100)),
    PriceScore=rescale(price_score, to=c(0,100))
  )
  page_reviews %>% mutate(GeneralScore = (SupportScore + QualityScore + PriceScore) / 3)
  
  all_reviews <- rbind(all_reviews, page_reviews)
  # Removing the variable also properly closes the connection.
  rm(page)
}

# move this and do it more nicely than current gsub mess.  
all_reviews$Date <- all_reviews$Date %>% 
  gsub(" januari ", "-01-", .) %>%
  gsub(" februari ", "-02-", .) %>%
  gsub(" maart ", "-03-", .) %>%
  gsub(" april ", "-04-", .) %>%
  gsub(" mei ", "-05-", .) %>%
  gsub(" juni ", "-06-", .) %>%
  gsub(" juli ", "-07-", .) %>%
  gsub(" augustus ", "-08-", .) %>%
  gsub(" september ", "-09-", .) %>%
  gsub(" oktober ", "-10-", .) %>%
  gsub(" november ", "-11-", .) %>%
  gsub(" december ", "-12-", .) %>%
  as.Date(format="%d-%m-%Y ")
