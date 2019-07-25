library(rvest)
library(scales)
base_url <- "http://www.hostingwijzer.nl/kieswijzer/providers/hostnet/reviews/?page="

first_page <- "1"

page <- read_html(paste(base_url, first_page, sep=""))

total_pages <- html_nodes(page, ".index.pull-left") %>%
  html_text() %>%
  gsub("\\d* - \\d* van ", "", .) %>%
  gsub(" .*", "", .) %>%
  as.numeric()

# Close the connection.
rm(page)

per_page <- 15

# Always round up because there is no such thing as half a page.
pages_to_fetch <- 1:ceiling(total_pages / per_page)

all_reviews <- data.frame(Date=c(), Package=c(), Review=c())

for(i in pages_to_fetch) {
  page <- read_html(paste(base_url, i, sep=""))

  date <- html_nodes(page, ".providerreview .date") %>% html_text()

  review <- html_nodes(page, ".providerreview p") %>% html_text()
  reviewer <- html_nodes(page, '.providerreview span[itemprop="author"]') %>% 
    html_text() %>%
    tolower()
  
  score  <- html_nodes(page, ".providerreview .dl-horizontal span") %>%
    html_text() %>%
    gsub(",", ".", .) %>%
    as.numeric()
  
  support_elements <- seq(1, length(score), by = 4)
  support_score <-score[support_elements]

  quality_elements <- seq(2, length(score), by = 4)
  quality_score <- score[quality_elements]

  price_elements <- seq(3, length(score), by = 4)
  price_score <- score[price_elements]

  general_elements <- seq(4, length(score), by=4)
  general_score <- score[general_elements]

  page_reviews <- data.frame(
    Date=date,
    Review=review,
    Reviewer=reviewer,
    SupportScore=support_score,
    QualityScore=quality_score,
    PriceScore=price_score,
    GeneralScore=general_score
  )

  all_reviews <- rbind(all_reviews, page_reviews)
  # Removing the variable also properly closes the connection.
  rm(page)
}
all_reviews$SupportScore <- rescale(all_reviews$SupportScore, to=c(0,100))
all_reviews$QualityScore <- rescale(all_reviews$QualityScore, to=c(0,100))
all_reviews$PriceScore <- rescale(all_reviews$PriceScore, to=c(0,100))
all_reviews$GeneralScore <- rescale(all_reviews$GeneralScore, to=c(0,100))
all_reviews %>% mutate(AverageScore = (SupportScore + QualityScore + PriceScore + GeneralScore) / 4)

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

all_reviews$Origin <- 'HostingWijzer'
