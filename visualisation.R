# A basic template for quickly reading and inspecting csv data.
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(tm)
library(wordcloud2)
library(caret)
library(klaR)
library(lubridate)
library(reshape2)

source("utility.R")

all_reviews <- hosting_reviews_ip %>% select(Date, AverageScore, CumulativeAverageScore, Origin, CumulativeAmount, Review, Reviewer)

all_reviews <- rbind(all_reviews, select(hosting_reviews_hos, Date, AverageScore, CumulativeAverageScore, Origin, CumulativeAmount, Review, Reviewer))
all_reviews <- rbind(all_reviews, select(kiyoh_reviews, Date, AverageScore, CumulativeAverageScore, Origin, CumulativeAmount, Review, Reviewer))

all_reviews <- all_reviews %>% arrange(Date)
all_reviews$AllCumAverage <- cummean(all_reviews$AverageScore)
all_reviews$AllAmount <- 1:nrow(all_reviews)

from_date <- as.Date('2000-12-31')

grouped <- all_reviews %>% 
  group_by(month=floor_date(Date, "month")) %>% 
  summarise(
    average=mean(AllCumAverage),
    amount= last(AllAmount)
  )

ggplot(grouped %>% filter(month > from_date), aes(x = month, y = amount)) +
  geom_line(stat="identity") +
  ylab("Amount of reviews") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(grouped %>% filter(month > from_date), aes(x = month, y = average)) +
  geom_line(stat="identity") +
  ylab("Score (cumulative mean)") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grouped <- all_reviews %>% 
  group_by(month=floor_date(Date, "month"), Origin) %>% 
  summarise(
    average=mean(CumulativeAverageScore),
    amount= last(CumulativeAmount)
  )

ggplot(grouped %>% filter(month > from_date), aes(x = month, y = amount, color=Origin)) +
  geom_line(stat="identity") +
  ylab("Amount of reviews per site") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(grouped %>% filter(month > from_date), aes(x = month, y = average, color=Origin)) +
  geom_line(stat="identity") +
  ylab("Score per site") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


tableized.data <- tbl_df(all_reviews)

# Let's start on some machine learning modelling and shit! 

# First, set the seed. We always want a set seed for reproducibility.
# I've chosen a completely arbitrary seed. Nothing to see here.
set.seed(80085)

train.size <- floor(0.75 * nrow(tableized.data))
train.indices <- sample(1:nrow(tableized.data), size = train.size)

train.df <- tableized.data[train.indices,]
test.df  <- tableized.data[-train.indices,]

reviews <- train.df$Review
corpus <- createCleanCorpus(reviews)
dtm <- DocumentTermMatrix(corpus)
createWordCloud(TermDocumentMatrix(corpus) %>% removeSparseTerms(0.99))

dtm <- dtm %>% removeSparseTerms(0.99)

dtm.df <- dtm %>% as.matrix() %>% as.data.frame()
dtm.df <- apply(dtm.df, MARGIN = 2, convert_counts)
# Let's remove the original Review column, and re-add the dtm's values for prediction
train.df <- train.df %>% select(-Review, -Reviewer,-Origin, -QualityScore, -PriceScore, -SupportScore)
train.df <- cbind(train.df, dtm.df)

# Train a naive bayes model using the  formula of every other column being used in 
# predicting the AverageScore
model <- train(
  AverageScore ~ . ,
  train.df,
  method = "rf",
  trControl=trainControl(method = "cv", number = 10)
)



