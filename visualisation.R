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

all_reviews <- hosting_reviews %>% select(Date, AverageScore, cumulativeAverageScore, Origin, amount)

all_reviews <- rbind(all_reviews, select(webhosting_reviews, Date, AverageScore, cumulativeAverageScore, Origin, amount))
all_reviews <- rbind(all_reviews, select(kiyoh_reviews, Date, AverageScore, cumulativeAverageScore, Origin, amount))

all_reviews <- all_reviews %>% arrange(Date)
all_reviews$AllCumAverage <- cummean(all_reviews$AverageScore)
all_reviews$AllAmount <- 1:nrow(all_reviews)


grouped <- all_reviews %>% 
  group_by(month=floor_date(Date, "month"), Origin) %>% 
  summarise(
    average=mean(cumulativeAverageScore),
    amount= last(amount)
  )

ggplot(grouped %>% filter(month > as.Date('2017-12-31')), aes(x = month, y = amount, color=Origin)) +
  geom_line(stat="identity", alpha=0.30) +
  ylab("Score") +
  xlab("Time") +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %Y")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


csv.name <- paste(getwd(), 'hostnetreviews.csv', sep = "/")
csv.header.required <- TRUE
csv.sep <- ','

# Read the data into our environment.
data <- read.csv(
  file             = csv.name,
  header           = csv.header.required,
  sep              = csv.sep,
  stringsAsFactors = FALSE,
  strip.white      = TRUE
) 

tableized.data <- tbl_df(data)
tableized.data <- tableized.data %>% dplyr::select(-X)

tableized.data$Date <- as.Date(tableized.data$Date)
tableized.data <- tableized.data %>% mutate(AverageScore=(SupportScore + QualityScore + PriceScore) / 3)


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
dtm <- dtm %>% removeSparseTerms(0.99)

dtm.df <- dtm %>% as.matrix() %>% as.data.frame()
dtm.df <- apply(dtm.df, MARGIN = 2, convert_counts)
# Let's remove the original Review column, and re-add the dtm's values for prediction
train.df <- train.df %>% dplyr::select(-Review, -Reviewer,-Origin, -QualityScore, -PriceScore, -AverageScore)
train.df <- cbind(train.df, dtm.df)

# Train a naive bayes model using the  formula of every other column being used in 
# predicting the SupportScore.
model <- train(
  SupportScore ~ . ,
  train.df,
  method = "rf",
  trControl=trainControl(method = "cv", number = 10)
)



