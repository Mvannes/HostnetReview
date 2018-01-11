library(pattern.nlp)
library(magrittr)
library(dplyr)
library(ggplot2)

refresh_data <- function() {
  working_set$Package <- working_set$Package %>%
    gsub('Hostnet anders', 'Anders', .) %>%
    gsub('Hostnet Start', 'Webhosting Start', .) %>% 
    gsub('Hostnet Pro', 'Webhosting Pro', .) %>%
    gsub('Hostnet Plus', 'Webhosting Plus', .)
  
  sentiment_data <<- working_set$Review %>% 
    as.vector() %>% 
    lapply(., FUN=pattern_sentiment, language="dutch") %>%
    do.call("rbind", .)
  
  sentimental <<- cbind(sentiment_data, working_set)
  
  # pattern_data <<- working_set$Review %>% 
  #   as.vector() %>% 
  #   lapply(., FUN=pattern_pos, language="dutch", core=TRUE) %>%
  #   do.call("rbind", .)
}

# Bar chart of average polarity by package
sentimental %>% 
  select(polarity, Package) %>% 
  group_by(Package) %>% 
  summarise(MeanPolarity=mean(polarity)) %>% 
  ggplot(aes(x=Package, y=MeanPolarity, color=Package, fill=Package)) + geom_col()

# Bar chart of average subjectivity by package
sentimental %>% 
  select(subjectivity, Package) %>% 
  group_by(Package) %>% 
  summarise(MeanSubjectivity=mean(subjectivity)) %>% 
  ggplot(aes(x=Package, y=MeanSubjectivity, color=Package, fill=Package)) + geom_col()

# Boxplot of subjectivity by package
sentimental %>% 
  select(subjectivity, Package) %>% 
  group_by(Package) %>% 
  ggplot(aes(x=Package, y=subjectivity, color=Package, fill=Package)) + geom_boxplot()

# Boxplot of polarity by package
sentimental %>% 
  select(polarity, Package) %>% 
  group_by(Package) %>% 
  ggplot(aes(x=Package, y=polarity, color=Package, fill=Package)) + geom_boxplot()

# Scatterplot of polarity vs subjectivity, showing that the best and worst reviews
# (high and low polarity), have a higher subjectivity score.
sentimental %>%
  select(polarity, subjectivity) %>%
  ggplot(aes(x=polarity, y=subjectivity, color=polarity>0.0)) + geom_point(size=4)
  
