library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidyr)
library(tidytext)
library(Hmisc)
getwd()

# First run ABB_text_mining.ipynb to clean the text. Then try:
setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")
listings <- fread("listings.csv", 
                  sep=",", strip.white = T, stringsAsFactors = F)

listings$text <- paste(listings$summary, listings$space, listings$description,
                       listings$experiences_offered, listings$neighborhood_overview,
                       listings$notes, listings$transit, listings$access, listings$interaction,
                       sep='') 

setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/diction_input_test")
for (i in 1:4){
  cat(listings$text[i], file = paste(listings$id[i],".txt", sep=""))
}

# Export reviews to txt files:
setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")
reviews <- fread("sentiment_data_03072017.csv", 
                  sep=",", strip.white = T, stringsAsFactors = F)
reviews$list_rev_id <- paste(reviews$listing_id, reviews$id,
                       sep='_') 

setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/diction_input_reviews")
for (i in 1:dim(reviews)[1]){
  cat(reviews$comments[i], file = paste(reviews$list_rev_id[i],".txt", sep=""))
}

## Read Diction Data:
diction <- fread("diction_data.csv", 
                  sep=",", strip.white = T, stringsAsFactors = F)
diction <- as.data.frame(diction)
diction$id <- diction$`File ID`

listings <- data.frame(listings)
vars <- c("id","review_scores_rating", "review_scores_accuracy",
  "review_scores_cleanliness", "review_scores_checkin",
  "review_scores_communication", "review_scores_location",
  "review_scores_value")
ratings <- as.data.frame(listings[vars])

data <- merge(diction,ratings, on = "id")
cor_data <- data[c(3:46,53:60)]
cor_data <- sapply( cor_data, as.numeric )

dat <- rcorr(cor_data, type="pearson")

write.csv(rcorr(cor_data, type="pearson")$r,"corr_coef.csv")
write.csv(rcorr(cor_data, type="pearson")$P,"corr_pvalue.csv")


text_mining <- fread("sentiment_subjectivity_emotion_data_03072017.csv",sep=",", 
              strip.white = T, stringsAsFactors = F)
text_mining <- as.data.frame(text_mining)

text_mining <- text_mining %>%
  group_by(listing_id) %>%
  mutate(count = n()) %>%
  mutate(mean_sentiment = mean(sentiment)) %>%
  mutate(mean_subject = mean(subjectivity)) %>%
  mutate(mean_w_count = mean(word_count)) %>%
  mutate(mean_anger = mean(anger)) %>%
  mutate(mean_anticipate = mean(anticipate)) %>%
  mutate(mean_disgust = mean(disgust)) %>%
  mutate(mean_fear = mean(fear)) %>%
  mutate(mean_joy = mean(joy)) %>%
  mutate(mean_sad = mean(sad)) %>%
  mutate(mean_surprise = mean(surprise)) %>%
  mutate(mean_trust = mean(trust)) %>%
  mutate(mean_negative = mean(negative))%>%
  mutate(mean_positive = mean(positive))%>%
  mutate(sd_sentiment = sd(sentiment)) %>%
  mutate(sd_subject = sd(subjectivity)) %>%
  mutate(sd_w_count = sd(word_count)) %>%
  mutate(sd_anger = sd(anger)) %>%
  mutate(sd_anticipate = sd(anticipate)) %>%
  mutate(sd_disgust = sd(disgust)) %>%
  mutate(sd_fear = sd(fear)) %>%
  mutate(sd_joy = sd(joy)) %>%
  mutate(sd_sad = sd(sad)) %>%
  mutate(sd_surprise = sd(surprise)) %>%
  mutate(sd_trust = sd(trust)) %>%
  mutate(sd_negative = sd(negative))%>%
  mutate(sd_positive = sd(positive))%>%
  mutate(min_sentiment = min(sentiment)) %>%
  mutate(min_subject = min(sentiment)) %>%
  mutate(min_w_count = min(word_count)) %>%
  mutate(min_anger = min(anger)) %>%
  mutate(min_anticipate = min(anticipate)) %>%
  mutate(min_disgust = min(disgust)) %>%
  mutate(min_fear = min(fear)) %>%
  mutate(min_joy = min(joy)) %>%
  mutate(min_sad = min(sad)) %>%
  mutate(min_surprise = min(surprise)) %>%
  mutate(min_trust = min(trust)) %>%
  mutate(min_negative = min(negative))%>%
  mutate(min_positive = min(positive))%>%
  mutate(max_sentiment = max(sentiment)) %>%
  mutate(max_subject = max(subjectivity)) %>%
  mutate(max_w_count = max(word_count)) %>%
  mutate(max_anger = max(anger)) %>%
  mutate(max_anticipate = max(anticipate)) %>%
  mutate(max_disgust = max(disgust)) %>%
  mutate(max_fear = max(fear)) %>%
  mutate(max_joy = max(joy)) %>%
  mutate(max_sad = max(sad)) %>%
  mutate(max_surprise = max(surprise)) %>%
  mutate(max_trust = max(trust)) %>%
  mutate(max_negative = max(negative))%>%
  mutate(max_positive = max(positive))

text_mining <- text_mining[!duplicated(text_mining[,c('listing_id')]),]
text_mining$pd_sentiment <- abs((text_mining$max_sentiment-
                                   text_mining$min_sentiment)/text_mining$mean_sentiment)
text_mining$pd_w_count <- abs((text_mining$max_w_count-
                                 text_mining$min_w_count)/text_mining$mean_w_count)
text_mining$pd_subject <- abs((text_mining$max_subject-
                                 text_mining$min_subject)/text_mining$mean_subject)
text_mining$cv_sentiment <- abs(text_mining$sd_sentiment/text_mining$mean_sentiment)
text_mining$cv_w_count <- abs(text_mining$sd_w_count/text_mining$mean_w_count)
text_mining$cv_subject <- abs(text_mining$sd_subject/text_mining$mean_subject)

