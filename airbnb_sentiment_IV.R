library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidyr)
library(tidytext)
library(syuzhet)
library(rdd)
getwd()

setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")
data <- fread("sentiment_subjectivity_emotion_data_sf.csv",sep=",", 
              strip.white = T, stringsAsFactors = F)
data <- as.data.frame(data)
data$V1 <- NULL

data$min_date <- ave(data$date,data$listing_id, FUN = min)
data$max_date <- ave(data$date,data$listing_id, FUN = max)
data$after <- ifelse(data$date>as.Date("2014-07-10"), 1, 0)

data$before_after <- ifelse(data$min_date<as.Date("2014-03-01"),
                            ifelse(data$max_date>=as.Date("2014-07-10"),1,0),0)

t.test(data$sentiment~data$after)
t.test(data$word_count~data$after)
hist(data$sentiment)
hist(data$word_count)

quarter = TRUE
if (quarter){
  data$time <- as.yearqtr(data$date, format = "%Y-%m-%d")
}else{
  data$time <- year(data$date)
}

# Adding Data for Studying Interactions:
listings <- as.data.frame(fread("listings_sf.csv", 
                    sep=",", strip.white = T, stringsAsFactors = F, 
                    na.strings = c("","NA","NaN","nan"," ","N/A")))

listings$listing_id <- listings$id
listings$host_total_listings_count <- 
  as.numeric(listings$host_total_listings_count)
# listings$new_listings <- data$new_listings[match(listings$listing_id, data$listing_id)]
# listings <- subset(listings, new_listings>=0)
listings$host_acceptance_rate <- as.numeric(sub("%", "", listings$host_acceptance_rate))
listings$host_response_rate <- as.numeric(sub("%", "", listings$host_response_rate))
listings$price2 <- as.numeric(sub("[$]","", listings$price))
listings$cleaning_fee2 <- as.numeric(sub("[$]","", listings$cleaning_fee))
listings[is.na(listings$cleaning_fee2),c("cleaning_fee2")] <- 0
listings$total_price <- listings$price2+listings$cleaning_fee2

data <- merge(data,listings, by = "listing_id", all.x=T)
data <- data[!(data$date>as.Date("2014-03-31") & data$date<=as.Date("2014-07-10")),]

# detach("package:plyr", unload=TRUE)
data <- data %>%
  group_by(listing_id, time) %>%
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
  mutate(max_after = max(after))%>%
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

data <- data %>%
  group_by(time, neighbourhood_cleansed) %>%
  mutate(count_zip = n()) %>%
  mutate(mean_count_zip = count_zip / length(unique(listing_id))) %>%
  mutate(mean_sentiment_zip = mean(sentiment)) %>%
  mutate(mean_subject_zip = mean(subjectivity)) %>%
  mutate(mean_w_count_zip = mean(word_count)) 

data <- data[!duplicated(data[,c('listing_id','time')]),]

data$pd_sentiment <- abs((data$max_sentiment-data$min_sentiment)/data$mean_sentiment)
data$pd_w_count <- abs((data$max_w_count-data$min_w_count)/data$mean_w_count)
data$pd_subject <- abs((data$max_subject-data$min_subject)/data$mean_subject)
data$cv_sentiment <- abs(data$sd_sentiment/data$mean_sentiment)
data$cv_w_count <- abs(data$sd_w_count/data$mean_w_count)
data$cv_subject <- abs(data$sd_subject/data$mean_subject)
data$pd_anger <- abs((data$max_anger-data$min_anger)/data$mean_anger)

# data$pd_sentiment[is.na(data$pd_sentiment)] <- 0
summary(data$pd_sentiment)
summary(data$cv_sentiment)
summary(data$pd_subject)
summary(data$cv_subject)

## Add Topic Data:
topic_data <- as.data.frame(fread("sentiment_subject_3topics_sf.csv",
                                  sep=",", strip.white = T, stringsAsFactors = F))
quarter = TRUE
if (quarter){
  topic_data$time <- as.yearqtr(topic_data$date, format = "%Y-%m-%d")
}else{
  topic_data$time <- year(topic_data$date)
}

hed <- topic_data[,c("listing_id","date","time","top1","top2","top3")]
hed_long <- gather(hed, topic_set, topic, top1:top3, factor_key=TRUE)
hed_long <- hed_long %>%
  filter(!is.na(topic)) %>%
  group_by(listing_id,time) %>%
  summarise(count_topic = n_distinct(topic))

hed <- hed_long[!duplicated("listing_id","time"),]

data <- merge(data,hed[c("listing_id","time","count_topic")], 
              by = c("listing_id","time"), all.x=T)

data$count_topic[is.na(data$count_topic)] <- 0
data$norm_count_topic <- data$count_topic / data$count

data$inter_price <- data$max_after * data$total_price
data$inter_review <- data$max_after * data$review_scores_accuracy
summary(as.factor(data$room_type))
data$room_type2 = ifelse(data$room_type=="Entire home/apt",1,0)
data$inter_room_type <- data$max_after * data$room_type2
data$host_is_superhost = ifelse(data$host_is_superhost=="t",1,0)
data$inter_host_listings <- data$max_after * data$host_total_listings_count
data$tenure <- data$time - as.yearqtr(data$min_date, format = "%Y-%m-%d")
data$tenure <- (data$tenure + 0.25)*4

write.csv(data,"~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/zip_level_sf.csv", 
          row.names = F)

## Start from Here: (Source: https://www.princeton.edu/~otorres/Panel101R.pdf)
data <- read.csv("zip_level_sf.csv", na.strings = "NA")
plot(table(data$listing_id))
mean(table(data$listing_id))

p <- ggplot(subset(data)) + 
  geom_boxplot(aes(x=time, y=sentiment, group = time)
               ,outlier.shape=NA
  ) 

p + geom_vline(xintercept=as.yearqtr("2014 07", "%Y %m")) 

# mydata <- data[complete.cases(data[c("review_scores_rating","review_scores_accuracy",
#                                      "review_scores_cleanliness","review_scores_checkin",
#                                      "review_scores_communication","review_scores_location",
#                                      "review_scores_value")]),]
# 
# library(corrplot)
# cor(mydata[c("review_scores_rating","review_scores_accuracy",
#              "review_scores_cleanliness","review_scores_checkin",
#              "review_scores_communication","review_scores_location",
#              "review_scores_value")])


library(plm)
library("sandwich")
library("lmtest")
library(psych)
library(Hmisc)
library(corrplot)
library(foreign)
library(gplots)

plotmeans(mean_sentiment ~ time, main="Heterogeineity across Listings (SF)", 
          data=subset(data, before_after==1))
abline(v=20, col="green")
abline(v=21, col="red")

plotmeans(mean_sentiment ~ zipcode, main="Heterogeineity across Listings (SF)", 
          data=subset(data))

fixed <- plm(mean_sentiment ~ factor(max_after) +  count,
             data=subset(data),
             index=c("listing_id", "time"), model="within")
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))

random <- plm(mean_sentiment ~ factor(max_after) +  count + factor(time),
             data=subset(data),
             index=c("listing_id", "time"), model="random")

phtest(fixed, random)

pFtest(fixed_time, fixed)
plmtest(fixed, c("time"), type="bp")


# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
pool <- plm(mean_sentiment ~ factor(max_after) +  count + factor(time),
              data=subset(data),
              index=c("listing_id", "time"), model="pooling")
plmtest(pool, type=c("bp"))

### Test of cross-sectional dependence:
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

### Test of serial correlation:
pbgtest(fixed)

## 2SLS:
tsls <- plm(mean_negative ~ factor(max_after) + count + 
              factor(time)| factor(max_after) + 
              mean_count_zip + factor(time), data = data,
            index=c("listing_id", "time"), model="within")
coeftest(tsls,vcov=vcovHC(tsls,type="HC0",cluster="group"))

c_model <- plm(count ~ mean_count_zip + factor(time) + 
                 factor(max_after), data=data,
               index=c("listing_id", "time"), model="within")
data$pred_count <- as.numeric(c_model$model[[1]] - c_model$residuals) 

f <- plm(pd_sentiment ~ factor(max_after) + pred_count + 
           factor(time), data = data,
            index=c("listing_id", "time"), model="within")
coeftest(f,vcov=vcovHC(f,type="HC0",cluster="group"))

library(AER)
tsls <- ivreg(count_topic ~ factor(max_after) + 
                count + factor(time)+ 
                factor(listing_id)| factor(max_after) + 
                mean_count_zip +factor(time)+ 
                factor(listing_id), data = data)
coeftest(tsls,vcov=vcovHC(tsls,type="HC0",cluster="group"))


## Aggregate Data (Before and After):
agg_data <- data %>%
  group_by(max_after) %>%
  mutate(agg_sent = mean(sentiment)) %>%
  mutate(agg_sub = mean(subjectivity)) %>%
  mutate(agg_wcount = mean(word_count)) %>%
  mutate(agg_topic = mean(count_topic)) 

agg_data <- agg_data[,c("listing_id","max_after","agg_sent","agg_sub","agg_wcount",
                        "agg_topic")]
agg_data <- agg_data[!duplicated(agg_data[,c("listing_id","max_after")]),]

fixed <- plm(agg_topic ~ factor(max_after), 
             data=subset(agg_data),
             index=c("listing_id","max_after"), model="within")

coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))



