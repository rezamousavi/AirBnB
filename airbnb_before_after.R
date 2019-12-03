library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidyr)
library(tidytext)
library(rdd)
library(EventStudy)
library(plm)
library(sandwich)
library(lmtest)
library(psych)
library(Hmisc)
library(corrplot)
library(foreign)
library(gplots)
library(devtools)
# install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
library(RDDtools)
getwd()

# First run ABB_text_mining.ipynb to clean the text. Then try:
setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")
data_au <- read.csv("sentiment_subjectivity_emotion_data_03072017.csv", na.strings = "NA")
data_au$X <- NULL
data_nash <- read.csv("sentiment_subjectivity_emotion_data_nash.csv", na.strings = "NA")
data_nash$clean_comments <- NULL           
data_nash$is_business_travel_ready <- NULL
data_bos <- read.csv("sentiment_subjectivity_emotion_data_bos.csv", na.strings = "NA")
data_bos$clean_comments <- NULL           
data_bos$is_business_travel_ready <- NULL
data_sf <- read.csv("sentiment_subjectivity_emotion_data_sf.csv", na.strings = "NA")
data_sf$clean_comments <- NULL           
data_sf$is_business_travel_ready <- NULL

data <- rbind(data_au, data_nash, data_bos, data_sf)
data$time <- as.Date(data$date, format = "%Y-%m-%d")
data$quarter <- as.yearqtr(data$time)

data <- subset(data, as.yearqtr(time) > as.yearqtr("2009-12-01", format="%Y-%m-%d")
                & as.yearqtr(time) < as.yearqtr("2017-01-01", format="%Y-%m-%d"))

length(unique(data$listing_id))
min(data$time)
max(data$time)

data$min_date <- ave(data$time,data$listing_id, FUN = min)
data$max_date <- ave(data$time,data$listing_id, FUN = max)
data <- subset(data, min_date < as.Date("2014-07-10") &
                              max_date > as.Date("2014-07-10"))


data$after <- ifelse(data$time>as.Date("2014-07-10"), 1, 0)
data <- data[!(data$time>as.Date("2014-05-01") & data$time<=as.Date("2014-07-10")),]

t.test(data$sentiment~data$after)
t.test(data$word_count~data$after)
hist(data$sentiment)
hist(data$word_count)

data$ymon <- format(as.Date(data$date), "%Y-%m")

data <- data %>%
  group_by(listing_id, after) %>%
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

data$pd_sentiment <- abs((data$max_sentiment-data$min_sentiment)/data$mean_sentiment)
data$pd_w_count <- abs((data$max_w_count-data$min_w_count)/data$mean_w_count)
data$pd_subject <- abs((data$max_subject-data$min_subject)/data$mean_subject)
data$cv_sentiment <- abs(data$sd_sentiment/data$mean_sentiment)
data$cv_w_count <- abs(data$sd_w_count/data$mean_w_count)
data$cv_subject <- abs(data$sd_subject/data$mean_subject)

data <- data[!duplicated(data[,c('listing_id','after')]),]

# data$pd_sentiment[is.na(data$pd_sentiment)] <- 0
summary(data$pd_sentiment)
summary(data$cv_sentiment)
summary(data$pd_subject)
summary(data$cv_subject)

data <- data[-c(2:16)]

# Descriptive Stats:
data$y = data$mean_sentiment

normalize <- function(a){
  (a-min(a))/(max(a)-min(a))
}
data$y_norm <- normalize(data$y)
summary(data$y_norm)
# data$y <- data$y_norm 

data3 <- data[complete.cases(data$y),]

data3$q <- as.numeric(as.factor(data3$ymon))
# data3$july_before_listing <- ifelse(data3$after==0,ifelse(data3$q==55,1,0),0)
# data3 <- subset(data3, q != 53 & q != 54 & july_before_listing != 1)
data3[is.na(data3$count),c("count")] <- 0

mn_data <- aggregate(y ~ listing_id + max_after, data=data3, mean)
mn_data <- subset(mn_data, max_after==0)
mn_data$max_after <- NULL
colnames(mn_data) <- c("listing_id","y_bar_before")
data3 <- merge(data3, mn_data, by = c("listing_id"))

data3$adj_y = data3$y - data3$y_bar_before

summary(data3$y)
min(data3$y)
max(data3$y)
sd(data3$y)
tapply(data3$y, data3$after, mean)

# plotmeans(y ~ q, main="The Average Word Count Over Time (All Cities)", 
#           data=subset(data3))
# abline(v=54, col="green")
# abline(v=55, col="red")

ggplot(subset(data3, q>15), aes(x=q, y=y, colour=factor(max_after))) + 
  stat_summary(fun.y="mean", geom="point") + 
  geom_smooth(method='lm') + xlab("Quarter ID") +
  ylab("Negative_Emotion") +
  scale_x_continuous(breaks=c(15:80), 
                     labels=c(15:80),limits=c(15,80)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

data_rdd_Z <- RDDdata(y = data3$adj_y, x = data3$q, 
                      covar = data3$count, cutpoint = 53)
covarTest_mean(data_rdd_Z)
plot(data_rdd_Z, device = "ggplot", h=1)

full_rdd <- RDDreg_lm(data_rdd_Z, covar.opt = 
                        list(strategy = "include",
                             slope = "separate"), order = 3)
summary(full_rdd)
coeftest(full_rdd, 
         vcov.=vcovCluster(full_rdd,
                           clusterVar=data3$listing_id)) 

# FE:
data3$num_q <- as.numeric(as.factor(data3$quarter))
data3$year <- as.factor(year(data3$quarter))

fixed <- plm(y ~ factor(max_after) + count + factor(year), 
             data=subset(data3),
             index=c("listing_id", "ymon"), model="within")
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))












# Adding Data for Studying Interactions:
inter_data <- fread("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/jonloyens-inside-airbnb-austin/listings_03072017.csv", 
                    sep=",", strip.white = T, stringsAsFactors = F, 
                    na.strings = c("","NA","NaN","nan"," ","N/A"))
# unique(inter_data$id)
inter_data$listing_id <- inter_data$id
inter_data <- as.data.frame(inter_data)
inter_data$host_total_listings_count <- 
  as.numeric(inter_data$host_total_listings_count)
# inter_data$new_listings <- data$new_listings[match(inter_data$listing_id, data$listing_id)]
# inter_data <- subset(inter_data, new_listings>=0)
inter_data$host_acceptance_rate <- as.numeric(sub("%", "", inter_data$host_acceptance_rate))
inter_data$host_response_rate <- as.numeric(sub("%", "", inter_data$host_response_rate))
inter_data$price2 <- as.numeric(sub("[$]","", inter_data$price))

agg_data <- merge(data, inter_data[,c("price2","host_acceptance_rate",
                                      "host_response_rate","host_total_listings_count",
                                      "listing_id","review_scores_accuracy",
                                      "room_type","host_is_superhost")], 
                                   by = c("listing_id"), all.x=T)
agg_data$inter_price <- agg_data$max_after * agg_data$price2
agg_data$inter_review <- agg_data$max_after * agg_data$review_scores_accuracy
summary(as.factor(agg_data$room_type))
agg_data$room_type2 = ifelse(agg_data$room_type=="Entire home/apt",1,0)
agg_data$inter_room_type <- agg_data$max_after * agg_data$room_type2
agg_data$host_is_superhost = ifelse(agg_data$host_is_superhost=="t",1,0)
agg_data$inter_host_listings <- agg_data$max_after * agg_data$host_total_listings_count

# Panel Data Analysis:
library(plm)
library("sandwich")
library("lmtest")
# install.packages("corrplot")
library(psych)
library(Hmisc)
library(corrplot)

# describe(data2[c("MEAN_of_sentiment","PD_Sen","CV_Sen","MEAN_of_word_count","PD_Word",
#                  "CV_Word","review_scores_rating","Total_Price","calculated_host_listings",
#                  "Number_Reviews")])
# 
# rcorr(as.matrix(data2[c("MEAN_of_sentiment","PD_Sen","CV_Sen","MEAN_of_word_count","PD_Word",
#                         "CV_Word","review_scores_rating","Total_Price","calculated_host_listings",
#                         "Number_Reviews")]))
# 
# res <- cor(data2[c("MEAN_of_sentiment","PD_Sen","CV_Sen","MEAN_of_word_count","PD_Word",
#                    "CV_Word","review_scores_rating","Total_Price","calculated_host_listings",
#                    "Number_Reviews")])

# corrplot(res, type = "upper", order = "hclust", 
         # tl.col = "black", tl.srt = 45)

describe(data[c("mean_sentiment","pd_sentiment","cv_sentiment","mean_w_count","pd_w_count",
                 "cv_w_count","review_scores_rating","price2","calculated_host_listings_count",
                "count")])

rcorr(as.matrix(data[c("mean_sentiment","pd_sentiment","cv_sentiment","mean_w_count","pd_w_count",
                       "cv_w_count","review_scores_rating","price2","calculated_host_listings_count",
                       "count")]))

res <- cor(data[c("mean_sentiment","pd_sentiment","cv_sentiment","mean_w_count","pd_w_count",
                      "cv_w_count","review_scores_rating","price2","calculated_host_listings_count",
                      "count")])

# data[is.na(data$review_scores_value),c('review_scores_value')]<-mean(data$review_scores_value,na.rm=T)
agg_data %>%
  ggplot(aes(x = cv_sentiment)) +
  geom_bar(color= "red", fill= "red") + coord_flip() + 
  facet_wrap(~ max_after, scales="free",ncol=2)


fixed <- plm(pd_sentiment ~ factor(max_after) + factor(ymon),
             data=subset(agg_data, count>1),
             index=c("listing_id", "ymon"), model="within")
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))[1,]

agg_data$n = 1
agg_data <- agg_data %>%
  group_by(listing_id) %>%
  mutate(month = cumsum(n))

write.csv2(agg_data,file="complete_data_monthly.csv",row.names = F)

#

fixed <- plm(CV_Word ~ factor(After) + Number_Reviews + inter_review +
               inter_price + inter_listing + factor(YQ),
             data=data2,
             index=c("listing_id", "YQ"), model="within")
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))[1:5,]
# 
# fixed <- plm(PD_Sen ~ factor(After) + factor(YQ), 
#              data=data2, 
#              index=c("listing_id", "YQ"), model="within")
# summary(fixed)




fixed <- plm(mean_negative ~ factor(max_after) + count + inter_review + 
               inter_price + inter_host_listings + factor(time), 
             data=subset(agg_data, room_type2==1), 
             index=c("listing_id", "time"), model="within")

coeftest(fixed)
# summary(fixed)

#
random <- plm(pd_sentiment ~ after + factor(time),
              data=subset(data,(new_listings==0)&
                            (time>=as.yearqtr("2014-01-01", format = "%Y-%m-%d"))), 
              index=c("listing_id", "time"), model="random")
summary(random)
phtest(fixed, random) # If sig then use fixed effects

data$num_time <- as.factor(as.numeric(data$time))
write.csv(data,"agg_data.csv")

summary(plm(mean_sentiment ~ after, data=subset(data,mean_sentiment<2), 
            index=c("listing_id", "time"), model="within"))

fixed_time <- plm(mean_sentiment ~ after + factor(time), data=data, 
                  index=c("listing_id", "time"), model="within")
coeftest(fixed_time)

## time-fixed effects: (should use time-fixed effects if sig)
pFtest(fixed_time, fixed)
plmtest(fixed, c("time"), type=("bp"))

## cross-sectional dependence: (exsits if sig)
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

## serial correlation: (exists if sig)
pbgtest(fixed)

## testing unit root/ stationary (no unit roots present if sig)
Panel.set <- plm.data(data, index = c("listing_id", "time"))
library(tseries)
adf.test(Panel.set$mean_sentiment, k=2)

## testing heteroskedasticity (present if sig)
library(lmtest)
bptest(mean_sentiment ~ after + factor(listing_id), data = data, studentize=F)
### If hetero is present:
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

fixed_time <- plm(mean_w_count ~ after , data=data, 
                  index=c("listing_id", "time"), model="within")

coeftest(fixed_time)
pFtest(fixed_time, fixed)

# Adding Data for Studying Interactions:
inter_data <- fread("jonloyens-inside-airbnb-austin/listings.csv", 
                    sep=",", strip.white = T, stringsAsFactors = F, 
                    na.strings = c("","NA","NaN","nan"," ","N/A"))
# unique(inter_data$id)
inter_data$listing_id <- inter_data$id
inter_data <- as.data.frame(inter_data)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(inter_data,2,pMiss)
inter_data$host_total_listings_count <- 
  as.numeric(inter_data$host_total_listings_count)
inter_data$new_listings <- data$new_listings[match(inter_data$listing_id, data$listing_id)]
inter_data <- subset(inter_data, new_listings>=0)
inter_data$host_acceptance_rate <- as.numeric(sub("%", "", inter_data$host_acceptance_rate))
inter_data$host_response_rate <- as.numeric(sub("%", "", inter_data$host_response_rate))
inter_data$price2 <- as.numeric(sub("[$]","", inter_data$price))
split(names(inter_data),sapply(inter_data, function(x) paste(class(x), collapse=" ")))
match_cols <- c("listing_id",
                "host_acceptance_rate",
                "host_total_listings_count",
                "reviews_per_month","price2")
propensity_data <- inter_data[match_cols]

# md.pattern(propensity_data)
colnames(propensity_data)[colSums(is.na(propensity_data)) > 0]
split(names(propensity_data),sapply(propensity_data, function(x) paste(class(x), collapse=" ")))
tempData <- mice(propensity_data,m=5,maxit=10,meth='pmm',seed=500)
summary(tempData)
complete_data <- complete(tempData,1)
complete_data$new_listings <- data$new_listings[match(complete_data$listing_id, data$listing_id)]

m_ps <- glm(new_listings ~ host_acceptance_rate + host_total_listings_count + 
              reviews_per_month + price2 ,
            family = binomial(), data = complete_data)
summary(m_ps)
prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     new_listings = m_ps$model$new_listings)
complete_data$pr_score=predict(m_ps, type = "response")
library(ggplot2)
library(ROCR)
predicted_values <- predict(m_ps, complete_data,type= "response")
pred <- prediction(predicted_values, complete_data$new_listings)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
# I know, the following code is bizarre. Just go with it.
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GLM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

library(MatchIt)
complete_data$group <- as.logical(complete_data$new_listings == 0)
match.it <- matchit(group ~ host_acceptance_rate + host_total_listings_count +
                      reviews_per_month + price2, 
                    data = complete_data, method="nearest", ratio=1)

summary(match.it)
plot(match.it, type = 'jitter', interactive = FALSE)
df.match <- match.data(match.it)[1:ncol(complete_data)]
agg_data <- merge(df.match,data,by = "listing_id")
after_data_matched <- subset(agg_data, after == 1 )
after_data_matched$listing = ifelse(after_data_matched$new_listings==1,"New","Old")
t.test(after_data_matched$mean_sentiment~after_data_matched$new_listings)
t.test(after_data_matched$mean_w_count~after_data_matched$new_listings)
t.test(subset(after_data_matched,review_scores_rating<95)$mean_sentiment~
         subset(after_data_matched,review_scores_rating<95)$new_listings)
t.test(subset(after_data_matched,review_scores_rating<95)$mean_w_count~
         subset(after_data_matched,review_scores_rating<95)$new_listings)
ggplot(after_data_matched, aes(mean_sentiment, fill =listing)) + geom_density(alpha = 0.2)
ggplot(after_data_matched, aes(mean_w_count, fill =listing)) + geom_density(alpha = 0.2)
ggplot(subset(after_data_matched,review_scores_rating<95), aes(mean_sentiment, fill =listing)) + 
  geom_density(alpha = 0.2)
ggplot(subset(after_data_matched,review_scores_rating<95), aes(mean_w_count, fill =listing)) + 
  geom_density(alpha = 0.2)

p <- ggplot(subset(agg_data,(new_listings==0)&(review_scores_rating<95))) + geom_boxplot(aes(x=time, y=mean_sentiment, group = time)) 
p + geom_vline(xintercept=as.yearqtr("2014-07-01", format = "%Y-%m-%d"), linetype="dotted")

ggplot(subset(agg_data,review_scores_rating<90), 
       aes(mean_sentiment,fill=as.character(after))) + geom_density(alpha = 0.2)

summary(plm(mean_sentiment ~ after + time,
            data=subset(agg_data,(new_listings==0)&(review_scores_rating<92)), 
            index=c("listing_id", "time"), model="within"))

mean(subset(after_data, new_listings==0)$mean_w_count)





#
agg_data <- merge(data,inter_data,
                 by = "listing_id", type='left', match='all.x')
detach(data)
attach(agg_data)
fixed_time <- plm(cv_sentiment ~ after * price2 + 
                    after * host_total_listings_count + 
                    after * review_scores_value, data=subset(agg_data,new_listings.x==0), 
                  index=c("listing_id", "time"), model="within")
coeftest(fixed_time)


write.csv(agg_data,"panel_data.csv")
colnames(agg_data)

agg_data<-as.data.frame(agg_data)
agg_data$date_month = as.Date(agg_data$time)
monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")); lt$year*12 + lt$mon } 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }
agg_data$host_exper <- mondf(as.POSIXlt(as.Date(agg_data$first_review, format = "%m/%d/%Y")), as.Date(agg_data$date_month))
agg_data$host_exper <- ifelse(agg_data$host_exper<0,0,agg_data$host_exper)

fixed_time <- plm(mean_sentiment ~ after + factor(host_exper), data=agg_data, 
                  index=c("listing_id", "time"), model="within")
# summary(fixed_time)
coeftest(fixed_time)

# Matching Samples
summary(agg_data$mean_sentiment)
summary(agg_data[agg_data$new_listings==1,"mean_sentiment"])
summary(agg_data[agg_data$new_listings==0,"mean_sentiment"])
old_listings <- subset(agg_data,new_listings==0)
summary(old_listings$mean_sentiment)
summary(old_listings[old_listings$after==0,"mean_sentiment"])
summary(old_listings[old_listings$after==1,"mean_sentiment"])
t.test(agg_data$mean_sentiment~agg_data$new_listings)


head(prs_df)
labs <- paste("Actual Listing:", c("new", "old"))
prs_df %>%
  mutate(new_listings = ifelse(new_listings == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~new_listings) +
  xlab("Probability of new_listings") +
  theme_bw()
#
library("MatchIt")
library(mice)


match.it <- matchit(new_listings ~ host_acceptance_rate +
                    host_total_listings_count +
                    reviews_per_month + price2,
                    data = complete_data, method="nearest")
summary(match.it)
plot(match.it)
dta_m <- match.data(match.it)


dim(dta_m)
t.test(dta_m$mean_sentiment ~ dta_m$new_listings)
lm_treat1 <- lm(mean_sentiment ~ new_listings, data = dta_m)
summary(lm_treat1)


#
View(agg_data[colnames(agg_data)=="date_month" | colnames(agg_data)=="time" 
              | colnames(agg_data)=="host_exper" | colnames(agg_data)=="listing_id"])

fixed_time <- plm(sentiment ~ after + host_exper, data=agg_data, 
                  index=c("listing_id", "time"), model="within")
coeftest(fixed_time)
fixed_time <- plm(sentiment ~ after * host_total_listings_count + host_exper, data=agg_data, 
                  index=c("listing_id", "time"), model="within")
coeftest(fixed_time)

agg2_data <- aggregate(agg_data[,c("sentiment","word_count",
                                   "sd_sentiment","sd_word_count")],
                       by=list(agg_data$listing_id,agg_data$after), 
                       FUN=mean, na.rm=TRUE)
colnames(agg2_data) <- c("listing_id","after","sentiment","word_count",
                         "sd_sentiment","sd_word_count")
summary(plm(sentiment ~ after, data=agg2_data, 
            index=c("listing_id", "after"), model="within"))
t.test(agg2_data$sentiment~agg2_data$after)
t.test(agg2_data$word_count~agg2_data$after)
t.test(agg2_data$sd_sentiment~agg2_data$after)
t.test(agg2_data$sd_word_count~agg2_data$after)







# library(zoo)
# agg_data <- aggregate(data[,c("sentiment","word_count")],
#                       by=list(data$listing_id,data$time), 
#                       FUN=mean, na.rm=TRUE)
# colnames(agg_data) <- c("listing_id","time","sentiment","word_count")
# agg_data2 <- aggregate(data[,c("sentiment","word_count")],
#                        by=list(data$listing_id,data$time), 
#                        FUN=sd, na.rm=TRUE)
# colnames(agg_data2) <- c("listing_id","time","sd_sentiment","sd_word_count")
# agg_data <- merge(agg_data,agg_data2,by = c("listing_id","time"),all.x = TRUE)
# agg_data$after <- ifelse(agg_data$time > as.yearqtr("2014-07-10", format = "%Y-%m-%d"),1,0)
# agg_data <- as.data.frame(agg_data)
# 
# 
# 
# 
# 
# #
# old_listings <- subset(data, min_date<=as.Date("2014-07-10"))
# old_listings <- subset(old_listings, max_date>as.Date("2014-07-10"))
# new_listings <- subset(data, min_date>as.Date("2014-07-10"))
# t.test(old_listings$sentiment~old_listings$after)
# t.test(old_listings$word_count~old_listings$after)
# 
# summary(old_listings$sentiment)
# summary(new_listings$sentiment)
# summary(old_listings$word_count)
# summary(new_listings$word_count)
# # old_listings <- old_listings[,-c(1,3,5:7)]
# # new_listings <- new_listings[,-c(1,3,5:7)]