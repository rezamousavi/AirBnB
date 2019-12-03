library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidyr)
library(tidytext)
library(rdd)
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
library(rddtools)
library(psych)
library(Hmisc)
library(corrplot)
library(lubridate)
library(tsibble)
getwd()

setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/Data")

data_au <- read.csv("zip_level_au_v2.csv", na.strings = "NA")
data_nash <- read.csv("zip_level_nash_v2.csv", na.strings = "NA")
data_nash$clean_comments <- NULL
data_nash$is_business_travel_ready <- NULL
data_bos <- read.csv("zip_level_bos_v2.csv", na.strings = "NA")
data_bos$clean_comments <- NULL
data_bos$is_business_travel_ready <- NULL
data_sf <- read.csv("zip_level_sf_v2.csv", na.strings = "NA")
data_sf$clean_comments <- NULL
data_sf$is_business_travel_ready <- NULL

data <- rbind(data_au, data_nash, data_bos, data_sf)
rm(data_au, data_bos, data_nash, data_sf)

data$year <- year(as.yearqtr(data$time))

####################### Monthly Analysis ################
data <- subset(data, year >= 2013 & year <= 2016)

data$time <- ymd(data$date)
data$month <- floor_date(data$time, "month")
sort(unique(data$month))
# data$week <- strftime(data$time, format = "%Y-%W", tz = "CET")

sort(unique(data$month))

data2 <- data %>%
  filter(month <= ymd("2015-06-01") & month >= ymd("2013-04-01"))

# Descriptive Stats:
normalize <- function(a){
  (a-min(a, na.rm = T))/(max(a, na.rm = T)-min(a, na.rm = T))
}

data2 <- data2 %>%
  group_by(listing_id, month) %>%
  mutate(pd_sentiment = abs(( max(sentiment) - min(sentiment) )/ 
                           mean(sentiment, na.rm = T)) ) %>% 
  mutate(cv_sentiment = abs(sd(sentiment, na.rm = T) / 
           mean(sentiment, na.rm = T) ))


rcorr(as.matrix(data2[c("count_topic","word_count","objectivity","sentiment",
                       "pd_sentiment","cv_sentiment")]))

data2$y = data2$count_topic

data2$y_norm <- normalize(data2$y)
# data2$y_norm <- data2$y
summary(data2$y_norm)

data3 <- data2[complete.cases(data2$y_norm),]

data4 <- data3 %>%
  group_by(listing_id,month) %>%
  mutate(mean_y = mean(y_norm, na.rm = T)) %>%
  mutate(num_rev = n()) %>%
  dplyr::select(listing_id,mean_y,num_rev,month) %>%
  distinct() %>%
  filter(!(as.character(month) %in% c("2014-05-01","2014-06-01",
                        "2014-04-01") ))

# data4 <- data4 %>%
#   group_by(listing_id) %>%
#   mutate(months_count = n()) 
# %>%
#   filter(months_count == 24)

data4$m <- as.numeric(as.factor(data4$month))

data4$after <- ifelse(data4$m > 12, 1,0)
# t.test(data4$mean_y~data4$after)

library(rdrobust)
rdplot(x=data4$m, y=data4$mean_y,
       c = 12.5, 
       title = "RD Plot for Topic_Count",
       x.label = "Month (Numbers from 1 to 12)",
       y.label = "Topic_Count Scores"
       # , y.lim = c(0.1,0.4)
       )

data_rdd_Z <- rdd_data(y = data4$mean_y, 
                       covar = data4$num_rev,
                       x = data4$m, cutpoint = 12.5
                       )

(bw_ik <- rdd_bw_ik(data_rdd_Z))
# reg_nonpara <- rdd_reg_np(rdd_object=data_rdd_Z, bw=bw_ik)
# summary(reg_nonpara)
# plot(x=reg_nonpara)
# full_rdd <- rdd_gen_reg(data_rdd_Z,
#                         # covariates = T,
#                         order = 3)
# summary(full_rdd)
# print(full_rdd)

full_rdd_para <- rdd_reg_lm(data_rdd_Z, 
                        covariates = T
                        , bw = bw_ik
                        , order = 2
                        )
summary(full_rdd_para)
plot(full_rdd_para)

clusterInf(full_rdd_para, clusterVar= data4$listing_id,type = c("HC"))

plotSensi(full_rdd_para)
# plotPlacebo(full_rdd_para, from = 0.1, to = 1, same_bw = T)

###### Create the placebo plots:
data_rdd_placebo <- rdd_data(y = data4$mean_y, 
                       covar = data4$num_rev,
                       x = data4$m, cutpoint = 9.5
)

full_rdd_placebo <- rdd_reg_lm(data_rdd_placebo, 
                            covariates = T
                            , bw = bw_ik
                            , order = 2
)

mu <- full_rdd_placebo$coefficients[2]
sd <- print(full_rdd_placebo)[2]

upp <- mu + (2 * sd)
low <- mu - (2 * sd)

#############################
data4$after_policy <- ifelse(data4$month >= as.Date("2014-07-01", format = "%Y-%m-%d"),
                             1,0)
ggplot(subset(data4), aes(x=month, y=mean_y, colour=factor(after_policy))) + 
  stat_summary(fun.y="mean", geom="point") + 
  geom_smooth(method='lm') + xlab("Quarter ID") +
  ylab("Topic_Count") +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))


################## END Monthly Analysis #################

################## Quarterly Analysis (Short) #################
data$quarter <- floor_date(data$month, "quarter")
unique(data$quarter)

# Descriptive Stats:
normalize <- function(a){
  (a-min(a))/(max(a)-min(a))
}

data <- data %>%
  group_by(listing_id, quarter) %>%
  mutate(pd_sentiment = abs(( max(sentiment) - min(sentiment) )/ 
                              mean(sentiment, na.rm = T)) ) %>% 
  mutate(cv_sentiment = abs(sd(sentiment, na.rm = T) / 
                              mean(sentiment, na.rm = T) ))

data$y = data$count_topic

# data$y_norm <- normalize(data$y)
data$y_norm <- data$y
summary(data$y_norm)

data2 <- subset(data, before_after==1 & 
                  as.yearqtr(time) > as.yearqtr("2012-12-01", format="%Y-%m-%d")
                & as.yearqtr(time) < as.yearqtr("2016-01-01", format="%Y-%m-%d"))

data3 <- data2[complete.cases(data2$y_norm),]

data4 <- data3 %>%
  group_by(listing_id, quarter) %>%
  mutate(mean_y = mean(y_norm, na.rm = T)) %>%
  mutate(num_rev = n()) %>%
  select(mean_y,listing_id,num_rev,quarter) %>%
  distinct() %>%
  filter(!(as.character(quarter) %in% c("2014-04-01") ))

data4$q <- as.numeric(as.factor(data4$quarter))

data_rdd_Z <- rdd_data(y = data4$mean_y, 
                       covar = data4$num_rev,
                       x = data4$q, cutpoint = 12)
plot(data_rdd_Z, device = "ggplot", h=1)
# covarTest_mean(data_rdd_Z)

# full_rdd <- rdd_gen_reg(data_rdd_Z, 
#                         covariates = T,
#                         order = 3)
# summary(full_rdd)
# print(full_rdd)

full_rdd_para <- rdd_reg_lm(data_rdd_Z, 
                            covariates = T,
                            order = 3)
summary(full_rdd_para)
coeftest(full_rdd_para,
         vcov.=vcovCluster(full_rdd_para,
                           clusterVar=data4$listing_id)) 

plotPlacebo(full_rdd_para,from = .35, to = .9, by = .1, level = 0.95)
################## END Quarterly Analysis (Short) #################

################## Overidentification Test ########################
data$quarter <- floor_date(data$month, "quarter")
unique(data$quarter)

# Descriptive Stats:
normalize <- function(a){
  (a-min(a))/(max(a)-min(a))
}

data <- data %>%
  group_by(listing_id, quarter) %>%
  mutate(pd_sentiment = abs(( max(sentiment) - min(sentiment) )/ 
                              mean(sentiment, na.rm = T)) ) %>% 
  mutate(cv_sentiment = abs(sd(sentiment, na.rm = T) / 
                              mean(sentiment, na.rm = T) ))

data$y = data$count_topic

data2 <- subset(data, before_after==1 & 
                  as.yearqtr(time) > as.yearqtr("2009-12-01", format="%Y-%m-%d")
                & as.yearqtr(time) < as.yearqtr("2017-01-01", format="%Y-%m-%d"))

data3 <- data2[complete.cases(data2$y),]

data3$q <- as.numeric(data3$time) - 5
data3$q2 <- ifelse(data3$q<18,data3$q,data3$q+1)
data3$july_before_listing <- ifelse(data3$max_after==0,ifelse(data3$q2==19,1,0),0)
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
tapply(data3$y, data3$max_after, mean)

plotmeans(y ~ q, main="The Average Word Count Over Time (All Cities)", 
          data=subset(data3, before_after==1))
abline(v=17, col="green")
abline(v=18, col="red")

ggplot(subset(data3, july_before_listing==0), aes(x=q2, y=y, colour=factor(max_after))) + 
  stat_summary(fun.y="mean", geom="point") + 
  geom_smooth(method='lm') + xlab("Quarter ID") +
  ylab("Topic_Count") +
  scale_x_continuous(breaks=c(1:30), 
                     labels=c(1:30),limits=c(1,30)) +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22,face="bold"))

# FE:
fixed <- plm(y ~ factor(max_after) + count + 
               factor(year),
             data=subset(data3, before_after==1),
             index=c("listing_id", "time"), model="within")
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))

## 2SLS:
data3 <- data3 %>%
  group_by(neighbourhood_cleansed, time) %>%
  mutate(mean_count_zip = mean(count, na.rm = T))

summary(data3$mean_count_zip)

tsls <- plm(y ~ factor(max_after) + count + 
              factor(year)| factor(max_after) + 
              mean_count_zip + factor(year), 
            data = subset(data3),
            index=c("listing_id", "time"), model="within")
summary(tsls)


################## END Overidentification Test ####################

################## Interrupted Time Series Design #################
data$time <- ymd(data$date)
data$month <- floor_date(data$time, "month")
library(remote)
data("australiaGPCP")

aus_dsn <- deseason(australiaGPCP, 12)

opar <- par(mfrow = c(1,2))
plot(australiaGPCP[[1]], main = "original")
plot(aus_dsn[[1]], main = "deseasoned")
par(opar)






#









#####################################################################
data$time <- ymd(data$date)
data$week <- yearweek(data$time)

data <- data %>%
  group_by(listing_id, week) %>%
  mutate(pd_sentiment = abs(( max(sentiment) - min(sentiment) )/ 
                              mean(sentiment, na.rm = T)) ) %>% 
  mutate(cv_sentiment = abs(sd(sentiment, na.rm = T) / 
                              mean(sentiment, na.rm = T) ))

data$y = data$sentiment

normalize <- function(a){
  (a-min(a))/(max(a)-min(a))
}
data$y_norm <- normalize(data$y)
# data$y_norm <- data$y
summary(data$y_norm)

data2 <- subset(data, before_after==1 & 
                  as.yearqtr(time) > as.yearqtr("2013-03-01", format="%Y-%m-%d")
                & as.yearqtr(time) < as.yearqtr("2015-06-01", format="%Y-%m-%d"))

data3 <- data2[complete.cases(data2$y_norm),]
unique(data3$week)

yearweek("2014-05-01")
yearweek("2014-07-10")

# seq.Date(min(yearweek("2014-05-01")), max(yearweek("2014-07-10")), by="week")

data3$Policy_Change <- ifelse(data3$week>
                                yearweek("2014-07-10"),1,0)

data4 <- data3 %>%
  group_by(week) %>%
  mutate(mean_y = mean(y_norm, na.rm = T)) %>%
  mutate(num_rev = n()) %>%
  select(mean_y,num_rev,week,Policy_Change) %>%
  distinct() 

full_dates <- as.data.frame(seq.Date(min(yearweek(data4$week)), max(yearweek(data4$week)), 
                  by = "week"))

colnames(full_dates) <-"week"

data5 <- full_dates %>%
  left_join(data4, by = "week")

data5$week_id <- as.numeric(as.factor(data5$week))

ggplot(data5, aes(x = week_id, y = mean_y, color = factor(Policy_Change))) + 
  geom_point()+
  geom_smooth(method = "lm") 
# + ylim(0,50)

# Remove Seasonality from Data:
library(forecast)

ts_y = ts(data5$mean_y, frequency = 52)

library(zoo)
decompose_y = decompose(na.StructTS(ts_y))

decompose_y = decompose(ts_y, "additive", na.action= na.rm)

plot(as.ts(decompose_y$seasonal))
plot(as.ts(decompose_y$trend))
plot(as.ts(decompose_y$random))
plot(decompose_y)


stl_ts = stl(na.StructTS(ts_y), "periodic")
seasonal_stl_ts   <- stl_ts$time.series[,1]
trend_stl_ts     <- stl_ts$time.series[,2]
random_stl_ts  <- stl_ts$time.series[,3]

plot(ts_y)
plot(as.ts(seasonal_stl_ts))
plot(trend_stl_ts)
plot(random_stl_ts)
plot(stl_ts)



#######################
data_rdd_Z <- rdd_data(y = data4$mean_y, 
                       covar = data4$num_rev,
                       x = data4$week_id, cutpoint = 56)
plot(data_rdd_Z, device = "ggplot", h=1)

rdd_bw_ik(data_rdd_Z)
rdd_bw_rsw(data_rdd_Z)

full_rdd_para <- rdd_reg_lm(data_rdd_Z, 
                            covariates = T,
                            order = 3)
summary(full_rdd_para)

plotSensi(full_rdd_para)
plotPlacebo(full_rdd_para,from = .5, to = .8, by = .1, level = 0.95)
