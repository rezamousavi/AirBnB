library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(janeaustenr)
library(stringr)
library(tidyr)
library(tidytext)
library(rdd)
library(eventstudies)
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

setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")

data_au <- read.csv("zip_level.csv", na.strings = "NA")
data_nash <- read.csv("zip_level_nash.csv", na.strings = "NA")
data_nash$clean_comments <- NULL           
data_nash$is_business_travel_ready <- NULL
data_bos <- read.csv("zip_level_bos.csv", na.strings = "NA")
data_bos$clean_comments <- NULL           
data_bos$is_business_travel_ready <- NULL
data_sf <- read.csv("zip_level_sf.csv", na.strings = "NA")
data_sf$clean_comments <- NULL           
data_sf$is_business_travel_ready <- NULL

data <- rbind(data_au, data_nash, data_bos, data_sf)
data$year <- year(as.yearqtr(data$time))

# Descriptive Stats:
data$y = data$mean_joy

normalize <- function(a){
  (a-min(a))/(max(a)-min(a))
  }
data$y_norm <- normalize(data$y)
summary(data$y_norm)
# data$y <- data$y_norm 

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
  ylab("Negative_Emotion") +
  scale_x_continuous(breaks=c(1:30), 
                     labels=c(1:30),limits=c(1,30)) +
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))


# plot(table(data$listing_id))
# mean(table(data$listing_id))

# p <- ggplot(subset(data3)) +
# geom_boxplot(aes(x=time, y=y, group = time)
# ,outlier.shape=NA
# )
# 
# p + geom_vline(xintercept=as.yearqtr("2014 07", "%Y %m"))

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


# RDD Analysis- code from: https://github.com/MatthieuStigler/RDDtools:
# data_rdd <- RDDdata(y = data3$adj_y, x = data3$q, cutpoint = 18)
# plot(data_rdd)
# 
# reg_para <- RDDreg_lm(RDDobject = data_rdd, order = 3)
# reg_para
# plot(reg_para)
# plotSensi(reg_para, from = -0.1, to = 0, by = 0.05)
# plotPlacebo(reg_para)
# dens_test(reg_para)
# 
# # bw_ik <- RDDbw_IK(data_rdd)
# 
# reg_nonpara <- RDDreg_np(RDDobject = data_rdd)
# print(reg_nonpara)
# plot(x = reg_nonpara, xlab="Time (Quarter- year)", ylab="word_count")
# plotSensi(reg_nonpara, device='base')
# plotPlacebo(reg_nonpara)
# dens_test(reg_nonpara)
data4 <- subset(data3, july_before_listing==0)

data_rdd_Z <- RDDdata(y = data3$adj_y, x = data3$q, 
                      covar = data3$count, cutpoint = 18)
covarTest_mean(data_rdd_Z)
plot(data_rdd_Z, device = "ggplot", h=1)

full_rdd <- RDDreg_lm(data_rdd_Z, covar.opt = 
                    list(strategy = "include",
                         slope = "separate"), order = 3)
summary(full_rdd)
coeftest(full_rdd, 
         vcov.=vcovCluster(full_rdd,
                           clusterVar=data3$listing_id)) 
# Ref:Wooldridge (2003) Cluster-sample methods in applied 
# econometrics. AmericanEconomic Review, 93, p. 133-138

plotPlacebo(full_rdd)
plotPlaceboDens(full_rdd, device ="ggplot")
plotSensi(full_rdd)

np_rdd <- RDDreg_np(data_rdd_Z) # Non-parametric RDD
summary(np_rdd)

# FE:
fixed <- plm(y ~ factor(max_after) + count + 
               factor(year),
             data=subset(data3, before_after==1),
             index=c("listing_id", "time"), model="within")
summary(fixed)
coeftest(fixed,vcov=vcovHC(fixed,type="HC0",cluster="group"))

## 2SLS:
tsls <- plm(y ~ factor(max_after) + count + 
              factor(year)| factor(max_after) + 
              mean_count_zip + factor(year), 
            data = subset(data3),
            index=c("listing_id", "time"), model="within")
summary(tsls)

# FE with Interactions:
summary(plm(y ~ factor(max_after) + 
                   factor(max_after) * review_scores_rating + 
                   count + 
               factor(year),
             data=subset(data3, before_after==1),
             index=c("listing_id", "time"), model="within"))

summary(plm(y ~ factor(max_after) + 
                   factor(max_after) * total_price + 
                   count + 
                   factor(year),
                 data=subset(data3, before_after==1),
                 index=c("listing_id", "time"), model="within"))

summary(plm(y ~ factor(max_after) + 
                   factor(max_after) * calculated_host_listings_count + 
                   count + 
                   factor(year),
                 data=subset(data3, before_after==1),
                 index=c("listing_id", "time"), model="within"))

summary(plm(y ~ factor(max_after) + 
              factor(max_after) * host_response_rate + 
              count + 
              factor(year),
            data=subset(data3, before_after==1),
            index=c("listing_id", "time"), model="within"))

### Other models:
random <- plm(y ~ factor(max_after) + count + 
                factor(year),
              data=subset(data3, before_after==1),
              index=c("listing_id", "time"), model="random")

phtest(fixed, random)

fixed_no_time <- plm(y ~ factor(max_after) + count,
             data=subset(data3, before_after==1),
             index=c("listing_id", "time"), model="within")

pFtest(fixed_no_time, fixed)
plmtest(fixed, c("time"), type="bp")

## Checking the instrumentl variable:
summary(plm(count ~  count_zip +
              factor(year),
            data=subset(data3, before_after==1),
            index=c("listing_id", "time"), model="within"))

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
pool <- plm(y ~ factor(max_after) +  count + factor(year),
            data=subset(data3),
            index=c("listing_id", "time"), model="pooling")
plmtest(pool, type=c("bp"))

### Test of cross-sectional dependence:
pcdtest(fixed, test = c("lm"))
pcdtest(fixed, test = c("cd"))

### Test of serial correlation:
pbgtest(fixed)


### Another way to do 2sls:
c_model <- plm(count ~ mean_count_zip + factor(year) + 
                 factor(max_after), data=data3,
               index=c("listing_id", "time"), model="within")
data3$pred_count <- as.numeric(c_model$model[[1]] - c_model$residuals) 

f <- plm(y ~ factor(max_after) + pred_count + 
           factor(year), data = data3,
         index=c("listing_id", "time"), model="within")
coeftest(f,vcov=vcovHC(f,type="HC0",cluster="group"))


## Aggregate Data (Before and After):
agg_data <- data3 %>%
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
