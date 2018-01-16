# Based on http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html

library(tm)
library(topicmodels)
library(data.table)
library(slam)
library(Rmpfr)
library(ggplot2)

getwd()
setwd("~/Dropbox (UNC Charlotte)/Kexin_Reza/Sharing Economy/")

data <- fread("sentiment_subjectivity_emotion_data_sf.csv")
data <- as.data.frame(data)
data$id <- row.names(data)
# data <-data[c("listing_id","id","date","reviewer_id","reviewer_name","clean_comments",
#               "sentiment","word_count","subjectivity")]

# Removing names from the data:
library(gender)
# install_genderdata_package()

sets <- data(package = "genderdata")$results[,"Item"]
data(list = sets, package = "genderdata")
stopwords <- unique(kantrowitz$name)

removeWords <- function(txt, words, n = 30000L) {
  l <- cumsum(nchar(words)+c(0, rep(1, length(words)-1)))
  groups <- cut(l, breaks = seq(1,ceiling(tail(l, 1)/n)*n+1, by = n))
  regexes <- sapply(split(words, groups), function(words) sprintf("(*UCP)\\b(%s)\\b", paste(sort(words, decreasing = TRUE), collapse = "|")))
  for (regex in regexes)  txt <- gsub(regex, "hostx", txt, perl = TRUE, ignore.case = TRUE)
  return(txt)
}
data$clean_noname <- removeWords(data$clean_comments, stopwords)

# fwrite(data,"noname_data_nash.csv", row.names=F)
colnames(data)

m <- list(content = "clean_noname", old_text = "clean_comments", listing_id = "listing_id", 
          date = "date", anger = "anger", anticipate = "anticipate", disgust = "disgust",
          fear = "fear", joy = "joy", sad = "sad", surprise = "surprise", trust = "trust",
          negative = "negative", positive = "positive", sentiment = "sentiment", 
          word_count = "word_count", subjectivity = "subjectivity", id = "id")

abb_corpus <- tm::Corpus(tm::DataframeSource(data), 
                         readerControl = list(reader = tm::readTabular(mapping = m)))

abb_corpus.dtm <- tm::DocumentTermMatrix(abb_corpus, 
                                        control = list(minWordLength = 2, 
                                                       removeNumbers = TRUE))

term_tfidf <- tapply(abb_corpus.dtm$v/slam::row_sums(abb_corpus.dtm)[abb_corpus.dtm$i], 
                     abb_corpus.dtm$j, mean) *
  log2(tm::nDocs(abb_corpus.dtm)/slam::col_sums(abb_corpus.dtm > 0))
summary(term_tfidf)

## Keeping the rows with tfidf >= to the 0.32800
abb_reduced.dtm <- abb_corpus.dtm[,term_tfidf >= 0.33]
summary(slam::col_sums(abb_reduced.dtm))

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# Determine k using harmonic mean:
## Remove empty rows:

ui = unique(abb_reduced.dtm$i)
dtm.new = abb_reduced.dtm[ui,]

k <- 25
burnin <- 1000
iter <- 1000
keep <- 50
fitted <- topicmodels::LDA(dtm.new, k = k, method = "Gibbs",
                           control = list(burnin = burnin, iter = iter, keep = keep) )
## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 25 topics.
harmonicMean(logLiks)

# Now get the log-likelihood for each k:
seqk <- seq(10, 100, 10)
burnin <- 1000
iter <- 1000
keep <- 50
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm.new, k = k,
                                                                     method = "Gibbs",control = list(burnin = burnin,
                                                                                                     iter = iter, keep = keep) )))
# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') 

ldaplot
seqk[which.max(hm_many)]

system.time(abb.model <- topicmodels::LDA(dtm.new, 30, method = "Gibbs", control = list(iter=2000, seed = 0622)))

#
topicProbabilities <- as.data.frame(abb.model@gamma)
# df <- posterior(abb.model, dtm.new)

abb.topics <- topicmodels::topics(abb.model, 3)
## In this case I am returning the top 10 terms.
abb.terms <- as.data.frame(topicmodels::terms(abb.model, 100), stringsAsFactors = FALSE)
abb.terms[16]
write.csv(abb.terms,"lda_R_topics_sf.csv",row.names = F)

# Creates a dataframe to store the most likely topics
doctopics.df <- as.data.frame(abb.topics)
# doctopics.df <- dplyr::transmute(doctopics.df, 
#                                  id = rownames(doctopics.df), topic = abb.topics)
doctopics.df <- as.data.frame(t(doctopics.df))
doctopics.df <- tibble::rownames_to_column(doctopics.df, "id")
doctopics.df$id <- as.integer(doctopics.df$id)
data$id <- as.integer(data$id)
names(doctopics.df) <- c("id","top1", "top2", "top3")

## Adds topic number to original dataframe of lessons
data2 <- dplyr::inner_join(data, doctopics.df, by = "id")
write.csv(data2,"sentiment_subject_3topics_sf.csv",row.names = F)
