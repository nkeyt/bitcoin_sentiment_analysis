# Directories

getwd();
setwd("PracticeProjects");

# Knitr

require("knitr")
opts_knit$set(root.dir = '/Users/nathankeyt/Documents/PracticeProjects', include=FALSE)

# Packages

install.packages("tidyverse");
library(tidyverse);
library(lubridate);
library(dplyr);
library(ggplot2);
library(rtweet);
library(tidytext);
library(slam);
library(tm);
library(wordcloud);

# Data Cleaning

btcDataH = read.csv('Coinbase_BTCUSD_1h.csv', header = FALSE);
btcData = read.csv('Coinbase_BTCUSD_d_Cut.csv', header = FALSE);

summary(btcData);
head(btcData);
summary(btcDataD);
head(btcDataD);

# Callnames

colnames(btcData) = c("Timestamps", "Symbol", "Open", "High", "Low", "Close", "Volume_BTC", "Volume_USD");
colnames(btcDataD) = c("Timestamps", "Symbol", "Open", "High", "Low", "Close", "Volume_BTC", "Volume_USD");

# Timestamp Parsing

btcData$High = as.numeric(btcData$High);
btcData$Open = as.numeric(btcData$Open);
btcData$Low = as.numeric(btcData$Low);
btcData$Close = as.numeric(btcData$Close);
btcData$Volume_BTC = as.numeric(btcData$Volume_BTC);
btcData$Volume_USD = as.numeric(btcData$Volume_USD);

btcDataD$High = as.numeric(btcDataD$High);
btcDataD$Open = as.numeric(btcDataD$Open);
btcDataD$Low = as.numeric(btcDataD$Low);
btcDataD$Close = as.numeric(btcDataD$Close);
btcDataD$Volume_BTC = as.numeric(btcDataD$Volume_BTC);
btcDataD$Volume_USD = as.numeric(btcDataD$Volume_USD);

dateTest = ymd_h("2020-07-08 02-PM");

btcData = btcData %>%
  mutate(Timestamps_Parsed = ymd(btcData$Timestamps));

btcDataD = btcDataD %>%
  mutate(Timestamps_Parsed = ymd(btcDataD$Timestamps));


btcData$Symbol = NULL;

btcDataNA = btcData[-c(1, 2),];

head(btcDataNA);
summary(btcDataNA);


set.seed(22);
split <-  sample(x = 1:nrow(data),size = 0.8*nrow(data));
train <- btcDataNA[split,];
test <- btcDataNA[-split,];

nrow(train);
nrow(test);


start <- Sys.time();

twitterData <- get_timeline("@BTCTN", n = 5000, include_rts = FALSE);

end <- Sys.time();

end-start;

tail(twitterData);
summary(twitterData);

twitterDataClean <- twitterData %>%
  filter(is_retweet==FALSE) %>% #exclude retweets
  filter(is.na(reply_to_status_id)) #exclude replies

hashtags <- unlist(twitterDataClean$hashtags) #flattern nested lists 
hashtags <- hashtags[!is.na(hashtags)] #remove cases with no hashtags translated as NAs

ts_plot(twitterDataClean);

twitterDataCleanText <- twitterDataClean[, c('created_at', 'text')]
twitterDataCleanText %>%
  mutate(day = format(created_at, "%D")) %>%
  group_by(day) %>%
  summarise(total = n()) 

twitterDataCleanText <- twitterDataCleanText %>%
  mutate(day = format(created_at, "%D")) 

words <- twitterDataCleanText%>%
  select(day, text)%>%
  unnest_tokens(output = word,input = text)

words;

# Data Plots

ggplot(btcDataNA, aes(x = Timestamps_Parsed, y = Close, color = Sentiment_BTC)) + geom_point();

ggplot(btcDataNA, aes(x = Timestamps_Parsed, y = Close, color = Sentiment_BTC)) + geom_line();

ggplot(btcData, aes(x = Volume_BTC, y = Close)) + geom_point();

ggplot(btcData, aes(x = Timestamps_Parsed, y = Volume_BTC)) + geom_point();
ggplot(btcData, aes(x=Timestamps_Parsed)) + 
  geom_line(aes(y = Close), color = "darkred") + 
  geom_line(aes(y = Volume_BTC), color="steelblue", linetype="twodash") +
  xlab("Time") +
  ylab("Volume BTC");

ggplot(btcDataNA, aes(Timestamps_Parsed, Close)) + geom_boxplot();

install.packages("zoo");
library(zoo);
mov.avg <- rollmean(Close, 5, fill=NA)
plot(Timestamps_parsed, Close, "l")
lines(Timestamps_parsed, mov.avg, col="orange", lwd=2)

ggplot(btcDataD, aes(x=Timestamps_Parsed)) + 
  geom_line(aes(y = Close), color = "darkred") + 
  geom_line(aes(y = Volume_BTC), color="steelblue", linetype="twodash") +
  xlab("Concerta Peak1 Cmax Distribution") +
  ylab("Density");

# Linear Regression

train$Volume_USD = as.numeric(train$Timestamps_Parsed);

train$Timestamps_Parsed = as.numeric(train$Timestamps_Parsed)/(24*60*60);

lr_model1 <- lm(formula = Timestamps_Parsed ~ Close, data = train);
summary(lr_model);

lr_model2 <- lm(formula = Close ~ Volume_BTC, data = train);
summary(lr_model2);

lr_model3 <- lm(formula = Close ~ Sentiment_BTC, data = train);
summary(lr_model2);

lr_model3 <- lm(formula = Close ~ Sentiment_BTC * Count_BTC * Volume_BTC * Timestamps_Parsed, data = train);

summary(train);
head(train);

as.Date(predict(q, btcDataNA(sum=6000)), origin="2017-07-01 11:00:00");

par(mfrow=c(2,2));
plot(lr_model2);
par(mfrow=c(1,1));

par(mfrow=c(2,2));
plot(lr_model3);
par(mfrow=c(1,1));

predictions = predict(lr_model2, newdata = test);
predictions;

predictions = predict(lr_model3, newdata = test);
predictions;

results <- as.data.frame(cbind(test$Close, predictions));
colnames(results) <- c('true', 'predicted');

head(results);

ggplot(results, aes(true, predicted)) + 
  geom_point(color='#C70A62') + 
  geom_abline(slope=1, intercept = 0);

print(length(predictions));

btcDataNA = btcData[-c(3, 4),];

ggplot(btcDataNA, aes(x=Timestamps_Parsed)) + 
  geom_line(aes(y = Close), color = "darkred") + 
  geom_line(aes(y = Volume_BTC), color="steelblue", linetype="twodash") +
  geom_line(aes(y = predictions), color = "darkgreen") + 
  xlab("Time") +
  ylab("Volume BTC");

# Sentiment Analysis

bing <- get_sentiments('bing');
tail(bing);

words %>%
  inner_join(bing, by='word') %>%
  group_by(day, sentiment) %>%
  summarise(count=n());

wordsBing = inner_join(words, bing, by='word');
wordsGrouped = group_by(wordsBing, day, sentiment);
wordsSummarised = summarise(wordsGrouped, count=n());

colnames(wordsSummarised) = c("Days", "Sentiment", "Count");

wordsGrouped;
wordsSummarised;

nrow(wordsSummarised);
nrow(wordsNegative);
nrow(wordsPositive);

for (val in 1:nrow(wordsSummarised)) {
  if (val < 823) {
    if (wordsSummarised[val, ]$Days == wordsSummarised[val + 1, ]$Days) {
      if (wordsSummarised[val, ]$Count > wordsSummarised[val + 1, ]$Count) {
        wordsSummarised[val, ]$Count = wordsSummarised[val, ]$Count - wordsSummarised[val + 1, ]$Count;
        wordsSummarised[val + 1, ]$Sentiment = NA;
      }
      else if (wordsSummarised[val, ]$Count < wordsSummarised[val + 1, ]$Count) { 
        wordsSummarised[val + 1, ]$Count =wordsSummarised[val + 1, ]$Count - wordsSummarised[val, ]$Count;
        wordsSummarised[val, ]$Sentiment = NA;
      }
      else {
        wordsSummarised[val, ]$Count = 0;
        wordsSummarised[val, ]$Sentiment = "neutral";
        wordsSummarised[val + 1, ]$Sentiment = NA;
      }
    }
  }
  else {
    break;
  }
}

wordsSummarised = na.omit(wordsSummarised);

wordsSummarised$Sentiment[wordsSummarised$Sentiment == "negative"] = "0";
wordsSummarised$Sentiment[wordsSummarised$Sentiment == "positive"] = "2";
wordsSummarised$Sentiment[wordsSummarised$Sentiment == "neutral"] = "1";

wordsSummarised$Sentiment = as.numeric(wordsSummarised$Sentiment);

wordsSummarised;

view(wordsSummarised);
head(wordsSummarised);
tail(wordsSummarised);
summary(wordsSummarised);

ggplot(wordsSummarised, aes(x= Days)) + 
  geom_line(aes(y = Sentiment), color = "darkred") + 
  xlab("Time") +
  ylab("Sentiment");

wordsSummarised$Days = mdy(wordsSummarised$Days);

wordsSummarised$Days;

wordsSummarised = arrange(wordsSummarised, Days);

wordsSummarised = wordsSummarised %>% 
  map_df(rev)

wordsSummarised = wordsSummarised[-c(1, 2, 3, 4, 5, 6, 7, 8), ];

btcDataNA = btcDataNA %>%
  mutate(Sentiment_BTC = wordsSummarised$Sentiment);

btcDataNA = btcDataNA %>%
  mutate(Count_BTC = wordsSummarised$Count);

summary(btcDataNA);

nrow(btcDataNA);
nrow(wordsSummarised);

view(btcDataNA);
view(wordsSummarised);

date_range <- seq(min(wordsSummarised$Days), max(wordsSummarised$Days), by = 1);
missingDates = date_range[!date_range %in% wordsSummarised$Days]; 
date_range_BTC <- seq(min(btcDataNA$Timestamps_Parsed), max(btcDataNA$Timestamps_Parsed), by = 1);
missingDatesBTC = date_range[!date_range %in% btcDataNA$Timestamps_Parsed]; 

missingDates;
missingDatesBTC;

missingDates[1];
length(missingDates);

1:length(missingDates);

btcDataNA = btcDataNA[- grep(missingDates[36], btcDataNA$Timestamps_Parsed),];

wordsSummarised = wordsSummarised[- grep(missingDatesBTC[12], wordsSummarised$Days),];

wordsSummarisedSafe = wordsSummarised;
btcDataSafe = btcDataNA;
btcDataNASafe = btcDataNA;

btcDataNA = btcDataNASafe;

words %>%
  inner_join(bing, by='word') %>%
  group_by(sentiment) %>%
  summarise(count=n()) %>%
  ggplot(aes(x = sentiment, y = count)) + 
  geom_bar(stat = 'identity', fill=c('red', 'darkgreen')) +
  ggtitle('Polarity in Bitcoin News Tweets');

words;










