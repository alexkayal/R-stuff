library(twitteR)
url <- "http://www.rdatamining.com/data/rdmTweets-201306.RData"
download.file(url, destfile = "~/Desktop/rdmTweets-201306.Rdata")
load (file ="~/Desktop/rdmTweets-201306.Rdata")
n.tweets <- length(tweets)
tweets.df <- twListToDF(tweets)
dim(tweets.df)

library(tm)
myCorpus <- Corpus(VectorSource(tweets.df$text))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- setdiff(myStopwords, c("r","big"))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpusCopy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)

for (i in c(1:2, 320)){
  cat(paste0("[",i,"]"))
  writeLines(strwrap(as.character(myCorpus[[i]]),60))
  
}
a <- ts(1:30, frequency=12, start=c(2011,3))
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)

U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2))
legend("topleft", c("Actual", "Forecast", "Error Bounds (95% Confidence)"), col=c(1,2,4), lty=c(1,1,2))
