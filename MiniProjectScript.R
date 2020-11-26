#Mini Project by Ben Cook

rm(list=ls()) #clears environment after every run

#getwd()
#setwd("/Users/bencook/Desktop/BTMA 431/MiniProject")
#getwd()

#librarys

#install.packages("rtweet")
#install.packages("plotly")
library(plotly) #used for data visualization
library(rtweet) #used for data gathering
library(dplyr)
library(quantmod)
library(lubridate)

tweetDF <- get_timelines("elonmusk",n = 30000) #fetches the last ~3200 tweets (max that Twitter allows, approximately 10 months of data)
#View(tweetDF)

#function for stripping off the date of a Tweet from a datetime string
getDatesFromDateTime <- function(DateTimeInput){
  return(substr(min(DateTimeInput),1,10))
}

#Strip off times in the created at column
tweetDF$created_at <-sapply(tweetDF$created_at, getDatesFromDateTime)
#strip all unused columns other than than date and text
tweetDF <- tweetDF %>%
  select(created_at,text)
#View(tweetDF)

#uses Quantmod to return a dataframe of Tesla Stock data between the oldest and newest date
#Note: there are gaps in this data as TSLA stock does not trade every day
StockData <- as.data.frame(getSymbols(Symbols = "TSLA", src = "yahoo",
                         from = min(tweetDF$created_at), to = max(tweetDF$created_at), env = NULL ))

#View(StockData)
#this list contains the dates where stock data is reported
DateList <- rownames(StockData)
#DateList

#filter out tweets that fall on days that the stock price is not reported
#this includes todays date as no financial information is reported today yet
tweetDF <- tweetDF[tweetDF$created_at %in% DateList,]
#View(tweetDF)

#adds a daily return column based on closing price - used for determining how the stock price increased or decreased
todaysPrice <- (StockData$TSLA.Close)[2:nrow(StockData)]
yesterdaysPrice <- (StockData$TSLA.Close)[1:nrow(StockData)-1]
dailyReturn <- c(NA,(todaysPrice - yesterdaysPrice)/yesterdaysPrice)
StockData <- data.frame(StockData,dailyReturn)
#View(StockData)

#Question 1 - Frequency of Tweets

frequencyList <- numeric(length(DateList)) #initalize a list of frequencies
#loop through the dates of tweets and increment the corresponding index in the frequency list
#when a date is given
for(i in 1:length(tweetDF$created_at)){
  frequencyIndex <- which(DateList == tweetDF$created_at[i]) #finds the index in the datelist that matches
  frequencyList[frequencyIndex] <- frequencyList[frequencyIndex] + 1 #increments the frequencylist in the corresponding index
}

outputDF <- data.frame(DateList,frequencyList, StockData$dailyReturn)
#View(outputDF)

#regressional model
frequencyFit <- lm(outputDF$StockData.dailyReturn ~ outputDF$frequencyList)
summary(frequencyFit)

#so According to the P-value, the frequency of tweets 
#does not significantly impact the daily return of Tesla Stock, p-value 0.0507
#Scatterplot: for frequency
xfreq <- list(title = "Number of Tweets Per Day")
yfreq <- list(title = "Change in Stock Price Per Day (USD)")
freqfig <- plot_ly(data = outputDF, x = ~frequencyList,y = ~dailyReturn) %>%
  layout(title = "Tesla Daily Return vs Tweet Frequency",
         xaxis = xfreq, yaxis = yfreq)
freqfig

#----------------------------------------#
#QUESTION 2 - Content of Tweets

#To analyze the content of Tweets we will use hueristics to determine if a Tweet should be considered a "meme" or "professional"
#to do this I manually take a list of words, phrases and users that would indicate if a Tweet is considered a meme
#NOTE: this is NOT a foolproof way as many Tweets will be misclassified, however this is a quick method of estimating
#The only true way to classify these tweets definitively is to create an artificial intelligence program capable of understanding
#sarcasm with 100% accuracy (which is currently not possible)

#lowercase the content of each tweet to avoid having to search for meme-words multiple times due to capitalization
tweetDF$text <- tolower(tweetDF$text)

#list of "meme-indicating" words
#this list was created by manually scanning through the Tweets and picking out words or "sub-words" that commonly appeared in joke tweets
#note that some words must have spaces inserted within them to enforce them being seperate words rather than a prefix or suffix
#(buff for example can be a prefix to "buffer" which is not a meme, but alone it likely is as it refers to a video game term)
memeWords <- c("lol", "haha", "teslaquila", "pewdiepie", "youtube", "meme", " fart ","snoopy","prophecy","xbox", "what is love",
               "overwatch", " buff ", "turret", "goosebumps", "lotr","naughty", "starman", "sex", "game","paintball",
               "boring","flamethrower","speedrun","parrot"," hug ","gary the snail"," lego ","butt","candy","metaphor for life",
               "uh oh"," techno "," elevator music ","panzer", "evelvator music"," aliens ","lover","sense of humor",
              " sperm "," joke "," hulk "," doge ","porn","tequila","420","cringe","4/20","69"," rave ","gta",
               "dick", "party", " catapults ","episode","guillotine","martini","idiot","smōl","anime","deus ex", "rhyme", "autocorrect",
               "coachella", "vibe","flextape","vampire","sigh","tiesto","lyrics","song", " emo ","sinner","fluffy", "pls",
              "the simulation", "combat","jack in the box")

#for each tweet, if the text contains at least one of these words, then the tweet will be considered a "meme", (value of 1) otherwise it will be "professional (0)
#put a 1 or 0 into this list and add it as a new column to the tweetDF
isMemeTweetList <- numeric(length(tweetDF$text))
#isMemeTweetList

for(i in 1:length(tweetDF$text)){
   for(j in 1:length(memeWords)){
     if(grepl(memeWords[j],tweetDF$text[i],fixed = TRUE)){
       isMemeTweetList[i] <- 1
     }
   }
}
#isMemeTweetList
#add this data to the tweet dataframe
tweetDF <- tweetDF %>%
  mutate(isMemeTweetList)
#View(tweetDF)

numMemeTweet <- numeric(length(DateList)) #initalize a list of memetweet

for(i in 1:length(tweetDF$created_at)){
  numMemeIndex <- which(DateList == tweetDF$created_at[i]) #finds the index in the datelist that matches
  numMemeTweet[numMemeIndex] <- numMemeTweet[numMemeIndex] + tweetDF$isMemeTweetList[i] #increments the memetweetlist ONLY if the tweet is classed as a "meme"
}
#numMemeTweet
outputDF <- outputDF %>%
  mutate(numMemeTweet)

#View(outputDF)

#regression model for memes
memeFit <- lm(outputDF$StockData.dailyReturn ~ outputDF$numMemeTweet)
summary(memeFit)
#So according to the p-value, there is not a significant relationships between daily stock return and 

#Bar chart: for number of meme tweets
xmeme <- list(title = "Number of 'Meme' Tweets Per Day")
ymeme <- list(title = "Change in Stock Price Per Day (USD)")
memefig <- plot_ly (data = outputDF, x = ~numMemeTweet, y = ~dailyReturn, type = "bar") %>%
  layout(title = "Tesla Daily Return vs Meme Tweet Frequency",
         xaxis = xmeme, yaxis = ymeme)
memefig
#extra analysis and graphs

#Since both were rejected, I decided to make a combined regressional model
#to see if together they will have a statistical impact
combinedFit <- lm(outputDF$StockData.dailyReturn ~ outputDF$numMemeTweet + outputDF$frequencyList)
summary(combinedFit)
#according to the combined regressional model, there is still no impact

#combined 3D scatterplot
xcombined <- xmeme
ycombined <- xfreq
zcombined <- ymeme
combinedfig <- plot_ly(data = outputDF, x = ~numMemeTweet, y = ~frequencyList, z = ~dailyReturn) %>%
  layout(title = "Combined 3-D Plot",scene = list(
         xaxis = xcombined, zaxis = zcombined, yaxis = ycombined))
combinedfig

print(paste("Correlation between number meme tweets and tweet frequency is",
            cor(outputDF$numMemeTweet,outputDF$frequencyList)))

TRUE * TRUE
