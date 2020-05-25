library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)

getTweetPlot <- function(key) {
#Setup Twitter
setup_twitter_oauth(consumer_key = 'rFso4eDsm9ZnSnPq2olxXiqnO',
                    consumer_secret = 'DibqYFs3aAA53K65OVN6A1MLGQqHTTVISEqoMHNqIS3JnaM6Yv',
                    access_token = '2961659266-DeKk1fuB5K4Um7LSbPmegIBqLCGnHqTWmwssERT',
                    access_secret = '9C35XgBwdGEqyYhWP3KdnGO1P0RbKxOmx8dtBBBXSDpQT')



#Set criteria and get tweets
numberOfTweets <- 750
#Scrape tweets containing "#jeffBezos" and "@jeffBezos"
tweets <- searchTwitter(searchString=key, n = numberOfTweets, lang="en")

tweetsDF <- twListToDF(tweets)

#Convert to dataframe and encode to native
x <- tweetsDF
x$text <- enc2native(x$text)

#Clean text
x$text <- gsub("^[[:space:]]*","",x$text) # Remove leading whitespaces
x$text <- gsub("[[:space:]]*$","",x$text) # Remove trailing whitespaces
x$text <- gsub(" +"," ",x$text) #Remove extra whitespaces
x$text <- gsub("'", "%%", x$text) #Replace apostrophes with %%
x$text <- iconv(x$text, "latin1", "ASCII", sub="") # Remove emojis
x$text <- gsub("<(.*)>", "", x$text) #Remove Unicodes like <U+A>
x$text <- gsub("\\ \\. ", " ", x$text) #Replace orphaned fullstops with space
x$text <- gsub("  ", " ", x$text) #Replace double space with single space
x$text <- gsub("%%", "\'", x$text) #Change %% back to apostrophes
x$text <- gsub("https(.*)*$", "", x$text) #Remove tweet URL
x$text <- gsub("\\n", "-", x$text) #Replace line breaks with "-"
x$text <- gsub("--", "-", x$text) #Remove double "-" from double line breaks
x$text <- gsub("&amp;", "&", x$text) #Fix ampersand &
x$text[x$text == " "] <- "<no text>"

for (i in 1:nrow(x)) {
  if (x$truncated[i] == TRUE) {
    x$text[i] <- gsub("[[:space:]]*$","...",x$text[i])
  }
}

#Select desired column
cleanTweets <- x %>% 
  select("text")


#Analyze sentiment
sentiment <- analyzeSentiment(cleanTweets)
#Extract dictionary-based sentiment according to the QDAP dictionary
sentiment2 <- sentiment$SentimentQDAP
#View sentiment direction (i.e. positive, neutral and negative)
sentiment3 <- convertToDirection(sentiment$SentimentQDAP)

#Extract and convert 'date' column
date <- x$created
date <- str_extract(date, "\\d{4}-\\d{2}-\\d{2}")
date <- as.Date(date)
date <- as.Date(date, format = "%m/%d/%y")

#Create new dataframe with desired columns
df <- cbind(cleanTweets, sentiment2, sentiment3, date)
#Remove rows with NA
df <- df[complete.cases(df), ]


#Calculate the average of daily sentiment score
df2 <- df %>% 
  group_by(date) %>%
  summarize(meanSentiment = mean(sentiment2, na.rm=TRUE))


DT::datatable(df2, editable = TRUE)


#Get frquency of each sentiment i.e. positive, neutral, and negative  
freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

#Convert data from long to wide
freq2 <- freq %>% 
  spread(key = sentiment3, value = Freq)

DT::datatable(freq2, editable = TRUE)


tweetplot <- ggplot() + 
  geom_bar(mapping = aes(x = freq$date, y = freq$Freq, fill = freq$sentiment3), stat = "identity") +
  ylab("Sentiment Frequency") + labs(title=paste0('Twitter sentiments over time for ', key))+
  xlab('Date') + theme_bw() +guides(fill=guide_legend(title="Sentiments"))
return(tweetplot)
}
