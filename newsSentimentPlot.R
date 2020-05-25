news <- function(term = "Hot") {
  term = "Hot"
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-US&gl=US&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link,
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text()
    
  )
  
  news_dat$time = html_dat %>% html_nodes("div article div div time") %>%
    html_text()
  
  return(news_dat)
}

getnewsSentiment <- function(key){

library(rvest)
library(stringr)
library(magrittr)


df.news <- news(key)
df.news$text <- df.news$Description

x <- df.news

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

#Select desired column
cleannews <- x %>% 
  select("text")


#Analyze sentiment
sentiment <- analyzeSentiment(cleannews)
sentiment2 <- sentiment$SentimentQDAP
#View sentiment direction (i.e. positive, neutral and negative)
sentiment3 <- convertToDirection(sentiment$SentimentQDAP)

library(zoo)

as.Date(0)
#Extract and convert 'date' column
date <- x$time
date <- ifelse( grepl("minutes", date, fixed = TRUE), as.Date(Sys.Date(), "%b %d %Y") , 
                ifelse( grepl("hours", date, fixed = TRUE), as.Date(Sys.Date(), "%b %d %Y") ,
                                ifelse( grepl("days", date, fixed = TRUE), as.Date(Sys.Date() - as.numeric(gsub(" days ago", "",date),"%b %d %Y") ),date)))

date <- ifelse(grepl("Apr", date, fixed = TRUE), paste(date, "2020"),
                               ifelse(grepl("Mar", date, fixed = TRUE), paste(date, "2020"),
                                      ifelse(grepl("Jan", date, fixed = TRUE), paste(date, "2020"),date)))

date <- ifelse( grepl("Yesterday", date, fixed = TRUE), as.Date(Sys.Date()-1, "%b %d %Y"),date)

library(lubridate)
date <- ifelse( grepl(" 2020", date, fixed = TRUE), as.Date(date, format='%b %d %Y'),date)
date <- as.Date(as.numeric(date), origin = "1970-01-01")


#Create new dataframe with desired columns
df <- cbind(cleannews, sentiment2, sentiment3, date)
#Remove rows with NA
df <- df[complete.cases(df), ]


freq <- df %>% 
  group_by(date,sentiment3) %>% 
  summarise(Freq=n())

newsplot <- freq %>% filter(as.Date(date) > as.Date("2020-05-01")) %>%  ggplot() + 
  geom_bar(mapping = aes(x = date, y = Freq, fill = sentiment3), stat = "identity") +
  ylab("Sentiment Frequency") + labs(title=paste0('News Sentiments over time for ', key))+
  xlab('Date') + theme_bw() +guides(fill=guide_legend(title="Sentiments"))

return(newsplot)

}



