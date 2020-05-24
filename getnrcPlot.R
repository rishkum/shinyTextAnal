articlewords <- function(url) {
  
   dat.merged <- cleanText(url)
    #Get the senteces 
    article_sentences <- tibble(text = dat.merged$value) %>%
      unnest_tokens(value, text, token = "sentences") %>%
      mutate(sentence_id = row_number()) %>%
      select(sentence_id, sentence=value)
    
    #Convert it into tokens
    article_words <- article_sentences %>%
      unnest_tokens(word, sentence)
    
    #Remove stop words
    article_words <- article_words %>%
      anti_join(stop_words, by = "word")
    
    return(article_words)
}

getNRCplot <- function(url) {
  
    article_words <- articlewords(url)
    # Attach the NRC sentiments
    library(textdata)
    article_words <- article_words %>%
    inner_join(get_sentiments("nrc"))
    
    
    #get a plot of NRC sentiments
    nrc_plot <- article_words %>%
      group_by(sentiment) %>%
      summarise(word_count = n()) %>%
      ungroup() %>%
      mutate(sentiment = reorder(sentiment, word_count)) %>%
      #Use `fill = -word_count` to make the larger bars darker
      ggplot(aes(sentiment, word_count, fill = -word_count)) +
      geom_col() +
      guides(fill = FALSE) + #Turn off the legend
      labs(x = NULL, y = "Word Count") +
      scale_y_continuous(limits = c(0, 150)) + 
      coord_flip()#Hard code the axis limit
      labs(title = "Sentiment shown in the document")
      
    
    return(nrc_plot)
}
