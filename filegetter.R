

fileGetter <- function(url) {
  
  my_urls <- c(as.character(url))

  #declare saving name
  save_here <- paste0("document", ".pdf")

  #run loop: useful for multiple files
  mapply(download.file, my_urls, save_here)
  file <- "document.pdf"
  #Load file as txt
  df <- pdf_text(pdf = "document.pdf")
# 
#   text_df <- tibble( text = df)
#   text_df <- text_df %>% rownames_to_column('sections') %>% filter(!(sections == 1|
#                                                                      sections == 7|
#                                                                      sections == 9)) 
#   
#   text_df <- text_df %>%
#     unnest_tokens(word, text)
#   
  #Load data properly
  text_df <- pdf_text("document.pdf") %>% strsplit(split = "\n")
  text_df <- text_df[2:(length(text_df)-2)]
  text_df <- unlist(text_df)
  text_df <- trimws(text_df, "l")
  text_df <- as_tibble(text_df)
  
  # remove page numers and footers
  text_df$rem <- grepl("^[0-9]+$", text_df$value)
  # remove page citations
  text_df$rem2 <- grepl("\\(.*\\)$", text_df$value)
  # remove headings
  text_df$rem3 <- grepl("^[XVI]+[.XVI]", text_df$value)
  #Remove speeh links
  text_df$rem4 <- grepl("/(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", text_df$value, TRUE)
  text_df <- text_df %>% filter(!(rem==T| rem2==T | rem3 == T| rem4 == T))
  
  dat.merged <- text_df %>%
    dplyr::group_by(rem) %>%
    dplyr::summarise(value = paste(value, collapse = " "))
  print(head(dat.merged))
  
  top_3 = lexRankr::lexRank(dat.merged$value,
                            #only 1 article; repeat same docid for all of input vector
                            docId = rep(1, length(dat.merged$value)),
                            #return 3 sentences to mimick /u/autotldr's output
                            n = 5,
                            continuous = TRUE)
  
  #reorder the top 3 sentences to be in order of appearance in article
  order_of_appearance = order(as.integer(gsub("_","",top_3$sentenceId)))
  #extract sentences in order of appearance
  ordered_top_3 = top_3[order_of_appearance, "sentence"]
  ordered_top_3
  
  
  library(tidytext)
  library(textrank)
  
  
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
  
  # Apply Text Rank algorithm to get the top 5 sentences
  article_summary <- textrank_sentences(data = article_sentences, 
                                        terminology = article_words)
  
  article_summary <- as.tibble(summary(article_summary, n = 5, keep.sentence.order = T))
  article_summary <- article_summary %>% rename(`Text Rank Output` = value)
  
  #Function to sentence case the sentences
  firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  
  article_summary$`Text Rank Output` <- firstup(article_summary$`Text Rank Output`)
  return(article_summary)
}
