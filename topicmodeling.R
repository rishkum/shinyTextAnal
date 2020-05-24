#GET LDA TOPICS

showTopics <-  function(url) {
article_words <- articlewords(url)
# Find the uniquess in the document
word_summary <- article_words %>% 
  anti_join(stop_words) %>% 
  count(sentence_id, word, sort =T) 

# Topic Modellinh #######
library(topicmodels)
library(SnowballC)
sentence_dtm <- word_summary %>%
  cast_dtm(sentence_id, word, n )

sentence_lda <- LDA(sentence_dtm, k = 4, control = list(seed = 1234))
sentence_lda

sentence_topics <- tidy(sentence_lda, matrix = "beta")
sentence_topics


top_terms <- sentence_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


library(ggplot2)

plot <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  ggtitle("LDA based topics in the speech") +
  scale_x_reordered()
return(plot)

}
