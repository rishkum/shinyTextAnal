cleanText <- function(url) {
  
  my_urls <- c(as.character(url))
  
  #declare saving name
  save_here <- paste0("document", ".pdf")
  
  #run loop: useful for multiple files
  mapply(download.file, my_urls, save_here)
  file <- "document.pdf"
  #Load file as txt
  df <- pdf_text(pdf = "document.pdf")
  
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
      dplyr::summarise(value = paste(value, collapse = ""))
    return(dat.merged)
}
