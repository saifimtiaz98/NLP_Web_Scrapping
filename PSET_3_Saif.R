
# Question 1 --------------------------------------------------------------


library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(sentimentr)
library(textdata)
install.packages('countrycode')
library(countrycode)

setwd('/Users/saifimtiaz/Desktop/PSET 3')
file_path <- "/Users/saifimtiaz/Desktop/PSET 3/vol101_1_publichealthroundup.txt"
text <- read_file(file_path)

text_df <- tibble(text = text)
word_tokens <- unnest_tokens(text_df, word_tokens, text, token = "words")

no_stop_words <- word_tokens %>% 
  anti_join(stop_words, by = c("word_tokens" = "word"))

# Sentiment Analysis using AFINN, NRC, and Bing
sentiment_afinn <- get_sentiments("afinn") %>% rename(afinn = value)
sentiment_nrc <- get_sentiments("nrc") %>% rename(nrc = sentiment)
sentiment_bing <- get_sentiments("bing") %>% rename(bing = sentiment)

# Joining the sentiment lexicons to the tokens
sentiment_df <- no_stop_words %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"), relationship = "many-to-many") %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"), relationship = "many-to-many")

ggplot(data = filter(sentiment_df, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "Sentiment Categories (NRC)", x = "Sentiment Category", y = "Count") +
  theme_minimal() +
  ggsave("question1_plot_sentiment_nrc.png")

# Overall sentiment score 
sentiment_summary <- data.frame(sentiment(sentiment_df$word_tokens))
sentiment_summary <- summarise(sentiment_summary, mean_sentiment = mean(sentiment, na.rm = TRUE))
print(sentiment_summary)

# Extracting country mentions
countries_list <- tolower(countrycode::codelist$country.name.en)
no_stop_words <- no_stop_words %>% mutate(word_tokens = tolower(word_tokens))

countries_mentioned <- no_stop_words %>%
  filter(word_tokens %in% countries_list) %>%
  group_by(word_tokens) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

if (nrow(countries_mentioned) > 0) {
  country_plot <- ggplot(countries_mentioned, aes(x = reorder(word_tokens, count), y = count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Countries Mentioned in the Article", x = "Country", y = "Count") +
    theme_minimal()
  ggsave("question1_plot_countries_mentioned.png", plot = country_plot)
} else {
  message("No countries were mentioned in the text.")
}


# Dependency parsing using udpipe to understand sentence context
ud_model <- udpipe_download_model(language = "english")
ud_model_path <- ud_model$file_model
ud_model <- udpipe_load_model(ud_model_path)
parsed_text <- udpipe(text, object = ud_model)

parsed_countries <- parsed_text %>%
  mutate(lemma = tolower(lemma)) %>%
  filter(lemma %in% countries_list) %>%
  select(doc_id, sentence_id, token, upos, lemma)

View(parsed_countries)

write_csv(parsed_countries, "parsed_countries_mentions.csv")

# README file description
#' The code reads the WHO Public Health Roundup text file, tokenizes it, and performs NLP to extract sentiment and country mentions.
#' Sentiment analysis is conducted using NRC, AFINN, and Bing lexicons. The outputs include summary statistics of sentiment and the number of times countries are mentioned.
#' Additionally, two visualizations are generated where one showing the sentiment distribution and one showing country mentions.

rm(list = ls()) 

#'The plot, which represents sentiment categories, shows various emotions expressed 
#'in the article.The dominant sentiments are fear, trust, positive, and anticipation 
#'which indicates a mixed sentiment, with a notable emphasis on both positive emotions 
#'like trust and anticipation, and negative emotions like fear. The presence of trust 
#'and positivity sjpws a focus on potential improvements or hope, while fear might 
#'reflect concerns about public health issues. The emotions like fear and trust might 
#'reflect the health challenges faced globally, as well as the actions being taken to 
#'address them. Anger and sadness are present but less dominant, which might indicate 
#'that the article acknowledges challenges but focuses more on actionable items and 
#'responses.
#'
#'In the Countries Discussed in the Article, the plot shows the countries most mentioned 
#'in the article. These include Uganda, Sudan, Switzerland, Oman, and Kenya. Uganda and 
#'Sudan are mentioned the most frequently, indicating that these countries are likely 
#'discussed in the context of recent public health efforts or crises. Switzerland might
#' be mentioned in a policy or administrative context, given its association with global
#'  health organizations like WHO.


# Question 2 --------------------------------------------------------------
library(rvest)
library(httr)
library(tidyverse)
library(tidytext)
library(sentimentr)

scrape_who_africa <- function(start_month, start_year) {
  base_url <- "https://www.afro.who.int/news/news-releases?page="
  articles <- list()
  current_date <- Sys.Date()
  
  for (page in 0:48) { # Iterate through pages
    url <- paste0(base_url, page)
    
    webpage <- read_html(url)
    
    # Extracting article metadata
    titles <- webpage %>% html_nodes(".teaser-full__title a") %>% html_text()
    dates <- webpage %>% html_nodes(".field--name-field-date time") %>% html_attr("datetime") %>% as.Date()
    links <- webpage %>% html_nodes(".teaser-full__title a") %>% html_attr("href")
    links <- ifelse(grepl("^http", links), links, paste0("https://www.afro.who.int", links)) # Fix incomplete links
    
    #tibble for the current page
    page_data <- tibble(
      title = titles,
      date = dates,
      link = links
    )
    
    # Filtering articles by date
    page_data <- page_data %>%
      filter(date >= as.Date(paste0(start_year, "-", start_month, "-01")) & date <= current_date)
    
    # Appending page data
    articles[[page + 1]] <- page_data
  }
  
  # Combining all pages into a single tibble, ensuring no duplicates
  bind_rows(articles) %>%
    distinct()
}

extract_clean_text <- function(article_links, output_dir) {
  dir.create(output_dir, showWarnings = FALSE) # Create the output directory if not exists
  text_data <- list()
  
  for (i in seq_along(article_links$link)) {
    full_url <- str_trim(article_links$link[i]) 
    full_url <- URLdecode(full_url) # Decoding URL-encoded characters
    
    # Skipping invalid URLs
    if (!grepl("^https://", full_url) || nchar(full_url) < 10) {
      message(paste("Skipping malformed URL at index", i, ":", full_url))
      next
    }
    
    tryCatch({
      article <- httr::GET(full_url)
      
      # Checking HTTP status
      if (httr::status_code(article) != 200) {
        message(paste("Skipping URL at index", i, "due to non-200 status code:", full_url))
        next
      }
      
      # Parsing HTML content
      article_html <- read_html(article)
      
      # Extracting and clean text
      text <- article_html %>%
        html_nodes(".content") %>%
        html_text() %>%
        str_squish()
      
      # Saving the text to a file
      file_name <- paste0(output_dir, "/article_", i, ".txt")
      writeLines(text, file_name)
      
      # Storing the data in a tibble
      text_data[[i]] <- tibble(
        title = article_links$title[i],
        date = article_links$date[i],
        text = text
      )
    }, error = function(e) {
      # Logging errors but continue processing
      message(paste("Error processing article at index", i, "with URL:", full_url, "\nError message:", e$message))
    })
  }
  
  # Combining all text data into a single tibble
  bind_rows(text_data)
}

# Updated Logging Code
log_file <- "url_debug_log.txt"
cat("Start of Log\n", file = log_file)

for (i in seq_along(news_data$link)) {
  full_url <- str_trim(news_data$link[i])
  full_url <- URLdecode(full_url)
  
  cat(paste("Index:", i, "URL:", full_url, "\n"), file = log_file, append = TRUE)
  
  tryCatch({
    # Testing the URL
    response <- httr::GET(full_url)
    if (httr::status_code(response) != 200) {
      cat(paste("Non-200 Status Code:", httr::status_code(response), "\n"), file = log_file, append = TRUE)
    } else {
      cat("Valid URL\n", file = log_file, append = TRUE)
    }
  }, error = function(e) {
    cat(paste("Error:", e$message, "\n"), file = log_file, append = TRUE)
  })
}


extract_clean_text <- function(article_links, output_dir) {
  dir.create(output_dir, showWarnings = FALSE) 
  text_data <- list()
  
  for (i in seq_along(article_links$link)) {
    full_url <- str_trim(article_links$link[i])
    full_url <- URLdecode(full_url) 
    
    # Skipping invalid URLs
    if (!grepl("^https://", full_url) || nchar(full_url) < 10) {
      message(paste("Skipping malformed URL at index", i, ":", full_url))
      next
    }
    
    tryCatch({
      # Fetching the page content
      article <- httr::GET(full_url)
      
      # Checking HTTP status
      if (httr::status_code(article) != 200) {
        message(paste("Skipping URL at index", i, "due to non-200 status code:", full_url))
        next
      }
      
      # Parsing HTML content
      article_html <- read_html(article)
      
      text <- article_html %>%
        html_nodes(".content") %>%
        html_text() %>%
        str_squish()
      
      # Saving the text to a file
      file_name <- paste0(output_dir, "/article_", i, ".txt")
      writeLines(text, file_name)
      
      # Storing the data in a tibble
      text_data[[i]] <- tibble(
        title = article_links$title[i],
        date = article_links$date[i],
        text = text
      )
    }, error = function(e) {
      # Log errors but continue processing
      message(paste("Error processing article at index", i, "with URL:", full_url, "\nError message:", e$message))
    })
  }
  
  # Combining all text data into a single tibble
  bind_rows(text_data)
}




news_data <- scrape_who_africa(start_month, start_year)
text_data <- extract_clean_text(news_data, output_dir)


View(news_data)
start_month <- 9
start_year <- 2023
output_dir <- "who_africa_texts"

# Sentiment Analysis

sentiment_results <- text_data %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(title, date, sentiment, sort = TRUE)

# Sentiment by country (example: Ghana)
country_sentiment <- text_data %>%
  filter(str_detect(text, "Ghana")) %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment, sort = TRUE)

# Plotting Sentiments
# Overall Sentiment Plot
sentiment_plot <- ggplot(sentiment_results, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Overall Sentiment in WHO Africa News Releases", x = "Sentiment", y = "Count") +
  theme_minimal()

ggsave("question2_plot_overall_sentiment.png", plot = sentiment_plot)

# Sentiment about Ghana

ghana_sentiment_plot <- ggplot(country_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment About Ghana in WHO Africa News Releases", x = "Sentiment", y = "Count") +
  theme_minimal()

ggsave("question2_plot_ghana_sentiment.png", plot = ghana_sentiment_plot)


write_csv(news_data, "news_articles_metadata.csv")
write_csv(text_data, "news_articles_text.csv")
write_csv(sentiment_results, "sentiment_analysis_results.csv")
write_csv(country_sentiment, "ghana_sentiment_results.csv")









