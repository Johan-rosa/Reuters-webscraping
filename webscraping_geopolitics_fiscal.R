# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(rvest)

# webscraping tools ----------------------------------------------------------------

# Function to get individuals news url --- ---- 

get_url <- function(url, topic) {
  
  if(topic == "geopolitics"){
    
    read_html(url) %>%
      html_nodes(".article-title a") %>%
      html_attr("href")
    
  } else if (topic == "fiscalpolicy") {
    read_html(url) %>%
      html_nodes(".published+ .published h2 a") %>%
      html_attr("href") %>%
      unique() %>%
      paste0("https://theconversation.com", .)
  } else if (topic == "tradewar") {
    
  }
  
}


# Function to get the text and date of the news --- ----
  
get_text <- function(url, topic){
    
    # if geopolitics 
    if(topic == "geopolitics") {
     
      # Reading the news page
       html <- read_html(url)
      
      # Extracting the date 
      date <- html %>%
        html_nodes(".article-single .article-date") %>%
        html_text()
      
      # Extracting the news body  
      body <- html %>%
        html_nodes(".article-body") %>%
        html_text() %>%
        str_remove_all("\n") %>%
        paste(colapse = " ")
      
      # header
      header <- html %>%
        html_nodes(".article-single .article-title") %>%
        html_text()
      
      # bind all elements in a dataframe 
      df <- data.frame(
        date = date,
        header = header,
        text = body)
      
      return(df)
      
    } else if (topic == "fiscalpolicy") {
      
      html <- read_html(url)
      
      date <- html %>%
        html_nodes("time") %>%
        html_text()
      
      header <- html %>%
        html_nodes("strong") %>%
        html_text() %>%
        str_remove_all("\n") %>%
        paste(collapse = " ")
      
      body <- html %>%
        html_nodes(".instapaper_body p") %>%
        html_text() %>%
        str_remove_all("\n") %>%
        paste(collapse = " ")
      
      df <- data.frame(
        date = date,
        header = header,
        text = body
      )
      
      return(df)
    }
    
  }



# Geopolitical News webscraping ----------------------------------------------------

# url to general wb --- ---
geopolitics_url <- paste0(
  "https://www.geopoliticalmonitor.com/page/",
    1:235, 
  "/?s&section&topic&start&end&region&country&advanced"
) 



# Extracting individual news --- ---

geopolitics_news_url <- list()
  
for (i in seq_along(geopolitics_url)) { # Quedó en 151
 
    geopolitics_news_url[[i]] <- get_url(geopolitics_url[i], topic = "geopolitics")
  
  print(i) 
  
  if(i %in% seq(50, 300, by = 50)) {saveRDS(geopolitics_news_url, "geopolitics_url.RDS")}
}

geopolitics_news_url_ <- unlist(geopolitics_news_url) %>%
  unique()

# getting the news --- --- --- 
geopolitics_news <- list()

for (i in 235:length(geopolitics_news_url_)) {
  geopolitics_news[[i]] <- get_text(geopolitics_news_url_[i+4], "geopolitics")
  
  if(i %in% seq(100, 3000, by = 100)) {write_rds(geopolitics_news, "geopolitics_news.RDS")}
  print(i)
}


geopolitics_news <- bind_rows(geopolitics_news)

write_csv(geopolitics_news, "data/geopolitics_news.csv")

# Fiscal news webscraping ----------------------------------------------------------

# URL to general wp --- ---
fiscal_url <- paste0(
  "https://theconversation.com/us/topics/fiscal-policy-973?page=",
  1:3
)

fiscal_url_2 <- paste0(
  "https://theconversation.com/us/search?adapter=pg3&date=all&language=en&page=",
  1:10,
  "&q=fiscal&sort=relevancy"
)

# Extracting individuals news url --- ---


fiscal_news_url <- list()

for (i in seq_along(fiscal_url)) {
  
  fiscal_news_url[[i]]  <- get_url(fiscal_url[i], "fiscalpolicy") 
  print(i)
  
}


fiscal_news_url <- unlist(fiscal_news_url) %>%
  unique()

# getting the news --- --- ---

fiscal_news <- list()

for (i in seq_along(fiscal_news_url)) {
  
  fiscal_news[[i]] <- get_text(fiscal_news_url[i], "fiscalpolicy")
  print(i)
}

fiscal_news <- bind_rows(fiscal_news)



write_csv(fiscal_news, "fiscal_news.csv")
write_csv(geopolitics_news, "geopolitics_news.csv")


