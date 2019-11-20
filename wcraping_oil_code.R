## Datos generales -----------------------------------------------------------------

    # Autor : Johan Rosa
    # Fecha : Agosto 2019
    # Objetivo : Descargar noticias OilPrice.com. 

# Paquetes -------------------------------------------------------------------------
library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)


# Url generales --------------------------------------------------------------------

paginas_generales <- paste0(
  "https://oilprice.com/Latest-Energy-News/World-News/Page-",
  1:1011,".html")


# Funciones para hacer el webscraping ----------------------------------------------

# Función para extraer los url de las noticias individuales

get_news_url <- function(url) {
  
  read_html(url) %>%
    html_nodes('.categoryArticle, a') %>%
    html_attr('href') %>%
    unique() %>%
    str_subset(fixed("News")) %>%
    str_subset(fixed("Page-"), negate = TRUE) %>%
    str_subset("News$", negate = TRUE)
    
}


# Función para extraer el texto de la noticia y la fecha

get_news_text <- function(url) {

news_html <- read_html(url)

header <- news_html %>%
  html_node("h1") %>% 
  html_text()
    
date <- news_html %>%
    html_node('.article_byline') %>%
    html_text() %>%
    str_remove("^.*- ")  

text <- news_html %>%
  html_nodes("#news-content p") %>%
  html_text() %>%
  paste0(collapse = " ") %>%
  str_trim()

news <- cbind(date, text) %>%
  cbind(header) %>%
  as.data.frame(stringsAsFactors = F)

return(news)

}


# Extrayendo las noticias individuales ---------------------------------------------

# Objeto con con el url de las noticias
oil_news_urls <- list()

  for(i in 1:length(paginas_generales)) {
    
    oil_news_urls[i] <- get_news_url(paginas_generales[i]) %>%
      list()
    
    print(i)
    
  }


# Creando un vector con la lista de listas resultantes

oil_news_urls_ <- oil_news_urls %>%
  unlist() %>%
  unique()


# extrayendo el texto de las noticias y la fecha en una lista de data frame

oil_news_list <- list()

for (i in 5093:length(oil_news_urls_)) {
  
  oil_news_list[[i]] <- get_news_text(oil_news_urls_[i])
  
  print(i)
  
}

# unificando todo a un solo df
oil_news_df <- bind_rows(oil_news_list)


# combirtiendo a fecha la variable date

oil_news_df <-  oil_news_df %>%
  mutate(date = mdy_hm(date)) %>%
  glimpse()


# Guardar noticias en un csv
write_csv(oil_news_df ,"oil_news.csv")

# Guardar ambiente de trabajo
save.image("oil_news_ws")

