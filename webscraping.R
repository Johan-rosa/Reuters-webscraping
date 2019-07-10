# Webscaping de Noticias financieras de Reuters --------------------------------------

    # Autor: Johan Rosa
    # fecha: julio 2019

# Paquetes ---------------------------------------------------------------------------
library(tidyverse) # visualización y transformación de datos
library(XML)       # Descargar y manejar archivos html y xml
library(rvest)     # Extraer información de un xml
library(rebus)     # Expresiones regulares

# Preparativos -----------------------------------------------------------------------

# Definiendo una función que extraiga el url de cada noticia en las páginas generales

get.news.url <- function(url) {
    read_html(url) %>%
        html_nodes("story-title, a") %>%
        html_attr('href') %>%
        str_subset(fixed("article")) %>%
        unique() %>%
        paste0("https://www.reuters.com/", .)
    
}

#  Función para extraer la fecha y el cuerpo de la noticia
get.news.text <-function(url){
    news_html <- read_html(url)
    
    text <- news_html %>%
        html_node(".StandardArticle_content .TwoColumnLayout_left") %>%
        html_text() %>%
        str_trim() %>%
        str_to_lower() %>%
        setNames("text")
    
    date <- news_html %>%
        html_node(".ArticleHeader_date") %>%
        html_text() %>%
        setNames("date")
    
    news <- cbind(date, text) %>%
        as.data.frame()
    
    #Sys.sleep(5)
    
    news
}

# Descargando las noticias de Markets ------------------------------------------------

# Url de las páginas principales de markets

paginaweb_general_markets <- paste0(
    "https://www.reuters.com/news/archive/",
    "marketsNews?view=page&page=",
    1:3000, # esta parte del cóndigo se puede cambiar
    "&pageSize=10")

# Extrayendo los url de las noticias individuales.
# Por temas de conexión no se hizo todo de una vez, se fue cambiando
# el rango en el block de codigos anterior y se fue combinando

markets_news_urls_temp <-   map(paginaweb_general_markets, get.news.url) %>%
    unlist()

# Archivo final
markets_news_urls <- c(markets_news_urls, markets_news_urls_temp)

# Algunas noticias se duplicaban
markets_news_urls <-  markets_news_urls %>%
    unique()

# Guardando un objeto con los urls individuales
saveRDS(markets_news_urls, "markets_news_url")

# Descargando el texto de las noticias de  markets -----------------------------------


noticias_markets_temp <- list()
    
for(i in 35731:length(markets_news_urls)) {

   noticias_markets_temp[[i]] <- get.news.text(markets_news_urls[i])
   print(i)
}


markets_news <- bind_rows(market_news, bind_rows(noticias_markets_temp))

#bind_rows(noticias_markets_temp) %>%
#    View()

# Descargando las noticias de business -------------------------------------------------

# Url de las páginas principales de business
paginaweb_general_business <- paste0(
    "https://www.reuters.com/news/archive/",
    "businessNews?view=page&page=",
    1:3400, # esta parte del cóndigo se puede cambiar
    "&pageSize=10")

# Extrayendo los url de las noticias individuales.
# Por temas de conexión no se hizo todo de una vez, se fue cambiando
# el rango en el block de codigos anterior y se fue combinando

business_news_urls_temp <- list()

for(i in 1:length(paginaweb_general_business)) {
    
    business_news_urls_temp[i] <- get.news.url(paginaweb_general_business[i]) %>%
        list()
    print(i)
        #map(paginaweb_general_business, get.news.url)
}


business_news_url <- unlist(business_news_urls_temp) %>%
    unique()

# Descargando el texto de las noticias de  markets -----------------------------------


noticias_business_temp <- list()

for(i in 1:length(business_news_url)) {
    
    noticias_business_temp[[i]] <- get.news.text(business_news_url[i])
    print(i)
}


business_news <- bind_rows(noticias_business_temp)




save.image("webscraping_ws")

