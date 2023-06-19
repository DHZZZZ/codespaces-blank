install.packages("xml2")
install.packages("rvest")
library(rvest)
library(xml2)

alamatweb <- 'https://myanimelist.net/topanime.php'
lamanweb <- read_html(alamatweb)
lamanweb
