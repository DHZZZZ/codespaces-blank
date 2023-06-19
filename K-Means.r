---
title: "Data Cleaning and K-Means Clustering"

---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r rlib}
library(tm) #membersihkan data
library(vroom) #load dataset
library(here) #menyimpan dataset
library(wordcloud)
library(cluster)
library(NLP)
library(colorspace)
library(tidytext)
library(factoextra)
```

```{r load dataset}
d <- vroom(here('reviewsLazada.csv'))
ulasan <- d$reviewContent
ulasan1 <- Corpus(VectorSource(ulasan))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
reviewclean <- tm_map(ulasan1, removeURL)
removeNL <- function(y) gsub("\n", " ", y)
reviewclean <- tm_map(reviewclean, removeNL)
replacecomma <- function(y) gsub(",", "", y)
reviewclean <- tm_map(reviewclean, replacecomma)
removetitik2 <- function(y) gsub(":", "", y)
reviewclean <- tm_map(reviewclean, removetitik2)
removetitikkoma <- function(y) gsub(";", " ", y)
reviewclean <- tm_map(reviewclean, removetitikkoma)
removetitik3 <- function(y) gsub("p…", "", y)
reviewclean <- tm_map(reviewclean, removetitik3)
removeamp <- function(y) gsub("&amp;", "", y)
reviewclean <- tm_map(reviewclean, removeamp)
removeUN <- function(z) gsub("@\\w+", "", z)
reviewclean <- tm_map(reviewclean, removeUN)
remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
reviewclean <- tm_map(reviewclean,remove.all)
reviewclean <- tm_map(reviewclean, removePunctuation)
reviewclean <- tm_map(reviewclean, tolower)
myStopwords = readLines("stopwords-id.txt")
reviewclean <- tm_map(reviewclean,removeWords,myStopwords)
View(reviewclean)

dataframe<-data.frame(text=unlist(sapply(reviewclean, `[`)), stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'ulasan1clean.csv')
```

```{r worldcloud}
reviews <-read.csv("ulasan1clean.csv")
corpusreviews <- Corpus(VectorSource(reviews$text))
dtm <- TermDocumentMatrix(corpusreviews)
pm <- DocumentTermMatrix(corpusreviews,control = 
                           list(weighting=weightTfIdf))
am <- as.matrix(pm)
headmatrix_tfidf <- head.matrix(am,10)
em <- as.matrix(dtm) 
ve <- sort(rowSums(em),decreasing=TRUE) 
de <- data.frame(word = names(ve),freq=ve) 
wordcloud(words = de$word, freq = de$freq, min.freq = 1, max.words=50, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
```

```{r KMEANS}
fviz_nbclust(em, kmeans, method = "silhouette")
final=kmeans(em,2,nstart = 25)
print(final)
```

```{r}
# Fancy kmeans
kmeans_fancy <- kmeans(scale(em[,7:32]), 2, nstart = 100)
# plot the clusters
fviz_cluster(kmeans_fancy, data = scale(em[,7:32]), geom = c("point"),ellipse.type = "euclid")
carsCluster <- kmeans(em, 2)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
