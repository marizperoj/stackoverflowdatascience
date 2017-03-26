library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(tidyr)
library(qgraph)
library(ggplot2)
library(ggrepel)
library(proxy)
library(tsne)
library(lsa)
library(ggthemes)

#Help function
nearest_vector <- function(x, vocabulary){
  distance <- map_dbl(vocabulary, ~cosine(., x))
  res <- data_frame(
    terms = names(vocabulary),
    distance 
  ) %>% 
    arrange(-distance)
  return(res)
}

#Data import and wrangling
raw_data <- read_csv("data/language_coocurrence.csv")
tag_frequency <- raw_data %>% 
                   group_by(tagged) %>% 
                   summarize(count = n()) %>% 
                   arrange(-count)

tags_100_most_frequent <- tag_frequency$tagged[1:80]


co_occurence <- raw_data %>% 
                filter(tagged %in% tags_100_most_frequent) %>% 
                mutate(value = 1) %>% 
                spread(tagged, value, fill = 0)

numeric_data <- co_occurence[, -1] %>% 
                as.matrix %>% 
                t
#LSA

model <- lsa(numeric_data, 10)


d <- dist(model$tk, method = "cosine") 

fit <- tsne(
  d,
  perplexity = 5,
  max_iter = 5000
)

projection <- data_frame( label = names(d),
                          x1 = fit[,1],
                          x2 = fit[,2])

projection$cluster <- as.factor(kmeans(projection[,2:3], 12)$cluster)



ggplot(projection) +
  aes(x = x1, y = x2, label = label, color = cluster) +
  geom_point() +
  geom_text_repel() +
  guides(color = F) +
  theme_fivethirtyeight() +
  ggtitle("StackOverflow tags, 2016, semantic space")

#coocurrence

r_users <- co_occurence %>% 
           filter(r == 1)

coocurrences <- co_occurence %>% 
                filter(javascript == 1) %>% 
                gather("language", value, 2:ncol(.)) %>% 
                group_by(language) %>% 
                summarize(n = sum(value)) %>% 
                arrange(-n)
  
#Vector algebra



vectors <- t(model$tk) %>% as_tibble
normalized_vectors <- map_df(vectors, ~./sqrt(sum(.^2)))
new_vector <- normalized_vectors$php  - normalized_vectors$mysql + normalized_vectors$javascript 
nearest_vector(new_vector, vectors)

