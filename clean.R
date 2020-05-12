require(recommenderlab)
require(tidyverse)
require(reshape2)
require(ggplot2)
require(ggpubr)
require(binaryLogic)

set.seed(1)

data <- read.csv('Data/movie.csv')
ratings <- read.csv('Data/Ratings.csv') 
ratings.date <- ratings$date # Talvez usar isto no futuro 
ratings <- ratings %>% select(userid,movieid,rating)

top_critics <- ratings %>% group_by(userid) %>% filter(n() > 2000) %>% as.data.frame()
sample_ratings <- top_critics[sample(nrow(top_critics), nrow(top_critics)/200), ]
sample_ratings <- merge(sample_ratings, data, by='movieid')
sample_ratings <- sample_ratings %>% select(userid,moviename,rating)

sample_ratings_binary <- sample_ratings
sample_ratings_binary$rating <-
  ifelse(sample_ratings_binary$rating >= 2.5,
         1,
         0
  )

head(sample_ratings_binary)

x <- dcast(sample_ratings, userid ~ moviename, value.var = "rating", na.rm = FALSE)
y <- dcast(sample_ratings_binary, userid ~ moviename, value.var = "rating", na.rm = TRUE)
x <- as.matrix(x[,-1])
y <- as.matrix(y[,-1])
y[!is.finite(y)] <- 0
useless <- 
  ifelse(rowSums(y) < 1,
         TRUE,
         FALSE
  )
y <- y[!useless ,]

r <- as(x, "realRatingMatrix")
b <- as(y, "binaryRatingMatrix")
model_params <- list(support=1/dim(b)[2],
                     confidence=0.8)

set.movies <-evaluationScheme(r,method='cross-validation',train=.7,given=1,goodRating=2.5,k=10)
set.movies_ <-evaluationScheme(b,method='cross-validation',train=.7,given=-1,k=10)

rec_POPULAR <- Recommender(getData(set.movies,"train"), method='POPULAR')
rec_COLAB_FILTER <- Recommender(getData(set.movies,"train"), method='UBCF')
rec_AR <- Recommender(getData(set.movies_,"train"), "AR", parameter=model_params)

pred_POPULAR <- predict(rec_POPULAR,getData(set.movies,"known"))
pred_COLAB_FILTER <- predict(rec_COLAB_FILTER,getData(set.movies,"known"))
pred_AR <- predict(rec_AR,getData(set.movies_,"known"))

methods <- list("popular" = list(name="POPULAR", param = NULL),
                "user-based CF" = list(name="UBCF", param = NULL))

methods_ <- list("popular" = list(name="POPULAR", param = NULL),
                "user-based CF" = list(name="UBCF", param = NULL),
                "association rules" = list(name="AR"), param = model_params)

acc_REC <- evaluate(set.movies, methods, n=c(1,2,5)) 
acc_REC_ <- evaluate(set.movies_, methods_, n=c(1,2,5)) # ESTE CABRAO NAO DA

plot(acc_REC, annotate = 1:4, legend="topleft")
