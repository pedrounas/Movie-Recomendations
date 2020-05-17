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


top_critics <- ratings %>% group_by(userid) %>% filter(n() > 1000) %>% as.data.frame()
sample_ratings <- top_critics[sample(nrow(top_critics), nrow(top_critics)/50), ]
sample_ratings <- merge(sample_ratings, data, by='movieid')
sample_ratings <- sample_ratings %>% select(userid,moviename,rating)


sample_ratings_binary <- sample_ratings
sample_ratings_binary$rating <-
  ifelse(sample_ratings_binary$rating >= 3.5,
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
model_params <- list(support=10/dim(b)[2],
                     confidence=0.1)



set.movies <-evaluationScheme(r,method='cross-validation',given=2,goodRating=3.5,k=10)
set.movies_ <-evaluationScheme(b,method='cross-validation',given=1,k=10)

###################################### Foi para experimentar ######################
#Evaluating the Rating
  
rec_POPULAR <- Recommender(getData(set.movies,"train"), method='POPULAR')
rec_UBCF <- Recommender(getData(set.movies,"train"), method='UBCF')

  

pred_POPULAR <- predict(rec_POPULAR,getData(set.movies,"known"), type="ratings")
pred_UBCF <- predict(rec_UBCF,getData(set.movies,"known"), type="ratings")


#Nao me parece correto,
qplot(rowCounts(pred_POPULAR)) + geom_histogram() +   
  ggtitle("Distribution of movies per user")

#Same, n deviamos ter users sem filmes 
qplot(rowCounts(pred_UBCF)) + geom_histogram() +
  ggtitle("Distribution of movies per user")


eval_accuracy_POPULAR <- calcPredictionAccuracy(pred_POPULAR, getData(set.movies, "unknown"), byUser = TRUE, )
head(eval_accuracy_POPULAR)

eval_accuracy_UBCF <- calcPredictionAccuracy(pred_UBCF, getData(set.movies, "unknown"), byUser = TRUE)
head(eval_accuracy_UBCF)

#######################3

rec_POPULAR <- Recommender(getData(set.movies,"train"), method='POPULAR')
rec_UBCF <- Recommender(getData(set.movies,"train"), method='UBCF')
rec_AR <- Recommender(getData(set.movies_,"train"), "AR", parameter=model_params)

getModel(rec_AR)$rule_base

pred_POPULAR <- predict(rec_POPULAR,getData(set.movies,"known"))
pred_UBCF <- predict(rec_UBCF,getData(set.movies,"known"))
pred_AR <- predict(rec_AR,getData(set.movies_,"known"))


#Best Model

methods <- list("popular" = list(name="POPULAR", param = NULL),
                "user-based CF" = list(name="UBCF", param = NULL))
methods_ <- list("popular" = list(name="POPULAR", param = NULL),
                 "user-based CF" = list(name="UBCF", param = NULL),
                 "association rules" = list(name="AR", param = model_params))

#real
acc_REC <- evaluate(set.movies, methods, n=c(1,2,5)) 
#binary
b_acc_REC <- evaluate(set.movies_, methods_, n=c(1,2,5)) 

avg.conf <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 2, 5)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

final.results.real <- acc_REC %>% map(avg.conf) %>% 
  enframe() %>% unnest(cols = value)

final.results.binary <- b_acc_REC %>% map(avg.conf) %>% 
  enframe() %>% unnest(cols = value)

ggplot(final.results.real, aes(x=FPR, y=TPR, colour = fct_reorder2(as.factor(name), 
                                                              FPR, TPR))) + geom_line() +
  geom_label(aes(label = n)) + 
  labs(title = "ROC curves", colour = "Model")


ggplot(final.results.binary, aes(x=FPR, y=TPR, colour = fct_reorder2(as.factor(name), 
                                                                     FPR, TPR))) + geom_line() +
  geom_label(aes(label = n)) + 
  labs(title = "ROC curves", colour = "Model")

