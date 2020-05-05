require(recommenderlab)
require(tidyverse)
require(reshape2)

set.seed(1)

data <- read.csv('Data/movie.csv')
ratings <- read.csv('Data/Ratings.csv') %>% select(userid,movieid,rating)
top_critics <- ratings %>% group_by(userid) %>% filter(n() > 2000) %>% as.data.frame()
sample_ratings <- top_critics[sample(nrow(top_critics), nrow(top_critics)/200), ]
sample_ratings <- merge(sample_ratings, data, by='movieid')
sample_ratings <- sample_ratings %>% select(userid,moviename,rating)

#### Experimental zone ###

x <- acast(sample_ratings, userid ~ moviename)
R <- as.matrix(x)
r <- as(R, "realRatingMatrix")
b <- binarize(r, minRating=2.5)
model_params <- list(support=1/dim(b)[2],
                     confidence=0.8)

rec_POPULAR <- Recommender(r, method='POPULAR')
rec_COLAB_FILTER <- Recommender(r, method='UBCF')
rec_AR <- Recommender(b, "AR", parameter=model_params)

pred_POPULAR <- predict(rec_POPULAR, r[1:5], n = 2)
pred_COLAB_FILTER <- predict(rec_COLAB_FILTER, r[1:5], n = 2)
pred_AR <- predict(rec_AR, b[1:5], n = 2)
as(pred_POPULAR,"list")
as(pred_COLAB_FILTER,"list")
as(pred_AR,"list")

# rules <- getModel(rec_AR)$rule_base
# inspect(rules)
