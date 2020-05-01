require(recommenderlab)
require(tidyverse)
require(reshape2)

data <- read.csv('Data/movie.csv')
ratings <- read.csv('Data/Ratings.csv') %>% select(userid,movieid,rating)
sample_ratings <- ratings[sample(nrow(ratings), 100), ]
sample_ratings <- merge(sample_ratings, data, by='movieid')
sample_ratings <- sample_ratings %>% select(userid,moviename,rating)
# top_critics <- head(names(sort(table(ratings$userid), decreasing = T)), 5)
# sample_ratings <- ratings[ratings$userid %in% top_critics, ]

#### Experimental zone ###

x <- acast(sample_ratings, userid ~ moviename)
R <- as.matrix(x)
r <- as(R, "realRatingMatrix")


rec_POPULAR <- Recommender(r, method='POPULAR')
rec_COLAB_FILTER <- Recommender(r, method='UBCF')

pred_POPULAR <- predict(rec_POPULAR, r[1:5], n = 2)
pred_COLAB_FILTER <- predict(rec_COLAB_FILTER, r[1:5], n = 2)
as(pred_POPULAR,"list")
as(pred_COLAB_FILTER,"list")


b <- binarize(r, minRating=2.5)


model_params <- list(support=0.001,
                     confidence=0.8)

rec_AR <- Recommender(b, "AR", parameter=model_params)
rules <- getModel(rec_AR)$rule_base
inspect(rules)

pred_AR <- predict(rec_AR, b[1:99], n = 2)
as(pred_AR,"list")






# Old way 
# # Create average rating column by movies
# x <- table(unlist(ratings$movieid))
# y <- aggregate(cbind(rating) ~ movieid, data = ratings, sum)
# y$num_ratings <- x
# y$avg_rating <- y$rating/y$num_ratings
# 
# 
# popular <- ifelse(y$num_ratings > mean(y$num_ratings), # Consider only "popular" movies
#                   TRUE,
#                   FALSE)
# 
# popular_table <- y[popular,]
# top_rated <- head(popular_table[order(popular_table$avg_rating, decreasing=T),])
# top_rated <- merge(data,top_rated[ , c("movieid", "avg_rating")], by ='movieid')
# top_rated <- top_rated[order(top_rated$avg_rating, decreasing = T),]
# top_rated
