data <- read.csv('Data/movie.csv')
ratings <- read.csv('Data/Ratings.csv')

dim(data) # 66730 movies, all with unique ID

# Create average rating column by movies
x <- table(unlist(ratings$movieid))
y <- aggregate(cbind(rating) ~ movieid, data = ratings, sum)
y$num_ratings <- x
y$avg_rating <- y$rating/y$num_ratings


popular <- ifelse(y$num_ratings > median(y$num_ratings), # Consider only "popular" movies
                  TRUE,
                  FALSE)

popular_table <- y[popular,]
top_rated <- head(popular_table[order(popular_table$avg_rating, decreasing=T),])
top_rated <- merge(data,top_rated[ , c("movieid", "avg_rating")], by ='movieid')
top_rated <- top_rated[order(top_rated$avg_rating, decreasing = T),]
top_rated
