require(recommenderlab)
require(tidyverse)
require(reshape2)
require(ggplot2)
require(ggpubr)

set.seed(1)

data <- read.csv('Data/movie.csv')
ratings <- read.csv('Data/Ratings.csv') 
ratings.date <- ratings$date # Talvez usar isto no futuro 
ratings <- ratings %>% select(userid,movieid,rating)

top_critics <- ratings %>% group_by(userid) %>% filter(n() > 2000) %>% as.data.frame()
sample_ratings <- top_critics[sample(nrow(top_critics), nrow(top_critics)/200), ]
sample_ratings <- merge(sample_ratings, data, by='movieid')
sample_ratings <- sample_ratings %>% select(userid,moviename,rating)

### Analysis ###

ratings.date.df <- table(ratings.date) %>% as.data.frame 
ratings.date.df$ratings.date <- as.Date(ratings.date.df$ratings.date, "%Y-%m-%d")
ratings.date.df <- ratings.date.df[ratings.date.df$ratings.date >= "2005-01-01", ] # Little data before this date

ggplot(ratings.date.df, aes(x=ratings.date, y=Freq)) +
  geom_line() + 
  labs(title='Number of Ratings Over Time', x='Date', y='Frequency')

agg.ratings <- table(ratings$rating) %>% as.data.frame %>% rename(rating = Var1, freq = Freq)

ggplot(agg.ratings, aes(x=rating, y=freq)) + 
  geom_bar(stat = "identity") +
  labs(title='Rating Distribution', x = 'Rating', y='Frequency')

top.movies <- table(ratings$movieid) %>% as.data.frame %>% rename(movieid = Var1, freq = Freq) %>%
              merge(data, by='movieid') %>% select(moviename,freq) %>% arrange(desc(freq)) %>% head(20)
levels(top.movies$moviename) <- c(levels(top.movies$moviename), "Harry Potter and the Sorcerer's Stone")
top.movies$moviename[top.movies$moviename == "Harry Potter and the Sorcerer's Stone (Harry Potter and the Philosopher's Stone)"] <- "Harry Potter and the Sorcerer's Stone"
top.movies$moviename <- factor(top.movies$moviename, levels = top.movies$moviename[order(top.movies$freq)])

ggplot(top.movies, aes(x=moviename, y=freq)) + 
  geom_bar(stat = "identity") +
  labs(title='Most Popular Movies', x = 'Movie', y='Frequency') +
  coord_flip()

best.critics <- table(ratings$userid) %>% as.data.frame %>% rename(critic = Var1, freq = Freq) %>%
                select(critic,freq) %>% arrange(desc(freq)) %>% head(20)
best.critics$critic <- factor(best.critics$critic, levels = best.critics$critic[order(best.critics$freq)])

ggplot(best.critics, aes(x=critic, y=freq)) + 
  geom_bar(stat = "identity") +
  labs(title='Top Critics', x = 'Critic', y='Frequency') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

best.movies <- ratings %>% select(movieid, rating) %>% group_by(movieid) %>% filter(n() > 2000) %>% 
              as.data.frame
best.movies <- aggregate(rating ~ movieid, best.movies, mean) %>% arrange(desc(rating)) %>% head(10) %>%
              merge(data, by='movieid') %>% select(moviename,rating) # Originally ≈ 1000 movies
best.movies$moviename <- factor(best.movies$moviename, levels = best.movies$moviename[order(best.movies$rating)])

ggplot(best.movies, aes(x=moviename, y=rating)) + 
  geom_bar(stat = "identity") +
  labs(title='Best Movies', x = 'Movie', y='Average Rating') +
  coord_flip(ylim = c(4.0,4.25))

# ggarrange(a,b,c,d)

### Recommenders ###

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
