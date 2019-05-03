## ------------------------------------------------------------------------
## RMSE compute root mean square error (RMSE)
RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}


## ------------------------------------------------------------------------
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
     semi_join(edx, by = "movieId") %>%
     semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ------------------------------------------------------------------------
# generate reproducible partition

set.seed(1)

# test set will be 10% of edx data

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set

test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set

removed <- anti_join(temp, test)
train <- rbind(train, removed)


## ------------------------------------------------------------------------
# to save space
rm(removed, temp, test_index) 


## ------------------------------------------------------------------------
train %>% as_tibble()


## ------------------------------------------------------------------------
train %>% summarize(
  n_users=n_distinct(userId),
  n_movies=n_distinct(movieId),
  min_rating=min(rating),  
  max_rating=max(rating))


## ------------------------------------------------------------------------
keep <- train %>% 
  count(movieId) %>% 
  top_n(5, n) %>% 
  .$movieId

tab <- train %>% 
  filter(movieId%in%keep) %>% 
  filter(userId %in% c(13:20)) %>% 
  select(userId, title, rating) %>% 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, ":.*")) %>%
  spread(title, rating)
tab %>% knitr::kable()


## ------------------------------------------------------------------------
users <- sample(unique(train$userId), 100)
rafalib::mypar()
train %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


## ------------------------------------------------------------------------
train %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")


## ------------------------------------------------------------------------
train %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")


## ------------------------------------------------------------------------
# to save space
rm(tab, keep, users)


## ------------------------------------------------------------------------
mu <- mean(train$rating) # compute mean rating
mu

naive_rmse <- RMSE(test$rating, mu) # compute root mean standard error
naive_rmse


## ------------------------------------------------------------------------
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


## ---- eval=FALSE---------------------------------------------------------
## fit <- lm(rating ~ as.factor(movieId), data = train)


## ------------------------------------------------------------------------
mu <- mean(train$rating) 
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))


## ------------------------------------------------------------------------
predicted_ratings <- mu + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model on test set",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()


## ------------------------------------------------------------------------
train %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "blue")


## ---- eval = FALSE-------------------------------------------------------
## lm(rating ~ as.factor(movieId) + as.factor(userId))


## ------------------------------------------------------------------------
user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


## ------------------------------------------------------------------------
predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie and User Effects Model on test set",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()


## ------------------------------------------------------------------------
# Regularization

# connect movieId to movie title
movie_titles <- train %>% 
  select(movieId, title) %>%
  distinct()


## ------------------------------------------------------------------------
# top 10 best movies based on b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()


## ------------------------------------------------------------------------
# top 10 worse movies based on b_i
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()


## ------------------------------------------------------------------------
# add number of rating of the "best" obscure movies
train %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


## ------------------------------------------------------------------------
# add number of rating of the "worse" obscure movies

train %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


## ------------------------------------------------------------------------
lambda <- seq(0, 10, 0.25)

rmses <- sapply(lambda, function(l){
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    train %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(train$rating, predicted_ratings))
})

qplot(lambda, rmses)  

lambda <- lambda[which.min(rmses)]


## ------------------------------------------------------------------------
b_i <- test %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- test %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  
predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
 
model_3_rmse <- RMSE(test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie and User Effect Model on test set",  
                                 RMSE = model_3_rmse))
rmse_results %>% knitr::kable()


## ------------------------------------------------------------------------
train <- train %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

test <- test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)


## ------------------------------------------------------------------------

# Using recosystem 
# Install/Load recosystem
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

train_data <- data_memory(user_index = train$userId, item_index = train$movieId, 
                          rating = train$res, index1 = T)
  
test_data <- data_memory(user_index = test$userId, item_index = test$movieId, index1 = T)
  
recommender <- Reco()

## ------------------------------------------------------------------------
set.seed(1) # This is a randomized algorithm

# res = recommender$tune(
#     train_data,
#     opts = list(dim = c(10, 20, 30),
#                 costp_l1 = 0, costq_l1 = 0,
#                 lrate = c(0.05, 0.1, 0.2), nthread = 2)
# )
#  res[["min"]]
# $dim
# [1] 30
# 
# $costp_l1
# [1] 0
# 
# $costp_l2
# [1] 0.01
# 
# $costq_l1
# [1] 0
# 
# $costq_l2
# [1] 0.1
# 
# $lrate
# [1] 0.1
# 
# $loss_fun
# [1] 0.8007686



## ------------------------------------------------------------------------
# Training model from a data file
set.seed(1) # This is a randomized algorithm
suppressWarnings(recommender$train(train_data, opts = c(dim = 30, costp_l1 = 0,
                                                        costp_l2 = 0.01, costq_l1 = 0,
                                                        costq_l2 = 0.1, lrate = 0.1,
                                                        verbose = FALSE)))


## ------------------------------------------------------------------------
predicted_ratings <- recommender$predict(test_data, out_memory()) + mu + test$b_i + test$b_u 


## ------------------------------------------------------------------------
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5
  
ind <- which(predicted_ratings < 0.5)
predicted_ratings[ind] <- 0.5
  
model_4_rmse <- RMSE(test$rating, predicted_ratings)
  
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg. Movie and User + Matrix Fact. on test set",  
                                   RMSE = model_4_rmse))
rmse_results %>% knitr::kable()



## ------------------------------------------------------------------------

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

edx <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

validation <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

# Using recosystem 
train_data <- data_memory(user_index = edx$userId, item_index = edx$movieId, 
                          rating = edx$res, index1 = T)
  
test_data <- data_memory(user_index = validation$userId, item_index = validation$movieId, index1 = T)
  
recommender <- Reco()
set.seed(1)

suppressWarnings(recommender$train(train_data, opts = c(dim = 30, costp_l1 = 0,
                                                        costp_l2 = 0.01, costq_l1 = 0,
                                                        costq_l2 = 0.1, lrate = 0.1,
                                                        verbose = FALSE)))


predicted_ratings <- recommender$predict(test_data, out_memory()) + mu + 
                       validation$b_i + validation$b_u 

  
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5
  
ind <- which(predicted_ratings < 0.5)
predicted_ratings[ind] <- 0.5
  
model_5_rmse <- RMSE(validation$rating, predicted_ratings)
  
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg. Movie and User + Matrix Fact. on validation set",  
                                   RMSE = model_5_rmse))
rmse_results %>% knitr::kable()
