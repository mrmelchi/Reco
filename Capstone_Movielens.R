#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

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

# Learners will develop their algorithms on the edx set
# For grading, learners will run algorithm on validation set to generate ratings

# validation <- validation %>% select(-rating)

# Ratings will go into the CSV submission file below:

write.csv(validation %>% select(userId, movieId) %>% mutate(rating = NA),
          "submission.csv", na = "", row.names=FALSE)
rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx <- edx %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)") %>%str_remove_all("[\\(\\)]")) %>%
  mutate(title = str_remove(title, "\\(\\d{4}\\)$") %>% str_trim())

edx$year <- as.integer(edx$year)

validation <- validation %>%
  mutate(year = str_extract(title, "\\(\\d{4}\\)") %>%str_remove_all("[\\(\\)]")) %>%
  mutate(title = str_remove(title, "\\(\\d{4}\\)$") %>% str_trim())

validation$year <- as.integer(validation$year)

set.seed(1)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in validation set are also in edx set

test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(edx, removed, temp, test_index) 

train %>% as_tibble()

train %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

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

users <- sample(unique(train$userId), 100)
rafalib::mypar()
train %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

train %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

train %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

rm(tab, keep, users)
gc()

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train$rating)
mu_hat

naive_rmse <- RMSE(test$rating, mu_hat)
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

## Modeling movie effects

mu <- mean(train$rating) 
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie Effect Model",  
                                     RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()

### User effects

train %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "blue")

user_avgs <- train %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Regularization

test %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- train %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

train %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

## Penalized Least Squares

lambda <- 3
mu <- mean(train$rating)
movie_reg_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

predicted_ratings <- test %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
model_3_rmse <- RMSE(predicted_ratings, test$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()
## Choosing the penalty terms

lambdas <- seq(0, 10, 0.25)
mu <- mean(train$rating)
just_the_sum <- train %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

alphas <- seq(0, 10, 0.25)
rmses <- sapply(alphas, function(l){
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(test$rating,predicted_ratings))
})
qplot(alphas, rmses)  

alpha <- alphas[which.min(rmses)]
alpha

mu <- mean(train$rating)

b_i <- train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+ lambda))

b_u <- train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+ alpha))
predicted_ratings <- 
  test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_4_rmse <- RMSE(test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie + reg User Effect Model",  
                                 RMSE = model_4_rmse))
rmse_results %>% knitr::kable()


genres <- as.data.frame(train$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
rm(genres)
colnames(genres2) <- c(1:7)
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0, dim(train) [1], length(genre_list)) #empty matrix
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_list == genres2[i,c])
    genre_matrix[i,genmat_col] <- 1L
  }
}
rm(genres2)
genre <- as.data.frame(genre_matrix)
rm(genre_matrix)
train <- cbind(genre,train)
rm(genre, just_the_sum, movie_reg_avgs,movie_avgs, user_avgs, 
   alphas, alpha, lambdas,lambda, predicted_ratings)

train <- train %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(res = rating - mu - b_i - b_u)

lm_model <- lm(res ~ ., train[,c(1:18,28)])

genres <- as.data.frame(test$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
rm(genres)
colnames(genres2) <- c(1:7)
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0, dim(test) [1], length(genre_list)) #empty matrix
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_list == genres2[i,c])
    genre_matrix[i,genmat_col] <- 1L
  }
}
rm(genres2)
genre <- as.data.frame(genre_matrix)
rm(genre_matrix)
test <- cbind(genre,test)
rm(genre)

test <- test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(gu_i =  predict(lm_model, test[, 1:18]))

predicted_ratings <- 
  test %>% 
  mutate( pred = mu + b_i + b_u + gu_i) %>%
  .$pred

ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

ind <- which(predicted_ratings < 0)
predicted_ratings[ind] <- 0

model_5_rmse <- RMSE(test$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg Movie + Reg User + Genres Effect Model",  
                                 RMSE = model_5_rmse))
rmse_results %>% knitr::kable()


# # time effect
# 
# train %>% group_by(year) %>%
#   filter(n() > 10) %>% 
#   summarize(rating = mean(rating)) %>%
#   ggplot(aes(year, rating)) +
#   geom_point() +
#   geom_smooth()
# 
# # The plot can be generated using the following code:
#   
#   train %>% 
#   group_by(movieId) %>%
#   summarize(n = n(), years = 2019 - first(year),
#             title = title[1],
#             res = mean(res)) %>%
#   mutate(rate = n/years) %>%
#   filter(rate >= 10) %>%
#   ggplot(aes(rate, res)) +
#   geom_point() +
#   geom_smooth()
# 
# # We see that the trend is that the more often a movie is rated,
# # the higher its average rating.

rm(predicted_ratings) 

# Install/Load recosystem
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
  
  
# Using recosystem 
train_data <- data_memory(user_index = train$userId, item_index = train$movieId, 
                          rating = train$res, index1 = T)
  
test_data <- data_memory(user_index = test$userId, item_index = test$movieId, index1 = T)
  
  
# Choices for parameters were optimized using RMSE as a loss function. The code is in the report but not here as it takes over 24 hours to optimize.
  
recommender <- Reco()
set.seed(1245)

recommender$train(train_data,opts = c(dim = 30, costp_l2 = 0.1, costq_l2 = 0.1, 
                                      lrate = 0.1, niter = 100, nthread = 6, verbose = F)) 
  
predicted_ratings <- recommender$predict(test_data, out_memory()) + mu + 
                       test$b_i + test$b_u + test$gu_i

  
ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5
  
ind <- which(predicted_ratings < 0)
predicted_ratings[ind] <- 0
  
model_6_rmse <- RMSE(test$rating, predicted_ratings)
  
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg. Movie + Reg. User + Genres + Matrix Fact. on test set",  
                                   RMSE = model_6_rmse))
rmse_results %>% knitr::kable()

rm(test_data, train_data, movie_titles, c, genmat_col,
   genre_list, i, ind, model_1_rmse, model_2_rmse, model_3_rmse, model_4_rmse, 
   model_5_rmse, model_6_rmse, mu_hat, naive_rmse, predicted_ratings, rmses)

genres <- as.data.frame(validation$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
rm(genres)
colnames(genres2) <- c(1:7)
genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

genre_matrix <- matrix(0, dim(validation) [1], length(genre_list)) #empty matrix
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_list == genres2[i,c])
    genre_matrix[i,genmat_col] <- 1L
  }
}
rm(genres2)
genre <- as.data.frame(genre_matrix)
rm(genre_matrix)
validation <- cbind(genre,validation)
rm(genre)

#lm_model <- lm(res ~ ., train[,c(1:18,28)])

validation <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(gu_i =  predict(lm_model, validation[, 1:18]))

# Using recosystem 
#train_data <- data_memory(user_index = train$userId, item_index = train$movieId, 
#                         rating = train$res, index1 = T)

validation_data <- data_memory(user_index = validation$userId, item_index = validation$movieId, index1 = T)


# Choices for parameters were optimized using RMSE as a loss function. The code is in the report but not here as it takes over 24 hours to optimize.

#recommender <- Reco()
#recommender$train(train_data, opts = c(dim = 30, 
                                       # costp_l1 = 0.0, costp_l2 = 0.01, 
                                       # costq_l1 = 0.0, ostq_l2 = 0.1,
                                       # lrate = 0.1, niter = 500, nthread = 6,
                                       # verbose = F))  


predicted_ratings <- recommender$predict(validation_data, out_memory()) + mu + 
                     validation$b_i + validation$b_u + validation$gu_i


ind <- which(predicted_ratings > 5)
predicted_ratings[ind] <- 5

ind <- which(predicted_ratings < 0)
predicted_ratings[ind] <- 0

model_7_rmse <- RMSE(validation$rating, predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          tibble(method="Reg. Movie + Reg. User + Genres + Matrix Fact. on Validation set",  
                                 RMSE = model_7_rmse))
rmse_results %>% knitr::kable()

rm(b_i, b_u, train_data, validation_data, predicted_ratings, recommender,
   c, genmat_col, genre_list, i, ind, model_7_rmse, lm_model)
                     
train <- train %>%
  mutate(gu_i = rating - res - mu - b_i - b_u) %>%
  select(b_u, b_i, gu_i, rating)

test <- test %>%
  select(b_u, b_i, gu_i, rating)
         
test <- test %>%
  select(b_u, b_i, gu_i, rating)

validation <- validation %>%
  select(b_u, b_i, gu_i, rating)

P <- na.omit(res$P)
Q <- na.omit(res$Q)
pq <- P%*%t(Q)

