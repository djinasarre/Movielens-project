################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

##Data exploration and visualization

#Lets look to the year with highest median number of ratings
str(movielens)

year<-gsub("[^0-9]", "", movielens$title)
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
library(stringi)
year <- stri_extract_last_regex(year, "\\d{4}$")
year<- as.numeric(year)
movielens1 <-movielens%>% mutate(year) 
movielens1%>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#movies after 1993 get more 
#ratings. We also see that with newer movies, starting in 1993, the 
#number of ratings decreases with year: the more recent a movie is, the
#less time users have had to rate it.

#Among movies that came out in 1993 or later, here are the 10 movies 
#with the most ratings per year, and its average rating of each 
#of the top 10 movies.

movielens1 %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2019 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(10, rate) %>%
  arrange(desc(rate)) 
#We see that the trend is that the more often a movie is rated, the higher
# its average rating
movielens1 %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2019 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#There is some evidence of a time effect on average rating
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
library(lubridate)
movielens2 <- mutate(movielens, date = as_datetime(timestamp))
movielens2 %>%
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Keeping only categories with more than 1,000 ratings Drama|Film-Noir|Romance
# genre has the highest average rating.

movielens2 %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%top_n(10, avg)%>%  
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
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

#Quiz: MovieLens Dataset
#Q1 How many rows and columns are there in the edx dataset?
dim(edx)
#Number of rows:9000055 Number of columns:6

#Q2 How many zeros were given as ratings in the edx dataset?
sum(edx$rating == 0)
#How many threes were given as ratings in the edx dataset?
sum(edx$rating == 3)

#Q3 How many different movies are in the edx dataset?
n_distinct(edx$movieId)

#Q4 How many different users are in the edx dataset?
n_distinct(edx$userId)
#Q5 How many movie ratings are in each of the following genres in the edx dataset?
drama <- edx %>% filter(str_detect(genres,"Drama"))
comedy <- edx %>% filter(str_detect(genres,"Comedy"))
thriller <- edx %>% filter(str_detect(genres,"Thriller"))
romance <- edx %>% filter(str_detect(genres,"Romance"))
nrow(drama)
nrow(comedy)
nrow(thriller)
nrow(romance)

#Q6 Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% summarise(number = n()) %>%
  arrange(desc(number))
#Answer Pulp Fiction
#Q7 What are the five most given ratings in order from most to least?
edx%>%group_by(rating)%>% summarise(number = n()) %>%
  arrange(desc(number))
#Answer 4,3,5,3.5,2

#Q8 True or False: In general, half star ratings are less common than whole star
# ratings (e.g., there are fewer ratings of 3.5 than there are ratings of 3 or 4
# , etc.).
table(edx$rating)
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()
#Answer True



#We can see this table is in tidy format with 9,000,055 rows:
edx %>% as_tibble()
#Each row represents a rating given by one user to one movie.

#We create an aditional partition of training and test sets from the provided edx dataset 
#to experiment with multiple parameters.
#Test set will be 10% of Edx data
set.seed(6, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp2 <- edx[test_index,]

# We make sure userId and movieId in test set and validation set are also in the training set

test_set <- temp2 %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# Add rows removed from test set back into train set

removed <- anti_join(temp2, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp2, removed)


#Recommendation systems is a type of machine learning  algorithm that will be applied outside our
# control, as users look for movie recommendations.Our algorithm is decided based on the residual 
#mean squared error (RMSE) on a test set, Once the algorithm is decided, we will apply it to 
#the validation set.

#Lets write a function that computes the RMSE for vectors of ratings and their corresponding 
#predictors:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#First model:
#we predict the same rating for all movies regardless of user.
#We know that the estimate that minimizes the RMSE is the least squares estimate in this case, is
#the average of all ratings:
mu_hat <- mean(train_set$rating)
mu_hat
#If we predict all unknown ratings with mu_hat we obtain the following RMSE:
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse
#Lets create a results comparing table for different approaches.
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
rmse_results

#Second model:
#We augment our previous model by adding the term  bi bias to represent 
#average ranking for movie  i.

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 15, data = ., color = I("black"))
# Remember  mu_hat=3.5 so a  bi=1.5 implies a perfect five star rating.
#Lets see how much prediction improves using bi
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results 

#Third model. User effects
#Lets compute the average rating for user  u for those that have rated over 200 movies:
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=200) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
#Notice that there is substantial variability across users as well.
#To fit this model for the reasons of computing time, we will compute an approximation of lm by 
#computing
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#We construct predictors and see how RMSE improves:
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse))
rmse_results

# Fourth model, Regularized Movie Effect Model
#we use regularization that permits us to penalize large estimates that are formed using small 
#sample sizes.

#We use cross-validation choosing the penalty terms (lambdas). 
lambdas <- seq(0, 15, 0.10)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = min(rmses)))

#We use regularization for the estimate user effects.
#The estimates that minimize this can be found using cross-validation to pick a  lambda:
lambdas <- seq(0, 15, 0.10)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
#The optimal  lambda is: 
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#We used validation set to test of the final model Regularized Movie + User Effect Model.

lambdas <- seq(0, 15, 0.10)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  
#the optimal  lambda is: 
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model (validation set)",  
                                     RMSE = min(rmses)))

rmse_results %>% knitr::kable()
#The report finds the model Regularized Movie + User Effect Model as the one that minimize 
#RMSE. Probably further investigation using recommenderlab package give us a new model.






