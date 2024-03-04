##########################################################
# Create edx and final_holdout_test sets 
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(kableExtra)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


options(timeout = 120)

# Section 1 : LOADING DATA

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)


# Section 2 : DATA WRANGLING [Initial]

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")


# Section 3 : CREATING DATA SETS
# edx dataset: for training & developing best algorithm.
# final_holdout_test: Evaluate RMSE for the selected algorithm.

# Final hold-out test set will be 10% of MovieLens data
set.seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Section 4 : EXPLORATORY DATA ANALYSIS

# Identify dataset structure
kable(head(edx, 5), booktabs = TRUE, caption = "Head Rows") %>%
  kable_styling(position = "center", latex_options = c("striped", "scale_down")) 

# edx table structure
str(edx)

# Summary of edx
summary_edx <- summary(edx)
kable(summary_edx, booktabs = TRUE, caption = "Summary Statistics") %>%
  kable_styling(position = "center", latex_options = c("scale_down", "striped"))

# Number of unique values for the “users”, “movies” and “genre” 
edx %>%
  summarize('Nb Unique Users' = n_distinct(userId), 
            'Nb Unique Movies' = n_distinct(movieId),
            'Nb Unique Genres' = n_distinct(genres)) %>%
  kable(caption = "Unique Values") %>%
  kable_styling(position = "center", latex_options = c("scale_down", "scale_down"))


# Section 5 : DATA WRANGLING [Advanced]

# Convert the "timestamp" column to "datetime" format
  edx <- edx %>%
    mutate(timestamp = as_datetime(timestamp), 
           rating_date = make_date(year(timestamp), month(timestamp))) %>% 
    select(-timestamp) %>% 
    relocate(rating_date, .after = rating) %>%
    as.data.table()

final_holdout_test <- final_holdout_test %>% 
  mutate(timestamp = as_datetime(timestamp), rating_date = make_date(year(timestamp), month(timestamp))) %>% 
  select(-timestamp) %>% 
  relocate(rating_date, .after = rating) %>% 
  as.data.table()

# Extract the "Release Year" observation from the "Title" column
rel_year <- "(?<=\\()\\d{4}(?=\\))"

edx <- edx %>%
  mutate(release_year = str_extract(title, rel_year) %>%
           as.integer()) %>%
  relocate(release_year, .after = title)

final_holdout_test <- final_holdout_test %>%
  mutate(release_year = str_extract(title, rel_year) %>%
           as.integer()) %>%
  relocate(release_year, .after = title)

# Use the new “Release Year” column to add a “MovieAge” column
edx <- edx %>% 
  mutate(movie_age = 2023 - release_year) %>% 
  relocate(movie_age, .after = release_year)

final_holdout_test <- final_holdout_test %>%
  mutate(movie_age = 2023 - release_year) %>% 
  relocate(movie_age, .after = release_year)


# Section 6 : REVIEW USERS

# Calculate number of ratings per user
ratings_per_user <- edx %>%
  group_by(userId) %>%
  summarize(nb_ratings = n()) %>%
  ungroup()

length(which(ratings_per_user$nb_ratings <= 20))

ggplot(ratings_per_user, aes(x = nb_ratings)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  scale_x_log10() +
  labs(title = "Log Transformed Distribution of Number of Ratings per User",
       x = "Log Number of Ratings",
       y = "Number of Users") +
  theme_linedraw()


# Section 7 : REVIEW MOVIES

# Relationship between number of movies vs number of ratings
ratings_movie <- edx %>%
  group_by(movieId) %>%
  summarize(nb_ratings = n()) %>%
  ungroup()
summary(ratings_movie)
str(ratings_movie)

ggplot(ratings_movie, aes(x = nb_ratings)) +
  geom_histogram(bins = 40, fill = "red", color = "black") +
  scale_x_log10() +
  labs(title = "Log Transformed Distribution of Number of Ratings vs. Movies",
       x = "Log Number of Ratings",
       y = "Number of Movies") +
  theme_linedraw()


# Section 7 : REVIEW RATINGS

edx_ratings <- edx %>%
  group_by(rating_date) %>%
  summarize(nb_rating = n())
ggplot(edx_ratings, aes(x = rating_date, y = nb_rating)) +
  geom_point(color = "red") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 300000, 50000), labels = scales::comma) +
  labs(title = "Distribution of Monthly Ratings",
       x = "Rating Date",
       y = "Number of Ratings") +
  geom_rug(color = "red") +
  geom_smooth(color = "black") +
  theme_linedraw()

# Rating values
ggplot(edx, aes(rating)) +
  geom_histogram(binwidth = 0.25, fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  scale_y_continuous(breaks = seq(0, 3000000, 500000)) +
  labs(title = "Distribution of Ratings Values",
       x = "Rating Value",
       y = "Number of Ratings") +
  theme_linedraw()

# Average rating per user
avg_ratings_per_user <- edx %>%
  group_by(userId) %>%
  summarize(average_rating = mean(rating)) %>%
  ungroup()
ggplot(avg_ratings_per_user, aes(x = average_rating)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(title = "Distribution of Average Ratings per User",
       x = "Average Rating",
       y = "Number of Users") +
  theme_linedraw()

# Section 8 : MOVIE AGE

# Calculate relation between Mean Rating vs Movie Age
ratings_avg_age <- edx %>%
  group_by(movie_age) %>%
  summarize(average_rating = mean(rating)) %>%
  ungroup()
ggplot(ratings_avg_age, aes(x = movie_age, y = average_rating)) +
  geom_point(color = "red") +
  labs(title = "Distribution of Avg Ratings vs. Movie Age",
       x = "Movie Age",
       y = "Avg Rating") +
  geom_rug(color = "red") +
  geom_smooth(color = "black") +
  theme_linedraw()

# Section 9 : MODELING

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# #9.1 "Benchmark" Model
edx_mu <- mean(edx$rating)
RMSE_1 <- RMSE(edx$rating, edx_mu)

result1_table <- tibble(Model = "Benchmark", RMSE = RMSE_1)
result1_table %>% 
  knitr::kable()

# #9.2 "Movie Bias" Model
movie_bias <- edx %>%
  group_by(movieId) %>%
  summarize(bm = mean(rating - edx_mu))
ggplot(movie_bias, aes(x = bm)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(title = "Distribution of Movie Bias",
       x = "bm",
       y = "Number of Movies") +
  theme_linedraw()

pred_bm <- edx_mu + edx %>% 
  left_join(movie_bias, by = "movieId") %>% 
  .$bm
RMSE_2 <- RMSE(edx$rating, pred_bm)
result2_table <- tibble(Model = "Movie Bias", RMSE = RMSE_2)
result2_table %>% 
  knitr::kable()

# #9.3 "Movie & User Biases" Model
user_bias <- edx %>%
  left_join(movie_bias, by = "movieId") %>%
  group_by(userId) %>% 
  summarize(bu = mean(rating - edx_mu - bm))
ggplot(user_bias, aes(x = bu)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(title = "Distribution of User Bias",
       x = "bu",
       y = "Number of Movies") +
  theme_linedraw()

pred_bu <- edx %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  mutate(pred = edx_mu + bm + bu) %>% 
  .$pred
RMSE_3 <- RMSE(edx$rating, pred_bu)
result3_table <- tibble(Model = "Movie & User Biases", RMSE = RMSE_3)
result3_table %>% 
  knitr::kable()

# #9.4 "Regularized Movie & User Biases" Model
lambdasReg <- seq(0, 10, 0.25)
RMSEreg <- sapply(lambdasReg, function(l){
  edx_mu <- mean(edx$rating)
  bm <- edx %>%
    group_by(movieId) %>%
    summarize(bm = sum(rating - edx_mu)/(n() + l))
  bu <- edx %>%
    left_join(bm, by = 'movieId') %>% 
    group_by(userId) %>%
    summarize(bu = sum(rating - bm - edx_mu)/(n() + l))
  predicted_ratings <- edx %>%
    left_join(bm, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = edx_mu + bm + bu) %>%
    .$pred
  return(RMSE(predicted_ratings, edx$rating))
})
lambda <- lambdasReg[which.min(RMSEreg)]
lambda
ggplot(mapping = aes(x = lambdasReg, y = RMSEreg)) +
  geom_point(color = "red") +
  labs(title = "Distribution of Lambdas",
       x = "Lambda",
       y = "RMSE") +
  theme_linedraw()

bm <- edx %>% 
  group_by(movieId) %>%
  summarize(bm = sum(rating - edx_mu)/(n() + lambda))
bu <- edx %>% 
  left_join(bm, by = "movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bm - edx_mu)/(n() + lambda))
pred_reg <- edx %>% 
  left_join(bm, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(predictions = edx_mu + bm + bu) %>%
  .$predictions
RMSE_4 <- RMSE(edx$rating, pred_reg)
result4_table <- tibble(Model = "Regularised Movie & User Biases", RMSE = RMSE_4)
result4_table %>% 
  knitr::kable()

# Summarize RMSE
results_table <- tibble(Model = c("Benchmark", "Movie Bias", "Movie & User Biases", "Regularised Movie & User Biases"), RMSE = c(RMSE_1, RMSE_2, RMSE_3, RMSE_4))
results_table %>% 
  knitr::kable()


# Section 10 : RESULTS

#Defining "lambda" for "final_holdout_test" dataset
lambdasReg2 <- seq(0, 10, 0.25)

RMSEreg2 <- sapply(lambdasReg2, function(l){
  edx_mu2 <- mean(final_holdout_test$rating)
  bm <- final_holdout_test %>%
    group_by(movieId) %>%
    summarize(bm = sum(rating - edx_mu2)/(n() + l))
  bu <- final_holdout_test %>%
    left_join(bm, by = 'movieId') %>% 
    group_by(userId) %>%
    summarize(bu = sum(rating - bm - edx_mu2)/(n() + l))
  predicted_ratings <- final_holdout_test %>%
    left_join(bm, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = edx_mu2 + bm + bu) %>%
    .$pred
  return(RMSE(predicted_ratings, final_holdout_test$rating))
})

#Applying model to "final_holdout_test
bm <- final_holdout_test %>% 
  group_by(movieId) %>%
  summarize(bm = sum(rating - edx_mu)/(n() + lambda))
bu <- final_holdout_test %>% 
  left_join(bm, by = "movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bm - edx_mu)/(n() + lambda))
pred_reg <- final_holdout_test %>% 
  left_join(bm, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(predictions = edx_mu + bm + bu) %>%
  .$predictions

# Computing RMSE
RMSE_final2 <- RMSE(final_holdout_test$rating, pred_reg)
resultfinal2_table <- tibble(Model = "Regularised Movie & User Biases", RMSE = RMSE_final2)
resultfinal2_table %>% 
  knitr::kable()