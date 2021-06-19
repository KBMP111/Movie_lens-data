
#BETWEEN LINES 3 AND 51 IS THE CODE TO DOWNLOAD 
#THE DATA SET
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

# Validation set will be 10% of MovieLens data

set.seed(1)
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

# WE EXPLORE OUR DATA IN THE NEXT LINES
#####
#fINDING The number of unique users
edx$userId %>% unique() %>% length()
#THE RESULT PROVES USERS RATE MULTIPLE MOVIES
#the number of unique movies
edx$movieId %>% unique() %>% length()
#THE RESULT SHOWS A RELATIVELY LOW NUMBER OF
#MOVIES TO THE DATA, HENCE MOVIES ARE RATED A 
#BY MANY USERS

# WE OBSERVE the most rated films
movie_counts <- edx %>% group_by(title) %>% summarise(count = n()) 
movie_counts[order(movie_counts$count, decreasing = TRUE),]

# AND the least rated films 
movie_counts[order(movie_counts$count, decreasing = FALSE),]

#FREQUENCY OF RATINGS
# WE ALSO OBSERVE THE DISTRIBUTION OF THE RATINGS
edx %>%
  group_by(rating)%>%
  summarise(count = n()) %>% 
  ggplot(aes(x = rating, y = count))+
  geom_smooth(se= FALSE)
#WE TEST THE IMPACT OF REMOVING MOVIES AND USERS WITH A FEW RATINGS
# REMOVING MOVIES WITH few RATINGS
edx %>% 
  group_by(movieId) %>%
  summarise(mcount=n())%>%
  filter(mcount>500)%>%
  left_join(edx,by= 'movieId')%>%
  group_by(rating)%>%
  summarise(count = n())%>%
  ggplot(aes(x = rating, y = count ))+
  geom_col()
# REMOVING users WITH few RATINGS
edx %>% 
  group_by(userId) %>%
  summarise(ucount=n())%>%
  filter(ucount>1000)%>%
  left_join(edx,by= 'userId') %>%
  group_by(rating)%>%
  summarise(count = n())%>%
  ggplot(aes(x = rating, y = count))+
  geom_col()
######
#FINALLY WE BUILD OUR MODEL
#####
# We fist split our data into training and testing sets,
#We will carry out cross validation
#so they will be 9 training sets and one test set.
ind = createFolds(edx$rating, k = 4)

test1 = edx[ind$Fold1,]
train1 = edx[-ind$Fold1,]
#To ensure all users in the train set appear in the test and likewise for movies we use the semi join function

test1 <- test1 %>%
  semi_join(train1, by = "movieId") %>%
  semi_join(train1, by = "userId")

# After splitting our data in to train and test
#we create a naive model that assings the average movie rating
# to all movies, thus our first model is y(rating) = mu(average rating) + e(error)
avg_rat1 = mean(train1$rating)

#Thus our RMSE for our naive prediction is:
naive_rmse1 <- RMSE(test1$rating, avg_rat1)
naive_rmse1

# Our value is 1.06 which is quite high our goal is to go below 0.865, 
#however any other value will in this simple model will be worse, 
#We shall store our results
rmse_results <- tibble(method = "Just the average1", RMSE = naive_rmse1)

#So now we know all movie cannot have the same rating 
#so we improve our model by including a specifif movie effect
# y = mu + bi(specific movie effect) + e
mu = avg_rat1
movie_avgs1 <- train1 %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
#So we now have each movies average rating, 
#thus we know find our predicted rating with this new model
predicted_ratings1 <- mu + test1 %>%
  left_join(movie_avgs1, by='movieId') %>%
  pull(b_i)
model_1_rmse1 <- RMSE(predicted_ratings1, test1$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Mode1l",
                                     RMSE = model_1_rmse1))
rmse_results
# we observe a large improvement our RMSE is now 0.944
# We can now include a user effect 
user_avgs1 <- train1 %>%
  left_join(movie_avgs1, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#we now complete the model
predicted_ratings1 <- test1 %>%
  left_join(movie_avgs1, by='movieId') %>%
  left_join(user_avgs1, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
model_2_rmse1 <- RMSE(predicted_ratings1, test1$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model1",
                                 RMSE = model_2_rmse1))
rmse_results
#THESE MODELS FAIL TO ACHEIVE THE DESIRED LEVEL OF ACCURACY IT IS OBVIOUS 
#A BETTER MODEL IS REQUIRED. TO ACHIEVE THIS WE SHALL EXPERIMENT WITH REGULARISATION
#THAT IS FINDING A PENALTY TERM THAT WILL MINIMISE THE ERROR
#THIS INTUTION IS OSERVED WHEN WE EXPLORE 
#THE BEST RATED AND WORST RATED MOVIES IN OUR INITIAL MODEL
# AS SHOWN
movie_titles <-edx[,c('movieId','title')] 
train1 %>% count(movieId) %>%
  left_join(movie_avgs1 ) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10)

train1 %>% count(movieId) %>%
  left_join(movie_avgs1) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10)
#WE QUICKLY OBSERVE EXTREME BIASES FOR MOVIES WITH FEW RATINGS, 
#THUS WE PENALISE THESE VALUES AND ARE JUSTIFED IN USING REGULARISATION
#####

#####
#####
#The RMSE this time around is 0.867 we hope to drop to less than 0.8649
#We now penalise the rating of movies that have few ratings 
#that is popular movies will get the average rating plus the movie effect and the user effect, 
#but unpopular movies will be penalised
#That is they will be regularised
#Before we can regularise the model an appropriate lamda has to be selected

lambdas <- seq(-10, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train1$rating)
  b_i <- train1 %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train1 %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test1 %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test1$rating))
})
lambdas[which.min(rmses)]
#####
#OUR FINAL MODEL WITH L = 5 AS FOUND ABOVE USING THE COMPLETE EDX DATASET
#FOR TRAINING AND THE VALIDATION SET FOR CALCULATING THE RMSE
#####
# WE INITIALLY NOTE THE MEAN RATING
mu <- mean(edx$rating)
# USING THE MEAN RATING WE TRY TO EXPLAIN THE RESIDUAL
#AS A BIAS EACH MOVIE GETS TO DO THIS WE FIND THE MEAN 
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+5))
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+5))
final_predicted_ratings <-
  validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(final_predicted_ratings, validation$rating)






#the predicted chart
cbind(validation,final_predicted_ratings)%>%
  ggplot(aes(final_predicted_ratings))+
  geom_density()
#the residual after our final prediction
cbind(validation,final_predicted_ratings)%>%
  mutate(residual = rating-final_predicted_ratings)%>%
  ggplot(aes(residual))+
  geom_density()
#mean and sd of residual
mean(validation$rating - final_predicted_ratings)
sd(validation$rating - final_predicted_ratings)

