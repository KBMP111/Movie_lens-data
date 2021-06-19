---
title: "EDX Capstone"
author: "Kabwe Mpundu"
date: "18/08/2019"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### MOVIE RECOMMENDATION MODEL

## Introduction
A movie recommendation system was built using the movielenz dataset. The dataset contained tables with user ratings for different movies. The goal was to use these rating to train a machine learn model that would accurately predict a test set. The challenge meant creating a simple yet effective model to accuratly predict the ratings for an unknown dataset.

This challenge of was inspired by the famous Netflix challenge, were a $1, 000, 000  reward was offered to a model that would improve the existing recommendation engines by at least 10%. The data used in this project is called the Movielens dataset it has 10, 000, 000 entries and is thus a substantive dataset. Users gave a movie a rating between  0 and 5.

Besides movie recommendation, similar problems to this one can be used to recommend goods to online shoppers and websurfers. The applications of a recommendation model have been endless and widespread, they have saved users time and increased sales for suppliers and retailers. 

## Methodology
To achieve the desired accuracy a regularised linear model was fit to a training set and tested on the test set. The regularised linear model was inspired due to the low accuracy of generalised linear model and a lack of computing power to create a principal component analysis.

Secondly, the linear model was justified due to the nature of the data. The analaysis begun with the naive assumption that the mean rating would be a good model for prediction. With this approach, an analysis of the residuals revealed a more intituiteve approach would be required. The most obvious strategy was to consider the bias for specific movies. That meant average the ratings for each movie and using the average as a predictor for the rating. This method led to an improvement in the accuracy.

y = mu 
--------

With the bias of specific movies noted and an improved accuracy the desried level of precision remained un met. In address the cause of the remaining residual it was noted that, there was user bias, The user bias could account for differences in the expected rating for movies. This approach was able to improve the accuracy further.

y = mu + b_i
---

With the user and movie biases considered the model was near the desired level of accuracy but it was noted the significant residuals remained on obsure and unpopular movies. A logical explantion to this issue was that there was more uncertainty in movies and users with few ratings. Thus a penalty term was added to the model to vary the weeighting of the user and movie effects based on the number of ratings.

y = mu + b_i + b_u
---

To compute the best penalty term cross validation was used, that was several penalty terms were used and the most accuracte was selected to use for final test data set.

$$ y = \frac{1}{l+N} \ \sum_{n=1}^{N} b_i \ + \frac{1}{l+N}\sum_{m=1}^{M} b_u +\epsilon $$ 
---

## Results
The data was downloaded from movielens website and the required packages were installed.
```{r data, echo=FALSE, results='hide'}

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
```
The data was complex and diverse:
As shown by the number of users
```{r The number of unique Movies and users, echo= FALSE}
#The number of unique users
user_count <- edx$userId %>% unique() %>% length()
user_count
```
As well as the number of unique movies
```{r The number of unique movies, echo=FALSE}
#the number of unique movies
movie_count <- edx$movieId %>% unique() %>% length()
movie_count
```
# Model
Using the formula shown above in the methodology section the final code was as follows;
```{r Final Model}
library(dplyr)
mu = mean(edx$rating)
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
perfected_ratings <-ifelse(final_predicted_ratings>5,5,ifelse(final_predicted_ratings<0.5,0.5,final_predicted_ratings))

RMSE(perfected_ratings, validation$rating)
```
To show the nature of the Here is a plot of the predicted ratings in blue and the actual ratings in red. The vertical black line indicates the mean of the distribution. 
```{r The predicted distribution, echo= FALSE}
#the predicted chart
M_bias <- edx %>%
  left_join(b_i, by = 'movieId')%>%
  mutate(pred = mu + b_i)%>%
  select(pred)
U_bias <- edx %>%
  left_join(b_i, by = 'movieId')%>%
  left_join(b_u, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u)%>%
  select(pred)
mu <- mean(edx$rating)
rate_freq <-  edx %>%group_by(rating)%>% 
   summarise(count = n())

ggplot()+
   geom_smooth(data = rate_freq, aes(x=rating,y = count),se= FALSE, color = 'red')+
  geom_vline(xintercept = mu)+
  geom_freqpoly(data = U_bias, aes(pred), color = 'blue')
  

```
It has been shown in the diagram above, predicted ratings range from -1 to 6.2. The permitted range is from 0.5 to 5. Thus our final result has adjusted all values below 0.5 to 0.5 and all values above 5 to 5.

An analysis of the residual after fitting the model suggested the distribution was normal and centered at zero. This observation suggested the error is random, coupled with the fact that the RMSE was within the target of 0.8649 the model building excercise concluded.
```{r Residual after completing model}
#the residual after our final prediction
cbind(validation,perfected_ratings)%>%
  mutate(residual = rating-perfected_ratings)%>%
  ggplot(aes(residual))+
  geom_density()

```
The mean of the residual is almost zero and the standard devaiton of the ratings is less than one, indicating our ratings are mostly right and rarely differ with the actual ratings by more than one star
```{r Mean and Standard Deviation of Residual}
#mean and sd of residual
mean(validation$rating - perfected_ratings)
sd(validation$rating - perfected_ratings)
```



## Conclusion
Specific Movies received similar ratings from users and Specific users gave similar ratings to the movies they rated. This was most accurate for users who rated movies more frequently and for movies which were rated more frequently. The error recorded was less than a minimal and unbiased. 

Give these findings the error can still be reduced by More analysis can be done to explain the remaining residual error.


