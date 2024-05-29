library(data.table)
setwd("~/Desktop/R project_Movie Recommnedation System /DataSet")

# read the data
movies_data = fread("movies.csv")
ratings_data = fread("ratings.csv",stringsAsFactors = FALSE)

# View the data
View(movies_data)
View(rating_data)
# length(movies_data$movieId)
# length(ratings_data$userId)

# Getting some info about the data
summary(movies_data) # the statistics on id isnt useful as id is just an index
summary(ratings_data)


# remove unnecessary columns : rating_data&timestamp
ratings_data = ratings_data[, timestamp := NULL] # := modifies the data.table directly without creating a copy which is both speed and memory efficient
# View(ratings_data)



# Q : WHAT IS THE MOST OCCURING RATING?
# A : WE USE A HIST
library(ggplot2)

ggplot(ratings_data,aes(x=rating)) + 
  geom_histogram(bindwidth = 5)



# Q : WHICH GENRE HAS THE MOST RATING?
# A : to do this , first we need to create sql db and then 
      #join the tables using sql 

# install.packages("duckdb")
# install.packages("dplyr")
# install.packages("ggplot2")
library(dplyr, quietly = T)
library(duckdb, quietly = T)
library(ggplot2, quietly = T)

# Create a DUCKDB database called Movies
con <- dbConnect(duckdb::duckdb(), dbname = "Movies_Ratings.db")
# Create a "MOVIES" table and add the tuples from the movies_data CSV file 
dbWriteTable(con, "MOVIES", movies_data, row.names = FALSE)
# Create a "RATINGS" table and add the tuples from the ratings_data CSV file 
dbWriteTable(con, "RATINGS", ratings_data, row.names = FALSE)


# Join the two tables
movies_rating_data = inner_join(movies_data , rating_data , by = "movieId")
View(movies_rating_data)


# Q : WHAT IS THE AVG RATING FOR EACH FILM
# A : Get the avg rating for each film
film_avg_rating = movies_rating_data [ , .( avg_rating = round( mean(rating) ,digits = 1 ) ) ,by = factor(movieId) ]
View(film_avg_rating)
summary(film_avg_rating)

# film_avg_rating will have two colns : factor , rating , convert the name from "factor" to "movieId"
names(film_avg_rating)[names(film_avg_rating) == "factor"] <- "movieId"
View(film_avg_rating)
length(film_avg_rating$movieId)
# NB : we can't do use := cuz this means we want to add avg_rating column to 
# the movies_ratings_data table , so for each row we will have the avg_rating
# which means we will have replicates ( we will have replicates cuz 
# we are adding to the original table thus the new avg_rating column will have
# same number of rows )


# Q : WHAT IS THE AVG RATING FOR EACH "SPECIFIC" GENRE
# A : Get the avg rating for each genre
genres_avg_rating = movies_rating_data [ , .( avg_rating = round( mean(rating) ,digits = 1 ) ) ,by = factor(genres) ]
View(genres_avg_rating)
names(genres_avg_rating)[names(genres_avg_rating) == "factor"] <- "genres"
length(genres_avg_rating$genres)  # 918



# Q : HOW MANY USERS ARE RATING EACH FILM
film_count_ratings= movies_rating_data [ , .N ,by = factor(movieId) ]
head(film_count_ratings)
View(film_count_ratings)
summary(film_count_ratings)
# we can see that each film is being rated by too many users which makes our
# model more robust 



 
# reduce the specific-genre-names to a more general ones  (CLEANING PROCESS)
# ex : Adventure/Animation/Children -> Adventure
genres_avg_rating[,genres := as.character(genres)]
genres_avg_rating[,common_genre:= sapply(strsplit(genres,"\\|"), "[[", 1) ]
View(genres_avg_rating)





# Q : WHAT IS THE AVG RATING FOR EACH "GENERAL" GENRE
# A : Then calculate the avg rating for each genre and plot
common_genre_rating = genres_avg_rating[, .(avg_rating = round(mean(avg_rating),1))  , by = factor(common_genre)]
View(common_genre_rating)
names(common_genre_rating)[names(common_genre_rating) == "factor"] <- "genres"
View(common_genre_rating)


common_genre_rating$genres <- as.character(common_genre_rating$genres)
barplot(height = common_genre_rating$avg_rating , 
        names.arg= common_genre_rating$genres ,
        xlab = "Genres",
        ylab = "Average Rating",
        col = "skyblue",
        las = 2,
        ylim = c(0, max(common_genre_rating$avg_rating) + 0.5)
        )

# as we can see , all the generic genres are highly rated , so 
# we expect that our model recommend films that belong to several
# genres, there is no bias , but if we were to plot the specific-genres 
#  ratings then we will find bias (we wont plot specific-genres-ratings cuz
# there are 1779 specific-genres which doesnt fit in our plot )





# so we have about 1779 specific genres, 20 general genres,  83239 movies , 
# and around 33 Million ratings
# Also each film is being rated by hundreds and thousands of people which 
# indicates that our model will be robust





# Q : IS THERE CORRELATION BTW GENRE AND RATING ? 
# A :  Plot correlation between genre and rating
genres_avg_rating$common_genre = as.factor(genres_avg_rating$common_genre)
genre_rating_correlation = lm(avg_rating ~ common_genre , data = genres_avg_rating) 

plot(genre_rating_correlation)
plot(genre_rating_correlation , which = 1)
plot(genre_rating_correlation , which = 2)
plot(genre_rating_correlation , which = 3)

plot(genre_rating_correlation , which = 5)


residuals <- residuals(genre_rating_correlation)
summary(residuals) # we can see that residuals follow N dist which is a positive sign that our assumption of having a correlation is right
sum(is.na(residuals)) # to check if there are NA




# Q : ARE THERE HIGH LEVERAGE POINTS , IF SO , DO THEY IMPACT THE CORRELATION,
# I.E, IF WE REMOVE THE HIGH LEVERAGE POINTS , WILL THE CORRELATION GET BETTER?


# Definition:
#   High leverage points are observations (data points) that have extreme values of predictor variables (independent variables).
# These points disproportionately affect the regression model because they exert significant leverage on the estimated regression coefficients.
# Characteristics:
#   High leverage points tend to have extreme values in one or more predictor variables.
# They can pull the regression line closer to or farther away from them, affecting the slope and intercept of the line.
# High leverage points are not necessarily outliers; they can be valid data points,if so keep them in the model
leverage = hatvalues(genre_rating_correlation)
high_leverage_points <- which(leverage > (2 * mean(leverage))) # setting a threshold to what we consider high leverage
print(high_leverage_points)
high_leverage_data = genres_avg_rating[high_leverage_points,]
View(high_leverage_data)

# We can see that high_leverage_points are valid/genuine data points , 
#  not outliers or potential errors , so we keep them


# Though high_leverage_pts are valid points and we wont remove them,
# we will assess them for experiment purposes only

# plot the data to visualize the leverage points
plot(genres_avg_rating$common_genre, genres_avg_rating$avg_rating,
     main = "Leverage Points in Genre Rating Data",
     xlab = "Common Genre",
     ylab = "Average Rating",
     pch = 20, frame = FALSE,
     las = 2)
points(high_leverage_data$common_genre, high_leverage_data$avg_rating,
       pch = 4, col = "red")


# Assess the impact of high leverage points on the model
# Fit the model without high leverage points
genre_rating_correlation_no_high_leverage <- lm(avg_rating ~ common_genre, 
                                                data = genres_avg_rating[-high_leverage_points, ])

summary(genre_rating_correlation)
summary(genre_rating_correlation_no_high_leverage)
# We can see that with and without the high_leverage_points ,  
# R is small and p-value > alpha of F-stat which implies that we do not
# reject H0 , thus no correlation 
# Thus the existance and the non-existance of high_leverage_pts doesnt 
# change anything

# This implies that we cannot guess the rating of a film based on a 
# common_genre , so no predicting model will work






# We analyzed the data  , now time to build the recommendation model





























# # what is sapply and "[[" and why we do [[1]][1] in 
# x = (strsplit(genres_avg_rating$genres[1] , "\\|"))
# x[[1]][2]





# PDF : x = ratings , y= nb of times that rating exist 
# so this will give us the probability that a specific rating will occur
# Useful maybe when we want to add the feature where we add to the model
# a new film and it gives us the rating based on the film genre and tags maybe
# ggplot(data.frame(x = movies_rating_data$rating) , aes(x=x)) +
#   stat_function(fun = dnorm , 
#                 args = list( mean = mean(movies_rating_data$rating) , 
#                             sd = sd(movies_rating_data$rating)  ) ,
#                 color = "blue") +
#   labs(title = "PDF" , x = "Ratings" , y = "Density")



# These check for NA values
#sum(is.na(common_genre_rating$genres))
# sum(is.na(common_genre_rating$avg_rating))
# common_genre_rating <- na.omit(common_genre_rating) # this deletes missing values
