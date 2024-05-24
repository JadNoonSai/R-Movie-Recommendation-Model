library(data.table)
setwd("~/Desktop/R project_Movie Recommnedation System /DataSet")

# read the data
movies_data = fread("movies.csv")
ratings_data = fread("ratings.csv",stringsAsFactors = FALSE)

# Getting some info about the data
View(movies_data)
View(rating_data)
# length(movies_data$movieId)
# length(ratings_data$userId)

summary(movies_data) # the statistics on id isnt useful as id is just an index
summary(ratings_data)


# remove unnecessary columns : rating_data&timestamp
ratings_data = ratings_data[, timestamp := NULL] # := modifies the data.table directly without creating a copy which is both speed and memory efficient
View(ratings_data)


# display which rating we have the most
library(ggplot2)

ggplot(ratings_data,aes(x=rating)) + 
  geom_histogram(bindwidth = 5)


# display which genre has the most rating  , to do this we need to join the tables using sql 
# Better use sql for query operations cuz the db is so large

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


# first , we need to join the two tables 
movies_rating_data = inner_join(movies_data , rating_data , by = "movieId")
View(movies_rating_data)

# Get the avg rating for every film
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



# Get the avg rating for each genre
genres_avg_rating = movies_rating_data [ , .( avg_rating = round( mean(rating) ,digits = 1 ) ) ,by = factor(genres) ]
View(genres_avg_rating)
names(genres_avg_rating)[names(genres_avg_rating) == "factor"] <- "genres"
length(genres_avg_rating$genres)  # 1779



# we check how many users are rating each film 
film_count_ratings= movies_rating_data [ , .N ,by = factor(movieId) ]
head(film_count_ratings)
summary(film_count_ratings)
# we can see that each film is being rated by too many users which makes our
# model more robust 



 
# reduce the specific-genre-names to a more general ones  (CLEANING PROCESS)
# ex : Adventure/Animation/Children -> Adventure
genres_avg_rating[,genres := as.character(genres)]
genres_avg_rating[,common_genre:= sapply(strsplit(genres,"\\|"), "[[", 1) ]
View(genres_avg_rating)



# Then calculate the avg rating for each genre and plot
common_genre_rating = genres_avg_rating[, .(avg_rating = round(mean(avg_rating),1))  , by = factor(common_genre)]
View(common_genre_rating) # what is row 20






# so we have about 1779 specific genres, 20 general genres,  83239 movies , 
# and around 33 Million ratings
# Also each film is being rated by hundreds and thousands of people which 
# indicates that our model will be robust





# # what is sapply and "[[" and why we do [[1]][1] in 
# x = (strsplit(genres_avg_rating$genres[1] , "\\|"))
# x[[1]][2]







