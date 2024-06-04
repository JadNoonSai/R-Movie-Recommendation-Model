# import recommendation library
library(recommenderlab)

# Load MovieLens data (MovieLens is already implemented in recommenderlab)
data(MovieLense)

# Class MovieLense
class(MovieLense) # MovieLense is an object of RealRatingMatrix class , defined in recommenderlab module

# Overview the data
str(MovieLense) # complicated to understand
str( as(MovieLense,"data.frame") ) # better to understand
str(as(MovieLense, "matrix")) # better to understand as well

slotNames(MovieLense)
View(as(MovieLense, "data.frame"))
View(as(MovieLense, "matrix"))




# NB : as(MovieLense , "data.frame") != as(MovieLense , "matrix")
# "data.frame" : it gives you the original table , the table where you have the 
# cols : id , movie , rating . While , "matrix" gives you the matrix where you have
# rows = user id , columns = movie , and c(i,j) , i.e , cells , repr rating

# Try : 
# View( as(MovieLense ,"data.frame") )
# View( as(MovieLense ,"matrix") )


# EDA : Exploratory Data Analysis , simply : ANALYSIS
image(MovieLense[1:25 , 1:25]) # blank spaces in the graph demostrate the "sparse data" issue
# head(MovieLense) # doesn't work cuz head() only works for data.frame and matrices and RealRatingMatrix doesnt have head() method defined
head( as(MovieLense ,"data.frame") )
head( as(MovieLense,"matrix"))[1:5,1:5] # as function is overwritten in RealRatingMatrix , that's why it knows that the cells will contain ratings


# check the number of ratings for each user
View(as(MovieLense , "data.frame"))
summary(rowCounts(MovieLense)) # # rowCounts(MovieLense) calculates nb of non NA cells for each row , 
# since rows repr users , col repr movies and cells repr ratings , then 
# it counts number of ratings for each user

hist(rowCounts(MovieLense) , main = "Nb of ratings for each user" , 
     xlab = "user" , ylab = "nb of ratings")
hist(rowCounts(MovieLense) , main = "Nb of ratings for each user" , 
     xlab = "user" , ylab = "nb of ratings" , breaks = 400)

# we can see that the first 200 users almost rated the most ,and  specifically ,
# the first 50 users rated the most


# NB : 
# rowCounts(MovieLense) is same as doing : 
# movies = (as(MovieLense , "data.frame"))
# movies = as(movies , "data.table")
# film_count_ratings= movies[ , .N ,by = factor(user) ] # this [, ,] works only for data.table not data.table
# summary(film_count_ratings)

# This indicates that our calculations using our methods are correct




# check the nb of ratings for each movie
summary(colCounts(MovieLense))
# we can see that almost the first 50 movies got the highest number of movies
hist(colCounts(MovieLense) , main = "Nb of ratings for each movie" , 
     xlab = "movie" , ylab = "nb of ratings")
# More specific
hist(colCounts(MovieLense) , main = "Nb of ratings for each movie" , 
     xlab = "movie" , ylab = "nb of ratings", breaks = 30)


# summary(colCounts(MovieLense)) is same as : 
# movies = (as(MovieLense , "data.frame"))
# movies = as(movies , "data.table")
# film_count_ratings= movies[ , .N ,by = factor(item) ]
# summary(film_count_ratings)




# Average User ratings
hist(rowMeans(MovieLense) , col = "red" , main = "Rating Hist" , 
     xlab = "ratings" , ylab = "frequency/density")
summary(rowMeans(MovieLense) )



# Get nb of users
nb_users = dim(MovieLense)[1] # 943 ,# dim(MovieLense)[1] : is the number of users without repetition
# same as : 
length( unique(as(MovieLense , "data.frame")$user) ) 



# Get nb of movies
nb_movies = dim(MovieLense)[2] # 1664


# So our matrix should have 1664 * 943 elements/cells
length(as(MovieLense , "matrix")) # 1569152 = 1664 * 943 elements


# Image a RANDOM part of the data
image( MovieLense[sample(nb_users,25) , sample(nb_movies,25)] ) # sample()  selects randomly 
View( as(MovieLense[sample(nb_users,25) , sample(nb_movies,25)], "matrix") )


# SlotNames of MovieLense : 
slotNames(MovieLense) # slotNames() get the names of the non-staticattr/fields in the MovieLense object
MovieLense@data # "@" is used to access a specific slot/attr/field of an object
class(MovieLense@data)

# NB : the "MovieLense@data" attribute is a matrix (aka sparse rating matrix)
# but with 0 instead of NA
MovieLense@data[2,2]

# NB : 
#  MovieLense object is a S4 object of class realRatingMatrix
# In R OOP ,there are two types of classes : S4 and S3
# S4 is more robust as it is more strict with datatypes


# Check how the movies have been rated
slotNames(MovieLense)
vector_ratings = as.vector(MovieLense@data) # vector simply is a 1D array
unique(vector_ratings)



# Check the count of each rating value
table_ratings = table(vector_ratings)
barplot(table_ratings , ylim = c(0,40000))
help(barplot)
# In our method when we calculated and plot as hist the most occuring rating ,
# we got the same answer , the only thing that is different is that here zero's
# are the most frequent , this is because in our case we didn't deal with NA as zero's
# (cuz we didnt do the matrix) while here we assume NA as 0


# Repeat after removing the un-rated items
vector_ratings2 = vector_ratings[vector_ratings!=0]
table_ratings2 = table(vector_ratings2)
barplot(table_ratings2)
# Now after removing zero's , it is the same as our plot


# Check out the available Recommender Algorithms
algos = recommenderRegistry$get_entries(dataType = "realRatingMatrix")
algos


# Examine the similarity of few users
similarity_users = similarity(MovieLense[1:4 ,] , method = "cosine" , which = "users")
similarity_users
class(similarity_users)
as.matrix(similarity_users) # vs matrix(similarity_users)





# Collaborative Filtering :
#   -UBCF : User-Based Collaborative Filtering ,i.e, recommendation is based on users' similarities
#   -IBCF : Item-Based Collaborative Filtering ,i.e, recommendation is based on items' similarities




# BUILDING THE UBCF COLLABORATIVE FILTERING MODEL : 

# Create an evaluation scheme by splitting the data and specifying other parameters
evlS = evaluationScheme(MovieLense , method = "split" , train = 0.9 , given = 12) # NB : evaluationScheme(method = "split") splits the data into 3 parts : training data , know testing data , unknown testing data
evlS

trg = getData(evlS , "train")
trg


test_known = getData(evlS , "known") 
test_known

test_unknown = getData(evlS , "unknown")
test_unknown



# Create UBCF recommender model with the training data
rcmnd_ub = Recommender(trg , "UBCF")

# Use the model to predict the "known_test"
pred_ub = predict(rcmnd_ub, test_known , type = "ratings")

# Evaluate model accuracy for the "unknown_test" 
acc_ub = calcPredictionAccuracy(pred_ub , test_unknown) # RMSE = 1.248 , not good , i.e , the model didn't identify the optimal solution
as(acc_ub,"matrix")
# RMSE : Root Mean Square Error. The nearer the value to 0 the better the prediction model (standard: btw 0.2 and 0.5)
# MSE  : Mean Square Error
# MAE  : Mean Absolute Error
# For all of these , the nearer to 0 the better 



# Compare the results
as(test_unknown , "matrix")[1:8 , 1:5]
as(pred_ub , "matrix")[1:8 , 1:5]





# --------------------------------------------------------------------------

# BUILDING THE IBCF COLLABORATIVE FILTERING MODEL 
rcmnd_ib = Recommender(trg , "IBCF")
pred_ib = predict(rcmnd_ib , test_known , type = "ratings")
acc_ib = calcPredictionAccuracy(pred_ib , test_unknown)
acc_ib
as(acc_ib , "matrix")

# as(test_unknown , "matrix")[1:8 , 1:5]
# as(pred_ib , "matrix")[1:8 , 1:5] # strange result !! All are NA

----------------

# compare the difference between IBCF and UBCF model
rbind(UBCF = acc_ub , IBCF = acc_ib) # we can see that ibcf is much better


----------------
# Get the top recommendations given by UBCF and IBCF
pred_ub_top = predict(rcmnd_ub , test_known)
pred_ub_top

movies = as(pred_ub_top, "list") # this will give a list of top 10 recommended movies for each user
View(movies)
movies[1] # top 10 recommended movies for FIRST person



pred_ib_top = predict(rcmnd_ib , test_known)
pred_ib_top

movies = as(pred_ib_top, "list") # this will give a list of top 10 recommended movies for each user
View(movies)
movies[2] # top 10 recommended movies for FIRST person





# Questions : 
# 1)how rowCounts and colCounts work# 1)hotest_unknownw rowCounts and colCounts work
# 2) are the colCounts and rowCounts same results as i got when i did them manually , 
#    using MovieLens[, ,] , if no , maybe because i counted NA values 
# 3) evlS = evaluationScheme(Movie_Lense , method = "split" , train = 0.9 , given = 12) 
# 4) why evaluationScheme(method = "split") splits the data into 3 parts : training data , know testing data , unknown testing data







