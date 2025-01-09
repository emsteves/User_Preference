
library(recommenderlab)
library(tidyverse)

#Importing Data set

data_package <- data(package = "recommenderlab")
data_package$results[, "Item"]

# In our work , we will use the MovieLense dataset; the data is about movies. The 
table contains the ratings that the users give to movies. Let's load the data and take a 
look at it:

data(MovieLense)
View(MovieLense)
 
##Each row of MovieLense corresponds to a user, and each column corresponds to a 
movie. There are more than 943 x 1664 = 1,500,000 combinations between a user and 
a movie. Therefore, storing the complete matrix would require more than 1,500,000 
cells. However, not every user has watched every movie. Therefore, there are fewer 
than 100,000 ratings, and the matrix is sparse. The recommenderlab package allows 
us to store it in a compact way.

# Going forward we will explore the data in detail using class

 class(MovieLense)
 
  methods(class = class(MovieLense))
  
## Computing the similarity matrix
 Collaborative filtering algorithms are based on measuring the similarity between 
users or between items. For this purpose, recommenderlab contains the similarity 
function. The supported methods to compute similarities are cosine, pearson,  
and jaccard.
 For instance, we might want to determine how similar the first five users are with 
each other. Let's compute this using the cosine distance:
  
similarity_users <- similarity(MovieLense[1:4, ], method =  
                                   "cosine", which = "users")
The similarity_users object contains all the dissimilarities. Let's explore it:

class(similarity_users)
 ## [1] "dist"
 As expected, similarity_users is an object containing distances. Since dist is a 
base R class, we can use it in different ways. For instance, we could use hclust to 
build a hierarchic clustering model.
 We can also convert similarity_users into a matrix and visualize it:
 
 as.matrix(similarity_users)
 
  image(as.matrix(similarity_users), main = "User similarity")
  
##Using the same approach, we can compute and visualize the similarity between the 
f
 irst four items:
 
 similarity_items <- similarity(MovieLense[, 1:4], method =  
"cosine", which = "items")
 as.matrix(similarity_items)
 
 image(as.matrix(similarity_items), main = "Item similarity")
 
##Recommendation models
 The recommenderlab package contains some options for the recommendation 
algorithm. We can display the model applicable to the realRatingMatrix objects 
using recommenderRegistry$get_entries:

 recommender_models <- recommenderRegistry$get_entries(dataType = 
"realRatingMatrix")
 The recommender_models object contains some information about the models. First, 
let's see which models we have:
  
names(recommender_models)

lapply(recommender_models, "[[", "description")

## Out of them, we will use IBCF and UBCF.
The recommender_models object also contains some other information, such as its 
parameters:
  
  recommender_models$IBCF_realRatingMatrix$parameters

## Exploring The Data

MovieLense is a realRatingMatrix object containing a dataset about movie ratings. 
Each row corresponds to a user, each column to a movie, and each value to a rating.
there 
are some generic methods that can be applied to realRatingMatrix objects. We can 
extract their size using dim:

  dim(MovieLense)
slotNames(MovieLense)
class(MovieLense@data)
dim(MovieLense@data)

vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

table_ratings <- table(vector_ratings)
table_ratings

According to the documentation, a rating equal to 0 represents a missing value, so 
we can remove them from vector_ratings:

vector_ratings <- vector_ratings[vector_ratings != 0]

Now, we can build a frequency plot of the ratings. In order to visualize a bar plot 
with frequencies, we can use ggplot2. Let's convert them into categories using factor 
and build a quick chart:

vector_ratings <- factor(vector_ratings)

 Let's visualize their distribution using qplot:

qplot(vector_ratings) + ggtitle("Distribution of the ratings")

## Tracking viewed movies

views_per_movie <- colCounts(MovieLense)

table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
)
table_views <- table_views[order(table_views$views, decreasing = 
                                   TRUE), ]

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +  
  geom_bar(stat="identity") + theme(axis.text.x =  
                                      element_text(angle = 45, hjust = 1)) + ggtitle("Number of views  
of the top movies")

#average rating

average_ratings <- colMeans(MovieLense)

qplot(average_ratings) + stat_bin(binwidth = 0.1) + 
  ggtitle("Distribution of the average movie rating")

average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant) + stat_bin(binwidth = 0.1) + 
  ggtitle(paste("Distribution of the relevant average ratings"))

##visualizing matrix

image(MovieLense, main = "Heatmap of the rating matrix")

image(MovieLense[1:10, 1:15], main = "Heatmap of the first rows and 
columns")

min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_movies

image(MovieLense[rowCounts(MovieLense) > min_n_movies, 
                 colCounts(MovieLense) > min_n_users], main = "Heatmap of the top users 
and movies")

ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,  
                             colCounts(MovieLense) > 100] 
ratings_movies

average_ratings_per_user <- rowMeans(ratings_movies)

##Normalizing Data

ratings_movies_norm <- normalize(ratings_movies)

sum(rowMeans(ratings_movies_norm) > 0.00001)

##image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies, 
                          colCounts(ratings_movies_norm) > min_users], main = "Heatmap of the 
top users and movies")

## binarizing the data

ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)

image(ratings_movies_watched[rowCounts(ratings_movies) > min_movies_binary,colCounts(ratings_movies) > min_users_binary], main = "Heatmap 
of the top users and movies")

ratings_movies_good <- binarize(ratings_movies, minRating = 3)

#IBCF
## Training and Test set

which_train <- sample(x = c(TRUE, FALSE), size = nrow(ratings_movies), 
                      replace = TRUE, prob = c(0.8, 0.2))
head(which_train)

recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

##Building The Recommender Model
##checking parameters

recommender_models <- recommenderRegistry$get_entries(dataType = 
                                                        "realRatingMatrix")
recommender_models$IBCF_realRatingMatrix$parameters
## the model
recc_model <- Recommender(data = recc_data_train, method = "IBCF", 
                          parameter = list(k = 30)) 
recc_model

class(recc_model)

## exploring the model

model_details <- getModel(recc_model)
model_details$description
model_details$k

class(model_details$sim)

dim(model_details$sim)

n_items_top <- 20

image(model_details$sim[1:n_items_top, 1:n_items_top],  
      main = "Heatmap of the first rows and columns")

row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

col_sums <- colSums(model_details$sim > 0)

qplot(col_sums) + stat_bin(binwidth = 1) + ggtitle("Distribution of 
the column count")

which_max <- order(col_sums, decreasing = TRUE)[1:6]
rownames(model_details$sim)[which_max]

##Applying the model on the test set

n_recommended <- 6

recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted

class(recc_predicted)
slotNames(recc_predicted)

recc_predicted@items[[1]]

recc_user_1 <- recc_predicted@items[[1]] 
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]
movies_user_1

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

recc_matrix[, 1:4]

##viz

number_of_items <- factor(table(recc_matrix)) 
chart_title <- "Distribution of the number of items for IBCF"

qplot(number_of_items) + ggtitle(chart_title)

number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(names(number_of_items_top),  
                        number_of_items_top)
table_top

IBCF recommends items on the basis of the similarity matrix. It's an eager-learning 
model, that is, once it's built, it doesn't need to access the initial data. For each item, 
the model stores the k-most similar, so the amount of information is small once the 
model is built. This is an advantage in the presence of lots of data.
 In addition, this algorithm is efficient and scalable, so it works well with big rating 
matrices. Its accuracy is rather good, compared with other recommendation models.
 In the next section, we will explore another branch of techniques: user-based 
collaborative filtering.