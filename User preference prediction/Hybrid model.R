library(recommenderlab)
library(tidyverse)
library(data.table)

##Importing datalibrary(readr)
website_wata <- read_csv("website_wata.csv")
View(website_wata)

web<- website_wata[, 1:2]
View(web)

web<- data.table(web)
head(web)

web[, value := 1]
web_wide <- reshape(data = web,
                      direction = "wide",
                      idvar = "Page Views",
                      timevar = "Session Duration",
                      v.names = "value")

head(web_wide[, 1:5, with = FALSE])

vector_users <- web_wide[, "Page Views"]
web_wide[, "Page Views":= NULL]

setnames(x = web_wide,
         old = names(web_wide),
         new = substring(names(web_wide), 7))

matrix_wide <- as.matrix(web_wide)
rownames(matrix_wide) <- vector_users

head(matrix_wide[, 1:6])

matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide, "binaryRatingMatrix")
ratings_matrix

image(ratings_matrix[1:50, 1:50], main = "Binary rating matrix")

Page_viewers <- colCounts(ratings_matrix)
qplot(Page_viewers) + stat_bin(binwidth = 100) + ggtitle("Distribution of the 
number of viewers")

#Building the model

which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]

recc_model <- Recommender(data = recc_data_train,
                         method = "IBCF",
                         parameter = list(method = "Jaccard"))
class(recc_model@model$sim)
dim(recc_model@model$sim)

image(recc_model@model$sim)
