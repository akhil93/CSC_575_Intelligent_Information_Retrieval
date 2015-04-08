# Set the Working Directory to the folder where the data is stored 
#setwd("C:/Users/Akhilkumar/Desktop/depaul/CSC 575/project/ml-100k/")

# Load the data
u.data <- read.delim("u.data", sep = "\t", header = FALSE)
colnames(u.data) <- c("User.ID","Movie.ID","Ratings","Time")

# Creating a User X Movies Matrix
spm <- matrix(NA, max(u.data$User.ID), max(u.data$Movie.ID))
for (i in 1:length(u.data[,1]))
{
  uid <- u.data[i,1]                  # For Each User
  mid <- u.data[i,2]                  # For Each Movie
  spm[uid,mid] <- u.data[i,3]         # Corresponding Rating
}

# Loading the u.item data which has the movies names
u.item <- read.delim("u.item", sep = "|", header = FALSE)
colnames(spm) <- u.item[,2]

# Transforming the NA in the User X Movies Matrix to Zero's
spm[is.na(spm)] <- 0


# This is the function which takes only one argument and display 
#top 5 recommended movie based the movies which that user has rated.

recommend_user <- function(x){
  #if you haven't installed "lsa" package uncomment the below line.
  #install.packages("lsa")
  library(lsa)
  # user which we are going to predict the movie ratings
  target_user <- spm[x,]  
  user_similarity <- matrix(NA, nrow(spm),1)        
  for(i in 1:nrow(spm)){
    user_similarity[i,1] <- cosine(spm[i,], target_user)
  }
  
  # this stores the indexes of the movie which that user has not rated
  zero.movie_index <- which(target_user == 0)
  movies_actually_rated <- which(target_user != 0)
  
  # Creating a seperate matrix which has all the unrated movies of that user
  movies.notrated <- spm[,zero.movie_index]
  
  # Creating a new weighted matrix done by multiplying user similarities and movie ratings
  weighted_matrix <- matrix(NA, nrow(movies.notrated), ncol(movies.notrated))
  for(i in 1:nrow(user_similarity)){
    weighted_matrix[i,] <- user_similarity[i,1] * movies.notrated[i,]
  }
  
  # This is the sum of ratings of each movie
  total.movie_rating <- colSums(weighted_matrix)
  
  # This is the sum of similarity measures of users who have rated a particular movies
  ss.users_rated <- data.frame()
  for(i in 1:ncol(weighted_matrix)){
    # For each movies extracting indices where the movie is actually rated
    temp_index <- which(weighted_matrix[,i] >0)
    ss.users_rated[1,i] <- sum(user_similarity[temp_index])
  }
  
  # The predicted movie rating
  predicted.ratings <- total.movie_rating/ss.users_rated
  colnames(predicted.ratings) <- colnames(spm[,zero.movie_index])
  ordered.index <- order(predicted.ratings, decreasing =T)
  top5_index <- ordered.index[1:5]
  
  movies_actually_rated <- which(target_user != 0)
  rated.movies <- target_user[movies_actually_rated]
  movies.predicted <- predicted.ratings[movies_actually_rated]
  accuracy <- sum(abs(rated.movies - movies.predicted))/length(rated.movies)
  print(paste0("The Mean Accuracy Error is: ",round(accuracy, 4)))
  print(paste0("Top 5 Recommended Movie are:"))
  final <- predicted.ratings[,top5_index]
  for (i in 1:5){
    print(paste0(colnames(final[i])))
  }
  
}
recommend_user(48)

#####################################################################

#Clustering

colnames(u.item) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
#Remove unnecessary attributes
u.item$ID = NULL
u.item$ReleaseDate = NULL
u.item$VideoReleaseDate = NULL
u.item$IMDB = NULL

# Saving only unique values.
movies = unique(u.item)

#calculate the distances.
distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method = "complete")
clusterGroups = cutree(clusterMovies, k = 10)
recommend_user_Clust <- function(x){
  user <- spm[x,]
  highest_rated_movie <- which.max(user)
  CG <- clusterGroups[highest_rated_movie]
  clusterX = subset(movies, clusterGroups==CG)
  final_movies <- clusterX$Title[1:10]
  for (i in 1:5){
    print(paste0(final_movies[i]))
  }
}
recommend_user_Clust(48)

###############################################################
# item based

movies_sim <- matrix(NA, nrow = ncol(spm), ncol = ncol(spm))
for(i in 1:ncol(spm)){
  for(i in 1:ncol(spm)){
    movies_sim[i,j] <- cosine(spm[,i], spm[,j])
  }
}
colnames(movies_sim) <- u.item[,1]
samp <- movies_sim
for(i in 1:ncol(samp)){
  samp[,i] <- order(samp[,i], decreasing = T)
}
recommend_user_ib <- function(x){
  #library(lsa)
  # user which we are going to predict the movie ratings
  target_user_i <- spm[x,]  
  movies_actually_rated_i <- which(target_user_i !=0)
  na.i.movies <- which(target_user_i == 0)
  pred_i <- data.frame()
  
  for(i in 1:ncol(spm)){
    temp_index <- i
    top_5_similar_movies <- samp[2:21,i]
    ratings_i <- max(spm[x,top_5_similar_movies])
    pred_i[1,i] <- ratings_i
  }
  p_order <- pred_i[,na.i.movies]
  for(i in 1:ncol(p_order)){
    p_order[,i] <- order(p_order[,i], decreasing = T)
  }
  colnames(p_order) <- colnames(spm[,na.i.movies])
  final_movies_it <- colnames(p_order[1,1:5])
  print(paste0("Top 5 Recommended Movie are:"))
  for(i in 1:5){
    print(paste0(final_movies_it[i]))
  }
  rated_movies_ac <- spm[x,movies_actually_rated_i]
  pred_movies_ac <- p_order[1,movies_actually_rated_i]
  accuracy <- sum(abs(rated_movies_ac - pred_movies_ac))/length(rated_movies_ac)
  print(paste0("The Mean Accuracy Error is: ",round(accuracy, 4)))
}
recommend_user_ib(48)
