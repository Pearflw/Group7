
#title: "429final"

rm(list = ls())
library(data.table)
library(dplyr)
library(ggpubr)
library(arules)
library(tidyr)
library("readxl")
library("gridExtra")
library("stringr")
library ("recommenderlab")
library(reshape2)
book_tags <- fread('book_tags.csv')
tags <- fread('tags.csv')
ratings <- fread('ratings.csv')
books <- read_excel("all_books.xlsx")

##################
# PRE PROCESSING # includes subset selection
##################

#To achieve a computationally feasible dataset, we first sample from the
#ratings dataset 3,000 users
#Then, we remove all books that have less than 30 ratings per book
#and all users that have rated less than 30 books

#Define a function to make the dataset have good distribution
Good_subset<- function(dataset){
  I=T;
  while (I) {
    dataset[, 'Number of books this user rated' := .N, .(user_id)] 
    dataset[, 'Popularity(Number of ratings)' := .N, .(book_id)]
    N_min <- min(dataset$'Number of books this user rated')
    P_min <- min(dataset$'Popularity(Number of ratings)')
    if (N_min < 30 ){
      dataset <- dataset[dataset$`Number of books this user rated`>=30,]
    }else{
      if (P_min < 30 ){
        dataset <- dataset[dataset$`Popularity(Number of ratings)`>=30,]
      }else{
        I=F;  }}}
  return(dataset)
}

y = data.frame(table(ratings$user_id) %>% sort(decreasing =T))
colnames(y) = c("user_id", "Number of books this user rated")
set.seed(7)
indices_y = sample(1:nrow(y), size = 3000, replace = FALSE)
y <- y[indices_y,]
y$user_id <- as.numeric(as.character(y$user_id))
ratings = merge(ratings, y, by="user_id")
ratings <-Good_subset(ratings)

# Adjust accordingly the books dataset
#select columns
books <- books %>% select(id, book_id, books_count,authors, original_publication_year, title, language_code, average_rating,	work_ratings_count)
#clean language
table(books$language_code)
books$language_code[books$language_code == "en-CA"] <- "en"
books$language_code[books$language_code == "en-GB"] <- "en"
books$language_code[books$language_code == "en-US"] <- "en"
books$language_code[books$language_code == "eng"] <- "en"

# to create a file with book and its id being combined
aaa=books
colnames(aaa)[2]='goodreads_book_ID'
colnames(aaa)[1]='book_id'
aaa$book=paste0(aaa$book_id,"_",aaa$title) 
books_with_id=select(aaa,book_id,book)

#to give a table of books we choose
The_2000_books<-data.frame(table(ratings$book_id) %>% sort(decreasing =T))
colnames(The_2000_books) = c("book_id", "Popularity(Number of ratings)")
books_with_title <- merge(x=The_2000_books,y=books_with_id,by="book_id") %>% select(book_id,book)
books_with_title$book_id <- as.numeric(as.character(books_with_title$book_id))

#input ourselves into the rating dataset (used for user evaluation)
# user 77777
user_c <- head(ratings,30)
user_c$user_id= 77777
user_c$book_id <- c(4,95,11,1204,172,180,2144,43,48,5,80,560,1725,10,1015,121,13,55,852,171,193,47,42,736,92,63,33,61,35,8)
user_c$rating <- c(5,5,5,5,5,5,5,5,5,5,5,4,2,5,5,5,5,5,5,4,4,2,5,5,5,4,4,4,3,2)
user_c$`Number of books this user rated`<-30
# see the books rated by user 77777
see_user_c <-merge(user_c, books_with_title, by="book_id")

#user 88888 
user_a <- head(ratings,30)
user_a$user_id= 88888
user_a$book_id <- c(4,95,323,380,1009,14,1454,200,478,125,269,97,284,103,1410,924,63,1320,451,1203,564,590,155,2029,658,804,1725,130,194,977)
user_a$rating  <- c(5,5,5,5,4,4,4,4,4,3,3,2,1,5,5,5,4,3,3,2,2,1,4,3,3,3,2,1,1,1)
user_a$`Number of books this user rated`<-30
# see the books rated by user 88888
see_user_a <-merge(user_a, books_with_title, by="book_id")

#combine us with the rest
ratings<-rbind(ratings,user_c,user_a)

# random user 11405
random_user <-ratings[ratings$user_id == 	11405]
random_user$book_id <- as.numeric(as.character(random_user$book_id))
books_with_title$book_id <- as.numeric(as.character(books_with_title$book_id))
random_user <- merge(random_user, books_with_title, by="book_id")


# Now Clean the 'books' dataset for preliminary analysis
# from the 10k books, seect only the 2430 according to the ratings selection
# rename book_id to goodreads_book_id to match later with the tags files
names(books)[names(books)=="book_id"] <- "goodreads_book_id"
# rename id to book_id to match the books_with title
names(books)[names(books)=="id"] <- "book_id"
books <- merge(books, books_with_title, by= "book_id")
books <- books[,-10]

#########################################
# Preliminary analysis on books dataset #
#########################################

theme_set(theme_pubr())
temp <- within(books, language_code <- factor(language_code,levels=names(sort(table(language_code), decreasing=FALSE))))
ggplot(temp,aes(x=language_code))+geom_bar()
plot1 <-ggplot(data.frame(temp), aes(x=language_code,fill=language_code)) +
  geom_bar()+
  labs(x = "language", title = "Including English") +
  theme_pubclean()+ 
  theme(legend.position = "none")+
  coord_flip()
books_wo_english <-books[!grepl("en", books$language_code),]
temp1 <- within(books_wo_english, language_code <- factor(language_code,levels=names(sort(table(language_code), decreasing=FALSE))))
plot2<-ggplot(data.frame(temp1), aes(x=language_code,fill=language_code)) +
  geom_bar()+
  labs(x = "language", title = "Excluding English") +
  theme_pubclean()+
  theme(legend.position = "none")+
  coord_flip()
grid.arrange(plot1, plot2, ncol=2)
# original_publication_year
hist(books$original_publication_year, col = "blue", main = "Publication year", xlab = " ")
# book count
hist(books$books_count, col = "blue", main = "Number of book editions", xlab = " ")
# Average Rating of books
hist(books$average_rating, col = "blue", main = "Average Rating", xlab = " ")
hist(books$work_ratings_count, col = "blue", main = "Average Rating", xlab = " ")


# Combine 'books' dataset with 'tags' and 'book_tags' (also named Genres)

#first merge book_tags and tags
genres <- merge(book_tags,tags,by="tag_id")
genres <- merge(genres,books, by= "goodreads_book_id")
genres<- genres[,1:4]
# 2 approaches to genres
# use the genres provided by goodreads 
goodreads_genres <-(c("Art", "Biography", "Business", "Chick Lit", "Children's", "Christian", "Classics", "Comics", "Contemporary", "Cookbooks", "Crime", "Ebooks", "Fantasy", "Fiction", "Gay and Lesbian", "Graphic Novels", "Historical Fiction", "History", "Horror", "Humor and Comedy", "Manga", "Memoir", "Music", "Mystery", "Nonfiction", "Paranormal", "Philosophy", "Poetry", "Psychology", "Religion", "Romance", "Science", "Science Fiction", "Self Help", "Suspense", "Spirituality", "Sports", "Thriller", "Travel", "Young Adult"))

c <- goodreads_genres[str_to_lower(goodreads_genres) %in% genres$tag_name]
available_tags <- genres$tag_id[match(str_to_lower(c), genres$tag_name)]
genres <- genres %>% filter(genres$tag_id %in% available_tags)
#now the dataset contains a book with multiples tags but from the list above
temp4 <- within(genres, tag_name <- factor(tag_name,levels=names(sort(table(tag_name), decreasing=FALSE))))
plot3 <- ggplot(data.frame(temp4), aes(x=tag_name,fill=tag_name)) +
  geom_bar()+
  labs(x = " ", title = "Allowing multiple genres") +
  theme_pubclean()+ 
  theme(legend.position = "none")+
  coord_flip()
# select the most popular one
genres_temp <- as.data.table(genres)
genres_temp <- genres_temp[genres_temp[, .I[which.max(count)], by=goodreads_book_id]$V1]
temp5 <- within(genres_temp, tag_name <- factor(tag_name,levels=names(sort(table(tag_name), decreasing=FALSE))))
plot4 <- ggplot(data.frame(temp5), aes(x=tag_name,fill=tag_name)) +
  geom_bar()+
  labs(x = " ", title = "The most popular genre per") +
  theme_pubclean()+ 
  theme(legend.position = "none")+
  coord_flip()
par(mfrow=c(1,3))
grid.arrange(plot3, plot4, ncol=2)


#Final books dataset
books <- merge(books,genres_temp,by="goodreads_book_id")
books = subset(books, select = -c(tag_id,count) )


#################################
# Content Based Recommendations #
#################################

books_1 <- books
main_tags_labels<- str_to_lower(goodreads_genres)
#map two tables together by tag_id
main_tags = merge(x=book_tags,y=tags,by="tag_id")
#combine tags for the same book and the original tag data of 30,000 rows is reduced to 10,000 rows
main_tags1 = main_tags[,.(tags = paste(tag_name,collapse=",")),.(goodreads_book_id)]
#generate a book-genre matrix for each book
for(j in main_tags_labels){
  set(main_tags1,j = j,value = grepl(x = main_tags1$tags,pattern = j)*1)
}
main_tags1[,tags:=NULL]
bookDF = merge(x= books_1,y=main_tags1,by="goodreads_book_id")
bookDF = bookDF[,-1,drop=FALSE]
bookDF = bookDF[,-2:-4,drop=FALSE]
bookDF = bookDF[,-3:-6,drop=FALSE]
userDF <- ratings[,-(4:5)]
#cluster books based on their genre using k-means, set k = 5
clusterBooks<-function(bookDF){
  set.seed(11)
  bookDF<-bookDF[,-(1:2)]
  bookCluster<-kmeans(bookDF,5)
  return(bookCluster)
}
#find all books with the associated ratings that selected user has read
UserInfo<-function(dat,id){
  #Select all rows for the same user and keep the columns book_id & rating
  a<-subset(dat, user_id==id,select=c(book_id, rating))
  # allocate 0 to the cluster column
  cluster<-0
  activeUser <- data.frame(a[order(a$book_id),] ,cluster)
  return(activeUser)
}
#assign to each book the corresponding cluster number
UserBookCluster<-function(bookCluster, activeUser){
  #create temporary dataframe to match cluster assignments to book_ids
  df1<- data.frame(cbind(bookDF$book_id, clusterNum = bookCluster$cluster))
  names(df1)<-c("book_id", "cluster")
  #this matches the cluster number to the activeUser book id
  activeUser$cluster<-df1[match(activeUser$book_id, df1$book_id),2]
  return(activeUser)
}
#calculate for each cluster the average of the book ratings
MeanClusterRating<-function(bookCluster, activeUser){
  #aggregate() function is used along with the cluster memberships to determine variable means for each cluster in the original metric
  like<-aggregate(activeUser$rating, by=list(cluster=activeUser$cluster), mean)
  #if the max mean rating is below three it gives out the dummy value zero
  if(max(like$x)<3){
    like<-as.vector(0)
    #else it gives out the cluster number of the max mean value
  } else{
    like<-as.vector(t(max(subset(like, x>=3, select=cluster))))
  }
  return(like)
}
#if there is no cluster with a rating of 3 or above, select at random 100 books
GoodBooks<-function(like, bookCluster, bookDF){
  #a temporary dataframe is created to get a list of all books and their associated clusters
  df1<- data.frame(cbind(bookDF$book_id, clusterNum = bookCluster$cluster))
  names(df1)<-c("book_id", "cluster")
  #if like has the value zero it selects randomly 100 books
  if(like==0){
    recommend<-bookDf[sample.int(n = dim(bookDF)[1], size = 100), 1]
  }
  #else it selects all books from the winning max mean cluster
  else{
    recommend<-as.vector(t(subset(df1, cluster==like, select=book_id)))
  }
  return(recommend)
}
#finalise the final recommendation list
RecommendedBooks<-function(bookDF, userDF, user_id){
  #recall all functions
  bookCluster<-clusterBooks(bookDF)
  activeUser<-UserInfo(userDF, user_id)
  activeUser<-UserBookCluster(bookCluster, activeUser)
  like<-MeanClusterRating(bookCluster, activeUser)
  recommend<-GoodBooks(like, bookCluster, bookDF)
  # only select not yet read books
  recommend<-recommend[-activeUser$book_id]
  # add book title
  title<-bookDF[match(recommend,bookDF$book_id),2]
  recommend<-data.frame(recommend,title)
  return(recommend)
}
#make suggestion
#set the specific user id and the number of suggestions as inputs
suggestBooks<-function(bookDF, userDF, user_id, no_books){
  #get suggestions
  suggestions = RecommendedBooks(bookDF, userDF, user_id)
  #select stated number of selections
  suggestions = suggestions[1:no_books,]
  cat("For use with id",user_id,':\n')
  writeLines("You may also like these books:")
  #print suggestions
  write.table(suggestions, row.names = FALSE, col.names = FALSE)
}
#try with an example: suggest 10 books to user whose ID is 11405
suggestBooks(bookDF, userDF, 11405, 10)

#####################
# Apriori Algorithm #
#####################
joined_ratings = left_join(x=ratings,y=books_with_id,by="book_id") 
#we choose the books that are rated >= 4 by user A as A's reading history with satisfaction
joined_ratings = joined_ratings[which(joined_ratings$rating>=4),]
#to get the sparse matrix
tr=split(joined_ratings$book_id,joined_ratings$user_id)
transactions = as(tr,"transactions")
#summary(transactions)

#record the start time
start.time <- Sys.time() 
#construct Apriori Algorithm
rules.Apriori = apriori(data=transactions, 
                        parameter = list(supp = 0.01, conf = 0.60, target = "rules",minlen=1,maxlen=10));
#record the time when the modelling is finished
end.time.model <- Sys.time() 
#record time taken for modelling
time.taken.model <- end.time.model - start.time
time.taken.model #3.292656 secs

#shows the rules found by Apriori
#inspect(rules.Apriori)
user_history <- ratings[ratings$user_id==11405 &ratings$rating>=4]
Book_id_of_history <- as.array(user_history$book_id)
#input some random user's reading history
basket = as(list(x=Book_id_of_history),"itemMatrix")
#choose the rules of which lhs is subset of user's reading history and rhs is not in user's reading history
basket_subset = as.logical(is.subset(rules.Apriori@lhs,basket)) & !as.logical(is.subset(rules.Apriori@rhs,basket))
#we choose the best five books to recommond by value of lift
rules.sorted<-sort(rules.Apriori[basket_subset],by='lift')

#A function that collects all rules with different outputs (choose the rule with highest lift)
Rules_Collection <- function( SomeRules) {
  ItemSet <- as(list(x=NA),"itemMatrix")
  rules <- head(rules.sorted, 0)
  Max <- length(SomeRules)
  for (i in 1:Max) {
    I <- !as.logical(sum(is.subset(SomeRules@rhs[i],ItemSet))>=1)
    if (I){
      rules <-union(rules,SomeRules[i])
      ItemSet <- union(ItemSet,SomeRules@rhs[i])}}
  return(rules)
}

Rules_for_User <- Rules_Collection(rules.sorted)
#shows all the rules with different recommendations for this user
inspect(Rules_for_User)
top5.rules<-head(Rules_for_User, 5)
as(top5.rules,'data.frame')

#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time is how long this code was running 
time.taken <- end.time - end.time.model
time.taken #11.24939 secs

#Prepare input for the Colaborative filtering method
#(sparse matrix)
dimension_indices <- list(user_id = sort(unique(ratings$user_id)), 
                          book_id = sort(unique(ratings$book_id)))
summary(dimension_indices)

ratingsForSparseMatrix<-ratings
ratingsForSparseMatrix$user_id <- as.numeric(as.factor(ratings$user_id)) 
ratingsForSparseMatrix$book_id <- as.numeric(as.factor(ratings$book_id)) 

#construct sparse matrix
sparse_ratings <- sparseMatrix(i = ratingsForSparseMatrix$user_id,j = ratingsForSparseMatrix$book_id, x = ratingsForSparseMatrix$rating, dims = c(length(unique(ratingsForSparseMatrix$user_id)), length(unique(ratingsForSparseMatrix$book_id))),dimnames = dimension_indices)

r <- new("realRatingMatrix", data = sparse_ratings) 

sparse_ratings[1:10,1:10]

###################################################
# Find the best parameters using cross-validation #
###################################################

########
# UBCF # n
########
#find the best nn for UBCF method. Cross-validation produces more robust results and error estimates.
ubcf<-data.frame()
set.seed(7)
cat('running for nn =')
#record the start time
start.time <- Sys.time() 
e <- evaluationScheme(r, method="cross-validation", k=10, given=10)
for (i in c(10,20,30,40,50,100,200,400,800)){
  cat(i,'--')
  ubcf_add<-data.frame(Nearest_Neighbours = 0, test_RMSE=0) 
  Rec.ubcf <- Recommender(getData(e, "train"), "UBCF", param=list( nn=i))
  #making predictions on the test data set
  p_ubcf_test <- predict(Rec.ubcf, getData(e, "known"), type="ratings")
  # obtaining the error metrics for both approaches and comparing them
  test_RMSE_ubcf <-calcPredictionAccuracy(p_ubcf_test, getData(e, "unknown")) 
  ubcf_add$Nearest_Neighbours <-i
  ubcf_add$test_RMSE <- test_RMSE_ubcf[1]
  ubcf = rbind(ubcf,ubcf_add) 
}
plot(ubcf,type="b")
#the result shows we use nn =30
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time is how long this code was running 
time.taken <- end.time - start.time
time.taken #45.98088 secs

########
# IBCF #  k
########

#find the best k for IBCF method. Running this code takes about 15 mins.
ibcf<-data.frame()
cat('running for k =')
#record the start time
start.time <- Sys.time() 
for (i in c(20,30,50,100,200,500,1000,2000)){
  cat(i,'--')
  ibcf_add<-data.frame(Nearest_Neighbours = 0, test_RMSE=0) 
  Rec.ibcf <- Recommender(getData(e, "train"), "IBCF", param=list( k=i))
  #making predictions on the test data set
  p_ibcf_test <- predict(Rec.ibcf, getData(e, "known"), type="ratings")
  # obtaining the error metrics for both approaches and comparing them
  test_RMSE_ibcf <-calcPredictionAccuracy(p_ibcf_test, getData(e, "unknown")) 
  ibcf_add$Nearest_Neighbours <-i
  ibcf_add$test_RMSE <- test_RMSE_ibcf[1]
  ibcf = rbind(ibcf,ibcf_add) 
}
plot(ibcf,type="b")
#the result shows we use k =2000
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time is how long this code was running 
time.taken <- end.time - start.time
time.taken #13.62264 mins



########
# SVDF # lambda
########

#find the best lambda for SVDF method. Running this code takes about 15 mins.
SVDF<-data.frame()
#record the start time
start.time <- Sys.time() 
for (i in c(0.001,0.003,0.005,0.007,0.01)){
  SVDF_add<-data.frame(lambda = 0, test_RMSE=0) 
  Rec.SVDF <- Recommender(getData(e, "train"), "SVDF", 
                          param=list(k=10,gamma=0.02,lambda=i,
                                     min_epochs=20,max_epochs=100,min_improvement=1e-05))
  #making predictions on the test data set
  p_SVDF_test <- predict(Rec.SVDF, getData(e, "known"), type="ratings")
  # obtaining the error metrics for both approaches and comparing them
  test_RMSE_ibcf <-calcPredictionAccuracy(p_SVDF_test, getData(e, "unknown")) 
  SVDF_add$lambda <-i
  SVDF_add$test_RMSE <- test_RMSE_ibcf[1]
  SVDF = rbind(SVDF,SVDF_add) 
}
plot(SVDF,type="b")
#the result shows we use lambda= 0.005
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time is how long this code was running 
time.taken <- end.time - start.time
time.taken # 19.00094 mins

########
# SVDF # N (number of features)
########

#find the best number of featrues for SVDF method. Running this code takes about 30 mins.
SVDF_N<-data.frame()
#record the start time
start.time <- Sys.time() 
#e <- evaluationScheme(r, method="split",0.8,k=10, given=-5)
for (i in c(1,3,5,8,10,20,30)){
  SVDF_add <- data.frame(N_features = 0, test_RMSE=0) 
  Rec.SVDF <- Recommender(getData(e, "train"), "SVDF", 
                          param=list(k=i,gamma=0.05,lambda=0.005,
                                     min_epochs=10,max_epochs=200,min_improvement=1e-06,verbose=T))
  #making predictions on the test data set
  p_SVDF_test <- predict(Rec.SVDF, getData(e, "known"), type="ratings")
  # obtaining the error metrics for both approaches and comparing them
  test_RMSE_ibcf <-calcPredictionAccuracy(p_SVDF_test, getData(e, "unknown")) 
  SVDF_add$N_features <-i
  SVDF_add$test_RMSE <- test_RMSE_ibcf[1]
  SVDF_N = rbind(SVDF_N,SVDF_add) 
}    
plot(SVDF_N,type="b")
#the result shows we use number of featrues = 8
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time is how long this code was running 
time.taken <- end.time - start.time
time.taken # 58.23097 mins

#####################################################
# compare 4 methods :UBCF, IBCF, Random items, SVDF #
#####################################################
#Running this code takes about 15-20 mins.
set.seed(7)
#Use cross validation for performance measurement
scheme <- evaluationScheme(r, method="cross-validation", k=10, given=10, goodRating=4)

#list all the methods we are using
Algorithm <-list("UBCF_30"  = list(name="UBCF", param=list(nn = 30)),
                 "IBCF_2000"  = list(name="IBCF", param=list(k  = 2000)),
                 "Random Items" = list(name="RANDOM", param=NULL),
                 "SVDF"=list(name="SVDF", param=list(k=8,gamma=0.05,lambda=0.005,
                                                     min_epochs=10,max_epochs=200,min_improvement=1e-06)))
results <- evaluate(scheme, Algorithm , type = "ratings")
#plot the result
plot(results, ylim = c(0,2))


################################################
# select some random user to do recommendation #
################################################
#input his/her real user_id (make sure it is in the sample rating dataset)
Real_User_id <-11405
#record it is place in sparse ratings matrix
IndexForPrediction<-which(rownames(sparse_ratings)==Real_User_id)

###############
# predictions #
###############

# Do prediction based on IBCF
# record the start time
start.time <- Sys.time() 
r_recom_IBCF <- Recommender ( r , method = "IBCF" , param=list(k  = 2000))
#record the time when the modelling is finished
end.time.model <- Sys.time() 
#record time taken for modelling
time.taken.model <- end.time.model - start.time
time.taken.model #2.099918 mins
# predict 5 books based on IBCF
pred_IBCF <- predict ( r_recom_IBCF , r [IndexForPrediction] , type="ratings") 
top_ibcf <- getTopNLists(pred_IBCF, n=5)
top_IBCF_list <- as(top_ibcf,"list")
cat('For user with id',Real_User_id,':\n')
cat('The best 5 books to recommend (Based on IBCF)  are: \n')
#Print the top 5 books
for (i in 1:5) {
  b <- unique(books_with_id$book[books_with_id$book_id == top_IBCF_list[[1]][i]]) 
  artist <- unique(books$authors[books$book_id == top_IBCF_list[[1]][i]]) 
  print(paste(b,artist,sep = " - "))
}
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time of prediction
time.taken <- end.time - end.time.model
time.taken # 0.1990631 secs


# Do prediction based on UBCF 
start.time <- Sys.time() 
r_recom_UBCF <- Recommender ( r , method = "UBCF" , param=list(nn = 30))
#record the time when the modelling is finished
end.time.model <- Sys.time() 
#record time taken for modelling
time.taken.model <- end.time.model - start.time
time.taken.model #0.07267308 secs
# predict 5 books based on UBCF
pred_UBCF <- predict ( r_recom_UBCF , r [IndexForPrediction] , type="ratings") 
top.ubcf <- getTopNLists(pred_UBCF, n=5)
top_UBCF_list<-as ( top.ubcf , "list" )
cat('\nFor user with id',Real_User_id,':\n')
cat('The best 5 books to recommend (Based on UBCF) are: \n')
#Print the top 5 books
for (i in 1:5) {
  b <- unique(books_with_id$book[books_with_id$book_id == top_UBCF_list[[1]][i]]) 
  artist <- unique(books$authors[books$book_id == top_UBCF_list[[1]][i]]) 
  print(paste(b,artist,sep = " - "))
}
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time of prediction
time.taken <- end.time - end.time.model
time.taken # 0.760457 secs

# predict 5 books based on FSVD
start.time <- Sys.time() 
# create the FSVD model
r_recom_SVDF <- Recommender (r, method = "SVDF" , 
                             param=list(k=8,gamma=0.05,lambda=0.005,
                                        min_epochs=10,max_epochs=200,min_improvement=1e-06))
#record the time when the modelling is finished
end.time.model <- Sys.time() 
#record time taken for modelling
time.taken.model <- end.time.model - start.time
time.taken.model #9.855945 mins
# do predictions
pred_SVDF <- predict ( r_recom_SVDF , r [IndexForPrediction] , type="ratings") 
top_SVDF <- getTopNLists(pred_SVDF, n=5)
top_SVDF_list <- as(top_SVDF,"list")
cat('\nFor user with id',Real_User_id,':\n')
cat('The best 5 books to recommend (Based on FSVD)  are: \n')
#Print the top 5 books
for (i in 1:5) {
  b <- unique(books_with_id$book[books_with_id$book_id == top_SVDF_list[[1]][i]]) 
  artist <- unique(books$authors[books$book_id == top_SVDF_list[[1]][i]]) 
  print(paste(b,artist,sep = " - "))
}
#record the time when the running is finished
end.time <- Sys.time() 
#Difference between start end end time of prediction
time.taken <- end.time - end.time.model
time.taken # 8.104227 secs

# to see the two matrix U and V, run the following code, takes 15 mins
#fsvd<- funkSVD(r, k = 8, gamma = 0.05, lambda = 0.005, min_improvement = 1e-06, min_epochs = 10, max_epochs = 200, verbose = T)
#fsvd$U
#fsvd$V

########################################
# test the 3 methods: IBCF, UBCF, SVDF #
########################################
set.seed(7)
scheme <- evaluationScheme(r, method="split", train=0.8 , given=10,  goodRating=5)
results_IBCF <- evaluate(scheme, method="IBCF",param=list(k  = 2000), type = "topNList",  n=c(3,5,10,15,20,30))
results_UBCF <- evaluate(scheme, method="UBCF",param=list(nn = 30)  , type = "topNList",  n=c(3,5,10,15,20,30))
results_SVDF <- evaluate(scheme, method="SVDF",
                         param=list(k = 8,gamma = 0.05,lambda= 0.005,
                                    min_epochs = 10,max_epochs = 200,
                                    min_improvement = 1e-06), type = "topNList", n = c(3,5,10,15,20,30))
results_RAND <- evaluate(scheme, method='RANDOM',param=NULL,  n=c(3,5,10,15,20,30))
cat('The performance of IBCF is : \n')
getConfusionMatrix(results_IBCF)[[1]]
cat('\nThe performance of UBCF is : \n')
getConfusionMatrix(results_UBCF)[[1]]
cat('\nThe performance of SVDF is : \n')
getConfusionMatrix(results_SVDF)[[1]]
cat('\nThe performance of RANDOM is : \n')
getConfusionMatrix(results_RAND)[[1]]

######################################
# plot the precision and recall rate #
######################################
table1<- as.data.frame(getConfusionMatrix(results_IBCF)[[1]][,c('precision','recall'),drop=F])
table2<- as.data.frame(getConfusionMatrix(results_UBCF)[[1]][,c('precision','recall'),drop=F])
table3<- as.data.frame(getConfusionMatrix(results_SVDF)[[1]][,c('precision','recall'),drop=F])
table4<- as.data.frame(getConfusionMatrix(results_RAND)[[1]][,c('precision','recall'),drop=F])

table_precision <- cbind(table1[,1],table2[,1],table3[,1],table4[,1])
colnames(table_precision) <- c('IBCF','UBCF','SVDF','RANDOM')
table_precision <-as.data.frame(table_precision)
table_precision$N <- as.factor(c(3,5,10,15,20,30))

table_recall <- cbind(table1[,2],table2[,2],table3[,2],table4[,2])
colnames(table_recall) <- c('IBCF','UBCF','SVDF','RANDOM')
table_recall <-as.data.frame(table_recall)
table_recall$N <- as.factor(c(3,5,10,15,20,30))

Comparision <- function(df,txt){
  dfm <- melt(df[,c('N','IBCF','UBCF','SVDF','RANDOM')],id.vars = 1)
  ggplot(data=dfm, aes(x=N, y=value, fill=variable)) +
    geom_bar(colour="black", stat="identity",
             position=position_dodge(),size=.3) +                        
    xlab("Number of recommendations") + ylab(paste(txt,"Rate")) +
    ggtitle(paste(txt,"for 4 methods")) + theme_bw()
}

Comparision(as.data.frame(table_precision),'Precision')
Comparision(as.data.frame(table_recall),'Recall')

