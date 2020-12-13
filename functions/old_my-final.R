library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
################################# read tarings
myurl = "https://liangfgithub.github.io/MovieData/"
# use colClasses = 'NULL' to skip columns
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'rating', 'Timestamp')

#### read movies

movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title[73]
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Title[73]

# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

######## read users

users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')
#dim(users)
length(unique(ratings$UserID))
########################################################################

################### System 2 UBCF
library(recommenderlab)
library(Matrix)
library(dplyr)
set.seed(100)


system2_ubcf = function(user_rating){
  start = Sys.time()
  
  user_rating = ratings[54:70, 1:4]
  print(user_rating)
  
  train_x = ratings
  
  i = paste0('u', train_x$UserID)
  j = paste0('m', train_x$MovieID)
  x = train_x$rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  rec_UBCF = Recommender(Rmat, method = 'UBCF',
                         parameter = list(normalize = 'Z-score', 
                                          method = 'Cosine',
                                          nn = 25))
  
  #Summary of model parameters
  rec_UBCF@model
  
  
  #i = paste0('u', user_rating$UserID)
  #j = paste0('m', train_x$MovieID)
  #x = rep(NA, length(train_x$rating))
  #x = train_x$rating
  #tmp = data.frame(i, j, x, stringsAsFactors = T)
  
  t = tmp %>% filter(i == 'u1') 
  #print(t)
  #t = t %>% filter(j == paste0('m',user_rating$MovieID))
  
  for(tm in tmp){
    print(tm[1])
    if(tm[1] == 'u1'){
      tm[3] = NA
      m_id = tm[2]
      print(m_id)
      rate = user_rating %>% filter(m_id == paste0('m',MovieID))
      if(!is.null(rate$rating)){
        tm[3] = rate
      }
    }
  }
  print(tmp)
  
  pred_Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(pred_Rmat) = levels(tmp$i)
  colnames(pred_Rmat) = levels(tmp$j)
  pred_Rmat = new('realRatingMatrix', data = pred_Rmat)
  
  #print(dim(Rmat))
  #print(dim(pred_Rmat))
  # This may take a long time
  recom = predict(rec_UBCF, 
                  pred_Rmat[1], type = 'ratings')  
  rec_list = as(recom, 'list')  # each element are ratings of that user
  
  
  
  
  #print(rec_list$u1)
  top10 = tail(sort(unlist(rec_list)), 10)
  #print(top5)
  col_names = names(top10)
  m_ids = list()
  i = 1
  for(m in col_names){
    MovieID = sapply(strsplit(m, "m."), function(x) ifelse(length(x) > 1, x[[2]], NA))
    m_ids[i] = MovieID
    i = i + 1
  }
  movs = movies %>% filter(movies$MovieID %in% m_ids)
  end = Sys.time()
  time_taken = end - start
  print(time_taken)
  print(movs)
  return(movs)
}
#system2_ubcf(ratings[1:10,1:4])


######################### System 2 IBCF

system2_ibcf = function(user_rating){
  start = Sys.time()
  #user_rating = ratings[1:10,1:4]
  
  train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.8)
  train = ratings[train.id, ]
  train_x = train
  
  i = paste0('u', train_x$UserID)
  j = paste0('m', train_x$MovieID)
  x = train_x$rating
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(Rmat) = levels(tmp$i)
  colnames(Rmat) = levels(tmp$j)
  Rmat = new('realRatingMatrix', data = Rmat)
  
  rec_IBCF = Recommender(Rmat, method = 'IBCF',
                         parameter = list(normalize = 'Z-score', 
                                          method = 'Cosine',k=25))
  
  #Summary of model parameters
  rec_IBCF@model
  
  #i = paste0('u', user_rating$UserID)
  #j = paste0('m', user_rating$MovieID)
  x = rep(0, length(train_x$rating))
  for(id in user_rating$MovieID){
    x[id] = (user_rating %>% filter(MovieID == id))$rating
  }
  
  tmp = data.frame(i, j, x, stringsAsFactors = T)
  pred_Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
  rownames(pred_Rmat) = levels(tmp$i)
  colnames(pred_Rmat) = levels(tmp$j)
  pred_Rmat = new('realRatingMatrix', data = pred_Rmat)
  
  
  # This may take a long time
  recom = predict(rec_IBCF, 
                  pred_Rmat[1,], type = 'ratings')  
  rec_list = as(recom, 'list')  # each element are ratings of that user
  
  #print(rec_list)
  top5 = tail(sort(unlist(rec_list)), 10)
  #print(top5)
  col_names = names(top5)
  m_ids = list()
  i = 1
  for(m in col_names){
    MovieID = sapply(strsplit(m, "m."), function(x) ifelse(length(x) > 1, x[[2]], NA))
    m_ids[i] = MovieID
    i = i + 1
  }
  movs = movies %>% filter(movies$MovieID %in% m_ids)
  print(movs)
  end = Sys.time()
  time_taken = end - start
  print(time_taken)
  #Time difference of 8.561292 mins
  return(movs)
}

#system2_ibcf(ratings[1:10,1:4])
