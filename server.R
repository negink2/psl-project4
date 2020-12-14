####
#The authors of this project are:
#Mani Bayani (NetID: mbayani2 - University ID: 658712716)
#Omar Kahwaji (NetID: kahwaji2 - University ID: 659375761)
#Negin Kashkooli (NetID: negink2 - University ID: 653412774)
####

# load functions
source('functions/my-final.R') # collaborative filtering


############################# The App Code ###################################
path = 'movie_data/'
movies = readLines(paste0(path, 'movies.dat'))
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

movie_images = paste("https://liangfgithub.github.io/MovieImages/", movies$MovieID, '.jpg?raw=true', sep='')
genres = chartr(old = "|", new = ",", movies$Genres)


shinyServer(function(input, output, session) {
  observeEvent(input$disp, {
    display_mode = input$disp
    if(display_mode == 'recommender1'){
      shinyjs::show(id = "recommender1")
      shinyjs::hide(id = "recommender2")
    }
    else{
      shinyjs::show(id = "recommender2")
      shinyjs::hide(id = "recommender1")
    }
    
  })
  
  ######################## System 1 ################################
  system1_df = eventReactive(input$btn_system1, {
    withBusyIndicatorServer("btn_system1", {
      
      #useShinyjs()
      #jsCode <- "document.querySelector('[data-widget=expand]').click();"
      #runjs(jsCode)
      
      system1_algo = input$algo
      system1_genre = input$fav_genre
      
      #path = 'https://pls-project4.shinyapps.io/Project4_MovieRecommendation_App'
      path = 'https://negink2.github.io/psl-project4/system1_data'
      #path = '/Users/negin/Documents/Illinois/Courses/CS598_Practical_Statistical_Learning/Projects/Project4/Project4_MovieRecommendation_App/movie_data/system1_data'
      if(system1_algo == 'algo1'){
        movs_titles = get_system1_recommendations(system1_genre, '1', path)
        recom_results = subset(movies, Title %in% movs_titles$x)
      }
      else{
        movs_ids = get_system1_recommendations(system1_genre, '2', path)
        recom_results = subset(movies, MovieID %in% movs_ids$x)
      }
      return(recom_results)
    })
  })
  
  
  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 5
    num_movies <- 3
    recom_result <- system1_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        if(!is.na(recom_result$MovieID[(i - 1) * num_movies + j])){
        box(width = 4, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", img(src = paste("https://liangfgithub.github.io/MovieImages/", recom_result$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true', sep='')
                                                 , height = 150)),
            div(style = "text-align:center; color: #999999; font-size: 80%",
                chartr(old = "|", new = ",", recom_result$Genres[(i - 1) * num_movies + j])
            ),
            div(style="text-align:center; font-size: 100%",
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
  ########################### System 2 ############################
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 100
    num_movies <- 3 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 4,
                 div(style = "text-align:center", img(src = toString(movie_images[movies$MovieID[(i - 1) * num_movies + j]]), style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center", paste('Genre:', genres[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        #print(head(value_list))
        recom_results <- get_sys2_recommendations(input$system2_algo, value_list)
        
        # add user's ratings as first column to rating matrix
        #rmat <- cbind(user_ratings, ratingmat)
        
        #system2_algo = input$system2_algo
        #print(head(user_ratings))
        #print('##########')
        #print(head(rmat))
        #recom_results <- get_sys2_recommendations(system2_algo, rmat)
        
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 5
    num_movies <- 3
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        if(!is.na(recom_result$MovieID[(i - 1) * num_movies + j])){
          box(width = 4, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
              
            div(style = "text-align:center", img(src = paste("https://liangfgithub.github.io/MovieImages/", recom_result$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true', sep=''), height = 150)),
            div(style = "text-align:center; color: #999999; font-size: 80%", 
                genres[recom_result$MovieID[(i - 1) * num_movies + j]]
               ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
               )
            
          )
      }
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
