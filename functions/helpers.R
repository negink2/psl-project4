
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                     time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

appCSS <- "
.btn-loading-container {
  margin-left: 10px;
  font-size: 1.2em;
}
.btn-done-indicator {
  color: green;
}
.btn-err {
  margin-top: 10px;
  color: red;
}
"

get_unique_genres = function(){
  #myurl = "https://liangfgithub.github.io/MovieData/"

  #movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
  #movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  #movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  #movies = data.frame(movies, stringsAsFactors = FALSE)
  #colnames(movies) = c('MovieID', 'Title', 'Genres')
  #movies$MovieID = as.integer(movies$MovieID)
  
  ## convert accented characters
  #movies$Title[73]
  #movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  #movies$Title[73]
  
  ## extract year
  #movies$Year = as.numeric(unlist(
   # lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
  
  #u_gs = list()
  #i = 1
  #for(g in movies$Genres){
   # g_separated = strsplit(g, '|')
    #for(s in g_separated){
     # if(s %in% u_gs){print("")}
      #else{
       # u_gs[i] = s
      #}
      #i = i + 1
    #}
  #}
  
  #u_gs = unique(u_gs)
  #print(u_gs)
  genres = c('Animation','Adventure','Comedy','Action','Drama','Thriller','Crime',
             'Children\'s','Documentary','Sci-Fi','Horror','Western','Mystery','Film-Noir','War','Fantasy','Musical')
  return(genres)
  
}

get_system1_recommendations = function(genre, type, path){
  rec_results <- read.csv(file = paste(path, '/system1_', type, '_', genre, '.csv', sep=''))
  return(rec_results)
}

get_sys2_recommendations = function(system2_algo, value_list){
  user_ratings = get_user_ratings(value_list)
  if(system2_algo == 'ubcf'){
    rec_results <- system2_ubcf(user_ratings)
  }
  else if(system2_algo == 'ibcf'){
    rec_results <- system2_ibcf(user_ratings)
  }
  return(rec_results)
}


# define functions
get_user_ratings <- function(value_list) {
  
  dat <- data.table(UserID = 1, MovieID = sapply(strsplit(names(value_list), "_"), function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    rating = unlist(as.character(value_list)))
  dat <- dat[!is.null(rating) & !is.na(MovieID)]
  dat[rating == " ", rating := 0]
  dat[, ':=' (UserID = 1, MovieID = as.numeric(MovieID), rating = as.numeric(rating))]
  dat <- dat[rating > 0]

  return(dat)
}
