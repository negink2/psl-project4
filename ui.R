####
#The authors of this project are:
#Mani Bayani (NetID: mbayani2 - University ID: 658712716)
#Omar Kahwaji (NetID: kahwaji2 - University ID: 659375761)
#Negin Kashkooli (NetID: negink2 - University ID: 653412774)
####


library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
#devtools::install_github("stefanwilhelm/ShinyRatingInput")
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/movies.css"),
              fluidRow(id="authors", box(width = 12, solidHeader = TRUE,
                           div("Authors:"),
                           div(class='div-authors', "Mani Bayani (NetID: mbayani2 - University ID: 658712716)"),
                           div(class='div-authors', "Omar Kahwaji (NetID: kahwaji2 - University ID: 659375761)"),
                           div(class='div-authors', "Negin Kashkooli (NetID: negink2 - University ID: 653412774)"),
                           br(),
                           div(class='div-authors', "Reference(s):"),
                           div(class='div-authors', "Book Recommendation", a(href='https://github.com/pspachtholz/BookRecommender', '[Link to the Repository]'))
              )),
              br(),
              radioButtons("disp", "Recommendation type:",
                           c("Recommendation System I (Movie Recomendation by Genre)" = "recommender1",
                             "Recommendation System II (Collaborative Recommendation System)" = "recommender2")),
              # recommender 1
              fluidRow(id='recommender1',
                       fluidRow(
                         box(width = 12, title = "System I: Movie Recommendation by Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                             div(class="genre-div", selectInput("fav_genre", "Please enter your favorite movie genre",
                                                                  choices = get_unique_genres())),
                             br(),
                             radioButtons("algo", "Algorithm type of System I:",
                                          c("System I - Algorithm 1 (high rates)" = "algo1",
                                            "System I - Algorithm 2 (popular)" = "algo2")),
                             withBusyIndicatorUI(
                               actionButton("btn_system1", "Click to see the recommendations", class = "btn-success")
                             ),
                             br(),
                             tableOutput("results1")
                         )
                       )
              ), # End of recommender 1
              
              br(),
              
              # recommender 2
              fluidRow(id='recommender2', class='row-recommender2',
                fluidRow(
                    box(width = 12, title = "System II - Step 1: Collaborative Recommendation System", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class="step-desc","Step 1: Rate as many movies as you wish"),
                        div(class = "ratings", uiOutput('ratings'))
                    )
                  ),
                useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                      title = "System II - Step 2: Collaborative Recommendation System",
                      div(class="step-desc","Discover movies that you might like"),
                      br(),
                      radioButtons("system2_algo", "Algorithm type of System II:",
                                   c("System II - Algorithm 1 (UBCF)" = "ubcf",
                                     "System II - Algorithm 2 (IBCF) - *Please note that this algorithm takes almost 9 minutes to run!" = "ibcf")),
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn", "Click to see the recommendations", class = "btn-success")
                      ),
                      br(),
                      tableOutput("results")
                    )
                 #)
              ) # End of recommender 2
          )
    )
) 