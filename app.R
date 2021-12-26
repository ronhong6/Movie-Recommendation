library(shiny)
library(shinydashboard)
library(shinyjs)
library(ShinyRatingInput)
library(Matrix)
library(data.table)
library(recommenderlab)

withBusyIndicatorUI = function(button) {
  id = button[["attribs"]][["id"]]
  div(
    `data-for-btn` = id,
    button,
    span(class = "btn-loading-container", hidden(
      icon("spinner", class = "btn-loading-indicator fa-spin"),
      icon("check", class = "btn-done-indicator")
    )),
    hidden(div(class = "btn-err", div(
      icon("exclamation-circle"),
      tags$b("Error: "),
      span(class = "btn-err-msg")
    )))
  )
}

withBusyIndicatorServer = function(buttonId, expr) {
  loadingEl = sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl = sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl = sprintf("[data-for-btn=%s] .btn-err", buttonId)
  disable(buttonId)
  shinyjs::show(selector = loadingEl)
  hide(selector = doneEl)
  hide(selector = errEl)
  on.exit({
    enable(buttonId)
    hide(selector = loadingEl)
  })
  
  tryCatch({
    value = expr
    shinyjs::show(selector = doneEl)
    delay(2000,
          hide(
            selector = doneEl,
            anim = TRUE,
            animType = "fade",
            time = 0.5
          ))
    value
  }, error = function(err) {
    errorFunc(err, buttonId)
  })
}

errorFunc = function(err, buttonId) {
  errEl = sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg = sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage = gsub("^ddpcr: (.*)", "\\1", err$message)
  html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl,
                anim = TRUE,
                animType = "fade")
}

ratings = read.csv(
  "https://liangfgithub.github.io/MovieData/ratings.dat?raw=true",
  sep = ":",
  colClasses = c("integer", "NULL"),
  header = FALSE
)
colnames(ratings) = c("UserID", "MovieID", "Rating", "Timestamp")
ratings$Timestamp = NULL

movies = readLines("https://liangfgithub.github.io/MovieData/movies.dat?raw=true")
movies = strsplit(movies,
                  split = "::",
                  fixed = TRUE,
                  useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c("MovieID", "Title", "Genres")
movies$MovieID = as.integer(movies$MovieID)
movies$N = NA
for (i in 1:nrow(movies)) {
  movies$N[i] = length(ratings$Rating[ratings$MovieID == movies$MovieID[i]])
}
movies = movies[order(-movies$N), ]
movies$N = NULL

ui = dashboardPage(
  dashboardHeader(title = "Movie Recommender"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(fluidRow(tabBox(
    width = 12,
    tabPanel("Genres", fluidRow(
      box(
        title = "Select your favorite genre",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        selectInput(
          "genre",
          "",
          c(
            "Action",
            "Adventure",
            "Animation",
            "Children's",
            "Comedy",
            "Crime",
            "Documentary",
            "Drama",
            "Fantasy",
            "Film-Noir",
            "Horror",
            "Musical",
            "Mystery",
            "Romance",
            "Sci-Fi",
            "Thriller",
            "War",
            "Western"
          )
        )
      )
    ), fluidRow(
      useShinyjs(),
      box(
        title = "Discover movies you might like",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        br(),
        withBusyIndicatorUI(
          actionButton("btn1", "Click here to get your recommendations",
                       class = "btn-warning")
        ),
        br(),
        tableOutput("results1")
      )
    )),
    tabPanel("Collaborative", fluidRow(
      box(
        title = "Rate as many movies as possible",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        div(uiOutput("ratings"))
      )
    ), fluidRow(
      useShinyjs(),
      box(
        title = "Discover movies you might like",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        br(),
        withBusyIndicatorUI(
          actionButton("btn2", "Click here to get your recommendations",
                       class = "btn-warning")
        ),
        br(),
        tableOutput("results2")
      )
    ))
  )))
)

server = function(input, output, session) {
  df1 = eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", {
      useShinyjs()
      jsCode = "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      genre = movies[grepl(input$genre, movies$Genres), ]
      genre$Rating = NA
      genre$N.Positive = NA
      for (i in 1:nrow(genre)) {
        genre$Rating[i] =
          mean(ratings$Rating[ratings$MovieID == genre$MovieID[i]])
        genre$N.Positive[i] =
          sum(ratings$Rating[ratings$MovieID == genre$MovieID[i]] >= 4)
      }
      genre[order(-genre$N.Positive, -genre$Rating), ][1:5, 1:2]
    })
  })
  
  output$results1 = renderUI({
    k = 5
    recom = df1()
    fluidRow(lapply(1:k, function(i) {
      box(
        title = paste0("#", i),
        status = "success",
        solidHeader = TRUE,
        width = 2,
        div(style = "text-align:center", a(img(
          src = paste0(
            "https://liangfgithub.github.io/MovieImages/",
            recom$MovieID[i],
            ".jpg?raw=true"
          ),
          height = 144
        ))),
        div(style = "text-align:center", strong(recom$Title[i]))
      )
    }))
  })

  output$ratings = renderUI({
    n = 20
    k = 5
    lapply(1:n, function(i) {
      list(fluidRow(lapply(1:k, function(j) {
        list(box(
          width = 2,
          div(style = "text-align:center", img(
            src = paste0(
              "https://liangfgithub.github.io/MovieImages/",
              movies$MovieID[(i - 1) * k + j],
              ".jpg?raw=true"
            ),
            height = 144
          )),
          div(style = "text-align:center", strong(movies$Title[(i - 1) * k + j])),
          div(style = "text-align:center", ratingInput(
            paste0("select_", movies$MovieID[(i - 1) * k + j]),
            label = "",
            dataStop = 5
          ))
        ))
      })))
    })
  })
  
  df2 = eventReactive(input$btn2, {
    withBusyIndicatorServer("btn2", {
      useShinyjs()
      jsCode = "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      Rmat = sparseMatrix(ratings$UserID, ratings$MovieID, x = ratings$Rating)
      colnames(Rmat) = 1:max(ratings$MovieID)
      Rmat = as(Rmat, "realRatingMatrix")
      value_list = reactiveValuesToList(input)
      dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                        function(x) {
                                          ifelse(length(x) > 1, x[[2]], NA)
                                        }),
                       Rating = unlist(as.character(value_list)))
      dat = dat[!is.null(Rating) & !is.na(MovieID)]
      dat[Rating == " ", Rating := 0]
      dat[, ":=" (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
      dat = dat[Rating > 0]
      if (nrow(dat) == 0) {
        return(NA)
      }
      user_ratings = sparseMatrix(rep(1, nrow(dat)),
                                  dat$MovieID,
                                  x = dat$Rating,
                                  dims = c(1, ncol(Rmat)))
      user_ratings = as(user_ratings, "realRatingMatrix")
      predict(Recommender(Rmat, method = "UBCF"), user_ratings, 10)@items[[1]]
    })
  })
  
  output$results2 = renderUI({
    n = 2
    k = 5
    recom = df2()
    if (!is.na(recom)) {
      lapply(1:n, function(i) {
        list(fluidRow(lapply(1:k, function(j) {
          box(
            title = paste0("#", (i - 1) * k + j),
            status = "success",
            solidHeader = TRUE,
            width = 2,
            div(style = "text-align:center", a(img(
              src = paste0(
                "https://liangfgithub.github.io/MovieImages/",
                recom[(i - 1) * k + j],
                ".jpg?raw=true"
              ),
              height = 144
            ))),
            div(style = "text-align:center",
                strong(movies$Title[movies$MovieID == recom[(i - 1) * k + j]]))
          )
        })))
      })
    }
  })
}

shinyApp(ui = ui, server = server)