library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)

if (interactive()) {
  options(shiny.port = 8100) ##set stable port for testing
  }

## Basic UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("CFDE: GitHub Onboarding Helper"),
  sidebarLayout(
    sidebarPanel(
      actionButton("login", "Sign in with GitHub"),
      uiOutput("repo_selector"),
      textInput("topic", "Enter a topic to add"),
      actionButton("add_topic", "Add Topic")
    ),
    mainPanel(
      textOutput("status"),
      verbatimTextOutput("user_info"),
      tableOutput("repo_table")
    )
  )
)

## Server
server <- function(input, output, session) {
  # Helper Function
  replace_null <- function(x) {
    lapply(x, function(y) if (is.null(y)) NA else y)
  }

  # GitHub OAuth App
  app <- oauth_app(appname = "github", 
                   key = Sys.getenv('onboard_helper_client'), 
                   secret = Sys.getenv('onboard_helper_secret')
                  )

  # Create Reactives to store GitHub Auth Token and User Data
  github_token <- reactiveVal(NULL)
  user_data <- reactiveVal(NULL)
  
  # Wait for login, create access token
  observeEvent(input$login, {
    # scopes <- "repo read:org" ##read:org may be required
    scopes <- "repo"
    github_token(oauth2.0_token(oauth_endpoints("github"), app, scope = scopes, cache = FALSE))
    # Extract user information
    user_info <- GET("https://api.github.com/user", config(token = github_token()))
    user_data(content(user_info))
    output$user_info <- renderPrint({ user_data()$login })
    # Extract org information
    # org_info <- GET(glue::glue("https://api.github.com/{user_data()$login}/orgs"), config(token = github_token())) ##May require additional scopes
    shinyjs::show("repo_selector")
  })
  
  # Repo UI
  output$repo_selector <- renderUI({
    if (!is.null(github_token())) {
      req_repos <- GET("https://api.github.com/user/repos", config(token = github_token()))
      repos <- content(req_repos)
      repo_names <- sapply(repos, function(x) x$name)
      selectInput("repo", "Select a repository to tag", choices = repo_names)
    }
  })
  
  # Add Topic
  ## This is a 3 step process as adding individual topics is not supported by GitHub API. In this way,
  ## any existing repository topics are preserved.
  ### 1. GET existing topics
  ### 2. Append new topic and format
  ### 3. PUT all topics
  observeEvent(input$add_topic, {
    ### Get Existing Repository Topics
    get_topics <- GET(
      url = glue::glue("https://api.github.com/repos/{user_data()$login}/{input$repo}/topics"),
      add_headers("Accept: application/vnd.github+json"),
      add_headers(Authorization = paste("Bearer", github_token()$credentials$access_token)),
      add_headers("X-GitHub-Api-Version: 2022-11-28")
    )
    ### Process, appending new topic
    topics <- content(get_topics)$names
    all_topics <- toJSON(list(names = append(topics, input$topic)), auto_unbox = TRUE)
    
    ### Send it!
    put_topics <- PUT(
      url = glue::glue("https://api.github.com/repos/{user_data()$login}/{input$repo}/topics"),
      add_headers("Accept: application/vnd.github+json"),
      add_headers(Authorization = paste("Bearer", github_token()$credentials$access_token)),
      add_headers("X-GitHub-Api-Version: 2022-11-28"),
      body = all_topics,
      encode = "json"
    )

    ## Verify the status code of the topic addition
    output$status <- renderText({
      if (status_code(put_topics) == 200) {
        "Topic added successfully!"
        } else {
          "Failed to add topic."
        }
    })
  })
  
  ## Show all User GitHub Repos
  output$repo_table <- renderTable({
    if (!is.null(github_token())) {
      req_repo_table <- GET("https://api.github.com/user/repos", config(token = github_token()))
      repos_table <- content(req_repo_table)
      ## Process Repo Info for Display
      name <- sapply(repos_table, function(x) x$name)
      description <- unlist(replace_null(sapply(repos_table, function(x) x$description)))
      url <- sapply(repos_table, function(x) x$html_url)
      tibble::tibble(Name = name, 
                     Description = description,
                     URL = url)
    }
  })
}

shinyApp(ui = ui, server = server)
