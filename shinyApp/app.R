library(shiny)
library(bslib)
library(httr)
library(tibble)
library(jsonlite)
library(shinyjs)
library(DT)

if (interactive()) {
  options(shiny.port = 8100) ##set stable port for testing
  }

## Basic UI
ui <- 
  page_fluid(
    useShinyjs(),
    tags$style(
      HTML("
        .centered-button {
        height: 37px; 
        margin-bottom: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        }
      ")
    ),
    titlePanel(title = "Common Fund Data Ecosystem: Onboard Helper", 
               windowTitle = "Onboard Helper"
              ),
    layout_sidebar(
      sidebar = sidebar(
        actionButton("login", "Sign in with GitHub", icon = icon("github")),
        verbatimTextOutput("user_info")
      ),
      page_fluid(
        textOutput("status"),
        h4('NIH Project Locator'),
        p("If you already know your NIH Core Project number, great! You can sign-in with your GitHub credentials and
          tag related repositories below. If not, try searching for your project using the table below. You can also 
          try searching by project keywords to help locate your core project number."),
        card(
          card_body(
            dataTableOutput("project_table"),
            style = "width: auto; height: 800px;"
          )
        ),
        hidden(
          div(
            id = "github_authorized_ui",
            fluidRow(
              id = "github_repo_instructions",
              h4("GitHub Repositories"),
              p("Select a repository to tag with your NIH Core Project number. Selections may be made from the dropdown, or
                by clicking rows in the table below."
              )
            ),
            fluidRow(
              id = "repo_topic_components",
              style = "display: flex; align-items: flex-end;",
              column(
                width = 4,
                uiOutput("repo_selector")
              ),
              column(
                width = 4,
                textInput("topic", "Enter a topic to add:")
              ),
              column(
                width = 3,
                actionButton("add_topic", "Add Topic", style = "height: 35px;", class = "centered-button")
              )
            ),
            card(
              card_body(
                dataTableOutput("repo_table"),
                style = "width: auto; height: 800px;"
              )
            )
          )
        )
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

  # When toekn available, show repo UI components
  observe({
    if (!is.null(github_token())) {
      shinyjs::show("github_authorized_ui")
      } else {
        shinyjs::hide("github_authorized_ui")
        }
  })
  
  # NIH Project Selector
  projects <- read.csv('shinyApp/data/reporter-projects.csv')
  output$project_table <- renderDataTable({
    projects %>% 
      DT::datatable(
        rownames = F,
        colnames = c("Core Project Number", "Project Number", "Project Title", "Project Detail URL", "Organization"),
        escape = FALSE,
        filter = list(position = "top", clear = FALSE),             
        selection = "single",
        options = list(
          scrollY = 500,
          paging = FALSE,
          columnDefs = list(
            list(
              targets = c(5,6),
              searchable = TRUE,
              visible = FALSE
            )
          )
        )
      )
  })
  observeEvent(input$project_table_rows_selected, {
    selected_row <- input$project_table_rows_selected
    if (length(selected_row) > 0) {
      row_data <- projects[selected_row, 'core_project_num']
      updateTextInput(session, "topic", value = paste(row_data, collapse = ", "))
    }
  })

  # Repo Selector
  output$repo_selector <- renderUI({
    if (!is.null(github_token())) {
      req_repos <- GET("https://api.github.com/user/repos", config(token = github_token()))
      repos <- content(req_repos)
      repo_names <- sapply(repos, function(x) x$name)
      selectInput("repo", "Select a repository to tag:", choices = repo_names)
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
  output$repo_table <- renderDataTable({
    if (!is.null(github_token())) {
      req_repo_table <- GET("https://api.github.com/user/repos", config(token = github_token()))
      repos_table <- content(req_repo_table)
      ## Process Repo Info for Display
      name <- sapply(repos_table, function(x) x$name)
      description <- unlist(replace_null(sapply(repos_table, function(x) x$description)))
      url <- sapply(repos_table, function(x) x$html_url)
      tibble::tibble(Name = name, 
                     Description = description,
                     URL = glue::glue("<a href='{url}' target='_blank'>{url}</a>")
                    ) %>% 
        DT::datatable(
          rownames = F,
          escape = FALSE,
          filter = list(position = "top", clear = FALSE),             
          options = list(
            scrollY = 500,
            paging = FALSE
          )
        )
    }
  })
}

shinyApp(ui = ui, server = server)
