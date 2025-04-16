library(tidyverse)
library(magrittr)
library(jsonlite)
library(httr2)
library(glue)
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
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
        margin-top: 20px;
        display: flex;
        align-items: center;
        justify-content: center;
        }
      ")
    ),
    titlePanel(
      title = "Common Fund Data Ecosystem: Onboard Helper", 
      windowTitle = "Onboard Helper"
    ),
    layout_sidebar(
      sidebar = sidebar(
        div(
          id = 'login_div', 
          actionButton("login", "Sign in with GitHub", icon = icon("github"))
        ),
        hidden(
          div(
            id = 'logout_div', 
            verbatimTextOutput("user_info"),
            actionButton("logout", "Logout", icon = icon(name = 'sign-out-alt'))
          )
        ), 
      ),
      page_fluid(
        h4('Getting Started'),
        p('If you want to sign in with GitHub and allow this app to add NIH Core Project Numbers as topics on your behalf, 
          first please install the ', a("CFDE GitHub App", href = "https://github.com/apps/common-fund-data-ecosystem-cfde"), 
          ' to your organization or repositories.'),
        h4('NIH Project Locator'),
        p("If you already know your NIH Core Project Number, great! You can sign-in with your GitHub credentials and
          tag related repositories below. If not, try searching for your project using the table. You can also try 
          searching by project keywords to help locate your Core Project Number."),
        card(
          card_body(
            min_height = '200px', 
            div(dataTableOutput("project_table") %>% withSpinner())
          )
        ),
        hidden(
          div(
            id = "github_authorized_ui",
            fluidRow(
              id = "github_repo_instructions",
              h4("GitHub Repositories"),
              p("Select a repository to tag with your NIH Core Project Number. Selections may be made from the dropdown
                or by clicking rows in the table below."
              )
            ),
            fluidRow(
              id = "repo_topic_components",
              style = "display: flex; align-items: flex-start;",
              column(
                width = 4,
                uiOutput("repo_selector")
              ),
              column(
                width = 4,
                textInput(inputId = "topic", label = "Enter a topic to add:", width = "100%")
              ),
              column(
                width = 3,
                actionButton("add_topic", "Add Topic", style = "height: 35px;", class = "centered-button")
              )
            ),
            card(
              card_body(
                min_height = '200px',
                div(dataTableOutput("repo_table") %>% withSpinner())
              )
            ),
            textOutput("status")
          )
        )
      )
    )
  )

## Server
server <- function(input, output, session) {
  # Helper Functions ----
  #' Replace NULL
  #' 
  #' @description
  #' Replace NULL values in a list with NA
  #' @param x A list
  replace_null <- function(x) {
    lapply(x, function(y) if (is.null(y)) NA else y)
  }
  #' Add Topic
  #' 
  #' @description
  #' A wrapper for the GitHub API endpoint that adds a repository topic
  #' @details
  #' This is a 3 step process as adding individual topics is not supported by GitHub API. In this way,
  #' any existing repository topics are preserved.
  #'   1. GET existing topics
  #'   2. Append new topic and format
  #'   3. PUT all topics
  #' @param owner A GitHub username
  #' @param repo The repository to add a topic to
  #' @param topic The topic to add
  add_topic <- function(owner, repo, topic, .token = github_token()$access_token) {
    ### Get Existing Repository Topics
    get_topics_req <- request(glue::glue("https://api.github.com/repos/{owner}/{repo}/topics")) %>% 
      req_auth_bearer_token(.token)
    get_topics <- get_topics_req %>% 
      req_perform() %>% 
      resp_body_json()
    ### Process, appending new topic
    existing_topics <- get_topics$names
    all_topics <- toJSON(list(names = append(existing_topics, topic)), auto_unbox = TRUE)
    
    ### Send it!
    put_topics_req <- request( glue::glue("https://api.github.com/repos/{owner}/{repo}/topics")) %>%
      req_method("PUT") %>%
      req_auth_bearer_token(.token) %>% 
      req_body_raw(all_topics)

    put_topics <- 
      tryCatch(
        {
          put_topics <- put_topics_req %>% 
            req_perform() 
        },
          error = function(cond) {
            put_topics <- list(status_code = 422)
        }
      )
    
    status <- if (put_topics$status_code == 200) {
      "Topic added successfully!"
      } else {
        "Failed to add topic."
        }
    Sys.sleep(1) ## No running
    return(status)
  }

  # GitHub OAuth ----
  ## Prep: Allow redirect to GitHub for auth if JS configured to prevent
  allow_nav_jscode <- 'window.onbeforeunload = null;'
  github_auth <- reactiveVal('no')

  ## Client URL Information
  protocol <- isolate(session$clientData$url_protocol)
  hostname <- if (isolate(session$clientData$url_hostname) == '127.0.0.1') {
    'localhost'
    } else { isolate(session$clientData$url_hostname)
      }
  port <- isolate(session$clientData$url_port)
  pathname <- isolate(session$clientData$url_pathname)
  client_url <- if(is.null(port) | port == '') {
    glue::glue('{protocol}//{hostname}{pathname}') %>% str_remove('/$')
    } else {
      glue::glue('{protocol}//{hostname}:{port}{pathname}')%>% str_remove('/$')
      }
  
  ## Monitor url bar for auth, store as params
  params <- reactive({ parseQueryString(isolate(session$clientData$url_search)) })

  ## When auth code, report authorized
  observeEvent(params(), {
    req(params()$code) 
    github_auth('yes')
    })

  ## GitHub App Client 
  github_client <- function(){
    app <- oauth_client(
      id = 'Iv23liIdOH9m46Wj8Bn6', 
      token_url = "https://github.com/login/oauth/access_token",
      secret = obfuscated("FZHOr1UHjYIsw0b8bx0kEZTB82j9CJ_5TatbnZlLiXLSnuOn5Fx2y_KhMF-xun66-ft4xL--GOA"), 
      name = "github"
    )  
  }
  
  ## GitHub Endpoint/Redirects
  scopes <- "repo read:org"
  github_auth_url <- httr2::oauth_flow_auth_code_url(
    client = github_client(), 
    auth_url = "https://github.com/login/oauth/authorize", 
    redirect_uri = client_url, 
    scope = scopes
  )
  redirect <- sprintf("location.replace(\"%s\");", github_auth_url)
  redirect_home <- sprintf("window.location.replace(\"%s\");", client_url)


  
  ## Wait for login, commence OAuth dance
  observeEvent(input$login, {
    shinyjs::runjs( HTML(allow_nav_jscode, redirect) ) ## TTYL. You'll be back
  })
   ### When the "log out" button is pressed, reset the UI by redirecting to the base url, minus the authorization code
   observeEvent(input$logout, {
    shinyjs::runjs( HTML(allow_nav_jscode, redirect_home) )
    shinyjs::hide(id = 'logout_div')
    shinyjs::show(id = 'login_div')
  })

  # Create and hold Authorization token
  github_token <- reactiveVal(NULL)
  user_data <- reactiveVal(NULL)

  observeEvent(github_auth, {
    if(github_auth() == 'yes') {
      github_token(
        request(github_client()$token_url) %>%
          req_method('POST') %>% 
          req_headers(Accept = "application/json") %>% 
          req_body_multipart(
            client_id = 'Iv23liIdOH9m46Wj8Bn6', 
            client_secret = obfuscated("FZHOr1UHjYIsw0b8bx0kEZTB82j9CJ_5TatbnZlLiXLSnuOn5Fx2y_KhMF-xun66-ft4xL--GOA"), 
            code = params()$code, 
            redirect_uri = client_url) %>%
          req_perform() %>% 
          resp_body_json()
      )
      
    # Extract user information
    user_info_req <- request("https://api.github.com/user") %>% 
      req_auth_bearer_token(github_token()$access_token)
    user_info <- user_info_req %>% 
      req_perform() %>% 
      resp_body_json()

    user_data(user_info)
    output$user_info <- renderPrint({ user_data()$login })
    shinyjs::hide('login_div')
    shinyjs::show('logout_div')
    shinyjs::show("repo_selector") 
    }
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
  projects <- reactive({
    projects_req <- request('https://raw.githubusercontent.com/nih-cfde/icc-eval-core/refs/heads/main/data/raw/reporter-projects.json')
    projects_req %>% 
      req_perform() %>% 
      resp_body_string() %>% 
      fromJSON() %>% 
      extract2('results') %>% 
      unnest(organization) %>% 
      mutate(project_detail_url = glue::glue("<a href='{project_detail_url}' target='_blank'>{project_detail_url}</a>")) %>% 
      select(core_project_num, project_num, project_title, project_detail_url, org_name, terms, pref_terms)
  })
  
  output$project_table <- renderDataTable({
    projects() %>% 
      DT::datatable(
        rownames = F,
        colnames = c("Core Project Number", "Project Number", "Project Title", "Project Detail URL", "Organization"),
        escape = FALSE,
        filter = list(position = "top", clear = FALSE),             
        selection = "single",
        options = list(
          search = list(regex = TRUE),
          scrollY = 250,
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
      row_data <- projects()[selected_row, 'core_project_num']
      updateTextInput(session, "topic", value = paste(row_data, collapse = ", "))
    }
  })

  # GET GitHub Repositories ----
  ## Create Reactive Repo Expression
  repos <- reactive({
    if(!is.null(github_token())) {
      all_repos <- list()
      page <- 1
      per_page <- 100
      repeat {
        ### Get first page of 100 results and iterate
        repo_url <- paste0("https://api.github.com/user/repos?per_page=", per_page, "&page=", page)
        repos_req <- request(repo_url) %>% 
          req_auth_bearer_token(github_token()$access_token)
        repo_content <- repos_req %>% 
          req_perform() %>% 
          resp_body_json()
        ### Append new page to previous list
        all_repos <- c(all_repos, repo_content)
        #### Check if there are fewer than `per_page` items in the response
        if (length(repo_content) < per_page) {
          break
        }
        #### Increment the page number
        page <- page + 1
      }
      owner <- sapply(all_repos, function(x) x$owner$login)
      repo <- sapply(all_repos, function(x) x$name)
      description <- unlist(replace_null(sapply(all_repos, function(x) x$description)))
      url <- sapply(all_repos, function(x) x$html_url)
      full_name <- sapply(all_repos, function(x) x$full_name)
      formatted_repos <-
        tibble(
          Owner = owner,
          Repo = repo, 
          Description = description,
          URL = glue::glue("<a href='{url}' target='_blank'>{url}</a>"),
          full_name = full_name
        )
    return(formatted_repos)
    } else {
      return(NULL)
      }
  })
  
  ## Repo Selector
  output$repo_selector <- renderUI({
    repo_names <- repos()[["full_name"]]
    pickerInput(inputId = "repo", 
                label = "Select a repository to tag:", 
                choices = repo_names,
                multiple = TRUE,
                options = 
                  pickerOptions(
                    container = "body", 
                    selectedTextFormat = "count > 3"),
                    width = "100%"
                  )
    })
  
  # Add Topic
  observeEvent(input$add_topic, {
    ## Add topic to all selected repos
    repo_owner <- repos() %>% 
      filter(full_name %in% input$repo) %>% 
      pull(Owner)
    repo <- repos() %>% 
      filter(full_name %in% input$repo) %>% 
      pull(Repo)
    put_status <- map2(.x = repo, .y = repo_owner, ~add_topic(owner = .y, repo = .x, topic = input$topic))
    ## Verify the status code of the topic addition
    output$status <- renderText({
      paste(put_status, sep = ",")
    })
  })
  
  ## Show all User GitHub Repos
  output$repo_table <- renderDataTable({
    req(repos())
    repos() %>% 
        DT::datatable(
          rownames = F,
          escape = FALSE,
          filter = list(position = "top", clear = FALSE),             
          options = list(
            search = list(regex = TRUE),
            scrollY = 250,
            paging = FALSE,
            columnDefs = list(
              list(
                targets = c(4),
                searchable = TRUE,
                visible = FALSE
              )
            )
          )
        )
  })
  observeEvent(input$repo_table_rows_selected, {
    selected_repo_row <- input$repo_table_rows_selected
    if (length(selected_repo_row) > 0) {
      repo_row_data <- repos()[selected_repo_row, 'full_name', drop = TRUE]
      updatePickerInput(session, "repo", selected = repo_row_data)
    }
  })
}

shinyApp(ui = ui, server = server)
