#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @importFrom bslib card card_body sidebar
#' @importFrom dplyr filter mutate pull select
#' @importFrom DT DTOutput renderDT
#' @importFrom httr2 obfuscated oauth_client req_headers resp_body_string req_body_multipart
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr extract2
#' @importFrom purrr map2
#' @importFrom rlang .data
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' 
#' @noRd
#' 
app_server <- function(input, output, session) {
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
            client_id = github_client()$id, 
            client_secret = github_client()$secret, 
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
      unnest(.data$organization) %>% 
      mutate(project_detail_url = glue::glue("<a href='{project_detail_url}' target='_blank'>{project_detail_url}</a>")) %>% 
      select(.data$core_project_num, .data$project_num, .data$project_title, .data$project_detail_url, .data$org_name, .data$terms, .data$pref_terms)
  })
  
  output$project_table <- renderDT({
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
      pull(.data$Owner)
    repo <- repos() %>% 
      filter(full_name %in% input$repo) %>% 
      pull(.data$Repo)
    put_status <- map2(.x = repo, .y = repo_owner, ~add_topic(owner = .y, repo = .x, topic = input$topic))
    ## Verify the status code of the topic addition
    output$status <- renderText({
      paste(put_status, sep = ",")
    })
  })
  
  ## Show all User GitHub Repos
  output$repo_table <- renderDT({
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
