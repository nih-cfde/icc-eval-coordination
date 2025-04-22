# Helper Functions ----
#' Replace NULL
#' 
#' @description
#' Replace NULL values in a list with NA
#' @param x A list
#' 
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
#' @param .token A GitHub Access token with appropriate permissions
#' 
#' @importFrom glue glue
#' @importFrom httr2 request req_auth_bearer_token req_body_raw req_method req_perform resp_body_json
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>% 
#' 
  add_topic <- function(owner, repo, topic, .token = NULL) {
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