#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyjs useShinyjs hidden show
#' @importFrom bslib page_fluid layout_sidebar
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
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
              div(DTOutput("project_table") %>% withSpinner())
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
                  div(DTOutput("repo_table") %>% withSpinner())
                )
              ),
              textOutput("status")
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CFDE.HelpR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
