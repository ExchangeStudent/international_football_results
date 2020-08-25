library("shiny")
library("tidyverse")
library("shinyWidgets")
library("shinydashboard")

# load data
results <- read_csv("results.csv")

# format dates
# dates after 1900 have been formated as "%d/%m/%Y" in the original data
# dates before 1900 have been formatted as "%Y-%m-%d" in the original data
results <- results %>% mutate(game_date = as.Date(ifelse(is.na(as.Date(date, "%d/%m/%Y")),as.Date(date, "%Y-%m-%d"),as.Date(date, "%d/%m/%Y")), origin="1970/01/01"))

ui <- fluidPage(
  titlePanel("International Football Results by Team"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("team", "Select team", choices=sort(unique(c(results$home_team,results$away_team))), options=list(title = "Select team")),
      pickerInput("tournament", "Select tournament", choices=NULL, options=list(title = "Select tournament")),
      dateRangeInput("date", "Select period",start=min(results$game_date), end= max(results$game_date), format="dd-MM-yyyy")
                ),
    mainPanel(
      tabsetPanel(
        tabPanel("stats",
                 fluidRow(
                   valueBox(value=textOutput("played"), subtitle = "Games played", width=2, icon=icon(name="futbol", class="fas fa-futbol")),
                   valueBox(value=textOutput("wins"), subtitle = "Wins", width=2, icon=icon('trophy')),
                   valueBox(value=textOutput("losses"), subtitle = "Losses", width=2),
                   valueBox(value=textOutput("draws"), subtitle = "Draws", width=2),
                   valueBox(value=textOutput("percent_wins"), subtitle = "% wins", width=2),
                 ),

                 fluidRow(
                   column(width = 12, offset = 0, style='padding-left:0px; padding-right:150px; padding-top:100px; padding-bottom:0px',
                   DT::DTOutput('data_table')
                   )
                 )
        ),
        tabPanel("data",
                 downloadButton("download")
        )
               )
    )
             )
)


server <- function(input,output,session){

  # stops app when browser is closed
  session$onSessionEnded(stopApp)

  team_data <- reactive({filter(results, home_team==input$team | away_team==input$team)})

  observeEvent(team_data(), {
    choices <- sort(unique(team_data()$tournament))
    updatePickerInput(session, "tournament", choices = choices)
  })

  tournament_data <- reactive({filter(team_data(), tournament==input$tournament)})

  analysis_data <- reactive({filter(tournament_data(), game_date>=input$date[1] & game_date<=input$date[2])})

  output$data_table <- DT::renderDT({analysis_data()[-1]})

  # summary statistics
  # generate reactive object to be used more than once
  played <- reactive({nrow(analysis_data())})

  output$played <- renderText(played())

  # generate reactive object to be used more than once
  wins <- reactive({analysis_data() %>%
                              filter((home_team == input$team & home_score > away_score) | (away_team == input$team & away_score > home_score)) %>%
                              nrow()})

  output$wins <- renderText(wins())

  output$losses <- renderText(analysis_data() %>%
                              filter((home_team == input$team & home_score < away_score) | (away_team == input$team & away_score < home_score)) %>%
                              nrow())

  output$draws <- renderText(analysis_data() %>% filter(home_score==away_score) %>% nrow())

  output$percent_wins <- renderText(ifelse(is.na(round(wins()/played()*100)),"-",round(wins()/played()*100)))

}


shinyApp(ui = ui, server = server)

