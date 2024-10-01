library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(reactable)

nba.salary <- read.csv("nbasalary.csv")

# UI
ui <- fluidPage(
  titlePanel("NBA Salary Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tabs", "Select a Tab:", 
                  choices = c("Salary Distribution", "Minutes Played vs Salary", 
                              "Salary vs Points Per Game vs Minutes Played", "Top 10 Paid Players")),
      conditionalPanel(
        condition = "input.tabs == 'Salary Distribution'",
        sliderInput("bins",
                    "Number of bins:",
                    min = 50,
                    max = 100,
                    step = 5,
                    value = 75),
        selectInput("selected_year",
                    label = "Select Year:",
                    choices = unique(nba.salary$Season),
                    selected = "2020-2021")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Minutes Played vs Salary'",
        selectInput("selected_team_mp",
                    label = "Select Team:",
                    choices = unique(nba.salary$Team),
                    selected = unique(nba.salary$Team)[1]),
        selectInput("selected_season_mp",
                    label = "Select Season:",
                    choices = unique(nba.salary$Season),
                    selected = unique(nba.salary$Season)[1]),
        actionButton("update_mp", "Update!")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Salary vs Points Per Game vs Minutes Played'",
        selectInput("selected_team_3d",
                    label = "Select Team:",
                    choices = unique(nba.salary$Team),
                    selected = unique(nba.salary$Team)[1]),
        selectInput("selected_season_3d",
                    label = "Select Season:",
                    choices = unique(nba.salary$Season),
                    selected = unique(nba.salary$Season)[1]),
        actionButton("update_3d", "Update!")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Top 10 Paid Players'",
        selectInput("selected_team_table",
                    label = "Select Team:",
                    choices = c("All Teams", unique(nba.salary$Team)),
                    selected = "All Teams"),
        selectInput("selected_season_table",
                    label = "Select Season:",
                    choices = c("All Seasons", unique(nba.salary$Season)),
                    selected = "All Seasons")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Salary Distribution", plotlyOutput("salary.dist")),
        tabPanel("Minutes Played vs Salary", plotlyOutput("mp_salary_plot")),
        tabPanel("Salary vs Points Per Game vs Minutes Played", plotlyOutput("scatterplot_3d")),
        tabPanel("Top 10 Paid Players", reactableOutput("top10_table"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Salary Distribution
  output$salary.dist <- renderPlotly({
    filtered_data <- nba.salary %>% filter(Season == input$selected_year)
    
    nba_ggplot <- filtered_data %>%
      ggplot(aes(x = Salary)) + 
      geom_histogram(fill='lightblue', position="dodge", bins = input$bins) +
      labs(title = "NBA Player Salary Distribution",
           x = "Salary (USD)",
           y = "Count of Players") +
      theme_minimal()
    
    ggplotly(nba_ggplot)
  })
  
  # Minutes Played vs Salary
  team_name_mp <- eventReactive(input$update_mp, {
    team_filtered <- nba.salary %>%
      filter(Team == input$selected_team_mp, Season == input$selected_season_mp)
    
    list(
      data = team_filtered,
      title = paste("Minutes Played vs Salary for", input$selected_team_mp, "in", input$selected_season_mp)
    )
  })
  
  output$mp_salary_plot <- renderPlotly({
    req(team_name_mp())  # Ensure data is available before rendering
    
    plot_ly(data = team_name_mp()$data, 
            x = ~MP, 
            y = ~Salary, 
            type = 'scatter', 
            mode = 'markers', 
            marker = list(color = 'blue'),
            text = ~paste("Player: ", Player, "<br>MP: ", MP, "<br>Salary: $", Salary),  
            hoverinfo = 'text') %>%
      layout(title = team_name_mp()$title,
             xaxis = list(title = "Minutes Played (MP)"),
             yaxis = list(title = "Salary (USD)"))
  })
  
  # 3D Scatter Plot: Salary vs Points Per Game vs Minutes Played
  filtered_pts <- eventReactive(input$update_3d, {
    nba.salary %>%
      filter(Team == input$selected_team_3d, Season == input$selected_season_3d)
  })
  
  plot_title_3d <- eventReactive(input$update_3d, {
    paste("Salary vs Points Per Game vs Minutes Played:", input$selected_team_3d, "in", input$selected_season_3d)
  })
  
  output$scatterplot_3d <- renderPlotly({
    plot_ly(
      data = filtered_pts(),
      x = ~MP, 
      y = ~PTS,  
      z = ~Salary, 
      text = ~Player,  
      type = "scatter3d", 
      mode = "markers",
      marker = list(size = 5, color = ~Salary, colorscale = 'Viridis', showscale = TRUE)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Minutes Played (MP)"),
          yaxis = list(title = "Points Per Game (PTS)"),
          zaxis = list(title = "Salary (USD)")
        ),
        title = plot_title_3d()
      )
  })
  
  # Top 10 Paid Players
  top10_players <- reactive({
    filtered_data <- nba.salary
    
    if (input$selected_team_table != "All Teams") {
      filtered_data <- filtered_data %>%
        filter(Team == input$selected_team_table)
    }
    
    if (input$selected_season_table != "All Seasons") {
      filtered_data <- filtered_data %>%
        filter(Season == input$selected_season_table)
    }
    
    filtered_data %>%
      arrange(desc(Salary)) %>%                
      select(Player, Salary, Team, Season, MP, PTS, AST, TRB) %>%  
      head(10)                                 
  })
  
  output$top10_table <- renderReactable({
    data <- top10_players()
    
    salary_normalized <- (data$Salary - min(data$Salary)) / 
      (max(data$Salary) - min(data$Salary))
    salary_colors <- rgb(colorRamp(c("white", "darkorange"))(salary_normalized), maxColorValue = 255)
    
    reactable(
      data,
      filterable = TRUE,
      pagination = TRUE,       
      defaultPageSize = 10,    
      showPagination = FALSE,
      columns = list(
        Player = colDef(name = "Player Name"),
        Salary = colDef(name = "Salary (USD)", 
                        format = colFormat(currency = "USD", 
                                           separators = TRUE), 
                        filterable = FALSE,
                        style = JS("function(rowInfo, column, state) {
                          const salaryColors = state.meta.salaryColors;
                          return { backgroundColor: salaryColors[rowInfo.index] };
                        }")
        ),
        Team = colDef(filterable = FALSE),
        Season = colDef(filterable = FALSE),
        MP = colDef(name = "Minutes Played per Game", filterable = FALSE),
        PTS = colDef(name = "Points per Game", filterable = FALSE),
        AST = colDef(name = "Assists per Game", filterable = FALSE),
        TRB = colDef(name = "Rebounds per Game", filterable = FALSE)
      ),
      defaultSorted = list(Salary = "desc"),
      highlight = TRUE,           
      meta = list(salaryColors = salary_colors)  
    )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
