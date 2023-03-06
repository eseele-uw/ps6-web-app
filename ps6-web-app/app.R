library(shiny)
library(tidyverse)

bob <- read_delim("bob_ross_statistics.csv")

ui <- fluidPage(
    titlePanel("Bob Ross Statistics"),
    tabsetPanel(type = "tabs",
                tabPanel("About", 
                        p("This app analyzes the elements in Bob Ross Paintings from his show."),  
                        p("There are ", strong(nrow(bob)), " rows and ", strong(ncol(bob)),
                          " variables in the dataset"),
                        p("The variable ", em("included"), " is the number of times the
                          element was in the specific painting"),
                        p("Here is a sample of the data:"),
                        tableOutput("sample")
                ),
                tabPanel("Plot",
                         sidebarLayout(
                           sidebarPanel(
                              p("You can analyze the variation in the number 
                                of the selected element throughout the seasons
                                of the show."),
                              radioButtons("color", "Choose color",
                                           choices = c("skyblue", "salmon", "seagreen3",
                                                                "lightslateblue", "maroon")),
                              uiOutput("elements")
                           ),
                           mainPanel(
                             plotOutput("plot"),
                             textOutput("plot_summary")
                           )
                         )
                ),
                tabPanel("Table",
                         sidebarLayout(
                           sidebarPanel(
                             p("This table shows the different elements ranked by number of 
                               appearances for each season."),
                             uiOutput("seasons")
                           ),
                           mainPanel(
                             textOutput("table_summary"),
                             tableOutput("table")
                           )
                         )
                )
    )
)
server <- function(input, output) {
    output$sample <- renderTable({
      bob %>% 
        sample_n(6)
    })
    
    output$elements <- renderUI({
      radioButtons("element", "Choose element",
                         choices = unique(bob$element)
      )
    })
    
    output$seasons <- renderUI({
      seas_samp <- bob %>% 
        mutate(season = substring(episode, 1, 3))
      
      radioButtons("season", "Choose season",
                   choices = unique(seas_samp$season)
      )
    })
    
    output$table <- renderTable({
      bob %>% 
        mutate(the_season = substring(episode, 1, 3)) %>% 
        filter(the_season == input$season) %>%
        group_by(element) %>% 
        summarize(element = unique(element), total = sum(included)) %>% 
        arrange(desc(total))
    })
    
    sample <- reactive({
      bob %>% 
        filter(element %in% input$element) %>% 
        mutate(season = substring(episode, 1, 3))
    })
    
    output$plot_summary <- renderText({
      paste("You have chosen to analyze the element, ", input$element, ".", sep = "")
    })
    
    output$table_summary <- renderText({
      paste("The season being analyzed is ", input$season, sep = "")
    })
    
    output$plot <- renderPlot({
      sample() %>% 
        ggplot(aes(season, included)) +
        geom_col(fill = input$color) +
        labs(title = input$element, x = "Season", y = "Number of times appeared")
    })
}
 
shinyApp(ui = ui, server = server)
