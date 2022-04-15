library(shiny)

ui <- fluidPage(
  selectInput(inputId = "Q",
              label = "Survey Question",
              choices = c("Aggregate", "Question 1")),
  plotOutput("sent_col")
)

server <- function(input, output) {
  output$sent_col <- renderPlot({ 
    data <- switch(input$Q,
                   "Aggregate" = agg_survey_afinn_sent,
                   "Question 1" = agg_q1_afinn_sent)
    
    ggplot(data, aes(x = reorder(Subject, mean), y = mean, fill = pos)) +
                                         geom_col(show.legend = FALSE) +
                                         labs(title = "Overall Sentiment of Survey Responses",
                                              x = "Subject",
                                              y = "Sentiment") +
                                         theme(axis.text.y = element_blank(),
                                               axis.ticks.y = element_blank(),
                                               panel.grid.major.y = element_blank()) +
                                         coord_flip() })
}

shinyApp(ui = ui, server = server)