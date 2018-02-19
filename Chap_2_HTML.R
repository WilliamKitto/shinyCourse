# Creating and formatting HTML output
# In the previous exercise you developed an app that reported averages of selected x and y variables as two separate outputs. An alternative approach would be to combine them into a single, multi-line output.
# 
# Use values calculated in app chunk in the paste() command to create customized HTML output with specified formatting (centered)
# 
# INSTRUCTIONS
# 100XP
# In the server, create a new output, named output$avgs, that replaces output$avg_x and output$avg_y (defined starting on line 46). For this output, calculate avg_x and avg_y like you did before, save the output text strings as str_x and str_y, and finally combine these two text strings with HTML(paste(str_x, str_y, sep = '<br/>')). Use the renderUI() function to render this output.
# In the UI, replace the textOutputs with one call to htmlOutput, calling the new HTML text string you created in the server.

library(shiny)
library(dplyr)
library(ggplot2)
load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))

# UI
ui <- fluidPage(
  sidebarLayout(
    
    # Input(s)
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "audience_score"),
      
      # Select variable for x-axis
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "critics_score")
      
    ),
    
    # Output(s)
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      htmlOutput(outputId = "avgs"),
      verbatimTextOutput(outputId = "lmoutput")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
  # Calculate averages
  output$avgs <- renderUI({
    avg_x <- movies %>% pull(input$x) %>% mean(,na.rm=TRUE) %>% round(2)
    avg_y <- movies %>% pull(input$y) %>% mean() %>% round(2)
    str_x <- paste("Average", input$x, "=", avg_x)
    str_y <- paste("Average", input$y, "=", avg_y)
    HTML(paste(str_x, str_y, sep = '<br/>'))
  })
  
  # Create regression output
  output$lmoutput <- renderPrint({
    x <- movies %>% pull(input$x)
    y <- movies %>% pull(input$y)
    print(summary(lm(y ~ x, data = movies)), digits = 3, signif.stars = FALSE)
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)