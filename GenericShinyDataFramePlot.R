library(shiny)
library(ggplot2)
#load(url("http://s3.amazonaws.com/assets.datacamp.com/production/course_4850/datasets/movies.Rdata"))
require(tigerstats)
data(ucdavis1)
mydata <- ucdavis1 # just plug in your data here

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = names(mydata),
                  selected = names(mydata)[1]),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = names(mydata),
                  selected = names(mydata)[2]),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = names(mydata),
                  selected = names(mydata)[3])
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = mydata, aes_string(x = input$x, y = input$y,
                                             color = input$z)) +
      geom_point()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)