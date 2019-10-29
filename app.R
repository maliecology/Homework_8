

##### Homework 8, Mali Hubert, October 30th

# Creating an interactive plot with the iris data set

# Run the libraries for Shiny
library(shiny)
library(tidyverse)

head(iris)

# Limit the range of selectable sepal lengths to the actual range of sepal lengths
min.sep.length <- min(iris$Sepal.Length)
max.sep.length <- max(iris$Sepal.Length)

# Need a vector of axis variables as characters
axis_vars <- names(iris)

# Create a character vector of those columns of diamonds that are 
factor.indices <- vapply(iris, is.factor, TRUE) 

factor.columns <- axis_vars[factor.indices]


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Iris Viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Adding in a range slider
      sliderInput("sepalrange",
                  "Range of Sepal Length",
                  min = min.sep.length,
                  max = max.sep.length,
                  value = c(min.sep.length, max.sep.length)),
      
      
      # Select x and y variables
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = axis_vars,
                  selected = "x"),
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = axis_vars,
                  selected = "y"),
      
      # Add color
      selectInput(inputId = "color",
                  label = "Color", 
                  choices = factor.columns,
                  selected = "Sepal Length"),
      
      #Adding a title
      textInput(inputId = "title",
                label = "Plot Title"),
      
      actionButton("go", 
                   "Go!",
                   icon = icon("thumbs-up"))),

    
    # Show a plot of iris data frame
    mainPanel(
      plotOutput("iris_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Filter iris based on sepal length 
  filt_iris <- reactive({
    iris %>%
      filter(Sepal.Length >= min(input$sepalrange)) %>%
      filter(Sepal.Length <= max(input$sepalrange))
  })
  
  # Make the plot
  p_iris <- eventReactive(input$go, {
    ggplot(filt_iris(), aes_string(x = input$xvar, y = input$yvar, colour = input$color)) + 
      geom_point() + ggtitle(input$title)
  })
  
  
  # Create diagnostic output window to show what kind of output the double slider creates
  output$diagnostic <- renderText(
    input$sepalrange
  )
  
  # Create a dynamic plot
  output$iris_plot <- renderPlot(
    p_iris()
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
