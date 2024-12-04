#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IMO Data Visualizaion App"),
    
    # Add Instructions
    p("- The folloiwng app allows for data visualization of different variables from the International Mathematical Olympiad"),
    p("- To engage with the app, simply select the variable you would like to visualize and it will create the appropriate graph to represent it"),
    p("- You can modify the range of data displayed on the graph by interacting with the slider widget as well as change the graph colors"),
    p("- To obtain a deeper understanding, descriptive statistics pertaining to each graph will be shown"),

 
    sidebarLayout(
        sidebarPanel(
          
          #Radio Button Input
          radioButtons("radio", label=h3("Variable of Interest"),
                       choices = list("Team Size"= 1, "Total Points" = 2, "Total Awards" = 3, "Gender" = 4, "Coutry"=5),
                       selected = 1), 
          
          # Slider to display variables depending on selected year
          
          sliderInput("slider",
                        "Year Range",
                        min = 2020,
                        max = 2024,
                        value = c(2020,2024))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
