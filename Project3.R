#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(kableExtra)
IMO <- read.csv("IMO.csv")

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
          radioButtons("radio", label=("Select Variable to Analyze"),
                       choices = list("Team Size"= 1, "Total Points" = 2, "Total Awards" = 3, "Country" = 4, "Year"=5),
                       selected = 1), 
          
          # Slider to display variables depending on selected year
          
          sliderInput("slider",
                        "Select Year Range",
                        min = 2020,
                        max = 2024,
                        value = c(2020,2024)),
         
           # Drop down menu to select graph color
          
          selectInput("color", label = "Select graph color", 
                      choices = list("Violet"= "violet", "Lime Green"= "yellowgreen", "Yellow"="yellow", "Dark Pink"="violetred"),
                      selected = "violet"),
          
          # Add check box that updates graphs
          
          checkboxInput("mean_line", label="Show Mean (numerical variable)", value=FALSE),
          checkboxInput("add_percentage", label= "Show Percentages (categorical variable)", value = FALSE),
          
          # Display Image
          img(src = "math_image.png", style = "float: right; width: 100%;"),
          p("Source: International Mathematical Olympiad")
          
          
          ),


        mainPanel(
           plotOutput("distPlot"),
           hr(),
           
           # Calculate Descriptive Statistics
           
           p("Mean: "),
           fluidRow(column(5, verbatimTextOutput("mean"))),
           p("Standard Deviation: "),
           fluidRow(column(5, verbatimTextOutput("sd"))),
           p("Proportion Table: "),
           fluidRow(column(11,verbatimTextOutput("prop.table"))),
           
        )
    )
)

# Define server logic 
server <- function(input, output) {
  
  # Allow for Data Filtering Based on Selected Years
  
  year_data <- reactive({
    IMO[IMO$year >= input$slider[1] & IMO$year <= input$slider[2],]
  })
  
    output$distPlot <- renderPlot({
        # Generate graph based on chosen variable
        variable <- input$radio
        data <- year_data()
        
        if(variable == 1){
          hist(data$team_size_all,
               main = "Histogram of Team Size during the IMO",
               xlab = "Team Size",
               col = input$color
               )
          
          if(input$mean_line == TRUE){
            abline(v= mean(data$team_size_all), col = "darkred", lwd = 2)
          }
          
        } else if(variable == 2){
          hist(data$total_points,
               main = "Histogram of Total Team Points during the IMO",
               xlab = "Total Points",
               col = input$color
          )
          
          if(input$mean_line == TRUE){
            abline(v= mean(data$total_points), col = "darkred", lwd = 2)
          }
        }else if(variable == 3){
          hist(data$total_awards,
               main = "Histogram of Total Team Awards during the IMO",
               xlab = "Total Awards",
               col = input$color
          )
          
          if(input$mean_line == TRUE){
            abline(v= mean(data$total_awards), col = "darkred", lwd = 2)
          }
        }else if(variable == 4){
          
         filtered_data <- data[data$total_points >= 189 & data$total_points <= 252, ]
        best_countries <- table(filtered_data$country)
        
        if(input$add_percentage == TRUE){
          pie(sort(best_countries, decreasing = TRUE),
              main = "Top Performing Countries: Scoring 75% to 100% of Maximum Points",
              col = rainbow(length(best_countries)),
              labels = paste0(round(prop.table(best_countries) * 100, 1), "%"),
          )
          
        } else{
          pie(sort(best_countries, decreasing = TRUE),
              main = "Top Performing Countries: Scoring 75% to 100% of Maximum Points",
              col = rainbow(length(best_countries))
              )
          
        }
          
        } else if(variable == 5){
          hist(data$year,
               main = "Histogram of IMO's popularity based on year",
               xlab = "Year",
               col = input$color
  
          )
          if(input$mean_line == TRUE){
            abline(v= mean(data$year), col = "darkred", lwd = 2)
          }
        }
        
      
    })
    
    # Display Descriptive Statistics 
    
    output$mean <- renderPrint({
      
      variable = input$radio
      data <- year_data()
      if (variable == 1){
        mean(data$team_size_all)
        
      } else if (variable == 2){
        mean(data$total_points)
        
      } else if(variable == 3){
        mean(data$total_awards)
        
      } else if(variable ==4){
        print("NA")
  
      } else if(variable == 5){
        mean(data$year)
      }
      
    })
    
    output$sd <- renderPrint({
      
      variable = input$radio
      data <- year_data()
      
      if (variable == 1){
        sd(data$team_size_all)
        
      } else if (variable == 2){
        sd(data$total_points)
        
      } else if(variable == 3){
        sd(data$total_awards)
        
      } else if(variable ==4){
        print("NA")
      } else if(variable == 5){
        sd(data$year)
      }
      
    })
    
    output$prop.table <- renderPrint({
      
      variable = input$radio
      data <- year_data()
      if (variable == 1){
      print("NA")
        
      } else if (variable == 2){
        print("NA")
        
      } else if(variable == 3){
        print("NA")
        
      } else if(variable ==4){
        
        filtered_data <- data[data$total_points >= 189 & data$total_points <= 252, ]
        best_countries <- table(filtered_data$country)
        prop.table(best_countries)
        
      } else if(variable == 5){
        print("NA")
      }
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
