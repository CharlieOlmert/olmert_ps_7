#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggrepel)

# reading in data from ps_7.rmd
read_data <- read_rds("error_data")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NYT/Upshot Polling Errors in Different Age Groups, 2018 Midterms"),
   
   # Sidebar with a dropdown input for age range and a checbox for line of best fit
   sidebarLayout(
     sidebarPanel(
       # age ranges and choice to show line of best fit
       selectInput("age", "Choose an Age Range:",
                   c("18-34" = "youngest",
                     "35-49" = "younger",
                     "50-64" = "older",
                     "65+" = "oldest")),
       checkboxInput("best_fit", label = "Line of Best Fit", value = FALSE),
       
       hr()
       ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("agePlot")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$agePlot <- renderPlot({
     
     # a series of if statements which show different graphs in the app depending 
     # on the age group that the user chooses. Also toggles whether or not to show the 
     # line of best fit. I discovered "if" statements in Ms. Fridkin's app, which we went over
     # in class. They have been EXTREMELY helpful.
     
     if (input$age == "youngest")
     {
       yngst_plot <- read_data %>% 
         ggplot(aes(x = yngst_per, y = accuracy, color = win_party)) +
         geom_point() +
         # labels
         labs(x = "Percentage of sample aged between 18 and 34", 
              y = "Accuracy of Poll", color = "Winning Party") +
         # linking the Democrat with the color blue and the Republican Races with red
         scale_color_manual(values=c("#0000FF", "#FF0000")) 
       
       print(yngst_plot)
       
       if (input$best_fit == TRUE) {
         # creates a straight line of best fit with no wide range around it.
         bf_line <- yngst_plot + geom_smooth(method = lm, se = FALSE)
         print(bf_line)
       }
     }
     
     else if (input$age == "younger")
     {
       yngr_plot <- read_data %>% 
         ggplot(aes(x = yngr_per, y = accuracy, color = win_party)) +
         geom_point() +
         # labels
         labs(x = "Percentage of sample aged between 35 and 49", 
              y = "Accuracy of Poll", color = "Winning Party") +
         # linking the Democrat with the color blue and the Republican Races with red
         scale_color_manual(values=c("#0000FF", "#FF0000"))
       
       print(yngr_plot)
       
       if (input$best_fit == TRUE) {
         # creates a straight line of best fit with no wide range around it.
         bf_line <- yngr_plot + geom_smooth(method = lm, se = FALSE)
         print(bf_line)
       }
     }
     
     else if (input$age == "older")
     {
       oldr_plot <- read_data %>% 
         ggplot(aes(x = oldr_per, y = accuracy, color = win_party)) +
         geom_point() +
         # labels
         labs(x = "Percentage of sample aged between 50 and 64", 
              y = "Accuracy of Poll", color = "Winning Party") +
         # linking the Democrat with the color blue and the Republican Races with red
         scale_color_manual(values=c("#0000FF", "#FF0000"))
       
       print(oldr_plot)
       
       if (input$best_fit == TRUE) {
         # creates a straight line of best fit with no wide range around it.
         bf_line <- oldr_plot + geom_smooth(method = lm, se = FALSE)
         print(bf_line)
       }
     }
     
     else if (input$age == "oldest")
     {
       oldst_plot <- read_data %>% 
         ggplot(aes(x = oldst_per, y = accuracy, color = win_party)) +
         geom_point() +
         # labels
         labs(x = "Percentage of sample older than 65", 
              y = "Accuracy of Poll", color = "Winning Party") +
         # linking the Democrat with the color blue and the Republican Races with red
         scale_color_manual(values=c("#0000FF", "#FF0000"))
       
       print(oldst_plot)
       
       if (input$best_fit == TRUE) {
         # creates a straight line of best fit with no wide range around it.
         bf_line <- oldst_plot + geom_smooth(method = lm, se = FALSE)
         print(bf_line)
       }
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

