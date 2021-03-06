#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in the top right corner.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("plotly")
library(shiny)
#library(plotly)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Interactive data explorer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       ## add fileInput widget ##
       fileInput("uploadedfile", "Choose CSV File",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
       ## add x-axis selector
       selectInput("x_axis", "Select x-axis", choices=NULL),
       ## add y-axis selector
       selectInput("y_axis", "Select y-axis", choices=NULL),
       ## add factor selector
       selectInput("fac", "Select factor", choices=NULL),
       tableOutput('table')
     ),
      
     # Show a plot of the selected relationships
     mainPanel(
       ## add plotOutput("")
       plotOutput('plot')
     )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  uploaded_file <- eventReactive(input$uploadedfile, {
    read.csv(input$uploadedfile$datapath)
  })
  
  observeEvent(input$uploadedfile, {
      vars = names(uploaded_file())
      updateSelectInput(session, "x_axis", choices = vars)
      updateSelectInput(session, "y_axis", choices = vars)
      updateSelectInput(session, "fac", choices = vars)
      
    })
  
  output$table <- renderTable({
    uploaded_file()[, c(input$x_axis, input$y_axis, input$fac), drop = FALSE]
  }, rownames = TRUE)
    
  output$plot <- renderPlot({
    ggplot(uploaded_file(), aes_string(x = input$x_axis, y = input$y_axis)) + 
      geom_point(aes_string(col = input$fac)) +
      scale_color_gradientn(colours = rainbow(5))+
      ggtitle(input$uploadedfile$name) +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

