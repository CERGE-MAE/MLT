library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MTCARS Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("xlim",
                     "Maximum of x axis:",
                     min = 1,
                     max = 500,
                     value = 50),
         selectInput("x", label = "x axis", choices = colnames(mtcars)),
         selectInput("y", label = "y axis", choices = colnames(mtcars))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h2(textOutput("desc")),
         h3(textOutput("desc2")),
         h4(textOutput("time")),
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$desc2 = reactive({
       paste0("The chosen limit of X axis is ", input$xlim,"!")
   })
   # react
   
   output$desc = reactive({
       
       if (input$x == input$y) {
           paste0("You have chosen the same variable ", input$x, " for both axes. Is that what you have really wanted to do?")
       } else {
           paste0("You have chosen the variable ", input$x, " for X axis and variable ", input$y, " for Y axis.")
       }
       
   })
   
   output$time = reactive({
       paste0("Analysis of ", input$x, " and ", input$y,
              " started at ", Sys.time(),
              ". Maximum value of variable ", input$x, " is ",
              max(mtcars[,input$x]), " initial limit was ",
              isolate(input$xlim), ".")
       #isolate
   })
   
   output$plot <- renderPlot({
       
       # plot chosen variables
       plot(mtcars[,input$x], mtcars[,input$y], xlim = c(0,input$xlim))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)