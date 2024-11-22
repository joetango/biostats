library(shiny)

## Global Functions

## DC Check

difficulty_check <- function(dc1, mod1=0){
  roll <- sample(1:20, 1)
  if((roll + mod1) >= dc1){
    paste("Roll:", roll, "+ Modifer:", mod1, "=", (roll + mod1), "...Success!")
  } 
  else {
    paste("Roll:", roll, "+ Modifer:", mod1, "=", (roll + mod1), "...Failure")
  }
}


ui <- fluidPage(
  h4("Difficulty Check"),
  selectInput("dc", "Select Difficulty Class", choices = c(1:20)),
  numericInput("mod", "Enter Modifier:", value = 0),
  actionButton("dc_roll", "Roll"),
  textOutput("dc_result")


) #ui


server <- function(input, output) {
  
  dc_result <- eventReactive(input$dc_roll, {
    difficulty_check(as.numeric(input$dc), input$mod)
  })

  output$dc_result <- renderText({
    dc_result()
  })
    
} #server


shinyApp(ui = ui, server = server)
