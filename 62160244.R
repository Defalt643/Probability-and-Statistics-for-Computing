library(UsingR)
ui <- fluidPage(titlePanel("weight and neck"),
                
                fluidRow(
                  column(7,plotOutput("scatterplot")),
                  column(5,numericInput("num", h3("Input your weight(lbs.)"), value = 0),offset = 0),
                  column(5,submitButton("Submit")),
                  column(5,h3(textOutput("num_output")))
                 
                  ),
               
                fluidRow(column(12,h5("Note:"),helpText("Name : Khummeung Wattanasaroj ID : 62160244"))))
 
# Define server logic ----
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    plot(fat$weight, fat$neck, main="Relationship between weight and neck",xlab="weight (lbs.)", ylab="neck (cm.)", pch=19)
    lines(lowess(fat$weight,fat$neck), col="blue")
    abline(lm(fat$neck ~ fat$weight), col="red")
  }, height=400)
  output$num_output <- renderText({
    model=lm(fat$neck~fat$weight)
    answer=model$coefficients[1]+(model$coefficients[2]*input$num)
    answer=format(round(answer, 2), nsmall = 2)
    paste("Your neck is ", answer,"cm.")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)