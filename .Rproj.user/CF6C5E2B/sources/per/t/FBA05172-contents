library(shiny)
library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(


  #Drop down
      selectInput("select", label = h3("Select box"),
              choices = list("T-test" = 1, "F-test" = 2, "Correlation" = 3),
              selected = 1),


  # Slider

      sliderInput("Participants", label = h3("Slider"), min = 0,
              max = 20, value = 10),

  #Table

      rHandsontableOutput("Table"),
        br(),

  # Action

       actionButton("saveBtn", label = "Save"),

        hr(),
        fluidRow(column(2, verbatimTextOutput("value")))


)

# Define server logic required to draw a histogram
server <- function(input, output) {

  Group1=sample(1:10)
  Group2=sample(1:10)
  Sum= Group1+Group2
  df1 =data.frame(Group1=Group1, Group2=Group2, Sum=Sum)

  datavalues <- reactiveValues(data=df)


  output$Table <-renderRHandsontable({
    rhandsontable(df1)

  })

  observeEvent(input$saveBtn,{
    df<-   data.frame(hot_to_r(input$Table))
    a <-df[,1]
    b <-df[,2]
    print(t.test(a,b))


  })

}

# Run the application
shinyApp(ui = ui, server = server)

