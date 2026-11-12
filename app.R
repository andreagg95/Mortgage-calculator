ui <- fluidPage(
    tags$head(
      tags$style(HTML("
      body {
        background-color: #f4f6f8;
      }

      .mortgage-card {
        max-width: 650px; margin: 50px auto;
        padding: 30px 35px; background-color: white;
        border-radius: 15px; box-shadow: 0 4px 20px rgba(0,0,0,0.08);
      }

      .mortgage-title {
        text-align: center; color: #1f3c5b;
        font-weight: 700; margin-bottom: 5px;
      }

      .mortgage-subtitle {
        text-align: center; color: #6c757d;
        margin-bottom: 30px;
      }

      .form-group {
        margin-bottom: 20px;
      }

      #clicca {
        width: 100%; background-color: #1f77b4;
        color: white; border: none; border-radius: 8px;
        padding: 12px; font-size: 16px; font-weight: 600;
        margin-top: 10px;
      }

      #clicca:hover {
        background-color: #165a87;
      }

      .result-box {
        margin-top: 25px; padding: 20px;
        background-color: #eaf4fb;
        border-radius: 10px; text-align: center;
        font-size: 22px; font-weight: 600;
        color: #1f3c5b;
      }
    "))
    ),

    div(
      class = "mortgage-card",
      h2(
        class = "mortgage-title", "Mortgage Calculator"
      ),
      p(
        class = "mortgage-subtitle",
        "Calculate your estimated monthly mortgage payment"
      ),

      numericInput(
        inputId = "capitale",
        label = "Mortgage amount (€)", value = 100000,
        min = 0, step = 1000
      ),

      sliderInput(
        inputId = "anni",
        label = "Mortgage duration (years)",
        value = 20, min = 1, max = 40, step = 1
      ),

      numericInput(
        inputId = "tasso", label = "Annual interest rate (%)",
        value = 2, min = 0, step = 0.1
      ),

      actionButton(
        inputId = "clicca", label = "Calculate monthly payment"
      ),

      div(
        class = "result-box", textOutput(outputId = "risultato")
      )
    )


)




server <- function(input, output, session) {
  capitale <- reactive(input$capitale)
  anni <- reactive(input$anni)
  tasso <- reactive(input$tasso/100)

  rata <- reactiveVal("   ")

  observeEvent(input$clicca,{
    tasso_mens <- tasso()/12
    # siccome hai il TAN, devi dividerlo per 12, quello è il tasso mensile
    p <- 1/(1+tasso_mens)
    mesi <- (anni()*12+1)
    somma_geom <- ((1-p^(mesi))/(1-p))-1
    R <- capitale()/somma_geom
    R <- round(R,2)

    rata(R)

  })

  output$risultato <- renderText({
    paste0("The mortgage payment is equal to ", rata(), " €")
  })


}

shinyApp(ui = ui, server = server)
