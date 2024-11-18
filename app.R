library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

ui <- fluidPage(
  
  shinyFeedback::useShinyFeedback(),
  
  tags$style(
    "body { background-color: #dee3ff;}"
  ),
  
  titlePanel("Modelo de Regressão Linear para Conjunto de Dados fornecidos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Faça o upload do banco de dados .csv aqui:"),
      
      uiOutput("variavel_resposta_ui"),  
      uiOutput("variaveis_preditoras_ui"),  
      actionButton("modelo_botao", "Construir modelo")
    ),
    
    mainPanel(
      verbatimTextOutput("R2"),
      verbatimTextOutput("modelo_resultado"), 
      plotOutput("grafico_predito_observado"),
      plotOutput("grafico_residuos"),  
      plotOutput("grafico_normalidade")
    )
  )
)

server <- function(input, output, session) {
  
  dados <- reactive({
    read_csv(input$upload$datapath)
  })
  
  output$variavel_resposta_ui <- renderUI({
    selectInput("variavel_resposta", 
                "Selecione a variável resposta numérica que será predita", 
                choices = colnames(dados() %>%
                                     select(where(is.numeric))
                                   ))
  })
  
  output$variaveis_preditoras_ui <- renderUI({
    checkboxGroupInput("variaveis_preditoras", 
                       "Selecione as variáveis preditoras", 
                       choices = colnames(dados() %>%
                                            select(!input$variavel_resposta)), 
                       selected = colnames(dados()))
  })
  
  modelo <- eventReactive(input$modelo_botao, {
    
    formula <- paste(input$variavel_resposta,
                         "~",
                         paste(input$variaveis_preditoras,
                               collapse = " + ")) %>%
      as.formula()
    
    modelo_regressao <- lm(formula, data = dados())
    
    return(modelo_regressao)
  })
  
  
  output$modelo_resultado <- renderPrint({
    summary(modelo()) 
  })
  
  
  output$R2 <- renderText({
    req(modelo())
    paste("O valor do coeficiente de determinação ajustado, R²adj, é:",
          round(summary(modelo())$adj.r.squared, 4))
  })
  
  output$grafico_predito_observado <- renderPlot({
    req(modelo())
    dados() %>%
      mutate(preditos = modelo()$fitted.values) %>%
      ggplot() +
      geom_point(aes(x = preditos,
                     y = .data[[input$variavel_resposta]]),
                 col = "#3c0c52") +
      geom_hline(yintercept = 0) +
      labs(title = "Gráfico de Valores Preditos x Valores Observados",
           x = "Valores Preditos",
           y = "Valores Observados") +
      theme_minimal()
  })
  
  
  output$grafico_residuos <- renderPlot({
    req(modelo())
    dados() %>%
      mutate(residuo = modelo()$residuals/sqrt(anova(modelo_)["Residuals", "Mean Sq"]),
             preditos = modelo()$fitted.values) %>%
      ggplot() +
      geom_point(aes(x = preditos,
                     y = residuo),
                 col = "#3c0c52") +
      geom_hline(yintercept = 0) +
      labs(title = "Gráfico de Valores Preditos x Resíduos",
           x = "Valores Preditos",
           y = "Resíduos Semi-Studentizados") +
      theme_minimal()
  })
  
  output$grafico_normalidade <- renderPlot({
    req(modelo())
    dados() %>%
      mutate(residuo = modelo()$residuals/sqrt(anova(modelo_)["Residuals", "Mean Sq"])) %>%
      ggplot(aes(sample = residuo)) +
      stat_qq(col = "#3c0c52") +
      stat_qq_line() +
      labs(title = "Gráfico Quantil-Quantil Para Resíduos do Modelo",
           x = "Quantis Normais Teóricos",
           y = "Quantis Empíricos") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
