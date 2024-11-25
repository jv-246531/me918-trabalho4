library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

ui <- function(request) { 
  fluidPage(
  
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  
  tags$style(
    "body { background-color: #dee3ff;}"
  ),
  
  titlePanel("Modelo de Regressão Linear para Conjunto de Dados fornecidos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Faça o upload do banco de dados .csv aqui:", accept = ".csv"),
      
      uiOutput("variavel_resposta_ui"),  
      uiOutput("variaveis_preditoras_ui"),  
      actionButton("modelo_botao",
                   "Construir modelo"),
      bookmarkButton(title = "Realiza bookmark da aplicação, gerando uma URL para acesso."),
      shinyWidgets::colorPickr("point_color", "Escolha a cor dos pontos: ",
                 selected = "#3c0c52"),
      tags$a(href = "https://github.com/jv-246531/me918-trabalho4", 
             class = "btn btn-info", "Ajuda"),
      tags$a(href = "https://en.wikipedia.org/wiki/Linear_regression", 
             class = "btn btn-secondary", "Teoria necessária"),
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
}

server <- function(input, output, session) {
  
  uploaded_file <- reactiveVal(NULL)
  
  observeEvent(input$upload, {
    req(input$upload)
    uploaded_file(input$upload)
  })
  
  onRestored(function(state) {
    if (is.null(uploaded_file())) {
      showNotification("Arquivo não carregado. Por favor, refaça o upload.", type = "warning")
    }
  })
  
  dados <- reactive({
    req(input$upload)
    if (!endsWith(input$upload$datapath, ".csv")) {
      shinyFeedback::feedbackWarning(
        inputId = "upload", 
        show = input$upload,
        text = "Por favor, selecionar um arquivo no formato .csv."
      )
      return(NULL)
    }
    dados <- tryCatch({
      read_delim(uploaded_file()$datapath)
    }, error = function(e) {
      showNotification("Erro na leitura do arquivo: verifique se o formato está correto e tente novamente.", type = "error")
      return(NULL)
    })
    
    validate(
      need(nrow(dados) > 0, "Não há nenhuma informação no arquivo carregado."),
      need(ncol(dados) > 0, "As colunas do conjunto de dados não estão preenchidas."),
      need(any(sapply(dados, is.numeric)), "O arquivo deve conter no mínimo uma variável numérica, para construção do modelo.")
    )
    
    dados
  })
  
  output$variavel_resposta_ui <- renderUI({
    req(dados())
    selectInput("variavel_resposta", 
                "Selecione a variável resposta numérica que será predita", 
                choices = colnames(dados() %>% select(where(is.numeric))))
  })
  
  output$variaveis_preditoras_ui <- renderUI({
    req(dados(), input$variavel_resposta)
    checkboxGroupInput("variaveis_preditoras", 
                       "Selecione as variáveis preditoras", 
                       choices = colnames(dados() %>% 
                                            select(-input$variavel_resposta)), 
                       selected = colnames(dados()))
  })
  
  
  modelo <- eventReactive(input$modelo_botao, {
    req(dados())
    formula <- paste(paste0("`", input$variavel_resposta, "`"),
                         "~",
                         paste(c("1", paste0("`", input$variaveis_preditoras, "`")),
                               collapse = " + ")) %>%
      as.formula()
    
    modelo_regressao <- dados() %>%
      select(c(input$variaveis_preditoras, input$variavel_resposta)) %>%
      filter(complete.cases(.)) %>%
      lm(formula, data = .)
    
    return(modelo_regressao)
  })
  
  
  output$modelo_resultado <- renderPrint({
    req(dados())
    anova(modelo()) 
  })
  
  
  output$R2 <- renderText({
    req(dados())
    req(modelo())
    paste("O valor do coeficiente de determinação ajustado, R²adj, é:",
          round(summary(modelo())$adj.r.squared, 4))
  })
  
  output$grafico_predito_observado <- renderPlot({
    req(dados())
    req(modelo())
    
    var_resposta <- modelo() %>%
      formula %>%
      {all.vars(.)[1]}
    
    dados() %>%
      select(c(input$variaveis_preditoras, input$variavel_resposta)) %>%
      filter(complete.cases(.)) %>%
      mutate(preditos = modelo()$fitted.values,
             observados = .data[[var_resposta]] ) %>%
      ggplot() +
      geom_point(aes(x = preditos,
                     y = observados),
                 col = input$point_color) +
      geom_hline(yintercept = 0) +
      labs(title = "Gráfico de Valores Preditos x Valores Observados",
           x = "Valores Preditos",
           y = "Valores Observados") +
      theme_minimal()
  })
  
  
  output$grafico_residuos <- renderPlot({
    req(dados())
    req(modelo())
    dados() %>%
      select(c(input$variaveis_preditoras, input$variavel_resposta)) %>%
      filter(complete.cases(.)) %>%
      mutate(residuo = modelo()$residuals/sqrt(anova(modelo())["Residuals", "Mean Sq"]),
             preditos = modelo()$fitted.values) %>%
      ggplot() +
      geom_point(aes(x = preditos,
                     y = residuo),
                 col = input$point_color) +
      geom_hline(yintercept = 0) +
      labs(title = "Gráfico de Valores Preditos x Resíduos",
           x = "Valores Preditos",
           y = "Resíduos Semi-Studentizados") +
      theme_minimal()
  })
  
  output$grafico_normalidade <- renderPlot({
    req(dados())
    req(modelo())
    dados() %>%
      select(c(input$variaveis_preditoras, input$variavel_resposta)) %>%
      filter(complete.cases(.)) %>%
      mutate(residuo = modelo()$residuals/sqrt(anova(modelo())["Residuals", "Mean Sq"])) %>%
      ggplot(aes(sample = residuo)) +
      stat_qq(col = input$point_color) +
      stat_qq_line() +
      labs(title = "Gráfico Quantil-Quantil Para Resíduos do Modelo",
           x = "Quantis Normais Teóricos",
           y = "Quantis Empíricos") +
      theme_minimal()
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")
