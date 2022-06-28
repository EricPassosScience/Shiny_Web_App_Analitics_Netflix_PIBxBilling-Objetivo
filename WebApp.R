# Analytics Web App Para Grandes Volumes de Dados: Como o PIB e a Desigualdade Social Influenciam no Crescimento da Netflix?


# Configurando o diretório de trabalho
#setwd("C:/Users/Eric/Documents/Bigdatanapratica/BigData")
#getwd()

########## Dashboard Analítico ##########

# Imports
library(shiny)
library(plotly)
library(shinythemes)

# Carrega o primeiro dataset limpo
dataset1 <- read.csv("datasets_limpos/dataset1.csv")

# Ajusta o tipo de dado de algumas colunas
dataset1$X..of.Subscribers.Q4.2021..Estimate. <- as.numeric(gsub(",", "", dataset1$X..of.Subscribers.Q4.2021..Estimate.))
dataset1$Q4.2021.Revenue....Estimate. <- as.numeric(gsub(",", "", dataset1$Q4.2021.Revenue....Estimate.))

# Cria dataframes filtrando os outliers
dataset1_scat_out <- filter(dataset1, Country != "United States")
dataset1_bar <- filter(dataset1, Country != "Switzerland")
dataset1_bar_out <- filter(dataset1_bar, Country != "South Africa")

# Carrega os datasets 2, 3 e 6
genre <- read.csv("datasets_limpos/dataset2.csv")
tree <- read.csv("datasets_limpos/dataset3.csv")
countries <- read.csv("datasets_limpos/dataset6.csv")

# Filtra a lista de países removendo valores NA
country_list <- filter(countries, is.na(parents))

########## UI - User Interface ##########

# https://rstudio.github.io/shinythemes/
ui <- navbarPage(theme = shinytheme("cerulean"), 
                 
                 "Big Data na Prática 2",

                 tabPanel("Visão Geral",
                          sidebarLayout(
                            sidebarPanel(                              
                              selectInput("select", 
                                          label = h4("Selecione a Variável do Eixo Y:"), 
                                          choices = list("Faturamento da Netflix Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                                                         "Assinaturas da Netflix Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate.",
                                                         "Tamanho Total do Catálogo" = "Total.Library.Size", 
                                                         "Preço da Assinatura Basic" = "Cost.Per.Month...Basic....", 
                                                         "Preço da Assinatura Standard"= "Cost.Per.Month...Standard....",
                                                         "Preço da Assinatura Premium" = "Cost.Per.Month...Premium...."), 
                                          selected = 1),
                              checkboxInput("outlierscatter", "Mostrar Outlier", FALSE)),
                          mainPanel(
                            plotlyOutput("scatPlot")))
                 ),
                 tabPanel("Desigualdade Salarial",
                          h4("Disparidade de Renda e Diferenças nos Preços da Assinatura Basic, Standard e Premium da Netflix (Mensal)"),
                          sidebarPanel(
                            checkboxInput("outlierbar", "Mostrar Outlier", FALSE)),
                          mainPanel(
                            plotlyOutput("barPlot"))
                 ),
                 tabPanel("Gêneros Populares",
                            tabPanel("Country", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("Country", 
                                                     label = h3("Selecione o País:"), 
                                                     choices = country_list$labels, 
                                                     selected = 1),
                                       ),
                                       mainPanel(
                                         h3("Popularidade de Gênero dos Filmes Por País"),
                                         h5("Com base no número de vezes que um filme/programa de TV de um determinado gênero esteve no Top 10 semanal da Netflix em um país (Dados de Junho 2021-Março 2022)."),
                                         plotlyOutput("countryPlot")
                                       )
                                     )
                                     ), 
                          ),
                 tabPanel("Assinantes Netflix",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("select3", 
                                          label = h3("Selecione a Escala:"), 
                                          choices = list("Faturamento Netflix Q4-2021" = "Q4.2021.Revenue....Estimate.", 
                                                         "Assinaturas Netflix Q4-2021" = "X..of.Subscribers.Q4.2021..Estimate."),
                                          selected = 1),
                              checkboxInput("outliermap", "Mostrar Outlier", FALSE),),
                            mainPanel(
                              plotlyOutput("mapPlot"),
                              h3("Faturamento x Assinaturas"),
                              plotlyOutput("mapscatPlot")),
                            
                          )
                 )
)

########## Server - Lógica do Servidor ##########

server <- function(input, output) {

  # Scatter Plot
  output$scatPlot <- renderPlotly({
    if (input$outlierscatter){
      dfs <- dataset1
    } else {
      dfs <- dataset1_scat_out
    }
    fig <- plot_ly(data = dfs, x = ~X2020.GDP..World.Bank., y = ~get(input$select), type = "scatter", mode = "markers", text = ~Country)
    fig <- fig %>% layout(yaxis = list(title = 'Variável Selecionada'), xaxis = list(title = 'PIB (USD)'))
    fig
    })

  # Bar Plot
  output$barPlot <- renderPlotly({
    if (input$outlierbar){
      dfb <- dataset1_bar
    } else {
      dfb <- dataset1_bar_out
    }
    fig <- plot_ly(dfb, x = ~gini_disp, y = ~Cost.Per.Month...Basic...., type = 'bar', name = 'Basic', text = ~Country)
    fig <- fig %>% add_trace(y = ~basic_standard_diff, name = 'Standard')
    fig <- fig %>% add_trace(y = ~standard_premium_diff, name = 'Premium')
    fig <- fig %>% layout(yaxis = list(title = 'Custo Mensal dos Planos Basic, Standard e Premium (USD)', titlefont = list(size=10)), xaxis = list(title = 'Desigualdade Salarial (GINI)'), barmode = 'stack')
    fig
  })
  
  # Country Plot
  output$countryPlot <- renderPlotly({
    country <- filter(countries, parents == input$Country)
    country <- rbind(filter(countries, labels == input$Country), country)
    fig <- plot_ly(country, ids = ~id, labels = ~labels, parents = ~parents, values = ~n, type = 'treemap', branchvalues = 'total', pathbar = list(visible = TRUE))
    
    fig
  })
  
  # Treemap Plot
  output$treePlot <- renderPlotly({
    fig <- plot_ly(tree, ids = ~id, labels = ~label, parents = ~parent, values = ~n, type = 'treemap', branchvalues = 'total', pathbar = list(visible = TRUE))
    
    fig
  })
  
  # Mapa
  output$mapPlot <- renderPlotly({
    if (input$outliermap){
      dfm <- dataset1
    } else {
      dfm <- dataset1_scat_out
    }
    
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # https://en.wikipedia.org/wiki/List_of_map_projections
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Miller')
    )
    
    fig <- plot_geo(dfm)
    fig <- fig %>% add_trace(z = ~get(input$select3), 
                             color = ~get(input$select3), 
                             colorscale = 'Purples', 
                             text = ~Country, 
                             locations = ~Alpha.3.code, 
                             marker = list(line = l))

    fig <- fig %>% colorbar(title = 'Escala')
    fig <- fig %>% layout(title = 'Mapa Global da Netflix em Q4-2021')
    fig
  })

  # Scatter Plot
  output$mapscatPlot <- renderPlotly({
    if (input$outliermap){
      dfms <- dataset1
    } else {
      dfms <- dataset1_scat_out
    }
    fig <- plot_ly(data = dfms, x = ~X..of.Subscribers.Q4.2021..Estimate., y = ~Q4.2021.Revenue....Estimate., type = "scatter", mode = "markers", text = ~Country)
    fig <- fig %>% layout(yaxis = list(title = 'Faturamento da Netflix em Q4-2021'), xaxis = list(title = 'Assinantes da Netflix em Q4-2021'))
    fig
  })
}

# Executa a app
shinyApp(ui, server)



                 