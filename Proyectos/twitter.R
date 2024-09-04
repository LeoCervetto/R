library(tidyverse)
library(rtweet)
library(tidytext)
library(syuzhet)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(DT)

library(shinydashboard)
library(shiny)

client_as("sentiment_resto")



myfile <- "https://raw.githubusercontent.com/leo-cerv/twit/main/stop_esp.txt"
stop_word <- read_table(myfile)
stop_word <- as_tibble(stop_word)
colnames(stop_word) <- "word"



custom_stop_words <- tribble(
  
  ~word, 
  
  "http", 
  "más",  
  "t.co",
  "julio",
  "coso",
  "está",
  "qué",
  "https",
  "rt",
  "httos"
) 


stop_words2 <- stop_word %>% 
  bind_rows(custom_stop_words)






# header
header <- dashboardHeader(title = span(tagList(icon("twitter"), "Twitter App")))

#sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem(text = "Trend",
             menuSubItem("Data", tabName = "t1"),
             menuSubItem("Info", tabName ="t2", icon = icon("bar-chart")),
             menuSubItem("Comparar", tabName ="t3", icon = icon("check-double"))
             
    ),
    
    menuItem(text = "User",
             menuSubItem("Data", tabName = "u1"),
             menuSubItem("Info", tabName ="u2", icon = icon("bar-chart"))
             
    ),
    
    menuItem(text = "Country",
             menuSubItem("Data", tabName = "c1"),
             menuSubItem("Info", tabName ="c2", icon = icon("bar-chart")))
    
    
    
    
  )
)

#body
body <- dashboardBody(
  
  tags$head(
    tags$style( 
      HTML('
      h3 {
        font-family: Courier New;
      }
      ')
    )
  ),
  
  

  tabItems(
    
      tabItem(tabName = "t1",
              
      fluidRow(        
      box(
        width = 4,
        title = "Selecciona los parametros",
        textInput("hash", "Tema"),
        
        selectInput(inputId = "idioma", 
                    label = "Idioma:",
                    choices = c("en", "es")),
                    
        selectInput(inputId = "cantidad", 
                    label = "Seleciona la cantidad de Tweets:",
                    choices = c(100, 200, 300, 400, 500)),
        
        selectInput(inputId = "retweet", 
                    label = "Incluir ReTweets",
                    choices = c(TRUE, FALSE)),
        
        actionButton("buscar", "Buscar")),
      
      box(
        width = 8,
        plotOutput("worclud_dt"))),
      
      dataTableOutput("trend_dt")
      
      
      
      ),
      
      tabItem(tabName = "t2",
               
           fluidRow(
                 box(width = 12,
                     title = "Ranking",
                     plotlyOutput("rank_palabras"))
               ),
               
            fluidRow(
                 box(width = 12,
                     title = "Analisis de sentimiento",
                     plotOutput("sentiment"))
               )
        
      ),
      
      tabItem(tabName = "t3",
              
              fluidRow(
                box(width = 6,
                    title = "Ingresa dos Hashtags (ej: #nike)",
                    textInput("hash1", "Hash 1"),
                    textInput("hash2", "Hash 2"),
                    
                    selectInput(inputId = "cantidad1", 
                                label = "Seleciona la cantidad de Tweets:",
                                choices = c(100, 200, 300, 400, 500)),
                    
                    selectInput(inputId = "retweet1", 
                                label = "Incluir ReTweets",
                                choices = c(TRUE, FALSE)),
                    
                    actionButton("buscar_comp", "Buscar"))
                    
                
              
                
                
              ),
              
              fluidRow(
                box(width = 12,
                    title = "Comparación",
                    plotlyOutput("comp_ply"))
              )
              
      ),
    
  
      
      
    tabItem(tabName = "u1",
    box(
      width = 12,
      
      textInput("usuario", "Usuario (ej: @Cristiano)"),
      
      selectInput(inputId = "cantidad_aut", 
                  label = "Seleciona la cantidad de Tweets:",
                  choices = c(100, 200, 300, 400, 500)),
      
      actionButton("buscar_aut", "Buscar")),
      
      dataTableOutput("aut_dt")
      
      ),
    
    tabItem(tabName = "u2",
            
            fluidRow(
            box(
              width = 12,
              title = "Reteets",
              plotlyOutput("retweet_ply"))
            ),
            fluidRow(
            box(
              width = 12,
              title = "Favorites",
              plotlyOutput("favourite_ply"))
            ),
            
            fluidRow(
              box(
                width = 12,
                title = "Correlación",
                plotlyOutput("cor_ply"))
            )
            
    ),
    
    tabItem(tabName = "c1",
            
            fluidRow(
              box(
                width = 12,
                title = "Seleciona pais",
                textInput("pais", "Pais"),
                actionButton("buscar_pais", "Buscar")
                )
            ),
            fluidRow(
              dataTableOutput("pais_dt")
            )
            
    ),
    
    tabItem(tabName = "c2",
            
            fluidRow(
              box(
                width = 12,
                title = "Trends",
                plotlyOutput("country_ply"))
            ))
    
    
    
    
    
))





















# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "purple")
















server <- function(input, output) {
  
  
datatw <- eventReactive(input$buscar, {
    
  tweets <-  search_tweets(q = as.character(input$hash), 
                           n = as.numeric(input$cantidad),
                           lang = as.character(input$idioma), 
                           include_rts = as.logical(input$retweet))
  
  tweets %>% 
    select(created_at, full_text, id, in_reply_to_user_id)
  })
  
  

output$trend_dt <- renderDataTable({
  
  datatw() %>% 
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 25,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))
  
})

  
  
output$tidy_tw <- reactive({
    

    
    datatw() %>% 
      unnest_tokens(word, full_text) %>% 
      anti_join(stop_words2) %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(25)
    

      
  })



  
  output$rank_palabras <- renderPlotly({
    
  tidiss <-  datatw() %>% 
    unnest_tokens(word, full_text) %>% 
    anti_join(stop_words2) %>% 
    count(word) %>% 
    arrange(desc(n)) %>% 
    head(25)  
  
  tidiss %>% 
      ggplot()+
      aes(fct_reorder(word, n), n) +
      geom_bar(stat = "identity", color = "purple", fill = "#6632a8", alpha = 0.65) +
      # Flip the plot coordinates
      coord_flip()+
      xlab(NULL)+
      ylab(NULL)+
      theme_classic()
  })
  
  
  
  
  
  output$worclud_dt <- renderPlot({
    

    
    tidys <- datatw() %>% 
      unnest_tokens(word, full_text) %>% 
      anti_join(stop_words2) %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(25)
    
    
    
    palabras <- tidys %>% 
      count(word) %>% 
      arrange(desc(n)) %>% 
      head(25)
    
    
    
    colores <- brewer.pal(5, "Dark2")  
    
    wordcloud(words = palabras$word, 
              freq = palabras$n, 
              scale = c(1.5, 0.5),    # tamaño para la letra mas grande y la mas chica
              random.order = F,      # le ponemos false para que el orden No sea random. asi me pone las mas grnades al centro y agrupadas
              max.words = 75,         # cantidad maxima de palabras
              rot.per = 0.25,         #porcentaje de palabras con orientacion vertical. el 25% tendrá orientacion verical
              colors = colores) 
  })
  
  
  
output$sentiment <- renderPlot({
  

  
  tidyy <- datatw() %>% 
    unnest_tokens(word, full_text) %>% 
    anti_join(stop_words2) %>% 
    count(word) %>% 
    arrange(desc(n)) %>% 
    head(25)
  
  

  
  sentimiento <- get_nrc_sentiment(tidyy$word, lang = "spanish")
  
barplot(
    colSums(prop.table(sentimiento[, 1:10])),
    space = 0.2,
    horiz = FALSE,
    las = 1,
    cex.names = 1,
    cex.axis = 1,
    ylim = c(0, 0.5),
    col = "#6632a8",
    ylab = NULL)


  
})  
  
  



melt_df <- eventReactive(input$buscar_comp, {
  
  hash1 <- search_tweets(as.character(input$hash1), n = as.numeric(input$cantidad1), include_rts = as.logical(input$retweet1))
  hash2 <- search_tweets(as.character(input$hash2), n = as.numeric(input$cantidad1), include_rts = as.logical(input$retweet1))
  
  
  hash1_ts <- ts_data(hash1, by ='hours')
  names(hash1_ts) <- c("time", "Hash1")
  
  
  
  hash2_ts <- ts_data(hash2, by ='hours')
  names(hash2_ts) <- c("time", "Hash2")
  
  merged_df <- merge(hash1_ts, hash2_ts, by = "time", all = TRUE)
  
  merged_df %>% pivot_longer(cols = 2:3,
                             names_to = "Variable",
                             values_to = "Valor")
  
})



output$comp_ply <- renderPlotly({
  
  compar_ply <- melt_df() %>% 
    ggplot()+
    aes(time, Valor, col = Variable)+
    geom_line(lwd = 0.8)+
    scale_colour_manual(values = c("#6632a8", "#f7cf07"))+
    theme_classic()
  
  ggplotly(compar_ply)
  
})












  
# pagina 2

data_aut <- eventReactive(input$buscar_aut, {

  get_aut <- get_timeline(user = as.character(input$usuario),
               n = as.numeric(input$cantidad_aut))
  
  get_aut %>% 
    select(created_at, full_text, retweet_count, favorite_count)
  
})
  

output$aut_dt <- renderDataTable({

  data_aut() %>% 
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 25,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))

  
})



output$retweet_ply <- renderPlotly({
  
  ret_ply <- data_aut() %>% 
    ggplot() +
    aes(created_at, retweet_count) +
    geom_line(color = "#6632a8")+
    geom_smooth(se = F, color = "#f7cf07")+
    ylab(NULL)+
    xlab(NULL)+
    theme_classic()
    
  ggplotly(ret_ply)
  
  
})


output$favourite_ply <- renderPlotly({
  
  fav_ply <- data_aut() %>% 
    ggplot() +
    aes(created_at, favorite_count) +
    geom_line(color = "#6632a8")+
    geom_smooth(se = F, color = "#f7cf07")+
    ylab(NULL)+
    xlab(NULL)+
    theme_classic()
  
  ggplotly(fav_ply)
  
  
})



output$cor_ply <- renderPlotly({
  
corelation_plot <- data_aut() %>% 
    ggplot()+
    aes(favorite_count, retweet_count)+
    geom_point(fill = "#6632a8", alpha = 0.7, color = "purple")+
    geom_smooth(method = "lm", color = "#f7cf07")+
    theme_classic()+
    ylab("Retweet")+
    xlab("Favorite")

ggplotly(corelation_plot)
  
})





data_pais <- eventReactive(input$buscar_pais, {
  
  get_trends(input$pais)
  
})


output$pais_dt <- renderDataTable({
  
  data_pais() %>% 
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 25,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))
  
  
})



output$country_ply <- renderPlotly({
  
  trend_df <- data_pais() %>%
    group_by(trend) %>%
    summarize(tweet_vol = mean(tweet_volume)) %>% 
    arrange(desc(tweet_vol))

  
  
  pais_trend <- trend_df %>% 
    head(20) %>% 
    ggplot()+
    aes(fct_reorder(trend,tweet_vol) , tweet_vol)+
    geom_bar(stat = "identity", color = "purple", fill = "#6632a8", alpha = 0.65)+
    coord_flip()+
    ylab(NULL)+
    xlab(NULL)+
    theme_classic()
  
  ggplotly(pais_trend)
})









  
}





shinyApp(ui, server)