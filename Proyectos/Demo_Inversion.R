library(shinydashboard)
library(plotly)

library(tidyquant)
library(TTR)

library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)
library(scales)


TIKER <- c(
  "ADA-USD",
  "BTC-USD",
  "ETH-USD",
  "EGLD-USD",
  "SOL-USD", 
  "DOT-USD", 
  "BNB-USD", 
  "VLX-USD", 
  "KDA-USD", 
  "MATIC-USD",
  "KSM-USD", 
  "ROSE-USD",
  "LPT-USD", 
  "SAND-USD",
  "MANA-USD"
  
  
 
)

TIKER <- sort(TIKER)



# Create an empty header
header <- dashboardHeader(title = span(tagList(icon("bitcoin"), "Hold")),
                          
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Leonardo C",
                              message = "¡Bienvenido! Click  para más Info",
                              href = "https://www.linkedin.com/in/leo-cerv/"
                            ))
                          
                          
                          )
                          

# Create an empty sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
              menuItem("Forecast", tabName =  "for"),
              menuItem("Indicadores", tabName =  "in")),
                                          
  
                              selectInput(inputId = "Tiker", 
                                label = "Ticker",
                                choices = TIKER))




# Create an empty body
body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "for",
            
            fluidRow(      
              box(
                title = "Forecast a 30 dias",
                width = 12,
                plotlyOutput("fore"))
            ),
            fluidRow(valueBoxOutput("val1" , width = 4))
            
            ),
    
    tabItem(tabName = "in",
            
            fluidRow(      
              tabBox(width = 12,
                     
                     tabPanel(title = "RSI", 
                              sliderInput("obs", "Selecciona periodo:",
                                                  min = 1, max = 90, value = 14
                              ),
                              plotlyOutput("rsi")),
                     
                     tabPanel(title = "MACD",
                              sliderInput("fast", "Fast:",
                                          min = 1, max = 90, value = 12
                              ),
                              sliderInput("slow", "Slow:",
                                          min = 1, max = 90, value = 26
                              ),
                              sliderInput("signal", "Signal:",
                                          min = 1, max = 90, value = 9
                              ),
                              plotlyOutput("mcd")),
                     
                     tabPanel(title = "Volumen",
                              plotlyOutput("vol")),
                     
                     
                     
                     tabPanel(title = "SMA",
                              
                              sliderInput("sma1", "SMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("sma2", "SMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              plotOutput("sma")),
                     
                     
                     
                     tabPanel(title = "EMA",
                              
                              sliderInput("ema1", "EMA 1:",
                                          min = 1, max = 90, value = 9
                              ),
                              sliderInput("ema2", "EMA 2:",
                                          min = 1, max = 200, value = 50
                              ),
                              
                              plotOutput("ema")),
                     
                     
                     tabPanel(title = "Candels",
                              plotlyOutput("candel"))
                     
                     
                     )
            )
            
            
    )
))






# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "yellow",
                    title = "Hold")









server <- function(input, output) {
  
top_react <- reactive({
  
  BTC <- getSymbols(input$Tiker, auto.assign = F)
  
  BTC %>% tail(360)
  
})






    

  
 


output$fore <-  renderPlotly({

  
 
  
  
  BTC_TS <- as.data.frame(top_react())
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date") %>% 
    drop_na()
  
  BTC_TS <- as_tibble(BTC_TS)
  
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  
  colnames(BTC_TS)[2] <- "Close"
  
  splits <- time_series_split(
    BTC_TS,
    assess     = "30 days",
    cumulative = TRUE
  )
  
  
  
  # FORECAST ----
  
  # * AUTO ARIMA ----
  model_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(Close ~ Date, training(splits))
  
  model_arima
  
  # * Prophet ----
  model_prophet <- prophet_reg(
    seasonality_yearly = TRUE
  ) %>%
    set_engine("prophet") %>%
    fit(Close ~ Date, training(splits))
  
  model_prophet
  
  # * Machine Learning - GLM ----
  model_glmnet <- linear_reg(penalty = 0.01) %>%
    set_engine("glmnet") %>%
    fit(
      Close ~ wday(Date, label = TRUE)
      + month(Date, label = TRUE)
      + as.numeric(Date),
      training(splits)
    )
  
  model_glmnet
  
  
  # MODELTIME COMPARE ----
  
  # * Modeltime Table ----
  model_tbl <- modeltime_table(
    model_arima,
    model_prophet,
    model_glmnet
  )
  
  # * Calibrate ----
  calib_tbl <- model_tbl %>%
    modeltime_calibrate(testing(splits))
  
  
  
  # * Forecast Future ----
  future_forecast_tbl <- calib_tbl %>%
    modeltime_refit(BTC_TS) %>%
    modeltime_forecast(
      h           = "30 days",
      actual_data = BTC_TS
    )
  
  future_forecast_tbl %>%
    plot_modeltime_forecast(.title = "")
  
  
})
  


output$val1 <- renderValueBox({
  
  val_1 <- top_react() %>% tail(1)
  val_1 <- val_1[1, 2]
  
  
  valueBox(round(val_1, 3),
           subtitle =  "Cotización actual",
           color = "yellow",
           icon = icon("dollar")
  )
})




output$rsi <- renderPlotly({
  
  
  
  
  BTC_TS <- as.data.frame(top_react())
  
  BTC_TS <- BTC_TS %>% 
    rownames_to_column(var = "Date") %>% 
    drop_na()
  
  BTC_TS <- as_tibble(BTC_TS)
  
  BTC_TS$Date <- as.Date(BTC_TS$Date)
  
  colnames(BTC_TS)[2] <- "Close"
  
  precio <- BTC_TS$Close
rsi <- RSI(precio, n = input$obs)




rsi_plot <- cbind(BTC_TS$Date, rsi)
rsi_plot <- as.data.frame(rsi_plot)
rsi_plot$V1 <- as.Date(rsi_plot$V1)
rsi_plot <- as_tibble(rsi_plot)
rsi_plot <- rsi_plot %>% drop_na()

pty_rsi <- rsi_plot %>% 
  ggplot()+
  aes(V1, rsi) +
  geom_line(color = "#f2a900") +
  geom_hline(yintercept = 20, color = "red")+
  geom_hline(yintercept = 80, color = "red")+
  geom_hline(yintercept = 50, color = "black", lty = 2)+
  theme_light()

ggplotly(pty_rsi)

})
 






output$mcd <- renderPlotly({
  
  BTC_M <- top_react() 
  
  
  BTC_M <- as.data.frame(BTC_M)
  
  BTC_M <- BTC_M %>% 
    rownames_to_column(var = "Date") %>% 
    drop_na()
  
  BTC_M <- as_tibble(BTC_M)
  
  BTC_M$Date <- as.Date(BTC_M$Date)
  
  colnames(BTC_M)[2] <- "Close"
  
  precio <- BTC_M$Close
  
  macdi  <- MACD(precio , nFast = input$fast, nSlow = input$slow, nSig = input$signal, maType="EMA" )
  
  macdi <- as_tibble(macdi)
  macdi <- macdi %>% drop_na()
  hasta <- dim(macdi)[1]
  
  macds <- macdi %>% mutate(id = seq(1, hasta) )
  
  
  pty_macd <- macds %>% 
    ggplot()+
    aes(id, macd)+
    geom_line(color = "#f2a900")+
    geom_line(aes(y = signal), color = "red")+
    geom_hline(yintercept = 0, color = "black", lty = 2)+
    theme_light()
  
  ggplotly(pty_macd)
  
  
})






output$vol <- renderPlotly({
  
  BTC_vol <- top_react() 
  
  BTC_vol <- as.data.frame(BTC_vol)
  

  
  BTC_vol <- BTC_vol %>% 
    rownames_to_column(var = "Date")
  
  BTC_vol <- BTC_vol[,c("Date", paste0(input$Tiker,".Volume"))]
  
  BTC_vol <- as_tibble(BTC_vol)
  BTC_vol$Date <- as.Date(BTC_vol$Date)
  
  colnames(BTC_vol)[2] <- "Volumen"
  
  
  pty_vol <- BTC_vol %>% 
    ggplot()+
    aes(Date, Volumen)+
    geom_bar(stat = "identity", fill = "#f2a900", color = "#f2a900")+
    geom_hline(yintercept = mean(BTC_vol$Volumen, na.rm = T), lty = 2)+
    theme_light()+
    ylab(NULL)+
    xlab(NULL)
  
  ggplotly(pty_vol)
  
  
  
})



output$sma <- renderPlot({
  
  BTC_m <- as.data.frame(top_react())
  
  BTC_m <- BTC_m %>% 
    rownames_to_column(var = "Date")
  
  BTC_m <- as_tibble(BTC_m)
  BTC_m$Date <- as.Date(BTC_m$Date)
  
  
  colnames(BTC_m) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
  
  BTC_m %>%
    ggplot(aes(x = Date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen", colour_down = "darkred", 
                     fill_up  = "darkgreen", fill_down  = "darkred")+
    
    geom_ma(ma_fun = SMA, n = input$sma1, linetype = 5, size = 1.25) +
    geom_ma(ma_fun = SMA, n = input$sma2, color = "red", size = 1.25) + 
    theme_tq()
  
})




output$ema <- renderPlot({
  
  BTC_m <- as.data.frame(top_react())
  
  BTC_m <- BTC_m %>% 
    rownames_to_column(var = "Date")
  
  BTC_m <- as_tibble(BTC_m)
  BTC_m$Date <- as.Date(BTC_m$Date)
  
  
  colnames(BTC_m) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
  
  BTC_m %>%
    ggplot(aes(x = Date, y = close)) +
    geom_candlestick(aes(open = open, high = high, low = low, close = close),
                     colour_up = "darkgreen", colour_down = "darkred", 
                     fill_up  = "darkgreen", fill_down  = "darkred")+
    
    geom_ma(ma_fun = EMA, n = input$ema1, linetype = 5, size = 1.25) +
    geom_ma(ma_fun = EMA, n = input$ema2, color = "red", size = 1.25) + 
    theme_tq()
  
  
  
})



output$candel <- renderPlotly({
  
  BTC_c <- as.data.frame(top_react())
  
  BTC_c <- BTC_c %>% 
    rownames_to_column(var = "Date")
  
  BTC_c <- as_tibble(BTC_c)
  BTC_c$Date <- as.Date(BTC_c$Date)
  
  
  colnames(BTC_c) <- c("Date", "open", "high", "low", "close", "volumen", "adjusted")
  
  
  
  i <- list(line = list(color = '#f2a900'))
  d <- list(line = list(color = '#4d4d4e'))
  
  BTC_c %>% tail(60) %>% plot_ly(x = ~Date, type="candlestick",
                                 open = ~open, close = ~close,
                                 high = ~high, low = ~low,
                                 increasing = i, decreasing = d)
  
})


 


}

shinyApp(ui, server)







