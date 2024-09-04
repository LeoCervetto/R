# setwd("C:/Users/leona/Desktop/Shiny/1_Ing_Menu")

library(shinydashboard)
library(shiny)


library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(plotly)
library(treemap)
library(DT)

library(modeltime)
library(tidymodels)
library(timetk)



myfile <- "https://raw.githubusercontent.com/leo-cerv/Ejemplo/main/VENTAS_TRI.csv"

Ventas_trii <- read_csv2(myfile)


Ventas_trii$Date <- dmy(Ventas_trii$Date)


modi <- Ventas_trii %>% 
  mutate(dia = wday(Date, label = T, abbr = F),
         mes = month(Date, label = T, abbr = F)) %>% 
  relocate(dia, .after = Date) %>% 
  relocate(mes, .after = dia)


modi$dia <- modi$dia %>% 
  factor(levels = c("martes", "miercoles", "jueves", "viernes", "sabado", "domingo" ))


modi <- modi %>% 
  mutate(total = Quantity * Precio) 



modi_ts <- modi %>% 
  group_by(Date) %>% 
  summarise(total = sum(total))




# TRAIN / TEST SPLITS ----

splits <- time_series_split(
  modi_ts,
  assess     = "30 days",
  cumulative = TRUE
)


# FORECAST ----

# * AUTO ARIMA ----
model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(total ~ Date, training(splits))


# * Machine Learning - GLM ----
model_glmnet <- linear_reg(penalty = 0.01) %>%
  set_engine("glmnet") %>%
  fit(
    total ~ wday(Date, label = TRUE)
    + month(Date, label = TRUE)
    + as.numeric(Date),
    training(splits)
  )



# * Modeltime Table ----
model_tbl <- modeltime_table(
  model_arima,
  #model_prophet,
  model_glmnet
)

# * Calibrate ----
calib_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))



future_forecast_tbl <- calib_tbl %>%
  modeltime_refit(modi_ts) %>%
  modeltime_forecast(
    h           = "30 days",
    actual_data = modi_ts
  )







ing_menu <- modi %>% 
  select(mes, Description, Quantity, Precio, Categoria) %>% 
  group_by(Description, mes) %>% 
  summarise(total = sum(Quantity),
            price = unique(Precio),
            categoria = unique(Categoria),
            mes = unique(mes))






myfile2 <- "https://raw.githubusercontent.com/leo-cerv/Ejemplo/main/costo.csv"

costo <- read_csv2(myfile2)





ing_menu <- ing_menu %>% 
  left_join(costo,
            by = "Description")

ing_menu <- ing_menu %>% 
  mutate(margen = price - costo)






names_pop <- c("BAJO", "ALTO")





#### principio 1 #####

Precio_max <- max(ing_menu$price)
Precio_min <- min(ing_menu$price)
Diferencia <- Precio_max - Precio_min 

fact <- Diferencia / 3


limites <- c(0, Precio_min + fact, Precio_min + fact*2, Precio_min + fact*3)
class <-  c("Bajo", "Medio", "Alto")


ing_menu_p1 <- ing_menu %>% 
  group_by(Description, mes) %>% 
  mutate(Principio_1 = cut(price,
                           breaks= limites,
                           labels= class))










ing_canasta <- modi %>% 
  select(Id_trans, Codigo, Description, Quantity, Date, Categoria)





library(arules)

# Splitting transactions
data_list <-  split(ing_canasta$Description,
                    ing_canasta$Id_trans)

# Transform data into a transactional dataset
Online_trx <-  as(data_list, "transactions")



# Apply the apriori function to the Online retail dataset
rules_online = apriori(Online_trx,
                       parameter = list(supp = 0.1, 
                                        conf = 0.8, 
                                        minlen = 2))


library(arulesViz)






modi %>% 
  filter(Description == "Saltimboca de Filete") %>% 
  group_by(Date) %>% 
  summarise(total = sum(Quantity)) 
  




##### extras #####


modi_ts2 <- modi %>% 
  filter(Description == "Saltimboca de Filete") %>% 
  group_by(Date) %>% 
  summarise(total = sum(Quantity)) 




# TRAIN / TEST SPLITS ----

splits2 <- time_series_split(
  modi_ts2,
  assess     = "30 days",
  cumulative = TRUE
)


# FORECAST ----

# * AUTO ARIMA ----
model_arima2 <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(total ~ Date, training(splits2))





# * Modeltime Table ----
model_tbl2 <- modeltime_table(
  model_arima2

)

# * Calibrate ----
calib_tbl2 <- model_tbl2 %>%
  modeltime_calibrate(testing(splits2))



future_forecast_tbl2 <- calib_tbl2 %>%
  modeltime_refit(modi_ts2) %>%
  modeltime_forecast(
    h           = "30 days",
    actual_data = modi_ts2
  )
























# Create an empty header
header <- dashboardHeader(title = span(tagList(icon("utensils"), "Restcom")),
                          
                          dropdownMenu(
                            type = "messages",
                            messageItem(
                              from = "Leonardo C",
                              message = "Bienvenido! Haz click aca para mas Info",
                              href = "http://restodata.byethost31.com/"
                            )),
                          
                          dropdownMenu(type="notifications",
                                       notificationItem(text="No olvides actualizar la BD")
                                       )
                          )




# Create an empty sidebar
sidebar <- dashboardSidebar(   
  sidebarMenu(id="sidebarID",
                menuItem("Ingenieria de menu", id = "chartsID", icon = icon("fish"),
                          menuSubItem("Forecast Ventas", tabName = "datos"),
                          menuSubItem("Matriz BCG", tabName = "plot"),
                          menuSubItem("Data", tabName = "tablas")
                         
                        ),
              
              
              menuItem("Omnes Smith",id = "omsm",icon = icon("rocket"),
                       menuSubItem("Principio 1", tabName = "p1"),
                       menuSubItem("Principio 2", tabName ="p2"),
                       menuSubItem("Principio 3", tabName ="p3")
              ),
              menuItem("Apriori", icon = icon("eye"), 
                       menuSubItem("Tabla", tabName = "t1"),
                       menuSubItem("Matriz", tabName = "m1"),
                       menuSubItem("Grafo", tabName = "g1")),
              
              menuItem("Extras", icon = icon("signal"),
                       menuSubItem("Graficas", tabName = "gr1"),
                       menuSubItem("Indicadores", tabName = "in1"),
                       menuSubItem("mapa", tabName = "mp1"),
                       selectInput(inputId = "pla1", 
                                   label = "Seleciona un plato:",
                                   choices = unique(modi$Description)
                       )),
              
                       
              selectInput(inputId = "mes1", 
                          label = "Seleciona el mes:",
                          choices = unique(modi$mes)
              ),
              
              selectInput(inputId = "cat1", 
                          label = "Seleciona la categoria:",
                          choices = unique(modi$Categoria)
              )
              
))








# Create an empty body
body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "datos",
            fluidRow(
              
              box(
                title = "Forecast a 30 dias",
                width = 12,
                plotlyOutput("forets")
              ),
              
              box(
                title = "Distribucion de las ventas percapita",
                id = "datos", height = "450px",
                tabPanel("Histograma", plotlyOutput("histo"))
                
              
              
            ),
            box(
              title = "Distribucion por categoria",
              width = 6,
              plotOutput("tree"))
            
            
        
            )),
    
    tabItem(tabName = "plot",
            fluidRow(
              
              box(
                title = "Matriz BCG",
                width = 12,
                plotlyOutput("bcg")
                
              )
            ),
            fluidRow(
              column(width = 8,  
                    
                     valueBoxOutput("valuebox"),
                     valueBoxOutput("infobo")
                 
                     
              )
              
              
              )
            
            ),
    
    tabItem(tabName = "tablas",
            dataTableOutput("datatb")),
    
    tabItem(tabName = "p1",
            
            box(
            title = "Distribucion del precio por clasificacion",
            width = 6,
            plotlyOutput("plotp1")),
            
            box(
            title = "Clasificacion de cada plato",
            width = 6,
            plotlyOutput("plotp1_2")),
            
            
            fluidRow(
              column(width = 12,
            valueBoxOutput("principio1"))
            
            )),
            
            
           
          
    
    tabItem(tabName = "p2",
            
            box(
              title = "Distribucion del precio",
              width = 12,
              plotlyOutput("plot_p2")),
            
            fluidRow(
              column(width = 12,
                     valueBoxOutput("value_p2"))
              
            )
            
           
            
            
          ),
              
    
            
            
            
    tabItem(tabName = "p3",
            "hola2"),
    
    
    
    
    
    
    
    tabItem(tabName = "t1",
            dataTableOutput("dtapriori")),
    
    
    
    
    tabItem(tabName = "m1",
            box(
              title = "Matriz",
              width = 12,
                       plot(rules_online, engine = "plotly"))
              
            ),
    
    
    
    tabItem(tabName = "g1",
            
            box(
              title = "Grafos",
              width = 12,
              
               plot(head(sort(rules_online, by="confidence"), 7),
                      method = "graph",
                      engine = "htmlwidget"))
              
              
              
            ),
    
 
    
       
    tabItem(tabName = "gr1",
            
            box(
              title = "platos mas vendidos",
              width = 6,
              plotlyOutput("top10")),
            
            box(
              title = "Venta individual",
              width = 6,
              plotlyOutput("ts_ind")
              ),
            
            fluidRow(
              
              box(
                title = "Forecast saltimboca",
                width = 12,
                plotlyOutput("forets_sal")
                
              )
            )
            
         
            
            ),
    
    tabItem(tabName = "in1",
            
            infoBox("Ventas Acumuladas", dollar(sum(modi$total))),
            infoBox("Rentabilidad Acumuladas", round( (sum(ing_menu$total) * sum(ing_menu$costo)) / (sum(ing_menu$total) * sum(ing_menu$price)),2)), 
            infoBox("Total platos vendidos", sum(modi$Quantity)),
            
            valueBox(dollar(mean(modi$total)),"Ingreso por Pax", 
                     icon=icon("money"),color= "green"),
            
            valueBox(dollar(mean(modi$Precio)), "Precio de venta promedio", 
                     icon=icon("bolt"),color= "red"),
            
            valueBox(round(mean(modi$pax), 2),"Promedio de Pax por mesa", 
                     icon=icon("user"),color= "blue"),
            
            
            
            
    )
    
    
    
    
    
    
    
    ))
    
    
    
    
    
    



# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "yellow")



server <- function(input, output) {
  

  
table2 <- reactive({
    modi %>% 
      
      group_by(Date, dia, mes, Turno, Mesa, pax) %>% 
      summarise(
        total_venta = sum(total)) %>% 
      mutate(percapita = total_venta / pax ) %>% 
      filter(mes == input$mes1) 
  })



table3 <- reactive({
  modi %>% 
    filter(mes == input$mes1) 
  
})

   



  
  output$histo <- renderPlotly({  
   
     table2() %>%   
    ggplot()+
      aes(percapita, y = ..density..)+
      geom_histogram(fill = "yellow", color = "orange", alpha = 0.3)+
      geom_density(color = "darkorange")+
      theme_light()+
      ylab(NULL)+
      xlab(NULL)->hi
    
    ggplotly(hi)
    
  })
  
  
  
  
  
  output$forets <- renderPlotly({
    
    future_forecast_tbl %>%
      plot_modeltime_forecast(.title = "") 
      
    })
  

  
output$tree <- renderPlot({
    table3() %>% treemap(
            index = c("Categoria", "Description"), 
            vSize = "total",       
            type = "index",
            title = ""
            )
  })
  
  
  
  

ing_menu2 <- reactive({
  
  ing_menu %>%
    filter(mes == input$mes1, categoria == input$cat1) %>% 
    mutate(
      corte_pop = (100/length(unique(.$Description))) * 0.7,
      indice_pop = ((total /sum(ing_menu$total)) * 1000),
      margen_total = margen * total,
      clas_pop = ifelse(indice_pop > corte_pop, "ALTO", "BAJO"),
      
      venta_total = total * price,
      indice_renta = margen / price,
      
      corte_rent = sum(margen_total) / sum(total),
      clas_rent = ifelse(margen > corte_rent, "ALTO", "BAJO")) %>% 
    
    mutate(corte_rent = sum(.$margen_total)/ sum(.$total),
           clas_rent = ifelse(margen > corte_rent, "ALTO", "BAJO"),
           
           clasific_final = ifelse(clas_rent == "ALTO" & clas_pop == "BAJO", "VACA",
                                   ifelse(clas_rent == "ALTO" & clas_pop == "ALTO","ESTRELLA", 
                                          ifelse(clas_rent == "BAJO" & clas_pop == "ALTO", "ENIGMA",
                                                 "PERRO"))))
    
  
}) 
  



output$datatb <- renderDataTable({
  
  ing_menu2() %>%
    
    datatable(rownames = F, extensions = "Buttons",
              options = list(pageLength = 100,
                             dom = "Bfrtip",
                             buttons = c("copy", "csv")))
    
})
  
  



  
 

output$bcg <- renderPlotly({
    

  
  ing_menu2() %>% 
    ggplot()+
    aes(total, margen, color = clasific_final, label = Description)+
    geom_text()+
    theme_light()+
    ylab("Margen")+
    xlab("Unidades vendidas del mes")-> mbcg
    
  ggplotly(mbcg)

  
})


output$valuebox<-renderValueBox({
  
  
  valueBox(dollar(sum(ing_menu2() %>% .$venta_total)),"Ventas totales", 
                                         icon = icon("money"),color = "yellow")
  })



 output$infobo <- renderValueBox({
   
   valueBox(round(mean(ing_menu2() %>% .$indice_renta),2),"Margen de rentabilidad", 
            icon = icon("percent"),color = "yellow")
   
 
   
 })
 
 
 ing_menu_p1r <- reactive({
   
   ing_menu_p1 %>% 
     filter(mes == input$mes1, categoria == input$cat1 ) 

 })
 
 
 
 output$plotp1 <- renderPlotly({
   
   violp  <-  ing_menu_p1r() %>% 
     ggplot()+
     aes(Principio_1, price, fill = Principio_1)+
     geom_violin(trim=FALSE)+
     xlab(NULL)+
     scale_fill_brewer(palette="Accent")+
     theme_light()+
     theme(legend.position="none")+
     stat_summary(fun=mean, geom="point", size=2, color="red")+
     ylab("Precio de Venta")
   
   ggplotly(violp)
   
   
     
 })
 
 output$plotp1_2 <- renderPlotly({
   
   barp <- ing_menu_p1r() %>% 
     ggplot()+
     aes(Principio_1)+
     geom_bar(aes(fill = Description))+
     theme_light()+
     theme(legend.position="none")+
     xlab(NULL)+
     ylab(NULL)+
     ylab("Unidades")
   
   ggplotly(barp)
   
 })
  
 
 
 output$principio1 <- renderValueBox({
   

   
   prin1 <- ifelse(sum(ing_menu_p1r() %>% 
                         group_by(Description, Principio_1) %>% 
                         summarise(Description = unique(Description)) %>% .$Principio_1 == "Medio")>=
                     sum(ing_menu_p1r() %>% 
                           group_by(Description, Principio_1) %>% 
                           summarise(Description = unique(Description)) %>% .$Principio_1 == "Bajo")+
                     sum(ing_menu_p1r() %>% 
                           group_by(Description, Principio_1) %>% 
                           summarise(Description = unique(Description)) %>% .$Principio_1 == "Alto"),
                   "Se cumple",
                   "NO se cumple")
   
   colorr <- ifelse(prin1 == "Se cumple",
                    "green",
                    "red")
   
   
   
   inconn <- ifelse(prin1 == "Se cumple",
                    "check",
                    "remove")
   
   valueBox(prin1,
            "Principio 1",
            color = colorr,
            icon = icon(inconn))
 })
 
 
 
 
 
 
 ingmenu_p2 <- reactive({
   ing_menu %>% 
   filter(categoria == input$cat1, mes == input$mes1) %>% 
   group_by(Description, price, categoria) %>% 
   summarise(total = sum(total))
 })
 
 
 output$plot_p2 <- renderPlotly({
   
   densp2 <- ingmenu_p2() %>% 
     ggplot()+
     aes(price, ..density..)+
     geom_vline(xintercept = mean(ingmenu_p2() %>% .$price), color = "blue", lty = 3, lwd = 0.5)+
     geom_vline(xintercept = min(ingmenu_p2() %>% .$price), color = "black", lty = 2, lwd = 0.5)+
     geom_vline(xintercept = max(ingmenu_p2() %>% .$price), color = "black", lty = 2, lwd = 0.5)+
     geom_density(fill = "yellow", alpha = 0.3, color = "orange")+
     theme_light()
   
   ggplotly(densp2)
   
 })
 
 
 

 
 output$value_p2 <- renderValueBox({
   
   
   
   factor2 <- ifelse(ingmenu_p2() %>% nrow() <= 9, 2.5, 3)
   Precio_max <- max(ingmenu_p2() %>% .$price)
   Precio_min <- min(ingmenu_p2() %>% .$price)
   
   resul_factor <- Precio_min*factor2
   
   
   Principio2 <- ifelse(resul_factor <= Precio_max , 
                        "Se cumple", 
                        "No se cumple")
   
   
   
   colorr <- ifelse(Principio2 == "Se cumple",
                    "green",
                    "red")
   
   
   
   inconn <- ifelse(Principio2 == "Se cumple",
                    "check",
                    "remove")
   
   
   
   valueBox(Principio2,
            "Principio 2",
            color = colorr,
            icon = icon(inconn))
   
 })
 
 
 
 
 
 
 
 output$dtapriori <- renderDataTable({
   inspectDT(rules_online)
 })
 
 
 
 
 
 
 
 ing_canasta <- reactive({
   
   modi %>% 
     filter(Categoria == input$cat1, mes == input$mes1 ) %>% 
     select(Id_trans, Codigo, Description, Quantity, Date, mes, Categoria)
   
   
 })
 
 
 
 output$top10 <- renderPlotly({
   
  top_10 <-  ing_canasta() %>% 
    group_by(Description) %>% 
    summarise(Quantity = sum(Quantity)) %>% 
    ggplot()+
    aes(reorder(Description, Quantity,
                function(x) sum(x)), 
        Quantity)+
    geom_bar(stat = "identity", fill = "yellow", alpha = 0.3, color = "orange")+
    coord_flip()+
    theme_light()+
    xlab(NULL)
   
  ggplotly(top_10)
   
 })
 
 
 
 
 ts_individual <- reactive({ 
   
   modi %>% 
   group_by(Date, Description) %>% 
   summarise(total = sum(Quantity)) %>% 
   filter(Description == input$pla1)
 })
 
 
 
 output$ts_ind <- renderPlotly({ 
   
 ts_in <- ts_individual() %>% 
   ggplot()+
   aes(Date, total)+
   geom_line(color = "orange")+
   geom_smooth(se = F)+
   theme_minimal()
 
 ggplotly(ts_in)
 
 })
 
 
 
 
 
 output$forets_sal <- renderPlotly({
   
   future_forecast_tbl2 %>%
     plot_modeltime_forecast(.title = "") 
   
 })
 
 
  
}

shinyApp(ui, server)




  
