# Shinydashboard
# el UI del shinydashboard se compone de tres cosas. el header, el sidebar, y el body



library(shinydashboard)
library(tidyverse)
library(plotly)
library(fmsb)



# Extract & transform data ------------------------------------------------

mi_nota <- "https://raw.githubusercontent.com/leo-cerv/Ejemplo/main/Notas2.csv"

notas <- read_csv2(mi_nota)

notas <- notas[,1:8]

for(i in 1:34){
  for(c in 2:8){
    ifelse(notas[i,c] > 10, 
           notas[i,c] <- notas[i,c] /10 ,
           notas[i,c] <- notas[i,c])
    
  }}

notas2 <- notas %>% 
  pivot_longer(cols = 2:8,
               names_to = "Producto")


line_mult <- notas2 %>% 
  group_by(Vendedor, Producto) %>% 
  summarise(promedio = mean(value)) 


# hclust

notasd <- notas %>% column_to_rownames(var = "Vendedor")


modelo <-  hclust(dist(notasd),
                  method = "ward.D2"
)




# Principal ---------------------------------------------------------------

#box
pty_bxp <- line_mult %>% 
  ggplot()+
  aes(Producto, promedio)+
  geom_boxplot(aes(fill = Producto, alpha = 0.3))+
  geom_jitter(aes(color = Vendedor), alpha = 0.7)+
  theme_light()+
  theme(legend.position="none")+
  geom_hline(yintercept = 4, color = "red", lty = 2)+
  xlab(NULL)+
  ylab(NULL)





#heat
pty_heat <- notas2 %>% 
  ggplot()+
  aes(Producto, Vendedor, fill = value)+
  geom_tile()+
  scale_fill_gradient(low = "red", high = "lightblue")+
  theme_minimal()+
  theme(legend.position="none")+
  theme(axis.text.y = element_text(size = rel(0.8)))+
  ylab(NULL)+
  xlab(NULL)





#top10

top <- notas2 %>% 
  group_by(Vendedor) %>% 
  summarise(promedio = mean(value)) %>% 
  arrange(desc(promedio))


pty_t10 <- top %>% 
  head(10) %>% 
  ggplot()+
  aes(fct_reorder(Vendedor, promedio), round(promedio,2))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y = element_text(size = rel(0.8)))+
  xlab(NULL)+
  ylab(NULL)




#worst10

pty_w10 <- top %>% 
  tail(10) %>% 
  ggplot()+
  aes(fct_reorder(Vendedor, promedio), round(promedio,2))+
  geom_bar(stat = "identity", fill = "red")+
  coord_flip()+
  theme_minimal()+
  theme(axis.text.y = element_text(size = rel(0.8)))+
  xlab(NULL)+
  ylab(NULL)







# Comparador alumnas  --------------------------------------------------------------



# valuebox promedio curso

prom_curso <- notas2 %>% 
  group_by(Vendedor) %>% 
  summarise(promedio = mean(value)) 




# histograma

best_asig_ind2 <- notas2 %>% 
  group_by(Producto) %>% 
  summarise(Promedio = mean(value))




# radar

# agregando a la alumna




# agregando el promedio

prom <- sapply(notas, mean)
prom <- prom[2:8]






















# hclust mas comparacion por cluster --------------------------------------


















# Create an empty header
header <- dashboardHeader(title = span(tagList(icon("signal"), "Sales Scan")))

# Create an empty sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
   
    menuItem(text = "Panel Principal", 
             tabName = "prin"
    ),
    
    menuItem(text = "Rendimiento",
             tabName = "des"
            
    ),
    
    menuItem(text = "Clustering",
             tabName = "cla")
    
    

    
  ))






# Create an empty body
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
  
  tags$style(".fa-trophy {color:#363535}"),
  
  
  tabItems(
    tabItem(tabName = "prin",
            fluidRow(
              
              box(
                title = "Distribución de las ventas por Producto",
                width = 12,
                status = "primary",
                ggplotly(pty_bxp)
              )),
            
            fluidRow(
              
              box(
                width = 12,
                status = "primary",
                ggplotly(pty_heat)
                
              )),
            
            fluidRow(
              
              box(
                title = "Mejores 10",
                width = 6,
                status = "primary",
                ggplotly(pty_t10)
              ),
              
              box(
                title = "Peores 10",
                width = 6,
                status = "primary",
                ggplotly(pty_w10)
              ))
            ),
    
  
    
    tabItem(tabName = "des",
            fluidRow(
              
              box(
                width = 4,
                status = "primary",
                
                selectInput(inputId = "pla1", 
                            label = "Vendedor:",
                            choices = unique(notas$Vendedor)
                )),
             
                valueBoxOutput("val1" , width = 4),
              
              
              valueBox(paste0("$", formatC(mean(prom_curso$promedio), format = "f", big.mark = ".", digits = 0)),
                       subtitle =  "Promedio General",
                       color = "black",
                       icon = icon("trophy"))
              
              ),
            
            fluidRow(
              
              box(
                width = 6,
                status = "primary",
                plotlyOutput("histog")
              ),
              
              tabBox(width = 6,
                     
                     tabPanel("Radar", 
                              plotOutput("radarr")),
                     
                     tabPanel("Promedio",
                              plotlyOutput("pty_best")))
              )),
    
    
    
    
    
    tabItem(tabName = "cla",
            
            fluidRow(
              
              box(
                title = "Parámetros",
                width = 6,
                status = "primary",
                selectInput(inputId = "numclu", 
                            label = "Número de clusters:",
                            choices = c(2, 3, 4, 5))
                
              )),
            
            fluidRow(
              
              box(
                width = 12,
                status = "primary",
                plotOutput("dendo")
                
              )),
            
            fluidRow(
              
              box(
                width = 12,
                status = "primary",
                plotlyOutput("facet")
                
              ))
            
            
            
            
           )
    
    
    
  )
)






# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
                    )






server <- function(input, output) {
  

top_react <- reactive({
  top %>% 
    filter(Vendedor == input$pla1)
  
})



output$val1 <- renderValueBox({
  
  val_1 <- top_react() %>% .$promedio
  
  
  valueBox(paste0("$", formatC(val_1, format = "f", big.mark = ".", digits = 0)),
           subtitle =  "Promedio",
           color = "blue",
           icon = icon("dollar-sign")
           )
})






best_asig_ind <- reactive({
  
  notas2 %>% 
  filter(Vendedor == input$pla1) %>% 
  group_by(Producto) %>% 
  summarise(Promedio = mean(value))
  
})




output$histog <- renderPlotly({
  
  pty_hist <- best_asig_ind() %>% 
    ggplot()+
    aes(Promedio)+
    geom_density(color = "blue", fill = "blue", alpha = 0.4)+
    geom_density(data = best_asig_ind2, color = "black", fill = "black", alpha = 0.2)+
    theme_light()+
    geom_vline(xintercept = 4, lty = 2, color = "red")+
    geom_vline(xintercept = mean(best_asig_ind2$Promedio), color = "black")+
    geom_vline(xintercept = mean(best_asig_ind() %>% .$Promedio), color = "blue")+
    ylab(NULL)+
    xlab(NULL)

  ggplotly(pty_hist)
  
})




rad <- reactive({
  
  notas %>% 
  filter(Vendedor == input$pla1) %>% 
  column_to_rownames(var = "Vendedor")

})



rad3 <- reactive({ 
  
  rbind(rep(1000000,1000000) , rep(1,1000000), rad() , prom)
  
})



output$radarr <- renderPlot({
  
  rad3() %>%   
    radarchart(axistype=1 , 
               pcol= c("blue", "black"),
             #custom polygon
             plwd=2 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels= seq(1,7, 7), cglwd=0.8,
             #custom labels
             vlcex=0.8)
})




best_asig <- reactive({
  
  notas2 %>% 
  filter(Vendedor == input$pla1) %>% 
  group_by(Producto) %>% 
  summarise(Promedio = mean(value))

})



output$pty_best <- renderPlotly({
  
  pty_best1 <- best_asig() %>% 
  ggplot()+
  aes(fct_reorder(Producto, Promedio), Promedio) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black")+
  geom_hline(yintercept = 4, lty = 2, color = "red")+
  coord_flip()+
  theme_light()+
  xlab(NULL)+
  ylab(NULL)

ggplotly(pty_best1)

})



output$dendo <- renderPlot({
  

 
  
  plot(modelo, xlab = "", sub = "", ylab = "", main = "", cex = 0.8)
  
  
  rect.hclust(modelo, 
              k= as.numeric(input$numclu),
              border = "magenta") 
  
})


output$facet <- renderPlotly({
  
  
  
  clusters <- cutree(modelo,
                     k=input$numclu
  )
  
  Estudiantes_con_clusters <- cbind(notas, clusters)
  
  Estudiantes_con_clusters_p <- Estudiantes_con_clusters %>% 
    pivot_longer(cols = 2:8,
                 names_to = "Producto",
                 values_to = "nota")
  
  pty_facet <- Estudiantes_con_clusters_p %>%
    group_by(Vendedor) %>% 
    ggplot()+
    aes(nota) +
    geom_density()+
    facet_grid(clusters~Producto)+
    theme_light()+
    theme(
      strip.background = element_rect(fill = "darkBlue", color = "darkBlue", size = 1),
      strip.text = element_text(colour = "white"))
  
  ggplotly(pty_facet)
  
  
})
  
}

shinyApp(ui, server)
