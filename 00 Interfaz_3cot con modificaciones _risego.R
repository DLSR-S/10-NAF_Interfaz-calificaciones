#rm(list=ls())
library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(readxl)
library(tibble)
library(dplyr)
library(tidyr)
library(timetk)
library(xts)
library(purrr)
library(quantmod)
library(TTR)
library(highcharter)
library(shinyWidgets)
library(shinythemes)
#==============================================================================#
  # Pagina 01 UI: 01 Seleccion de emisoras sin ESG =====================####

setwd("C:/Users/DLSR_33/Desktop/00 NAF-DRV ESG")
df_Jun21 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Jun21",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()

"Emisoras" <- df_Jun21$Emisoras
"Operación" <- rowMeans(df_Jun21[2:5])
"Valuación" <- rowMeans(df_Jun21[6:7])
"Apalancamiento" <- rowMeans(df_Jun21[8:11])
"Ope_mercado" <- rowMeans(df_Jun21[12])
"ESG" <- rowMeans(df_Jun21[12])

calif_no_esg <- tibble(Operación,
                       Valuación,
                       Apalancamiento,
                       Ope_mercado)

  # Pagina 01 UI: 01 Seleccion de emisoras con ESG =====================####

setwd("C:/Users/DLSR_33/Desktop/00 NAF-DRV ESG")
df_Jun21 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Jun21",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()

df_esg <- read_xlsx("ESG.xlsx",
                    sheet = "Calificaciones",
                    range = "B1:U65",
                    col_names = T) %>% as.data.frame()

"Emisora" <- df_esg$Emisora
`2017` <- df_esg$"2017"
`2018` <- df_esg$"2018"
`2019` <- df_esg$"2019"
`2020` <- df_esg$"2020"

df_5_ESG <- tibble(`2017`,
                   `2018`,
                   `2019`,
                   `2020`)
row.names(df_5_ESG) <- Emisora

prom_mediana <- df_esg$`Promedio mediana`

cal_ESG <- function(Emisora,df_5_ESG,prom_mediana){
  dicotomico <- data.frame(matrix(ncol = 4, nrow = 64))#Crear marco de datos.
  for (f in 1:64){
    for (c in 1:4){
      dicotomico[f,c] <- ifelse(df_5_ESG[f,c]>prom_mediana,1,0)
    }}
  
  dicotomico_suma <- as.data.frame(rowSums(dicotomico))
  rownames(dicotomico_suma) <- Emisora
  
  columnas <- 0
  calificacion <- data.frame(matrix(data=0,nrow=64,ncol=columnas))
  promedio <- mean(dicotomico_suma[,1],na.rm=TRUE)
  maximo <- max(dicotomico_suma,na.rm=TRUE)
  minimo <- min(dicotomico_suma,na.rm=TRUE)
  calificacion <- ((dicotomico_suma-minimo)/(maximo-minimo)*(10-1)+1)
  rownames(calificacion) <- Emisora
  
  cal <- calificacion[,1]
  names(cal) <-Emisora
  return(cal)
}

ESG <- cal_ESG(Emisora, df_5_ESG, prom_mediana)

"Emisoras" <- df_Jun21$Emisoras
"Operación" <- rowMeans(df_Jun21[2:5])
"Valuación" <- rowMeans(df_Jun21[6:7])
"Apalancamiento" <- rowMeans(df_Jun21[8:11])
"Ope_mercado" <- rowMeans(df_Jun21[12])
"ESG" <- ESG



calif_esg <- tibble(Operación,
                    Valuación,
                    Apalancamiento,
                    Ope_mercado,
                    ESG)

  # Pagina 02 UI: 02 Calificaciones historicas =========================####
setwd("C:/Users/DLSR_33/Desktop/00 NAF-DRV ESG")
df_Jun21 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Jun21",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()
df_Mar21 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Mar21",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()
df_Dic20 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Dic20",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()
df_Sep20 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Sep20",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()
df_Jun20 <- 
  read_xlsx("DetalleTrimestres21.xlsx", sheet = "Jun20",
            range = "B1:M65", col_names = TRUE) %>% as.data.frame()

df_esg <- read_xlsx("ESG.xlsx", sheet = "Calificaciones",
                    range = "B1:U65", col_names = T) %>% as.data.frame()
#======================#
df_1_Ope <- tibble(df_Jun21$Emisoras,
                   rowMeans(df_Jun20[2:5]),
                   rowMeans(df_Sep20[2:5]),
                   rowMeans(df_Dic20[2:5]),
                   rowMeans(df_Mar21[2:5]),
                   rowMeans(df_Jun21[2:5])) %>%
  `colnames<-`(c("Emisoras","2020 Q2","2020 Q3","2020 Q4","2021 Q1","2021 Q2"))

df_1_Ope.mej <- 
  df_1_Ope %>% #Agrupa en df la primera col y la ultima.
  arrange(desc(df_1_Ope[,dim(df_1_Ope)[2]])) %>% #Ajsusta la ultima columnas en orden descendente.
  slice(1:12) #Toma n número de obseraciones.

df_1_Ope.grf <- data.frame(emisora=
                             df_1_Ope.mej$"Emisoras",
                           calificacion=c(
                             df_1_Ope.mej$"2020 Q2",
                             df_1_Ope.mej$"2020 Q3",
                             df_1_Ope.mej$"2020 Q4",
                             df_1_Ope.mej$"2021 Q1",
                             df_1_Ope.mej$"2021 Q2"),
                           trimestre=c(
                             rep("2020 Q2", nrow(df_1_Ope.mej)),
                             rep("2020 Q2", nrow(df_1_Ope.mej)),
                             rep("2020 Q4", nrow(df_1_Ope.mej)),
                             rep("2021 Q1", nrow(df_1_Ope.mej)),
                             rep("2021 Q2", nrow(df_1_Ope.mej))))

#======================#
df_2_Val <- tibble(df_Jun21$Emisoras,
                   rowMeans(df_Jun20[6:7]),
                   rowMeans(df_Sep20[6:7]),
                   rowMeans(df_Dic20[6:7]),
                   rowMeans(df_Mar21[6:7]),
                   rowMeans(df_Jun21[6:7])) %>%
  `colnames<-`(c("Emisoras","2020 Q2","2020 Q3","2020 Q4","2021 Q1","2021 Q2"))

df_2_Val.mej <- 
  df_2_Val %>% #Agrupa en df la primera col y la ultima.
  arrange(desc(df_2_Val[,dim(df_2_Val)[2]])) %>% #Ajsusta la ultima columnas en orden descendente.
  slice(1:12) #Toma n número de obseraciones.

df_2_Val.grf <- data.frame(emisora=
                             df_2_Val.mej$"Emisoras",
                           calificacion=c(
                             df_2_Val.mej$"2020 Q2",
                             df_2_Val.mej$"2020 Q3",
                             df_2_Val.mej$"2020 Q4",
                             df_2_Val.mej$"2021 Q1",
                             df_2_Val.mej$"2021 Q2"),
                           trimestre=c(
                             rep("2020 Q2", nrow(df_2_Val.mej)),
                             rep("2020 Q2", nrow(df_2_Val.mej)),
                             rep("2020 Q4", nrow(df_2_Val.mej)),
                             rep("2021 Q1", nrow(df_2_Val.mej)),
                             rep("2021 Q2", nrow(df_2_Val.mej))))

#======================#
df_3_Apa <- tibble(df_Jun21$Emisoras,
                   rowMeans(df_Jun20[8:11]),
                   rowMeans(df_Sep20[8:11]),
                   rowMeans(df_Dic20[8:11]),
                   rowMeans(df_Mar21[8:11]),
                   rowMeans(df_Jun21[8:11])) %>%
  `colnames<-`(c("Emisoras","2020 Q2","2020 Q3","2020 Q4","2021 Q1","2021 Q2"))

df_3_Apa.mej <- 
  df_3_Apa %>% #Agrupa en df la primera col y la ultima.
  arrange(desc(df_3_Apa[,dim(df_3_Apa)[2]])) %>% #Ajsusta la ultima columnas en orden descendente.
  slice(1:12) #Toma n número de obseraciones.

df_3_Apa.grf <- data.frame(emisora=
                             df_3_Apa.mej$"Emisoras",
                           calificacion=c(
                             df_3_Apa.mej$"2020 Q2",
                             df_3_Apa.mej$"2020 Q3",
                             df_3_Apa.mej$"2020 Q4",
                             df_3_Apa.mej$"2021 Q1",
                             df_3_Apa.mej$"2021 Q2"),
                           trimestre=c(
                             rep("2020 Q2", nrow(df_3_Apa.mej)),
                             rep("2020 Q2", nrow(df_3_Apa.mej)),
                             rep("2020 Q4", nrow(df_3_Apa.mej)),
                             rep("2021 Q1", nrow(df_3_Apa.mej)),
                             rep("2021 Q2", nrow(df_3_Apa.mej))))

#======================#
df_4_OMe <- tibble(df_Jun21$Emisoras,
                   rowMeans(df_Jun20[12]),
                   rowMeans(df_Sep20[12]),
                   rowMeans(df_Dic20[12]),
                   rowMeans(df_Mar21[12]),
                   rowMeans(df_Jun21[12])) %>%
  `colnames<-`(c("Emisoras","2020 Q2","2020 Q3","2020 Q4","2021 Q1","2021 Q2"))

df_4_OMe.mej <- 
  df_4_OMe %>% #Agrupa en df la primera col y la ultima.
  arrange(desc(df_4_OMe[,dim(df_4_OMe)[2]])) %>% #Ajsusta la ultima columnas en orden descendente.
  slice(1:12) #Toma n número de obseraciones.

df_4_OMe.grf <- data.frame(emisora=
                             df_4_OMe.mej$"Emisoras",
                           calificacion=c(
                             df_4_OMe.mej$"2020 Q2",
                             df_4_OMe.mej$"2020 Q3",
                             df_4_OMe.mej$"2020 Q4",
                             df_4_OMe.mej$"2021 Q1",
                             df_4_OMe.mej$"2021 Q2"),
                           trimestre=c(
                             rep("2020 Q2", nrow(df_4_OMe.mej)),
                             rep("2020 Q2", nrow(df_4_OMe.mej)),
                             rep("2020 Q4", nrow(df_4_OMe.mej)),
                             rep("2021 Q1", nrow(df_4_OMe.mej)),
                             rep("2021 Q2", nrow(df_4_OMe.mej))))

#======================#
df_5_ESG <- tibble(df_esg$Emisora,
                   df_esg$"2017",
                   df_esg$"2018",
                   df_esg$"2019",
                   df_esg$"2020") %>%
  `colnames<-`(c("Emisoras","2017","2018","2019","2020"))

df_5_ESG.mej <- 
  df_5_ESG %>% #Agrupa en df la primera col y la ultima.
  arrange(desc(df_5_ESG[,dim(df_5_ESG)[2]])) %>% #Ajsusta la ultima columnas en orden descendente.
  slice(1:12) #Toma n número de obseraciones.

df_5_ESG.grf <- data.frame(emisora=
                             df_5_ESG.mej$Emisoras,
                           calificacion=c(
                             df_5_ESG.mej$"2017",
                             df_5_ESG.mej$"2018",
                             df_5_ESG.mej$"2019",
                             df_5_ESG.mej$"2020"),
                           trimestre=c(
                             rep("2027", nrow(df_5_ESG.mej)),
                             rep("2018", nrow(df_5_ESG.mej)),
                             rep("2019", nrow(df_5_ESG.mej)),
                             rep("2020", nrow(df_5_ESG.mej))))
  # Pagina 03 UI: 03 Cotizaciones ======================================####
tickers <- c("AC.MX",
             "AEROMEX.MX",
             "AGUA.MX")

prices <- 
  getSymbols(tickers,
             src = 'yahoo',
             from = "2015-01-01",
             to =   "2021-01-01",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(tickers) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(symbol, close, -date) %>%
  group_by(symbol) %>%
  na.omit()


  # Pagina 04 UI: 04 Rendimientos ======================================####
returns <- 
  getSymbols(tickers,
             src = 'yahoo',
             from = "2015-01-01",
             to =   "2021-01-01",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(tickers) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(symbol, close, -date) %>%
  group_by(symbol) %>%
  mutate(close = (log(close) - log(lag(close)))) %>%
  na.omit()


# UI ===========================================================================
  # Dashboard  ==========================================================####
  ui<-dashboardPage(
  title = "Dashboard", 
  skin = "black",
  dashboardHeader(title = h3("NAF-ESG")),
  dashboardSidebar(
    sidebarMenu(id ="sidebarID",
                menuItem(h5("00 Inicio"),
                         tabName = "inicio"),
                menuItem(h5("01 Metodología original"),
                         tabName = "seleccion_sin_ESG"),
                menuItem(h5("02 Metodología ESG."),
                         tabName = "seleccion_con_ESG"),
                menuItem(h5("03 Calificaciones historicas"),
                         tabName ="calificacion"),
                menuItem(h5("04 Cotizaciones"),
                         tabName = "cotizacion"),
                menuItem(h5("05 Retornos"),
                         tabName = "rendimiento"),
                menuItem(h5("06 Contacto"),
                         tabName = "contacto")
                )
    ),
  
  dashboardBody(
    tabItems(
  # Pagina 00 UI: 00 Inicio. =====================####      
  tabItem(tabName = "inicio",
          
          fluidPage(
          position="center",img(src="primera.png",width="100%",heigth="100%"))
          
  ),
  # Pagina 01 UI: 01 Seleccion de emisoras sin ESG =====================####
      tabItem(tabName = "seleccion_sin_ESG",
              
              pageWithSidebar(
                headerPanel(h3("Metodología original: Junio 2021/2021 Q2")),
                sidebarPanel(
                  radioButtons(inputId = "ciclo_no_esg", 
                               label = "Ciclo económico:",
                               choices = c("Expansivo." = "expan_no_esg",
                                           "Neutral." = "neutr_no_esg",
                                           "Recesivo." = "reces_no_esg"))),
                mainPanel(DT::dataTableOutput("tabla_no_esg")))
              
              ),
  # Pagina 01 UI: 01 Seleccion de emisoras con ESG =====================####
      tabItem(tabName = "seleccion_con_ESG",
              
              pageWithSidebar(
                headerPanel(h3("Metodología ESG: Junio 2021/2021 Q2")),
                sidebarPanel(
                  radioButtons(inputId = "ciclo_esg", 
                               label = "Ciclo económico:",
                               choices = c("Expansivo." = "expan_esg",
                                           "Neutral." = "neutr_esg",
                                           "Recesivo." = "reces_esg")),
                  sliderInput(inputId = "obs",
                              label = "Ponderación ESG:",
                              min = 0,
                              max = 1,
                              value = .01)
                ),
                mainPanel(DT::dataTableOutput("tabla_esg")))
              
              ),
  # Pagina 02 UI: 02 Calificaciones historicas =========================####
      tabItem(tabName = "calificacion",
              
              fluidPage(
                titlePanel(h3("Calificaciones historicas por rubro")),
                br(),
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Operación.",
                                       highchartOutput("grafica1"),
                                       width=100,
                                       DT::dataTableOutput("tabla1")),
                              tabPanel("Valuación.",
                                       highchartOutput("grafica2"),
                                       width=100,
                                       DT::dataTableOutput("tabla2")),
                              tabPanel("Apalancamiento.",
                                       highchartOutput("grafica3"),
                                       width=100,
                                       DT::dataTableOutput("tabla3")),
                              tabPanel("Ope. mercado.",
                                       highchartOutput("grafica4"),
                                       width=100,
                                       DT::dataTableOutput("tabla4")),
                              tabPanel("ESG.",
                                       highchartOutput("grafica5"),
                                       width=100,
                                       DT::dataTableOutput("tabla5"))
                  )
                )
              )
              
              ),
  # Pagina 03 UI: 03 Cotizaciones ======================================####
      tabItem(tabName = "cotizacion",
              
              fluidPage(#theme = shinytheme("cyborg"),
                # Title
                titlePanel(h3("Cotizaciones: Selección de emisoras")),
                # Sidebar 
                sidebarLayout(
                  sidebarPanel(width = 3,
                               # Let user pick stocks
                               pickerInput(
                                 inputId = "stocks",
                                 label = h4("Emisoras:"),
                                 choices = c(
                                   "AC"                  = tickers[1], 
                                   "AEROMEX"             = tickers[2],
                                   "AGUA"                = tickers[3]),
                                 selected = tickers,   
                                 options = list(`actions-box` = TRUE), 
                                 multiple = TRUE
                               )
                  ),
                  # Plot results
                  mainPanel(
                    mainPanel(highchartOutput("grafica_cot"),
                              width=100,
                              DT::dataTableOutput("tabla_cot")
                              )
                    )
                  )
                )
                  
              ),
    # Pagina 04 UI: 04 Rendimientos ======================================####
  tabItem(tabName = "rendimiento",
          
          fluidPage(#theme = shinytheme("cyborg"),
            # Title
            titlePanel(h3("Retornos")),
                mainPanel(highchartOutput("grafica_rend"),
                          width=100,
                          DT::dataTableOutput("tabla_rend")
                          )
            )
          
          
          ),
      # Pagina 04 UI: 04 Contacto. =====================####  
  tabItem(tabName = "contacto",
          
          fluidPage(
            position="center",img(src="ultimo.png",width="100%",heigth="100%"))
  )
)))

# Server =======================================================================
server <- function(input, output, session) {
  # Pagina 01 Server: 01 Selección de emisoras sin ESG =====================####
  
  
  output$tabla_no_esg = DT::renderDataTable({
    
    expansv_no_esg <- c(0.35,#Operacion.
                        0.15,#Valuacion.
                        0.15,#Apalancamiento.
                        0.35)#Ope. mercado.
    
    neutral_no_esg <- c(0.25,#Operacion.
                        0.25,#Valuacion.
                        0.25,#Apalancamiento.
                        0.25)#Ope. mercado
    
    recesv_no_esg <- c(0.20,#Operacion.
                       0.10,#Valuacion.
                       0.40,#Apalancamiento.
                       0.30)#Ope. mercado.
    
    ciclo_no_esg <- switch(input$ciclo_no_esg,
                           expan_no_esg = data.frame(mapply(`*`,
                                                            calif_no_esg,
                                                            expansv_no_esg,
                                                            SIMPLIFY=FALSE)),
                           neutr_no_esg = data.frame(mapply(`*`,
                                                            calif_no_esg,
                                                            neutral_no_esg,
                                                            SIMPLIFY=FALSE)),
                           reces_no_esg = data.frame(mapply(`*`,
                                                            calif_no_esg,
                                                            recesv_no_esg,
                                                            SIMPLIFY=FALSE)))
    
    calif_no_esg$Total <- rowSums(ciclo_no_esg)
    row.names(calif_no_esg) <- Emisoras
    
    DT::datatable(calif_no_esg)
  })
  
  # Pagina 01 Server: 01 Selección de emisoras con ESG =====================####
  # Tabla que será desplegada en el programa.
  
  output$tabla_esg = DT::renderDataTable({
    cambio <- input$obs
    ciclo_esg <- switch(input$ciclo_esg,
                        expan_esg = data.frame(mapply(`*`,
                                                      calif_esg,
                                                      c(0.35-(cambio/4),#Operacion.
                                                        0.15-(cambio/4),#Valuacion.
                                                        0.15-(cambio/4),#Apalancamiento.
                                                        0.35-(cambio/4),#Ope. mercado.
                                                        cambio),
                                                      SIMPLIFY=FALSE)),
                        
                        neutr_esg = data.frame(mapply(`*`,
                                                      calif_esg,
                                                      c(0.25-(cambio/4),#Operacion.
                                                        0.25-(cambio/4),#Valuacion.
                                                        0.25-(cambio/4),#Apalancamiento.
                                                        0.25-(cambio/4),#Ope. mercado
                                                        cambio),
                                                      SIMPLIFY=FALSE)),
                        
                        reces_esg = data.frame(mapply(`*`,
                                                      calif_esg,
                                                      c(0.20-(cambio/4),#Operacion.
                                                        0.10-(cambio/4),#Valuacion.
                                                        0.40-(cambio/4),#Apalancamiento.
                                                        0.3-(cambio/4),
                                                        cambio),
                                                      SIMPLIFY=FALSE)))
    
    calif_esg$Total <- rowSums(ciclo_esg)
    row.names(calif_esg) <- Emisoras
    
    DT::datatable(calif_esg)
  })
  # Pagina 02 Server: 02 Calificaciones historicas =========================####
  
  # TAB 1: Tabla Operacion.
  output$tabla1 = DT::renderDataTable({
    DT::datatable(
      df_1_Ope
    )
  })
  # TAB 1: Grafica Operacion.
  output$grafica1 <- renderHighchart({
    
    highchart(type="chart") %>%
      hc_title(text = "Calificaciones operación (2020 Q2-2021 Q2).") %>%
      hc_add_series(df_1_Ope.grf, type = "line",
                    hcaes(y = calificacion, 
                          x = trimestre,
                          group = emisora)) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)
  })
  
  
  # TAB 2: Tabla Valuacion.
  output$tabla2 = DT::renderDataTable({
    DT::datatable(
      df_2_Val
    )
  })
  # TAB 2: Grafica Valuacion.
  output$grafica2 <- renderHighchart({
    highchart(type="chart") %>%
      hc_title(text = "Calificaciones valuación (2020 Q2-2021 Q2).") %>%
      hc_add_series(df_2_Val.grf, type = "line",
                    hcaes(y = calificacion, 
                          x= trimestre,
                          group = emisora)) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)
  })
  
  
  # TAB 3: Tabla apalancamiento.
  output$tabla3 = DT::renderDataTable({
    DT::datatable(
      df_3_Apa
    )
  })
  # TAB 3: Grafica apalancamiento.
  output$grafica3 <- renderHighchart({
    highchart(type="chart") %>%
      hc_title(text = "Calificaciones apalancamiento (2020 Q2-2021 Q2).") %>%
      hc_add_series(df_3_Apa.grf, type = "line",
                    hcaes(y = calificacion, 
                          x = trimestre, 
                          group = emisora)) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)
  })
  
  
  # TAB 4: Tabla ope_mercado.
  output$tabla4 = DT::renderDataTable({
    DT::datatable(
      df_4_OMe
    )
  })
  # TAB 4: Grafica ope_mercado.
  output$grafica4 <- renderHighchart({
    highchart(type="chart") %>%
      hc_title(text = "Calificaciones operación de mercado (2020 Q2-2021 Q2).") %>%
      hc_add_series(df_4_OMe.grf, type = "line",
                    hcaes(y = calificacion, 
                          x = trimestre,
                          group = emisora)) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)
  })
  
  # TAB 5: ESG.
  output$tabla5 = DT::renderDataTable({
    DT::datatable(
      df_5_ESG
    )
  })
  # TAB 5: ESG.
  output$grafica5 <- renderHighchart({
    highchart(type="chart") %>%
      hc_title(text = "Calificaciones ESG (2017-2020).") %>%
      hc_add_series(df_5_ESG.grf, type = "line",
                    hcaes(y = calificacion, 
                          x = trimestre,
                          group = emisora)) %>%
      hc_navigator(enabled = FALSE) %>%
      hc_scrollbar(enabled = FALSE)
  })
  # Pagina 03 Server: 03 Cotizaciones ======================================####
  
  # server logic based on user input
  observeEvent(c(input$stocks), {
    
    # Seleccion de datos.
    prices <- 
      prices %>%
      filter(symbol %in% input$stocks)
    
    # Crear la grafica.
    output$grafica_cot <- renderHighchart({
      highchart(type="stock") %>%
        hc_title(text = "") %>%
        hc_add_series(prices, type = "line",
                      hcaes(y = close,
                            x = date,
                            group = symbol)) %>%
        hc_navigator(enabled = TRUE) %>%
        hc_scrollbar(enabled = FALSE)
    })
    
    # Crear la tabla.
    output$tabla_cot = DT::renderDataTable({
      DT::datatable(
        prices.df <- pivot_wider(prices,
                                 names_from = "symbol",
                                 values_from = "close"))
    })
    
  })
  # Pagina 04 Server: 03 Rendimientos ======================================####
  # server logic based on user input
  observeEvent(c(input$stocks), {
    
    # Seleccion de datos.
    returns <- 
      returns %>%
      filter(symbol %in% input$stocks)
    
    # Crear la grafica.
    output$grafica_rend <- renderHighchart({
      highchart(type="stock") %>%
        hc_title(text = "") %>%
        hc_add_series(returns, type = "line",
                      hcaes(y = close,
                            x = date,
                            group = symbol)) %>%
        hc_navigator(enabled = TRUE) %>%
        hc_scrollbar(enabled = FALSE)
    })
    
    # Crear la tabla.
    output$tabla_rend = DT::renderDataTable({
      DT::datatable(
        returns.df <- pivot_wider(returns,
                                  names_from = "symbol",
                                  values_from = "close"))
    })
    
    
  })
  
  }
# shinyApp =====================================================================
shinyApp(ui,server)
