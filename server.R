# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, 
       shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr, jsonlite, 
       leaflet, geosphere, readxl)

shinyServer(function(input, output){
  
  # file = reactive({
  #   fichero <- input$fichero
  #   df <- read_excel(paste(fichero$datapath, '.xlsx'))
  #   df
  # })
  
  # CARGA DE DATOS ------------------------------------------------------------
  
  url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/' 
  data <- fromJSON(url_)
  ds_raw <- data$ListaEESSPrecio
  ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble()
  ds_f %>% count(rotulo) 
  ds_f %>% distinct(rotulo) 
  no_low_cost <- c('REPSOL','CEPSA', 'GALP','SHELL','BP','PETRONOR','AVIA','Q8', 'CAMPSA','BONAREA')
  ds_low_cost <- ds_f %>% mutate(low_cost = !rotulo %in% no_low_cost)
  
  
  # FRONT ------------------------------------------------------------
  
  observeEvent(input$boton, {
    show_modal_spinner(
      spin = 'bounce',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
  
    if (input$CCAA == 1){
      output$datos = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'datosss',
          color = 'blue',
          width = 12
        )
      })
    }
    else if (input$CCAA != 1){
      output$datos = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'holi',
          color = 'blue',
          width = 12
        )
      })
    }
    output$rango = renderInfoBox({
      valueBox (
        value = input$pob,
        subtitle = 'poblacion',
        color = 'blue',
        width = 12
      )
    })
    
    
    output$gas = renderDataTable(ds_low_cost %>% select(rotulo, municipio),
                                 options = list(pageLength = 10, info = TRUE))
    
    output$descargar <- downloadHandler(
      filename = function(){"thename.csv"}, 
      content = function(fname){
        write.csv(ds_low_cost %>% select(rotulo, municipio), fname)
      }
    )
      
  })
})
  