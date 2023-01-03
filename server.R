# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, 
       shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr, jsonlite, 
       leaflet, geosphere, readxl)

DT <- data.table(
  Andalucía = c('Almería', 'Cádiz', 'Córdoba', 'Granada', 'Huelva', 'Jaén', 'Málaga', 'Sevilla'),
  Aragón = c('Huesca', 'Teruel', 'Zaragoza'),
  Asturias = c('Asturias'),
  Islas_Baleares = c('Islas Baleares'),
  Islas_Canarias = c('Las Palmas', 'Santa Cruz de Tenerife'),
  Cantabria = c('Cantabria'),
  Castilla_y_León = c('granada', 'sevilla', 'cordoba', 'jaen'),
  Castilla_La_Mancha = c('guada', 'albacete', 'cuenca', ''),
  Cataluña = c('granada', 'sevilla', 'cordoba', 'jaen'),
  Comunidad_Valenciana = c('guada', 'albacete', 'cuenca', ''),
  Extremadura = c('granada', 'sevilla', 'cordoba', 'jaen'),
  Galicia = c('guada', 'albacete', 'cuenca', ''),
  Comunidad_de_Madrid = c('granada', 'sevilla', 'cordoba', 'jaen'),
  Murcia = c('guada', 'albacete', 'cuenca', ''),
  Navarra = c('granada', 'sevilla', 'cordoba', 'jaen'),
  País_Vasco = c('guada', 'albacete', 'cuenca', ''),
  La_Rioja = c('granada', 'sevilla', 'cordoba', 'jaen'),
  Ceuta = c('guada', 'albacete', 'cuenca', ''),
  Melilla = c('granada', 'sevilla', 'cordoba', 'jaen')
)


shinyServer(function(input, output, session){
  
  logged_in <- reactiveVal(FALSE)
  
  # switch value of logged_in variable to TRUE after login succeeded
  observeEvent(input$login, {
    logged_in(ifelse(logged_in(), FALSE, TRUE))
  })
  
  # show "Login" or "Logout" depending on whether logged out or in
  output$logintext <- renderText({
    if(logged_in()) return("Logout here.")
    return("Login here")
  })
  
  # show text of logged in user
  output$logged_user <- renderText({
    if(logged_in()) return("User 1 is logged in.")
    return("")
  })
  
  
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
  ds_low_cost2 <- ds_low_cost%>% mutate(ds_low_cost,ccaa = ifelse (idccaa=="01","ANDALUCIA",ifelse (idccaa=="02","ARAGON", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","BALEARES", 
                                                                                                                                           ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA Y LEON", 
                                                                                                                                                                                                                     ifelse (idccaa=="08","CASTILLA - LA MANCHA", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD VALENCIANA",
                                                                                                                                                                                                                                                                                                           ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                     ifelse (idccaa=="16","PAIS VASCO", ifelse (idccaa=="17","LA RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA"))))))))))))))))))))
  
  # FRONT ------------------------------------------------------------
  
  observeEvent(input$CCAA, {
    freezeReactiveValue(input, "PROVINCIA")
    updateSelectizeInput(
      session,
      inputId = "PROVINCIA",
      choices = DT[[input$CCAA]],
      selected = 1
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$boton, {
    show_modal_spinner(
      spin = 'bounce',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
    
    
    # FILTROS DE DATOS ------------------------------------------------------------
    
    if (input$CCAA == 1){
      output$comunidad = renderInfoBox({
        valueBox (
          value = 'ANDALUCÍA',
          subtitle = 'ccaa',
          color = 'blue',
          width = 12,
          icon = icon("person")
        )
      })
      output$provincia = renderInfoBox({
        valueBox (
          value = input$pr,
          subtitle = 'provincia',
          color = 'blue',
          width = 12,
          icon = icon("person")
        )
      })
    }
    else if (input$CCAA == 2){
      output$datos = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'holi',
          color = 'blue',
          width = 12
        )
      })
    }
    else if (input$CCAA == 3){
      output$precios = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'datosss',
          color = 'blue',
          width = 12
        )
      })
    }
    else if (input$CCAA == 4){
      output$precios = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'datosss',
          color = 'blue',
          width = 12
        )
      })
    }
    # else if (output$value <- renderText({ input$caption }))
    
    output$rango = renderInfoBox({
      valueBox (
        value = input$pob,
        subtitle = 'población',
        color = 'blue',
        width = 12,
        icon = icon("person")
      )
    })
    
    
    output$gas = renderDataTable(ds_low_cost2 %>% select(rotulo, municipio),
                                 options = list(pageLength = 10, info = TRUE))
    
    output$descargar <- downloadHandler(
      filename = function(){"thename.csv"}, 
      content = function(fname){
        write.csv(ds_low_cost2 %>% select(rotulo, municipio), fname)
      }
    )
    pal <- colorFactor(palette = "YlGnBu", levels = ds_low_cost2$ccaa, reverse = TRUE)
    
    
    output$mymap <-renderLeaflet({
      
      leaflet(ds_low_cost2)%>% 
        addTiles()%>%
        addCircleMarkers(lng = ~ds_low_cost2$longitud_wgs84,lat = ~ds_low_cost2$latitud,radius = 1.2,color = ~pal(ccaa))%>% 
        setView(lng = ~ds_low_cost2$longitud_wgs84,lat = ~ds_low_cost2$latitud,zoom=2)
      
    })
    
    #leafletProxy("mymap", data = ds_low_cost2) %>%
    #  addTiles() %>% 
    #  clearShapes() %>% 
    #  addPolygons(data = ds_low_cost2, fillColor = ~pal(ccaa), fillOpacity = 0.7, weight = 2)  
  })
})
  
