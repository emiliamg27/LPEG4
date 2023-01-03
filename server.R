# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, 
       shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr, jsonlite, 
       leaflet, geosphere, readxl, data.table)

DT <- data.table(
  ANDALUCÍA = c('ALMERÍA', 'CÁDIZ', 'CÓRDOBA', 'GRANADA', 'HUELVA', 'JAÉN', 'MÁLAGA', 'SEVILLA', ''),
  ARAGÓN = c('HUESCA', 'TERUEL', 'ZARAGOZA', '', '', '', '', '', ''),
  ASTURIAS = c('ASTURIAS','', '', '', '', '', '', '', ''),
  ISLAS_BALEARES = c('BALEARS (ILLES)','', '', '', '', '', '', '', ''),
  CANARIAS = c('PALMAS (LAS)', 'SANTA CRUZ DE TENERIFE','', '', '', '', '', '', ''),
  CANTABRIA = c('CANTABRIA','', '', '', '', '', '', '', ''),
  CASTILLA_Y_LEÓN = c('ÁVILA', 'BURGOS', 'LEÓN', 'PALENCIA', 'SALAMANCA', 'SEGOVIA', 'SORIA', 'VALLADOLID', 'ZAMORA'),
  CASTILLA_LA_MANCHA = c('ALBACETE', 'CIUDAD REAL', 'CUENCA', 'GUADALAJARA', 'TOLEDO', '', '', '', ''),
  CATALUÑA = c('BARCELONA', 'GIRONA', 'LLEIDA', 'TARRAGONA','', '', '', '', ''),
  COMUNIDAD_VALENCIANA = c('ALICANTE', 'CASTELLÓN / CASTELLÓ', 'VALENCIA / VALÈNCIA','', '', '', '', '', ''),
  EXTREMADURA = c('BADAJOZ', 'CÁCERES','', '', '', '', '', '', ''),
  GALICIA = c('CORUÑA (A)', 'LUGO', 'OURENSE', 'PONTEVEDRA', '', '', '', '', ''),
  COMUNIDAD_DE_MADRID = c('MADRID','', '', '', '', '', '', '', ''),
  MURCIA = c('MURCIA','', '', '', '', '', '', '', ''),
  NAVARRA = c('NAVARRA','', '', '', '', '', '', '', ''),
  PAÍS_VASCO = c('ARABA/ÁLAVA', 'BIZKAIA', 'GIPUZKOA', '', '', '', '', '', ''),
  LA_RIOJA = c('RIOJA (LA)','', '', '', '', '', '', '', ''),
  CEUTA = c('CEUTA','', '', '', '', '', '', '', ''),
  MELILLA = c('MELILLA','', '', '', '', '', '', '', '')
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
  ds_low_cost2 <- ds_low_cost%>% mutate(ds_low_cost,ccaa = ifelse (idccaa=="01","ANDALUCÍA",ifelse (idccaa=="02","ARAGÓN", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","ISLAS_BALEARES", 
                                                           ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA_Y_LEÓN", 
                                                           ifelse (idccaa=="08","CASTILLA_LA_MANCHA", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD_VALENCIANA",
                                                           ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","COMUNIDAD_DE_MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                           ifelse (idccaa=="16","PAÍS_VASCO", ifelse (idccaa=="17","LA_RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA"))))))))))))))))))))

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
    output$comunidad = renderInfoBox({
      valueBox (
        value = input$CCAA,
        subtitle = 'ccaa',
        color = 'blue',
        width = 12,
        icon = icon("person")
      )
    })
    output$provincia = renderInfoBox({
      valueBox (
        value = input$PROVINCIA,
        subtitle = 'provincia',
        color = 'blue',
        width = 12,
        icon = icon("person")
      )
    })
    output$rango = renderInfoBox({
      valueBox (
        value = input$precio,
        subtitle = 'población',
        color = 'blue',
        width = 12,
        icon = icon("person")
      )
    })
    
    df_provincia <- filter(ds_low_cost2,provincia==input$PROVINCIA)
    
    output$gas = renderDataTable(df_provincia %>% select(provincia,rotulo, municipio),
                                 options = list(pageLength = 10, info = TRUE))
    
    output$descargar <- downloadHandler(
      filename = function(){"thename.csv"}, 
      content = function(fname){
        write.csv(ds_low_cost2 %>% select(rotulo, municipio), fname)
      }
    )
    #pal <- colorFactor(palette = "YlGnBu", levels = ds_low_cost2$ccaa, reverse = TRUE)
    
    
    # output$mymap <-renderLeaflet({
    #   
    #   leaflet(ds_low_cost2)%>% 
    #     addTiles()%>%
    #     addCircleMarkers(lng = ~ds_low_cost2$longitud_wgs84,lat = ~ds_low_cost2$latitud,radius = 1.2,color = ~pal(ccaa))%>% 
    #     setView(lng = ~ds_low_cost2$longitud_wgs84,lat = ~ds_low_cost2$latitud,zoom=2)
    #   
    # })
    
    #leafletProxy("mymap", data = ds_low_cost2) %>%
    #  addTiles() %>% 
    #  clearShapes() %>% 
    #  addPolygons(data = ds_low_cost2, fillColor = ~pal(ccaa), fillOpacity = 0.7, weight = 2)  
  })
})
  
