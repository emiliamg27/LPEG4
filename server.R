# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# CARGA DE LIBRERÍAS ------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, 
       shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr, jsonlite, 
       leaflet, geosphere, readxl, data.table)


# DICCIONARIOS PARA LOS FILTROS -------------------------------------------

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



# COMIENZO SHINY SERVER ----------------------------------------------------------

shinyServer(function(input, output, session){
  

  # LOG IN ------------------------------------------------------------------
  
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
  
  # CARGA DE DATOS ------------------------------------------------------------
  
  url_='https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/' 
  data <- fromJSON(url_)
  ds_raw <- data$ListaEESSPrecio
  ds_f <- ds_raw %>% clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% as_tibble()
  ds_f %>% count(rotulo) 
  ds_f %>% distinct(rotulo) 
  

  # LIMPIEZA DE DATOS -------------------------------------------------------

  #Eliminamos columnas innecesarias (carburantes poco comunes)
  borrar <- c('precio_biodiesel','precio_bioetanol','precio_gas_natural_comprimido','precio_gas_natural_licuado',
              'precio_gases_licuados_del_petroleo','precio_gasoleo_b','precio_gasolina_95_e10','precio_gasolina_95_e5_premium',
              'precio_gasolina_98_e10','precio_hidrogeno')
  datos <- ds_f[ , !(names(ds_f) %in% borrar)]
  
  #Diferenciamos por lowcost
  no_low_cost <- c('REPSOL','CEPSA', 'GALP','SHELL','BP','PETRONOR','AVIA','Q8', 'CAMPSA','BONAREA')
  ds_low_cost <- datos %>% mutate(low_cost = !rotulo %in% no_low_cost)
  
  #Creamos columna con el nombre de las CCAA
  ds_low_cost2 <- ds_low_cost%>% mutate(ds_low_cost,ccaa = ifelse (idccaa=="01","ANDALUCÍA",ifelse (idccaa=="02","ARAGÓN", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","ISLAS_BALEARES", 
                                                           ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA_Y_LEÓN", 
                                                           ifelse (idccaa=="08","CASTILLA_LA_MANCHA", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD_VALENCIANA",
                                                           ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","COMUNIDAD_DE_MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                           ifelse (idccaa=="16","PAÍS_VASCO", ifelse (idccaa=="17","LA_RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA"))))))))))))))))))))
  
  #Convertimos los valores nulos en 0
  ds_low_cost2[is.na(ds_low_cost2)] <- 0
  
  #Diferenciamos por 24h
  no_24h <- ds_low_cost2  %>% mutate('si_24H' = horario %in% 'L-D: 24H')
  
  #Diferenciamos por autoservicio
  tipo_servicio <- read_excel("preciosEESS_es.xls", skip = 3)
  columnaserv <- select(tipo_servicio, direccion, Tipo_servicio)
  df_servicio <- merge(x = no_24h, y = columnaserv, by=c("direccion"), all.x = TRUE)
  df_autoserv <- df_servicio %>% mutate(autoservicio = str_detect(Tipo_servicio,pattern = "(A)"))
  
  
  # FRONT ------------------------------------------------------------
  
  #Imponemos las provinicas en base a la CCAA elegida
  observeEvent(input$CCAA, {
    freezeReactiveValue(input, "PROVINCIA")
    updateSelectizeInput(
      session,
      inputId = "PROVINCIA",
      choices = DT[[input$CCAA]],
      selected = 1
    )}, ignoreInit = TRUE)
  
  #Botón cargando
  observeEvent(input$boton, {
    show_modal_spinner(
      spin = 'bounce',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
    
    # PESTAÑA DASHBOARD ------------------------------------------------------------
    
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
    
    # FILTROS DE DATOS ------------------------------------------------------------
    
    #Filtro provincia
    df_provincia <- filter(df_autoserv,provincia==input$PROVINCIA)
    
    #Filtro de low_cost
    if (input$lowcost==1){
      df_lo <- filter(df_provincia,low_cost == TRUE)
    }else if (input$lowcost!=1){
      df_lo <- filter(df_provincia,low_cost == FALSE)
    }
    
    #Filtro de 24_h
    if (input$si_24_h==1){
      df_24 <- filter(df_lo,si_24H == TRUE)
    }else if (input$si_24_h!=1){
      df_24 <- filter(df_lo,si_24H == FALSE)
    }
    
    #Filtro de autoservicio
    if (input$si_autoservicio==1){
      df_auto <- filter(df_24,autoservicio == TRUE)
    }else if (input$si_autoservicio!=1){
      df_auto <- filter(df_24,autoservicio == FALSE)
    }
    
    #Filtro por precio
    df_precio <- subset(df_auto, df_auto[input$tipogasoleo] < input$precio & df_auto[input$tipogasoleo] > 0 )
    
    tabla_final <- df_precio %>% select(ccaa, provincia,rotulo, municipio, input$tipogasoleo, low_cost, si_24H, horario, autoservicio, latitud, longitud_wgs84)
    output$gas = renderDataTable(tabla_final,
                                 options = list(pageLength = 10, info = TRUE))
    
    # output$descargar <- downloadHandler(
    #   filename = function(){"gasolineras.csv"}, 
    #   content = function(fname){
    #     write.csv(df_precio, fname)
    #   }
    # )
    
    output$descargar <- downloadHandler(
      # Para la salida en PDF, usa "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copia el reporte a un directorio temporal antes de porcesarlo, en
        #caso de que no tengamos permiso de escritura en el directorio actual
        #puede ocurrir un error
        tempReport <- file.path(tempdir(), "informe.Rmd")
        file.copy("informe.Rmd", tempReport, overwrite = TRUE)

        # configurar los parametros para pasar al documento .Rmd
        params <- list(n = tabla_final)

        #Copilar el documento con la lista de parametros, de tal manera que se
        #evalue de la misma manera que el entorno de la palicacipon.
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # library(rmarkdown)
    # function(input, output){
    #   output$downloadWordReport =
    #     downloadHandler(filename = "report.html",
    #                     content = function(file){
    #                       
    #                       render("informe_prueba.Rmd", output_format ="html_document",
    #                              output_file = file,
    #                              quiet = TRUE)
    #                     })
    # }
    
    # MAPA ------------------------------------------------------------
    
    pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), 
                       domain = c("Charity", "Government", "Private"))
    
    output$mymap <- renderLeaflet({
      leaflet(tabla_final) %>% 
        addCircles(lng = ~longitud_wgs84, lat = ~latitud) %>% 
        addTiles() %>%
        addCircleMarkers(data = tabla_final, lat =  ~latitud, lng =~longitud_wgs84, 
                         radius = 3, 
                         color = ~pal(provincia),
                         stroke = FALSE, fillOpacity = 0.8)%>%
        addLegend(pal=pal, values=tabla_final[input$tipogasoleo,],opacity=1, na.label = "Not Available")%>%
        addEasyButton(easyButton(
          icon="fa-crosshairs", title="ME",
          onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    })
    
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
  
