# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------

if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets, leaflet,leaflet.extras, data.table)

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
combustible <- data.table(
  precio_gasoleo_a = c(''),
  precio_gasoleo_premium = c(''),
  precio_gasolina_95_e5 = c(''),
  precio_gasolina_98_e5 = c('')
)

# COMIENZA SHINY UI -------------------------------------------

ui = dashboardPage(
  skin = 'blue',
  
  # CABECERA ----------------------------------------------------------------
  
  dashboardHeader(
    title = 'Gasolineras',
    # titleWidth = 300,
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
  ),
  

  # BARRA LATERAL -----------------------------------------------------------

  dashboardSidebar(
    useShinyjs(),
    # tags$style(HTML("
    #   .main-sidebar{
    #     width: 300px;
    #   }
    # ")),
    # column(10,
    #       fluidRow(
    #         textInput(inputId = 'pr', label = NULL , value = "", width = NULL, placeholder = "Escriba una provincia")
    #       )
    #       ),
    
    #Filtro CCAA
    column(12,
           fluidRow(
             selectizeInput(
               inputId = "CCAA",
               label = "Escoja la CCAA",
               choices = names(DT),
               selected = 1
               )
             )
    ),
    #Filtro provincia
    column(12,
           fluidRow(
             selectizeInput(
               inputId = "PROVINCIA",
               label = "Escoja la provincia",
               choices = DT[['ANDALUCÍA']],
               selected = 2
             )
           )
    ),
    #Filtro carburante
    column(12,
           fluidRow(
             selectInput ('tipogasoleo',
                          label = 'Seleccione un carburante',
                          choices = names(combustible),
                          selected = 'precio_gasoleo_a'
             )
           )
    ),
    #Filtro precio
    column(12, 
           fluidRow(
             sliderInput('precio',
                         label = 'Seleccione el precio máximo del carburante',
                         min = 0, 
                         max = 3, 
                         value = 1.9, 
                         round = FALSE,
                         step = 0.1
             )
           )
    ),
    
    # column(12, 
    #        fluidRow(
    #          sliderInput('pob',
    #                      label = 'Seleccione el rango de población',
    #                      min = 0, 
    #                      max = 100, 
    #                      value = 50
    #          )
    #        )
    #        ),
    
    #Filtro lowcost
    column(12,
           fluidRow(
             radioButtons('lowcost',
                          label = '¿Quiere una gasolinera LowCost?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          )
             )
           )
    ),
    #Filtro 24h
    column(12,
           fluidRow(
             radioButtons('si_24_h',
                          label = '¿Quiere una gasolinera 24H?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          )
             )
           )
    ),
    #Filtro autoservicio
    column(12,
           fluidRow(
             radioButtons('si_autoservicio',
                          label = '¿Quiere una gasolinera autoservicio?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2,
                            'Indiferente' = 3
                          )
             )
           )
    ),
    #Botón
    column(12,
           fluidRow(
             column(
               width = 10,
               actionBttn(
                 'boton', 
                 label = 'Buscar',
                 size = 'md',
                 color = 'primary',
                 style = 'minimal'
               ),
               align = 'center'
             )
           )
           )
    ),
  

# CUERPO ------------------------------------------------------------------

  dashboardBody(
    #Tabs
    tabsetPanel(
      #Tab principal: Mapa
      tabPanel(
        'Mapa', 
        fluidRow(
          box(width=12,
              status = 'primary',
              leafletOutput(outputId = "mymap", height = 600)),
          )
        ),
      #Tab Dashboard
      tabPanel(
        'Dashboard', 
        fluidRow(
          box(width = 12,
              title = 'Dashboard',
              status = 'primary',
              infoBoxOutput("rango", width = 3),
              infoBoxOutput("comunidad", width = 3),
              infoBoxOutput("provincia", width = 3)
          )
        )
      ),
      #Tab estadísticas
      tabPanel(
        'Estadísticas', 
        fluidRow(
          box(width = 12,
              title = 'hola',
              status = 'primary',
              dataTableOutput('gas'),
              downloadButton("descargar", "Descargar csv")
              )
          )
        ),
      )
    )
  )

