# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets, leaflet,leaflet.extras, data.table)

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
  precio_gasolina_98_e10 = c(''),
  precio_gas_natural_licuado = c('')
)

ui = dashboardPage(
  skin = 'blue', 
  #Cabecera
  dashboardHeader(
    
    title = 'Gasolineras',
    # titleWidth = 300,
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
  ),
  
  # Barra lateral  ------------------------------------------------------------
  #Barra lateral
  dashboardSidebar(
    useShinyjs(),
    # tags$style(HTML("
    #   .main-sidebar{
    #     width: 300px;
    #   }
    # ")),
    # column(10,
    #        fluidRow(
    #          fileInput(
    #            'fichero',
    #            label = 'Agregar fichero',
    #            accept = c('.xls')
    #          )
    #        )
    #        ),
    # column(10,
    #        fluidRow(
    #          selectInput('CCAA',
    #                      label = 'Seleccione una CCAA',
    #                      choices = list(
    #                        'Andalucía' = 1,
    #                        'Aragón' = 2,
    #                        'Asturias' = 3,
    #                        'Islas Baleares' = 4,
    #                        'Canarias' = 5,
    #                        'Cantabria' = 6,
    #                        'Castilla y León' = 7,
    #                        'Castilla - La Mancha' = 8,
    #                        'Cataluña' = 9,
    #                        'Comunidad Valenciana' = 10,
    #                        'Extremadura' = 11,
    #                        'Galicia' = 12,
    #                        'Comunidad de Madrid' = 13,
    #                        'Murcia' = 14,
    #                        'Navarra' = 15,
    #                        'País Vasco' = 16,
    #                        'La Rioja' = 17,
    #                        'Ceuta' = 18,
    #                        'Melilla' = 19
    #                      ),
    #                      selected = 1
    #                     )
    #             )
    #        ),
    # column(10, 
    #        fluidRow(
    #          selectInput('provi', 
    #                      label = 'Seleccione una Provincia', 
    #                      choices = list(
    #                        'Granada' = 1,
    #                        'Guadalajara' = 2,
    #                        'Valencia' = 3,
    #                        'Soria' = 4,
    #                        'Barcelona' = 5,
    #                        'A Coruña' = 6
    #                      ),
    #                      selected = 1
    #          )
    #        )
    # ),
    # column(10,
    #       fluidRow(
    #         textInput(inputId = 'pr', label = NULL , value = "", width = NULL, placeholder = "Escriba una provincia")
    #       )
    #       ),
    column(10,
           fluidRow(
             selectizeInput(
               inputId = "CCAA",
               label = "Escoja la CCAA",
               choices = names(DT),
               selected = 1
               )
             )
    ),
    column(10,
           fluidRow(
             selectizeInput(
               inputId = "PROVINCIA",
               label = "Escoja la provincia",
               choices = DT[['ANDALUCÍA']],
               selected = 2
             )
           )
    ),
    
    column(10, 
           fluidRow(
             sliderInput('pob',
                         label = 'Seleccione el rango de población',
                         min = 0, 
                         max = 100, 
                         value = 50
             )
           )
           ),
    column(10,
           fluidRow(
             radioButtons('lowcost',
                          label = '¿Quiere una gasolinera LowCost?',
                          choices = list(
                            'Si' = 1,
                            'No' = 2
                          )
             )
           )
    ),
   column(10,
          fluidRow(
          selectInput ('tipogasoleo',
                       label = 'Seleccione un carburante',
                      choices = names(combustible),
                      selected = 'precio_gasoleo_a'
                 )
          )
          ),
   column(10, 
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
    column(10,
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
  
  #Cuerpo
  dashboardBody(
    tabsetPanel(
      tabPanel(
        'Mapa', 
        fluidRow(
          box(width=12,
              leafletOutput(outputId = "mymap")),
          )
        ),
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

