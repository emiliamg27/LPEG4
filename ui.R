# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets, leaflet,leaflet.extras)

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

ui = dashboardPage(
  skin = 'blue', 
  #Cabecera
  dashboardHeader(
    title = 'Gasolineras',
    tags$li(class = "dropdown",
            tags$li(class = "dropdown", textOutput("logged_user"), style = "padding-top: 15px; padding-bottom: 15px; color: #fff;"),
            tags$li(class = "dropdown", actionLink("login", textOutput("logintext"))))
  ),
  
  # Barra lateral  ------------------------------------------------------------
  #Barra lateral
  dashboardSidebar(
    useShinyjs(),
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
               label = "To period:",
               choices = DT[['Andalucía']],
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
                      choices = list(
                        'Precio Gasóleo A' = 1,
                       'Precio Gasóleo Premium' = 2,
                      'Precio Gasolina 95 e5' = 3,
                     'Precio Gasolina 98 e5' = 4
                  ),
                 selected = 1
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

