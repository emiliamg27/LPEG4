# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets, leaflet,leaflet.extras)

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
    column(10, 
           fluidRow(
             selectInput('CCAA', 
                         label = 'Seleccione una CCAA', 
                         choices = list(
                           'Andalucía' = 1,
                           'Aragón' = 2,
                           'Asturias' = 3,
                           'Islas Baleares' = 4,
                           'Canarias' = 5,
                           'Cantabria' = 6,
                           'Castilla y León' = 7,
                           'Castilla - La Mancha' = 8,
                           'Cataluña' = 9,
                           'Comunidad Valenciana' = 10,
                           'Extremadura' = 11,
                           'Galicia' = 12,
                           'Comunidad de Madrid' = 13,
                           'Murcia' = 14,
                           'Navarra' = 15,
                           'País Vasco' = 16,
                           'La Rioja' = 17,
                           'Ceuta' = 18,
                           'Melilla' = 19
                         ),
                         selected = 1
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
          box(width = 12,
            infoBoxOutput('datos', width = 6),
            infoBoxOutput('rango', width = 6)
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
      
    )
  )
)
)