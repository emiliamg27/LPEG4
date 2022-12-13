# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets)

ui = dashboardPage(
  skin = 'blue-light', 
  #Cabecera
  dashboardHeader(title = 'Gasolineras'),
  #Barra lateral
  dashboardSidebar(
    useShinyjs(),
    column(10, 
           fluidRow(
             selectInput('CCAA', 
                         shiny::HTML("<p><span style='color: rgb(3, 3, 3)'>Seleccione una CCAA</span></p>"),
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
                         selected = NULL
                        )
                )
           ),
    column(10, 
           fluidRow(
             sliderInput('pob',
                         shiny::HTML("<p><span style='color: rgb(3, 3, 3)'>Seleccione el rango de población</span></p>"),
                         min = 0, 
                         max = 100, 
                         value = 50
               
             )
           )
      
    ),
    column(10,
           fluidRow(
             radioButtons('lowcost',
                          shiny::HTML("<p><span style='color: rgb(3, 3, 3)'>¿Quiere una gasolinera LowCost?</span></p>"),
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
             ),
           )
    )
  ),
  
  #Cuerpo
  dashboardBody(
    tabsetPanel(
      tabPanel(
        'mapa', 
        fluidRow(
          box(
            width = 12,
            title = 'Mapa gasolineras',
            status = 'primary'
          )
        )
      ),
      tabPanel(
        'mapa2', 
        fluidRow(
          box(
            width = 12,
            title = 'Mapa2 gasolineras',
            status = 'primary'
          )
        )
      )
    )
  )
)
