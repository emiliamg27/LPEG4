# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinyjs, shinyWidgets)

ui = dashboardPage(
  skin = 'blue', 
  dashboardHeader(title = 'Buscador de gasolineras'),
  dashboardSidebar(
    useShinyjs(),
    column(10, 
           fluidRow(
             selectInput(
               'CCAA', 
               'Selecciona una CCAA',
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
           )
  )
)
