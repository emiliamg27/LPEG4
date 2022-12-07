# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinydashboardPlus, shinyjs, shinywidgets, shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr)

shinyServer(function(input, output)
{
  if(input$CCAA ==1){
    print('hola')
  }else{
    print('hola2')
  }
}
  
)