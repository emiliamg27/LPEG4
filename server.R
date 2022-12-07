# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr)

shinyServer(function(input, output)
{
  
  output$result <- renderText({
    paste("You chose", input$CCAA)
  })   
    
  # if(input$CCAA ==1){
  #   output$ccaa = renderInfoBox({
  #     valueBox(
  #       value = 'hola',
  #       subtitle = '',
  #       color = 'blue',
  #       width = 12
  #     )
  #   })
  # }else{
  #   output$ccaa = renderInfoBox({
  #     valueBox(
  #       value = 'hola2',
  #       subtitle = '',
  #       color = 'green',
  #       width = 12
  #     )
  #   })
  
  })

  
