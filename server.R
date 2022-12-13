# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(readxl, shiny, shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets, shinybusy, tidyverse, magrittr, janitor, lubridate, tidyr, httr)

shinyServer(function(input, output){
  file = reactive({
    fichero <- input$fichero
    df <- read_excel(paste(fichero$datapath, '.xlsx'))
    df
  })
  observeEvent(input$boton, {
    show_modal_spinner(
      spin = 'bounce',
      color = 'blue',
      text = 'Cargando...'
    )
    Sys.sleep(2)
    remove_modal_spinner()
  
    if (input$CCAA == 1){
      output$datos = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'datosss',
          color = 'blue',
          width = 12
        )
      })
    }
    else if (input$CCAA != 1){
      output$datos = renderInfoBox({
        valueBox (
          value = 1,
          subtitle = 'holi',
          color = 'blue',
          width = 12
        )
      })
    }
    output$rango = renderInfoBox({
      valueBox (
        value = input$pob,
        subtitle = 'poblacion',
        color = 'blue',
        width = 12
      )
    })
  })
})
  
