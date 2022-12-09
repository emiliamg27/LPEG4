require(pacman)
p_load(shiny, shinythemes,leaflet,leaflet.extras,mapsapi,geosphere,ggmap,httr,
       xml2,jsonlite,readxl,sparklyr,janitor,stringr,tidyverse,
       shinydashboard, shinyjs, shinyWidgets)
api <- "AIzaSyCHI04kh-YwFaqzmC8uqwZ4vp_mu7nhCPE"

ui <- navbarPage("Proyecto Lep",id = "idTabset",
                 
                 tabPanel("Login", value = "panelLogin",
                          fluidPage(theme = shinytheme("superhero"),
                                    verticalLayout(
                                      h2("Bienvenido a la aplicacion Lep"),
                                      p("Introduce su usuaarioy contraseña"),
                                      textInput("direccion_in", NULL),
                                      fluidRow(actionButton('irLogin', 'Tools for consumers'),
                                               actionButton('irEmpresario', 'Tools for entrepreneurs'))
                                    )
                          )),
                 
                 tabPanel("Precios", value = "panelPrecios",
                          fluidPage(theme = shinytheme("superhero"),
                                    verticalLayout(
                                      h2("Panel de precios"),
                                      p("Introduce an address:"),
                                      textInput("direccion_in", NULL),
                                      fluidRow(actionButton('irConsumidor', 'Tools for consumers'),
                                               actionButton('irEmpresario', 'Tools for entrepreneurs'))
                                    )
                          )),
                 
                 tabPanel("Estadisticas", value = "panelEstadisticas",
                          fluidPage(theme = shinytheme("superhero"),
                                    verticalLayout(
                                      h2("Panel de Estadisticas"),
                                      fluidRow(actionButton('irConsumidor', 'Por Comunidades Autonomas'),
                                               actionButton('irEmpresario', 'Por Provincias'),
                                               actionButton('irEmpresario', 'Por Municipios')
                                      )
                                    )
                          )
                 ),
                 
                 
                 tabPanel("Consumidores", value = "panelPersonalizado",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                h5("Introduzca una nueva direccion:"),
                                textInput("direccion_in_v2", NULL),
                                actionButton('irConsumidor_v2', 'Buscar direccion', class = "btn-success"),
                                h3(""),
                                h5("Establezca los filtros para su mapa:"),
                                selectInput("abierto24h",
                                            label = "Seleccione un horario:",
                                            choices = c("Abiertas 24 horas",
                                                        "No abiertas 24h",
                                                        "Todos los horarios"),
                                            selected = "Todos los horarios"),
                                h3(""),
                                sliderInput("distancia",label = "Rango de gasolineras",min = 0, max = 10000, value = 10000),
                                h3(""),helpText("Tipo de combustible:"), # Checkboxes
                                checkboxInput("ofreceGasolina95", label = "Gasolina E95", value = FALSE, width = NULL),
                                checkboxInput("ofreceDiesel", label = "Gasoleo A", value = FALSE, width = NULL),
                                checkboxInput("ofreceGLP", label = "GLP", value = FALSE, width = NULL),
                                checkboxInput("ofreceGas", label = "Gas natural", value = FALSE, width = NULL)
                              ),
                              mainPanel(
                                leafletOutput(outputId = "distPlot", width = "700px",height = "300px"),
                                h3(""),
                                h5("Listado de gasolineras:"),
                                dataTableOutput("table_usu")
                              )
                            )
                            
                            
                          ),
                          
                 ),
)

server <- function(input, output) {
}

shinyApp(ui, server)
