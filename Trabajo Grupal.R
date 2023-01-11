# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# LOADING LIBS ------------------------------------------------------------
if(!require("pacman")) install.packages("pacman")
p_load(tidyverse, magrittr, janitor, lubridate, tidyr, httr)


# CARGA DE DATOS ---------------------------------------------------------
url_ <- "https://sedeaplicaciones.minetur.gob.es/ServiciosRESTCarburantes/PreciosCarburantes/EstacionesTerrestres/"

f_raw <- jsonlite::fromJSON(url_)
df_source <- f_raw$ListaEESSPrecio %>% glimpse()
janitor::clean_names(df_source) %>% glimpse()

#ver config
locale() 
#Cambiamos config
df1<-df_source %>% janitor::clean_names() %>% type_convert(locale = locale(decimal_mark = ",")) %>% view()
str_squish(df1$rotulo)

# CREAR NUEVAS VARIABLES ------------------------------------------------

##Clasificamos por gasolineras baratas y no baratas NUEVA COLUMNA
df2 <- df1 %>% mutate(lowcost =! rotulo %in% c("CEPSA", "REPSOL", "BP", "SHELL"))

#Filtrado por 24 h

df2 %>% count(horario, sort = TRUE)%>% view()
no_24h <- df2  %>% mutate('24H' = horario %in% 'L-D: 24H') %>% view()

##Columna nombre ccaa
df3 <- df2%>% mutate(df2,ccaa = ifelse (idccaa=="01","ANDALUCIA",ifelse (idccaa=="02","ARAGON", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","BALEARES", 
                                                                                                                                                ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA Y LEON", 
                                                                                                                                                                                                                   ifelse (idccaa=="08","CASTILLA - LA MANCHA", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD VALENCIANA",
                                                                                                                                                                                                                                                                                                                ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                                                                                                                                                                                                                                                                                                                                                                                                                          ifelse (idccaa=="16","PAIS VASCO", ifelse (idccaa=="17","LA RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA"))))))))))))))))))))
##Precio medio del gasoleo en las CCAA
df3 %>% select(precio_gasoleo_a, precio_gasolina_95_e5, idccaa, rotulo, lowcost) %>% drop_na() %>% group_by(idccaa, lowcost) %>% summarize(mean(precio_gasoleo_a), mean(precio_gasolina_95_e5)) %>% view()


# write.csv(dflow,"ds21903993(33).csv")
# 
# 
# write.csv(dfccaa,"ds21903993(34).csv")
##Gasolinera más barata


##Unimos con un dataset con la poblacion total, hombres y mujeres de cada municipio

library(readxl)
pobmun21 <- read_excel("/Users/albalagunamoraleda/Documents/LPE22017327/LPEG4/pobmun21.xlsx", skip = 1)%>%view()
columnapob<-select(pobmun21, POB21, municipio,id_provincia,HOMBRES,MUJERES)

columnapob%>%view()
df_pob <-merge(x = df3, y = columnapob, by=c("municipio"), all.x = TRUE)
nrow(df_pob[duplicated(df_pob), ])

df_pob_d <- df_pob %>% distinct(direccion, .keep_all = TRUE)

#Añadimos tipo servicio

library(readxl)
tipo_servicio <- read_excel("/Users/albalagunamoraleda/Documents/LPE22017327/LPEG4/preciosEESS_es.xls", skip = 3)
columnaserv<-select(tipo_servicio, direccion, Tipo_servicio)
df_servicio <-merge(x = df_pob_d, y = columnaserv, by=c("direccion"), all.x = TRUE)
df_servicio_personal<-df_servicio %>% mutate(autoservicio = str_detect(Tipo_servicio,pattern = "(A)")) %>%view()

#Añadimos distancia
p_load(tidyverse, janitor, jsonlite, leaflet, geosphere, mapsapi,xml2,mapsapi)
distancias<-df_servicio_personal %>% select(latitud,longitud_wgs84, rotulo,direccion)

uem <- c(-3.919257897378161, 40.373942679873714)
direccion <- "Calle de Goya, 88, 28009 Madrid"
key= "AIzaSyBRMg-WgkdJSg30zDdtALjHapL1Z4v06As"
res<-mp_geocode(direccion,key=key)
bar<-mp_get_points(res)%>%select(pnt)
bar2<-as.numeric(unlist(bar))

#distancias_villa <- distancias %>%select(longitud_wgs84,latitud) %>% distGeo(uem)

#df_distancias<-df_servicio_personal%>%  mutate(distancias = round(distancias_villa/1000, digits = 2)) %>% view()

distancias_cualquiera<-distancias %>% select(longitud_wgs84,latitud) %>% distGeo(bar2)

df_cualquiera<-df_servicio_personal%>%  mutate(distancias = round(distancias_cualquiera/1000, digits = 2))%>%view()
# READING AND WRITING (FILES) ---------------------------------------------


