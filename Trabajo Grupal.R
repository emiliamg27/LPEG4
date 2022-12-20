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
#df2 %>% count(horario, sort = TRUE)
#no_24h <- df2 %>% filter(horario == 'L-D: 24H') %>% select(!horario) %>% view()

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
pobmun21 <- read_excel("pobmun21.xlsx", skip = 1)%>%view()
columnapob<-select(pobmun21, POB21, municipio,id_provincia,HOMBRES,MUJERES)

columnapob%>%view()
df_pob <-merge(x = df3, y = columnapob, by=c("municipio"), all.x = TRUE)
nrow(df_pob[duplicated(df_pob), ])

df_pob_d <- df_pob %>% distinct(direccion, .keep_all = TRUE)

#Añadimos tipo servicio

library(readxl)
tipo_servicio <- read_excel("preciosEESS_es.xls", skip = 3)
columnaserv<-select(tipo_servicio, direccion, Tipo_servicio)
df_servicio <-merge(x = df_pob_d, y = columnaserv, by=c("direccion"), all.x = TRUE)
#Añadimos distancia
p_load(tidyverse, janitor, jsonlite, leaflet, geosphere, mapsapi,xml2,mapsapi)
prueba<-df_servicio %>% select(latitud,longitud_wgs84,municipio, rotulo,direccion)



#ds_w_pob %>% select(pob21,municipio) %>% filter(pob21>=15000) %>%count(pob21,municipio, sort = TRUE) %>%  view()
uem <- c(-3.919257897378161, 40.373942679873714)



distancias_villa <- prueba %>%select(longitud_wgs84,latitud) %>% distGeo(uem) %>% view()

prueba2<-distancias_villa%>%  mutate(distancias = round(distancias_villa/1000, digits = 2)) %>% view()



# READING AND WRITING (FILES) ---------------------------------------------




