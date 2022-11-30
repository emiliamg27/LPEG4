# TRABAJO GRUPO 4 ---------------------------------------------------------------

## LENGUAJES DE PROGRAMACIÓN ESTADÍSTICA
## PROFESOR: CHRISTIAN SUCUZHANAY AREVALO

# SHORTCUTS ---------------------------------------------------------
## ctrl + l = clean console
## ctrl + shift + r = new section

# GIT COMMANDS ------------------------------------------------------------
# git status
# git init 
# git add
# git commit -m "message"
# git push -u origin main 
# git branch Emilia
# git merge
# git remote add origin https://github.com/martadzf/LPE21535220.git
# git clone https://github.com/martadzf/LPE21535220.git
# git pull
# git fetch

# TERMINAL COMANDS --------------------------------------------------------
# ls listar 
# cd meterte en sitios
# pwd para ver donde estas
# cd .. volver atras 
# mkdir crear directorio
# touch crear archivo
# nano
# less abrir archivos
# cat abrir archivo moviendo raton
# clear
# which

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
df1<-df_source %>% janitor::clean_names() %>% type_convert(locale = locale(decimal_mark = ","))
str_squish(df1)

# CREAR NUEVAS VARIABLES ------------------------------------------------

##Clasificamos por gasolineras baratas y no baratas NUEVA COLUMNA
df2 <- df1 %>% mutate(lowcost =! rotulo %in% c("CEPSA", "REPSOL", "BP", "SHELL"))

##Precio medio del gasoleo en las CCAA
dflow %>% select(precio_gasoleo_a, idccaa, rotulo, lowcost) %>% drop_na() %>% group_by(idccaa, lowcost) %>% summarize(mean(precio_gasoleo_a)) %>% view()


write.csv(dflow,"ds21903993(33).csv")

##Columna nombre ccaa
dfccaa <- dflow%>% mutate(dflow,ccaa = ifelse (idccaa=="01","ANDALUCIA",ifelse (idccaa=="02","ARAGON", ifelse (idccaa=="03","ASTURIAS", ifelse (idccaa=="04","BALEARES", 
                                                                                          ifelse (idccaa=="05","CANARIAS",ifelse (idccaa=="06","CANTABRIA", ifelse (idccaa=="07","CASTILLA Y LEON", 
                                                                                                                                                                    ifelse (idccaa=="08","CASTILLA - LA MANCHA", ifelse (idccaa=="09","CATALUÑA", ifelse (idccaa=="10","COMUNIDAD VALENCIANA",                                                                                                                                                                                                                                                          ifelse (idccaa=="11","EXTREMADURA", ifelse (idccaa=="12","GALICIA", ifelse (idccaa=="13","MADRID", ifelse (idccaa=="14","MURCIA", ifelse (idccaa=="15","NAVARRA",
                                                                                                                                                                                                                                                                  ifelse (idccaa=="16","PAIS VASCO", ifelse (idccaa=="17","LA RIOJA",ifelse (idccaa=="18","CEUTA",ifelse (idccaa=="19","MELILLA","NA")))))))))))))))))))) %>% view()
write.csv(dfccaa,"ds21903993(34).csv")
##Gasolinera más barata
  
# READING AND WRITING (FILES) ---------------------------------------------




