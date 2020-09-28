library(lubridate)
library(tidyverse)
library(readxl)

# Parte 1 Predecir un eclipse solar

ult_eclipse <- dmy_hms("21 august 2017 18:26:40")
ult_eclipse

Synodic_month <- days(29)+hours(12)+minutes(44)+seconds(3)
Synodic_month

Saros <- Synodic_month*223
Saros

eclipse <- ult_eclipse+Saros
eclipse  

# El proximo eclipse será el dos de septiembre del 2035
# con un horario de 2 horas con 9 minutos y 49 segundos UTC


# Parte 2 agrupaciones por fecha 
df <- read_delim("data.csv", delim = ";")
View(df)
str(df)

df$`Fecha Creación`<- dmy(df$`Fecha Creación`)
View(df)

df$`Hora Creación`<- hm(df$`Hora Creación`)
View(df)

df$`Fecha Final`<- dmy(df$`Fecha Final`)
df$`Hora Final`<- hm(df$`Hora Final`)
View(df)

## En que mes existen una mayor cantidad de llamadas por codigo 

y<- df %>% 
  select(Cod,`Fecha Creación`) %>% 
  group_by(Cod, month=floor_date(`Fecha Creación`, "month")) %>% 
  summarise(conteo = n())
View(y)

y %>% 
  select(Cod,conteo) %>% 
  group_by(Cod) %>% 
  summarise(maximos = max(conteo))

# Para los codigos:
# 0, Mes7. 
#Act Info, Mes5. 
# Cancelaciones, Mes3. 
# Cobros, Mes1.
# Consultas, Mes10. 
# Empresrial, Mes10.
# Otros/Varios, Mes4. 


## Que día de la semana es el más ocupado 

df$dia <- weekdays(df$`Fecha Creación`)
View(df)

df %>% group_by(dia) %>% summarise(ocupados = n()) %>% 
  arrange(desc(ocupados))

# El día más ocupado es el Domingo

## Que més es el más ocupado 

df$mes <- months(df$`Fecha Creación`)
View(df)

df %>% group_by(mes) %>% summarise(ocup = n()) %>% 
  arrange(desc(ocup))

# El mes mas ocupado es Marzo 

## Existe una estacionalidad en la cantidad de llamadas
# No existe un camnio drástico en la estacionalidad de las llamadas
# Los meses estan bastante dispersos y no se encuentra un patrón claro

## Cuantos minutos dura la llamada promedio 

View(df)
gsubfechas <- function(date_string){
  
  return(gsub("[., ' ']","" ,date_string))
  
        
}

gsubfechas("10:18 p. m.")
  
df$`Hora Creación` <- lapply(df$`Hora Creación`, gsubfechas)
df$`Hora Final` <- lapply(df$`Hora Final`, gsubfechas)

df$`Hora Creación` <- parse_date_time(df$`Hora Creación`, '%I:%M%p')
df$`Hora Final` <- parse_date_time(df$`Hora Final`, '%I:%M%p')

df$duración <- df$`Hora Final`-df$`Hora Creación`

df$druaciónmin <- df$duración/60




mean(df$druaciónmin)
# la media es de 7.7 Minutoa 

# Crear tabla de frecuencias 

df %>% 
  group_by(druaciónmin) %>% 
  summarise(conteon = n()) %>% 
  arrange(desc(conteon))



### Función del signo Zodiacal

zodiac_sign <- function(input){
  library(lubridate)
  
  input <- ymd(as.character(as.Date(input,format="%B %d")))
  
 
  zodiac_sign_df <- data.frame(Month = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"),
                               Zodiac_Separation_Dt = c(21, 20, 21, 21, 23, 23, 23, 23, 22, 22, 20, 19),
                               LowerSideZS = c("Pisces", "Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn","Aquarius"),
                               UpperSideZS = c("Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn","Aquarius", "Pisces"),
                               stringsAsFactors = FALSE )
  

  val_df <- zodiac_sign_df[zodiac_sign_df$Month == months(input), ] 
  
  
  zodiac_sign <- ifelse( day(input) >= val_df$Zodiac_Separation_Dt, val_df$UpperSideZS, val_df$LowerSideZS)  
  
  zodiac_sign
}

zodiac_sign("March 12")

### Flights 

library(nycflights13)
View(flights)

No lo logre 

