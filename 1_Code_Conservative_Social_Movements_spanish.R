#Codigo Reproducible y comentado

#limpiar ambiente de trabajo
rm(list=ls())

# Cargar Materiales -------------------------------------------------------


#Paquetes
library(sjmisc)
library(car)
library(dplyr)
library(ggplot2)
library(survey)
library(sjPlot)
library(panelr)
library(corrplot)
library(lme4)
library(texreg)
library(GLMMadaptive)
library(tidyverse)
library(sticky)

#Función de extracción de variables
extract.mm <- function(model) {
  s <- summary(model)
  
  beta <- s$coef_table
  beta.names <- as.character(rownames(s$coef_table))
  coef <- as.numeric(beta[, 1])
  se <- as.numeric(beta[, 2])
  pval <- as.numeric(beta[, 4])
  
  var_mu <- model$D
  AIC <- AIC(model)
  BIC <- BIC(model)
  logLik <- s$logLik
  N_obs <- s$N
  N_grs <- length(levels(unlist(model$id)))
  
  l=createTexreg(coef.names=beta.names, 
                 coef=coef, se=se, pvalues=pval,
                 gof.names = c("Sigma2", "AIC", "BIC", "Log Likelihood",
                               "Num. obs.", "Num respondents"), 
                 gof = c(var_mu, AIC, BIC, logLik, N_obs, N_grs),
                 gof.decimal = c(TRUE, TRUE,TRUE, TRUE, FALSE, FALSE)) 
  
}



#Cargar base de datos desde el repositorio de Dataverse
load(url("https://dataverse.harvard.edu/api/access/datafile/4606527"))


# Transformación de Base de datos -----------------------------------------


# Pasar base WIDE a formato LONG:
elsoc_long.0 <- elsoc_wide_2016_2019 %>%
  pivot_longer( cols = contains('_w'), names_to = c('.value', 'ola'), names_sep = '_w')

# Copiar etiquetas de valores desde base wide para traspasar a base long (se pierden al pivotear base):
elsoc_aux <- elsoc_wide_2016_2019 %>% select(!contains('_w') | contains('_w04'))  # Base auxiliar con nombres de variables igual a formato long para copiar labels
names(elsoc_aux) <- str_replace_all(names(elsoc_aux), c("_w04" = ""))

# Traspasar etiquetas de base wide a base long
elsoc_long.1 <- sjlabelled::copy_labels(df_origin = elsoc_aux, df_new = elsoc_long.0)

# Agregar etiqueta a ola (que queda sin etiqueta porque no contiene _w0 en su nombre) OJO ESTE COMANDO NO FUNCIONA
#elsoc_long.1$ola <- sjlabelled::set_labels(elsoc_long.1$ola, 
#                                           labels = c('01' = "2016", '02' = "2017", '03' = "2018", '04' = "2019"))

elsoc_long.1$ola<-factor(car::recode(elsoc_long.1$ola, "'01' = '2016'; '02' = '2017'; '03' = '2018'; '04' = '2019'"),)

attr(elsoc_long.1$ola, which = 'label') <- 'Ola de encuesta'

# Las etiquetas de base wide tienen el año de la ola, y como se copiaron de ahí, hay que quitarlas 
var_labels <- trimws(str_replace_all(sjlabelled::get_label(elsoc_long.1), 
                                     c('\\(2016\\)' = '',
                                       '\\(2017\\)' = '',
                                       '\\(2018\\)' = '', 
                                       '\\(2019\\)' = '' )))
elsoc_long.2 <- sjlabelled::set_label(elsoc_long.1, var_labels)

# Eliminar observaciones que no existen, pero que se crean al pasar a base long (por ejemplo, observaciones en olas 1 y 2 de muestra 2)
# Se usa la variable segmento porque todas las observaciones tienen segmento definido
elsoc_long.3 <- elsoc_long.2 %>% filter(!is.na(segmento))

# Reconocer -888 y -999 como NAs eliminar este proceso
#elsoc_long.4 <- sjlabelled::set_na(elsoc_long.3, na = c(-888, -999))

### Factorizar todas las variables posibles ###
# Porque para ggplot es mucho más conveniente que variables sean factores en vez de variables numéricas

# Creo una función ad-hoc que pasa variables a factor, reciclando las etiquetas de variables numéricas a etiquetas de factores
# Se filtarn casos de variables que tienen demasiados valores posibles (más de 5) o que no tienen etiquetas
pasar_a_factor <- function(var) {
  if ((length(na.omit(unique(var))) <= 5 )  & 
      length(na.omit(unique(var))) == length(attr(var, which = 'labels')) & 
      class(var) != 'factor') { 
    return(factor(var, labels = names(attr(var, which = 'labels'))))
  } else {
    return(var)
  }
}

# Aplicar función recien definida a cada variable de base long:
elsoc_long.5 <- lapply(elsoc_long.3, pasar_a_factor) %>% data.frame()

# Volver a poner etiquetas, porque se pierden con al función y listo:
elsoc_long_2016_2019 <- sjlabelled::set_label(elsoc_long.5, var_labels) %>%
  dplyr::filter(!is.na(ponderador02)) # corregir problema que se genera al pivotear en 1 observacion


# Conservar datos relevantes
rm(list = setdiff(ls(), c('elsoc_long_2016_2019')))

#save(elsoc_long_2016_2019, file = 'elsoc_long.RData')

# Recodificación de variables ---------------------------------------------

#Recodificación de movimientos sociales

#recuperar casos otros.
elsoc_long_2016_2019$c20_otrotemp<- car::recode(elsoc_long_2016_2019$c20_otro,
                                                "'Aborto'=6;
'Acceso A Salud Igualitaria'=11;
'Animalista'=3;
'Anti Maltrato Animal'=3;
'Apoyar A Los Enfermos'=11;
'Apoyo A Las Ciencias Economico'=11;
'Apoyo A Ninnos Vulnerables'=11;
'Aumetar El Sueldo Minimo'=2;
'Ayuda A Indigente'=11;
'Ayuda A Los Discapacitados'=11; 
'Barra De Futbol Los Panzers'=11; 
'Cambio En La Salud Publica'=11; 
'Cambio Sename Mas Control'=11;
'Colegio De Profesores De Chile Y Metropolitano.'=1;
'Comite De Vivienda'=11; 
'Consejo Municipal De Salud'=11; 
'Contra El Cobro Excesivo En Autopistas'=11; 
'Contra Las Afp'=9;
'Damas De Amarillo'=11; 
'De Apoyo A La Causa Infantil'=11; 
'De La Igual De Genero'=8;
'De Los Pensionados'=9;
'Defensa Del Rio Cautin, Por La Instalacion De Represa.'=3;
'Dirigente Vecinal'=11;
'Discapacidad'=11;
'El Del Respeto Por Las Personas'=11; 
'En Apoyo A La Familia'=6;
'Fundacion Derribando Barreras'=11; 
'Ideal Mas Justo Para Las Personas'=11;
'Iglesia Evangelica.'=11;
'Igualdad De Inclicion Para Personas Con Discapacidad'=11;
'Indigenas'=4;
'Islita Dialoga'=11; 
'Junta De Vecinos'=11;
'La Afp'=9;
'Legalidad De La Marihuana'=11; 
'Legalizacion De Marihuana Para Uso Medicinal Y Recreativo'=11; 
'Ley De Autismo'=11;
'Mejor Salud'=11;
'Metrotren A Esta Comuna'=11; 
'Movimiento Animalista'=3;
'Movimiento Anti Afp'=9;
'Movimiento Cambio Al Sistema De Salud'=11; 
'Movimiento Cuidadores'=11;
'Movimiento De Defender Los Animales'=3; 
'Movimiento De Integracion A Los Discapacitados'=11; 
'Movimiento De Mejora En La Salud'=11;
'Movimiento Derechos De Las Mujeres'=8;
'Movimiento Mejora De Sistema Previsional'=9; 
'Movimiento No Mas Afp'=9;
'Movimiento Por Aborto'=8;
'Movimiento Por Causa De Las Pensiones De Vejez'=9; 
'Movimiento Por Eliminacion De Afp'=9;
'Movimiento Por La Tercera Edad'=11;
'Movimiento Por Los Derechos De La Infancia'=11; 
'Movimiento Por Los Derechos De La Primera Infancia'=11; 
'Movimiento Por Tener El Derecho A Elegir'=8;
'Movimiento Proteccion Adulto Mayor Y Ninnos'=11; 
'Movimiento Social Anti Afp'=9;
'Movimiento Social En Contra Del Trafico De Drogas.(Pasta Base).'=11; 
'Movimiento Social No Mas Afp'=9;
'Movimiento Social Por El Cambio Del Sistema De Salud'=11; 
'Movimiento Social Por El Respeto E Igualdad A La Mujer'=8;
'Movimiento Social Por La Salud'=11;
'Movimientos De Campesinos Y Campesinos Por El Buen Vivir'=11; 
'Moviminto Feminista'=8; 
'Musicales, Talleres, Deportivos'=11; 
'Ni Una Menos'=8; 
'Ni Una Menos, Antifemicidio'=8; 
'Ni Una Menos, Antifeminicidio'=8; 
'Ninnos Vulnerables Y En Riesgo Social'=11; 
'No +Afp'=9;
'No A La Baura De Temuco'=11; 
'No A Las Afp'=9;
'No Mas Afp'=9; 
'No Permitir Muertes De Carabineros A Manos De Delincuentes'=7;
'Organizacion De Adulto Mayor'=11;
'Para La Vivienda'=11;
'Pensiones Afp'=9;
'Por La Salud'=11;
'Pro Aborto'=8;
'Pro Aborto, Apoya Al Aborto De Las 3 Causas Del Parlamento'=8; 
'Proteccion A Los Ninnos'=11;
'Protencion De Los Animales'=3;
'Religioso'=11;
'Representantes Ley Emilia'=11;
'Salud'=11;
'Salud Atencion Y Enfermedades Raras'=11; 
'Sename Ninnos'=11;
'Servicio Social De Ayuda A Los Pobres'=11; 
'Sistema Salud Publica Mala Atencion Demasiada Demanda'=11; 
'Social De Iglesia. Ayuda A Personas Neceaitadas'=11;
'Todas'=11;
'Tuning'=11;
'Violencia A La Mujer'=8; 
'Violencia De Genero'=8;''=-999")

#reemplazar
elsoc_long_2016_2019$c20_rec<-ifelse(elsoc_long_2016_2019$c20_otrotemp==-999,elsoc_long_2016_2019$c20,elsoc_long_2016_2019$c20_otrotemp)

#Codificar a movimientos sociales agregados
elsoc_long_2016_2019$movsoc<- car::recode(elsoc_long_2016_2019$c20_rec, 
                                          "1='Progresista';2='Progresista';3='Progresista';
                                          4='Progresista';5='Progresista';6='Conservador';7='Conservador';
                                          8='Progresista';9='Progresista';10='Progresista';11=NA;12='Ninguno'")

# Recodificar a variable dependiente definitiva
elsoc_long_2016_2019$conserva <- car::recode(elsoc_long_2016_2019$movsoc, "'Conservador'=1;c('Progresista')=0;c('Ninguno')=NA;c(-888,-999)=NA", as.factor = T)


#Recodificación a posición política: OJO NS/NR se codifica dentro de las respuestas validas
elsoc_long_2016_2019$pos_id  <- factor(car::recode(elsoc_long_2016_2019$c15,"c(11,12,-888,-999)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                       levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

#Recodificación de variable de autoritarismo

#pasar a variable de numeros enteros
elsoc_long_2016_2019$c18_04n<-as.integer(car::recode(elsoc_long_2016_2019$c18_04, "1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))
elsoc_long_2016_2019$c18_05n<-as.integer(car::recode(elsoc_long_2016_2019$c18_05, "1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))
elsoc_long_2016_2019$c18_06n<-as.integer(car::recode(elsoc_long_2016_2019$c18_06, "1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))
elsoc_long_2016_2019$c18_07n<-as.integer(car::recode(elsoc_long_2016_2019$c18_07, "1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))

#REVISAR CALCULO DE PROMEDIOS: esta recodificación genera el promedio con los casos disponibles.
elsoc_long_2016_2019$auto_num<- round(rowMeans(elsoc_long_2016_2019[c("c18_04n","c18_05n","c18_06n","c18_07n")],na.rm = T),0)
summary(elsoc_long_2016_2019$c18_04n)
summary(elsoc_long_2016_2019$c18_05n)
summary(elsoc_long_2016_2019$c18_06n)
summary(elsoc_long_2016_2019$c18_07n)
summary(elsoc_long_2016_2019$auto_num) #33 casos perdidos

#Educación
elsoc_long_2016_2019$educ <- car::recode(elsoc_long_2016_2019$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4; c(-888,-999)=NA")
elsoc_long_2016_2019$educ <- factor(elsoc_long_2016_2019$educ,labels = c("Basica","Media","Tecnica","Universitaria"))

#Recodificación de educación a años en base a casen 2017
elsoc_long_2016_2019$educ_n<- car::recode(elsoc_long_2016_2019$m01, "1=0;2=4.3;3=7.5;4=9.8;5=12.02;6=13.9;7=14.8;8=14.9;9=16.9;10=19.07;c(-888,-999)=NA", as.numeric = T)
class(elsoc_long_2016_2019$educ_n)
summary(elsoc_long_2016_2019$educ_n)
frq(elsoc_long_2016_2019$educ_n)

#Imputar punto medio de rangos de ingreso

elsoc_long_2016_2019$m30_rec <- as.numeric(car::recode(elsoc_long_2016_2019$m30,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA;c(-888,-999)=NA"))
elsoc_long_2016_2019$m29_rec <- as.numeric(car::recode(elsoc_long_2016_2019$m29,"c(-888,-999)=NA"))


elsoc_long_2016_2019$m29_imp <- ifelse(!is.na(elsoc_long_2016_2019$m29_rec), elsoc_long_2016_2019$m29_rec, elsoc_long_2016_2019$m30_rec)

# Deflactar a precios de cada año:
elsoc_long_2016_2019$deflactor <- with(elsoc_long_2016_2019, case_when(
  ola == 2016 ~ 113.88/123.82,
  ola == 2017 ~ 116.46/123.82,
  ola == 2018 ~ 119.45/123.82,
  ola == 2019 ~ 123.82/123.82
))

# N hogar:
elsoc_long_2016_2019$n_hogar <- with(elsoc_long_2016_2019, case_when(
  ola == 2016 ~ nhogar1,
  ola == 2017 ~ m46_nhogar,
  ola == 2018 ~ m54,
  ola == 2019 ~ m54
))

#Aplicar nsnr a NA
elsoc_long_2016_2019$n_hogar_r<-car::recode(elsoc_long_2016_2019$n_hogar,"c(-888,-999)=NA")

# Ingreso per capita del hogar:
elsoc_long_2016_2019$ing_pc <- elsoc_long_2016_2019$m29_imp/elsoc_long_2016_2019$n_hogar_r

elsoc_long_2016_2019 <- elsoc_long_2016_2019 %>% 
  group_by(ola) %>% 
  mutate(quintil = ntile(-desc(ing_pc), 5)) %>% 
  ungroup()

elsoc_long_2016_2019$quintil <- factor(elsoc_long_2016_2019$quintil,
                                       levels = c(1, 2, 3, 4, 5),
                                       labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')) # Quintiles como factores

#incluir nueva categoría de quintil con casos perdidos.
elsoc_long_2016_2019$quintil1<-car::recode(elsoc_long_2016_2019$quintil, " 'Q1'='Q1'; 'Q2'= 'Q2'; 'Q3'='Q3'; 'Q4'='Q4';   'Q5'='Q5'; NA='QNA'")


with(elsoc_long_2016_2019,table(quintil1,ola))

#Creo que el cambio social es posible
frq(elsoc_long_2016_2019$c18_08)
elsoc_long_2016_2019$cambioSocial<-as.integer(car::recode(elsoc_long_2016_2019$c18_08, "1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))

#Participación en marchas
elsoc_long_2016_2019$marcha<-as.integer(car::recode(elsoc_long_2016_2019$c08_02,"1=1;2=2;3=3;4=4;5=5; c(-999,-888)=NA" ))

#Edad en tramos
elsoc_long_2016_2019$edad <- factor(car::recode(elsoc_long_2016_2019$m0_edad, "18:29=1;30:49=2;50:64=3;65:150=4"),
                                    labels = c('18-29', '30-49', '50-64', '65 o más'))
elsoc_long_2016_2019$edad <- sjlabelled::set_label(elsoc_long_2016_2019$edad, label = c("Edad en Tramos")) 

#Religión
rellab = c("Catolico","Evangelico","Otra Religion","no religioso")
elsoc_long_2016_2019$religid<-factor(Recode(elsoc_long_2016_2019$m38,"1=1;2=2;3:6=3;7:9=4;-999:-888=4"),labels=rellab)

#Apego al regimen democratico
elsoc_long_2016_2019$democracia <- car::recode(elsoc_long_2016_2019$c25,
                                               "1='Democrata';
                                               2='Autoritario';
                                               c(3,4)='Indiferente';
                                               c(-888,-999)='Indiferente'",as.factor = T)

table(elsoc_long_2016_2019$democracia)

#grado de acuerdo de cambiar la constitución
elsoc_long_2016_2019$cont<-as.integer(car::recode(elsoc_long_2016_2019$c28,"1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))

#Social Dominance
elsoc_long_2016_2019$c18_01n<-as.integer(car::recode(elsoc_long_2016_2019$c18_01,"1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))

#Estos dos itesm de SDO equalitarism se recodifican inversamente
elsoc_long_2016_2019$c18_02nr<-as.integer(car::recode(elsoc_long_2016_2019$c18_02,"1=5;2=4;3=3;4=2;5=1;c(-999,-888)=NA"))
elsoc_long_2016_2019$c18_03nr<-as.integer(car::recode(elsoc_long_2016_2019$c18_03,"1=5;2=4;3=3;4=2;5=1;c(-999,-888)=NA"))
#elsoc_long_2016_2019$c18_12n<-as.integer(car::recode(elsoc_long_2016_2019$c18_12,"1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA")) No se encuentra disponible para 2016; table(elsoc_long_2016_2019$c18_12, elsoc_long_2016_2019$ola)
#elsoc_long_2016_2019$c18_02n<-as.integer(car::recode(elsoc_long_2016_2019$c18_02,"1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))
#elsoc_long_2016_2019$c18_03n<-as.integer(car::recode(elsoc_long_2016_2019$c18_03,"1=1;2=2;3=3;4=4;5=5;c(-999,-888)=NA"))

#elsoc_long_2016_2019$sdo_equidadO <- round(rowMeans(elsoc_long_2016_2019[c("c18_02n","c18_03n")],na.rm = T),0)
elsoc_long_2016_2019$sdo_equidad <- round(rowMeans(elsoc_long_2016_2019[c("c18_02nr","c18_03nr")],na.rm = T),0)

summary(elsoc_long_2016_2019$sdo_equidad) #DEJAR ESTE
#summary(elsoc_long_2016_2019$sdo_equidadO) #DEJAR ESTE

# Centrado y variables en t1 ----------------------------------------------


#autoritarismo
elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(autogroup=mean(auto_num,na.rm=T))
elsoc_long_2016_2019$auto_cwc<- elsoc_long_2016_2019$auto_num-elsoc_long_2016_2019$autogroup 
summary(elsoc_long_2016_2019$auto_cwc)
summary(elsoc_long_2016_2019$autogroup)

#creencia en cambio social
elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(cambiosocialgroup=mean(cambioSocial,na.rm=T))
elsoc_long_2016_2019$cambioSocial_cwc<- elsoc_long_2016_2019$cambioSocial-elsoc_long_2016_2019$cambiosocialgroup 
summary(elsoc_long_2016_2019$cambioSocial_cwc)
summary(elsoc_long_2016_2019$cambiosocialgroup)


#frecuencia de participación en marchas
elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(marchagroup=mean(marcha,na.rm=T))
elsoc_long_2016_2019$marcha_cwc<- elsoc_long_2016_2019$marcha-elsoc_long_2016_2019$marchagroup 
summary(elsoc_long_2016_2019$marcha_cwc)
summary(elsoc_long_2016_2019$marchagroup)



#Nivel de confianza en el presidente
#elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(presidentegroup=mean(presidente,na.rm=T))
#elsoc_long_2016_2019$presidente_cwc<- elsoc_long_2016_2019$presidente-elsoc_long_2016_2019$presidentegroup 

#grado de acuerdo de cambiar la constitución
#elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(contgroup=mean(cont,na.rm=T))
#elsoc_long_2016_2019$cont_cwc<- elsoc_long_2016_2019$cont-elsoc_long_2016_2019$contgroup 
#summary(elsoc_long_2016_2019$cont_cwc)
#summary(elsoc_long_2016_2019$contgroup)

#sdo dominancia
#elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(sdo_dominanciagroup=mean(sdo_dominancia,na.rm=T))
#elsoc_long_2016_2019$sdo_dominancia_cwc<- elsoc_long_2016_2019$sdo_dominancia-elsoc_long_2016_2019$sdo_dominanciagroup 
#summary(elsoc_long_2016_2019$sdo_dominancia_cwc)
#summary(elsoc_long_2016_2019$sdo_dominanciagroup)


#sdo equidad
elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(sdo_equidadgroup=mean(sdo_equidad,na.rm=T))
elsoc_long_2016_2019$sdo_equidad_cwc<- elsoc_long_2016_2019$sdo_equidad-elsoc_long_2016_2019$sdo_equidadgroup 
summary(elsoc_long_2016_2019$sdo_equidad_cwc)
summary(elsoc_long_2016_2019$sdo_equidadgroup)



#pos_id
elsoc_long_2016_2019$centro<-car::recode(elsoc_long_2016_2019$pos_id, "'Centro'=1; c('Izquierda','Derecha','No se identifica')=0")
elsoc_long_2016_2019$derecha<-car::recode(elsoc_long_2016_2019$pos_id, "'Derecha'=1; c('Izquierda','Centro','No se identifica')=0")
elsoc_long_2016_2019$noId<-car::recode(elsoc_long_2016_2019$pos_id, "'No se identifica'=1; c('Izquierda','Derecha','Centro')=0")


elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(centrogroup=mean(as.integer(centro),na.rm=T))
elsoc_long_2016_2019$centro_cwc<- as.integer(elsoc_long_2016_2019$centro)-elsoc_long_2016_2019$centrogroup 

elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(derechagroup=mean(as.integer(derecha),na.rm=T))
elsoc_long_2016_2019$derecha_cwc<- as.integer(elsoc_long_2016_2019$derecha)-elsoc_long_2016_2019$derechagroup

elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(noIdgroup=mean(as.integer(noId),na.rm=T))
elsoc_long_2016_2019$noId_cwc<- as.integer(elsoc_long_2016_2019$noId)-elsoc_long_2016_2019$noIdgroup 
summary(elsoc_long_2016_2019$noId_cwc)

#democracia

elsoc_long_2016_2019$democrata<-car::recode(elsoc_long_2016_2019$democracia,"'Democrata'=1;c('Autoritario','Indiferente')=0")
elsoc_long_2016_2019$indiferente<-car::recode(elsoc_long_2016_2019$democracia,"'Indiferente'=1;c('Autoritario','Democrata')=0")


elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(democratagroup=mean(as.integer(democrata),na.rm=T))
elsoc_long_2016_2019$democrata_cwc<- as.integer(elsoc_long_2016_2019$democrata) -elsoc_long_2016_2019$democratagroup

elsoc_long_2016_2019<- elsoc_long_2016_2019 %>% group_by(idencuesta) %>% mutate(indiferentegroup=mean(as.integer(indiferente),na.rm=T))
elsoc_long_2016_2019$indiferente_cwc<- as.integer(elsoc_long_2016_2019$indiferente)-elsoc_long_2016_2019$indiferentegroup 


#Construcción de variables fijadas a t1
elsoc_long_2016_2019$educ_t1 <- elsoc_long_2016_2019$educ[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] 
elsoc_long_2016_2019$sexo_t1 <- elsoc_long_2016_2019$m0_sexo[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] 
elsoc_long_2016_2019$edad_t1 <- elsoc_long_2016_2019$edad[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] 
elsoc_long_2016_2019$religid_t1 <- elsoc_long_2016_2019$religid[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] 
elsoc_long_2016_2019$quintil_t1 <- elsoc_long_2016_2019$quintil1[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] 
elsoc_long_2016_2019$educ_n_t1 <- as.numeric(elsoc_long_2016_2019$educ_n[elsoc_long_2016_2019$ola==2016][as.factor(elsoc_long_2016_2019$idencuesta)] )



#fin de recodificación

# Tabla 1: Descriptivos ---------------------------------------------------
options(OutDec= ".")
elsoc_long_2016_2019$gender<- as.integer(elsoc_long_2016_2019$m0_sexo)-1

#autoritarismo equidad y educacion
x<-t(aggregate(elsoc_long_2016_2019[c("auto_num","sdo_equidad","educ_n","gender",
                                      "cont","cambioSocial","marcha")],
               list(elsoc_long_2016_2019$ola), mean, na.rm=T))
t(aggregate(elsoc_long_2016_2019[c("auto_num","sdo_equidad","educ_n","gender",
                                   "cont","cambioSocial","marcha")],
            list(elsoc_long_2016_2019$ola), sd, na.rm=T))

#religion
round(with(elsoc_long_2016_2019,prop.table(table(religid,ola),2)),3)*100 

#ingreso
y<-round(with(elsoc_long_2016_2019,prop.table(table(quintil1,ola),2)),3)*100
#Posisción política
w<-round(with(elsoc_long_2016_2019,prop.table(table(pos_id,ola),2)),3)*100
#democracia
z<-round(with(elsoc_long_2016_2019,prop.table(table(democracia,ola),2)),3)*100

rbind(x,y,w,z)

#sacar las desviaciones



# Preambulo de modelos ----------------------------------------------------

#Filtro por atrición y casos anomalos
els_l<-elsoc_long_2016_2019 %>% filter(tipo_atricion<=7)




#Seleccionar variables para el analisis
els_l <-  els_l %>% select(conserva,ola,idencuesta,
                           sexo_t1,educ_n_t1,edad_t1,religid_t1,quintil_t1,
                           centro_cwc,derecha_cwc,noId_cwc,democrata_cwc,indiferente_cwc,auto_cwc,cambioSocial_cwc,marcha_cwc,sdo_equidad_cwc,
                           derechagroup,centrogroup,noIdgroup,democratagroup,indiferentegroup,autogroup,cambiosocialgroup,marchagroup,marchagroup,
                           sdo_equidadgroup)
els1<-els_l %>% na.omit() #base con casos completos

# Tabla 2: Modelos nulos con casos completos --------------------------------------------------

m01c<-glmer(conserva~ (1|idencuesta),family=binomial,data=els1)
(vc<-VarCorr(m01c))
print(vc,comp=c("Variance"),digits=5) #1.3044
tau2<-1.3044

# computing the ICC for the intercept
ICC1 <- tau2 / (tau2 + (pi^2 / 3) )
ICC1

m02c<-glmer(conserva~ as.numeric(ola) + (1|idencuesta),family=binomial,data=els1)

## Random slope model
m03c<-glmer(conserva~ as.numeric(ola) + (1 + as.numeric(ola)|idencuesta),family=binomial,data=els1)

#Ola como variable categorica
m04c<-glmer(conserva~ as.factor(ola) + (1|idencuesta),family=binomial,data=els1)


anova(m02c, m03c) ## Indica variabilidad en pendiente aleatoria

screenreg(c(m01c,m02c,m03c,m04c), single.row = F)

#stargazer::stargazer(list(m01c,m02c,m03c,m04c),type = "html",single.row = F,
# out="Tabla_2_modelos_nulos.doc")


# Tabla 3: Modelos sustantivos con casos completos --------------------------------------------


#modelos de recurso
m2c<- mixed_model(fixed = conserva ~as.factor(ola) + 
                    as.factor(sexo_t1)+ educ_n_t1 + edad_t1 +religid_t1+quintil_t1 , random = ~1|idencuesta, data=els1,
                  family=binomial(), nAGQ=3)
## Modelo 3 actitudinales (within) sólo con sdo equidad
m3c<- mixed_model(fixed = conserva ~as.factor(ola) + 
                    as.factor(sexo_t1)+ educ_n_t1 + edad_t1 +religid_t1 +quintil_t1+
                    centro_cwc+derecha_cwc+noId_cwc +  democrata_cwc+indiferente_cwc+auto_cwc+cambioSocial_cwc+marcha_cwc+sdo_equidad_cwc,
                  random = ~1|idencuesta, data=els1,
                  family=binomial(), nAGQ=3)
## Modelo 6 actitudinales (between) solos sdo equidad
m6c<- mixed_model(fixed = conserva ~as.factor(ola) + 
                    as.factor(sexo_t1)+ educ_n_t1 + edad_t1 +religid_t1 +quintil_t1+
                    derechagroup+centrogroup+noIdgroup+democratagroup+indiferentegroup+autogroup+cambiosocialgroup+marchagroup+marchagroup+
                    sdo_equidadgroup,
                  random = ~1|idencuesta, data=els1,
                  family=binomial(), nAGQ=3)
## Modelo 9 full sdo equidad
m9c<- mixed_model(fixed = conserva ~as.factor(ola) + 
                    as.factor(sexo_t1)+ educ_n_t1 + edad_t1 +religid_t1 +quintil_t1+
                    centro_cwc+derecha_cwc+noId_cwc +  democrata_cwc+indiferente_cwc+auto_cwc+cambioSocial_cwc+marcha_cwc++sdo_equidad_cwc+
                    derechagroup+centrogroup+noIdgroup+democratagroup+indiferentegroup+autogroup+cambiosocialgroup+marchagroup+marchagroup+
                    sdo_equidadgroup,
                  random = ~1|idencuesta, data=els1,
                  family=binomial(), nAGQ=3)

screenreg(c(extract.mm(m2c),extract.mm(m3c),extract.mm(m6c),extract.mm(m9c)), single.row = T)


