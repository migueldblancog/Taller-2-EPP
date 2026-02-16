install.packages(c("tidyverse","fixest","modelsummary","sandwich","lmtest",
                   "AER","ivreg","did","rdrobust","rddensity","broom"))
library(tidyverse)
library(fixest)
library(modelsummary)
library(sandwich); 
library(lmtest)
library(psych)
library(dplyr)

VIF_ENDS2010 <- read.csv("https://raw.githubusercontent.com/migueldblancog/Taller-2-EPP/refs/heads/main/VIF_ENDS2010.csv")

# Creando la base de datos separada ####

df <- VIF_ENDS2010[, c(
  "qhperson",
  "Q1103AA", #Celos
  "Q1103BA", #Infiel
  "Q1103CA", #Amigo
  "Q1103DA", #Famila
  "Q1103FA", #Gastos
  "Q1104",   #Insultos
  "Q1107AB", #Empujon
  "Q1107CB", #Golpe Objeto
  "Q1107FB", #Arma
  "Q1107IB", #Sexo Forzado
  "Q1107AA", #Empujon
  "Q1107CA", #Golpe Objeto
  "Q1107FA", #Arma
  "Q1107IA"  #Sexo Forzado
)]

view(df) 
 
# Limpiando las variables y renombrando #### 
  
df$celos <- ifelse(df$Q1103AA == "Si", 1,
                     ifelse(df$Q1103AA == "No", 0, NA))

df$infiel <- ifelse(df$Q1103BA == "Si", 1,
                    ifelse(df$Q1103BA == "No", 0, NA))

df$amigos <- ifelse(df$Q1103CA == "Si", 1,
                    ifelse(df$Q1103CA == "No", 0, NA))

df$familia <- ifelse(df$Q1103DA == "Si", 1,
                     ifelse(df$Q1103DA == "No", 0, NA))

df$gastos <- ifelse(df$Q1103FA == "Si", 1,
                     ifelse(df$Q1103FA == "No", 0, NA))

df$Q1104 <- factor(
  df$Q1104,
  levels = c("Nunca", "Algunas veces", "Muchas veces"),
  ordered = TRUE
)

df$insultos <- df$Q1104

df$insultos_num <- as.numeric(df$insultos)  # Nunca=1, Algunas veces=2, Muchas veces=3


df$empujon <- ifelse(df$Q1107AB == "Si", 1,
                     ifelse(df$Q1107AB == "No", 0, NA))

df$golpe_obj <- ifelse(df$Q1107CB == "Si", 1,
                       ifelse(df$Q1107CB == "No", 0, NA))

df$arma <- ifelse(df$Q1107FB == "Si", 1,
                  ifelse(df$Q1107FB == "No", 0, NA))

df$sexo_forz <- ifelse(df$Q1107IB == "Si", 1,
                       ifelse(df$Q1107IB == "No", 0, NA))

df$empujon_completo <- ifelse(df$Q1107AA == "Si", 1,
                     ifelse(df$Q1107AA == "No", 0, NA))

df$golpe_obj_completo <- ifelse(df$Q1107CA == "Si", 1,
                       ifelse(df$Q1107CA == "No", 0, NA))

df$arma_completo <- ifelse(df$Q1107FA == "Si", 1,
                  ifelse(df$Q1107FA == "No", 0, NA))

df$sexo_forz_completo <- ifelse(df$Q1107IA == "Si", 1,
                       ifelse(df$Q1107IA == "No", 0, NA))

# Estadistica Descripitiva ####

## Celos ####

prop.table(table(df$celos))
describe(df$celos)

## Infiel #####

prop.table(table(df$infiel))
describe(df$infiel)

## Amigos ####

prop.table(table(df$amigos))
describe(df$amigos)

## Familia ####

prop.table(table(df$familia))
describe(df$familia)

## Gastos ####

prop.table(table(df$gastos))
describe(df$gastos)

## Insultos ####

table(df$insultos)
prop.table(table(df$insultos))

## Empujon ####

table(df$empujon)
prop.table(table(df$empujon))
describe(df$empujon)
prop_empujon1 <- sum(df$empujon == 1, na.rm = TRUE) / 4539
prop_empujon1

## Golpe Objeto ####

table(df$golpe_obj)
prop.table(table(df$golpe_obj))
describe(df$golpe_obj)
prop_golpe_obj <- sum(df$golpe_obj == 1, na.rm = TRUE) / 4539
prop_golpe_obj

## Arma ####

table(df$arma)
prop.table(table(df$arma))
describe(df$arma)
prop_arma <- sum(df$arma == 1, na.rm = TRUE) / 4539
prop_arma

## Sexo forzado ####

table(df$sexo_forz)
prop.table(table(df$sexo_forz))
describe(df$sexo_forz)
prop_sexo_forz <- sum(df$sexo_forz == 1, na.rm = TRUE) / 4539
prop_sexo_forz


# Alpha de Cronbach ####

  #Sacando el alpha para todos 
 # alfa_df <- alpha(df, check.keys=TRUE)
  
  #Ahora tomando solo ciertas varibles
psych::alpha(df[,c("celos","infiel","amigos","familia","gastos","insultos_num","empujon_completo","golpe_obj_completo","arma_completo","sexo_forz_completo")])
  
  #Y tomando la sugerencia
  #alpha(df[,c("JobSat1","JobSat2")])
  
# Indice ####
  
  cols <- c("celos","infiel","amigos","familia","gastos","insultos_num","empujon_completo","golpe_obj_completo","arma_completo","sexo_forz_completo")
  df <- df %>%
    mutate(across(all_of(cols), ~ case_when(
      tolower(trimws(.)) %in% c("si","sí") ~ 1L,
      tolower(trimws(.)) %in% c("no") ~ 0L,
      TRUE ~ NA_integer_   # no sabe / no responde
    ))) %>%
    mutate(
      indice_violencia_sum = rowSums(across(all_of(cols)), na.rm = TRUE),
      indice_violencia_0a1 = indice_violencia_sum / length(cols),
      indice_violencia_0a100 = 100 * indice_violencia_0a1
    )
  
  # Descriptivos
  summary(df$indice_violencia_0a100)
  table(df$indice_violencia_sum, useNA="ifany")
  
  
  cols <- c("celos","infiel","amigos","familia","gastos",
                "empujon_completo","golpe_obj_completo","arma_completo","sexo_forz_completo")
  
  df <- df %>%
    #mutate(
     #across(all_of(cols),
      #       ~ case_when(
     #          str_to_lower(str_trim(as.character(.))) %in% c("si","sí") ~ 1L,
      #         str_to_lower(str_trim(as.character(.))) == "no" ~ 0L,
       #        TRUE ~ NA_integer_
        #     )),
    #  insultos_prev = case_when(
     #   insultos_num %in% c(2L,3L) ~ 1L,
      #  insultos_num == 1L ~ 0L,
       # TRUE ~ NA_integer_
  #    )
   # ) %>%
    mutate(
      indice_prev_sum = rowSums(pick(all_of(c(cols, "insultos_num"))), na.rm = TRUE),
      indice_prev_0a1 = indice_prev_sum / length(c(cols, "insultos_num")),
      indice_prev_0a100 = 100 * indice_prev_0a1
    )
  
  