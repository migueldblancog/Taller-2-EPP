install.packages(c("tidyverse","fixest","modelsummary","sandwich","lmtest",
                   "AER","ivreg","did","rdrobust","rddensity","broom"))
library(tidyverse)
library(readr)
library(fixest)
library(modelsummary)
library(sandwich); 
library(lmtest)
library(psych)
library(dplyr)
library(ggplot2)
library(purrr)
library(stargazer)



VIF_ENDS2010 <- read.csv("https://raw.githubusercontent.com/migueldblancog/Taller-2-EPP/refs/heads/main/VIF_ENDS2010.csv")

# Creando la base de datos separada ####

df <- VIF_ENDS2010[, c(
  "QHWLTHI5",#Indice de riqueza
  "QH03",  #edad declarada
  "Q1103AA", #Celos
  "Q1103BA", #Infiel
  "Q1103CA", #Amigo
  "Q1103DA", #Famila
  "Q1103FA", #Gastos
  "Q1104",   #Insultos
  "Q1107AB", #Empujon
  "Q1107BB", #Golpe Mano ## variable de convergencia
  "Q1107CB", #Golpe Objeto
  "Q1107FB", #Arma
  "Q1107IB", #Sexo Forzado
  "Q1107AA", #Empujon
  "Q1107BA", #Golpe Mano ## variable de convergencia
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

df$golpe_mano <- ifelse(df$Q1107BB == "Si", 1,
                       ifelse(df$Q1107BB == "No", 0, NA))

df$golpe_obj <- ifelse(df$Q1107CB == "Si", 1,
                       ifelse(df$Q1107CB == "No", 0, NA))

df$arma <- ifelse(df$Q1107FB == "Si", 1,
                  ifelse(df$Q1107FB == "No", 0, NA))

df$sexo_forz <- ifelse(df$Q1107IB == "Si", 1,
                       ifelse(df$Q1107IB == "No", 0, NA))

df$empujon_completo <- ifelse(df$Q1107AA == "Si", 1,
                     ifelse(df$Q1107AA == "No", 0, NA))

df$golpe_mano_completo <- ifelse(df$Q1107BA == "Si", 1,
                                ifelse(df$Q1107BA == "No", 0, NA))

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

## Tabla combinada ####

var <- c("celos","infiel","amigos","familia","gastos",
         "insultos","empujon","golpe_obj","arma","sexo_forz")

summary_stats <- psych::describe(df[var])
print(summary_stats)

library(officer)
library(flextable)

ft <- flextable(summary_stats) |> autofit()

doc <- read_docx() |>
  body_add_par("Estadísticas descriptivas", style = "heading 1") |>
  body_add_flextable(ft)

print(doc, target = "/Users/miguelblanco/Library/CloudStorage/OneDrive-Personal/Materias Uniandes/2026-10/Evaluacion de Politicas Publicas/Semana 4/summary_stats.docx")


# Alpha de Cronbach ####

  #Sacando el alpha para todos 
 # alfa_df <- alpha(df, check.keys=TRUE)
  
  #Ahora tomando solo ciertas varibles
psych::alpha(df[,c("celos","infiel","amigos","familia","gastos","insultos_num","empujon_completo","golpe_obj_completo","arma_completo","sexo_forz_completo")])
  
  #Y tomando la sugerencia
  #alpha(df[,c("JobSat1","JobSat2")])
  
# Indice ####
 ## Indice de prevalencia ##### 

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
      indice_prev_sum = rowSums(pick(all_of(c(cols))), na.rm = TRUE),
      indice_prev_0a1 = indice_prev_sum / length(c(cols)),
      indice_prev_0a100 = 100 * indice_prev_0a1
    )
 
  
  ### Estadistica descriptiva ####
  describe(df$indice_prev_0a100)

  boxplot(df$indice_prev_0a100)
  boxplot(df$indice_prev_0a1)
  boxplot(df$indice_prev_sum)
  
  hist(df$indice_prev_0a100)
  
  ggplot(df, aes(x = indice_prev_0a100)) +
    geom_bar(fill = "#C44E52") +
    labs(
      x = "Índice de prevalencia (0–100)",
      y = "Frecuencia"
    ) +
    theme_minimal()
  
  library(ggplot2)
  
  library(ggplot2)
  
  ggplot(df, aes(x = indice_prev_0a100)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 10,
      fill = "grey80",
      color = "black",
      na.rm = TRUE
    ) +
    geom_density(
      color = "#C44E52",
      linewidth = 1.2,
      na.rm = TRUE
    ) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(
      title = "Distribución del índice de prevalencia (0–100)",
      x = "Índice de prevalencia",
      y = "Densidad"
    ) +
    theme_minimal()
  
  library(dplyr)
  library(tidyr)
 
  vars <- c(
    "celos",
    "infiel",
    "amigos",
    "familia",
    "gastos",
    "empujon_completo",
    "golpe_obj_completo",
    "arma_completo",
    "sexo_forz_completo"
  )
  
   
  df_sum <- df %>%
    summarise(across(all_of(vars), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(
      cols = everything(),
      names_to = "categoria",
      values_to = "total"
    )
  
  
  ggplot(df_sum, aes(x = reorder(categoria, total), y = total)) +
    geom_bar(stat = "identity", fill = "#4C72B0") +
    coord_flip() +
    labs(
      title = "Prevalencia de distintas formas de violencia",
      x = "",
      y = "Número de personas"
    ) +
    theme_minimal()
  
## Indice ponderado ####
  vars_2 <- c(
    "celos",
    "infiel",
    "amigos",
    "familia",
    "gastos",
    "insultos_num",
    "empujon_completo",
    "golpe_obj_completo",
    "arma_completo",
    "sexo_forz_completo"
  )
  
  pesos <- c(
    0.10,  # celos
    0.06,  # infiel
    0.06,  # amigos
    0.06,  # familia
    0.06,  # gastos
    0.12,  # insultos_num
    0.10,  # empujon
    0.10,  # golpe_obj
    0.19,  # arma
    0.15   # sexo_forz
  )
  
  sum(pesos)  # debe ser 1
  
  X <- as.matrix(df[, vars_2])
  
  df$indice_ponderado <- X %*% pesos
  ### Estadisticas descriptivas ####
  describe(df$indice_ponderado)
  
  
  ggplot(df, aes(x = indice_ponderado)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 20,
      fill = "grey80",
      color = "black",
      na.rm = TRUE
    ) +
    geom_density(
      color = "#C44E52",
      linewidth = 1.2,
      na.rm = TRUE
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    labs(
      title = "Distribución del índice ponderado",
      x = "Índice ponderado",
      y = "Densidad"
    ) +
    theme_minimal()
  
  ggplot(df, aes(y = indice_ponderado)) +
    geom_boxplot(
      fill = "grey80",
      color = "black",
      width = 0.25,
      outlier.color = "black"
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Distribución del índice ponderado",
      y = "Índice ponderado (0–1)",
      x = ""
    ) +
    theme_minimal()
  
# Variacion por grupo de edad ####
  df$rango_edad <- cut(
    df$QH03,
    breaks = c(-Inf, 18, 35, Inf),
    labels = c("Adolescentes", "Adultos Jovenes", "Adultos"),
    right = FALSE
  )
  
  df %>%
    group_by(rango_edad) %>%
    summarise(
      N = n(),
      media = mean(indice_ponderado, na.rm = TRUE),
      mediana = median(indice_ponderado, na.rm = TRUE),
      sd = sd(indice_ponderado, na.rm = TRUE)
    )
  
  
  
  
  
  
  
# Variaciones por Indice de Riqueza   ####
  df %>%
    group_by(QHWLTHI5) %>%
    summarise(
      N = n(),
      media = mean(indice_ponderado, na.rm = TRUE),
      mediana = median(indice_ponderado, na.rm = TRUE),
      sd = sd(indice_ponderado, na.rm = TRUE)
    )
  
 # Validación Convergente ##### 
  
  ## Estadisticas Descriptivas ####
  
  table(df$golpe_mano)
  prop.table(table(df$golpe_mano))
  describe(df$golpe_mano)
  prop_golpe_mano <- sum(df$golpe_mano == 1, na.rm = TRUE) / 4539
  prop_golpe_mano
  
  ##Pruebas de convergencia ####
  
  options(scipen = 999)
  
  cor.test(df$indice_ponderado, as.numeric(as.character(df$golpe_mano)), use="complete.obs")
  # o si golpe_mano ya es 0/1 numérica:
  cor.test(df$indice_ponderado, df$golpe_mano, use="complete.obs")

  