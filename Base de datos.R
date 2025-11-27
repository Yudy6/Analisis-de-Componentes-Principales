# Instalar paquetes si es necesario
if (!require("FactoMineR")) install.packages("FactoMineR")
if (!require("Factoshiny")) install.packages("Factoshiny")

# Cargar librerias
library(Factoshiny)
library(haven)
library(dplyr)
library(tidyverse)

# Cargar los archivos de datos
load("BD_ENPOL_2021.RData") 

# En ENPOL 2021
datos_completos <- ENPOL2021_2_3 %>%
  left_join(ENPOL2021_4, by = "ID_PER") %>%
  left_join(ENPOL2021_5, by = "ID_PER") %>%
  left_join(ENPOL2021_6, by = "ID_PER") %>%
  left_join(ENPOL2021_7, by = "ID_PER") %>%
  left_join(ENPOL2021_8_9_10_11, by = "ID_PER") %>%
  left_join(ENPOL2021_SOC, by = "ID_PER")

# Verificar que todo se unió correctamente
nrow(datos_completos) # Debe ser 61449
str(datos_completos)


# CREAR DATAFRAME CON VARIABLES CORREGIDAS Y VERIFICADAS
datos_filtrados <- datos_completos %>%
  select(
    # ===== IDENTIFICACIÓN Y FACTORES DE EXPANSIÓN =====
    id_persona = ID_PER,
    
    # ===== VARIABLES SOCIODEMOGRÁFICAS (SOC) =====
    sexo = P1_2,                           # Sexo
    edad = P1_3,                           # Cuántos años cumplidos tiene?
    estado_civil = P1_7,                   # Estado civil
    escolaridad = P1_18_N,                 # Hasta qué año o grado aprobó en la escuela
    tiene_hijos = P1_8,                    # Tiene hijos
    numero_hijos = P1_9,                   # Número de hijos
    mantiene_alguien = P2_4,      # La semana antes de su detención, mantenia económicamente a alguien
    
    # ===== DETENCIÓN (MÓDULO 2-3) =====
    estado_detencion = P1_1,               #  IDENTIFIQUE EL TIPO DE CENTRO PENITENCIARIO
    
    # ===== CONDICIONES EN LA DETENCIÓN (MÓDULO 6) =====
    personas_celda = P6_1,                     # Con cuántas personas comparte su celda
    comida_dia = P6_13,             # Cuántas veces al dia le proporcionan alimentos en este Centro
    
    # ===== INGRESO AL CENTRO PENITENCIARIO (MÓDULO 7) =====
    horas_celda = P7_2,               #  Considerando las 24 horas del dia, ¿cuántas horas pasa en su celda?
    seguridad_celda = P7_35,          # En términos de violencia, ¿se siente seguro(a) o inseguro(a) en su celda o dormitorio?
    
    # ===== ACTIVIDADES Y PROGRAMAS (MÓDULO 9) =====
    antes_recluido = P9_1,            # Antes de su reclusión en este Centro, ¿usted había sido sentenciado(a) por un delito?
    veces_recluido = P9_4,        # ¿Cuántas veces estuvo recluido en un Centro penitenciario o en un Centro de Internamiento para Adolescentes
    vivio_con_madr = P9_8_1,             # Antes de cumplir los 15 años, ¿vivió con su madre
    vivio_con_padr = P9_8_2,           # Antes de cumplir los 15 años, ¿vivió con su padre
  ) %>%
  # CONVERSIÓN DE TIPOS Y RECODIFICACIÓN DE FACTORES
  mutate(
    # ===== VARIABLES NUMÉRICAS (INTEGER) =====
    edad = as.integer(edad),
    escolaridad = as.integer(escolaridad),
    numero_hijos = as.integer(numero_hijos),
    personas_celda = as.integer(personas_celda),
    comida_dia = as.integer(comida_dia),
    horas_celda = as.integer(horas_celda),
    veces_recluido = as.integer(veces_recluido),
    
    # ===== VARIABLES FACTOR CON CATEGORiAS =====
    
    # SEXO (P1_2)
    sexo = factor(sexo,
                  levels = c(1, 2),
                  labels = c("Hombre", "Mujer"),
                  ordered = FALSE),
    
    
    estado_detencion = factor(estado_detencion,
                              levels = c(1, 2, 3),
                              labels = c("Centro varonil", 
                                         "Centro femenil", 
                                         "Mixto"),
                              ordered = FALSE),
    
    # ESTADO CIVIL (P1_7)
    estado_civil = factor(estado_civil,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                          labels = c("unión libre",
                                     "separado(a) de una unión libre",
                                     "separado(a) de un matrimonio",
                                     "casado(a)",
                                     "soltero(a)",
                                     "divorciado(a)",
                                     "viudo(a)",
                                     "No sabe",
                                     "No responde"),
                          ordered = FALSE),
    
    # ESTADO CIVIL REDUCIDO A 4 CATEGORÍAS
    estado_civil = factor(case_when(
      estado_civil %in% c("casado(a)", "union libre") ~ "En union",
      estado_civil %in% c("soltero(a)") ~ "Soltero",
      estado_civil %in% c("separado(a) de una union libre", 
                          "separado(a) de un matrimonio",
                          "divorciado(a)") ~ "Separado/Divorciado",
      estado_civil %in% c("viudo(a)") ~ "Viudo",
      TRUE ~ "No especificado"
    ), levels = c("Soltero", "En union", "Separado/Divorciado", "Viudo", "No especificado"),
    ordered = FALSE),
    
    # ESCOLARIDAD (P1_18_N)
    escolaridad = factor(escolaridad,
                         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 98, 99),
                         labels = c("Ninguno",
                                    "Preescolar",
                                    "Primaria",
                                    "Secundaria",
                                    "Carrera técnica con secundaria terminada",
                                    "Normal básica (con antecedente en secundaria)",
                                    "Preparatoria o bachillerato",
                                    "Carrera técnica con preparatoria terminada",
                                    "Licenciatura o profesional",
                                    "Maestria o doctorado",
                                    "No sabe",
                                    "No responde"),
                         ordered = TRUE), # Se puede dejar como ordenado por nivel educativo
    
    # ESCOLARIDAD REDUCIDA A 4 CATEGORÍAS
    escolaridad = factor(case_when(
      escolaridad %in% c("Ninguno", "Preescolar", "Primaria") ~ "Primaria o menos",
      escolaridad %in% c( "Secundaria") ~ "Secundaria",
      escolaridad %in% c("Preparatoria o bachillerato", 
                         "Carrera técnica con secundaria terminada",
                         "Carrera técnica con preparatoria terminada",
                         "Normal basica (con antecedente en secundaria)") ~ "Media Superior/Tecnica",
      escolaridad %in% c("Licenciatura o profesional", 
                         "Maestria o doctorado") ~ "Superior o mas",
      escolaridad %in% c("No sabe", "No responde") ~ "No especificado",
      TRUE ~ as.character(escolaridad)
    ), levels = c("Primaria o menos", 
                  "Secundaria", 
                  "Media Superior/Tecnica", 
                  "Superior o mas",
                  "No especificado"),
    ordered = TRUE),
    
    # TIENE HIJOS (P1_8)
    tiene_hijos = factor(tiene_hijos,
                         levels = c(1, 2, 8, 9),
                         labels = c("Si", "No", "No sabe", "No responde"),
                         ordered = FALSE),
    
    # OCUPACION ANTES DE LA DETENCION (P2_4)
    mantiene_alguien = factor(mantiene_alguien,
                              levels = c(1, 2, 8, 9),
                              labels = c("Si", "No", "No sabe", "No responde"),
                              ordered = FALSE),
    
    # SEGURIDAD EN CELDA (P7_35)
    seguridad_celda = factor(seguridad_celda,
                             levels = c(1, 2, 8, 9),
                             labels = c("Seguro", "Inseguro", "No sabe", "No responde")),
    
    # VIVIÓ CON MADRE ANTES DE 15 AÑOS (P9_8_1)
    vivio_con_madr = factor(vivio_con_madr,
                            levels = c(1, 2, 8, 9),
                            labels = c("Si", "No", "No sabe", "No responde")),
    
    # VIVIÓ CON PADRE ANTES DE 15 AÑOS (P9_8_2)
    vivio_con_padr = factor(vivio_con_padr,
                            levels = c(1, 2, 8, 9),
                            labels = c("Si", "No", "No sabe", "No responde")),
    
    # ANTES RECLUIDO (P9_1)
    antes_recluido = factor(antes_recluido,
                            levels = c(1, 2, 8, 9),
                            labels = c("Si", "No", "No sabe", "No responde"))
  )

rm(list = setdiff(ls(), c("datos_filtrados", "datos_completos")))

# ANTES: Verificar cantidad de NA por variable
sapply(datos_filtrados, function(x) sum(is.na(x)))

# Asignar 0 a todos los que no tienen hijos (incluyendo "No sabe" y "No responde")
datos_filtrados$numero_hijos[datos_filtrados$tiene_hijos %in% c("No", "No sabe", "No responde")] <- 0
datos_filtrados$veces_recluido[datos_filtrados$antes_recluido %in% c("No", "No sabe", "No responde")] <- 0

# Eliminar filas con NAs en esas variables
datos_filtrados <- datos_filtrados %>%
  filter(!if_any(all_of(c("escolaridad", "comida_dia", "veces_recluido")), is.na))

# Limpieza específica por variable con diferentes umbrales
datos_filtrados <- datos_filtrados %>%
  mutate(
    # Edad: valores razonables entre 18 y 97
    edad = ifelse(edad < 18 | edad > 97, NA, edad),
    
    # Personas en celda: valores razonables (0-50 por ejemplo)
    personas_celda = ifelse(personas_celda > 50, NA, personas_celda),
    
    # Comidas por día: valores razonables (1-5)
    comida_dia = ifelse(comida_dia < 1 | comida_dia > 5, NA, comida_dia),
    
    # Horas en celda: valores razonables (0-24)
    horas_celda = ifelse(horas_celda > 24, NA, horas_celda),
    
    # Veces recluido: valores razonables 
    veces_recluido = ifelse(veces_recluido > 97, NA, veces_recluido),
    
    # Número de hijos: valores razonables 
    numero_hijos = ifelse(numero_hijos > 97, NA, numero_hijos)
  ) %>%
  drop_na(edad, personas_celda, comida_dia, horas_celda, veces_recluido, numero_hijos)

# DESPUÉS: Verificar cantidad de NA por variable
sapply(datos_filtrados, function(x) sum(is.na(x)))

# Eliminar categorías "No sabe", "No responde", "No especificado"
datos_filtrados <- datos_filtrados %>%
  # Filtrar filas que NO tengan estas categorías en las variables clave
  filter(
    # Estado civil - eliminar "No especificado"
    estado_civil != "No especificado",
    
    # Escolaridad - eliminar "No especificado" 
    escolaridad != "No especificado",
    
    # Tiene hijos - eliminar "No sabe" y "No responde"
    !tiene_hijos %in% c("No sabe", "No responde"),
    
    # Mantiene a alguien - eliminar "No sabe" y "No responde"
    !mantiene_alguien %in% c("No sabe", "No responde"),
    
    # Seguridad celda - eliminar "No sabe" y "No responde"
    !seguridad_celda %in% c("No sabe", "No responde"),
    
    # Antes recluido - eliminar "No sabe" y "No responde"
    !antes_recluido %in% c("No sabe", "No responde"),
    
    # Vivió con madre - eliminar "No sabe" y "No responde"
    !vivio_con_madr %in% c("No sabe", "No responde"),
    
    # Vivió con padre - eliminar "No sabe" y "No responde"
    !vivio_con_padr %in% c("No sabe", "No responde")
  ) %>%
  # Recodificar las variables para eliminar los niveles no usados
  mutate(
    estado_civil = droplevels(estado_civil),
    escolaridad = droplevels(escolaridad),
    tiene_hijos = droplevels(tiene_hijos),
    mantiene_alguien = droplevels(mantiene_alguien),
    seguridad_celda = droplevels(seguridad_celda),
    antes_recluido = droplevels(antes_recluido),
    vivio_con_madr = droplevels(vivio_con_madr),
    vivio_con_padr = droplevels(vivio_con_padr)
  )

# Resumen de datos limpios
summary(datos_filtrados)

# Crear variables dummies
datos_con_dummies <- datos_filtrados %>%
  mutate(
    # Variables dicotómicas simples
    mujer = as.integer(sexo == "Mujer"),
    tiene_hijos = as.integer(tiene_hijos == "Si"),
    mantiene_alguien = as.integer(mantiene_alguien == "Si"),
    seguridad_celda = as.integer(seguridad_celda == "Seguro"),
    antes_recluido = as.integer(antes_recluido == "Si"),
    vivio_con_madr = as.integer(vivio_con_madr == "Si"),
    vivio_con_padr = as.integer(vivio_con_padr == "Si"),
    
    # Dummies para estado_detencion (evitar multicolinealidad)
    detencion_varo = as.integer(estado_detencion == "Centro varonil"),
    detencion_fem = as.integer(estado_detencion == "Centro femenil")
    # centro_mixto es la categoría de referencia
  ) %>% select(-c(sexo, estado_detencion))
summary(datos_con_dummies)


rm(list = setdiff(ls(), c("datos_filtrados", "datos_con_dummies")))

# Verificar valores especificos de variables clave
cat("\n=== DISTRIBUCIÓN DE VARIABLES CLAVE ===\n")
table(datos_filtrados$sexo)
table(datos_filtrados$estado_detencion)
table(datos_filtrados$estado_civil)
table(datos_filtrados$escolaridad)
table(datos_filtrados$tiene_hijos)
table(datos_filtrados$mantiene_alguien)
table(datos_filtrados$seguridad_celda)
table(datos_filtrados$antes_recluido)
table(datos_filtrados$vivio_con_madr)
table(datos_filtrados$vivio_con_padr)



# Crear muestra del 10% (aproximadamente 5,471 observaciones)
muestra_10pct <- datos_con_dummies %>% 
  sample_frac(0.10)
nrow(muestra_10pct) # Verificar tamaño de la muestra
# 1. ACP estándar
result <- PCAshiny(muestra_10pct %>% select(-id_persona))

# 2. ACP manual
#Preparar datos separando variables cuantitativas y cualitativas
datos_qual <- datos_limpios %>%
  select(sexo, estado_civil, escolaridad, tiene_hijos)

datos_quant <- datos_limpios %>%
  select(edad, personas_celda, comida_dia, horas_celda, veces_recluido) %>%
  mutate(across(everything(), as.numeric))

# ACP con FactoMineR (maneja mejor grandes datasets)
result_pca <- PCA(datos_quant, graph = FALSE)

# Ver resultados
summary(result_pca)
# Graficar resultados
plot(result_pca, choix = "ind")
plot(result_pca, choix = "var")
plot(result_pca, choix = "varcor")

# correlacion con corplot
library(corrplot)
corr_matrix <- cor(datos_quant)

