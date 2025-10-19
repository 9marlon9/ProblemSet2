#Taller 2 BdMl

# 0.Cargar datos  =========
install.packages("pacman")
library(pacman)
p_load(readr,tidyverse,googledrive, skimr, naniar, dplyr, caret, themis, recipes)

drive_auth()

folder <- drive_get("ProblemSet2")
files <- drive_ls(folder)

## A. Leer directamente sin guardar archivo local
train_hogares <- read_csv(drive_download(
                                  files[files$name == "train_hogares.csv",]$id,
                                  path = tempfile(fileext = ".csv"),
                                  overwrite = TRUE)$local_path)

train_personas <- read_csv(drive_download(
                                  files[files$name == "train_personas.csv",]$id,
                                  path = tempfile(fileext = ".csv"),
                                  overwrite = TRUE)$local_path)

test_hogares <- read_csv(drive_download(
                                  files[files$name == "test_hogares.csv",]$id,
                                  path = tempfile(fileext = ".csv"),
                                  overwrite = TRUE)$local_path)

test_personas <- read_csv(drive_download(
                                  files[files$name == "test_personas.csv",]$id,
                                  path = tempfile(fileext = ".csv"),
                                  overwrite = TRUE)$local_path)

# 0.1 Selección de variables ============

# 0.1.2 Variables relevantes Hogares
#Etiquetas Variables Hogares:
# Diccionario variables
diccionario_hogares <- c(
  "P5000" = "n_cuartos",
  "P5010" = "cuartos_dormir",
  "P5090" = "tiene_vivienda",
  "P5100" = "cuota_amortizacion",
  "P5130" = "arriendo_estimado",
  "P5140" = "arriendo_mensual"
)

# Aplicar cambios
names(train_hogares)[names(train_hogares) %in% names(diccionario_hogares)] <- 
  diccionario_hogares[names(train_hogares)[names(train_hogares) %in% names(diccionario_hogares)]]
names(test_hogares)[names(test_hogares) %in% names(diccionario_hogares)] <- 
  diccionario_hogares[names(test_hogares)[names(test_hogares) %in% names(diccionario_hogares)]]


# 0.1.3 Variables relevantes Personas
# Etiquetas Variables Hogares:
# Diccionario variables

diccionario_personas <- c(
  "P6020" = "Sexo",
  "P6040" = "Edad",
  "P6050" = "Jefe_hogar",
  "P6090" = "SS_salud", #SS=Seguridad social
  "P6100" = "Régimen_SS_salud",
  "P6210" = "Nivel_educ", 
  "P6210s1" = "Grado_aprobado",
  "P6240" = "Act_principal_SP", #SP=Semana pasada
  "P6426" = "T_Tra_Emp", #Tiempo trabajado en la empresa (meses)
  "P6430" = "Pos_tra_pri", #Posición trabajo principal
  "P6510" = "Ing_HE", #Ingresos por horas extras
  "P6545" = "Ing_Pr", #Ingresos por primas
  "P6580" = "Ing_Bon", #Ingresos por Bonificaciones
  "P6585s1" = "Sub_Ali", #Subsidio alimentación
  "P6585s2" = "Sub_Trans",# Subsidio transporte
  "P6585s3" = "Sub_Fam", # Subsidio familiar
  "P6585s4" = "Sub_Edu",# Subsidio educativo
  "P6590" = "Ing_esp_ali",# Ingresos en especie por alimentación
  "P6600" = "Ing_esp_viv",# Ingresos en especie por vivienda
  "P6610" = "Trans_emp", # Uso transporte de la empresa
  "P6620" = "Ing_esp_otros",# Otros ingresos en especie
  "P6630s1" = "Pri_serv_12m", # Prima de servicios últimos 12 meses
  "P6630s2" = "Pri_nav_12m",# Prima de navidad últimos 12 meses
  "P6630s3" = "Pri_vac_12m",# Prima de vacaciones últimos 12 meses
  "P6630s4" = "Viat_per_12m", # Viáticos permanentes últimos 12 meses
  "P6630s6" = "Bon_anual_12m",# Bonificaciones anuales últimos 12 meses
  "P6800" = "Hras_sem_trab", # Horas trabajadas normalmente a la semana
  "P6870" = "Tam_empresa",# Tamaño de la empresa donde trabaja
  "P6920" = "Cot_pension",# Cotiza a fondo de pensiones
  "P7040" = "Seg_trab_SP",# Tuvo segundo trabajo la semana pasada
  "P7045" = "Hras_seg_trab",# Horas trabajadas en segundo trabajo
  "P7050" = "Pos_tra_sec",  # Posición ocupacional en segundo trabajo
  "P7090" = "Quiere_mas_horas",# Quiere trabajar más horas
  "P7110" = "Diligencias_mas_horas",# Hizo diligencias para trabajar más horas
  "P7120" = "Disp_mas_horas", # Disponible para trabajar más horas
  "P7150" = "Dilig_camb_trab",# Hizo diligencias para cambiar de trabajo
  "P7160" = "Disp_camb_trab", # Podría empezar nuevo trabajo antes de un mes
  "P7310" = "Busq_trab_primera",# Buscó trabajo por primera vez o había trabajado antes
  "P7350" = "Pos_ult_trab", # Posición ocupacional en último trabajo (desocupados)
  "P7422" = "Ing_trab_mes_desoc", # Ingresos por trabajo mes pasado (desocupados)
  "P7472" = "Ing_trab_mes_desoc2",# Ingresos por trabajo mes pasado (desocupados) - segunda pregunta
  "P7495" = "Ing_arriendo_pension", # Ingresos por arriendos y/o pensiones
  "P7500s2" = "Ing_pension_jub",# Ingresos por pensiones o jubilaciones
  "P7500s3" = "Ing_pension_ali",# Ingresos por pensión alimenticia
  "P7505" = "Ing_no_lab_12m", # Ingresos no laborales últimos 12 meses
  "P7510s1" = "Ing_din_hog_nac",# Ingresos por dinero de otros hogares en el país
  "P7510s2" = "Ing_din_hog_ext",# Ingresos por dinero de otros hogares fuera del país
  "P7510s3" = "Ing_ayuda_inst", # Ingresos por ayudas de instituciones
  "P7510s5" = "Ing_interes_div",# Ingresos por intereses, dividendos, utilidades
  "P7510s6" = "Ing_cesantias",  # Ingresos por cesantías e intereses
  "P7510s7" = "Ing_otras_fuentes" # Ingresos de otras fuentes
)

#Aplicar cambios
names(test_personas)[names(test_personas) %in% names(diccionario_personas)] <- 
  diccionario_personas[names(test_personas)[names(test_personas) %in% names(diccionario_personas)]]
names(train_personas)[names(train_personas) %in% names(diccionario_personas)] <- 
  diccionario_personas[names(train_personas)[names(train_personas) %in% names(diccionario_personas)]]

#Variables irrelevantes
#vars_a_eliminar <- c(
  #"P6500", "P6510s1", "P6510s2", "P6545s1", "P6545s2",
  #"P6580s1", "P6580s2", "P6585s1a1", "P6585s1a2", "P6585s2a1",
  #"P6585s2a2", "P6585s3a1", "P6585s3a2", "P6585s4a1", "P6585s4a2",
  #"P6590s1", "P6600s1", "P6610s1", "P6620s1", "P6630s1a1",
  #"P6630s2a1", "P6630s3a1", "P6630s4a1", "P6630s6a1", "P6750",
  #"P6760", "P550", "P7070", "P7140s1", "P7140s2", "P7422s1", "P7472s1", 
  #"P7500s1", "P7500s1a1","P7500s2a1", "P7500s3a1", "P7510s1a1", 
  #"P7510s2a1", "P7510s3a1","P7510s5a1", "P7510s6a1", "P7510s7a1",
  #"Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6", "Cclasnr7", "Cclasnr8", "Cclasnr11"
#)

#train_personas <- train_personas %>% 
  #select(-all_of(vars_a_eliminar))


# #variables_finales <- c(
#   # CLAVES PARA UNIR BASES
#   "id", "Orden", "Clase", "Dominio", "Fex_c", "Depto", "Fex_dpto",
#   
#   # VARIABLES PREDICTORAS (solo las que están en AMBOS)
#   "Oc",                  # Ocupado
#   "Nivel_educ",          # Nivel educativo
#   "Edad",                # Edad
#   "Pos_tra_pri",         # Posición ocupacional
#   "Cot_pension",         # Cotiza a pensiones
#   "SS_salud",            # Afiliación salud
#   "Hras_sem_trab",       # Horas trabajadas
#   "Jefe_hogar",          # Parentesco
#   "Act_principal_SP",    # Actividad principal
#   "T_Tra_Emp",           # Tiempo en empresa
#   "Ing_HE",              # Ingreso horas extras
#   "Sub_Trans",           # Subsidio transporte
#   "Pet",                 # Población edad trabajar
#   "Ina",                 # Inactivo
#   "Tam_empresa",         # Tamaño empresa
#   "Régimen_SS_salud",    # Régimen salud
#   "Grado_aprobado",      # Grado aprobado
#   "Sexo",                # Sexo
#   "Des"                  # Desocupado (agregamos para completar)
# )

# Para train_personas
#train_personas <- train_personas[, variables_finales]
# Para test_personas (mismas variables)
#test_personas <- test_personas[, variables_finales]


table(train_personas$Cot_pension)



# 1. Datos - Análisis variables Personas/Hogares===============
# Arreglos

# Modificar la función pre_process_personas para recodificar Cot_pension
pre_process_personas <- function(data) {
  data <- data |> 
    mutate(
      Sexo = ifelse(Sexo == 2, 1, 0),
      Jefe_hogar = ifelse(Jefe_hogar == 1, 1, 0),
      Niños = ifelse(Edad <= 6, 1, 0),
      Nivel_educ = ifelse(Nivel_educ == 9, 0, Nivel_educ),
      Oc = ifelse(is.na(Oc), 0, 1),
      Ina = ifelse(is.na(Ina), 0, 1), # 1=Inactivo, 0=Activo (NA→0)
      # Nueva recodificación para Cot_pension
      Cot_pension = case_when(
        Cot_pension == 1 ~ 1,  # Sí cotiza
        Cot_pension == 3 ~ 1,  # Ya pensionado, lo tratamos como 1
        Cot_pension == 2 ~ 0,  # No cotiza
        TRUE ~ 0  # Cualquier otro caso (NA, etc.) → 0
      )
    )
  return(data)
}

# Aplicar el preprocesamiento actualizado
train_personas <- pre_process_personas(train_personas)
test_personas <- pre_process_personas(test_personas)



#Variables de persona agregadas por hogar TRAIN

TR_personas_nivel_hogar <- train_personas |> 
  group_by(id) |>
  summarize(
    num_women    = sum(Sexo, na.rm = TRUE),
    num_minors   = sum(Niños, na.rm = TRUE),
    cat_maxEduc  = max(Nivel_educ, na.rm = TRUE),
    num_occupied = sum(Oc, na.rm = TRUE),
    # NUEVAS VARIABLES:
    num_inactivos = sum(Ina, na.rm = TRUE),  # Total de inactivos
    num_cotizantes = sum(Cot_pension, na.rm = TRUE)  # Total que cotizan/pensionados
  ) |> 
  ungroup()

##Variables por jefe del hogar Train:
TR_personas_nivel_hogar <- train_personas |> 
  filter(Jefe_hogar == 1) |>
  select(id, Sexo, Nivel_educ, Oc) |>
  rename(bin_headWoman = Sexo,
         bin_occupiedHead = Oc) |>
  left_join(TR_personas_nivel_hogar)

#Variables de persona agregadas por hogar TEST

TE_personas_nivel_hogar <- test_personas |> 
  group_by(id) |>
  summarize(
    num_women    = sum(Sexo, na.rm = TRUE),
    num_minors   = sum(Niños, na.rm = TRUE),
    cat_maxEduc  = max(Nivel_educ, na.rm = TRUE),
    num_occupied = sum(Oc, na.rm = TRUE),
    # NUEVAS VARIABLES:
    num_inactivos = sum(Ina, na.rm = TRUE),  # Total de inactivos
    num_cotizantes = sum(Cot_pension, na.rm = TRUE)  # Total que cotizan/pensionados
  ) |> 
  ungroup()

##Variables por jefe del hogar Test:
TE_personas_nivel_hogar <- test_personas |> 
  filter(Jefe_hogar == 1) |>
  select(id, Sexo, Nivel_educ, Oc) |>
  rename(bin_headWoman = Sexo,
         bin_occupiedHead = Oc) |>
  left_join(TE_personas_nivel_hogar)


#Arreglos a nivel hogar:

train_hogares <- train_hogares |>
  mutate(
    bin_rent = ifelse(tiene_vivienda == 3, 1, 0),
    prop_cuartos = n_cuartos / Nper,
    prop_cuartos_dormir = cuartos_dormir / Nper
  ) |>
  select(id, tiene_vivienda, Pobre, n_cuartos, cuartos_dormir, Nper, 
         prop_cuartos, prop_cuartos_dormir, bin_rent)

test_hogares <- test_hogares |>
  mutate(
    bin_rent = ifelse(tiene_vivienda == 3, 1, 0),
    prop_cuartos = n_cuartos / Nper,
    prop_cuartos_dormir = cuartos_dormir / Nper
  ) |>
  select(id, tiene_vivienda, n_cuartos, cuartos_dormir, Nper, 
         prop_cuartos, prop_cuartos_dormir, bin_rent)


# Crear las variables de proporción en la unión con hogares:
train <- train_hogares |> 
  left_join(TR_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4                             # Normalizar 0-1
  ) |>
  select(-id) # Solo eliminar en train


test <- test_hogares |> 
  left_join(TE_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4                             # Normalizar 0-1
  )
train <- train |> 
  mutate(prop_ocupados = num_occupied / Nper)

test <- test |> 
  mutate(prop_ocupados = num_occupied / Nper)


train <- train |> 
  mutate(Pobre   = factor(Pobre,levels=c(0,1))
  )

# 3. Modelo sin balanceo de muestras =================

ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  savePredictions = TRUE
)

set.seed(2025)

model_rf <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = expand.grid(mtry = c(3, 5, 7))  # Número de variables por split
)



# 3.2 Modelo con Down-Sampling N_tree 100 =================
# Definir función F1 para la CV INTERNA

f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
                                 # Más rápido 
  sampling = "down",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)


#Predicciones:
predicciones_test <- predict(model_rf_fast, newdata = test)
# Submission
submission <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_down.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission, ruta_completa, row.names = FALSE)



predicciones_model_rf_fast_down <- predict(model_rf_fast, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_rf_fast_down, train$Pobre)
print(confusion_matrix)


# 3.2.1 Modelo con Smoote-Sampling N_tree 100 =================
# Definir función F1 para la CV INTERNA


f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
  # Más rápido 
  sampling = "smoote",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)



#Predicciones:
predicciones_test <- predict(model_rf_fast, newdata = test)
# Submission
submission <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_down.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission, ruta_completa, row.names = FALSE)



predicciones_model_rf_fast <- predict(model_rf_fast, newdata = train)
confusion_matrix <- confusionMatrix(model_rf_fast, train$Pobre)
print(confusion_matrix)

# 3.2.2 Modelo con Up-Sampling N_tree 150 =================

f1_summary <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  sensitivity <- confusion$byClass["Sensitivity"]
  precision <- confusion$byClass["Pos Pred Value"]
  f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  c(F1 = f1,
    Sensibilidad = sensitivity,
    Precision = precision,
    Accuracy = confusion$overall["Accuracy"])
}

# Configurar control con F1 para CV
# Configuración SUPER rápida
ctrl_fast_up <- trainControl(
  method = "cv",
  number = 3,                    # Solo 3 folds (más rápido)
  classProbs = FALSE,
  savePredictions = FALSE,       # No guarden las predicciones para que les corra
  # Más rápido 
  sampling = "up",
  summaryFunction = f1_summary,
  verboseIter = FALSE           # Esto hace que se demore menos
)

set.seed(2025)

model_rf_fast_up <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_fast_up,
  metric = "F1",
  tuneGrid = expand.grid(mtry = c(3, 5)), 
  ntree = 100,                   
  importance = FALSE,            
  do.trace = FALSE             
)


predicciones_model_rf_up <- predict(model_rf_fast_up, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_rf_up, train$Pobre)
print(confusion_matrix)

#Predicciones:
predicciones_test_up <- predict(model_rf_fast_up, newdata = test)
# Submission
submission_up <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test_up) - 1  
)
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
best_mtry <- model_rf_fast_up$bestTune$mtry
nombre_archivo <- paste0("RF_mtry_", best_mtry, "_ntree_100_sampling_up.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)
# Guardar el submission
write.csv(submission_up, ruta_completa, row.names = FALSE)



# 3.3 Modelo con Down-Sampling N_tree 150 ===============

# Función F1 personalizada que penaliza más los Falsos Negativos
f1_weighted <- function(data, lev = NULL, model = NULL) {
  confusion <- caret::confusionMatrix(data$pred, data$obs)
  
  # Extraer métricas clave
  sensitivity <- confusion$byClass["Sensitivity"]  # Detección de pobres (clase 1)
  precision <- confusion$byClass["Pos Pred Value"]
  specificity <- confusion$byClass["Specificity"]
  
  # Calcular F1 tradicional
  f1_trad <- 2 * (precision * sensitivity) / (precision + sensitivity)
  
  # Penalización por Falsos Negativos (pobres no detectados)
  false_negatives <- confusion$table[1, 2]  # FNs: Reference=1, Prediction=0
  total_positives <- sum(confusion$table[, 2])  # Total pobres reales
  fn_ratio <- false_negatives / total_positives
  
  # F1 weighted: castigar más los Falsos Negativos
  f1_weighted <- f1_trad * (1 - fn_ratio * 0.5)  # Reducción del 50% por FNs
  
  c(F1_weighted = f1_weighted,
    F1_trad = f1_trad,
    Sensitivity = sensitivity,
    Precision = precision,
    Specificity = specificity,
    FN_Count = false_negatives,
    FN_Ratio = fn_ratio)
}


# Configuración de trainControl con F1 weighted
ctrl_weighted <- trainControl(
  method = "cv",
  number = 3,
  classProbs = FALSE,
  savePredictions = FALSE,
  sampling = "smote",
  summaryFunction = f1_weighted,  # Usar nuestra función personalizada
  verboseIter = FALSE
)

set.seed(2025)

# Entrenar modelo con mtry = 2 y F1 weighted
model_rf_weighted <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl_weighted,
  metric = "F1_weighted",  # Optimizar por F1 weighted
  tuneGrid = expand.grid(mtry = 2),  # Solo mtry = 2 para más sensibilidad
  ntree = 150,             # Un poco más de árboles
  importance = FALSE,
  do.trace = FALSE
)


# Predecir en test con el nuevo modelo
predicciones_test_weighted <- predict(model_rf_weighted, newdata = test)

# Crear submission
submission_weighted <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test_weighted) - 1
)

# Guardar con nombre descriptivo
nombre_weighted <- "RF_mtry_2_ntree_150_F1weighted.csv"
write.csv(submission_weighted, 
          file.path("C:/Users/Marlon Angulo/Downloads", nombre_weighted), 
          row.names = FALSE)

# Ver matriz de confusión
predicciones_train_weighted <- predict(model_rf_weighted, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_train_weighted, train$Pobre)
print(confusion_matrix)





# Modelo 3.5 con Boosting ==============
p_load(xgboost)

# Calcular peso para clase minoritaria
scale_ratio <- table(train$Pobre)[1] / table(train$Pobre)[2]  # ≈4

model_xgb <- train(
  Pobre ~ .,
  data = train,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 3, classProbs = FALSE),
  metric = "F1_weighted",
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  scale_pos_weight = scale_ratio  # Pesa más la clase pobre
)


predicciones_model_xgb <- predict(model_xgb, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_xgb, train$Pobre)
print(confusion_matrix)
















# MOdelo 3.6 =========
p_load(caret, DMwR)

set.seed(2025)

ctrl <- trainControl(
  method = "cv",
  number = 3,
  savePredictions = FALSE ,   # solo guarda las finales
  classProbs = FALSE,          # desactiva cálculo de probabilidades
  sampling = "smote",            # puedes usar "down", "up" o "smote" si hay desbalance
  verboseIter = FALSE 
)

model_rf <- train(
  Pobre ~ .,
  data = train,
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy",         # métrica simple, más rápida
  tuneGrid = expand.grid(mtry = c(3, 5, 7)),
  ntree= 100,
  importance = FALSE, do.trace = FALSE
)



predicciones_model_rf <- predict(model_rf, newdata = train)
confusion_matrix <- confusionMatrix(predicciones_model_xgb, train$Pobre)
print(confusion_matrix)


# 4. Sección de análisis de datos =============

p_load(broom, ggplot2)

# Estadísticas descriptivas por grupo de pobreza
estadisticas <- train %>%
  group_by(Pobre) %>%
  summarise(
    Nper_mean = mean(Nper, na.rm = TRUE),
    prop_ocupados_mean = mean(prop_ocupados, na.rm = TRUE),
    prop_inactivos_mean = mean(prop_inactivos, na.rm = TRUE),
    prop_cotizantes_mean = mean(prop_cotizantes, na.rm = TRUE),
    cat_maxEduc_mean = mean(cat_maxEduc, na.rm = TRUE),
    prop_cuartos_mean = mean(prop_cuartos, na.rm = TRUE)
  )

# Promedios totales
totales <- train %>%
  summarise(
    Nper_total = mean(Nper, na.rm = TRUE),
    prop_ocupados_total = mean(prop_ocupados, na.rm = TRUE),
    prop_inactivos_total = mean(prop_inactivos, na.rm = TRUE),
    prop_cotizantes_total = mean(prop_cotizantes, na.rm = TRUE),
    cat_maxEduc_total = mean(cat_maxEduc, na.rm = TRUE),
    prop_cuartos_total = mean(prop_cuartos, na.rm = TRUE)
  )

# 3. Pruebas de significancia (t-test)
variables_test <- c("Nper", "prop_ocupados", "prop_inactivos", 
                    "prop_cotizantes", "cat_maxEduc", "prop_cuartos")

pruebas_significancia <- list()
for(var in variables_test) {
  formula <- as.formula(paste(var, "~ Pobre"))
  prueba <- t.test(formula, data = train)
  pruebas_significancia[[var]] <- tidy(prueba)
}

# 4. Resultados para exportar
estadisticas
totales
pruebas_significancia

# Variable objetivo:
# Calcular frecuencias y porcentajes
frecuencias <- table(train$Pobre)
porcentajes <- prop.table(frecuencias) * 100

# Crear etiquetas para las barras
# Modificar solo esta línea:
etiquetas <- paste0(format(frecuencias, big.mark = ".", decimal.mark = ","), 
                    "\n(", round(porcentajes, 1), "%)")
# Gráfico con frecuencias y porcentajes
barplot(frecuencias,
        main = "Distribución de la Variable Objetivo: Pobreza",
        xlab = "Condición de Pobreza", 
        ylab = "Número de Hogares",
        col = c("gray70", "gray40"),
        names.arg = c("No Pobre (0)", "Pobre (1) "),
        ylim = c(0, max(frecuencias) * 1.1),
        border = "black")

# Agregar etiquetas encima de las barras
text(x = c(0.7, 1.9), 
     y = frecuencias + max(frecuencias) * 0.05,
     labels = etiquetas,
     cex = 0.8)















# 3.5 Modelo Boosting Trashehold 0.3 ===========


train$Pobre <- factor(ifelse(train$Pobre == 1, "Pobre", "NoPobre"),
                      levels = c("NoPobre", "Pobre"))


# Configuración del control
ctrl_xgb <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  verboseIter = FALSE
)

# Modelo XGBoost original
set.seed(2025)
model_xgb <- train(
  Pobre ~ .,
  data = train,
  method = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Obtener probabilidades del modelo original
probabilidades <- predict(model_xgb, train, type = "prob")

# Aplicar custom threshold
threshold_bajo <- 0.33
predicciones_custom <- ifelse(probabilidades$Pobre > threshold_bajo, "Pobre", "NoPobre")
predicciones_custom <- factor(predicciones_custom, levels = c("NoPobre", "Pobre"))

# Matriz de confusión
confusion_custom <- confusionMatrix(predicciones_custom, train$Pobre)
print(confusion_custom)







# CALCULAR F1 PARA EL MODELO ANTERIOR (que da 0.65 en test)
TP_old <- confusion_custom$table[2, 2]  # Pobre predicho como Pobre
FP_old <- confusion_custom$table[2, 1]  # NoPobre predicho como Pobre  
FN_old <- confusion_custom$table[1, 2]  # Pobre predicho como NoPobre

precision_old <- TP_old / (TP_old + FP_old)
recall_old <- TP_old / (TP_old + FN_old)
f1_old <- 2 * (precision_old * recall_old) / (precision_old + recall_old)

cat("🔍 MODELO ANTERIOR (que da 0.65 en Kaggle):\n")
cat("- True Positives:", TP_old, "\n")
cat("- False Positives:", FP_old, "\n") 
cat("- False Negatives:", FN_old, "\n")
cat("- Precision Pobre:", round(precision_old, 4), "\n")
cat("- Recall Pobre:", round(recall_old, 4), "\n")
cat("- F1 Train:", round(f1_old, 4), "\n")
cat("- F1 Kaggle: 0.65\n")
cat("- Overfitting:", round(f1_old - 0.65, 4), "\n")










# Obtener probabilidades del modelo original
probabilidades <- predict(model_xgb, test, type = "prob")

# Aplicar custom threshold
threshold_bajo <- 0.33
predicciones_custom <- ifelse(probabilidades$Pobre > threshold_bajo, "Pobre", "NoPobre")


# Submission
submission_xgb <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_custom == "Pobre")  # 1 si Pobre, 0 si NoPobre
)

ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"

# Puedes incluir parámetros del modelo en el nombre si quieres, por ejemplo el threshold usado
nombre_archivo <- paste0("XGB_threshold_", threshold_bajo, "_nrounds_100.csv")
ruta_completa <- file.path(ruta_descargas, nombre_archivo)

# Guardar el submission
write.csv(submission_xgb, ruta_completa, row.names = FALSE)















































































































# Modelo solo con top 8 variables ===========
vars_top <- c("Pobre", "prop_cotizantes", "prop_cuartos", "prop_ocupados", 
              "num_cotizantes", "tiene_vivienda", "cat_maxEduc", 
              "prop_cuartos_dormir", "Nivel_educ")

train_top <- train[, vars_top]

set.seed(2025)
model_xgb_top <- train(
  Pobre ~ .,
  data = train_top,
  method = "xgbTree",
  trControl = ctrl_xgb,
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Probar en test
probabilidades_top <- predict(model_xgb_top, test[, vars_top[-1]], type = "prob")
predicciones_top <- ifelse(probabilidades_top$Pobre > 0.3, 1, 0)


# Matriz de confusión en entrenamiento
probabilidades_train_top <- predict(model_xgb_top, train_top, type = "prob")
predicciones_train_top <- ifelse(probabilidades_train_top$Pobre > 0.3, "Pobre", "NoPobre")
predicciones_train_top <- factor(predicciones_train_top, levels = c("NoPobre", "Pobre"))

confusion_train_top <- confusionMatrix(predicciones_train_top, train_top$Pobre)
print(confusion_train_top)

# Exportar resultados del test
submission_top <- data.frame(
  id = test$id,
  poverty = predicciones_top
)

write.csv(submission_top, "C:/Users/Marlon Angulo/Downloads/XGB_top_variables_threshold_0.3.csv", row.names = FALSE)







# Modelo con variables avanzadas ==========



# Crear nuevas variables basadas en las relaciones más importantes
train_enhanced <- train %>%
  mutate(
    # Ratios sofisticados (como mencionan en "Feature Refinement")
    ratio_efectividad_ocupacional = num_occupied / (num_inactivos + 1),
    ratio_proteccion_social = num_cotizantes / (Nper + 1),
    indice_capital_humano = (cat_maxEduc * prop_ocupados) / (num_minors + 1),
    
    # Interacciones entre variables top del varImp
    interaccion_vivienda_educ = tiene_vivienda * Nivel_educ,
    interaccion_cotizantes_ocupados = prop_cotizantes * prop_ocupados,
    
    # Segmentación estratégica
    segmento_estrategico = case_when(
      prop_cotizantes < 0.2 & num_minors > 1 ~ "familias_vulnerables",
      prop_ocupados < 0.3 & vulnerability_index > 0.6 ~ "hogares_criticos",
      prop_cotizantes > 0.7 & prop_ocupados > 0.7 ~ "hogares_estables",
      TRUE ~ "hogares_medianos"
    ),
    
    # Variables de desigualdad interna del hogar
    dispersion_educativa = cat_maxEduc - Nivel_educ, # Jefe vs máximo
    brecha_genero_ocupacion = (num_women / Nper) - prop_ocupados
  )

# Convertir segmento a factor
train_enhanced$segmento_estrategico <- factor(train_enhanced$segmento_estrategico)

# Verificar nuevas variables
summary(train_enhanced[, c("ratio_efectividad_ocupacional", "ratio_proteccion_social", "indice_capital_humano")])


# Crear quintiles basados en el índice de vulnerabilidad (como hicieron con income)
train_enhanced <- train_enhanced %>%
  mutate(vulnerability_quintile = ntile(vulnerability_index, 5))

# Ver distribución de Pobre por quintil
table(train_enhanced$vulnerability_quintile, train_enhanced$Pobre)

# Estratificar la muestra - tomar misma cantidad por quintil
set.seed(2025)
train_stratified <- train_enhanced %>%
  group_by(vulnerability_quintile) %>%
  sample_n(min(30000, n())) %>%  # Ajustar según tamaño disponible
  ungroup()

# Verificar nuevo balance
table(train_stratified$vulnerability_quintile, train_stratified$Pobre)




# Configuración mejorada con validación
ctrl_xgb_enhanced <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = FALSE,
  allowParallel = TRUE,
  sampling = "up"  # Upsampling para balancear dentro de folds
)

# Grid de parámetros optimizado
tune_grid_enhanced <- expand.grid(
  nrounds = 150,
  max_depth = 6,
  eta = 0.05,        # Learning rate más bajo
  gamma = 1,         # Regularización
  colsample_bytree = 0.7,
  min_child_weight = 2,
  subsample = 0.8
)

# Entrenar modelo mejorado
set.seed(2025)

# Corregir los niveles del factor Pobre
train_stratified$Pobre <- factor(train_stratified$Pobre, 
                                 levels = c(0, 1),
                                 labels = c("NoPobre", "Pobre"))

# Verificar
table(train_stratified$Pobre)
levels(train_stratified$Pobre)

# Ahora ejecutar el modelo
set.seed(2025)
model_xgb_enhanced <- train(
  Pobre ~ .,
  data = train_stratified %>% select(-vulnerability_quintile),
  method = "xgbTree",
  trControl = ctrl_xgb_enhanced,
  tuneGrid = tune_grid_enhanced,
  metric = "ROC",
  verbose = FALSE
)


# Obtener probabilidades del modelo mejorado
probabilidades_enhanced <- predict(model_xgb_enhanced, train_stratified, type = "prob")

# Función para evaluar por quintiles
evaluar_por_quintiles <- function(probs, true_labels, vulnerability_quintiles, threshold = 0.3) {
  predictions <- ifelse(probs$Pobre > threshold, "Pobre", "NoPobre")
  
  results <- data.frame(
    quintil = vulnerability_quintiles,
    verdadero = true_labels,
    prediccion = predictions
  )
  
  confusion_por_quintil <- results %>%
    group_by(quintil) %>%
    summarise(
      accuracy = mean(prediccion == verdadero),
      recall_pobre = sum(prediccion == "Pobre" & verdadero == "Pobre") / sum(verdadero == "Pobre"),
      precision_pobre = sum(prediccion == "Pobre" & verdadero == "Pobre") / sum(prediccion == "Pobre"),
      n_pobres_reales = sum(verdadero == "Pobre"),
      n_pobres_predichos = sum(prediccion == "Pobre")
    )
  
  return(confusion_por_quintil)
}

# Evaluar con threshold 0.3
resultados_quintiles <- evaluar_por_quintiles(
  probabilidades_enhanced, 
  train_stratified$Pobre, 
  train_stratified$vulnerability_quintile
)

print(resultados_quintiles)








# Crear ensemble
crear_ensemble <- function(probs, thresholds = c(0.2, 0.3, 0.4)) {
  ensemble_score <- rowMeans(sapply(thresholds, function(th) {
    ifelse(probs$Pobre > th, 1, 0)
  }))
  return(ensemble_score)
}

ensemble_scores <- crear_ensemble(probabilidades_enhanced)

# Encontrar mejor threshold para ensemble
encontrar_mejor_threshold_ensemble <- function(ensemble_scores, true_labels) {
  thresholds <- seq(0.1, 0.6, by = 0.05)
  resultados <- data.frame()
  
  for(th in thresholds) {
    pred_temp <- ifelse(ensemble_scores > th, "Pobre", "NoPobre")
    cm_temp <- confusionMatrix(factor(pred_temp, levels = c("NoPobre", "Pobre")), true_labels)
    
    resultados <- rbind(resultados, data.frame(
      threshold = th,
      recall_pobre = cm_temp$byClass["Sensitivity"],
      precision_pobre = cm_temp$byClass["Pos Pred Value"],
      f1_pobre = cm_temp$byClass["F1"]
    ))
  }
  
  return(resultados)
}

resultados_ensemble <- encontrar_mejor_threshold_ensemble(ensemble_scores, train_stratified$Pobre)
print(resultados_ensemble)


























# Preparar test set con las mismas features
test_enhanced <- test %>%
  mutate(
    ratio_efectividad_ocupacional = num_occupied / (num_inactivos + 1),
    ratio_proteccion_social = num_cotizantes / (Nper + 1),
    indice_capital_humano = (cat_maxEduc * prop_ocupados) / (num_minors + 1),
    interaccion_vivienda_educ = tiene_vivienda * Nivel_educ,
    interaccion_cotizantes_ocupados = prop_cotizantes * prop_ocupados,
    segmento_estrategico = case_when(
      prop_cotizantes < 0.2 & num_minors > 1 ~ "familias_vulnerables",
      prop_ocupados < 0.3 & vulnerability_index > 0.6 ~ "hogares_criticos",
      prop_cotizantes > 0.7 & prop_ocupados > 0.7 ~ "hogares_estables",
      TRUE ~ "hogares_medianos"
    ),
    dispersion_educativa = cat_maxEduc - Nivel_educ,
    brecha_genero_ocupacion = (num_women / Nper) - prop_ocupados
  )

test_enhanced$segmento_estrategico <- factor(test_enhanced$segmento_estrategico)

# Obtener probabilidades del test
probabilidades_test <- predict(model_xgb_enhanced, test_enhanced, type = "prob")

# Aplicar ensemble con mejor threshold (0.35)
ensemble_test <- crear_ensemble(probabilidades_test, thresholds = c(0.2, 0.3, 0.4))
predicciones_finales <- ifelse(ensemble_test > 0.35, 1, 0)

# Submission final
submission_final <- data.frame(
  id = test$id,
  poverty = predicciones_finales
)

write.csv(submission_final, "C:/Users/Marlon Angulo/Downloads/XGB_enhanced_ensemble_0.77F1.csv", row.names = FALSE)



# Modelo final ============

# Tu modelo original que dio 0.65 - pero con las nuevas features
set.seed(2025)
model_original_enhanced <- train(
  Pobre ~ .,
  data = train,  # Usar TODOS los datos, no solo estratificados
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 3, classProbs = TRUE),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  ),
  verbose = FALSE
)



prob_train_enhanced <- predict(model_original_enhanced, train, type = "prob")
pred_train_enhanced <- ifelse(prob_train_enhanced$Pobre > 0.3, "Pobre", "NoPobre")
pred_train_enhanced <- factor(pred_train_enhanced, levels = c("NoPobre", "Pobre"))

confusion_train_enhanced <- confusionMatrix(pred_train_enhanced, train$Pobre)
print(confusion_train_enhanced)











# Probar en test
prob_original <- predict(model_original_enhanced, test_enhanced, type = "prob")
pred_original <- ifelse(prob_original$Pobre > 0.3, 1, 0)

submission_original <- data.frame(id = test$id, poverty = pred_original)
write.csv(submission_original, "C:/Users/Marlon Angulo/Downloads/XGB_original_enhanced.csv", row.names = FALSE)

# Modelo optimizando metrica Kaggle =========

train_original_backup <- train

# Balanceo 50/50 para que el modelo aprenda ambos patrones por igual
train_balanced <- train_original_backup %>%
  group_by(Pobre) %>%
  sample_n(min(n(), 6000)) %>%  # 6000 de cada clase
  ungroup()

print(table(train_balanced$Pobre))

train <- train_balanced

print("Dataset listo para feature engineering:")
print(table(train$Pobre))


# Featuring avanzado

# Feature engineering avanzado - versión compacta
advanced_feature_engineering <- function(data) {
  data <- data %>%
    mutate(
      # Ratios económicos clave
      dependency_ratio = (num_minors + num_inactivos) / (num_occupied + 0.1),
      educ_occupation_gap = cat_maxEduc / (prop_ocupados + 0.1),
      financial_inclusion = num_cotizantes / (Nper + 0.1),
      
      # Interacciones no lineales
      vulnerability_employment = vulnerability_index * (1 - prop_ocupados),
      minors_financial_stress = num_minors * (1 - financial_inclusion),
      
      # Severidad de hacinamiento
      overcrowding_severity = case_when(
        prop_cuartos < 0.25 ~ 3,
        prop_cuartos < 0.5 ~ 2,
        TRUE ~ 1
      )
    )
  return(data)
}

# Aplicar a train y test
train <- advanced_feature_engineering(train)
test <- advanced_feature_engineering(test)

# Verificar
print("Nuevas variables creadas:")
print(names(train)[(ncol(train)-5):ncol(train)])


# Optimización final:

# Optimización final de modelos

# 1. Configuración robusta de entrenamiento
ctrl_robust <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = FALSE
)

# 2. Grid de hiperparámetros optimizado
tune_grid_xgb <- expand.grid(
  nrounds = c(100, 150),
  max_depth = c(4, 6),
  eta = c(0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 0.8),
  min_child_weight = c(3, 5),
  subsample = c(0.8, 0.9)
)

# 3. Entrenar modelo optimizado

train$Pobre <- factor(ifelse(train$Pobre == 1, "Pobre", "NoPobre"),
                      levels = c("NoPobre", "Pobre"))

set.seed(2025)
model_xgb_optimized <- train(
  Pobre ~ .,
  data = train,
  method = "xgbTree",
  trControl = ctrl_robust,
  tuneGrid = tune_grid_xgb,
  metric = "ROC",
  verbose = FALSE
)

# 4. Mejores parámetros
print("Mejores parámetros encontrados:")
print(model_xgb_optimized$bestTune)

# 5. Optimizar threshold con F1
optimizar_threshold <- function(modelo, datos, verdad) {
  probs <- predict(modelo, datos, type = "prob")$Pobre
  thresholds <- seq(0.1, 0.5, by = 0.02)
  f1_scores <- sapply(thresholds, function(th) {
    preds <- ifelse(probs > th, "Pobre", "NoPobre")
    preds <- factor(preds, levels = c("NoPobre", "Pobre"))
    conf_matrix <- confusionMatrix(preds, verdad)
    conf_matrix$byClass["F1"]
  })
  best_threshold <- thresholds[which.max(f1_scores)]
  return(best_threshold)
}

best_threshold <- optimizar_threshold(model_xgb_optimized, train, train$Pobre)
print(paste("Mejor threshold:", round(best_threshold, 3)))

# Evaluar el modelo en train con el threshold óptimo
probabilidades_train <- predict(model_xgb_optimized, train, type = "prob")
predicciones_train <- ifelse(probabilidades_train$Pobre > best_threshold, "Pobre", "NoPobre")
predicciones_train <- factor(predicciones_train, levels = c("NoPobre", "Pobre"))

# Matriz de confusión
confusion_final <- confusionMatrix(predicciones_train, train$Pobre)

print(confusion_final)

# Predecir en test y guardar en Downloads
probabilidades_test <- predict(model_xgb_optimized, test, type = "prob")
predicciones_test <- ifelse(probabilidades_test$Pobre > 0.5, "Pobre", "NoPobre")

# Obtener los mejores hiperparámetros
best_params <- model_xgb_optimized$bestTune

# Crear nombre descriptivo según convención
nombre_archivo <- paste0(
  "XGB",
  "_nrounds_", best_params$nrounds,
  "_maxdepth_", best_params$max_depth,
  "_eta_", best_params$eta,
  "_gamma_", best_params$gamma,
  "_colsample_", best_params$colsample_bytree,
  "_minchild_", best_params$min_child_weight,
  "_subsample_", best_params$subsample,
  "_threshold_05.csv"
)

# Ruta completa a Downloads
ruta_descargas <- "C:/Users/Marlon Angulo/Downloads"
ruta_completa <- file.path(ruta_descargas, nombre_archivo)

# Submission final
submission_final <- data.frame(
  id = test$id,
  poverty = as.numeric(predicciones_test == "Pobre")
)

# Guardar
write.csv(submission_final, ruta_completa, row.names = FALSE)
print(paste("Submission guardado en:", ruta_completa))
print("Hiperparámetros utilizados:")
print(best_params)




# Modelo Optimizado 0.70 ========
# ENRIQUECIMIENTO MASIVO DE VARIABLES DESDE DATOS ORIGINALES

cat("🚀 AGREGANDO 50+ VARIABLES NUEVAS DESDE DATOS ORIGINALES\n")

# 1. AGREGAR MÁS VARIABLES DE PERSONAS A NIVEL HOGAR
train_personas_enriched <- train_personas |> 
  group_by(id) |>
  summarize(
    # ===== VARIABLES BÁSICAS MEJORADAS 
    edad_promedio = mean(Edad, na.rm = TRUE),
    edad_maxima = max(Edad, na.rm = TRUE),
    edad_minima = min(Edad, na.rm = TRUE),
    rango_edad = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
    
    # ===== VARIABLES DE EMPLEO DETALLADAS 
    total_horas_trabajo = sum(Hras_sem_trab, na.rm = TRUE),
    horas_promedio_trabajo = mean(Hras_sem_trab, na.rm = TRUE),
    num_trabajadores_tiempo_completo = sum(Hras_sem_trab >= 40, na.rm = TRUE),
    num_trabajadores_medio_tiempo = sum(Hras_sem_trab >= 20 & Hras_sem_trab < 40, na.rm = TRUE),
    
    # ===== VARIABLES DE TIPO DE EMPLEO 
    num_empleados_formales = sum(Pos_tra_pri == 3 & Cot_pension == 1, na.rm = TRUE),
    num_empleados_informales = sum(Pos_tra_pri == 3 & Cot_pension == 0, na.rm = TRUE),
    num_independientes = sum(Pos_tra_pri == 4, na.rm = TRUE),
    num_patrones = sum(Pos_tra_pri == 1, na.rm = TRUE),
    num_trabajadores_domesticos = sum(Pos_tra_pri == 5, na.rm = TRUE),
    
    # ===== VARIABLES DE EDUCACIÓN DETALLADAS 
    promedio_educacion = mean(Nivel_educ, na.rm = TRUE),
    max_educacion = max(Nivel_educ, na.rm = TRUE),
    num_sin_educacion = sum(Nivel_educ == 0, na.rm = TRUE),
    num_educacion_basica = sum(Nivel_educ %in% c(1, 2, 3), na.rm = TRUE),
    num_educacion_media = sum(Nivel_educ %in% c(4, 5), na.rm = TRUE),
    num_educacion_superior = sum(Nivel_educ %in% c(6, 7, 8, 9), na.rm = TRUE),
    
    # ===== VARIABLES DE SEGURIDAD SOCIAL 
    num_salud_subsidiado = sum(Régimen_SS_salud == 2, na.rm = TRUE),
    num_salud_contributivo = sum(Régimen_SS_salud == 1, na.rm = TRUE),
    num_salud_especial = sum(Régimen_SS_salud == 3, na.rm = TRUE),
    num_sin_salud = sum(is.na(Régimen_SS_salud) | Régimen_SS_salud == 0, na.rm = TRUE),
    
    # ===== VARIABLES DE BÚSQUEDA DE EMPLEO 
    num_buscando_trabajo = sum(Des == 1, na.rm = TRUE),
    num_disponibles_trabajar = sum(Disp_mas_horas == 1, na.rm = TRUE),
    num_quieren_mas_horas = sum(Quiere_mas_horas == 1, na.rm = TRUE),
    
    # ===== VARIABLES DE SUBSIDIOS 
    num_recibe_subsidio_transporte = sum(Sub_Trans == 1, na.rm = TRUE),
    num_recibe_subsidio_familiar = sum(Sub_Fam == 1, na.rm = TRUE),
    num_recibe_subsidio_educativo = sum(Sub_Edu == 1, na.rm = TRUE),
    
    # ===== VARIABLES DE INGRESOS (indicadores binarios) 
    num_ingreso_horas_extra = sum(Ing_HE > 0, na.rm = TRUE),
    num_ingreso_bonificaciones = sum(Ing_Bon > 0, na.rm = TRUE),
    num_ingreso_primas = sum(Ing_Pr > 0, na.rm = TRUE),
    
    # ===== VARIABLES DE ESTABILIDAD LABORAL 
    promedio_tiempo_empresa = mean(T_Tra_Emp, na.rm = TRUE),
    max_tiempo_empresa = max(T_Tra_Emp, na.rm = TRUE),
    num_empleados_estables = sum(T_Tra_Emp > 12, na.rm = TRUE), # +1 año en empresa
    
    # ===== VARIABLES DE TAMAÑO EMPRESA ===
    num_empresas_grandes = sum(Tam_empresa %in% c(4, 5), na.rm = TRUE),
    num_empresas_pequenas = sum(Tam_empresa %in% c(1, 2), na.rm = TRUE),
    
    # ===== VARIABLES DE ACTIVIDAD ECONÓMICA ===
    num_agricultura = sum(Act_principal_SP == 1, na.rm = TRUE),
    num_industria = sum(Act_principal_SP == 2, na.rm = TRUE),
    num_servicios = sum(Act_principal_SP %in% c(3, 4, 5, 6), na.rm = TRUE),
    
    # ===== VARIABLES DE JEFATURA Y GÉNERO ===
    edad_jefe_hogar = first(Edad[Jefe_hogar == 1]),
    educacion_jefe = first(Nivel_educ[Jefe_hogar == 1]),
    sexo_jefe = first(Sexo[Jefe_hogar == 1]),
    ocupacion_jefe = first(Oc[Jefe_hogar == 1])
  ) |>
  ungroup()

# 2. HACER LO MISMO PARA TEST
test_personas_enriched <- test_personas |> 
  group_by(id) |>
  summarize(
    edad_promedio = mean(Edad, na.rm = TRUE),
    edad_maxima = max(Edad, na.rm = TRUE),
    edad_minima = min(Edad, na.rm = TRUE),
    rango_edad = max(Edad, na.rm = TRUE) - min(Edad, na.rm = TRUE),
    total_horas_trabajo = sum(Hras_sem_trab, na.rm = TRUE),
    horas_promedio_trabajo = mean(Hras_sem_trab, na.rm = TRUE),
    num_trabajadores_tiempo_completo = sum(Hras_sem_trab >= 40, na.rm = TRUE),
    num_trabajadores_medio_tiempo = sum(Hras_sem_trab >= 20 & Hras_sem_trab < 40, na.rm = TRUE),
    num_empleados_formales = sum(Pos_tra_pri == 3 & Cot_pension == 1, na.rm = TRUE),
    num_empleados_informales = sum(Pos_tra_pri == 3 & Cot_pension == 0, na.rm = TRUE),
    num_independientes = sum(Pos_tra_pri == 4, na.rm = TRUE),
    num_patrones = sum(Pos_tra_pri == 1, na.rm = TRUE),
    num_trabajadores_domesticos = sum(Pos_tra_pri == 5, na.rm = TRUE),
    promedio_educacion = mean(Nivel_educ, na.rm = TRUE),
    max_educacion = max(Nivel_educ, na.rm = TRUE),
    num_sin_educacion = sum(Nivel_educ == 0, na.rm = TRUE),
    num_educacion_basica = sum(Nivel_educ %in% c(1, 2, 3), na.rm = TRUE),
    num_educacion_media = sum(Nivel_educ %in% c(4, 5), na.rm = TRUE),
    num_educacion_superior = sum(Nivel_educ %in% c(6, 7, 8, 9), na.rm = TRUE),
    num_salud_subsidiado = sum(Régimen_SS_salud == 2, na.rm = TRUE),
    num_salud_contributivo = sum(Régimen_SS_salud == 1, na.rm = TRUE),
    num_salud_especial = sum(Régimen_SS_salud == 3, na.rm = TRUE),
    num_sin_salud = sum(is.na(Régimen_SS_salud) | Régimen_SS_salud == 0, na.rm = TRUE),
    num_buscando_trabajo = sum(Des == 1, na.rm = TRUE),
    num_disponibles_trabajar = sum(Disp_mas_horas == 1, na.rm = TRUE),
    num_quieren_mas_horas = sum(Quiere_mas_horas == 1, na.rm = TRUE),
    num_recibe_subsidio_transporte = sum(Sub_Trans == 1, na.rm = TRUE),
    num_recibe_subsidio_familiar = sum(Sub_Fam == 1, na.rm = TRUE),
    num_recibe_subsidio_educativo = sum(Sub_Edu == 1, na.rm = TRUE),
    num_ingreso_horas_extra = sum(Ing_HE > 0, na.rm = TRUE),
    num_ingreso_bonificaciones = sum(Ing_Bon > 0, na.rm = TRUE),
    num_ingreso_primas = sum(Ing_Pr > 0, na.rm = TRUE),
    promedio_tiempo_empresa = mean(T_Tra_Emp, na.rm = TRUE),
    max_tiempo_empresa = max(T_Tra_Emp, na.rm = TRUE),
    num_empleados_estables = sum(T_Tra_Emp > 12, na.rm = TRUE),
    num_empresas_grandes = sum(Tam_empresa %in% c(4, 5), na.rm = TRUE),
    num_empresas_pequenas = sum(Tam_empresa %in% c(1, 2), na.rm = TRUE),
    num_agricultura = sum(Act_principal_SP == 1, na.rm = TRUE),
    num_industria = sum(Act_principal_SP == 2, na.rm = TRUE),
    num_servicios = sum(Act_principal_SP %in% c(3, 4, 5, 6), na.rm = TRUE),
    edad_jefe_hogar = first(Edad[Jefe_hogar == 1]),
    educacion_jefe = first(Nivel_educ[Jefe_hogar == 1]),
    sexo_jefe = first(Sexo[Jefe_hogar == 1]),
    ocupacion_jefe = first(Oc[Jefe_hogar == 1])
  ) |>
  ungroup()



train_with_id <- train_hogares |> 
  left_join(TR_personas_nivel_hogar) |>
  mutate(
    # PROPORCIONES EXISTENTES:
    prop_inactivos = num_inactivos / Nper,
    prop_cotizantes = num_cotizantes / Nper,
    prop_ocupados = num_occupied / Nper,
    
    # NUEVA VARIABLE AVANZADA:
    vulnerability_index = (
      (1 - prop_ocupados) +           # Desempleo
        (num_minors / Nper) +           # Carga de menores  
        (1 - prop_cotizantes) +         # Exclusión financiera
        (1 / (prop_cuartos + 0.1))      # Hacinamiento inverso
    ) / 4,                             # Normalizar 0-1
    
    Pobre = factor(ifelse(Pobre == 1, "Pobre", "NoPobre"), 
                   levels = c("NoPobre", "Pobre"))
  )




# 3. COMBINAR CON LOS DATOS EXISTENTES
train_enriched <- train_with_id |> 
  left_join(train_personas_enriched, by = "id")

test_enriched <- test |> 
  left_join(test_personas_enriched, by = "id")

# Reemplazar NAs con 0 en estas variables
train_enriched_clean <- train_enriched %>%
  mutate(
    horas_promedio_trabajo = ifelse(is.na(horas_promedio_trabajo), 0, horas_promedio_trabajo),
    promedio_tiempo_empresa = ifelse(is.na(promedio_tiempo_empresa), 0, promedio_tiempo_empresa)
  )

# Aplicar misma transformación a test
test_enriched_clean <- test_enriched %>%
  mutate(
    horas_promedio_trabajo = ifelse(is.na(horas_promedio_trabajo), 0, horas_promedio_trabajo),
    promedio_tiempo_empresa = ifelse(is.na(promedio_tiempo_empresa), 0, promedio_tiempo_empresa)
  )

train_enriched_final <- train_enriched_clean %>%
  mutate(max_tiempo_empresa = ifelse(is.infinite(max_tiempo_empresa), 0, max_tiempo_empresa))
test_enriched_final <- test_enriched_clean %>%
  mutate(max_tiempo_empresa = ifelse(is.infinite(max_tiempo_empresa), 0, max_tiempo_empresa))


# Verificar que no hay NAs
sum(is.na(train_enriched_clean))




# VERSIÓN MEJORADA CON ANTI-OVERFITTING
set.seed(2025)
model_enriched_improved <- train(
  Pobre ~ .,
  data = train_enriched_final %>% select(-id),
  method = "xgbTree",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 1,           # ✅ REGULARIZACIÓN AÑADIDA
    colsample_bytree = 0.7, # ✅ MÁS CONSERVADOR
    min_child_weight = 3,   # ✅ MÁS RESTRICTIVO  
    subsample = 0.8
  ),
  verbose = FALSE
)

table(train_enriched$Pobre)




# VER RESULTADOS DEL MODELO ORIGINAL CON THRESHOLD 0.4
prob_original <- predict(model_enriched_improved, train_enriched_final, type = "prob")$Pobre
pred_original <- ifelse(prob_original > 0.33, "Pobre", "NoPobre")
cm_original <- confusionMatrix(factor(pred_original, levels = c("NoPobre", "Pobre")), 
                               train_enriched_final$Pobre)

# CALCULAR F1 PARA LA CLASE "POBRE" CORRECTAMENTE
# Para la clase "Pobre":
TP <- cm_original$table[2, 2]  # Verdaderos positivos: Pobre predicho como Pobre
FP <- cm_original$table[2, 1]  # Falsos positivos: NoPobre predicho como Pobre  
FN <- cm_original$table[1, 2]  # Falsos negativos: Pobre predicho como NoPobre

precision_pobre_correct <- TP / (TP + FP)
recall_pobre_correct <- TP / (TP + FN)
f1_pobre_correct <- 2 * (precision_pobre_correct * recall_pobre_correct) / 
  (precision_pobre_correct + recall_pobre_correct)

cat("🎯 MÉTRICAS CORRECTAS PARA CLASE 'POBRE':\n")
cat("- True Positives (Pobre correcto):", TP, "\n")
cat("- False Positives:", FP, "\n") 
cat("- False Negatives:", FN, "\n")
cat("- Precision Pobre:", round(precision_pobre_correct, 4), "\n")
cat("- Recall Pobre:", round(recall_pobre_correct, 4), "\n")
cat("- F1 Pobre:", round(f1_pobre_correct, 4), "\n")





















#En teoría es mejor con 0.34
prob_train <- predict(model_enriched_improved, train_enriched_final, type = "prob")$Pobre
pred_train <- ifelse(prob_train > 0.34, "Pobre", "NoPobre")
confusionMatrix(factor(pred_train, levels = c("NoPobre", "Pobre")), train_enriched_final$Pobre)


# 6. PREDECIR CON THRESHOLD 0.34
prob_test_enriched <- predict(model_enriched, test_enriched_final, type = "prob")$Pobre
pred_test_enriched <- ifelse(prob_test_enriched > 0.34, "Pobre", "NoPobre")

# 7. SUBMISSION
submission_enriched <- data.frame(
  id = test_enriched$id,
  poverty = as.numeric(pred_test_enriched == "Pobre")
)

write.csv(submission_enriched, 
          "C:/Users/Marlon Angulo/Downloads/XGB_50plus_vars_th033.csv", 
          row.names = FALSE)



























#Modelo optmizado con advanced featuring 0.71===========
train_enriched_advanced <- train_enriched_final %>%
  mutate(
    # INTERACCIONES CLAVE
    interaccion_educ_ocupacion = promedio_educacion * prop_ocupados,
    densidad_por_educacion = Nper / (promedio_educacion + 0.1),
    ratio_formalidad = num_empleados_formales / (num_empleados_informales + 1),
    
    # NUEVAS VARIABLES COMPUESTAS
    score_empleabilidad = (prop_ocupados * 0.4) + (prop_cotizantes * 0.3) + 
      (num_educacion_superior / Nper * 0.3),
    
    indice_vulnerabilidad_mejorado = (
      (1 - prop_ocupados) * 0.3 +           # Desempleo
        (num_minors / Nper) * 0.2 +           # Dependencia
        (1 - prop_cotizantes) * 0.25 +        # Informalidad
        (1 / (prop_cuartos + 0.1)) * 0.15 +   # Hacinamiento
        (1 - (num_educacion_superior / Nper)) * 0.1  # Educación baja
    ),
    
    # VARIABLES DE COMPOSICIÓN FAMILIAR MEJORADAS
    prop_menores = num_minors / Nper,
    prop_adultos_mayores = ifelse(edad_maxima > 60, 1, 0),
    diversidad_ocupacional = num_empleados_formales + num_independientes + num_trabajadores_domesticos,
    
    # INDICADORES DE ESTABILIDAD
    estabilidad_laboral = num_empleados_estables / (num_occupied + 1),  # Corregido
    variabilidad_ingresos = (num_ingreso_horas_extra + num_ingreso_bonificaciones) / Nper
  )

# Aplicar a test
test_enriched_advanced <- test_enriched_final %>%
  mutate(
    interaccion_educ_ocupacion = promedio_educacion * prop_ocupados,
    densidad_por_educacion = Nper / (promedio_educacion + 0.1),
    ratio_formalidad = num_empleados_formales / (num_empleados_informales + 1),
    score_empleabilidad = (prop_ocupados * 0.4) + (prop_cotizantes * 0.3) + 
      (num_educacion_superior / Nper * 0.3),
    indice_vulnerabilidad_mejorado = (
      (1 - prop_ocupados) * 0.3 + (num_minors / Nper) * 0.2 + 
        (1 - prop_cotizantes) * 0.25 + (1 / (prop_cuartos + 0.1)) * 0.15 + 
        (1 - (num_educacion_superior / Nper)) * 0.1
    ),
    prop_menores = num_minors / Nper,
    prop_adultos_mayores = ifelse(edad_maxima > 60, 1, 0),
    diversidad_ocupacional = num_empleados_formales + num_independientes + num_trabajadores_domesticos,
    estabilidad_laboral = num_empleados_estables / (num_occupied + 1),
    variabilidad_ingresos = (num_ingreso_horas_extra + num_ingreso_bonificaciones) / Nper
  )

# Continuar con el modelo...


# Modelo con mejor feature engineering
set.seed(2025)
model_advanced <- train(
  Pobre ~ .,
  data = train_enriched_advanced %>% select(-id),
  method = "xgbTree",
  trControl = trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    verboseIter = TRUE
  ),
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.1,
    gamma = 1,
    colsample_bytree = 0.7,
    min_child_weight = 3,
    subsample = 0.8
  ),
  verbose = FALSE
)

# Evaluar
prob_advanced <- predict(model_advanced, train_enriched_advanced, type = "prob")$Pobre

# Encontrar mejor threshold para el nuevo modelo
thresholds <- seq(0.01, 0.5, 0.01)
f1_scores_advanced <- numeric(length(thresholds))

for(i in seq_along(thresholds)) {
  pred <- ifelse(prob_advanced > thresholds[i], "Pobre", "NoPobre")
  pred <- factor(pred, levels = c("NoPobre", "Pobre"))
  cm <- confusionMatrix(pred, train_enriched_advanced$Pobre)
  
  TP <- cm$table[2, 2]
  FP <- cm$table[2, 1]  
  FN <- cm$table[1, 2]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_scores_advanced[i] <- 2 * (precision * recall) / (precision + recall)
}

best_threshold_advanced <- thresholds[which.max(f1_scores_advanced)]
best_f1_advanced <- max(f1_scores_advanced)

cat("🎯 RESULTADOS CON MEJOR FEATURE ENGINEERING:\n")
cat("- Mejor threshold:", best_threshold_advanced, "\n")
cat("- Mejor F1:", round(best_f1_advanced, 4), "\n")
cat("- Mejora vs anterior:", round(best_f1_advanced - 0.7083, 4), "\n")
