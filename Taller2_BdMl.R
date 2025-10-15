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
vars_a_eliminar <- c(
  "P6500", "P6510s1", "P6510s2", "P6545s1", "P6545s2",
  "P6580s1", "P6580s2", "P6585s1a1", "P6585s1a2", "P6585s2a1",
  "P6585s2a2", "P6585s3a1", "P6585s3a2", "P6585s4a1", "P6585s4a2",
  "P6590s1", "P6600s1", "P6610s1", "P6620s1", "P6630s1a1",
  "P6630s2a1", "P6630s3a1", "P6630s4a1", "P6630s6a1", "P6750",
  "P6760", "P550", "P7070", "P7140s1", "P7140s2", "P7422s1", "P7472s1", 
  "P7500s1", "P7500s1a1","P7500s2a1", "P7500s3a1", "P7510s1a1", 
  "P7510s2a1", "P7510s3a1","P7510s5a1", "P7510s6a1", "P7510s7a1",
  "Cclasnr2", "Cclasnr3", "Cclasnr4", "Cclasnr5", "Cclasnr6", "Cclasnr7", "Cclasnr8", "Cclasnr11"
)

train_personas <- train_personas %>% 
  select(-all_of(vars_a_eliminar))


variables_finales <- c(
  # CLAVES PARA UNIR BASES
  "id", "Orden", "Clase", "Dominio", "Fex_c", "Depto", "Fex_dpto",
  
  # VARIABLES PREDICTORAS (solo las que están en AMBOS)
  "Oc",                  # Ocupado
  "Nivel_educ",          # Nivel educativo
  "Edad",                # Edad
  "Pos_tra_pri",         # Posición ocupacional
  "Cot_pension",         # Cotiza a pensiones
  "SS_salud",            # Afiliación salud
  "Hras_sem_trab",       # Horas trabajadas
  "Jefe_hogar",          # Parentesco
  "Act_principal_SP",    # Actividad principal
  "T_Tra_Emp",           # Tiempo en empresa
  "Ing_HE",              # Ingreso horas extras
  "Sub_Trans",           # Subsidio transporte
  "Pet",                 # Población edad trabajar
  "Ina",                 # Inactivo
  "Tam_empresa",         # Tamaño empresa
  "Régimen_SS_salud",    # Régimen salud
  "Grado_aprobado",      # Grado aprobado
  "Sexo",                # Sexo
  "Des"                  # Desocupado (agregamos para completar)
)

# Para train_personas
train_personas <- train_personas[, variables_finales]
# Para test_personas (mismas variables)
test_personas <- test_personas[, variables_finales]


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
    # NUEVAS PROPORCIONES:
    prop_inactivos = num_inactivos / Nper,  # Proporción de inactivos en el hogar
    prop_cotizantes = num_cotizantes / Nper  # Proporción que cotiza pensión
  ) |>
  select(-id) # Solo eliminar en train


train <- train |> 
  mutate(Pobre   = factor(Pobre,levels=c(0,1))
         )

test <- test_hogares |> 
  left_join(TE_personas_nivel_hogar) |>
  mutate(
    # NUEVAS PROPORCIONES:
    prop_inactivos = num_inactivos / Nper,  # Proporción de inactivos en el hogar
    prop_cotizantes = num_cotizantes / Nper  # Proporción que cotiza pensión
  )

train <- train |> 
  mutate(prop_ocupados = num_occupied / Nper)

test <- test |> 
  mutate(prop_ocupados = num_occupied / Nper)

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

# 3.1 Modelo con balanceo de muestras SMOTE =================

# Aplicar SMOTE con themis
train_balanced <- recipe(Pobre ~ ., data = train) %>%
  step_smote(Pobre, over_ratio = 0.5) %>%  # Balancea a 50/50
  prep() %>%
  bake(new_data = NULL)

# Verificar el nuevo balance
table(train_balanced$Pobre)
table(train$Pobre)

set.seed(2025)

model_rf1 <- train(
  Pobre ~ .,
  data = train_balanced,
  method = "rf",
  trControl = ctrl,
  metric = "Accuracy",
  tuneGrid = expand.grid(mtry = c(3, 5, 7))  # Número de variables por split
)



# 3.2 Modelo con balanceo de muestras Down-Sampling =================
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

