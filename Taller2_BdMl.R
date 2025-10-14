#Taller 2 BdMl

# 0.Cargar datos  =========
install.packages("pacman")
library(pacman)
p_load(readr,tidyverse,googledrive, skimr, naniar, dplyr)

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




# 1. Datos - Análisis variables Personas===============
# Arreglos
train_personas <- train_personas |> 
  mutate(
    Sexo = ifelse(Sexo == 2, 1, 0),                    # Mujer=1, Hombre=0
    Jefe_hogar = ifelse(Jefe_hogar == 1, 1, 0),        # 1=Jefe, 0=No jefe
    Edad = ifelse(Edad <= 6, 1, 0),                    # 1=Menor de 6, 0=Mayor
    Nivel_educ = ifelse(Nivel_educ == 9, 0, Nivel_educ), # 9→0, otros igual
    Oc = ifelse(is.na(Oc), 0, 1)                       # 1=Ocupado, 0=No ocupado
  )



# 2. Datos - Análisis variables Hogares =============



## Pobre
table(train_hogares$Pobre)
prop.table(table(train_hogares$Pobre)) * 100

