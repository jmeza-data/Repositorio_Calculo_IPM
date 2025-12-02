################################################################################
# CÁLCULO DEL ÍNDICE DE POBREZA MULTIDIMENSIONAL (IPM) 
# Encuesta Nacional de Calidad de Vida (ECV) 2024
# Metodología Oficial DANE - Versión Departamental
# Autor: Jhoan Sebastian Meza Garcia
# Universidad Nacional de Colombia
################################################################################

# CARGAR LIBRERÍAS ----
library(tidyverse)
library(readr)

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║     CÁLCULO IPM 2024 - METODOLOGÍA OFICIAL DANE            ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# CARGAR BASE_FINAL
# ==============================================================================
file.choose()
cat("Cargando base_final...\n")
base_final <- read_csv( "C:\\Users\\Jhoan meza\\Documents\\base_final_DEFINITIVA.csv")
cat("✓ Base cargada:", nrow(base_final), "personas\n\n")

################################################################################
# DIMENSIÓN 1: EDUCACIÓN
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("DIMENSIÓN 1: EDUCACIÓN\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Indicador 1: Bajo logro educativo ----
personas <- base_final %>%
  mutate(EDU_YEARS = case_when(
    P6040 >= 15 & P1088 == 1 ~ 0,
    P6040 >= 15 & (P1088 %in% c(2, 3) | P1088S1 %in% c(10, 11)) ~ P1088S1 - 1,
    P6040 >= 15 & (P1088S1 %in% c(12, 13)) ~ 11,
    P6040 >= 15 & (P1088 %in% c(5, 6, 7)) ~ 11 + P1088S1 - 1,
    P6040 >= 15 & P1088 == 8 ~ 16 + P1088S1 - 1,
    P6040 >= 15 & (P8587 %in% c(1, 2)) ~ 0,
    P6040 >= 15 & (P8587 %in% c(3, 4) | P8587S1 %in% c(10, 11)) ~ P8587S1,
    P6040 >= 15 & (P8587S1 %in% c(12, 13)) ~ 11,
    P6040 >= 15 & (P8587 %in% c(6, 7, 8, 9, 10, 11)) ~ 11 + P8587S1,
    P6040 >= 15 & (P8587 %in% c(12, 13)) ~ 16 + P8587S1,
    TRUE ~ NA_real_
  )) %>%
  group_by(llavehog) %>%
  mutate(EDU_YEARS_MEAN = mean(EDU_YEARS, na.rm = TRUE),
         MENOR = ifelse(P6040 <= 14, 1, 0),
         SUMAMENOR = sum(MENOR),
         CUENTAMIEMBROS = n(),
         SOLOMENORES = ifelse(SUMAMENOR == CUENTAMIEMBROS, 1, 0),
         LOGRO_2 = ifelse(EDU_YEARS_MEAN < 9 | SOLOMENORES == 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 1: Bajo logro educativo\n")

# Indicador 2: Analfabetismo ----
personas <- personas %>%
  mutate(ALFA = case_when(
    P6040 >= 15 & P6160 == 2 ~ 1,
    P6040 >= 15 & P6160 == 1 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  group_by(llavehog) %>%
  mutate(ALFA_SUMA = sum(ALFA, na.rm = TRUE),
         ALFA_2 = ifelse(ALFA_SUMA >= 1 | SOLOMENORES == 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 2: Analfabetismo\n\n")

# Guardar dimensión 1
hogares_1 <- personas %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ORDEN, llavehog, 
         FEX_C, LOGRO_2, ALFA_2)

################################################################################
# DIMENSIÓN 2: NIÑEZ Y JUVENTUD
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("DIMENSIÓN 2: NIÑEZ Y JUVENTUD\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Indicador 3: Inasistencia escolar ----
personas <- base_final %>%
  mutate(smart = ifelse(P1082S2 == 1, 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(SMART_SUMA = sum(smart, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(edad = ifelse(P6040 >= 6 & P6040 <= 16, 1, NA),
         ASISTE = case_when(
           P8586 == 2 & edad == 1 ~ 1,
           P8586 == 1 & edad == 1 ~ 0,
           TRUE ~ NA_real_
         )) %>%
  mutate(ASISTE = ifelse(edad == 1 & P8586 == 1 & P3336S2 == 1 & P1075 == 2, 1, ASISTE),
         ASISTE = ifelse(edad == 1 & P8586 == 1 & P3336S2 == 1 & P1075 == 1 & 
                           P1077S21 == 2 & P1077S22 == 2 & P1077S23 == 2 & SMART_SUMA == 0, 1, ASISTE),
         ASISTE = ifelse(edad == 1 & P8586 == 1 & P3336S2 == 1 & P1075 == 1 & 
                           (P1077S21 == 1 | P1077S22 == 1 | P1077S23 == 1 | SMART_SUMA > 0) & P3337 == 2, 1, ASISTE)) %>%
  group_by(llavehog) %>%
  mutate(ASISTE_SUMA = sum(ASISTE, na.rm = TRUE),
         ASISTE_2 = ifelse(ASISTE_SUMA >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 3: Inasistencia escolar\n")

# Indicador 4: Rezago escolar ----
personas <- personas %>%
  mutate(EDU_YEARS_REZ = case_when(
    P1088 == 1 ~ 0,
    (P1088 %in% c(2, 3) | P1088S1 %in% c(10, 11)) ~ P1088S1 - 1,
    P1088S1 %in% c(12, 13) ~ 11,
    P1088 %in% c(5, 6, 7) ~ 11 + P1088S1 - 1,
    P1088 == 8 ~ 16 + P1088S1 - 1,
    P8587 %in% c(1, 2) ~ 0,
    (P8587 %in% c(3, 4) | P8587S1 %in% c(10, 11)) ~ P8587S1,
    P8587S1 %in% c(12, 13) ~ 11,
    P8587 %in% c(6, 7, 8, 9, 10, 11) ~ 11 + P8587S1,
    P8587 %in% c(12, 13) ~ 16 + P8587S1,
    TRUE ~ NA_real_
  )) %>%
  mutate(REZAGO = case_when(
    (P6040 == 7 & EDU_YEARS_REZ < 1) | (P6040 == 8 & EDU_YEARS_REZ < 2) |
      (P6040 == 9 & EDU_YEARS_REZ < 3) | (P6040 == 10 & EDU_YEARS_REZ < 4) |
      (P6040 == 11 & EDU_YEARS_REZ < 5) | (P6040 == 12 & EDU_YEARS_REZ < 6) |
      (P6040 == 13 & EDU_YEARS_REZ < 7) | (P6040 == 14 & EDU_YEARS_REZ < 8) |
      (P6040 == 15 & EDU_YEARS_REZ < 9) | (P6040 == 16 & EDU_YEARS_REZ < 10) |
      (P6040 == 17 & EDU_YEARS_REZ < 11) ~ 1,
    TRUE ~ NA_real_
  )) %>%
  group_by(llavehog) %>%
  mutate(REZAGO_SUMA = sum(REZAGO, na.rm = TRUE),
         REZAGO_2 = ifelse(REZAGO_SUMA >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 4: Rezago escolar\n")

# Indicador 5: Barreras cuidado primera infancia ----
personas <- personas %>%
  mutate(salud = ifelse(P6040 <= 5 & P6090 != 1, 1, 0),
         CUIDADO = case_when(
           P6040 <= 4 & (P51 %in% c(3, 6, 7)) ~ 1,
           P6040 == 5 & P8586 == 2 ~ 1,
           TRUE ~ 0
         ),
         nutricion = case_when(
           P6040 <= 4 & P51 == 1 & P55 == 2 ~ 1,
           P6040 <= 4 & P51 == 1 & P55 == 1 & (P774 %in% c(1, 4)) ~ 1,
           P6040 == 5 & P8586 == 1 & P6180 == 2 ~ 1,
           TRUE ~ 0
         ),
         A_INTEGRAL = ifelse(salud == 1 | CUIDADO == 1 | nutricion == 1, 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(A_INTEGRAL_suma = sum(A_INTEGRAL, na.rm = TRUE),
         A_INTEGRAL_2 = ifelse(A_INTEGRAL_suma >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 5: Barreras cuidado primera infancia\n")

# Indicador 6: Trabajo infantil ----
personas <- personas %>%
  mutate(OCUPADO = ifelse(P6240 == 1 | P6250 == 1 | P6260 == 1 | P6270 == 1, 1, 0),
         TRABAJOINF = ifelse(P6040 >= 12 & P6040 <= 17 & OCUPADO == 1, 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(TRABAJOINF_SUMA = sum(TRABAJOINF, na.rm = TRUE),
         TRABAJOINF_2 = ifelse(TRABAJOINF_SUMA >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 6: Trabajo infantil\n\n")

# Guardar dimensión 2
hogares_2 <- personas %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ORDEN, llavehog,
         ASISTE_2, REZAGO_2, A_INTEGRAL_2, TRABAJOINF_2)

################################################################################
# DIMENSIÓN 3: TRABAJO
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("DIMENSIÓN 3: TRABAJO\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Indicador 7: Desempleo larga duración ----
personas <- base_final %>%
  mutate(DES_DURA = ifelse(P6040 >= 12 & P6351 == 1 & (P7250 >= 52 | P7250 == 999 | is.na(P7250)), 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(DES_DURA_SUMA = sum(DES_DURA, na.rm = TRUE),
         PEA = ifelse(P6240 == 1 | P6250 == 1 | P6260 == 1 | P6270 == 1 | P6351 == 1, 1, 0),
         PEA_SUMA = sum(PEA, na.rm = TRUE),
         PENSIONADO = ifelse(P6920 == 3, 1, 0),
         PENSIONADO_SUMA = sum(PENSIONADO, na.rm = TRUE),
         CUENTAMIEMBROS = n(),
         PENSION = ifelse(PENSIONADO_SUMA == CUENTAMIEMBROS, 1, 0),
         DES_DURA_2 = ifelse(DES_DURA_SUMA > 0 | PEA_SUMA == 0 | PENSION == 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 7: Desempleo larga duración\n")

# Indicador 8: Empleo informal ----
personas <- personas %>%
  mutate(OCUPADO_TRAB = ifelse(P6240 == 1 | P6250 == 1 | P6260 == 1 | P6270 == 1, 1, 0),
         TRABAJO_INF = ifelse(P6040 >= 12 & P6040 <= 17 & OCUPADO_TRAB == 1, 1, 0),
         EFORMAL = ifelse((P6240 == 1 | P6250 == 1 | P6260 == 1 | P6270 == 1) & 
                            (P6920 == 1 | P6920 == 3), 1, 0),
         EFORMAL = ifelse(TRABAJO_INF == 1, NA, EFORMAL)) %>%
  group_by(llavehog) %>%
  mutate(EFORMAL_SUMA = sum(EFORMAL, na.rm = TRUE),
         NUEVAPEA = if_else(is.na(PEA), 0, PEA),
         NUEVAPEA = ifelse(DES_DURA == 1 | TRABAJO_INF == 1, NA, NUEVAPEA),
         NUEVAPEA_SUMA = sum(NUEVAPEA, na.rm = TRUE),
         TASA_EMPLEO_FORMAL = EFORMAL_SUMA / NUEVAPEA_SUMA,
         EFORMAL_2 = coalesce(ifelse((TASA_EMPLEO_FORMAL != 1) & (PENSION != 1), 1, 0), 1)) %>%
  ungroup()

cat("✓ Indicador 8: Empleo informal\n\n")

# Guardar dimensión 3
hogares_3 <- personas %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ORDEN, llavehog,
         DES_DURA_2, EFORMAL_2)

################################################################################
# DIMENSIÓN 4: SALUD
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("DIMENSIÓN 4: SALUD\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Indicador 9: Sin aseguramiento salud ----
personas <- base_final %>%
  mutate(SEGURO_SALUD = ifelse(P6040 > 5 & (P6090 == 2 | P6090 == 9), 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(SEGURO_SALUD_SUMA = sum(SEGURO_SALUD, na.rm = TRUE),
         SEGURO_SALUD_2 = ifelse(SEGURO_SALUD_SUMA >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 9: Sin aseguramiento salud\n")

# Indicador 10: Barreras acceso salud ----
personas <- personas %>%
  mutate(SALUD_NEC = ifelse(P5665 == 1 & P8563 >= 3, 1, 0)) %>%
  group_by(llavehog) %>%
  mutate(SALUD_NEC_SUMA = sum(SALUD_NEC, na.rm = TRUE),
         SALUD_NEC_2 = ifelse(SALUD_NEC_SUMA >= 1, 1, 0)) %>%
  ungroup()

cat("✓ Indicador 10: Barreras acceso salud\n\n")

# Guardar dimensión 4
hogares_4 <- personas %>%
  select(DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, ORDEN, llavehog,
         SEGURO_SALUD_2, SALUD_NEC_2)

################################################################################
# DIMENSIÓN 5: VIVIENDA Y SERVICIOS PÚBLICOS
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("DIMENSIÓN 5: VIVIENDA Y SERVICIOS PÚBLICOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Cargar bases de hogares y viviendas
hogares <- read_csv("C:/Users/Jhoan meza/Desktop/BasesDatos-IMP-2024/Departamentos/Hogares (Departamental) 2024.csv")
viviendas <- read_csv("C:/Users/Jhoan meza/Desktop/BasesDatos-IMP-2024/Departamentos/Viviendas (Departamental) 2024.csv")

# Crear hogares_0
hogares_0 <- hogares %>%
  left_join(viviendas %>% select(DIRECTORIO, P3, P4005, P4015, P8520S3, P8520S5), 
            by = "DIRECTORIO") %>%
  rename(CLASE = P3) %>%
  mutate(CANT_PERSONAS_HOGAR = personas,
         llavehog = paste(DIRECTORIO, SECUENCIA_ENCUESTA, sep = "_"))

# Calcular indicadores de vivienda
hogares_5 <- hogares_0 %>%
  mutate(
    # Indicador 11: Sin acueducto
    ACUEDUCTO = case_when(
      CLASE == 1 & P8520S5 == 2 ~ 1,
      CLASE == 2 & (P8530 %in% c(4, 5, 6, 8, 9, 10)) ~ 1,
      TRUE ~ 0
    ),
    # Indicador 12: Sin alcantarillado
    ALCANTARILLADO = case_when(
      CLASE == 1 & P8520S3 == 2 ~ 1,
      CLASE == 2 & (P8526 %in% c(3, 5, 6)) ~ 1,
      TRUE ~ 0
    ),
    # Indicador 13: Pisos inadecuados
    PISOS = ifelse(P4015 == 7, 1, 0),
    # Indicador 14: Paredes inadecuadas
    PAREDES = case_when(
      CLASE == 1 & P4005 %in% c(5, 7, 8, 9, 10) ~ 1,
      CLASE == 2 & P4005 %in% c(7, 8, 9, 10) ~ 1,
      TRUE ~ 0
    ),
    # Indicador 15: Hacinamiento
    HAC = CANT_PERSONAS_HOGAR / P5010,
    HACINAMIENTO = case_when(
      CLASE == 1 & HAC >= 3 ~ 1,
      CLASE == 2 & HAC > 3 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  select(llavehog, DEPARTAMENTO, ACUEDUCTO, ALCANTARILLADO, PISOS, PAREDES, HACINAMIENTO)

cat("✓ Indicador 11: Sin acueducto\n")
cat("✓ Indicador 12: Sin alcantarillado\n")
cat("✓ Indicador 13: Pisos inadecuados\n")
cat("✓ Indicador 14: Paredes inadecuadas\n")
cat("✓ Indicador 15: Hacinamiento\n\n")

################################################################################
# UNIR TODAS LAS DIMENSIONES
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("UNIENDO DIMENSIONES Y CALCULANDO IPM\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Unir dimensiones 1-4 (nivel persona)
resultado <- hogares_1 %>%
  left_join(hogares_2, by = c("llavehog", "DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN")) %>%
  left_join(hogares_3, by = c("llavehog", "DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN")) %>%
  left_join(hogares_4, by = c("llavehog", "DIRECTORIO", "SECUENCIA_ENCUESTA", "SECUENCIA_P", "ORDEN")) %>%
  left_join(hogares_5, by = "llavehog")

cat("✓ Dimensiones unidas\n")
cat("  Registros:", nrow(resultado), "\n")
cat("  Variables:", ncol(resultado), "\n\n")

################################################################################
# CALCULAR IPM Y CLASIFICAR POBREZA
################################################################################

cat("Calculando IPM...\n")

resultado_ipm <- resultado %>%
  mutate(
    IPM = (LOGRO_2 * 0.10) + (ALFA_2 * 0.10) +
      (ASISTE_2 * 0.05) + (REZAGO_2 * 0.05) + 
      (A_INTEGRAL_2 * 0.05) + (TRABAJOINF_2 * 0.05) +
      (DES_DURA_2 * 0.10) + (EFORMAL_2 * 0.10) +
      (SEGURO_SALUD_2 * 0.10) + (SALUD_NEC_2 * 0.10) +
      (ACUEDUCTO * 0.04) + (ALCANTARILLADO * 0.04) +
      (PISOS * 0.04) + (PAREDES * 0.04) + (HACINAMIENTO * 0.04),
    POBRE = ifelse(IPM >= 0.33, 1, 0)
  )

cat("✓ IPM calculado\n\n")


###############################################################################
# DESCOMPOSICIÓN POR INDICADORES Y DIMENSIONES (ESTILO DANE / ALKIRE-FOSTER)
################################################################################
# (Pega aquí todo el código que te pasé: desde vars_indicadores hasta print(tabla_dimensiones))
################################################################################

################################################################################
# DESCOMPOSICIÓN POR INDICADORES Y DIMENSIONES (ALKIRE–FOSTER / ESTILO DANE)
################################################################################

# 1. Indicadores y pesos en el mismo orden que usas en el IPM
vars_indicadores <- c(
  "LOGRO_2", "ALFA_2",                    # Educación
  "ASISTE_2", "REZAGO_2", "A_INTEGRAL_2", "TRABAJOINF_2",  # Niñez y juventud
  "DES_DURA_2", "EFORMAL_2",              # Trabajo
  "SEGURO_SALUD_2", "SALUD_NEC_2",        # Salud
  "ACUEDUCTO", "ALCANTARILLADO", "PISOS", "PAREDES", "HACINAMIENTO"  # Vivienda
)

pesos_indicadores <- c(
  0.10, 0.10,   # Educación
  0.05, 0.05, 0.05, 0.05,   # Niñez y juventud
  0.10, 0.10,   # Trabajo
  0.10, 0.10,   # Salud
  0.04, 0.04, 0.04, 0.04, 0.04   # Vivienda
)

# 2. Población total expandida (N)
N_total <- sum(resultado_ipm$FEX_C, na.rm = TRUE)

# 3. Headcount censurado Hc_j:
#    proporción de la población que es pobre (POBRE==1) y está privada en el indicador j
Hc <- sapply(vars_indicadores, function(v) {
  num <- sum(resultado_ipm[[v]] * resultado_ipm$POBRE * resultado_ipm$FEX_C,
             na.rm = TRUE)
  num / N_total
})

# 4. IPM ajustado M0 a partir de la descomposición (debe ser muy cercano a ipm_nacional$ipm_promedio)
M0_from_decomp <- sum(Hc * pesos_indicadores)

# 5. Contribución porcentual de cada indicador al M0
contrib_indicadores <- (Hc * pesos_indicadores) / M0_from_decomp * 100

# 6. Construir tabla por indicador
tabla_indicadores <- tibble(
  Indicador = c(
    "Bajo logro educativo", "Analfabetismo",
    "Inasistencia escolar", "Rezago escolar",
    "Barreras primera infancia", "Trabajo infantil",
    "Desempleo larga duración", "Empleo informal",
    "Sin aseguramiento salud", "Barreras acceso salud",
    "Sin acueducto", "Sin alcantarillado",
    "Pisos inadecuados", "Paredes inadecuadas",
    "Hacinamiento"
  ),
  Dimension = c(
    "Educación", "Educación",
    "Niñez y juventud", "Niñez y juventud", "Niñez y juventud", "Niñez y juventud",
    "Trabajo", "Trabajo",
    "Salud", "Salud",
    "Vivienda y servicios", "Vivienda y servicios",
    "Vivienda y servicios", "Vivienda y servicios", "Vivienda y servicios"
  ),
  Peso = pesos_indicadores,
  Headcount_censurado = round(Hc * 100, 2),         # % población pobre y privada
  Contribucion_pct = round(contrib_indicadores, 2)  # % del M0 nacional
)

# 7. Tabla agregada por dimensión (esto es lo que se compara con el Gráfico 6 del DANE)
tabla_dimensiones <- tabla_indicadores %>%
  group_by(Dimension) %>%
  summarise(
    Contribucion_dimension = sum(Contribucion_pct),
    .groups = "drop"
  ) %>%
  mutate(
    Contribucion_dimension = round(Contribucion_dimension, 2)
  )

# 8. (Opcional) Tasas de privación ENTRE POBRES para cada indicador (descriptivo)
peso_pobres <- sum(resultado_ipm$POBRE * resultado_ipm$FEX_C, na.rm = TRUE)

tasa_privacion_pobres <- sapply(vars_indicadores, function(v) {
  num <- sum(resultado_ipm[[v]] * resultado_ipm$POBRE * resultado_ipm$FEX_C,
             na.rm = TRUE)
  num / peso_pobres * 100
})

tasa_privacion_pobres <- round(tasa_privacion_pobres, 2)

tabla_indicadores <- tabla_indicadores %>%
  mutate(Tasa_privacion_entre_pobres = tasa_privacion_pobres)

# 9. Mostrar resultados en consola
cat("═══════════════════════════════════════════════════════════\n")
cat("CONTRIBUCIONES POR INDICADOR (% del M0)\n")
cat("═══════════════════════════════════════════════════════════\n")
print(tabla_indicadores)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("CONTRIBUCIONES POR DIMENSIÓN (% del M0)\n")
cat("═══════════════════════════════════════════════════════════\n")
print(tabla_dimensiones)

# 10. (Opcional) Exportar a CSV
write_csv(tabla_indicadores, "descomposicion_indicadores_IPM.csv")
write_csv(tabla_dimensiones, "descomposicion_dimensiones_IPM.csv")










################################################################################
# CÁLCULO DE ESTADÍSTICAS PONDERADAS
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("RESULTADOS NACIONALES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# IPM Nacional ponderado
ipm_nacional <- resultado_ipm %>%
  summarise(
    total_personas = sum(FEX_C, na.rm = TRUE),
    personas_pobres = sum(POBRE * FEX_C, na.rm = TRUE),
    tasa_pobreza = (personas_pobres / total_personas) * 100,
    ipm_promedio = weighted.mean(IPM, FEX_C, na.rm = TRUE)
  )

cat("Población expandida:", format(round(ipm_nacional$total_personas), big.mark = ","), "personas\n")
cat("Personas pobres:", format(round(ipm_nacional$personas_pobres), big.mark = ","), "\n")
cat("Tasa de pobreza:", round(ipm_nacional$tasa_pobreza, 2), "%\n")
cat("IPM promedio:", round(ipm_nacional$ipm_promedio, 4), "\n\n")

cat("Comparación con DANE oficial:\n")
cat("  DANE: 11.5%\n")
cat("  Modelo:", round(ipm_nacional$tasa_pobreza, 2), "%\n")
cat("  Diferencia:", round(ipm_nacional$tasa_pobreza - 11.5, 2), "p.p.\n")
cat("  Error relativo:", round(abs(ipm_nacional$tasa_pobreza - 11.5) / 11.5 * 100, 1), "%\n\n")

# IPM por departamento
if("DEPARTAMENTO" %in% names(resultado_ipm)) {
  ipm_departamental <- resultado_ipm %>%
    group_by(DEPARTAMENTO) %>%
    summarise(
      total_personas = sum(FEX_C, na.rm = TRUE),
      personas_pobres = sum(POBRE * FEX_C, na.rm = TRUE),
      tasa_pobreza = (personas_pobres / total_personas) * 100,
      ipm_promedio = weighted.mean(IPM, FEX_C, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(tasa_pobreza))
  
  cat("═══════════════════════════════════════════════════════════\n")
  cat("TOP 10 DEPARTAMENTOS CON MAYOR POBREZA\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  print(head(ipm_departamental %>% 
               mutate(tasa_pobreza = round(tasa_pobreza, 2),
                      ipm_promedio = round(ipm_promedio, 4)) %>%
               select(DEPARTAMENTO, tasa_pobreza, ipm_promedio), 10))
  cat("\n")
}

################################################################################
# EXPORTAR RESULTADOS
################################################################################

cat("═══════════════════════════════════════════════════════════\n")
cat("EXPORTANDO RESULTADOS\n")
cat("═══════════════════════════════════════════════════════════\n\n")

write_csv(resultado_ipm, "IPM_personas_final.csv")
cat("✓ IPM_personas_final.csv\n")

if(exists("ipm_departamental")) {
  write_csv(ipm_departamental, "IPM_departamental_final.csv")
  cat("✓ IPM_departamental_final.csv\n")
}

resumen_nacional <- tibble(
  Indicador = c("Población total", "Personas pobres", "Tasa de pobreza (%)", 
                "IPM promedio", "DANE oficial (%)", "Diferencia (p.p.)"),
  Valor = c(
    format(round(ipm_nacional$total_personas), big.mark = ","),
    format(round(ipm_nacional$personas_pobres), big.mark = ","),
    round(ipm_nacional$tasa_pobreza, 2),
    round(ipm_nacional$ipm_promedio, 4),
    11.5,
    round(ipm_nacional$tasa_pobreza - 11.5, 2)
  )
)

write_csv(resumen_nacional, "resumen_nacional_IPM.csv")
cat("✓ resumen_nacional_IPM.csv\n\n")

################################################################################
# RESUMEN FINAL
################################################################################

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║           ✓ CÁLCULO IPM COMPLETADO EXITOSAMENTE ✓         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

validacion <- case_when(
  abs(ipm_nacional$tasa_pobreza - 11.5) <= 1.5 ~ "EXCELENTE",
  abs(ipm_nacional$tasa_pobreza - 11.5) <= 3.0 ~ "BUENO",
  TRUE ~ "REVISAR"
)

cat("🎯 Validación:", validacion, "\n")
cat("📊 Tasa de pobreza:", round(ipm_nacional$tasa_pobreza, 2), "%\n")
cat("📁 Archivos generados: 3\n\n")

cat("════════════════════════════════════════════════════════════\n")

cat("═══════════════════════════════════════\n")
cat("DIAGNÓSTICO COMPLETO\n")
cat("═══════════════════════════════════════\n\n")

# 1. Verificar base_final
cat("1. BASE_FINAL:\n")
cat("   Personas:", nrow(base_final), "\n")
cat("   Hogares únicos:", n_distinct(base_final$llavehog), "\n")
cat("   Viviendas únicas:", n_distinct(base_final$DIRECTORIO), "\n\n")

# 2. Verificar bases originales
personas_orig <- read_csv("C:/Users/Jhoan meza/Desktop/BasesDatos-IMP-2024/Departamentos/Personas (Departamental) 2024.csv")
hogares_orig <- read_csv("C:/Users/Jhoan meza/Desktop/BasesDatos-IMP-2024/Departamentos/Hogares (Departamental) 2024.csv")

cat("2. BASES ORIGINALES DEPARTAMENTALES:\n")
cat("   Personas:", nrow(personas_orig), "\n")
cat("   Hogares:", nrow(hogares_orig), "\n\n")

cat("3. COMPARACIÓN:\n")
cat("   Personas perdidas:", nrow(personas_orig) - nrow(base_final), "\n")
cat("   Hogares perdidos:", nrow(hogares_orig) - n_distinct(base_final$llavehog), "\n\n")

# 4. Verificar si resultado_ipm tiene todos los registros
cat("4. RESULTADO_IPM:\n")
cat("   Personas:", nrow(resultado_ipm), "\n")
cat("   Con IPM calculado:", sum(!is.na(resultado_ipm$IPM)), "\n")
cat("   Con NA en IPM:", sum(is.na(resultado_ipm$IPM)), "\n\n")

# 5. Verificar privaciones con NA
cat("5. PRIVACIONES CON NA:\n")
privaciones <- c("LOGRO_2", "ALFA_2", "ASISTE_2", "REZAGO_2", 
                 "A_INTEGRAL_2", "TRABAJOINF_2", "DES_DURA_2", "EFORMAL_2",
                 "SEGURO_SALUD_2", "SALUD_NEC_2", "ACUEDUCTO", "ALCANTARILLADO",
                 "PISOS", "PAREDES", "HACINAMIENTO")

for(var in privaciones) {
  na_count <- sum(is.na(resultado_ipm[[var]]))
  if(na_count > 0) {
    cat("   ", var, ":", na_count, "NA\n")
  }
}



































---------------------------------------------------------
  
  ################################################################################
# EXTRACCIÓN ORGANIZADA DE RESULTADOS IPM - PEGAR AL FINAL DEL CÓDIGO
################################################################################

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║           EXTRACCIÓN DE RESULTADOS PARA ANÁLISIS           ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# 1. RESUMEN NACIONAL COMPLETO
resumen_completo <- resultado_ipm %>%
  summarise(
    # Métricas principales
    Poblacion_Total = sum(FEX_C, na.rm = TRUE),
    Personas_Pobres = sum(POBRE * FEX_C, na.rm = TRUE),
    Tasa_Pobreza = (Personas_Pobres / Poblacion_Total) * 100,
    IPM_Promedio = weighted.mean(IPM, FEX_C, na.rm = TRUE),
    
    # Por cada indicador (tasas de privación)
    Privacion_Logro_Educativo = weighted.mean(LOGRO_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Analfabetismo = weighted.mean(ALFA_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Inasistencia = weighted.mean(ASISTE_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Rezago = weighted.mean(REZAGO_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Primera_Infancia = weighted.mean(A_INTEGRAL_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Trabajo_Infantil = weighted.mean(TRABAJOINF_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Desempleo = weighted.mean(DES_DURA_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Informalidad = weighted.mean(EFORMAL_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Sin_Salud = weighted.mean(SEGURO_SALUD_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Barreras_Salud = weighted.mean(SALUD_NEC_2, FEX_C, na.rm = TRUE) * 100,
    Privacion_Sin_Acueducto = weighted.mean(ACUEDUCTO, FEX_C, na.rm = TRUE) * 100,
    Privacion_Sin_Alcantarillado = weighted.mean(ALCANTARILLADO, FEX_C, na.rm = TRUE) * 100,
    Privacion_Pisos_Inadecuados = weighted.mean(PISOS, FEX_C, na.rm = TRUE) * 100,
    Privacion_Paredes_Inadecuadas = weighted.mean(PAREDES, FEX_C, na.rm = TRUE) * 100,
    Privacion_Hacinamiento = weighted.mean(HACINAMIENTO, FEX_C, na.rm = TRUE) * 100
  )

# 2. TABLA POR DIMENSIONES
dimensiones_resumen <- tibble(
  Dimension = c("EDUCACIÓN", "EDUCACIÓN", 
                "NIÑEZ Y JUVENTUD", "NIÑEZ Y JUVENTUD", "NIÑEZ Y JUVENTUD", "NIÑEZ Y JUVENTUD",
                "TRABAJO", "TRABAJO",
                "SALUD", "SALUD",
                "VIVIENDA", "VIVIENDA", "VIVIENDA", "VIVIENDA", "VIVIENDA"),
  
  Indicador = c("Bajo logro educativo", "Analfabetismo",
                "Inasistencia escolar", "Rezago escolar", "Primera infancia", "Trabajo infantil",
                "Desempleo larga duración", "Empleo informal",
                "Sin aseguramiento", "Barreras acceso",
                "Sin acueducto", "Sin alcantarillado", "Pisos inadecuados", "Paredes inadecuadas", "Hacinamiento"),
  
  Peso = c(0.10, 0.10, 0.05, 0.05, 0.05, 0.05, 0.10, 0.10, 0.10, 0.10, 0.04, 0.04, 0.04, 0.04, 0.04),
  
  Tasa_Privacion = c(
    resumen_completo$Privacion_Logro_Educativo,
    resumen_completo$Privacion_Analfabetismo,
    resumen_completo$Privacion_Inasistencia,
    resumen_completo$Privacion_Rezago,
    resumen_completo$Privacion_Primera_Infancia,
    resumen_completo$Privacion_Trabajo_Infantil,
    resumen_completo$Privacion_Desempleo,
    resumen_completo$Privacion_Informalidad,
    resumen_completo$Privacion_Sin_Salud,
    resumen_completo$Privacion_Barreras_Salud,
    resumen_completo$Privacion_Sin_Acueducto,
    resumen_completo$Privacion_Sin_Alcantarillado,
    resumen_completo$Privacion_Pisos_Inadecuados,
    resumen_completo$Privacion_Paredes_Inadecuadas,
    resumen_completo$Privacion_Hacinamiento
  )
) %>%
  mutate(
    Tasa_Privacion = round(Tasa_Privacion, 2),
    Contribucion_IPM = Peso * Tasa_Privacion / 100
  )

# 3. RESUMEN POR DEPARTAMENTO (si existe)
if("DEPARTAMENTO" %in% names(resultado_ipm)) {
  resumen_departamental <- resultado_ipm %>%
    group_by(DEPARTAMENTO) %>%
    summarise(
      Poblacion = sum(FEX_C, na.rm = TRUE),
      Personas_Pobres = sum(POBRE * FEX_C, na.rm = TRUE),
      Tasa_Pobreza = (Personas_Pobres / Poblacion) * 100,
      IPM_Promedio = weighted.mean(IPM, FEX_C, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(Tasa_Pobreza)) %>%
    mutate(across(c(Tasa_Pobreza, IPM_Promedio), ~round(., 2)))
}

# 4. IMPRIMIR RESULTADOS
cat("═══════════════════════════════════════════════════════════\n")
cat("RESULTADOS PRINCIPALES\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("\n📊 INDICADORES NACIONALES:\n")
cat("   • Tasa de pobreza:", round(resumen_completo$Tasa_Pobreza, 2), "%\n")
cat("   • IPM promedio:", round(resumen_completo$IPM_Promedio, 4), "\n")
cat("   • Población total:", format(round(resumen_completo$Poblacion_Total), big.mark = ","), "\n")
cat("   • Personas pobres:", format(round(resumen_completo$Personas_Pobres), big.mark = ","), "\n")

cat("\n📈 TOP 5 PRIVACIONES MÁS ALTAS:\n")
top_privaciones <- dimensiones_resumen %>% 
  arrange(desc(Tasa_Privacion)) %>% 
  head(5)
for(i in 1:5) {
  cat("   ", i, ". ", top_privaciones$Indicador[i], ": ", 
      top_privaciones$Tasa_Privacion[i], "%\n", sep = "")
}

if(exists("resumen_departamental")) {
  cat("\n🗺️ TOP 5 DEPARTAMENTOS CON MAYOR POBREZA:\n")
  top_deptos <- head(resumen_departamental, 5)
  for(i in 1:5) {
    cat("   ", i, ". ", top_deptos$DEPARTAMENTO[i], ": ", 
        top_deptos$Tasa_Pobreza[i], "%\n", sep = "")
  }
}

# 5. EXPORTAR RESULTADOS ORGANIZADOS
cat("\n💾 EXPORTANDO RESULTADOS...\n")

# Exportar a CSV
write_csv(dimensiones_resumen, "resultados_por_dimension.csv")
write_csv(resumen_completo %>% t() %>% as.data.frame() %>% rownames_to_column("Indicador"), 
          "resultados_nacionales.csv")

if(exists("resumen_departamental")) {
  write_csv(resumen_departamental, "resultados_departamentales.csv")
}

# Exportar a Excel (opcional, si tienes writexl)
if(require(writexl, quietly = TRUE)) {
  lista_hojas <- list(
    "Nacional" = resumen_completo %>% t() %>% as.data.frame() %>% rownames_to_column("Indicador"),
    "Por_Dimension" = dimensiones_resumen
  )
  if(exists("resumen_departamental")) {
    lista_hojas$Departamental <- resumen_departamental
  }
  write_xlsx(lista_hojas, "RESULTADOS_IPM_2024.xlsx")
  cat("   ✓ RESULTADOS_IPM_2024.xlsx\n")
}

cat("   ✓ Archivos CSV exportados\n")
cat("\n✅ EXTRACCIÓN COMPLETADA\n")




















#######################################



################################################################################
# GRÁFICO 3: DESCOMPOSICIÓN DEL IPM POR DIMENSIONES
# Usar los resultados ya calculados en tabla_dimensiones
################################################################################

library(ggplot2)
library(dplyr)
library(scales)

# --- IMPORTANTE: Este código va DESPUÉS de calcular tabla_dimensiones en tu script ---

# Verificar que tabla_dimensiones existe
if(!exists("tabla_dimensiones")) {
  stop("ERROR: tabla_dimensiones no existe. Asegúrate de ejecutar primero el cálculo del IPM.")
}

cat("\n═══════════════════════════════════════════════════════════\n")
cat("GENERANDO GRÁFICO DE DESCOMPOSICIÓN POR DIMENSIONES\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Mostrar los datos que vamos a graficar
cat("Datos de contribución por dimensión:\n")
print(tabla_dimensiones)
cat("\nSuma total:", sum(tabla_dimensiones$Contribucion_dimension), "%\n\n")

# Verificación
if(abs(sum(tabla_dimensiones$Contribucion_dimension) - 100) > 0.5) {
  warning("⚠️ La suma de contribuciones no es 100%. Verificar cálculos.")
}

# Ordenar de mayor a menor para el gráfico
datos_grafico <- tabla_dimensiones %>%
  arrange(desc(Contribucion_dimension)) %>%
  mutate(Dimension = factor(Dimension, levels = Dimension))

# --- GRÁFICO DE BARRAS ---
tema_pub <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    axis.title.y  = element_text(face = "bold", size = 12),
    axis.text.x   = element_text(angle = 15, hjust = 1, vjust = 1, size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    plot.caption = element_text(hjust = 0, color = "grey50", size = 9)
  )

p_descomp <- ggplot(datos_grafico, 
                    aes(x = Dimension, 
                        y = Contribucion_dimension, 
                        fill = Dimension)) +
  geom_col(width = 0.7, color = "white", alpha = 0.9) +
  geom_text(aes(label = paste0(round(Contribucion_dimension, 1), "%")), 
            vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_y_continuous(
    limits = c(0, max(datos_grafico$Contribucion_dimension) * 1.15),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    title = "Descomposición del IPM ajustado por dimensiones (M₀)",
    subtitle = "Contribución relativa calculada con metodología Alkire-Foster, ECV 2024",
    x = NULL,
    y = "Contribución al IPM (%)",
    caption = "Fuente: Elaboración propia con base en microdatos ECV - DANE (2024).\nCálculo siguiendo algoritmo oficial DANE con factor de expansión FEX_C."
  ) +
  tema_pub

# Guardar gráfico
out_dir <- "C:/Users/Jhoan meza/Documents/Graficos_Descriptivos"
dir.create(out_dir, showWarnings = FALSE)

ggsave(file.path(out_dir, "03_descomposicion_ipm_dimensiones.png"), 
       p_descomp, width = 10, height = 6, dpi = 300)

cat("✓ Gráfico guardado en:", file.path(out_dir, "03_descomposicion_ipm_dimensiones.png"), "\n")

# --- GRÁFICO ALTERNATIVO: BARRAS HORIZONTALES ---
p_horizontal <- ggplot(datos_grafico, 
                       aes(x = reorder(Dimension, Contribucion_dimension), 
                           y = Contribucion_dimension, 
                           fill = Dimension)) +
  geom_col(width = 0.75, color = "white", alpha = 0.9) +
  geom_text(aes(label = paste0(round(Contribucion_dimension, 1), "%")), 
            hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip() +
  scale_fill_grey(start = 0.3, end = 0.8) +
  scale_y_continuous(
    limits = c(0, max(datos_grafico$Contribucion_dimension) * 1.15),
    breaks = seq(0, 70, 10)
  ) +
  labs(
    title = "Descomposición del IPM ajustado por dimensiones (M₀)",
    subtitle = "Contribución relativa calculada con metodología Alkire-Foster, ECV 2024",
    x = NULL,
    y = "Contribución al IPM (%)",
    caption = "Fuente: Elaboración propia con base en microdatos ECV - DANE (2024)."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    axis.title.x  = element_text(face = "bold", size = 12),
    axis.text.y   = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    plot.caption = element_text(hjust = 0, color = "grey50", size = 9)
  )

ggsave(file.path(out_dir, "03_descomposicion_ipm_horizontal.png"), 
       p_horizontal, width = 10, height = 6, dpi = 300)

cat("✓ Gráfico horizontal guardado en:", file.path(out_dir, "03_descomposicion_ipm_horizontal.png"), "\n")

# --- TABLA PARA LA TESIS (formato LaTeX) ---
cat("\n═══════════════════════════════════════════════════════════\n")
cat("TABLA EN FORMATO LATEX\n")
cat("═══════════════════════════════════════════════════════════\n\n")

cat("\\begin{table}[h]\n")
cat("\\centering\n")
cat("\\caption{Descomposición del IPM por dimensiones, Colombia 2024}\n")
cat("\\label{tab:descomposicion_ipm}\n")
cat("\\begin{tabular}{lc}\n")
cat("\\hline\n")
cat("\\textbf{Dimensión} & \\textbf{Contribución (\\%)} \\\\\n")
cat("\\hline\n")

for(i in 1:nrow(datos_grafico)) {
  cat(sprintf("%s & %.1f \\\\\n", 
              datos_grafico$Dimension[i],
              datos_grafico$Contribucion_dimension[i]))
}

cat("\\hline\n")
cat(sprintf("\\textbf{Total} & %.1f \\\\\n", sum(datos_grafico$Contribucion_dimension)))
cat("\\hline\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

# --- EXPORTAR DATOS ---
write.csv(tabla_dimensiones, 
          file.path(out_dir, "tabla_descomposicion_dimensiones.csv"),
          row.names = FALSE)

cat("✓ Tabla exportada a CSV\n")

cat("\n✅ GRÁFICO DE DESCOMPOSICIÓN COMPLETADO\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Mostrar resumen final
cat("📊 RESUMEN DE CONTRIBUCIONES:\n\n")
for(i in 1:nrow(datos_grafico)) {
  cat(sprintf("   %s: %.1f%%\n", 
              datos_grafico$Dimension[i],
              datos_grafico$Contribucion_dimension[i]))
}
cat(sprintf("\n   TOTAL: %.1f%%\n", sum(datos_grafico$Contribucion_dimension)))


























# UNIR BASES Y ENRIQUECER CON VARIABLES SOCIOECONÓMICAS
resultado_ipm_ext <- resultado_ipm %>%
  left_join(
    base_final %>% 
      select(llavehog, DIRECTORIO, SECUENCIA_ENCUESTA, ORDEN, SECUENCIA_P,
             any_of(c("P6050", "P6040", "P6020", "P8587", "P1088", 
                      "CLASE", "ESTRATO", "P5090", "P9050"))),
    by = c("llavehog", "DIRECTORIO", "SECUENCIA_ENCUESTA", "ORDEN", "SECUENCIA_P")
  )

# CREAR BASE A NIVEL HOGAR - VERSIÓN CORREGIDA
hogares_ML_ready_enriquecida <- resultado_ipm_ext %>%
  group_by(llavehog) %>%  # Solo agrupar por llavehog
  summarise(
    # Identificación (tomar el primero)
    DEPARTAMENTO = first(DEPARTAMENTO),
    DIRECTORIO = first(DIRECTORIO),
    SECUENCIA_ENCUESTA = first(SECUENCIA_ENCUESTA),
    
    # Indicadores IPM (máximo del hogar = 1 si algún miembro tiene privación)
    LOGRO_2 = max(LOGRO_2, na.rm = TRUE),
    ALFA_2 = max(ALFA_2, na.rm = TRUE),
    ASISTE_2 = max(ASISTE_2, na.rm = TRUE),
    REZAGO_2 = max(REZAGO_2, na.rm = TRUE),
    A_INTEGRAL_2 = max(A_INTEGRAL_2, na.rm = TRUE),
    TRABAJOINF_2 = max(TRABAJOINF_2, na.rm = TRUE),
    DES_DURA_2 = max(DES_DURA_2, na.rm = TRUE),
    EFORMAL_2 = max(EFORMAL_2, na.rm = TRUE),
    SEGURO_SALUD_2 = max(SEGURO_SALUD_2, na.rm = TRUE),
    SALUD_NEC_2 = max(SALUD_NEC_2, na.rm = TRUE),
    ACUEDUCTO = first(ACUEDUCTO),  # Estos son de vivienda, son iguales para todo el hogar
    ALCANTARILLADO = first(ALCANTARILLADO),
    PISOS = first(PISOS),
    PAREDES = first(PAREDES),
    HACINAMIENTO = first(HACINAMIENTO),
    
    # Variables de salida
    IPM = mean(IPM, na.rm = TRUE),  # Promedio del IPM del hogar
    POBRE = max(POBRE, na.rm = TRUE),  # 1 si el hogar es pobre
    FEX_C = first(FEX_C),  # Factor de expansión del hogar
    
    # Variables del hogar
    TAMANO_HOGAR = n(),  # Contar miembros
    EDAD_PROMEDIO = mean(P6040, na.rm = TRUE),
    PROP_MUJERES = mean(if_else(P6020 == 2, 1, 0), na.rm = TRUE),
    
    # Educación - usar P8587 o P1088 según disponibilidad
    EDU_MAX = if(all(is.na(P8587))) {
      max(P1088, na.rm = TRUE)
    } else {
      max(P8587, na.rm = TRUE)
    },
    EDU_PROMEDIO = if(all(is.na(P8587))) {
      mean(P1088, na.rm = TRUE)
    } else {
      mean(P8587, na.rm = TRUE)
    },
    
    .groups = 'drop'
  ) %>%
  # Limpiar infinitos y NAs
  mutate(
    across(c(LOGRO_2:HACINAMIENTO, EDU_MAX), 
           ~if_else(is.infinite(.x) | is.na(.x), 0, .x))
  )

# VERIFICAR QUE ESTÁ CORRECTO
cat("\n✅ VERIFICACIÓN FINAL:\n")
cat("Hogares únicos (llavehog):", n_distinct(hogares_ML_ready_enriquecida$llavehog), "\n")
cat("Total de filas:", nrow(hogares_ML_ready_enriquecida), "\n")
cat("¿Una fila por hogar?:", 
    n_distinct(hogares_ML_ready_enriquecida$llavehog) == nrow(hogares_ML_ready_enriquecida), "\n\n")

# Ver distribución del tamaño del hogar
cat("Distribución tamaño del hogar:\n")
print(table(hogares_ML_ready_enriquecida$TAMANO_HOGAR))

# EXPORTAR BASE
write_csv(hogares_ML_ready_enriquecida, "hogares_ML.csv")
cat("\n✅ Base exportada: hogares_ML_ready_enriquecida.csv\n")
cat("   Hogares:", nrow(hogares_ML_ready_enriquecida), "\n")
cat("   Variables:", ncol(hogares_ML_ready_enriquecida), "\n")












