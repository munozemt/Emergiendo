## El sistema público de atención prehospitalaria en la Ciudad de México ##

# Librerias

pacman::p_load(sf, knitr, kableExtra, tidyverse)

# Bases de datos

Base_cruda_prehospital <- read_csv("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/Base_cruda_prehospital.csv")
Población <- read_csv("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/2_Gran_Gedad_09_CM.csv")
geojson <- st_read("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/limite-de-las-alcaldas.json")

# Cálculo del total de ambulancias y personal

total_ambulancias <- sum(Base_cruda_prehospital$`Ambulancias básicas`, 
                         Base_cruda_prehospital$`Ambulancias avanzadas`, 
                         Base_cruda_prehospital$`Ambulancias de terapia intensiva`)

total_personal <- sum(Base_cruda_prehospital$`TUM B`, Base_cruda_prehospital$`TUM I`,
                      Base_cruda_prehospital$`TUM A`, Base_cruda_prehospital$TSU,
                      Base_cruda_prehospital$Iatrotécnicos, Base_cruda_prehospital$Médicos,
                      Base_cruda_prehospital$Enfermeros, Base_cruda_prehospital$`Sin formación`)

# Cálculo de los porcentajes de ambulancias según nivel de atención

porcentajes_ambulancias <- data.frame(
  Tipo = c("Básicas", "Avanzadas", "Terapia Intensiva"),
  Porcentaje = round(c(
    sum(Base_cruda_prehospital$`Ambulancias básicas`) / total_ambulancias * 100,
    sum(Base_cruda_prehospital$`Ambulancias avanzadas`) / total_ambulancias * 100,
    sum(Base_cruda_prehospital$`Ambulancias de terapia intensiva`) / total_ambulancias * 100
  ), 2)
)

# Cálculo de los porcentajes de proveedores según nivel de atención

porcentajes_personal <- data.frame(
  Tipo = c("TUM B", "TUM I", "TUM A", "TSU", "Iatrotécnicos*", "Médicos", "Enfermeros", "Sin formación"),
  Porcentaje = round(c(
    sum(Base_cruda_prehospital$`TUM B`) / total_personal * 100,
    sum(Base_cruda_prehospital$`TUM I`) / total_personal * 100,
    sum(Base_cruda_prehospital$`TUM A`) / total_personal * 100,
    sum(Base_cruda_prehospital$TSU) / total_personal * 100,
    sum(Base_cruda_prehospital$Iatrotécnicos) / total_personal * 100,
    sum(Base_cruda_prehospital$Médicos) / total_personal * 100,
    sum(Base_cruda_prehospital$Enfermeros) / total_personal * 100,
    sum(Base_cruda_prehospital$`Sin formación`) / total_personal * 100
  ), 2)
)

# Tablas con porcentajes de ambulancias y proveedores por nivel

cat("Porcentajes de ambulancias por nivel de atención:\n")
kable(porcentajes_ambulancias, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

cat("\nPorcentajes de personal por nivel:\n")
kable(porcentajes_personal, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

# Estimación de población (residentes) por alcaldía para el año 2024

Población_2024 <- Población %>%
  filter(AÑO == 2024) %>%
  select(NOM_MUN, POB_TOTAL) %>%
  group_by(NOM_MUN) %>%
  summarize(POB_TOTAL = sum(POB_TOTAL, na.rm = TRUE))

# Estimación de población flotante por alcaldía para el año 2023 

poblacion_flotante <- data.frame(
  Alcaldía = c("Álvaro Obregón", "Azcapotzalco", "Benito Juárez", "Coyoacán", 
               "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero", 
               "Iztacalco", "Iztapalapa", "La Magdalena Contreras", 
               "Miguel Hidalgo", "Milpa Alta", "Tláhuac", "Tlalpan", 
               "Venustiano Carranza", "Xochimilco"),
  Total = c(164087, 109800, 254181, 193246, 72462, 484330, 143118, 
            73926, 152774, 24433, 327820, 5517, 30470, 125041, 128949, 33531)
)

# Población total por alcaldía (residentes + flotante)

poblacion_total <- Población_2024 %>%
  rename(Alcaldía = NOM_MUN) %>%
  left_join(poblacion_flotante, by = "Alcaldía") %>%
  mutate(POB_TOTAL = POB_TOTAL + Total) %>%
  select(Alcaldía, POB_TOTAL)

# 


