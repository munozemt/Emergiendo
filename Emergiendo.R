## El sistema público de atención prehospitalaria en la Ciudad de México ##

# Librerias

pacman::p_load(sf, knitr, kableExtra, tidyverse, grid)

# Bases de datos

Base_cruda_prehospital <- read_csv("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/Base_cruda_prehospital.csv")
Población <- read_csv("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/2_Gran_Gedad_09_CM.csv")
geojson <- st_read("https://raw.githubusercontent.com/munozemt/Emergiendo/refs/heads/main/limite-de-las-alcaldas.json")

# Cálculo del total de ambulancias y personal

Base_cruda_prehospital <- Base_cruda_prehospital %>%
  mutate(
    Personal = `TUM B` + `TUM I` + `TUM A` + TSU + Iatrotécnicos + Médicos + Enfermeros + `Sin formación`,
    Ambulancias = `Ambulancias básicas` + `Ambulancias avanzadas` + `Ambulancias de terapia intensiva`
  )

total_ambulancias <- sum(Base_cruda_prehospital$Ambulancias, na.rm = TRUE)
total_personal <- sum(Base_cruda_prehospital$Personal, na.rm = TRUE)  
  
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

# Creación de tablas de personal y ambulancias

personal <- Base_cruda_prehospital %>%
  select(-`Ambulancias básicas`, -`Ambulancias avanzadas`, -`Primer contacto`, -`Ambulancias de terapia intensiva`, -`Ambulancias`) %>%
  rename(Total = Personal) %>%
  mutate(across(everything(), ~ ifelse(. == 0, "-", .)))

ambulancias <- Base_cruda_prehospital %>%
  select(`Dependencia`, `Ambulancias básicas`, `Ambulancias avanzadas`, `Ambulancias de terapia intensiva`, `Ambulancias`) %>%
  rename(Total = Ambulancias) %>%
  mutate(across(everything(), ~ ifelse(. == 0, "-", .)))

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

# Proporción de población por alcaldía según regionalización y recursos de personal y ambulancias 

alcaldías_ERUM <- poblacion_total %>%
  filter(!Alcaldía %in% c("Iztapalapa", "Iztacalco", "Venustiano Carranza", "Benito Juárez")) %>%
  mutate(Proporcion = round((POB_TOTAL / sum(POB_TOTAL)) * 100, 2)) %>%
  mutate(Personal = round((Proporcion / 100) * 1232)) %>%
  mutate(Ambulancias = round((Proporcion / 100) * 52))

alcaldías_SAMU <- poblacion_total %>%
  filter(Alcaldía %in% c("Iztapalapa", "Iztacalco", "Venustiano Carranza", "Benito Juárez")) %>%
  mutate(Proporcion = round((POB_TOTAL / sum(POB_TOTAL)) * 100, 2)) %>%
  mutate(Personal = round((Proporcion / 100) * 99)) %>%
  mutate(Ambulancias = round((Proporcion / 100) * 16))

# Unión de Personal y Ambulancias por alcaldía 

personal_ambulancias <- data.frame(
  Alcaldía = c("Álvaro Obregón", "Azcapotzalco", "Benito Juárez", "Coyoacán", 
               "Cuajimalpa de Morelos", "Cuauhtémoc", "Gustavo A. Madero", 
               "Iztacalco", "Iztapalapa", "La Magdalena Contreras", 
               "Miguel Hidalgo", "Milpa Alta", "Tláhuac", "Tlalpan", 
               "Venustiano Carranza", "Xochimilco"),
  Ambulancias = c(12, 6, 11, 5, 7, 9, 13, 4, 22, 6, 10, 5, 5, 6, 3, 3)
)

# Unión de población total y poligonos georreferenciados

base_análisis <- poblacion_total %>%
  left_join(geojson %>% select(NOMGEO, geometry), by = c("Alcaldía" = "NOMGEO")) %>%
  left_join(personal_ambulancias, by = "Alcaldía")

# Tasas de ambulancias por cada 100,000 habitantes. 

base_análisis$tasa_ambulancia <- (base_análisis$Ambulancias / base_análisis$POB_TOTAL) * 100000

# Mapas de calor 

if (!inherits(base_análisis, "sf")) {
  base_análisis <- st_as_sf(base_análisis)
}

mapa_amb <- ggplot(data = base_análisis) +
  geom_sf(aes(fill = tasa_ambulancia), color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Tasa de Ambulancias") +
  geom_sf_text(aes(label = round(tasa_ambulancia, 2)), size = 3, color = "white") +
  theme_minimal() +
  labs(title = "Tasa de ambulancias por cada 100,000 habitantes en CDMX") +
  theme(
    legend.position = "bottom",
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    axis.text.x = element_blank(),   
    axis.text.y = element_blank(),   
    axis.ticks = element_blank(),    
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()   
  )

# Tabla de tasa de ambulancias por cada 100,000 habitantes

tasa_amb <- base_análisis %>%
  as.data.frame() %>%  
  select(Alcaldía, tasa_ambulancia) %>%
  mutate(tasa_ambulancia = round(tasa_ambulancia, 2)) %>%
  rename(`Tasa ambulancias` = tasa_ambulancia)

tabla_tasa_amb <- tasa_amb %>%
  kable(format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Impresión de mapas y tablas

# Tablas con porcentajes de ambulancias y proveedores por nivel

kable(porcentajes_ambulancias, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

kable(porcentajes_personal, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

kable(personal, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:ncol(personal), width = "auto", extra_css = "text-align: center;")

kable(ambulancias, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1:ncol(ambulancias), extra_css = "text-align: center;")

mapa_amb
tabla_tasa_amb

