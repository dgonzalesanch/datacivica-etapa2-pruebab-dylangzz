library(dplyr)
library(stringr)
library(sf)
library(geodata)
library(ggplot2)
library(tidyr)
library(forcats)
library(readr)

# --------------------------------------------------------------------------------------
# Funciones auxiliares para normalizar y hacer el mapa y clasificar los estados en zonas
# --------------------------------------------------------------------------------------

normaliza_entidad <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all(c("Á"="A","É"="E","Í"="I","Ó"="O","Ú"="U")) %>%
    recode(
      "CIUDAD DE MEXICO" = "DISTRITO FEDERAL",
      "COAHUILA DE ZARAGOZA" = "COAHUILA",
      "MICHOACAN DE OCAMPO" = "MICHOACAN",
      "VERACRUZ DE IGNACIO DE LA LLAVE" = "VERACRUZ"
    )
}

clasifica_zona <- function(entidad) {
  case_when(
    entidad %in% c("BAJA CALIFORNIA","SONORA","CHIHUAHUA","COAHUILA","NUEVO LEON","TAMAULIPAS") ~ "Frontera Norte",
    entidad %in% c("DISTRITO FEDERAL","MEXICO","QUERETARO","GUANAJUATO","HIDALGO","MORELOS","PUEBLA","TLAXCALA") ~ "Centro",
    entidad %in% c("AGUASCALIENTES","DURANGO","SAN LUIS POTOSI","ZACATECAS") ~ "Centro Norte",
    entidad %in% c("BAJA CALIFORNIA SUR","NAYARIT","SINALOA") ~ "Pacifico Norte",
    entidad %in% c("COLIMA","GUERRERO","JALISCO","MICHOACAN","OAXACA") ~ "Pacifico Sur",
    entidad %in% c("CAMPECHE","CHIAPAS","QUINTANA ROO","TABASCO") ~ "Frontera Sur",
    entidad %in% c("YUCATAN","VERACRUZ") ~ "Sureste"
  )
}

# ----------------------------------------------------------------------------------
# Base principal (si no filtramos por extranjeros el numero de datos es 33,241 que 
# coincide con el boletin del INEGI )
# ----------------------------------------------------------------------------------

df <- read_csv(
  "conjunto_de_datos_edr2024_csv/conjunto_de_datos/conjunto_de_datos_defunciones_registradas24_csv.csv",
  show_col_types = FALSE
) %>%
  filter(
    lista_mex == 55,
    !(edad_agru == 30 & sexo == 9 & nacionalid == 9 & escolarida == 99 &
        edo_civil == 9 & par_agre == 99 & ocurr_trab == 9 & area_ur == 9),
    nacionalid == 2,
    str_starts(nacesp_cve, "2"),
    !nacesp_cve %in% c("213","221","260")
  ) %>%
  transmute(
    entidad_registro = ent_regis,
    pais = nacesp_cve,
    lugar_ocurrencia = lugar_ocur,
    area_urbana_rural = area_ur
  )

# ---------------------------------------------------------
# Lectura y limpieza de catalogos  
# ---------------------------------------------------------

entidades <- read_csv(
  "conjunto_de_datos_edr2024_csv/catalogos/entidad_municipio_localidad_2024.csv",
  show_col_types = FALSE
) %>%
  filter(cve_mun == "000") %>%
  transmute(entidad_registro = cve_ent, entidad = nom_loc)

paises <- read_csv(
  "conjunto_de_datos_edr2024_csv/catalogos/paises.csv",
  show_col_types = FALSE
) %>%
  transmute(pais = cve, pais_nombre = descrip)

lugar_cat <- read_csv(
  "conjunto_de_datos_edr2024_csv/catalogos/lugar_ocurrencia.csv",
  show_col_types = FALSE
) %>%
  rename(X = ...3) %>%   # ← CLAVE
  mutate(
    lugar = if_else(
      !is.na(X) & X != "",
      paste0(str_trim(DESCRIP), ", ", str_trim(X)),
      str_trim(DESCRIP)
    )
  ) %>%
  transmute(
    lugar_ocurrencia = CVE,
    lugar
  )


area_cat <- read_csv(
  "conjunto_de_datos_edr2024_csv/catalogos/area_urbana_rural.csv",
  show_col_types = FALSE
) %>%
  transmute(area_urbana_rural = CVE, area = DESCRIP)

# ---------------------------------------------------------
# MAPA: homicidios por zona y estado
# ---------------------------------------------------------

# =========================
# Datos por entidad
# =========================
df_entidad <- df %>%
  left_join(entidades, by = "entidad_registro") %>%
  mutate(entidad = normaliza_entidad(entidad)) %>%
  count(entidad, name = "homicidios")

# =========================
# Mapa base por estado
# =========================
mapa <- gadm(country = "MEX", level = 1, path = tempdir()) %>%
  st_as_sf() %>%
  mutate(
    entidad = normaliza_entidad(NAME_1),
    zona = clasifica_zona(entidad)
  ) %>%
  left_join(df_entidad, by = "entidad") %>%
  mutate(homicidios = replace_na(homicidios, 0))

# =========================
# Mapa agregado por zona
# =========================
mapa_zonas <- mapa %>%
  group_by(zona) %>%
  summarise(homicidios_zona = sum(homicidios), .groups = "drop")

# =========================
# Centroides de estados (solo con datos)
# =========================
centroides <- mapa %>%
  filter(homicidios > 0) %>%
  st_centroid() %>%
  cbind(st_coordinates(.))

# =========================
# Centroides de zonas (ajustados)
# =========================
zonas_centroides <- mapa_zonas %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  mutate(
    X = X + case_when(
      zona %in% c("Centro", "Pacifico Sur") ~ 1,
      zona == "Frontera Sur" ~ 2,
      TRUE ~ 0
    ),
    Y = Y - case_when(
      zona %in% c("Pacifico Norte", "Pacifico Sur") ~ 3,
      zona == "Frontera Sur" ~ 2,
      TRUE ~ 0
    )
  )

# =========================
# Líneas Pacífico Norte / Sureste
# =========================
centroides_all <- mapa %>%
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  st_drop_geometry()

lineas_zonas <- tibble(
  x_start = c(
    rep(zonas_centroides$X[zonas_centroides$zona == "Pacifico Norte"], 2),
    rep(zonas_centroides$X[zonas_centroides$zona == "Sureste"], 2)
  ),
  y_start = c(
    rep(zonas_centroides$Y[zonas_centroides$zona == "Pacifico Norte"] + 0.5, 2),
    rep(zonas_centroides$Y[zonas_centroides$zona == "Sureste"] - 0.5, 2)
  ),
  x_end = c(
    centroides_all$X[centroides_all$entidad == "BAJA CALIFORNIA SUR"],
    centroides_all$X[centroides_all$entidad == "SINALOA"],
    centroides_all$X[centroides_all$entidad == "VERACRUZ"],
    centroides_all$X[centroides_all$entidad == "YUCATAN"]
  ),
  y_end = c(
    centroides_all$Y[centroides_all$entidad == "BAJA CALIFORNIA SUR"],
    centroides_all$Y[centroides_all$entidad == "SINALOA"],
    centroides_all$Y[centroides_all$entidad == "VERACRUZ"],
    centroides_all$Y[centroides_all$entidad == "YUCATAN"]
  )
)

# =========================
# Gráfico final
# =========================
ggplot() +
  geom_sf(data = mapa_zonas, aes(fill = homicidios_zona), color = NA) +
  geom_sf(data = mapa, fill = NA, color = "white", linewidth = 0.3) +
  geom_sf(data = mapa_zonas, fill = NA, color = "black", linewidth = 0.8) +
  geom_segment(
    data = lineas_zonas,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_point(
    data = centroides,
    aes(X, Y, size = homicidios),
    color = "yellow",
    alpha = 0.8
  ) +
  geom_text(
    data = centroides,
    aes(X, Y, label = homicidios),
    size = 2,
    fontface = "bold",
    nudge_y = 0.3
  ) +
  geom_text(
    data = zonas_centroides,
    aes(X, Y, label = zona),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_gradient(low = "mistyrose", high = "darkred") +
  scale_size(range = c(0, 15), guide = "none") +
  labs(
    title = "Homicidios de migrantes por zona y estado (2024)",
    caption = "INEGI, EDR 2024"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# ---------------------------------------------------------
# HEATMAP: región de origen vs fallecimiento
# ---------------------------------------------------------

df_heat <- df %>%
  mutate(pais = as.character(pais)) %>%
  left_join(paises, by = "pais") %>%
  left_join(entidades, by = "entidad_registro") %>%
  mutate(
    entidad = normaliza_entidad(entidad),
    region_fallecimiento = clasifica_zona(entidad),
    region_origen = case_when(
      pais_nombre %in% c("Belice","República de Guatemala","República de Honduras",
                         "República de El Salvador","República de Nicaragua","República de Costa Rica") ~ "Centroamérica",
      pais_nombre %in% c("República de Cuba","República de Haití","República Dominicana") ~ "Caribe",
      pais_nombre %in% c("República de Colombia","República Bolivariana de Venezuela",
                         "República del Ecuador","República del Perú","República Argentina") ~ "Sudamérica",
      TRUE ~ "Otros"
    )
  ) %>%
  count(region_fallecimiento, region_origen) %>%
  complete(region_fallecimiento, region_origen, fill = list(n = 0)) %>%
  group_by(region_origen) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(df_heat, aes(region_origen, region_fallecimiento, fill = prop)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(n == 0, "", n))) +
  scale_fill_gradient(low = "grey95", high = "darkred", labels = scales::percent) +
  labs(title = "Región de fallecimiento vs origen",
       fill = "Proporción") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# ---------------------------------------------------------
# BARRAS: lugar de ocurrencia vs área urbana
# ---------------------------------------------------------

df_barras <- df %>%
  left_join(lugar_cat, by = "lugar_ocurrencia") %>%
  left_join(area_cat, by = "area_urbana_rural") %>%
  mutate(
    lugar = if_else(
      lugar %in% c(
        "Área industrial (taller, fabrica u obra)",
        "Granja (rancho o parcela)",
        "Área comercial o de servicios"
      ),
      "Otro", lugar
    ),
    area = factor(
      area,
      levels = c("Urbana", "No especificada", "Rural")
    ),
    lugar = factor(
      lugar,
      levels = c(
        "Vivienda particular",
        "Otro",
        "Se ignora",
        "Calle o carretera (vía pública)"
      )
    )
  ) %>%
  count(area, lugar)

ggplot(df_barras, aes(area, n, fill = lugar)) +
  geom_col(color = "grey30", linewidth = 0.3) +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Lugar de ocurrencia de homicidios de migrantes",
    subtitle = "Distribución por área urbana y rural",
    x = "Área urbana / rural",
    y = "Número de casos",
    fill = "Lugar de ocurrencia"
  ) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())
