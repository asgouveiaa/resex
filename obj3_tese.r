library(sf)
library(tidyverse)
library(ggpubr)

# setwd("./Documents/git/resex/")

usosolo <- terra::rast("./resex2016.tif")
usosolo <- as.data.frame(usosolo,xy=T)

usosolo <- usosolo %>% filter(classification_2016 != 0) %>%
  mutate(class = case_when(
    classification_2016 == 3 ~ "Formação Florestal",
    classification_2016 == 9 ~ "Agropecuária",
    classification_2016 == 21 ~ "Agropecuária",
    classification_2016 == 15 ~ "Agropecuária",
    classification_2016 == 41 ~ "Agropecuária",
    classification_2016 == 29 ~ "Afloramento Rochoso",
    classification_2016 == 39 ~ "Agropecuária",
    classification_2016 == 33 ~ "Rio, Lago e Oceano",
    classification_2016 == 49 ~ "Restinga Arbórea",
    classification_2016 == 5 ~ "Mangue",
    classification_2016 == 40 ~ "Agropecuária",
    classification_2016 == 25 ~ "Outras Áreas não Vegetadas",
    classification_2016 == 24 ~ "Área Urbanizada",
    classification_2016 == 30 ~ "Mineração",
    classification_2016 == 31 ~ "Aquicultura",
    classification_2016 == 6 ~ "Floresta Alagável",
    classification_2016 == 11 ~ "Campo Alagado e Área Pantanosa",
    classification_2016 == 12 ~ "Formação Campestre",
    classification_2016 == 4 ~ "Mangue",
  ))

mapbiomas_colors <- c(
  "Formação Florestal" = "#1f8d49", 
  "Mangue" = "#04381d",    
  "Agropecuária" = "#ffefc3",    
  "Área Urbanizada" = "#d4271e",   
  "Outras Áreas não Vegetadas" = "#db4d4f",   
  "Afloramento Rochoso" = "#ffaa5f",  
  "Mineração" = "#9c0027",    
  "Aquicultura" = "#091077",    
  "Rio, Lago e Oceano" = "#2532e4",    
  "Restinga Arbórea" = "#02d659",
  "Floresta Alagável" = "#007785",
  "Campo Alagado e Área Pantanosa" = "#519799",
  "Formação Campestre" = "#d6bc74",
  "Mangue" = "#04381d"
)



# calcculo F(x) 

# 1. Classes floresta
classes_floresta <- c(
  "Formação Florestal",
  "Floresta Alagável", 
  "Mangue",
  "Campo Alagado e Área Pantanosa"
)

# 2. Converter usosolo para sf e classificar
usosolo_sf <- usosolo %>%
  mutate(is_forest = as.integer(class %in% classes_floresta)) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# 3. Pontos das comunidades
pontos <- pontos_limpo %>%
  select(Comunidade, lon, lat, geom) %>%
  st_as_sf(crs = 4326)

# salvando mapa
p_usosolo <- ggplot() +
  geom_tile(data=usosolo,
            aes(x,y, fill = class)) +
  geom_point(data=pontos %>%
               filter(Comunidade != "Escrivão"),
             aes(lon,lat), shape = 17) +
  scale_fill_manual(values = mapbiomas_colors) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", fill = "Uso solo (Classificação 2016)") + 
  coord_sf(datum = sf::st_crs(4326))

ggsave("./plots/usosolo_resex.png",p_usosolo, height = 5, width = 6)


# 4. Reprojetar para métrico
crs_metro <- 31981

pontos_m   <- st_transform(pontos, crs_metro)
usosolo_m  <- st_transform(usosolo_sf, crs_metro)

# 5. Criar buffers para todas as comunidades de uma vez
buffers <- st_buffer(pontos_m, dist = 5000) %>%
  select(Comunidade)


# salvando mapa
p_buffers <- ggplot() +
  geom_tile(data=usosolo,
            aes(x,y, fill = class)) +
  geom_point(data=pontos %>%
               filter(Comunidade != "Escrivão"),
             aes(lon,lat), shape = 17) +
  geom_sf(data=buffers %>% st_transform(crs = 4326) %>%
               filter(Comunidade != "Escrivão"), alpha = 0.4) +
  scale_fill_manual(values = mapbiomas_colors) +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude", fill = "Uso solo (Classificação 2016)") + 
  coord_sf(datum = sf::st_crs(4326))

ggsave("./plots/buffers_resex.png",p_buffers, height = 5, width = 6)


# 6. Join espacial — associa cada pixel ao buffer que o contém
#    muito mais rápido que loop
pixels_por_buffer <- st_join(
  usosolo_m,
  buffers,
  join = st_within
)

# 7. Agregar por comunidade
fx_df <- pixels_por_buffer %>%
  st_drop_geometry() %>%
  filter(!is.na(Comunidade)) %>%
  group_by(Comunidade) %>%
  summarise(
    n_pixels_total  = n(),
    n_pixels_forest = sum(is_forest),
    C_f = n_pixels_forest / n_pixels_total,
    F_x = 1 - C_f,
    .groups = "drop"
  ) %>%
  left_join(
    pontos %>% st_drop_geometry() %>% select(Comunidade, lon, lat),
    by = "Comunidade"
  )

# 8. Resultado
print(fx_df)

# 9. Visualização
i_fragmentacao <- fx_df %>% filter(Comunidade != "Escrivão") %>% 
  ggplot(aes(x = reorder(Comunidade, F_x), y = F_x)) +
  geom_col(fill = "#993C1D", alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  coord_flip() +
  labs(
    title = "Índice de fragmentação F(x) por comunidade",
    subtitle = "RESEX Tapajós-Arapiuns · raio = 5km",
    x = NULL,
    y = "F(x) — proporção não-florestal"
  ) +
  theme_bw(base_size = 12)

# write.csv(fx_df,file = "./frgamentação.csv")

ggsave("./plots/i_fragmentacao.png",i_fragmentacao, height = 5, width = 6)



# =============================================================================
# OBJETIVO 3 — Exemplo com dados fictícios
# Calcula Γ_Ev(x) e Γ_My(x) para cada comunidade
# =============================================================================

# -----------------------------------------------------------------------------
# 1. DADOS DAS COMUNIDADES — substitua pelos seus dados reais
#    F_x vem do cálculo anterior (fx_df)
#    h_obs e T_exp vêm dos dados RESEX
#    d_f vem da transformada de distância (calcularemos depois)
# -----------------------------------------------------------------------------

set.seed(42)
n_com <- 32

comunidades <- tibble(
  Comunidade = paste0("Comunidade_", 1:n_com),
  
  # Do cálculo anterior — aqui fictício
  F_x   = runif(n_com, 0.05, 0.80),
  
  # Dos dados RESEX — expedições por caçador por mês
  h_obs = runif(n_com, 1, 6),
  
  # Dos dados RESEX — duração média da expedição em horas
  T_exp = runif(n_com, 3, 10),
  
  # Da transformada de distância — distância ao fragmento mais próximo (km)
  d_f   = runif(n_com, 0, 8),
  
  # Número de caçadores por comunidade — dos dados RESEX
  H     = sample(5:40, n_com, replace = TRUE)
)

# -----------------------------------------------------------------------------
# 2. PARÂMETROS — da literatura
# -----------------------------------------------------------------------------

params <- list(
  # E. vogeli
  pi0_Ev  = 0.10,   # prevalência basal em C. paca
  delta   = 0.50,   # amplificação por cascata trófica
  gamma_P = 0.05,   # prob. transmissão via processamento
  gamma_O = 0.10,   # prob. transmissão via consumo oral
  
  # Mayaro
  pi0_My  = 0.15,   # prevalência basal no reservatório
  beta_F  = 2.0,    # curvatura da resposta unimodal
  beta_v  = 0.08,   # prob. transmissão por picada infectante
  
  # Exposição vetorial
  d0      = 3.0     # distância característica de decaimento (km)
)

# -----------------------------------------------------------------------------
# 3. MODELO DE NICHO — P_occ(x) fictício
#    Na prática vem do MaxEnt
#    Aqui simulamos como função decrescente de F(x) para paca
#    e unimodal para Didelphis
# -----------------------------------------------------------------------------

comunidades <- comunidades %>%
  mutate(
    # Paca: prefere floresta densa → P_occ cai com fragmentação
    P_occ_Ev = exp(-2 * F_x),
    
    # Didelphis: generalista → P_occ unimodal com F(x)
    P_occ_My = F_x * exp(-params$beta_F * F_x) /
      (1/params$beta_F * exp(-1))  # normalizado pelo pico
  )

# -----------------------------------------------------------------------------
# 4. PREVALÊNCIA NO RESERVATÓRIO — A_I/A(x)
# -----------------------------------------------------------------------------

comunidades <- comunidades %>%
  mutate(
    # E. vogeli — monotonicamente crescente com F(x)
    AI_A_Ev = params$pi0_Ev * (1 + params$delta * F_x) * P_occ_Ev,
    
    # Mayaro — unimodal com F(x)
    AI_A_My = params$pi0_My * (1 + F_x * exp(-params$beta_F * F_x)) * P_occ_My
  )

# -----------------------------------------------------------------------------
# 5. TEMPO EFETIVO NA FLORESTA — m(x) — só Mayaro
# -----------------------------------------------------------------------------

comunidades <- comunidades %>%
  mutate(
    phi_x = exp(-d_f / params$d0),
    m_x   = T_exp * phi_x
  )

# -----------------------------------------------------------------------------
# 6. FORÇAS DE INFECÇÃO — Γ_Ev(x) e Γ_My(x)
# -----------------------------------------------------------------------------

comunidades <- comunidades %>%
  mutate(
    # E. vogeli — depende de h_obs e prevalência no reservatório
    Gamma_Ev = h_obs * (params$gamma_P + params$gamma_O) * AI_A_Ev,
    
    # Mayaro — depende do tempo na floresta e prevalência vetorial
    Gamma_My = params$beta_v * m_x * AI_A_My,
    
    # Patógeno dominante por comunidade
    dominante = case_when(
      Gamma_Ev > Gamma_My * 1.2 ~ "E. vogeli",
      Gamma_My > Gamma_Ev * 1.2 ~ "Mayaro",
      TRUE                       ~ "Comparável"
    )
  )

# -----------------------------------------------------------------------------
# 7. VISUALIZAÇÕES
# -----------------------------------------------------------------------------

cores <- c("E. vogeli" = "#0F6E56", "Mayaro" = "#BA7517", "Comparável" = "#888780")

# --- 7.1 Γ_Ev por comunidade ---
p_ev <- comunidades %>%
  ggplot(aes(x = reorder(Comunidade, Gamma_Ev),
             y = Gamma_Ev, fill = dominante)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = cores) +
  coord_flip() +
  labs(
    title = "Índice de risco — E. vogeli",
    x = NULL, y = "Γ_Ev(x)", fill = "Risco dominante"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

# --- 7.2 Γ_My por comunidade ---
p_my <- comunidades %>%
  ggplot(aes(x = reorder(Comunidade, Gamma_My),
             y = Gamma_My, fill = dominante)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = cores) +
  coord_flip() +
  labs(
    title = "Índice de risco — Mayaro",
    x = NULL, y = "Γ_My(x)", fill = "Risco dominante"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

# --- 7.3 Scatter Γ_Ev vs Γ_My ---
p_scatter <- comunidades %>%
  ggplot(aes(x = Gamma_Ev, y = Gamma_My,
             color = dominante, label = Comunidade)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "gray60") +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_manual(values = cores) +
  labs(
    title = "Γ_Ev(x) vs Γ_My(x)",
    subtitle = "Acima da diagonal: Mayaro domina · Abaixo: E. vogeli domina",
    x = "Γ_Ev(x) — E. vogeli",
    y = "Γ_My(x) — Mayaro",
    color = "Risco dominante"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# --- 7.4 Γ_Ev e Γ_My vs F(x) ---
p_fx <- comunidades %>%
  select(Comunidade, F_x, Gamma_Ev, Gamma_My) %>%
  pivot_longer(cols = c(Gamma_Ev, Gamma_My),
               names_to = "patogeno",
               values_to = "Gamma") %>%
  mutate(patogeno = recode(patogeno,
                           Gamma_Ev = "E. vogeli",
                           Gamma_My = "Mayaro")) %>%
  ggplot(aes(x = F_x, y = Gamma, color = patogeno)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("E. vogeli" = "#0F6E56",
                                "Mayaro"    = "#BA7517")) +
  labs(
    title = "Risco de spillover vs fragmentação",
    subtitle = "E. vogeli sobe monotonicamente · Mayaro resposta unimodal",
    x = "F(x) — índice de fragmentação",
    y = "Índice de risco Γ(x)",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

# --- 7.5 Painel final ---
fig_obj3 <- ggarrange(
  p_scatter, p_fx,
  p_ev,      p_my,
  ncol = 2, nrow = 2,
  labels = "auto"
)

print(fig_obj3)

# -----------------------------------------------------------------------------
# 8. TABELA RESUMO
# -----------------------------------------------------------------------------

comunidades %>%
  select(Comunidade, F_x, h_obs, T_exp, d_f,
         AI_A_Ev, AI_A_My, Gamma_Ev, Gamma_My, dominante) %>%
  arrange(desc(Gamma_Ev)) %>%
  print(n = 32)

library(sf)

# =============================================================================
# MAPAS DO OBJETIVO 3
# =============================================================================

# =============================================================================
# RECRIAR comunidades com nomes reais
# =============================================================================

set.seed(42)

nomes_reais <- pontos_limpo %>%
  st_drop_geometry() %>%
  pull(Comunidade)

n_com <- length(nomes_reais)

comunidades <- tibble(
  Comunidade = nomes_reais,
  F_x   = runif(n_com, 0.05, 0.80),
  h_obs = runif(n_com, 1, 6),
  T_exp = runif(n_com, 3, 10),
  d_f   = runif(n_com, 0, 8),
  H     = sample(5:40, n_com, replace = TRUE)
) %>%
  mutate(
    # P_occ
    P_occ_Ev = exp(-2 * F_x),
    P_occ_My = F_x * exp(-params$beta_F * F_x) /
      (1/params$beta_F * exp(-1)),
    # Prevalência
    AI_A_Ev = params$pi0_Ev * (1 + params$delta * F_x) * P_occ_Ev,
    AI_A_My = params$pi0_My * (1 + F_x * exp(-params$beta_F * F_x)) * P_occ_My,
    # Exposição vetorial
    phi_x = exp(-d_f / params$d0),
    m_x   = T_exp * phi_x,
    # Forças de infecção
    Gamma_Ev = h_obs * (params$gamma_P + params$gamma_O) * AI_A_Ev,
    Gamma_My = params$beta_v * m_x * AI_A_My,
    # Dominante
    dominante = case_when(
      Gamma_Ev > Gamma_My * 1.2 ~ "E. vogeli",
      Gamma_My > Gamma_Ev * 1.2 ~ "Mayaro",
      TRUE                       ~ "Comparável"
    )
  )

# Juntar com geometria
comunidades_sf <- pontos_limpo %>%
  select(Comunidade, geom) %>%
  left_join(comunidades, by = "Comunidade") %>%
  st_as_sf()

# Checar
comunidades_sf %>%
  select(Comunidade, F_x, Gamma_Ev, Gamma_My, dominante) %>%
  print(n = 32)

# 1. Juntar resultados com geometria dos pontos
comunidades_sf <- pontos_limpo %>%
  select(Comunidade, geom) %>%
  left_join(comunidades, by = "Comunidade") %>%
  st_as_sf()

# 2. Garantir mesmo CRS do shapefile da reserva
comunidades_sf <- st_transform(comunidades_sf, st_crs(reserva_shp))

# =============================================================================
# MAPA 1 — F(x) por comunidade
# =============================================================================

m_fx <- ggplot() +
  geom_sf(data = reserva_shp, fill = "#C0DD97", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = F_x, size = F_x), alpha = 0.9) +
  scale_color_gradient(low = "#0F6E56", high = "#993C1D",
                       name = "F(x)") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  labs(
    title = "Fragmentação da paisagem F(x)",
    subtitle = "Raio = 5km · RESEX Tapajós-Arapiuns"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

# =============================================================================
# MAPA 2 — Γ_Ev(x) — risco E. vogeli
# =============================================================================

m_ev <- ggplot() +
  geom_sf(data = reserva_shp, fill = "#E1F5EE", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = Gamma_Ev, size = Gamma_Ev), alpha = 0.9) +
  scale_color_gradient(low = "#9FE1CB", high = "#085041",
                       name = "Γ_Ev(x)") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  labs(
    title = "Risco de spillover — E. vogeli",
    subtitle = "Tamanho e cor · Γ_Ev(x)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

# =============================================================================
# MAPA 3 — Γ_My(x) — risco Mayaro
# =============================================================================

m_my <- ggplot() +
  geom_sf(data = reserva_shp, fill = "#FAEEDA", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = Gamma_My, size = Gamma_My), alpha = 0.9) +
  scale_color_gradient(low = "#FAC775", high = "#412402",
                       name = "Γ_My(x)") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  labs(
    title = "Risco de spillover — Mayaro",
    subtitle = "Tamanho e cor · Γ_My(x)"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

# =============================================================================
# MAPA 4 — Patógeno dominante por comunidade
# =============================================================================

cores_dom <- c(
  "E. vogeli"  = "#0F6E56",
  "Mayaro"     = "#BA7517",
  "Comparável" = "#888780"
)

m_dom <- ggplot() +
  geom_sf(data = reserva_shp, fill = "gray95", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = dominante, size = pmax(Gamma_Ev, Gamma_My)),
          alpha = 0.9) +
  scale_color_manual(values = cores_dom, name = "Risco dominante") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  labs(
    title = "Patógeno de maior risco por comunidade",
    subtitle = "Tamanho · magnitude do risco dominante"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

# =============================================================================
# PAINEL FINAL
# =============================================================================

fig_mapas <- ggarrange(
  m_fx,  m_dom,
  m_ev,  m_my,
  ncol = 2, nrow = 2,
  labels = "auto"
)

print(fig_mapas)

# =============================================================================
# MAPA 5 — comparativo lado a lado com escala compartilhada
# útil para mostrar divergência espacial dos hotspots
# =============================================================================

# Normalizar Γ para escala 0-1 para comparação justa
comunidades_sf <- comunidades_sf %>%
  mutate(
    Gamma_Ev_norm = (Gamma_Ev - min(Gamma_Ev)) / (max(Gamma_Ev) - min(Gamma_Ev)),
    Gamma_My_norm = (Gamma_My - min(Gamma_My)) / (max(Gamma_My) - min(Gamma_My))
  )

m_ev_norm <- ggplot() +
  geom_sf(data = reserva_shp, fill = "gray95", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = Gamma_Ev_norm, size = Gamma_Ev_norm),
          alpha = 0.9, size = 2) +
  scale_color_gradient(low = "#F4E3D7", high = "#2D4A5C",
                       name = "Risco\nnormalizado",
                       limits = c(0, 1)) +
  scale_size_continuous(range = c(2, 7), limits = c(0, 1), guide = "none") +
  labs(title = "E. vogeli") +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

m_my_norm <- ggplot() +
  geom_sf(data = reserva_shp, fill = "gray95", color = "gray40",
          linewidth = 0.4) +
  geom_sf(data = comunidades_sf,
          aes(color = Gamma_My_norm, size = Gamma_My_norm),
          alpha = 0.9, size = 2) +
  scale_color_gradient(low = "#E6F0FA", high = "#2C3E4E",
                       name = "Risco\nnormalizado",
                       limits = c(0, 1)) +
  scale_size_continuous(range = c(2, 7), limits = c(0, 1), guide = "none") +
  labs(title = "Mayaro") +
  theme_bw(base_size = 11) +
  theme(legend.position = "right",
        axis.text = element_text(size = 7))

fig_comparativo <- ggarrange(
  m_ev_norm, m_my_norm,
  ncol = 2,
  labels = "auto",
  common.legend = FALSE
)

print(fig_comparativo)


library(sf)
library(terra)
library(tidyverse)
library(gstat)
library(stars)

# =============================================================================
# MAPA DE CALOR — IDW + máscara de uso do solo
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CONVERTER usosolo PARA RASTER via terra
# -----------------------------------------------------------------------------

# Classificar pixels como floresta ou não
usosolo_class <- usosolo %>%
  mutate(is_forest = as.integer(class %in% classes_floresta))

# Converter para raster terra
usosolo_vect <- usosolo_class %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(31981)

# Criar raster de referência com resolução dos dados (~30m)
# Estima resolução a partir dos dados
res_x <- abs(usosolo$x[2] - usosolo$x[1])
res_graus <- res_x

# Criar SpatRaster para floresta
r_forest <- rast(
  usosolo_class %>%
    select(x, y, is_forest),
  type = "xyz",
  crs = "EPSG:4326"
)

# Reprojetar para métrico
r_forest_m <- project(r_forest, "EPSG:31981", method = "near")

# Criar máscara da RESEX
reserva_m <- st_transform(reserva_shp, 31981)
r_resex   <- rasterize(vect(reserva_m), r_forest_m, field = 1)

# Aplicar máscara da RESEX ao raster de floresta
r_forest_masked <- mask(r_forest_m, r_resex)

# -----------------------------------------------------------------------------
# 2. CALCULAR d_f — distância ao pixel florestal mais próximo
#    para cada pixel da RESEX
# -----------------------------------------------------------------------------

# Raster binário: 1 = floresta, NA = não floresta
r_floresta_only <- r_forest_masked
r_floresta_only[r_floresta_only == 0] <- NA

# Distância de cada pixel ao pixel florestal mais próximo
r_dist_forest <- distance(r_floresta_only)
r_dist_forest <- mask(r_dist_forest, r_resex)

# Converter distância para km
r_dist_forest_km <- r_dist_forest / 1000

# -----------------------------------------------------------------------------
# 3. IDW — interpolar Γ_Ev e Γ_My para toda a RESEX
# -----------------------------------------------------------------------------

# Preparar pontos para gstat
comunidades_m <- comunidades_sf %>%
  st_transform(31981) %>%
  filter(!is.na(Gamma_Ev))

# Grid de interpolação — resolução ~500m para ser rápido
# (ajuste para res menor na versão final)
r_template <- aggregate(r_forest_masked, fact = 15)  # ~450m se original for 30m
grid_stars <- st_as_stars(r_template) %>%
  st_as_sf() %>%
  st_centroid() %>%
  st_filter(reserva_m)

# IDW para E. vogeli
idw_ev <- idw(
  formula = Gamma_Ev ~ 1,
  locations = comunidades_m,
  newdata   = grid_stars,
  idp       = 2          # potência do inverso da distância
)

# IDW para Mayaro
idw_my <- idw(
  formula = Gamma_My ~ 1,
  locations = comunidades_m,
  newdata   = grid_stars,
  idp       = 2
)

# Converter para tibble com coordenadas
coords_grid <- st_coordinates(grid_stars)

idw_df <- tibble(
  x        = coords_grid[,1],
  y        = coords_grid[,2],
  Gamma_Ev = idw_ev$var1.pred,
  Gamma_My = idw_my$var1.pred
)

# idw_df %>% ggplot() + geom_point(aes(x,y, color = Gamma_)) + scale_color_viridis_c(option = "inferno")

idw_df %>% 
  pivot_longer(cols = 3:4,
               names_to = "disease",
               values_to = "values") %>% 
  ggplot() + geom_point(aes(x,y, color = values*100)) +
  #geom_sf(data=comunidades_sf %>% 
   #         st_transform(crs = 31981), col = "white", alpha = 0.5) +
  scale_color_viridis_c(option = "plasma") + 
  labs(color = "Prob", x  = "", y = "") +
  facet_wrap(~disease) + 
  theme_minimal() + 
  theme(axis.text = element_blank())

# -----------------------------------------------------------------------------
# 4. EXTRAIR d_f E is_forest PARA CADA PIXEL DO GRID
# -----------------------------------------------------------------------------

grid_sf <- grid_stars %>%
  mutate(
    d_f_km   = terra::extract(r_dist_forest_km, vect(.))[,2],
    is_forest = terra::extract(r_forest_masked,  vect(.))[,2]
  ) %>%
  st_drop_geometry()

idw_df <- idw_df %>%
  bind_cols(grid_sf %>% select(d_f_km, is_forest)) %>%
  mutate(
    # Máscara florestal — risco zero fora da floresta
    Gamma_Ev_masked = ifelse(is_forest == 1, Gamma_Ev, NA),
    Gamma_My_masked = ifelse(is_forest == 1, Gamma_My, NA),
    
    # Modular Mayaro por distância à floresta
    phi             = exp(-d_f_km / params$d0),
    Gamma_My_final  = Gamma_My_masked * phi,
    Gamma_Ev_final  = Gamma_Ev_masked,  # E. vogeli não depende de d_f
    
    # Dominante pixel a pixel
    dominante = case_when(
      is.na(Gamma_Ev_final) & is.na(Gamma_My_final) ~ NA_character_,
      is.na(Gamma_Ev_final) ~ "Mayaro",
      is.na(Gamma_My_final) ~ "E. vogeli",
      Gamma_Ev_final > Gamma_My_final * 1.2 ~ "E. vogeli",
      Gamma_My_final > Gamma_Ev_final * 1.2 ~ "Mayaro",
      TRUE ~ "Comparável"
    )
  )

# -----------------------------------------------------------------------------
# 5. MAPAS DE CALOR
# -----------------------------------------------------------------------------

reserva_wgs <- st_transform(reserva_shp, 4326)
comunidades_wgs <- st_transform(comunidades_sf, 4326)

# Converter coords de volta para WGS84 para plotar
idw_wgs <- idw_df %>%
  st_as_sf(coords = c("x","y"), crs = 31981) %>%
  st_transform(4326) %>%
  bind_cols(st_coordinates(.) %>% as_tibble() %>% rename(lon=X, lat=Y)) %>%
  st_drop_geometry()

# --- Mapa calor E. vogeli ---
m_heat_ev <- ggplot() +
  geom_tile(data = idw_wgs %>% filter(!is.na(Gamma_Ev_final)),
            aes(x = lon, y = lat, fill = Gamma_Ev_final),
            alpha = 0.9) +
  geom_sf(data = reserva_wgs, fill = NA,
          color = "gray30", linewidth = 0.5) +
  geom_sf(data = comunidades_wgs,
          size = 2, shape = 21,
          fill = "white", color = "gray20", stroke = 0.5) +
  scale_fill_gradientn(
    colors = c("#E1F5EE","#5DCAA5","#0F6E56","#085041"),
    name   = "Γ_Ev(x)",
    na.value = "transparent"
  ) +
  labs(
    title    = "Risco de spillover — E. vogeli",
    subtitle = "IDW · mascarado por cobertura florestal",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right")

# --- Mapa calor Mayaro ---
m_heat_my <- ggplot() +
  geom_tile(data = idw_wgs %>% filter(!is.na(Gamma_My_final)),
            aes(x = lon, y = lat, fill = Gamma_My_final),
            alpha = 0.9) +
  geom_sf(data = reserva_wgs, fill = NA,
          color = "gray30", linewidth = 0.5) +
  geom_sf(data = comunidades_wgs,
          size = 2, shape = 21,
          fill = "white", color = "gray20", stroke = 0.5) +
  scale_fill_gradientn(
    colors = c("#FAEEDA","#EF9F27","#BA7517","#412402"),
    name   = "Γ_My(x)",
    na.value = "transparent"
  ) +
  labs(
    title    = "Risco de spillover — Mayaro",
    subtitle = "IDW · mascarado por floresta · ponderado por d_f",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right")

# --- Mapa dominante ---
cores_dom <- c(
  "E. vogeli"  = "#0F6E56",
  "Mayaro"     = "#BA7517",
  "Comparável" = "#888780"
)

m_heat_dom <- ggplot() +
  geom_tile(data = idw_wgs %>% filter(!is.na(dominante)),
            aes(x = lon, y = lat, fill = dominante),
            alpha = 0.85) +
  geom_sf(data = reserva_wgs, fill = NA,
          color = "gray30", linewidth = 0.5) +
  geom_sf(data = comunidades_wgs,
          size = 2, shape = 21,
          fill = "white", color = "gray20", stroke = 0.5) +
  scale_fill_manual(values = cores_dom, name = "Risco dominante",
                    na.value = "transparent") +
  labs(
    title    = "Patógeno dominante por área",
    subtitle = "Apenas pixels florestais",
    x = NULL, y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "right")

# --- Painel final ---
fig_heat <- ggarrange(
  m_heat_ev, m_heat_my, m_heat_dom,
  ncol   = 1, nrow = 3,
  labels = "auto"
)

print(fig_heat)
