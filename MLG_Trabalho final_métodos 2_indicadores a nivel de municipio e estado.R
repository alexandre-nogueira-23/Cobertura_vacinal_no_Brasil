###########################################################################
###   Título: Indicadores de cobertura vacinal (municípios e estados)   ###
###########################################################################

# Autor: Alexandre Silva Nogueira

# Objetivo:
#> Construir indicadores padronizados de cobertura vacinal em municípios
#> e estados e analisar a evolução temporal da cobertura por região.

library(pacman)
p_load(dplyr, basedosdados, bigrquery, glue, tidyr, broom,
       sf, geobr, gt, patchwork, cowplot, gridExtra, ragg, grid)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Calculando diferencas perc muni     |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

metadados_muni <- imun_long_2015_2021.4 %>%
  arrange(id_municipio, desc(ano)) %>%
  group_by(id_municipio) %>%
  summarise(
    name_muni         = dplyr::first(na.omit(name_muni)),
    abbrev_state      = dplyr::first(na.omit(abbrev_state)),
    name_immediate    = dplyr::first(na.omit(name_immediate)),
    name_intermediate = dplyr::first(na.omit(name_intermediate)),
    .groups = "drop"
  )

# --- 4 vacinas-alvo (apenas colunas de cobertura) ---
cols_cobertura <- c(
  "cobertura_bcg",
  "cobertura_poliomielite",
  "cobertura_triplice_bacteriana",
  "cobertura_triplice_viral_d1"
)

# --- tabela final em formato MUNICÍPIO (1 linha por município),
#     com as coberturas das 4 vacinas para 2015, 2021, 2023 e 2025 ---
tabela_coberturas_4anos <- imun_long_2015_2021.4 %>%
  select(id_municipio, ano, all_of(cols_cobertura)) %>%
  # limpa números: tira espaços, troca vírgula por ponto e converte pra numeric
  mutate(across(all_of(cols_cobertura), ~ {
    x <- as.character(.x)
    x <- gsub("\\s+", "", x)
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  })) %>%
  filter(ano %in% c(2015, 2021, 2023, 2025)) %>%
  pivot_longer(cols = all_of(cols_cobertura),
               names_to = "vacina",
               values_to = "cobertura") %>%
  # se houver duplicidade por (muni, vacina, ano), resolve pegando o primeiro não-NA
  group_by(id_municipio, vacina, ano) %>%
  summarise(cobertura = dplyr::first(na.omit(cobertura)), .groups = "drop") %>%
  # abre em largo: cria colunas vacina_ano (ex.: cobertura_bcg_2015)
  pivot_wider(
    id_cols = id_municipio,
    names_from = c(vacina, ano),
    values_from = cobertura,
    names_glue = "{vacina}_{ano}"
  ) %>%
  # junta metadados e organiza colunas
  left_join(metadados_muni, by = "id_municipio") %>%
  relocate(id_municipio, name_muni, abbrev_state, name_immediate, name_intermediate)

tabela_coberturas_4anos <- tabela_coberturas_4anos %>%
  mutate(across(starts_with("cobertura_"), ~ round(.x, 2)))

# helper: diferença "segura" (em p.p.); se algum ano > 300 ou NA -> NA
safe_diff <- function(a, b) {
  ifelse(is.na(a) | is.na(b) | a > 300 | b > 300, NA_real_, a - b)
}

tabela_coberturas_4anos_resumo <- tabela_coberturas_4anos %>%
  mutate(
    # BCG
    diff_bcg_21_15  = round(safe_diff(cobertura_bcg_2021,  cobertura_bcg_2015),  2),
    diff_bcg_25_23  = round(safe_diff(cobertura_bcg_2025,  cobertura_bcg_2023),  2),
    
    # Poliomielite
    diff_polio_21_15 = round(safe_diff(cobertura_poliomielite_2021, cobertura_poliomielite_2015), 2),
    diff_polio_25_23 = round(safe_diff(cobertura_poliomielite_2025, cobertura_poliomielite_2023), 2),
    
    # Tríplice bacteriana
    diff_dtp_21_15   = round(safe_diff(cobertura_triplice_bacteriana_2021, cobertura_triplice_bacteriana_2015), 2),
    diff_dtp_25_23   = round(safe_diff(cobertura_triplice_bacteriana_2025, cobertura_triplice_bacteriana_2023), 2),
    
    # Tríplice viral D1
    diff_tv1_21_15   = round(safe_diff(cobertura_triplice_viral_d1_2021, cobertura_triplice_viral_d1_2015), 2),
    diff_tv1_25_23   = round(safe_diff(cobertura_triplice_viral_d1_2025, cobertura_triplice_viral_d1_2023), 2)
  )

pasta <- "/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw"
write.xlsx(tabela_coberturas_4anos_resumo, "tabela_coberturas_4anos_resumo.xlsx")

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|       Mapas municípios diff perc     |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# -------------------------------
# (A) Malha municipal
# -------------------------------
mun <- read_municipality(year = 2020, showProgress = FALSE) %>%
  mutate(id_municipio = substr(as.character(code_muni), 1, 6)) %>%
  select(id_municipio, geom = geom)

# -------------------------------
# (B) Função para remover outliers (10 maiores e 10 menores)
# -------------------------------
remove_outliers <- function(x) {
  if (all(is.na(x))) return(x)
  ord <- order(x, na.last = NA)
  out <- x
  out[ord[1:10]] <- NA   # 10 menores
  out[ord[(length(ord)-9):length(ord)]] <- NA  # 10 maiores
  out}

# -------------------------------
# (C) Função de mapa
# -------------------------------
plot_mapa_diff <- function(df, var_col, titulo) {
  dados <- mun %>%
    left_join(df %>% select(id_municipio, all_of(var_col)), by = "id_municipio") %>%
    mutate(!!var_col := remove_outliers(.data[[var_col]]))
  
  lim <- max(abs(dados[[var_col]]), na.rm = TRUE)
  if (!is.finite(lim)) lim <- 0
  
  ggplot(dados) +
    geom_sf(aes(fill = .data[[var_col]]), size = 0.05, color = NA) +
    scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-lim, lim),
      labels = function(x) paste0(round(x, 1), " p.p."),
      name = "Diferença"
    ) +
    labs(title = titulo, caption = "Fonte: BD+ | Elaboração própria") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right")}

# -------------------------------
# (D) Conjuntos de mapas
# -------------------------------
# 2015–2021
m_bcg_21_15   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_bcg_21_15", "BCG (2021–2015)")
m_polio_21_15 <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_polio_21_15", "Poliomielite (2021–2015)")
m_dtp_21_15   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_dtp_21_15", "Tríplice bacteriana (2021–2015)")
m_tv1_21_15   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_tv1_21_15", "Tríplice viral D1 (2021–2015)")

# 2023–2025
m_bcg_25_23   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_bcg_25_23", "BCG (2025–2023)")
m_polio_25_23 <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_polio_25_23", "Poliomielite (2025–2023)")
m_dtp_25_23   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_dtp_25_23", "Tríplice bacteriana (2025–2023)")
m_tv1_25_23   <- plot_mapa_diff(tabela_coberturas_4anos_resumo, "diff_tv1_25_23", "Tríplice viral D1 (2025–2023)")

# -------------------------------
# (E) Mostrar os grids
# -------------------------------
# checagem rápida (opcional)
stopifnot(inherits(m_bcg_21_15, "ggplot"),
          inherits(m_polio_21_15, "ggplot"),
          inherits(m_dtp_21_15, "ggplot"),
          inherits(m_tv1_21_15, "ggplot"))

# ---- monta os grids 2x2 (2015–2021) ----
grid_21_15 <- gridExtra::arrangeGrob(
  m_bcg_21_15,  m_polio_21_15,
  m_dtp_21_15,  m_tv1_21_15,
  ncol = 2)

# ---- opcional: visualizar no Viewer ----
grid::grid.newpage(); grid::grid.draw(grid_21_15)
m_bcg_25_23
m_polio_25_23
m_dtp_25_23
m_tv1_25_23

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Mapas - estados - médias diff     |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°
# =========================================================
# A) Malha ESTADUAL (geobr)
# =========================================================
ufs <- read_state(year = 2020, showProgress = FALSE) %>%
  st_transform(4674) %>%  # opcional
  select(abbrev_state, name_state, geom = geom)

# =========================================================
# B) Agregação por UF com regra: remover 5↑ e 5↓ só se n_munis > 60
#    df_muni precisa ter: abbrev_state, id_municipio e a coluna de interesse
# =========================================================
state_mean_from_muni <- function(df_muni, col) {
  col <- rlang::ensym(col)
  df_muni %>%
    select(abbrev_state, id_municipio, !!col) %>%
    group_by(abbrev_state) %>%
    mutate(
      n_munis = n_distinct(id_municipio),
      # ranks só para valores não-NA
      rk_asc  = if_else(is.na(!!col), NA_integer_, rank( !!col,  ties.method = "first")),
      rk_desc = if_else(is.na(!!col), NA_integer_, rank(-!!col,  ties.method = "first")),
      drop_it = n_munis > 60 & !is.na(rk_asc) & (rk_asc <= 5 | rk_desc <= 5),
      val_adj = if_else(drop_it, NA_real_, as.numeric(!!col))
    ) %>%
    summarise(
      mean_diff = mean(val_adj, na.rm = TRUE),
      n_valid   = sum(!is.na(val_adj)),
      .groups = "drop"
    )
}

# =========================================================
# C) Função de mapa (ESTADOS)
# =========================================================
plot_mapa_estado <- function(df_state_mean, titulo, legenda = "Diferença (%)") {
  dados <- ufs %>% left_join(df_state_mean, by = "abbrev_state")
  
  lim <- max(abs(dados$mean_diff), na.rm = TRUE)
  if (!is.finite(lim)) lim <- 0
  
  ggplot(dados) +
    geom_sf(aes(fill = mean_diff), color = NA) +
    scale_fill_gradient2(
      low = "#b2182b", mid = "white", high = "#2166ac",
      midpoint = 0, limits = c(-lim, lim),
      labels = function(x) paste0(round(x, 1), "%"),
      name = legenda
    ) +
    labs(title = titulo, caption = "Fonte: BD+ e Painel MS | Elaboração própria") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right")
}

# =========================================================
# D) Seleção das colunas de diferenças 
#     Use: tabela_coberturas_4anos_resumo
#     (precisa conter: id_municipio, abbrev_state e as cols abaixo)
# =========================================================
cols_21_15 <- c(
  bcg   = "diff_bcg_21_15",
  polio = "diff_polio_21_15",
  dtp   = "diff_dtp_21_15",
  tv1   = "diff_tv1_21_15"
)
cols_25_23 <- c(
  bcg   = "diff_bcg_25_23",
  polio = "diff_polio_25_23",
  dtp   = "diff_dtp_25_23",
  tv1   = "diff_tv1_25_23"
)

# =========================================================
# E) Agregar por ESTADO (média com regra dos >60 munis) e mapear
# =========================================================
# --- Período 2015–2021 ---
uf_bcg_21_15   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_21_15["bcg"])
uf_polio_21_15 <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_21_15["polio"])
uf_dtp_21_15   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_21_15["dtp"])
uf_tv1_21_15   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_21_15["tv1"])

m_uf_bcg_21_15   <- plot_mapa_estado(uf_bcg_21_15,   "BCG — média da diferença (2021–2015)")
m_uf_polio_21_15 <- plot_mapa_estado(uf_polio_21_15, "Poliomielite — média da diferença (2021–2015)")
m_uf_dtp_21_15   <- plot_mapa_estado(uf_dtp_21_15,   "Tríplice bacteriana — média da diferença (2021–2015)")
m_uf_tv1_21_15   <- plot_mapa_estado(uf_tv1_21_15,   "Tríplice viral D1 — média da diferença (2021–2015)")

# --- Período 2023–2025 ---
uf_bcg_25_23   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_25_23["bcg"])
uf_polio_25_23 <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_25_23["polio"])
uf_dtp_25_23   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_25_23["dtp"])
uf_tv1_25_23   <- state_mean_from_muni(tabela_coberturas_4anos_resumo, !!cols_25_23["tv1"])

m_uf_bcg_25_23   <- plot_mapa_estado(uf_bcg_25_23,   "BCG — média da diferença (2025–2023)")
m_uf_polio_25_23 <- plot_mapa_estado(uf_polio_25_23, "Poliomielite — média da diferença (2025–2023)")
m_uf_dtp_25_23   <- plot_mapa_estado(uf_dtp_25_23,   "Tríplice bacteriana — média da diferença (2025–2023)")
m_uf_tv1_25_23   <- plot_mapa_estado(uf_tv1_25_23,   "Tríplice viral D1 — média da diferença (2025–2023)")

# =========================================================
# F) Grids 2×2 (sem pacotes extras)
# =========================================================
print_gg_grid <- function(plots, ncol = 2) {
  stopifnot(all(vapply(plots, inherits, logical(1), what = "ggplot")))
  n <- length(plots); nrow <- ceiling(n / ncol)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow, ncol)))
  k <- 1
  for (i in seq_len(nrow)) for (j in seq_len(ncol)) {
    if (k <= n) {
      print(plots[[k]], vp = grid::viewport(layout.pos.row = i, layout.pos.col = j))
      k <- k + 1
    }
  }
  invisible(NULL)
}

# 2015–2021
print_gg_grid(
  plots = list(m_uf_bcg_21_15, m_uf_polio_21_15, m_uf_dtp_21_15, m_uf_tv1_21_15),
  ncol = 2
)

# 2023–2025
print_gg_grid(
  plots = list(m_uf_bcg_25_23, m_uf_polio_25_23, m_uf_dtp_25_23, m_uf_tv1_25_23),
  ncol = 2
)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Mapas - estados - média 2024      |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# 1) Seleciona 2024 e as 4 coberturas
cols_cob <- c(
  "cobertura_bcg",
  "cobertura_poliomielite",
  "cobertura_triplice_bacteriana",
  "cobertura_triplice_viral_d1"
)

df24 <- imun_long_2015_2021.4 %>%
  filter(ano == 2024) %>%
  select(abbrev_state, id_municipio, all_of(cols_cob)) %>%
  # garante numérico (aceita vírgula como decimal)
  mutate(across(all_of(cols_cob), ~ {
    x <- as.character(.x)
    x <- gsub("\\s+", "", x)
    x <- gsub(",", ".", x)
    suppressWarnings(as.numeric(x))
  }))

medias_2024_por_estado <- df24 %>%
  pivot_longer(all_of(cols_cob), names_to = "vacina", values_to = "cobertura") %>%
  filter(!is.na(cobertura) & is.finite(cobertura)) %>%
  group_by(abbrev_state, vacina) %>%
  # tamanho do grupo (quantos municípios na UF p/ essa vacina)
  mutate(n_municipios_grp = n()) %>%
  arrange(desc(cobertura), .by_group = TRUE) %>%
  mutate(rank_desc = row_number()) %>%
  # se tiver >60 municípios, remove top-5; senão mantém todos
  filter(if_else(n_municipios_grp > 60, rank_desc > 5, TRUE)) %>%
  summarise(
    media_cobertura_2024 = mean(cobertura, na.rm = TRUE),
    n_municipios_usados  = n(),
    .groups = "drop"
  ) %>%
  # abrir em colunas por vacina
  mutate(vacina = recode(
    vacina,
    cobertura_bcg                 = "media_bcg_2024",
    cobertura_poliomielite        = "media_poliomielite_2024",
    cobertura_triplice_bacteriana = "media_triplice_bacteriana_2024",
    cobertura_triplice_viral_d1   = "media_triplice_viral_d1_2024"
  )) %>%
  select(abbrev_state, vacina, media_cobertura_2024) %>%
  pivot_wider(names_from = vacina, values_from = media_cobertura_2024) %>%
  arrange(abbrev_state)

# (opcional) arredonda para 2 casas
medias_2024_por_estado <- medias_2024_por_estado %>%
  mutate(across(-abbrev_state, ~ round(.x, 2)))

# --- malha estadual ---
ufs <- read_state(year = 2020, showProgress = FALSE) %>%
  select(abbrev_state, geom = geom)

# --- função para criar mapa por vacina ---
plot_mapa_estado <- function(df, col, titulo) {
  dados <- ufs %>%
    left_join(df %>% select(abbrev_state, all_of(col)), by = "abbrev_state")
  
  ggplot(dados) +
    geom_sf(aes(fill = .data[[col]]), color = NA) +
    scale_fill_gradient(
      low = "#fee8c8", high = "#e34a33", na.value = "grey90",
      labels = function(x) paste0(round(x, 1), "%"),
      name = "Cobertura média"
    ) +
    labs(title = titulo, caption = "Fonte: DataSUS | Elaboração própria") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 12),
          legend.position = "right")}

# --- cria os 4 mapas ---
m_bcg   <- plot_mapa_estado(medias_2024_por_estado, "media_bcg_2024", "BCG (média 2024)")
m_polio <- plot_mapa_estado(medias_2024_por_estado, "media_poliomielite_2024", "Poliomielite (média 2024)")
m_dtp   <- plot_mapa_estado(medias_2024_por_estado, "media_triplice_bacteriana_2024", "Tríplice bacteriana (média 2024)")
m_tv1   <- plot_mapa_estado(medias_2024_por_estado, "media_triplice_viral_d1_2024", "Tríplice viral D1 (média 2024)")

# --- função para organizar em grid 2x2 (sem pacotes extras) ---
print_gg_grid <- function(plots, ncol = 2) {
  n <- length(plots)
  nrow <- ceiling(n / ncol)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow, ncol)))
  k <- 1
  for (i in seq_len(nrow)) {
    for (j in seq_len(ncol)) {
      if (k <= n) {
        print(plots[[k]], vp = viewport(layout.pos.row = i, layout.pos.col = j))
        k <- k + 1}}}}

# --- mostrar o grid com os 4 mapas ---
print_gg_grid(
  plots = list(m_bcg, m_polio, m_dtp, m_tv1),
  ncol = 2)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Mapas estados: média das médias24   |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# 1) média das quatro vacinas (por estado)
medias_2024_por_estado <- medias_2024_por_estado %>%
  mutate(media_geral_2024 = rowMeans(
    cbind(media_bcg_2024,
          media_poliomielite_2024,
          media_triplice_bacteriana_2024,
          media_triplice_viral_d1_2024),
    na.rm = TRUE
  ))

# 2) malha estadual e join
ufs <- read_state(year = 2020, showProgress = FALSE) %>%
  select(abbrev_state, geom = geom) %>%
  left_join(medias_2024_por_estado, by = "abbrev_state")

# 3) centróides seguros (dentro do polígono) + ranking
centros <- ufs %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as.data.frame()
names(centros) <- c("cx","cy")

plot_df <- ufs %>%
  st_set_geometry(NULL) %>%
  bind_cols(centros) %>%
  mutate(rank_2024 = dplyr::min_rank(dplyr::desc(media_geral_2024)))

# 4) mapa
ggplot() +
  geom_sf(data = ufs, aes(fill = media_geral_2024), color = NA) +
  scale_fill_gradient(
    low = "#fee8c8", high = "#e34a33", na.value = "grey90",
    labels = function(x) paste0(round(x, 1), "%"),
    name = "Cobertura média\n(4 vacinas, 2024)"
  ) +
  geom_text(data = plot_df, aes(x = cx, y = cy, label = rank_2024),
            size = 3.5, fontface = "bold", color = "black") +
  labs(
    title = "Cobertura vacinal média (4 vacinas) — 2024",
    subtitle = "Média das coberturas de BCG, Poliomielite, Tríplice bacteriana e Tríplice viral D1",
    caption = "Fonte: DATASUS / Elaboração própria"
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 13),
        legend.position = "right")

#                 ~~~ Fim ~~~

