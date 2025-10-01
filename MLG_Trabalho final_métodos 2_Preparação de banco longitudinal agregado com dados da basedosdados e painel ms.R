###########################################################################
###   Título: Base longitudinal de cobertura vacinal (países)           ###
###########################################################################

# Autor: Alexandre Silva Nogueira

# Objetivo:
#> Integrar dados do DataSUS e da Base dos Dados para criar uma base
#> longitudinal de cobertura vacinal a nível de países, com análises
#> descritivas de tendência temporal.

library(pacman)
p_load(dplyr, basedosdados, bigrquery, glue, tidyr, broom, sf, geobr, gt)

# Autenticação (ajuste se necessário)
bq_auth(email = "alexandrepichilinga@gmail.com")
basedosdados::set_billing_id("adicionar ir próprio")

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|       Sinasc - basedosdados          |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

sql <- glue("
  SELECT
    CAST(id_municipio_residencia AS STRING) AS id_municipio_residencia,
    CAST(ano AS INT64) AS ano,
    COUNT(1) AS N
  FROM `basedosdados.br_ms_sinasc.microdados`
  WHERE ano BETWEEN 2014 AND 2025
  GROUP BY id_municipio_residencia, ano
  ORDER BY id_municipio_residencia, ano
")

nv_long_2014_2021 <- basedosdados::read_sql(sql, billing_project_id = basedosdados::get_billing_id())
nv_long_2014_2021.2 <- nv_long_2014_2021 %>% 
  mutate(id_municipio_residencia = gsub("^([0-9]{6}).*$", "\\1", id_municipio_residencia)) %>%
  filter(!is.na(id_municipio_residencia))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|            Censo 2022 - pop          |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

sql <- "
WITH base AS (
  SELECT
    dados.id_municipio AS id_municipio,
    CAST(dados.idade_anos AS INT64) AS idade_anos,
    SAFE_CAST(dados.populacao AS INT64) AS populacao
  FROM `basedosdados.br_ibge_censo_2022.populacao_idade_sexo` AS dados
)
SELECT
  b.id_municipio,
  d.nome AS id_municipio_nome,
  SUM(CASE WHEN b.idade_anos = 0 THEN b.populacao ELSE 0 END) AS pop_lt1,
  SUM(CASE WHEN b.idade_anos = 1 THEN b.populacao ELSE 0 END) AS pop_1a2,
  SUM(CASE WHEN b.idade_anos BETWEEN 0 AND 4 THEN b.populacao ELSE 0 END) AS pop_0a4
FROM base b
LEFT JOIN (
  SELECT DISTINCT id_municipio, nome
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
) d
ON b.id_municipio = d.id_municipio
GROUP BY id_municipio, id_municipio_nome
ORDER BY id_municipio
"

pop_criancas_mun <- basedosdados::read_sql(sql, billing_project_id = basedosdados::get_billing_id())
pop_criancas_mun.2 <- pop_criancas_mun %>% mutate(id_municipio = gsub("^([0-9]{6}).*$", "\\1", id_municipio)) %>% select(-id_municipio_nome)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|    Dados vacinas - Basedosdados      |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

library(pacman)
p_load(dplyr, tidyr, basedosdados, bigrquery, glue)

# --- Autenticação (ajuste o billing se necessário) ---
bq_auth(email = "alexandrepichilinga@gmail.com")
basedosdados::set_billing_id("hopeful-frame-456219-k5")

tabela_municipio <- "basedosdados.br_ms_imunizacoes.municipio"

sql <- glue("
SELECT
  CAST(dados.ano AS INT64)                  AS ano,
  CAST(dados.id_municipio AS STRING)        AS id_municipio,
  dir.nome                                   AS nome_municipio,

  dados.doses_bcg                           AS doses_bcg,
  dados.cobertura_bcg                       AS cobertura_bcg,

  dados.doses_dtp                           AS doses_dtp,
  dados.cobertura_dtp                       AS cobertura_dtp,
  dados.doses_dtp_ref                       AS doses_dtp_ref,
  dados.cobertura_dtp_ref                   AS cobertura_dtp_ref,

  dados.doses_poliomielite                  AS doses_poliomielite,
  dados.cobertura_poliomielite              AS cobertura_poliomielite,
  dados.doses_poliomielite_4anos            AS doses_poliomielite_4anos,
  dados.cobertura_poliomielite_4anos        AS cobertura_poliomielite_4anos,

  dados.doses_triplice_bacteriana           AS doses_triplice_bacteriana,
  dados.cobertura_triplice_bacteriana       AS cobertura_triplice_bacteriana,

  dados.doses_triplice_viral_d1             AS doses_triplice_viral_d1,
  dados.cobertura_triplice_viral_d1         AS cobertura_triplice_viral_d1,
  dados.doses_triplice_viral_d2             AS doses_triplice_viral_d2,
  dados.cobertura_triplice_viral_d2         AS cobertura_triplice_viral_d2

FROM `{tabela_municipio}` AS dados
LEFT JOIN (
  SELECT DISTINCT CAST(id_municipio AS STRING) AS id_municipio, nome
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
) AS dir
ON dados.id_municipio = dir.id_municipio

WHERE dados.ano BETWEEN 2015 AND 2021
ORDER BY id_municipio, ano
")

imun_long_2015_2021 <- basedosdados::read_sql(sql, billing_project_id = basedosdados::get_billing_id())

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|                Geobr                 |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Ajuste o ano se quiser (2017+ tem regiões imediatas/intermediárias)
mun  <- read_municipality(year = 2020, showProgress = FALSE)
imm  <- read_immediate_region(year = 2020, showProgress = FALSE)
intm <- read_intermediate_region(year = 2020, showProgress = FALSE)

# Usa centróides para um join espacial rápido
cent <- st_centroid(mun)

res <- cent %>%
  st_join(imm %>% select(name_immediate)) %>%
  st_join(intm %>% select(name_intermediate)) %>%
  st_drop_geometry() %>%
  select(code_muni, name_muni, abbrev_state, name_immediate, name_intermediate) %>%
  distinct()

res.2 <- res %>% mutate(code_muni = gsub("^([0-9]{6}).*$", "\\1", code_muni))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|            Painel datasus            |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

pasta <- "/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw"
arquivos <- list.files(pasta, pattern = "\\.xlsx$", full.names = TRUE)
nomes <- c("total.23", "total.24", "total.25", "cobertura.23", "cobertura.24", "cobertura.25")

listas <- lapply(arquivos, read.xlsx)
names(listas) <- nomes
list2env(listas, envir = .GlobalEnv)

cobertura.23 <- cobertura.23 %>% select(-1) %>% mutate(ano = 2023, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, `BCG`:`Varicela`)
cobertura.24 <- cobertura.24 %>% select(-1) %>% mutate(ano = 2024, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, `BCG`:`Varicela`)
#> No caso de polio oral bivalente para 2025
#> não existe essa variável nesse ano. Portanto,
#> usei a variável polio injetavel (vip) reforço
#> como equivalente a cobertura_poliomielite_4anos. 
cobertura.25 <- cobertura.25 %>% select(-1) %>% mutate(ano = 2025, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, `BCG`:`Varicela`)

#> Me baseei aqui p/ fazer equivalência:
#> https://www.gov.br/saude/pt-br/assuntos/noticias/2024/setembro/vacina-oral-da-poliomielite-sera-substituida-por-dose-ainda-mais-segura-e-eficiente 
map <- c(
  "BCG"                      = "cobertura_bcg",
  "DTP"                      = "cobertura_dtp",
  "DTP.(1°.Reforço)"         = "cobertura_dtp_ref",
  "Tríplice.Viral.-.1°.Dose" = "cobertura_triplice_viral_d1",
  "Tríplice.Viral.-.2°.Dose" = "cobertura_triplice_viral_d2",
  "Polio.Injetável.(VIP)"    = "cobertura_poliomielite",
  "Polio.Oral.Bivalente"     = "cobertura_poliomielite_4anos",
  "Polio.Injetável.(VIP).(Reforço)"     = "cobertura_poliomielite_4anos"
)

padronizar_cobertura <- function(df, map) {
  vacinas_alvo <- unname(map)
  cols <- intersect(names(map), names(df))  # apenas o que existe no DF
  df %>%
    dplyr::rename(!!!setNames(cols, map[cols])) %>%            # novo = velho
    mutate(id_municipio = as.character(id_municipio) |> str_trim()) %>%
    filter(
      !is.na(id_municipio),
      id_municipio != "Totais",
      str_detect(id_municipio, "^[0-9]{6}$")
    ) %>%
    select(any_of(c("ano", "id_municipio")), any_of(vacinas_alvo))
}

# aplica nos três bancos
cobertura.23 <- padronizar_cobertura(cobertura.23, map)
cobertura.24 <- padronizar_cobertura(cobertura.24, map)
cobertura.25 <- padronizar_cobertura(cobertura.25, map)

cobertura.23 <- cobertura.23 %>% mutate(cobertura_triplice_bacteriana = cobertura_dtp)
cobertura.24 <- cobertura.24 %>% mutate(cobertura_triplice_bacteriana = cobertura_dtp)
cobertura.25 <- cobertura.25 %>% mutate(cobertura_triplice_bacteriana = cobertura_dtp)

# ◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆ ----- ≋≋≋ Totais ≋≋≋ ----- ◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆◇◆

total.23 <- total.23 %>% mutate(ano = 2023, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, Imunobiológico, `Numerador.(Total)`)
total.24 <- total.24 %>% mutate(ano = 2024, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, Imunobiológico, `Numerador.(Total)`)
total.25 <- total.25 %>% mutate(ano = 2025, id_municipio = gsub("^([0-9]{6}).*$", "\\1", Município.Residência)) %>% select(ano, id_municipio, Imunobiológico, `Numerador.(Total)`)

# -------- função --------
to_wide_imun <- function(df_long) {
  df_long %>%
    mutate(
      vacina_std = case_when(
        Imunobiológico == "BCG"                                                    ~ "doses_bcg",
        Imunobiológico == "DTP"                                                    ~ "doses_dtp",
        str_detect(Imunobiológico, "^DTP\\s*\\((1[°º]).*Reforço\\)$")              ~ "doses_dtp_ref",
        str_detect(Imunobiológico, "^Tr[íi]plice\\s*Viral\\s*[–-]\\s*1[°º]\\s*Dose$") ~ "doses_triplice_viral_d1",
        str_detect(Imunobiológico, "^Tr[íi]plice\\s*Viral\\s*[–-]\\s*2[°º]\\s*Dose$") ~ "doses_triplice_viral_d2",
        str_detect(Imunobiológico, "^Poli?o?\\s*Injet[áa]vel\\s*\\(VIP\\)$")       ~ "doses_poliomielite",
        str_detect(Imunobiológico, "^Poli?o?\\s*Oral\\s*Bivalente")                ~ "doses_poliomielite_4anos",
        str_detect(Imunobiológico, "Polio\\s*Injet[áa]vel\\s*\\(VIP\\)\\s*\\(Reforço\\)") ~ "doses_poliomielite_4anos",
        TRUE ~ NA_character_
      ),
      valor = suppressWarnings(as.numeric(`Numerador.(Total)`))
    ) %>%
    filter(!is.na(vacina_std)) %>%
    group_by(ano, id_municipio, vacina_std) %>%
    summarise(valor = sum(valor, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = vacina_std,
      values_from = valor,
      values_fill = 0
    ) %>%
    arrange(id_municipio, ano)
}

# -------- aplicando nos três bancos --------
total.23.a <- to_wide_imun(total.23)
total.24.a <- to_wide_imun(total.24)
total.25.a <- to_wide_imun(total.25)

total.23.a <- total.23.a %>% mutate(doses_triplice_bacteriana = doses_dtp)
total.24.a <- total.24.a %>% mutate(doses_triplice_bacteriana = doses_dtp)
total.25.a <- total.25.a %>% mutate(doses_triplice_bacteriana = doses_dtp)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|           Juntando bases             |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

total.23.2 <- total.23.a %>% left_join(cobertura.23, by=c("ano", "id_municipio")) %>% 
  mutate(cobertura_triplice_viral_d2 = as.numeric(cobertura_triplice_viral_d2))

total.24.2 <- total.24.a %>% left_join(cobertura.24, by=c("ano", "id_municipio")) %>% 
  mutate(cobertura_triplice_viral_d2 = as.numeric(cobertura_triplice_viral_d2),
         cobertura_triplice_bacteriana = as.numeric(cobertura_triplice_bacteriana),
         doses_triplice_bacteriana = as.numeric(doses_triplice_bacteriana))

total.25.2 <- total.25.a %>% left_join(cobertura.25, by=c("ano", "id_municipio")) %>% 
  mutate(cobertura_triplice_viral_d2 = as.numeric(cobertura_triplice_viral_d2), 
         cobertura_dtp_ref = as.numeric(cobertura_dtp_ref),
         doses_poliomielite_4anos = as.numeric(doses_poliomielite_4anos),
         cobertura_poliomielite_4anos = as.numeric(cobertura_poliomielite_4anos))

imun_long_2015_2021.2 <- imun_long_2015_2021 %>%
  mutate(across(
      c(ano, doses_bcg, cobertura_bcg,
        doses_dtp, cobertura_dtp,
        doses_dtp_ref, cobertura_dtp_ref,
        doses_poliomielite, cobertura_poliomielite,
        doses_poliomielite_4anos, cobertura_poliomielite_4anos,
        doses_triplice_bacteriana, cobertura_triplice_bacteriana,
        doses_triplice_viral_d1, cobertura_triplice_viral_d1,
        doses_triplice_viral_d2, cobertura_triplice_viral_d2),
      as.numeric),
      cobertura_triplice_viral_d2 = ifelse(grepl("^\\s*[0-9]+(\\.[0-9]+)?\\s*$", cobertura_triplice_viral_d2),
                                           as.numeric(cobertura_triplice_viral_d2), NA_real_)) %>%
  bind_rows(total.23.2) %>%
  bind_rows(total.24.2) %>%
  bind_rows(total.25.2) %>%
  select(-nome_municipio) %>%
  mutate(id_municipio = gsub("^([0-9]{6}).*$", "\\1", id_municipio)) %>%
  left_join(res.2, by=c("id_municipio"="code_muni")) %>%
  left_join(pop_criancas_mun.2, by=c("id_municipio")) %>%
  left_join(nv_long_2014_2021.2 %>% mutate(ano=as.numeric(ano)), by=c("ano", "id_municipio"="id_municipio_residencia")) %>%
  rename(nasc_vivos_ano_atual = N)
  
imun_long_2015_2021.3 <- imun_long_2015_2021.2 %>%
  mutate(nasc_vivos_ano_atual = if_else(is.na(nasc_vivos_ano_atual), pop_lt1, nasc_vivos_ano_atual),
         nasc_vivos_ano_atual = as.numeric(nasc_vivos_ano_atual),
         cv_propria_bcg = doses_bcg/nasc_vivos_ano_atual * 100,
         cv_propria_poliomielite = doses_poliomielite/nasc_vivos_ano_atual * 100,
         cv_propria_triplice_bacteriana = doses_triplice_bacteriana/nasc_vivos_ano_atual * 100,
         cv_propria_triplice_viral_d1 = doses_triplice_viral_d1/nasc_vivos_ano_atual * 100,
         cv_propria_triplice_viral_d2 = doses_triplice_viral_d2/nasc_vivos_ano_atual * 100)

imun_long_2015_2021.4 <- imun_long_2015_2021.3 %>%
  mutate(across(
    c(cobertura_bcg,
      cobertura_dtp,
      cobertura_dtp_ref,
      cobertura_poliomielite,
      cobertura_poliomielite_4anos,
      cobertura_triplice_bacteriana,
      cobertura_triplice_viral_d1,
      cobertura_triplice_viral_d2),
    ~ if_else(ano %in% 2023:2025, .x * 100, .x))) %>%
  select(-cobertura_dtp, -cobertura_dtp_ref)

pasta <- "/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw"
write.xlsx(imun_long_2015_2021.4, "imun_long_2015_2021.4.xlsx")

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|           Análises.                  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

##=======================================================================##
##                Gráfico - cobertura vacinal Própria.                   ##
##=======================================================================##

medias_proprias_gap <- medias_proprias %>%
  mutate(
    ano = as.integer(ano),
    media_pct = ifelse(is.nan(media_pct), NA_real_, media_pct)
  ) %>%
  group_by(vacina) %>%
  complete(ano = 2015:2025) %>%   # cria 2022 com NA -> a linha vai quebrar
  arrange(vacina, ano) %>%
  ungroup()

ggplot(medias_proprias_gap, aes(x = ano, y = media_pct, color = vacina, group = vacina)) +
  geom_line() +  # não conecta através de NA
  geom_point(data = dplyr::filter(medias_proprias_gap, !is.na(media_pct))) +
  scale_x_continuous(breaks = 2015:2025, labels = 2015:2025) +  # todos os anos, inteiros
  labs(
    title = "Cobertura Vacinal no Brasil entre 2015 e 2025 (Indicador próprio)",
    subtitle = "O indicador de cobertura vacinal foi calculado usando como numerador o número de doses \nda Base dos Dados e do painel do Ministério da Saúde\nO denominador entre 2015 e 2022 veio do SINASC e entre 2023 e 2025 veio do Censo 2022",
    x = "Ano",
    y = "Cobertura média (%)",
    color = "Vacina"
  ) +
  theme_minimal()
  
##=======================================================================##
##           Gráfico - cobertura vacinal Base dos Dados                  ##
##=======================================================================##

medias_cobertura <- imun_long_2015_2021.4 %>%
  select(ano, starts_with("cobertura_")) %>%
  pivot_longer(
    -ano, names_to = "vacina", values_to = "cobertura_pct"
  ) %>%
  group_by(ano, vacina) %>%
  summarise(media_pct = mean(cobertura_pct, na.rm = TRUE),
            n_municipios = sum(!is.na(cobertura_pct)),
            .groups = "drop") %>%
  mutate(vacina = recode(vacina,
                         cobertura_bcg                 = "BCG",
                         #cobertura_dtp                 = "Tríplice bacteriana (DTP)",
                         #cobertura_dtp_ref             = "DTP (1º reforço)",
                         cobertura_poliomielite        = "Poliomielite",
                         cobertura_poliomielite_4anos  = "Poliomielite (reforço 4 anos)",
                         cobertura_triplice_bacteriana = "Tríplice bacteriana",
                         cobertura_triplice_viral_d1   = "Tríplice viral D1",
                         cobertura_triplice_viral_d2   = "Tríplice viral D2"))

medias_cobertura_gap <- medias_cobertura %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x))) %>%
  mutate(across(where(is.character), ~ na_if(.x, "NUN"))) %>%
  group_by(vacina) %>%
  complete(ano = 2015:2025) %>%   # insere 2022 como NA
  arrange(vacina, ano) %>%
  ungroup()

ggplot(medias_cobertura_gap, aes(x = ano, y = media_pct, color = vacina, group = vacina)) +
  geom_line() +
  geom_point(data = filter(medias_cobertura_gap, !is.na(media_pct))) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(
    title = "Cobertura Vacinal no Brasil entre 2015 e 2025",
    subtitle = "Entre 2015 e 2021 os dados são da Base dos Dados\nEntre 2023 e 2025 os dados são do painel do Ministério da Saúde",
    x = "Ano",
    y = "Cobertura média (%)",
    color = "Vacina"
  ) +
  theme_minimal()

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|         Tabelas crescimento          |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

##=======================================================================##
##                  Tabela crescimento - 2015 a 2021                     ##
##=======================================================================##

medias_15_21 <- medias_cobertura %>%
  filter(ano >= 2015, ano <= 2021)

# 2) Deltas (p.p. e relativo %)
indicadores_15_21 <- medias_15_21 %>%
  group_by(vacina) %>%
  summarise(
    primeira_ano = min(ano, na.rm = TRUE),
    ultima_ano   = max(ano, na.rm = TRUE),
    primeira_cob = media_pct[ano == primeira_ano][1],
    ultima_cob   = media_pct[ano == ultima_ano][1],
    delta_pp     = ultima_cob - primeira_cob,
    delta_rel_pct = ifelse(!is.na(primeira_cob) && primeira_cob != 0,
                           100 * (ultima_cob / primeira_cob - 1),
                           NA_real_),
    .groups = "drop"
  )

# 3) Tendência por regressão (coeficiente anual e p-valor)
tendencia_15_21 <- medias_15121 %>%
  group_by(vacina) %>%
  do(tidy(lm(media_pct ~ ano, data = .))) %>%
  filter(term == "ano") %>%
  transmute(vacina, coef_tendencia = estimate, p_value = p.value)

# 4) Resultado final
resultado_15_21 <- indicadores_15_21 %>%
  left_join(tendencia_15_21, by = "vacina") %>%
  arrange(coef_tendencia)

# Tabela
tabela_resumo <- resultado_15_21 %>%
  select(
    Vacina                      = vacina,
    `Cobertura média 2015`      = primeira_cob,
    `Cobertura média 2021`      = ultima_cob,
    `Variação (p.p.)`           = delta_pp,
    `Variação relativa (%)`     = delta_rel_pct,
    `Tendência (p.p./ano)`      = coef_tendencia,
    `p-valor`                   = p_value
  )

# 2) Formata para exibição
gt_tbl <- tabela_resumo %>%
  mutate(across(-Vacina, as.numeric)) %>%
  gt() %>%
  fmt_number(
    columns = c(`Cobertura média 2015`, `Cobertura média 2021`,
                `Variação (p.p.)`, `Variação relativa (%)`,
                `Tendência (p.p./ano)`),
    decimals = 1
  ) %>%
  fmt_number(columns = `p-valor`, decimals = 3) %>%
  tab_header(title = "Cobertura vacinal — tendências e variações (2015–2021)") %>%
  tab_source_note(md(
    "Nota: ‘Tendência (p.p./ano)’ é o coeficiente da regressão de **média de cobertura** sobre **ano**."
  ))

gt_tbl

##=======================================================================##
##                  Tabela crescimento - 2015 a 2019                     ##
##=======================================================================##

medias_15_19 <- medias_cobertura %>%
  filter(ano >= 2015, ano <= 2019)

# 2) Deltas (p.p. e relativo %)
indicadores_15_19 <- medias_15_19 %>%
  group_by(vacina) %>%
  summarise(
    primeira_cob = media_pct[ano == min(ano, na.rm = TRUE)][1],
    ultima_cob   = media_pct[ano == max(ano, na.rm = TRUE)][1],
    delta_pp     = ultima_cob - primeira_cob,
    delta_rel_pct = ifelse(!is.na(primeira_cob) && primeira_cob != 0,
                           100 * (ultima_cob / primeira_cob - 1),
                           NA_real_),
    .groups = "drop"
  )

# 3) Tendência por regressão (coeficiente anual e p-valor)
tendencia_15_19 <- medias_15_19 %>%
  group_by(vacina) %>%
  do(tidy(lm(media_pct ~ ano, data = .))) %>%
  filter(term == "ano") %>%
  transmute(vacina, coef_tendencia = estimate, p_value = p.value)

# 4) Junta resultados
resultado_15_19 <- indicadores_15_19 %>%
  left_join(tendencia_15_19, by = "vacina") %>%
  arrange(coef_tendencia)

# 5) Formata tabela bonita
tabela_resumo_15_19 <- resultado_15_19 %>%
  select(
    Vacina                      = vacina,
    `Cobertura média 2015`      = primeira_cob,
    `Cobertura média 2019`      = ultima_cob,
    `Variação (p.p.)`           = delta_pp,
    `Variação relativa (%)`     = delta_rel_pct,
    `Tendência (p.p./ano)`      = coef_tendencia,
    `p-valor`                   = p_value
  )

gt_tbl_15_19 <- tabela_resumo_15_19 %>%
  mutate(across(-Vacina, as.numeric)) %>%
  gt() %>%
  fmt_number(
    columns = c(`Cobertura média 2015`, `Cobertura média 2019`,
                `Variação (p.p.)`, `Variação relativa (%)`,
                `Tendência (p.p./ano)`),
    decimals = 1
  ) %>%
  fmt_number(columns = `p-valor`, decimals = 3) %>%
  tab_header(title = "Cobertura vacinal — tendências e variações (2015–2019)") %>%
  tab_source_note(md(
    "Nota: ‘Tendência (p.p./ano)’ é o coeficiente da regressão de **média de cobertura** sobre **ano**."
  ))

gt_tbl_15_19

##=======================================================================##
##                  Tabela crescimento - 2023 a 2025                     ##
##=======================================================================##

medias_23_25 <- medias_cobertura %>%
  filter(ano >= 2023, ano <= 2025)

# 2) Deltas (p.p. e relativo %)
indicadores_23_25 <- medias_23_25 %>%
  group_by(vacina) %>%
  summarise(
    primeira_cob  = media_pct[ano == min(ano, na.rm = TRUE)][1],
    ultima_cob    = media_pct[ano == max(ano, na.rm = TRUE)][1],
    delta_pp      = ultima_cob - primeira_cob,
    delta_rel_pct = ifelse(!is.na(primeira_cob) && primeira_cob != 0,
                           100 * (ultima_cob / primeira_cob - 1),
                           NA_real_),
    .groups = "drop"
  )

# 3) Tendência por regressão (coeficiente anual e p-valor)
tendencia_23_25 <- medias_23_25 %>%
  group_by(vacina) %>%
  do(tidy(lm(media_pct ~ ano, data = .))) %>%
  filter(term == "ano") %>%
  transmute(vacina, coef_tendencia = estimate, p_value = p.value)

# 4) Junta resultados
resultado_23_25 <- indicadores_23_25 %>%
  left_join(tendencia_23_25, by = "vacina") %>%
  arrange(coef_tendencia)

# 5) Tabela formatada (visualizar no Viewer)
tabela_resumo_23_25 <- resultado_23_25 %>%
  select(
    Vacina                      = vacina,
    `Cobertura média 2023`      = primeira_cob,
    `Cobertura média 2025`      = ultima_cob,
    `Variação (p.p.)`           = delta_pp,
    `Variação relativa (%)`     = delta_rel_pct,
    `Tendência (p.p./ano)`      = coef_tendencia,
    `p-valor`                   = p_value
  )

gt_tbl_23_25 <- tabela_resumo_23_25 %>%
  mutate(across(-Vacina, as.numeric)) %>%
  gt() %>%
  fmt_number(
    columns = c(`Cobertura média 2023`, `Cobertura média 2025`,
                `Variação (p.p.)`, `Variação relativa (%)`,
                `Tendência (p.p./ano)`),
    decimals = 1
  ) %>%
  fmt_number(columns = `p-valor`, decimals = 3) %>%
  tab_header(title = "Cobertura vacinal — tendências e variações (2023–2025)") %>%
  tab_source_note(md(
    "Nota: ‘Tendência (p.p./ano)’ é o coeficiente da regressão de **média de cobertura** sobre **ano**."
  ))

gt_tbl_23_25

##=======================================================================##
##                  Tabela crescimento - 2015 a 2025                      ##
##=======================================================================##

medias_15_25 <- medias_cobertura %>%
  filter(ano >= 2015, ano <= 2025)

# 2) Deltas (p.p. e relativo %)
indicadores_15_25 <- medias_15_25 %>%
  group_by(vacina) %>%
  summarise(
    primeira_ano  = min(ano, na.rm = TRUE),
    ultima_ano    = max(ano, na.rm = TRUE),
    primeira_cob  = media_pct[ano == primeira_ano][1],
    ultima_cob    = media_pct[ano == ultima_ano][1],
    delta_pp      = ultima_cob - primeira_cob,
    delta_rel_pct = ifelse(!is.na(primeira_cob) && primeira_cob != 0,
                           100 * (ultima_cob / primeira_cob - 1),
                           NA_real_),
    .groups = "drop"
  )

# 3) Tendência por regressão (coeficiente anual e p-valor)
tendencia_15_25 <- medias_15_25 %>%
  group_by(vacina) %>%
  do(tidy(lm(media_pct ~ ano, data = .))) %>%
  filter(term == "ano") %>%
  transmute(vacina, coef_tendencia = estimate, p_value = p.value)

# 4) Junta resultados
resultado_15_25 <- indicadores_15_25 %>%
  left_join(tendencia_15_25, by = "vacina") %>%
  arrange(coef_tendencia)

# 5) Tabela formatada (visualizar no Viewer)
tabela_resumo_15_25 <- resultado_15_25 %>%
  select(
    Vacina                          = vacina,
    `Cobertura média (1º ano)`      = primeira_cob,
    `Cobertura média (último ano)`  = ultima_cob,
    `Variação (p.p.)`               = delta_pp,
    `Variação relativa (%)`         = delta_rel_pct,
    `Tendência (p.p./ano)`          = coef_tendencia,
    `p-valor`                       = p_value
  )

gt_tbl_15_25 <- tabela_resumo_15_25 %>%
  mutate(across(-Vacina, as.numeric)) %>%
  gt() %>%
  fmt_number(
    columns = c(`Cobertura média (1º ano)`, `Cobertura média (último ano)`,
                `Variação (p.p.)`, `Variação relativa (%)`,
                `Tendência (p.p./ano)`),
    decimals = 1
  ) %>%
  fmt_number(columns = `p-valor`, decimals = 3) %>%
  tab_header(title = "Cobertura vacinal — tendências e variações (2015–2025)") %>%
  tab_source_note(md(
    "Nota: ‘Tendência (p.p./ano)’ é o coeficiente da regressão de **média de cobertura** sobre **ano**."
  ))

gt_tbl_15_25

#                 ~~~ Fim ~~~

