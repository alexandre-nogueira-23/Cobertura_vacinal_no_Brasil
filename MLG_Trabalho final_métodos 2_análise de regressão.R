

library(pacman)
p_load(dplyr, basedosdados, bigrquery, glue, tidyr, broom,
       sf, geobr, gt, patchwork, cowplot, gridExtra, ragg, grid,
       janitor, abjutils, stringi, fuzzyjoin, lmtest, sandwich, stargazer,
       nortest, tseries) 

options(scipen=999) # Evita aparecerem notações científicas nas tabelas

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Criancas ate 4 anos                 |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Autenticação (ajuste se necessário)
bq_auth(email = "alexandrepichilinga@gmail.com")
basedosdados::set_billing_id("adicionar id próprio")

# SQL: soma população total e 0–4 anos por município
sql <- glue("
WITH base AS (
  SELECT
    CAST(dados.id_municipio AS STRING) AS id_municipio,
    SAFE_CAST(dados.idade_anos AS INT64) AS idade_anos,
    SAFE_CAST(dados.populacao AS INT64)  AS populacao
  FROM `basedosdados.br_ibge_censo_2022.populacao_idade_sexo` AS dados
)
SELECT
  b.id_municipio,
  d.nome AS id_municipio_nome,
  SUM(b.populacao) AS pop_total,
  SUM(CASE WHEN b.idade_anos BETWEEN 0 AND 4 THEN b.populacao ELSE 0 END) AS pop_0a4
FROM base b
LEFT JOIN (
  SELECT DISTINCT id_municipio, nome
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
) d
ON b.id_municipio = d.id_municipio
GROUP BY b.id_municipio, id_municipio_nome
ORDER BY b.id_municipio
")

# Baixa e calcula percentual
criancas_0a4_por_municipio <- basedosdados::read_sql(
  sql, billing_project_id = basedosdados::get_billing_id()
) %>%
  mutate(
    pct_0a4 = if_else(pop_total > 0, 100 * pop_0a4 / pop_total, NA_real_)
  )

# (Opcional) manter também um ID de 6 dígitos, se você usa esse padrão em outros bancos
criancas_0a4_por_municipio <- criancas_0a4_por_municipio %>%
  mutate(id_municipio_6 = sub("^([0-9]{6}).*$", "\\1", id_municipio)) %>%
  relocate(id_municipio, id_municipio_6, id_municipio_nome, pop_total, pop_0a4, pct_0a4)

criancas_0a4_por_municipio.1 <- criancas_0a4_por_municipio %>%
  select(id_municipio_6, pct_0a4, pop_total)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Área e grande regiao                |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# carregar malha municipal
mun <- read_municipality(year = 2020)
mun <- mun %>%
  mutate(area_km2 = as.numeric(st_area(geom)) / 10^6)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Votos em Bolsonaro                  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

setwd("/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw/regressao")

eleicoes <- read_csv2(
  file   = "cand_mais_votado-municipio_presidente_2022.csv",
  locale = locale(encoding = "Latin1"),  # troque para "UTF-8" se precisar
  show_col_types = FALSE
) |>
  clean_names() # nomes de colunas em snake_case

# Se o arquivo tiver outras eleições/turnos, mantenha estes filtros:
eleicoes_wide <- eleicoes %>%
  filter(ds_cargo == "Presidente",
         nr_turno %in% c(2, "2", "2º turno", "segundo turno", "2º TURNO"),
         nr_candidato %in% c(13, 22)) %>%
  mutate(qt_votos_nom_validos = as.numeric(qt_votos_nom_validos)) %>%   # garante numérico
  group_by(sg_uf, nm_municipio, nr_candidato) %>%
  summarise(votos = sum(qt_votos_nom_validos, na.rm = TRUE), .groups = "drop") %>%
  # abre em colunas: votos_13 e votos_22
  mutate(nr_candidato = paste0("votos_", nr_candidato)) %>%
  pivot_wider(names_from = nr_candidato, values_from = votos, values_fill = 0)

eleicoes_wide <- eleicoes_wide %>%
  mutate(total_validos = votos_13 + votos_22,
         prop_13 = if_else(total_validos > 0, votos_13 / total_validos, NA_real_),
         prop_22 = if_else(total_validos > 0, votos_22 / total_validos, NA_real_))

normalize_nome <- function(x) {
  x |>
    str_trim() |>
    str_to_upper(locale = "pt") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^A-Z0-9 ]", " ") |>
    str_squish() |>
    rm_accent()
  }

#------------------------------------------------------------
# 1) CHAVE a partir do eleicoes_wide (já com votos_13 / votos_22)
#    -> mantenho também total_validos / prop_13 / prop_22, se existirem
#------------------------------------------------------------
eleicoes_key <- eleicoes_wide %>%
  mutate(nm_municipio_norm = normalize_nome(nm_municipio)) %>%
  select(sg_uf, nm_municipio_norm, nm_municipio,
    matches("^votos_"), matches("^prop_"), total_validos) %>%
  distinct()

#------------------------------------------------------------
# 2) SUA TABELA MUNICIPAL (alvo do casamento)
#    Ex.: outra_tbl com id_municipio, sg_uf, name_muni
#------------------------------------------------------------
mun <- mun %>%
  mutate(nm_municipio_norm = normalize_nome(name_muni))

ex_cols <- c("votos_13", "votos_22", "total_validos", "prop_13", "prop_22")

eleicoes_key <- eleicoes_wide %>%
  mutate(nm_municipio_norm = normalize_nome(nm_municipio)) %>%
  select(
    sg_uf, nm_municipio, nm_municipio_norm,
    any_of(ex_cols)
  ) %>%
  distinct()

# 2) SUA TABELA MUNICIPAL (mun) já tem:
#    - code_muni (ID do município)
#    - abbrev_state (UF)
#    - name_muni (nome)
#    - nm_municipio_norm (nome normalizado)  [já feito por você]

# 3) JOIN EXATO por (UF + nome_normalizado)
joined_exato <- mun %>%
  left_join(
    eleicoes_key,
    by = c("abbrev_state" = "sg_uf",
           "nm_municipio_norm" = "nm_municipio_norm")
  ) %>%
  # renomeia o nome do município que veio do CSV das eleições
  rename(nm_municipio_ex = nm_municipio) %>%
  # adiciona sufixo _ex às métricas numéricas trazidas do join
  rename_with(~ paste0(.x, "_ex"), any_of(ex_cols))

# Registros que ainda não casaram
restantes <- joined_exato %>%
  filter(is.na(prop_22_ex)) %>%
  select(code_muni, abbrev_state, nm_municipio_norm) %>%
  distinct()

# 4) JOIN APROXIMADO (fuzzy) *dentro da mesma UF*
#    - Jaro-Winkler (0=igual, ~1=diferente); ajuste max_dist se precisar
candidatos_por_uf <- eleicoes_key %>%
  select(sg_uf, nm_municipio_norm, nm_municipio) %>%
  distinct()

fuzzy_idx <- restantes %>%
  stringdist_left_join(
    candidatos_por_uf,
    by = c("nm_municipio_norm" = "nm_municipio_norm"),
    method = "jw", distance_col = "dist", max_dist = 0.15
  ) %>%
  # garante MESMA UF
  filter(abbrev_state == sg_uf) %>%
  group_by(code_muni) %>%
  slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    code_muni,
    abbrev_state,
    nm_municipio_norm_match = nm_municipio_norm.y,  # chave normalizada do eleicoes_wide
    nm_municipio_fz = nm_municipio,                 # nome original (p/ conferência)
    dist
  )

# colunas numéricas a trazer do eleicoes_key
ex_cols <- c("votos_13", "votos_22", "total_validos", "prop_13", "prop_22")

# --- Traz as colunas de votos/props para o match fuzzy (evitando duplicação de nomes) ---
fuzzy_enriched <- fuzzy_idx %>%
  left_join(
    eleicoes_key %>%
      # evita colidir com nm_municipio_fz que já existe em fuzzy_idx
      rename(nm_municipio_src = nm_municipio),
    by = c("abbrev_state" = "sg_uf",
           "nm_municipio_norm_match" = "nm_municipio_norm")
  ) %>%
  # adiciona sufixo _fz apenas às métricas numéricas vindas do eleicoes_key
  rename_with(~ paste0(.x, "_fz"), any_of(ex_cols))

# 5) CONSOLIDA: prioriza EXATO; se ausente, usa FUZZY
final_match <- joined_exato %>%
  left_join(
    fuzzy_enriched,
    by = c("code_muni", "abbrev_state")
  ) %>%
  mutate(
    nm_municipio_resolvido = coalesce(nm_municipio_ex, nm_municipio_fz),
    votos_13       = coalesce(votos_13_ex,       votos_13_fz),
    votos_22       = coalesce(votos_22_ex,       votos_22_fz),
    total_validos  = coalesce(total_validos_ex,  total_validos_fz),
    prop_13        = coalesce(prop_13_ex,        prop_13_fz),
    prop_22        = coalesce(prop_22_ex,        prop_22_fz),
    fonte_casamento = case_when(
      !is.na(nm_municipio_ex) ~ "exato",
      is.na(nm_municipio_ex) & !is.na(nm_municipio_fz) ~ "fuzzy",
      TRUE ~ "sem_match"
    )
  ) %>%
  select(
    code_muni, abbrev_state, name_muni,
    nm_municipio_resolvido, fonte_casamento, dist,
    votos_13, votos_22, total_validos, prop_13, prop_22,
    everything()) %>%
  mutate(prop_22_ex = ifelse(is.na(prop_22_ex), prop_22_fz, prop_22_ex)) %>%
  select(code_muni, prop_22_ex)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Abrindo arquivos baixados na pasta  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Defina a pasta (já está no seu setwd, mas deixei explícito)
dir_path <- "/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw/regressao"
setwd(dir_path)

# Lista apenas CSV e XLSX
arqs <- list.files(path = dir_path, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)

# Função para transformar o nome do arquivo em um nome de objeto válido
obj_name_from <- function(filepath) {
  nm <- tools::file_path_sans_ext(basename(filepath))   # tira caminho e extensão
  nm <- gsub("[^[:alnum:]_]+", "_", nm)                 # substitui espaços/acentos/pontos por "_"
  nm <- gsub("^_|_$", "", nm)                           # remove "_" no início/fim se houver
  nm
}

# Lê cada arquivo e cria um objeto com o nome correspondente
for (f in arqs) {
  nm <- obj_name_from(f)
  ext <- tolower(tools::file_ext(f))
  
  if (ext == "csv") {
    dat <- readr::read_csv(f, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  } else if (ext == "xlsx") {
    dat <- readxl::read_excel(f)
  } else {
    next
  }
  
  assign(nm, dat, envir = .GlobalEnv)
  message(sprintf("Objeto criado: %s  <-  %s", nm, basename(f)))
}

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Escolaridade                        |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

escolaridade <- escolaridade %>%
  rename_with(
    .fn = ~ paste0("coluna", seq_along(.)),
    .cols = everything()
  ) 

escolaridade1 <- escolaridade %>%
  # remove as 5 primeiras linhas
  slice(-(1:5)) %>%
  # ajusta a coluna 1 (id_municipio) para ter apenas 6 dígitos
  mutate(coluna1 = substr(as.character(coluna1), 1, 6)) %>% select(-coluna2, -coluna3) %>%
  rename(media.anos.estudos = coluna4 )

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Estabelecimento                     |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

setwd("/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw/regressao")
# caminho do arquivo
arq <- "estabelecimentos.csv"

# 1) Descobrir a primeira linha "boa" (com vários ;)
linhas <- readr::read_lines(arq, n_max = 200)  # olhe só o começo
tem_ponto_virgula <- stringr::str_count(linhas, ";")

# regra: a primeira linha com >= 3 ; provavelmente é o cabeçalho real
linha_header <- which(tem_ponto_virgula >= 3)[1]
if (is.na(linha_header)) stop("Não encontrei linha de cabeçalho com ';' no início do arquivo.")

# 2) Ler o CSV a partir dessa linha
#    - skip = linha_header - 1 (pula o que vem antes)
#    - delim = ';'
#    - locale Latin-1 para evitar '�'
#    - NA extras (traços etc.)
dados_brutos <- readr::read_delim(
  file   = arq,
  delim  = ";",
  skip   = linha_header - 1,
  col_names = TRUE,
  locale = readr::locale(encoding = "Latin1", decimal_mark = ","),
  na = c("", "NA", "-", "–", "—")
)

# 3) Normalizar nomes de colunas
names(dados_brutos) <- janitor::make_clean_names(names(dados_brutos))

# 4) Separar a coluna de município (ex.: "110001 ALTA FLORESTA D'OESTE")
#    Ajuste o nome da coluna abaixo caso venha ligeiramente diferente (use names(dados_brutos) para conferir)
col_muni <- names(dados_brutos)[1]  # normalmente a 1ª coluna é "município"
dados <- dados_brutos %>%
  mutate(
    # extrai 6 primeiros dígitos como id
    id_municipio   = str_extract(.data[[col_muni]], "^[0-9]{6}"),
    nome_municipio = str_trim(str_remove(.data[[col_muni]], "^[0-9]{6}\\s*"))
  ) %>%
  relocate(id_municipio, nome_municipio) %>%
  select(-all_of(col_muni))

# 5) (Opcional) Converter as demais colunas numéricas
#    Muitas vêm como texto com vírgula decimal; vamos padronizar para número
cols_num <- setdiff(names(dados), c("id_municipio", "nome_municipio"))
dados <- dados %>%
  mutate(across(all_of(cols_num), ~ {
    x <- as.character(.x)
    x <- str_replace_all(x, "\\.", "")   # se houver separador de milhar ponto
    x <- str_replace_all(x, ",", ".")    # vírgula -> ponto decimal
    suppressWarnings(as.numeric(x))
  }))

estabelecimentos.1 <- dados %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)),
         n_estabelecimento = posto_de_saude + centro_de_saude_unidade_basica + pronto_atendimento + centro_de_imunizacao +
           centro_de_apoio_a_saude_da_familia + unidade_de_vigilancia_em_saude + unidade_de_atencao_a_saude_indigena +
           unidade_de_atencao_em_regime_residencial + hospital_geral) %>%
  select(id_municipio, n_estabelecimento) %>%
  filter(!is.na(id_municipio))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Rural                               |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°
rural2 <- rural %>%
  slice(-(1:5)) %>% 
  rename_with(
    .fn = ~ paste0("coluna", seq_along(.)),
    .cols = everything()
  ) %>%
  select(-2) %>%
  mutate(
    # converte valores não numéricos para NA
    coluna5 = readr::parse_number(coluna5),
    prop_rural = coluna5 / as.numeric(coluna3) * 100,
    coluna1 = substr(as.character(coluna1), 1, 6)) %>%
  select(1,5)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Religião                            |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

religiao2 <- religiao %>%
  slice(-(1:7)) %>% 
  rename_with(
    .fn = ~ paste0("coluna", seq_along(.)),
    .cols = everything()) %>%
  select(1, 3, 5) %>%
  mutate(prop_evangelicos = as.numeric(coluna5) / as.numeric(coluna3),
         coluna1 = substr(as.character(coluna1), 1, 6)) %>%
  select(1, 4) %>%
  mutate(prop_evangelicos = round(prop_evangelicos * 100, 2))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Pobreza                             |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°
pobreza2 <- pobreza %>%
  rename_with(
    .fn = ~ paste0("coluna", seq_along(.)),
    .cols = everything()) %>%
  select(1, 5) %>%
  rename(n_pobres = coluna5)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Perctual pretos e pardos e indig.   |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

sql <- "
WITH base AS (
  SELECT
    CAST(dados.id_municipio AS STRING)               AS id_municipio,
    LOWER(dados.cor_raca)                            AS cor_raca,
    SAFE_CAST(dados.populacao AS INT64)              AS populacao
  FROM `basedosdados.br_ibge_censo_2022.populacao_grupo_idade_sexo_raca` AS dados
),
agregado AS (
  SELECT
    id_municipio,
    SUM(populacao) AS pop_total,
    SUM(CASE WHEN cor_raca IN ('preta','parda') THEN populacao ELSE 0 END) AS pop_pretos_pardos,
    SUM(CASE WHEN REGEXP_CONTAINS(cor_raca, 'indígena') THEN populacao ELSE 0 END) AS pop_indigenas
  FROM base
  GROUP BY id_municipio
)
SELECT
  a.id_municipio,
  d.nome AS id_municipio_nome,
  a.pop_total,
  a.pop_pretos_pardos,
  a.pop_indigenas,
  100 * SAFE_DIVIDE(a.pop_pretos_pardos, a.pop_total) AS pct_pretos_pardos,
  100 * SAFE_DIVIDE(a.pop_indigenas,     a.pop_total) AS pct_indigenas
FROM agregado a
LEFT JOIN (
  SELECT DISTINCT id_municipio, nome
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
) d
ON a.id_municipio = d.id_municipio
ORDER BY a.id_municipio
"

raca_municipio <- basedosdados::read_sql(
  sql,
  billing_project_id = basedosdados::get_billing_id()
) %>%
  mutate(
    # opcional: criar ID de 6 dígitos, se você usa esse padrão
    id_municipio_6 = sub("^([0-9]{6}).*$", "\\1", id_municipio),
    across(c(pct_pretos_pardos, pct_indigenas), ~ round(.x, 2))
  ) %>%
  relocate(id_municipio, id_municipio_6, id_municipio_nome,
           pop_total, pop_pretos_pardos, pop_indigenas,
           pct_pretos_pardos, pct_indigenas)

raca_municipio.1 <- raca_municipio %>%
  select(id_municipio_6, pct_pretos_pardos, pct_indigenas)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Juntando tabelas                    |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

tabela_coberturas_4anos_resumo.1 <- tabela_coberturas_4anos_resumo %>%
  filter(!is.na(name_muni)) %>%
  mutate(media_diff_21_15 = (diff_bcg_21_15 + diff_polio_21_15 + diff_dtp_21_15 + diff_tv1_21_15) / 4) 

mun.1 <- mun %>%
  select(code_muni, name_region, area_km2) %>%
  mutate(code_muni = substr(as.character(code_muni), 1, 6)) %>%
  st_set_geometry(NULL)

tabela_coberturas_4anos_resumo.2 <- tabela_coberturas_4anos_resumo.1 %>%
  left_join(mun.1, by=c("id_municipio"="code_muni")) %>% # calcular densidade demográfica 
  left_join(pobreza2 %>% mutate(coluna1 = as.character(coluna1)), # calcular precentual de pobres 
            by=c("id_municipio"="coluna1")) %>%
  left_join(religiao2, by=c("id_municipio"="coluna1")) %>%
  left_join(rural2, by=c("id_municipio"="coluna1")) %>%
  left_join(estabelecimentos.1, by=c("id_municipio"="id_municipio")) %>% # calcular pessoas por estabelecimento de saúde 
  left_join(escolaridade1, by=c("id_municipio"="coluna1")) %>%
  left_join(final_match %>% st_set_geometry(NULL) %>%
              mutate(code_muni = substr(as.character(code_muni), 1, 6)), 
            by=c("id_municipio"="code_muni")) %>%
  left_join(criancas_0a4_por_municipio.1, by=c("id_municipio"="id_municipio_6")) %>%
  left_join(raca_municipio.1, by=c("id_municipio"="id_municipio_6")) %>%
  mutate(prop_pobres = n_pobres / pop_total * 100,
         densidade_demografica = pop_total / area_km2,
         pessoas_por_estabelecimento_saude = pop_total / n_estabelecimento,
         prop_22_ex = prop_22_ex * 100,
         media.anos.estudos = as.numeric(media.anos.estudos),
         faixa_estudos = cut(
           media.anos.estudos,
           breaks = c(-Inf, 6, 8, 10, Inf),
           labels = c("≤6 anos", "6–8 anos", "8–10 anos", ">10 anos"),
           right = TRUE),
         pop_total_10mil = pop_total / 10000)
  
# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Rodar regressão linear              |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# garante que não tem NA nas variáveis de interesse
dados_modelo <- tabela_coberturas_4anos_resumo.2 %>%
  select(
    media_diff_21_15,
    prop_pobres,
    densidade_demografica,
    pessoas_por_estabelecimento_saude,
    media.anos.estudos,
    prop_rural,
    prop_evangelicos,
    name_region,
    faixa_estudos,
    prop_22_ex, pct_pretos_pardos, pct_indigenas,
    pct_0a4, pop_total_10mil,
  ) %>%
  drop_na()

# Se ainda não estiver, garanta que 'faixa_estudos' é fator nas faixas que você criou
# (ajuste os levels conforme seu código anterior)
dados_modelo_ne <- dados_modelo %>%
  mutate(
    faixa_estudos = factor(faixa_estudos)  # mantenha as faixas já criadas
  ) %>%
  # opcional: remover linhas com NA nas variáveis usadas
  tidyr::drop_na(media_diff_21_15, prop_pobres, densidade_demografica,
                 pessoas_por_estabelecimento_saude, faixa_estudos,
                 prop_22_ex, prop_rural, prop_evangelicos, name_region, 
                 pct_pretos_pardos, pct_indigenas,
                 pct_0a4, pop_total_10mil)

# Modelo com Nordeste como referência
mod_ne <- lm(
  media_diff_21_15 ~ prop_pobres + densidade_demografica +
    pessoas_por_estabelecimento_saude + faixa_estudos + pct_pretos_pardos + pct_indigenas +
    pct_0a4 + pop_total_10mil +
    prop_22_ex + prop_rural + prop_evangelicos + factor(name_region),
  data = dados_modelo_ne
)

summary(mod_ne)
# (opcional) Tabela enxuta
broom::tidy(mod_ne) %>% arrange(p.value)

# Extrai coeficientes com IC 95%
coefs <- broom::tidy(mod_ne, conf.int = TRUE)

# Remove intercepto para facilitar leitura
coefs_plot <- coefs %>%
  filter(term != "(Intercept)")

ggplot(coefs_plot, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkred") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Efeito estimado (pontos percentuais)",
    y = "",
    title = "Coeficientes do modelo de regressão",
    subtitle = "Variação média da cobertura vacinal (2021–2015)",
    caption = "Fonte: elaboração própria"
  ) +
  theme_minimal(base_size = 12)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Testes de outliers                  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Teste de outliers
influencePlot(mod_ne, id.method="identify")

# ----------------------------
# 1) MODELO BASE (se já tiver mod_ne, pode pular)
# ----------------------------
form <- media_diff_21_15 ~  densidade_demografica +
  pessoas_por_estabelecimento_saude + faixa_estudos + pct_pretos_pardos + pct_indigenas +
  pct_0a4 + prop_22_ex + prop_rural + prop_evangelicos + factor(name_region) 

# ----------------------------
# 2) Identificar observações influentes
#    (regras comuns: |resíduo studentizado| > 3,
#     alavancagem .hat > 2p/n, Cook's D > 4/n)
# ----------------------------
aug <- augment(mod_ne)  # adiciona .cooksd, .hat, .std.resid
n <- nobs(mod_ne)
p <- length(coef(mod_ne))

cut_hat  <- 2 * p / n
cut_cook <- 4 / n

infl_idx <- aug %>%
  mutate(row_id = row_number(),
         flag = abs(.std.resid) > 3 |
           .hat > cut_hat      |
           .cooksd > cut_cook) %>%
  filter(flag) %>%
  pull(row_id)

cat("Obs. influentes detectadas:", length(infl_idx), "\n")
if ("id_municipio" %in% names(dados_modelo)) {
  cat("Primeiras influentes (id_municipio):\n")
  print(dados_modelo$id_municipio[infl_idx][1:min(10, length(infl_idx))])
}

# ----------------------------
# 3) Reestimar SEM influentes
# ----------------------------
dados_sens <- dados_modelo[-infl_idx, , drop = FALSE]
mod_ne_sens <- lm(form, data = dados_sens)

# ----------------------------
# 4) Comparar coeficientes com erros-robustos (HC3)
# ----------------------------
tidy_hc3 <- function(m, label){
  broom::tidy(m,
              conf.int = TRUE,
              conf.level = 0.95,
              vcov = sandwich::vcovHC(m, type = "HC3")) %>%
    mutate(model = label)
}

tab_comp <- bind_rows(
  tidy_hc3(mod_ne,       "Completo"),
  tidy_hc3(mod_ne_sens,  "Sem influentes")
) %>%
  # tirar o intercepto do gráfico (opcional)
  filter(term != "(Intercept)")

# Tabela rápida de comparação
tab_comp %>%
  select(model, term, estimate, std.error, conf.low, conf.high, p.value) %>%
  arrange(term, model) -> comparacao_coef
print(comparacao_coef, n = Inf)

# ----------------------------
# 5) Gráfico de coeficientes (com IC robustos) comparando os dois modelos
# ----------------------------
ggplot(tab_comp,
       aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high,
           color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  labs(title = "Comparação de coeficientes (HC3)",
       subtitle = "Modelo completo vs. sem observações influentes",
       x = "Efeito estimado (p.p.)", y = NULL, color = NULL) +
  theme_minimal(base_size = 12)

# ----------------------------
# 6) Estatísticas de ajuste para texto do artigo
# ----------------------------
# Coefs com EP robusto (HC3)
tidy_hc3 <- function(m, rotulo){
  broom::tidy(m, conf.int = TRUE, conf.level = 0.95,
              vcov = sandwich::vcovHC(m, type = "HC3")) |>
    dplyr::mutate(Modelo = rotulo)
}

tab <- dplyr::bind_rows(
  tidy_hc3(mod_ne, "Completo"),
  tidy_hc3(mod_ne_sens, "Sem influentes")
) |>
  dplyr::select(Modelo, term, estimate, std.error, conf.low, conf.high, p.value) |>
  dplyr::mutate(dplyr::across(c(estimate, std.error, conf.low, conf.high, p.value), ~round(., 4))) |>
  dplyr::rename(Variável = term, Estimativa = estimate, `EP (HC3)` = std.error,
                `IC95% baixo` = conf.low, `IC95% alto` = conf.high, `p-valor` = p.value)

gt_tab <- gt(tab) |>
  tab_header(
    title = "Determinantes da variação média da cobertura vacinal (2021–2015)",
    subtitle = "Erros-padrão robustos (HC3); IC95% entre colunas."
  ) |>
  tab_options(table.font.size = px(13))
gtsave(gt_tab, "tabela_regressao.png")

# ----------------------------
# 7) Analisando os 600 influentes
# ----------------------------

# 2) Anexar um ID de linha que bate com dados_modelo e marcar influentes
dados_infl <- dados_modelo %>%
  mutate(.row_id = dplyr::row_number()) %>%
  bind_cols(aug %>% select(.std.resid, .hat, .cooksd)) %>%
  mutate(influente = abs(.std.resid) > 3 | .hat > cut_hat | .cooksd > cut_cook)

# 3) Filtrar só os influentes (para você inspecionar)
influentes <- dados_infl %>% filter(influente)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Testes de Multicolineariedade       |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

library(car)
vif(mod_ne)                  # atenção se VIF > 5 (ou >10)

cor.test(dados_modelo_ne$prop_pobres, dados_modelo_ne$prop_22_ex,
         use = "complete.obs", method = "pearson")

cor.test(dados_modelo_ne$pct_pretos_pardos, dados_modelo_ne$prop_22_ex,
         use = "complete.obs", method = "pearson")

cor.test(dados_modelo_ne$pct_pretos_pardos, dados_modelo_ne$prop_pobres,
         use = "complete.obs", method = "pearson")

cor.test(dados_modelo_ne$densidade_demografica, dados_modelo_ne$pop_total_10mil,
         use = "complete.obs", method = "pearson")

cor.test(dados_modelo_ne$prop_evangelicos, dados_modelo_ne$prop_22_ex,
         use = "complete.obs", method = "pearson")

cor.test(dados_modelo_ne$prop_evangelicos, dados_modelo_ne$pct_pretos_pardos,
         use = "complete.obs", method = "pearson")

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Comparando 3 cenários               |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# ----------------------------
# 1) Fórmulas
# ----------------------------
form1 <- media_diff_21_15 ~ densidade_demografica +
  pessoas_por_estabelecimento_saude + faixa_estudos + pct_indigenas +
  pct_0a4 + prop_22_ex + prop_rural + prop_evangelicos + factor(name_region)

form2 <- update(form1, . ~ . + pct_pretos_pardos)

# ----------------------------
# 2) Modelos 1 e 2
# ----------------------------
mod1 <- lm(form1, data = dados_modelo)
mod2 <- lm(form2, data = dados_modelo)

# ----------------------------
# 3) Identificar observações influentes
# ----------------------------
aug2 <- augment(mod2)  # usando modelo com pretos/pardos
n <- nobs(mod2)
p <- length(coef(mod2))

cut_hat  <- 2 * p / n
cut_cook <- 4 / n

infl_idx <- aug2 %>%
  mutate(row_id = row_number(),
         flag = abs(.std.resid) > 3 |
           .hat > cut_hat |
           .cooksd > cut_cook) %>%
  filter(flag) %>%
  pull(row_id)

cat("Obs. influentes detectadas:", length(infl_idx), "\n")

dados_sens <- dados_modelo[-infl_idx, , drop = FALSE]
mod3 <- lm(form2, data = dados_sens)

# ----------------------------
# 4) Função tidy com EP robusto
# ----------------------------
tidy_hc3 <- function(m, label){
  broom::tidy(m, conf.int = TRUE, conf.level = 0.95,
              vcov = sandwich::vcovHC(m, type = "HC3")) %>%
    mutate(Modelo = label)
}

# ----------------------------
# 5) Comparar coeficientes
# ----------------------------
tab_comp <- bind_rows(
  tidy_hc3(mod1, "Sem pretos/pardos"),
  tidy_hc3(mod2, "Com pretos/pardos"),
  tidy_hc3(mod3, "Com pretos/pardos (sem influentes)")
) %>%
  filter(term != "(Intercept)")
print(tab_comp, n = Inf)

# ----------------------------
# 6) Tabela final (para artigo)
# ----------------------------
# ---- Função auxiliar: EP robusto + estrelas ----
tidy_hc3_stars <- function(model, model_label){
  broom::tidy(
    model,
    conf.int   = TRUE,
    conf.level = 0.95,
    vcov       = sandwich::vcovHC(model, type = "HC3")
  ) |>
    mutate(
      # códigos de significância
      stars = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        p.value < 0.1   ~ ".",
        TRUE            ~ ""
      ),
      Modelo = model_label
    ) |>
    # arredondamento para exibição
    mutate(
      estimate  = round(estimate, 4),
      std.error = round(std.error, 4),
      conf.low  = round(conf.low, 4),
      conf.high = round(conf.high, 4),
      p.value   = round(p.value, 4)
    )
}

# ---- Tabelas separadas para cada modelo ----
make_table <- function(tidy_df, file_png){
  out <- tidy_df |>
    # JÁ COM INTERCEPTO (não filtrar o "(Intercept)")
    transmute(
      Variável        = term,
      `Estimativa`    = paste0(estimate, stars),
      `EP (HC3)`      = std.error,
      `IC95% baixo`   = conf.low,
      `IC95% alto`    = conf.high,
      `p-valor`       = p.value
    )
  
  gt(out) |>
    tab_header(
      title = "Regressão linear – Variação média da cobertura (2021–2015)",
      subtitle = unique(tidy_df$Modelo)
    ) |>
    tab_spanner_delim(" ") |>
    cols_label(
      Variável = "Termo",
      `Estimativa` = "Coeficiente",
      `EP (HC3)` = "Erro-padrão (HC3)",
      `IC95% baixo` = "IC 95% (inferior)",
      `IC95% alto`  = "IC 95% (superior)",
      `p-valor` = "p-valor"
    ) |>
    tab_options(
      table.font.size    = gt::px(14),
      data_row.padding   = gt::px(4),
      heading.title.font.size = gt::px(16),
      heading.subtitle.font.size = gt::px(14)
    ) |>
    gtsave(filename = file_png)
}

# ---- Gerar e salvar as 3 tabelas ----
tab1 <- tidy_hc3_stars(mod1, "Modelo 1: Sem pretos/pardos")
tab2 <- tidy_hc3_stars(mod2, "Modelo 2: Com pretos/pardos")
tab3 <- tidy_hc3_stars(mod3, "Modelo 3: Com pretos/pardos (sem influentes)")

make_table(tab1, "tabela_modelo1_sem_pretos_pardos.png")
make_table(tab2, "tabela_modelo2_com_pretos_pardos.png")
make_table(tab3, "tabela_modelo3_com_pretos_pardos_sem_influentes.png")

message("Arquivos salvos:",
        "\n- tabela_modelo1_sem_pretos_pardos.png",
        "\n- tabela_modelo2_com_pretos_pardos.png",
        "\n- tabela_modelo3_com_pretos_pardos_sem_influentes.png")

# ----------------------------
# 7) Gráfico de coeficientes
# ----------------------------
ggplot(tab_comp,
       aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high,
           color = Modelo)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.7)) +
  labs(title = "Comparação de coeficientes (HC3)",
       subtitle = "Três especificações do modelo",
       x = "Efeito estimado (p.p.)", y = NULL, color = NULL) +
  theme_minimal(base_size = 12)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Homocedasticidade.                  |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Coloque aqui seus três objetos lm já estimados
mods <- list(
  `Sem pretos/pardos`                   = mod1,
  `Com pretos/pardos`                   = mod2,
  `Com pretos/pardos (sem influentes)`  = mod3)

# 1) Testes de homoscedasticidade
homosc_tests <- imap_dfr(mods, ~{
  m <- .x
  label <- .y
  # Breusch-Pagan (p<0.05 sugere heterocedasticidade)
  bp <- bptest(m)
  # White test (versão com termos quadráticos do ajustado)
  w  <- bptest(m, ~ fitted(m) + I(fitted(m)^2))
  
  tibble(
    Modelo        = label,
    BP_stat       = unname(bp$statistic),
    BP_df         = unname(bp$parameter),
    BP_pvalue     = unname(bp$p.value),
    White_stat    = unname(w$statistic),
    White_df      = unname(w$parameter),
    White_pvalue  = unname(w$p.value))
})

# 2) Tabela comparativa (interpretação: p<0.05 -> heterocedasticidade)
homosc_tests %>%
  mutate(across(ends_with("pvalue"), ~round(.x, 4)),
         across(ends_with("stat"),   ~round(.x, 3))) %>%
  arrange(Modelo) -> homosc_table

print(homosc_table, n = Inf)

# 3) Checagem visual: Resíduos vs. Ajustados (um painel por modelo)
plot_resid_fit <- function(m, title){
  df <- tibble(fit = fitted(m), res = resid(m))
  ggplot(df, aes(x = fit, y = res)) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 0.3, color = "grey50") +
    geom_point(alpha = 0.35, size = 1) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.6) +
    labs(title = title, x = "Valores ajustados", y = "Resíduos") +
    theme_minimal(base_size = 12)
}

p1 <- plot_resid_fit(mods[[1]], names(mods)[1])
p2 <- plot_resid_fit(mods[[2]], names(mods)[2])
p3 <- plot_resid_fit(mods[[3]], names(mods)[3])

# Se tiver o patchwork instalado, você pode combinar assim:
# install.packages("patchwork")
# library(patchwork)
# (p1 | p2 | p3)

# Sem patchwork: imprima individualmente
print(p1); print(p2); print(p3)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##| Normalidade dos resíduos             |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

models <- list(
  "Sem pretos/pardos"                  = mod1,
  "Com pretos/pardos"                  = mod2,
  "Com pretos/pardos (sem influentes)" = mod3)

## QQ-plots lado a lado (base R)
png("qqplots_residuos_base.png", width = 1600, height = 550, res = 130)
par(mfrow = c(1, 3), mar = c(4,4,2,1))
for(nm in names(models)){
  r <- residuals(models[[nm]])
  qqnorm(r, main = nm, pch = 19, cex = 0.5, col = "grey30")
  qqline(r, col = "red", lwd = 2)
}
par(mfrow = c(1,1))
dev.off()
cat("Salvo: qqplots_residuos_base.png\n")

## Tabela dos testes formais de normalidade
normal_tests <- do.call(rbind, lapply(names(models), function(nm){
  r  <- residuals(models[[nm]])
  ad <- nortest::ad.test(r)
  jb <- tseries::jarque.bera.test(r)
  data.frame(
    Modelo    = nm,
    AD_stat   = unname(ad$statistic), AD_pvalue = ad$p.value,
    JB_stat   = unname(jb$statistic), JB_pvalue = jb$p.value,
    row.names = NULL
  )
}))
normal_tests[] <- lapply(normal_tests, function(x) if(is.numeric(x)) round(x, 4) else x)
print(normal_tests)

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Outliers                            |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

par(mfrow = c(1,3))
plot(fitted(mod1), rstandard(mod1),
     main = "Sem pretos/pardos", xlab = "Ajustados", ylab = "Resíduos studentizados")
abline(h = c(-3,3), col = "red", lty = 2)

plot(fitted(mod2), rstandard(mod2),
     main = "Com pretos/pardos", xlab = "Ajustados", ylab = "Resíduos studentizados")
abline(h = c(-3,3), col = "red", lty = 2)

plot(fitted(mod3), rstandard(mod3),
     main = "Com pretos/pardos (sem influentes)", xlab = "Ajustados", ylab = "Resíduos studentizados")
abline(h = c(-3,3), col = "red", lty = 2)
par(mfrow = c(1,1))

#----------------------------------------------
par(mfrow = c(1,3))
plot(cooks.distance(mod1), main="Sem pretos/pardos", ylab="Cook's D", pch=19, cex=0.5)
abline(h = 4/nobs(mod1), col="red", lty=2)

plot(cooks.distance(mod2), main="Com pretos/pardos", ylab="Cook's D", pch=19, cex=0.5)
abline(h = 4/nobs(mod2), col="red", lty=2)

plot(cooks.distance(mod3), main="Com pretos/pardos (sem influentes)", ylab="Cook's D", pch=19, cex=0.5)
abline(h = 4/nobs(mod3), col="red", lty=2)
par(mfrow = c(1,1))

#----------------------------------------------

library(car)
par(mfrow = c(1,3))
influencePlot(mod1, main="Sem pretos/pardos", id.method="none", scale=8)
influencePlot(mod2, main="Com pretos/pardos", id.method="none", scale=8)
influencePlot(mod3, main="Com pretos/pardos (sem influentes)", id.method="none", scale=8)
par(mfrow = c(1,1))

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Especificação: RESET de Ramsey      |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

library(lmtest)
# RESET de Ramsey
reset1 <- resettest(mod1, power = 2:3, type = "fitted")
reset2 <- resettest(mod2, power = 2:3, type = "fitted")
reset3 <- resettest(mod3, power = 2:3, type = "fitted")

# Visualizar resultados
reset1
reset2
reset3

# .--------------------------------------------.
# |############################################|
# |##.--------------------------------------.##|
# |##|  Comparação de modelos: testes       |##|
# |##°--------------------------------------°##|
# |############################################|
# '--------------------------------------------°

# Comparação AIC e BIC
aic_bic <- data.frame(
  Modelo = c("Sem pretos/pardos", "Com pretos/pardos", "Com pretos/pardos (sem influentes)"),
  AIC = c(AIC(mod1), AIC(mod2), AIC(mod3)),
  BIC = c(BIC(mod1), BIC(mod2), BIC(mod3)),
  R2_Ajustado = c(summary(mod1)$adj.r.squared,
                  summary(mod2)$adj.r.squared,
                  summary(mod3)$adj.r.squared)
)

print(aic_bic)

# Teste F parcial entre mod1 e mod2 (modelo aninhado: com/sem pretos e pardos)
anova(mod1, mod2)

# Stepwise (baseado em AIC)
library(MASS)
step_mod2 <- stepAIC(mod2, direction = "both", trace = FALSE)
summary(step_mod2)
oi
teste 
#                 ~~~ Fim ~~~
