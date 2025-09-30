library(DBI)
library(duckdb)

pasta_base <- "/Users/alexandrepichilinga/Documents/projetos_outros/mlg_vacinacao_trabalho_final/raw"
meses <- c("fev","mar","mai","jun")  # ajuste aqui se quiser outros meses

for (mes in meses) {
  cat("\n====================", toupper(mes), "====================\n")
  zip_path     <- file.path(pasta_base, sprintf("vacinacao_%s_2024.json.zip", mes))
  unzipped_dir <- file.path(pasta_base, sprintf("json_%s_2024", mes))   # caso já esteja deszipado
  tmp_dir      <- file.path(pasta_base, sprintf("json_%s_tmp", mes))
  bd_path      <- file.path(pasta_base, sprintf("pni_2024_%s.duckdb", mes))  # 1 banco por mês
  tabela       <- "vacinacao"
  
  # -------- descobre de onde virão os JSONs --------
  fonte_zip <- FALSE
  if (file.exists(zip_path)) {
    ok <- tryCatch({ unzip(zip_path, list = TRUE); TRUE }, error = function(e) FALSE)
    if (!ok) { message(">> ZIP corrompido/inabrível, pulando ", mes); next }
    lst   <- unzip(zip_path, list = TRUE)
    jsons <- lst$Name[grepl("\\.json$", lst$Name, TRUE)]
    if (!length(jsons)) { message(">> ZIP sem JSONs, pulando ", mes); next }
    fonte_zip <- TRUE
    if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  } else if (dir.exists(unzipped_dir)) {
    jsons <- basename(list.files(unzipped_dir, pattern="\\.json$", full.names = TRUE))
    if (!length(jsons)) { message(">> Pasta deszipada sem JSONs, pulando ", mes); next }
  } else {
    message(">> Não encontrei ZIP nem pasta de JSON para ", mes, ". Pulando.")
    next
  }
  
  # -------- recria o banco do mês do zero --------
  if (file.exists(bd_path)) file.remove(bd_path)
  con <- dbConnect(duckdb(), dbdir = bd_path, read_only = FALSE)
  dbExecute(con, "INSTALL json"); dbExecute(con, "LOAD json")
  dbExecute(con, "SET enable_progress_bar = true")  # mude para false se quiser silêncio
  
  # -------- processa 1 JSON por vez --------
  criada <- FALSE
  for (f in jsons) {
    jf <- if (fonte_zip) {
      dst <- file.path(tmp_dir, basename(f))
      if (!file.exists(dst)) unzip(zip_path, files = f, exdir = tmp_dir, junkpaths = TRUE)
      dst
    } else {
      file.path(unzipped_dir, basename(f))
    }
    
    if (!criada) {
      dbExecute(con, sprintf(
        "CREATE OR REPLACE TABLE %s AS SELECT * FROM read_json_auto('%s')", tabela, jf))
      criada <- TRUE
    } else {
      dbExecute(con, sprintf(
        "INSERT INTO %s BY NAME SELECT * FROM read_json_auto('%s')", tabela, jf))
    }
    
    if (file.exists(jf)) file.remove(jf)  # apaga o JSON já consumido
  }
  
  # -------- consolida e compacta --------
  dbExecute(con, "CHECKPOINT")
  dbExecute(con, "VACUUM")
  dbDisconnect(con, shutdown = TRUE)
  
  # limpa pasta tmp se vazia
  if (dir.exists(tmp_dir)) {
    rest <- list.files(tmp_dir, all.files = TRUE, no.. = TRUE)
    if (!length(rest)) unlink(tmp_dir, recursive = TRUE, force = TRUE)
  }
  
  cat("✅ Banco do mês salvo em:", bd_path, "\n")
}
