
library(DBI)
library(RMariaDB)

conexao <- dbConnect(
  RMariaDB::MariaDB(),
  host = Sys.getenv("ECOMMERCE_DB_HOST"),
  port = 3306,
  user = Sys.getenv("ECOMMERCE_DB_USER"),
  password = Sys.getenv("ECOMMERCE_DB_PASSWORD"),
  dbname = "ecomcore"
)

tryCatch({
  
  consulta <- "
    WITH capturas_ordenadas AS (
      SELECT
        ca.id,
        tc.nome AS componente,
        tc.unidade_medida,
        ca.valor,
        ca.horario,
        s.nome AS servidor,
        mt.limite_atencao,
        mt.limite_critico,
        ROW_NUMBER() OVER (
          PARTITION BY tc.id
          ORDER BY ca.id DESC
        ) AS ordem
      FROM captura ca
      JOIN especificacao_tecnica et
        ON ca.fk_especificacao_tecnica = et.id
      JOIN componente co
        ON et.fk_componente = co.id
      JOIN tipo_componente tc
        ON co.fk_tipo_componente = tc.id
      JOIN servidor s
        ON et.fk_servidor = s.id
      JOIN metrica mt
        ON et.fk_metrica = mt.id
      WHERE s.id = ?
        AND et.ativo = 1
    )
    SELECT
      id,
      componente,
      unidade_medida,
      valor,
      horario,
      servidor,
      limite_atencao,
      limite_critico
    FROM capturas_ordenadas
    WHERE ordem <= 30
    ORDER BY id DESC
  "
  
  capturas <- dbGetQuery(
    conexao,
    consulta,
    params = list(1)
  )
  
  if (nrow(capturas) == 0) {
    stop("Nenhuma captura encontrada para o servidor selecionado.")
  }
  
  componentes <- unique(capturas$componente)
  
  ranking <- do.call(rbind, lapply(componentes, function(nome_componente) {
    
    dados_componente <- capturas[
      capturas$componente == nome_componente,
    ]
    
    media <- mean(dados_componente$valor, na.rm = TRUE)
    pico <- max(dados_componente$valor, na.rm = TRUE)
    
    limite_atencao <- dados_componente$limite_atencao[1]
    limite_critico <- dados_componente$limite_critico[1]
    
    indice <- (
      0.70 * (media / limite_critico) +
        0.30 * (pico / limite_critico)
    ) * 100
    
    estado <- if (pico >= limite_critico) {
      "Critico"
    } else if (pico >= limite_atencao) {
      "Atencao"
    } else {
      "Estavel"
    }
    
    data.frame(
      componente = nome_componente,
      unidade = dados_componente$unidade_medida[1],
      quantidade = nrow(dados_componente),
      media = round(media, 2),
      pico = round(pico, 2),
      indice = round(indice, 2),
      estado = estado
    )
  }))
  
  ranking <- ranking[
    order(ranking$indice, decreasing = TRUE),
  ]
  
  rownames(ranking) <- NULL
  ranking$posicao <- seq_len(nrow(ranking))
  
  ranking <- ranking[, c(
    "posicao",
    "componente",
    "unidade",
    "quantidade",
    "media",
    "pico",
    "indice",
    "estado"
  )]
  
  cat("\n--- Ranking dos componentes ---\n")
  print(ranking, row.names = FALSE)
  
  piores <- capturas[
    order(capturas$valor, decreasing = TRUE),
  ]
  
  piores <- head(piores, 10)
  
  piores <- piores[, c(
    "id",
    "componente",
    "valor",
    "unidade_medida",
    "horario",
    "servidor"
  )]
  
  cat("\n--- 10 maiores valores de captura ---\n")
  print(piores, row.names = FALSE)
  
}, finally = {
  dbDisconnect(conexao)
})
