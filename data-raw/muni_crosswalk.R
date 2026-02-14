# Build the muni_crosswalk bundled dataset
# Source: GV-CEPESP official crosswalk
# https://github.com/GV-CEPESP/cepespdata/blob/main/tabelas_auxiliares/dados/codigo_municipio_ibge_tse.csv

raw <- read.csv("data-raw/muni_ibge_tse.csv", colClasses = "character")

muni_crosswalk <- data.frame(
  code_ibge     = raw$COD_MUN_IBGE,
  code_tse      = raw$COD_MUN_TSE,
  uf            = raw$UF,
  nome          = raw$NOME_MUNICIPIO,
  stringsAsFactors = FALSE
)

usethis::use_data(muni_crosswalk, overwrite = TRUE)
