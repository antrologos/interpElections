#' IBGE-TSE Municipality Code Crosswalk
#'
#' Official crosswalk table mapping IBGE 7-digit municipality codes to TSE
#' 5-digit electoral codes for all 5,710 Brazilian municipalities. Sourced
#' from GV-CEPESP.
#'
#' @format A data frame with 5,710 rows and 4 columns:
#' \describe{
#'   \item{code_ibge}{Character. IBGE 7-digit municipality code.}
#'   \item{code_tse}{Character. TSE 5-digit municipality code.}
#'   \item{uf}{Character. Two-letter state abbreviation (e.g., "SP", "RJ").}
#'   \item{nome}{Character. Municipality name in uppercase.}
#' }
#'
#' @examples
#' data(muni_crosswalk)
#' # Look up TSE code for Sao Paulo (IBGE 3550308)
#' muni_crosswalk[muni_crosswalk$code_ibge == "3550308", ]
#'
#' @source
#' GV-CEPESP, FGV:
#' \url{https://github.com/GV-CEPESP/cepespdata/blob/main/tabelas_auxiliares/dados/codigo_municipio_ibge_tse.csv}
#'
#' @keywords datasets
"muni_crosswalk"
