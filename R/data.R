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

#' Brazilian election dates
#'
#' Election dates for turno 1 (first round) and turno 2 (runoff) for all
#' Brazilian elections from 2000 to 2024. Used internally for auto-deriving
#' departure datetime when GTFS transit routing is enabled.
#'
#' @format A data frame with 26 rows and 3 columns:
#' \describe{
#'   \item{year}{Integer. Election year.}
#'   \item{turno}{Integer. 1 = first round, 2 = runoff.}
#'   \item{date}{Date. Election date.}
#' }
#'
#' @source TSE (Tribunal Superior Eleitoral)
#'
#' @keywords datasets
"br_election_dates"
