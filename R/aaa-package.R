#' @description
#' Spatial interpolation of electoral data via inverse distance weighting
#' with per-zone optimized decay parameters. Designed for disaggregating
#' voting results from polling locations into census tracts using
#' travel-time-based IDW, with optional GPU acceleration via torch.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats optim
NULL

# Avoid R CMD check NOTE about .data pronoun used in dplyr pipelines
utils::globalVariables(".data")

# Avoid R CMD check NOTE about data.table NSE symbols and bundled data
utils::globalVariables(c(
  ":=", ".SD",
  "muni_crosswalk",
  "pop_00_04", "pop_05_09", "pop_10_14", "pop_15_17",
  "pop_18_20", "pop_21_24", "pop_25_29", "pop_30_39",
  "pop_40_49", "pop_50_59", "pop_60_69", "pop_70mais",
  "CD_MUNICIPIO", "CD_CARGO", "NR_TURNO", "SG_UF", "QT_VOTOS",
  "QT_COMPARECIMENTO", "QT_APTOS", "QT_ABSTENCOES",
  "NR_LATITUDE", "NR_LONGITUDE",
  "DS_GENERO", "DS_GRAU_ESCOLARIDADE",
  "SG_PARTIDO", "NR_PARTIDO", "NR_VOTAVEL",
  "genero_col", "educ_col", "party_key", "party_votes"
))

# Allow data.table's := operator to work inside this package
.datatable.aware <- TRUE

.onLoad <- function(libname, pkgname) {
  op <- options()
  op_interpElections <- list(
    interpElections.use_gpu = FALSE,
    interpElections.device = NULL,
    interpElections.dtype = "float32"
  )
  toset <- !(names(op_interpElections) %in% names(op))
  if (any(toset)) options(op_interpElections[toset])
  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Check suggested packages needed for the main workflow
  core_pkgs <- c("sf", "dplyr", "censobr", "geobr",
                 "data.table", "stringr", "osmextract", "r5r")
  missing <- core_pkgs[!vapply(core_pkgs, requireNamespace,
                               logical(1), quietly = TRUE)]

  # Check for OSM clipping tools
  has_clip <- !is.null(.find_tool_safe("osmium")) ||
    !is.null(.find_tool_safe("osmconvert")) ||
    !is.null(.find_tool_safe("osmconvert64")) ||
    file.exists(file.path(
      get_interpElections_cache_dir(), "bin",
      if (.Platform$OS.type == "windows") "osmconvert.exe"
      else "osmconvert"
    ))

  msgs <- character(0)

  if (length(missing) > 0) {
    msgs <- c(msgs, paste0(
      "Missing suggested packages: ",
      paste(missing, collapse = ", "),
      "\n  Install with: install.packages(c(",
      paste0('"', missing, '"', collapse = ", "), "))"
    ))
  }

  if (!has_clip) {
    msgs <- c(msgs, paste0(
      "No OSM clipping tool found (osmium/osmconvert)\n",
      "  Install with: interpElections::setup_osmium()"
    ))
  }

  if (length(msgs) > 0) {
    packageStartupMessage(
      "interpElections: some optional dependencies are missing:\n",
      paste0("- ", msgs, collapse = "\n"),
      "\nThese are needed for the full interpolation pipeline."
    )
  }
}

# Safe version of .find_tool for use in .onAttach
# (before the full package namespace is sealed)
.find_tool_safe <- function(tool_name) {
  path <- Sys.which(tool_name)
  if (nzchar(path)) path else NULL
}
