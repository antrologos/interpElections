#' Download candidate vote data from TSE
#'
#' Downloads the official candidate vote data (votação por seção eleitoral)
#' from the TSE open data portal for a given election year and state.
#' The data is downloaded as a ZIP file containing a semicolon-delimited CSV.
#'
#' @param year Integer. Election year (e.g., 2008, 2012, 2016, 2020).
#' @param uf Character. Two-letter state abbreviation (e.g., `"SP"`, `"RJ"`),
#'   or `"BR"` to download the national file (used for presidential data).
#' @param code_muni_tse Character or NULL. 5-digit TSE municipality code.
#'   If provided, filters results to this municipality only.
#' @param cargo Integer or NULL. Electoral office code to filter
#'   (e.g., 13 = Vereador, 11 = Prefeito). If NULL, returns all offices.
#' @param turno Integer. Election round (1 or 2). Default: 1.
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param cache Logical. If TRUE (default), downloaded files are stored
#'   in a persistent cross-session cache directory (see
#'   [get_interpElections_cache_dir()]). If FALSE, files are stored in a
#'   temporary directory and lost when R restarts.
#' @param verbose Logical. Default: TRUE.
#'
#' @return A data frame with one row per candidate per polling section,
#'   using the original TSE column names. Key columns:
#'   \describe{
#'     \item{ANO_ELEICAO}{Election year}
#'     \item{CD_MUNICIPIO}{TSE municipality code (5-digit)}
#'     \item{NR_ZONA}{Electoral zone number}
#'     \item{NR_SECAO}{Section number}
#'     \item{NR_LOCAL_VOTACAO}{Polling location number}
#'     \item{CD_CARGO}{Office code}
#'     \item{NR_VOTAVEL}{Candidate number (or 95 = blank, 96 = null)}
#'     \item{NM_VOTAVEL}{Candidate name}
#'     \item{QT_VOTOS}{Number of votes}
#'   }
#'
#' @details
#' Data is downloaded from
#' `https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/`.
#' Files are cached persistently by default and reused on subsequent
#' calls unless `force = TRUE`. Use [interpElections_cache()] to manage
#' cached files.
#'
#' The CSV files use semicolon (`;`) as delimiter and Latin-1 encoding.
#'
#' **Note:** For general elections, presidential vote data (cargo 1) is
#' published in a national file (`uf = "BR"`) rather than in per-state
#' files. [br_prepare_electoral()] handles this automatically.
#'
#' @seealso [br_download_turnout()] for attendance/turnout data,
#'   [br_prepare_electoral()] which uses this function internally,
#'   [interpElections_cache()] to manage cached downloads.
#'
#' @family Brazil downloads
#' @export
br_download_votes <- function(
    year,
    uf,
    code_muni_tse = NULL,
    cargo = NULL,
    turno = 1L,
    force = FALSE,
    cache = TRUE,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for br_download_votes()",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required for br_download_votes()",
         call. = FALSE)
  }

  uf <- toupper(uf)
  year <- as.integer(year)

  url <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_secao/votacao_secao_%d_%s.zip",
    year, uf
  )

  zip_name <- sprintf("votacao_secao_%d_%s.zip", year, uf)

  if (verbose) message(sprintf("  Candidate votes (%s %d)...", uf, year))
  zip_path <- .interpElections_download(
    url = url, filename = zip_name, subdir = .cache_subdirs()$votes,
    cache = cache, force = force, verbose = verbose
  )

  # Extract CSV from ZIP (to temp dir — CSV is transient)
  csv_files <- utils::unzip(zip_path, list = TRUE)$Name
  csv_file <- grep("\\.csv$", csv_files, value = TRUE, ignore.case = TRUE)
  if (length(csv_file) == 0) {
    stop("No CSV file found in ZIP archive: ", zip_path, call. = FALSE)
  }

  extract_dir <- tempfile("interpElections_votes_")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  csv_path <- file.path(extract_dir, csv_file[1])
  utils::unzip(zip_path, files = csv_file[1], exdir = extract_dir,
               overwrite = TRUE)

  # Read CSV
  if (verbose) message("  Reading vote data...")
  dados <- data.table::fread(
    csv_path, sep = ";", encoding = "Latin-1",
    colClasses = "character",
    showProgress = FALSE
  )

  # Ensure key columns are present
  required <- c("CD_MUNICIPIO", "NR_TURNO", "CD_CARGO",
                 "NR_VOTAVEL", "QT_VOTOS")
  missing <- setdiff(required, names(dados))
  if (length(missing) > 0) {
    stop("Expected TSE columns not found: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Convert numeric columns
  for (col in c("NR_TURNO", "CD_CARGO", "QT_VOTOS")) {
    if (col %in% names(dados)) {
      data.table::set(dados, j = col,
                      value = as.integer(dados[[col]]))
    }
  }

  # Pad municipality code
  data.table::set(
    dados, j = "CD_MUNICIPIO",
    value = stringr::str_pad(dados$CD_MUNICIPIO, 5, "left", "0")
  )

  # Apply filters
  if (!is.null(code_muni_tse)) {
    code_muni_tse <- stringr::str_pad(
      as.character(code_muni_tse), 5, "left", "0"
    )
    dados <- dados[dados$CD_MUNICIPIO == code_muni_tse, ]
  }
  if (!is.null(cargo)) {
    dados <- dados[dados$CD_CARGO == as.integer(cargo), ]
  }
  if (!is.null(turno)) {
    dados <- dados[dados$NR_TURNO == as.integer(turno), ]
  }

  if (verbose) {
    message(sprintf("  %d vote records loaded", nrow(dados)))
  }

  as.data.frame(dados)
}


#' Download turnout/attendance data from TSE
#'
#' Downloads the official turnout detail data (detalhe da votação por
#' seção) from the TSE open data portal. This file provides attendance
#' counts, abstentions, and vote type breakdowns per polling section.
#'
#' @param year Integer. Election year (e.g., 2008, 2012, 2016, 2020).
#' @param uf Character or NULL. Two-letter state abbreviation. Used to
#'   filter the nationwide file to a single state.
#' @param code_muni_tse Character or NULL. 5-digit TSE municipality code.
#'   If provided, filters results to this municipality only.
#' @param cargo Integer or NULL. Electoral office code to filter.
#'   If NULL, uses any office (turnout is the same across offices).
#' @param turno Integer. Election round (1 or 2). Default: 1.
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param cache Logical. If TRUE (default), downloaded files are stored
#'   persistently. See [get_interpElections_cache_dir()].
#' @param verbose Logical. Default: TRUE.
#'
#' @return A data frame with one row per section per office, using the
#'   original TSE column names. Key columns:
#'   \describe{
#'     \item{ANO_ELEICAO}{Election year}
#'     \item{CD_MUNICIPIO}{TSE municipality code (5-digit)}
#'     \item{NR_ZONA}{Electoral zone number}
#'     \item{NR_SECAO}{Section number}
#'     \item{CD_CARGO}{Office code}
#'     \item{QT_COMPARECIMENTO}{Number of voters who attended}
#'     \item{QT_APTOS}{Number of eligible voters}
#'     \item{QT_ABSTENCOES}{Number of abstentions}
#'   }
#'
#' @details
#' The TSE publishes this data as a single nationwide file at
#' `https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/`.
#' The file can be large (hundreds of MB). Results are filtered by
#' municipality and/or state immediately after reading.
#'
#' @family Brazil downloads
#'
#' @seealso [br_download_votes()] for candidate vote data,
#'   [br_prepare_electoral()] which can use this function internally,
#'   [interpElections_cache()] to manage cached downloads.
#'
#' @export
br_download_turnout <- function(
    year,
    uf = NULL,
    code_muni_tse = NULL,
    cargo = NULL,
    turno = 1L,
    force = FALSE,
    cache = TRUE,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for br_download_turnout()",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required for br_download_turnout()",
         call. = FALSE)
  }

  year <- as.integer(year)
  if (!is.null(uf)) uf <- toupper(uf)

  url <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_%d.zip",
    year
  )

  zip_name <- sprintf("detalhe_votacao_secao_%d.zip", year)

  if (verbose) message(sprintf("  Turnout data (%d)...", year))
  zip_path <- .interpElections_download(
    url = url, filename = zip_name, subdir = .cache_subdirs()$turnout,
    cache = cache, force = force, verbose = verbose
  )

  # Extract CSV from ZIP (to temp dir — CSV is transient)
  csv_files <- utils::unzip(zip_path, list = TRUE)$Name
  csv_file <- grep("\\.csv$", csv_files, value = TRUE, ignore.case = TRUE)
  if (length(csv_file) == 0) {
    stop("No CSV file found in ZIP archive: ", zip_path, call. = FALSE)
  }

  extract_dir <- tempfile("interpElections_turnout_")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  csv_path <- file.path(extract_dir, csv_file[1])
  utils::unzip(zip_path, files = csv_file[1], exdir = extract_dir,
               overwrite = TRUE)

  # Read CSV
  if (verbose) message("  Reading turnout data...")
  dados <- data.table::fread(
    csv_path, sep = ";", encoding = "Latin-1",
    colClasses = "character",
    showProgress = FALSE
  )

  # Ensure key columns
  required <- c("CD_MUNICIPIO", "NR_TURNO", "QT_COMPARECIMENTO")
  missing <- setdiff(required, names(dados))
  if (length(missing) > 0) {
    stop("Expected TSE columns not found: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Convert numeric columns
  for (col in c("NR_TURNO", "QT_COMPARECIMENTO", "QT_APTOS",
                 "QT_ABSTENCOES", "CD_CARGO")) {
    if (col %in% names(dados)) {
      data.table::set(dados, j = col,
                      value = as.integer(dados[[col]]))
    }
  }

  # Pad municipality code
  data.table::set(
    dados, j = "CD_MUNICIPIO",
    value = stringr::str_pad(dados$CD_MUNICIPIO, 5, "left", "0")
  )

  # Apply filters
  if (!is.null(uf)) {
    if ("SG_UF" %in% names(dados)) {
      dados <- dados[dados$SG_UF == uf, ]
    }
  }
  if (!is.null(code_muni_tse)) {
    code_muni_tse <- stringr::str_pad(
      as.character(code_muni_tse), 5, "left", "0"
    )
    dados <- dados[dados$CD_MUNICIPIO == code_muni_tse, ]
  }
  if (!is.null(cargo)) {
    dados <- dados[dados$CD_CARGO == as.integer(cargo), ]
  }
  if (!is.null(turno)) {
    dados <- dados[dados$NR_TURNO == as.integer(turno), ]
  }

  if (verbose) {
    message(sprintf("  %d turnout records loaded", nrow(dados)))
  }

  as.data.frame(dados)
}


#' Download party legends from TSE
#'
#' Downloads the official party legend data (consulta de legendas) from the
#' TSE open data portal for a given election year. This file maps party
#' numbers (`NR_PARTIDO`) to abbreviations (`SG_PARTIDO`) and full names
#' (`NM_PARTIDO`).
#'
#' @param year Integer. Election year (e.g., 2008, 2012, 2016, 2020, 2022).
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param cache Logical. If TRUE (default), downloaded files are stored
#'   persistently. See [get_interpElections_cache_dir()].
#' @param verbose Logical. Default: TRUE.
#'
#' @return A data frame with TSE party/coalition data. Key columns:
#'   \describe{
#'     \item{NR_PARTIDO}{Party number (2-digit)}
#'     \item{SG_PARTIDO}{Party abbreviation (e.g., "PT", "MDB")}
#'     \item{NM_PARTIDO}{Full party name}
#'   }
#'
#' @details
#' Data is downloaded from
#' `https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_coligacao/`.
#' This file contains coalition/party records for each election year,
#' from which the unique party number-to-abbreviation mapping is extracted.
#' Files are cached persistently by default and reused on subsequent
#' calls unless `force = TRUE`.
#'
#' @family Brazil downloads
#'
#' @seealso [br_download_votes()] for candidate vote data,
#'   [br_prepare_electoral()] which uses this function internally,
#'   [interpElections_cache()] to manage cached downloads.
#'
#' @export
br_download_party_legends <- function(
    year,
    force = FALSE,
    cache = TRUE,
    verbose = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for br_download_party_legends()",
         call. = FALSE)
  }

  year <- as.integer(year)

  url <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_coligacao/consulta_coligacao_%d.zip",
    year
  )

  zip_name <- sprintf("consulta_coligacao_%d.zip", year)

  if (verbose) message(sprintf("  Party legends (%d)...", year))
  zip_path <- .interpElections_download(
    url = url, filename = zip_name, subdir = .cache_subdirs()$legends,
    cache = cache, force = force, verbose = verbose
  )

  # Extract CSV from ZIP — prefer BRASIL file (contains all states)
  csv_files <- utils::unzip(zip_path, list = TRUE)$Name
  csv_all <- grep("\\.csv$", csv_files, value = TRUE, ignore.case = TRUE)
  if (length(csv_all) == 0) {
    stop("No CSV file found in ZIP archive: ", zip_path, call. = FALSE)
  }
  csv_brasil <- grep("_BRASIL\\.csv$", csv_all, value = TRUE, ignore.case = TRUE)
  csv_file <- if (length(csv_brasil) > 0) csv_brasil[1] else csv_all[1]

  extract_dir <- tempfile("interpElections_legends_")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  utils::unzip(zip_path, files = csv_file, exdir = extract_dir,
               overwrite = TRUE)
  csv_path <- file.path(extract_dir, csv_file)

  if (verbose) message("  Reading party legend data...")
  dados <- data.table::fread(
    csv_path, sep = ";", encoding = "Latin-1",
    colClasses = "character",
    showProgress = FALSE
  )

  required <- c("NR_PARTIDO", "SG_PARTIDO")
  missing <- setdiff(required, names(dados))
  if (length(missing) > 0) {
    stop("Expected TSE columns not found: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  if (verbose) {
    n_parties <- length(unique(dados$SG_PARTIDO))
    message(sprintf("  %d party records loaded (%d unique parties)",
                    nrow(dados), n_parties))
  }

  as.data.frame(dados)
}


#' Download geocoded polling station data from TSE
#'
#' Downloads the official polling station location data (eleitorado por
#' local de votação) from the TSE open data portal. This file includes
#' geographic coordinates (`NR_LATITUDE`, `NR_LONGITUDE`) for polling
#' locations, along with voter counts and addresses.
#'
#' TSE data is available from 2010 onward. For years before 2010 (e.g.,
#' 2008), this function returns `NULL` so the caller can fall back to
#' alternative sources.
#'
#' @param year Integer. Election year (e.g., 2010, 2012, 2016, 2020).
#' @param uf Character or NULL. Two-letter state abbreviation. If
#'   provided, filters results to this state.
#' @param code_muni_tse Character or NULL. 5-digit TSE municipality code.
#'   If provided, filters results to this municipality only.
#' @param force Logical. Re-download even if cached file exists.
#'   Default: FALSE.
#' @param cache Logical. If TRUE (default), downloaded files are stored
#'   persistently. See [get_interpElections_cache_dir()].
#' @param verbose Logical. Default: TRUE.
#'
#' @return A data frame with one row per section per round, using the
#'   original TSE column names. Returns `NULL` if no data is available
#'   for the given year (e.g., before 2010). Key columns:
#'   \describe{
#'     \item{CD_MUNICIPIO}{TSE municipality code (5-digit)}
#'     \item{NR_ZONA}{Electoral zone number}
#'     \item{NR_LOCAL_VOTACAO}{Polling location number}
#'     \item{NR_SECAO}{Section number}
#'     \item{NR_LATITUDE}{Latitude of polling location (-1 if missing)}
#'     \item{NR_LONGITUDE}{Longitude of polling location (-1 if missing)}
#'     \item{NM_LOCAL_VOTACAO}{Name of polling location}
#'     \item{DS_ENDERECO}{Address of polling location}
#'   }
#'
#' @details
#' Data is downloaded from
#' `https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/`.
#' Files are cached persistently by default and reused on subsequent
#' calls unless `force = TRUE`. Use [interpElections_cache()] to manage
#' cached files.
#'
#' The CSV files use semicolon (`;`) as delimiter and Latin-1 encoding.
#' Missing coordinates are stored as `-1` in the TSE data.
#'
#' @family Brazil downloads
#'
#' @seealso [br_prepare_electoral()] which uses this function internally
#'   to geocode polling stations (with Hidalgo fallback for missing
#'   coordinates), [interpElections_cache()] to manage cached downloads.
#'
#' @export
br_download_geocode <- function(
    year,
    uf              = NULL,
    code_muni_tse   = NULL,
    force           = FALSE,
    cache           = TRUE,
    verbose         = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for br_download_geocode()",
         call. = FALSE)
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The 'stringr' package is required for br_download_geocode()",
         call. = FALSE)
  }

  year <- as.integer(year)
  if (!is.null(uf)) uf <- toupper(uf)

  url <- sprintf(
    "https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_%d.zip",
    year
  )

  zip_name <- sprintf("eleitorado_local_votacao_%d.zip", year)

  if (verbose) message(sprintf("  Polling station locations (%d)...", year))
  zip_path <- tryCatch(
    .interpElections_download(
      url = url, filename = zip_name, subdir = .cache_subdirs()$geocode,
      cache = cache, force = force, verbose = verbose
    ),
    error = function(e) {
      if (verbose) {
        message(sprintf(
          "  TSE geocoded data not available for %d (no file at TSE CDN)",
          year
        ))
      }
      NULL
    }
  )
  if (is.null(zip_path)) return(NULL)

  # Verify the ZIP is valid
  csv_files <- tryCatch(
    utils::unzip(zip_path, list = TRUE)$Name,
    error = function(e) character(0)
  )
  csv_file <- grep("\\.csv$", csv_files, value = TRUE, ignore.case = TRUE)
  if (length(csv_file) == 0) {
    # Invalid or empty ZIP — likely a 404 page was saved
    unlink(zip_path)
    if (verbose) {
      message(sprintf("  TSE geocoded data not available for %d", year))
    }
    return(NULL)
  }

  extract_dir <- tempfile("interpElections_geocode_")
  dir.create(extract_dir, recursive = TRUE)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  csv_path <- file.path(extract_dir, csv_file[1])
  utils::unzip(zip_path, files = csv_file[1], exdir = extract_dir,
               overwrite = TRUE)

  # Read CSV
  if (verbose) message("  Reading polling station location data...")
  dados <- data.table::fread(
    csv_path, sep = ";", encoding = "Latin-1",
    colClasses = "character",
    showProgress = FALSE
  )

  # Check for coordinate columns
  if (!all(c("NR_LATITUDE", "NR_LONGITUDE") %in% names(dados))) {
    if (verbose) {
      message("  TSE file does not contain coordinate columns")
    }
    return(NULL)
  }

  # Pad municipality code
  if ("CD_MUNICIPIO" %in% names(dados)) {
    data.table::set(
      dados, j = "CD_MUNICIPIO",
      value = stringr::str_pad(dados$CD_MUNICIPIO, 5, "left", "0")
    )
  }

  # Convert coordinate columns to numeric
  data.table::set(dados, j = "NR_LATITUDE",
                  value = as.numeric(dados$NR_LATITUDE))
  data.table::set(dados, j = "NR_LONGITUDE",
                  value = as.numeric(dados$NR_LONGITUDE))

  # Convert turno to integer
  if ("NR_TURNO" %in% names(dados)) {
    data.table::set(dados, j = "NR_TURNO",
                    value = as.integer(dados$NR_TURNO))
  }

  # Apply filters
  if (!is.null(uf) && "SG_UF" %in% names(dados)) {
    dados <- dados[dados$SG_UF == uf, ]
  }
  if (!is.null(code_muni_tse) && "CD_MUNICIPIO" %in% names(dados)) {
    code_muni_tse <- stringr::str_pad(
      as.character(code_muni_tse), 5, "left", "0"
    )
    dados <- dados[dados$CD_MUNICIPIO == code_muni_tse, ]
  }

  if (verbose) {
    n_valid <- sum(dados$NR_LATITUDE != -1, na.rm = TRUE)
    message(sprintf(
      "  %d rows loaded (%d with valid coordinates, %d missing)",
      nrow(dados), n_valid, nrow(dados) - n_valid
    ))
  }

  as.data.frame(dados)
}
