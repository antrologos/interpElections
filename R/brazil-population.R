#' Prepare Brazilian census population data by age bracket per census tract
#'
#' Downloads census population data from IBGE (via the `censobr` package),
#' groups ages into brackets, and returns a data frame with one row per
#' census tract. Supports census years 2000, 2010, and 2022.
#'
#' @param code_muni Numeric or character vector. IBGE municipality codes.
#' @param year Integer. Census year: 2000, 2010, or 2022. Default: 2010.
#'
#' @return A data frame with columns: `code_muni`, `code_tract`, and
#'   population bracket columns (`pop_*`). The exact brackets depend on
#'   the census year (see Details).
#'
#' @details
#' Requires the `censobr`, `dplyr`, `tidyr`, and `data.table` packages.
#'
#' **Census 2000 and 2010** produce the following voting-age brackets:
#' `pop_18_20`, `pop_21_24`, `pop_25_29`, `pop_30_39`, `pop_40_49`,
#' `pop_50_59`, `pop_60_69`.
#'
#' **Census 2022** produces: `pop_15_19`, `pop_20_24`, `pop_25_29`,
#' `pop_30_39`, `pop_40_49`, `pop_50_59`, `pop_60_69`.
#'
#' All years also produce: `pop_00_04`, `pop_05_09`, `pop_10_14`,
#' `pop_15_17` (or `pop_15_19` for 2022), `pop_70mais`.
#'
#' @examples
#' \dontrun{
#' # Census 2010 population for Sao Paulo
#' pop <- br_prepare_population(code_muni = "3550308", year = 2010)
#' head(pop)
#' }
#'
#' @family Brazil helpers
#' @export
br_prepare_population <- function(code_muni, year = 2010) {
  for (pkg in c("censobr", "dplyr", "tidyr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "The '%s' package is required for br_prepare_population()", pkg
      ), call. = FALSE)
    }
  }

  if (!year %in% c(2000, 2010, 2022)) {
    stop("Census year must be 2000, 2010, or 2022", call. = FALSE)
  }

  code_muni <- as.character(code_muni)

  if (year == 2000) {
    .br_pop_2000(code_muni)
  } else if (year == 2010) {
    .br_pop_2010(code_muni)
  } else {
    .br_pop_2022(code_muni)
  }
}


# --- Census 2000 ---
.br_pop_2000 <- function(code_muni) {
  persons <- tryCatch(
    censobr::read_tracts(2000, "Pessoa"),
    error = function(e) {
      stop(sprintf(
        "Failed to download Census 2000 data from censobr: %s",
        e$message
      ), call. = FALSE)
    }
  )

  persons <- persons |>
    dplyr::select(
      "code_tract", "code_muni",
      dplyr::starts_with("pessoa1_V136"),
      dplyr::starts_with("pessoa1_V144"),
      dplyr::starts_with("pessoa1_V145"),
      dplyr::starts_with("pessoa1_V146")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) tidyr::replace_na(as.numeric(x), 0)
    ))

  # Filter to requested municipalities
  persons <- dplyr::filter(
    persons,
    as.character(.data$code_muni) %in% code_muni
  )

  if (nrow(persons) == 0) {
    warning(sprintf(
      "No census tracts found for municipality codes: %s",
      paste(code_muni, collapse = ", ")
    ), call. = FALSE)
  }

  # Build age brackets
  # V1362-V1364: individual ages 15, 16, 17
  # V1365-V1367: individual ages 18, 19, 20
  # V1448-V1464: pre-aggregated 5-year groups
  # V1451 = 15-19 group, V1452 = 20-24 group
  persons <- dplyr::mutate(persons,
    code_tract = as.character(.data$code_tract),
    pop_00_04  = .data$pessoa1_V1448,
    pop_05_09  = .data$pessoa1_V1449,
    pop_10_14  = .data$pessoa1_V1450,
    pop_15_17  = .data$pessoa1_V1362 + .data$pessoa1_V1363 +
                 .data$pessoa1_V1364,
    pop_18_20  = .data$pessoa1_V1365 + .data$pessoa1_V1366 +
                 .data$pessoa1_V1367,
    # pop_21_24 = (20-24 group) minus age 20
    pop_21_24  = pmax(.data$pessoa1_V1452 - .data$pessoa1_V1367, 0),
    pop_25_29  = .data$pessoa1_V1453,
    pop_30_39  = .data$pessoa1_V1454 + .data$pessoa1_V1455,
    pop_40_49  = .data$pessoa1_V1456 + .data$pessoa1_V1457,
    pop_50_59  = .data$pessoa1_V1458 + .data$pessoa1_V1459,
    pop_60_69  = .data$pessoa1_V1460 + .data$pessoa1_V1461,
    pop_70mais = .data$pessoa1_V1462 + .data$pessoa1_V1463 +
                 .data$pessoa1_V1464
  )

  dplyr::select(persons,
    "code_tract", "code_muni",
    dplyr::starts_with("pop")
  ) |>
    as.data.frame()
}


# --- Census 2010 ---
.br_pop_2010 <- function(code_muni) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required for census 2010",
         call. = FALSE)
  }

  persons <- tryCatch(
    censobr::read_tracts(2010, "Pessoa"),
    error = function(e) {
      stop(sprintf(
        "Failed to download Census 2010 data from censobr: %s",
        e$message
      ), call. = FALSE)
    }
  )

  # Select V034-V134 columns (single-age counts)
  # V034=age0, V035=age1, ..., V134=age100+
  # Note: V030-V033 are NOT age columns; ages start at V034
  persons <- persons |>
    dplyr::select(
      "code_tract", "code_muni",
      dplyr::matches("^pessoa13_V0[3][4-9]$|^pessoa13_V0[4-9][0-9]$|^pessoa13_V1[0-3][0-9]$")
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) tidyr::replace_na(as.numeric(x), 0)
    ))

  # Filter to requested municipalities
  persons <- dplyr::filter(
    persons,
    as.character(.data$code_muni) %in% code_muni
  )

  if (nrow(persons) == 0) {
    warning(sprintf(
      "No census tracts found for municipality codes: %s",
      paste(code_muni, collapse = ", ")
    ), call. = FALSE)
  }

  # Use data.table for fast row-wise aggregation
  dt <- data.table::as.data.table(persons)

  # Offset: V{034 + age} = that age
  # V034=age0, V035=age1, ..., V038=age4  => pop_00_04
  # V039=age5, ..., V043=age9             => pop_05_09
  # V044=age10, ..., V048=age14           => pop_10_14
  # V049=age15, V050=age16, V051=age17    => pop_15_17
  # V052=age18, V053=age19, V054=age20    => pop_18_20
  # V055=age21, ..., V058=age24           => pop_21_24
  # V059=age25, ..., V063=age29           => pop_25_29
  # V064=age30, ..., V073=age39           => pop_30_39
  # V074=age40, ..., V083=age49           => pop_40_49
  # V084=age50, ..., V093=age59           => pop_50_59
  # V094=age60, ..., V103=age69           => pop_60_69
  # V104=age70, ..., V134=age100+         => pop_70mais
  dt[, pop_00_04 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 34:38)]
  dt[, pop_05_09 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 39:43)]
  dt[, pop_10_14 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 44:48)]
  dt[, pop_15_17 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 49:51)]
  dt[, pop_18_20 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 52:54)]
  dt[, pop_21_24 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 55:58)]
  dt[, pop_25_29 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 59:63)]
  dt[, pop_30_39 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 64:73)]
  dt[, pop_40_49 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 74:83)]
  dt[, pop_50_59 := rowSums(.SD),
     .SDcols = paste0("pessoa13_V0", 84:93)]
  dt[, pop_60_69 := rowSums(.SD),
     .SDcols = c(paste0("pessoa13_V0", 94:99),
                 paste0("pessoa13_V", 100:103))]
  dt[, pop_70mais := rowSums(.SD),
     .SDcols = paste0("pessoa13_V", 104:134)]

  dplyr::as_tibble(dt) |>
    dplyr::select(
      "code_tract", "code_muni",
      dplyr::starts_with("pop")
    ) |>
    as.data.frame()
}


# --- Census 2022 ---
.br_pop_2022 <- function(code_muni) {
  persons <- tryCatch(
    censobr::read_tracts(2022, "Pessoas"),
    error = function(e) {
      stop(sprintf(
        "Failed to download Census 2022 data from censobr: %s",
        e$message
      ), call. = FALSE)
    }
  )

  persons <- persons |>
    dplyr::select(
      "code_tract", "code_muni",
      dplyr::starts_with("demografia")
    ) |>
    dplyr::collect()

  # Remove "demografia_" prefix from column names
  nm <- names(persons)
  names(persons) <- sub("^demografia_", "", nm)

  persons <- dplyr::mutate(persons,
    dplyr::across(
      -dplyr::all_of(c("code_tract", "code_muni")),
      \(x) tidyr::replace_na(as.numeric(x), 0)
    )
  )

  # Filter to requested municipalities
  persons <- dplyr::filter(
    persons,
    as.character(.data$code_muni) %in% code_muni
  )

  if (nrow(persons) == 0) {
    warning(sprintf(
      "No census tracts found for municipality codes: %s",
      paste(code_muni, collapse = ", ")
    ), call. = FALSE)
  }

  # V01031-V01041: pre-aggregated age groups
  persons <- dplyr::mutate(persons,
    pop_00_04  = .data$V01031,
    pop_05_09  = .data$V01032,
    pop_10_14  = .data$V01033,
    pop_15_19  = .data$V01034,
    pop_20_24  = .data$V01035,
    pop_25_29  = .data$V01036,
    pop_30_39  = .data$V01037,
    pop_40_49  = .data$V01038,
    pop_50_59  = .data$V01039,
    pop_60_69  = .data$V01040,
    pop_70mais = .data$V01041
  )

  dplyr::select(persons,
    "code_tract", "code_muni",
    dplyr::starts_with("pop")
  ) |>
    as.data.frame()
}
