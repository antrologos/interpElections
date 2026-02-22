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
#' Additionally, all years produce gender × age columns used for
#' calibration: `pop_hom_*` and `pop_mul_*` (7 age brackets each,
#' 14 columns total).
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
      -dplyr::all_of(c("code_tract", "code_muni")),
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

  persons <- dplyr::select(persons,
    "code_tract", "code_muni",
    dplyr::starts_with("pop")
  )

  # --- Gender x literacy columns from "Instrucao" dataset ---
  instrucao <- tryCatch(
    censobr::read_tracts(2000, "Instrucao"),
    error = function(e) {
      warning(sprintf(
        "Could not download Census 2000 Instrucao data: %s. ",
        "Gender x literacy columns will be missing.",
        e$message
      ), call. = FALSE)
      NULL
    }
  )

  if (!is.null(instrucao)) {
    # Literate men:   instrucao3_V{2644+age} (ages 5-80+)
    # Illiterate men: instrucao3_V{2736+age} (ages 5-80+)
    # Literate women: instrucao5_V{2929+age} (ages 5-80+)
    # Illiterate women: instrucao5_V{3021+age} (ages 5-80+)
    instrucao <- instrucao |>
      dplyr::select(
        "code_tract", "code_muni",
        dplyr::starts_with("instrucao3_V26"),
        dplyr::starts_with("instrucao3_V27"),
        dplyr::starts_with("instrucao3_V28"),
        dplyr::starts_with("instrucao5_V29"),
        dplyr::starts_with("instrucao5_V30"),
        dplyr::starts_with("instrucao5_V31")
      ) |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(
        -dplyr::all_of(c("code_tract", "code_muni")),
        \(x) tidyr::replace_na(as.numeric(x), 0)
      ))

    instrucao <- dplyr::filter(
      instrucao,
      as.character(.data$code_muni) %in% code_muni
    )

    # Column name builders
    ha  <- function(ages) paste0("instrucao3_V", 2644L + ages)
    hna <- function(ages) paste0("instrucao3_V", 2736L + ages)
    fa  <- function(ages) paste0("instrucao5_V", 2929L + ages)
    fna <- function(ages) paste0("instrucao5_V", 3021L + ages)

    age_brackets <- list(
      "18_20" = 18:20, "21_24" = 21:24, "25_29" = 25:29,
      "30_39" = 30:39, "40_49" = 40:49, "50_59" = 50:59,
      "60_69" = 60:69
    )

    dt <- data.table::as.data.table(instrucao)
    for (nm in names(age_brackets)) {
      ages <- age_brackets[[nm]]
      dt[, paste0("pop_hom_alf_", nm) := rowSums(.SD),
         .SDcols = ha(ages)]
      dt[, paste0("pop_hom_nalf_", nm) := rowSums(.SD),
         .SDcols = hna(ages)]
      dt[, paste0("pop_mul_alf_", nm) := rowSums(.SD),
         .SDcols = fa(ages)]
      dt[, paste0("pop_mul_nalf_", nm) := rowSums(.SD),
         .SDcols = fna(ages)]
    }

    lit_cols <- dplyr::as_tibble(dt) |>
      dplyr::select("code_tract",
                     dplyr::starts_with("pop_hom_"),
                     dplyr::starts_with("pop_mul_"))

    persons <- dplyr::left_join(
      persons, lit_cols, by = "code_tract"
    )
  }

  as.data.frame(persons)
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

  # Select columns for:
  #   pessoa13_V*: total population by single age (V034=age0 .. V134=age100+)
  #   pessoa02_V*: literate men (V002=age5..V077=age80+) and
  #                literate women (V087=age5..V162=age80+)
  #   pessoa11_V*: total men by single age (V035=age1..V134=age100+)
  #   pessoa12_V*: total women by single age (V035=age1..V134=age100+)
  persons <- persons |>
    dplyr::select(
      "code_tract", "code_muni",
      dplyr::matches(paste0(
        "^pessoa13_V0[3][4-9]$|^pessoa13_V0[4-9][0-9]$|",
        "^pessoa13_V1[0-3][0-9]$|",
        "^pessoa02_V[01][0-9][0-9]$|^pessoa02_V2[0-4][0-9]$|",
        "^pessoa11_V0[5-9][0-9]$|^pessoa11_V1[0-3][0-9]$|",
        "^pessoa12_V0[5-9][0-9]$|^pessoa12_V1[0-3][0-9]$"
      ))
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dplyr::across(
      -dplyr::all_of(c("code_tract", "code_muni")),
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

  # ---- Total population age brackets (pessoa13: V{034+age}) ----
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

  # ---- Gender x literacy age brackets ----
  # pessoa02: literate men V{age-3} (V002=age5..V077=age80+),
  #           literate women V{age+82} (V087=age5..V162=age80+)
  # pessoa11: total men V{034+age}, pessoa12: total women V{034+age}
  # Illiterate = total - literate (clamped to 0)

  # Helper: build column names for an age range
  p02 <- function(ages) paste0("pessoa02_V", sprintf("%03d", ages - 3L))
  p11 <- function(ages) paste0("pessoa11_V", sprintf("%03d", 34L + ages))
  p02f <- function(ages) paste0("pessoa02_V", sprintf("%03d", ages + 82L))
  p12 <- function(ages) paste0("pessoa12_V", sprintf("%03d", 34L + ages))

  age_brackets <- list(
    "18_20" = 18:20, "21_24" = 21:24, "25_29" = 25:29,
    "30_39" = 30:39, "40_49" = 40:49, "50_59" = 50:59,
    "60_69" = 60:69
  )

  for (nm in names(age_brackets)) {
    ages <- age_brackets[[nm]]
    hom_alf_cols  <- p02(ages)
    hom_tot_cols  <- p11(ages)
    mul_alf_cols  <- p02f(ages)
    mul_tot_cols  <- p12(ages)

    hom_alf_var  <- paste0("pop_hom_alf_", nm)
    hom_nalf_var <- paste0("pop_hom_nalf_", nm)
    mul_alf_var  <- paste0("pop_mul_alf_", nm)
    mul_nalf_var <- paste0("pop_mul_nalf_", nm)

    # Literate counts
    dt[, (hom_alf_var) := rowSums(.SD),
       .SDcols = hom_alf_cols]
    dt[, (mul_alf_var) := rowSums(.SD),
       .SDcols = mul_alf_cols]

    # Total men/women for age bracket (temp columns)
    tmp_hom <- paste0(".hom_tot_", nm)
    tmp_mul <- paste0(".mul_tot_", nm)
    dt[, (tmp_hom) := rowSums(.SD), .SDcols = hom_tot_cols]
    dt[, (tmp_mul) := rowSums(.SD), .SDcols = mul_tot_cols]

    # Illiterate = total - literate (clamped to 0)
    dt[, (hom_nalf_var) := pmax(
      get(tmp_hom) - get(hom_alf_var), 0
    )]
    dt[, (mul_nalf_var) := pmax(
      get(tmp_mul) - get(mul_alf_var), 0
    )]

    # Clean up temp columns
    dt[, (tmp_hom) := NULL]
    dt[, (tmp_mul) := NULL]
  }

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

  # --- Gender x literacy columns from alfabetizacao_V* ---
  # Download with literacy columns from the same Pessoas dataset
  lit_data <- tryCatch(
    censobr::read_tracts(2022, "Pessoas"),
    error = function(e) {
      warning(sprintf(
        "Could not re-download Census 2022 Pessoas data: %s. ",
        "Gender x literacy columns will be missing.",
        e$message
      ), call. = FALSE)
      NULL
    }
  )

  if (!is.null(lit_data)) {
    lit_data <- lit_data |>
      dplyr::select(
        "code_tract", "code_muni",
        dplyr::starts_with("alfabetizacao_V007"),
        dplyr::starts_with("alfabetizacao_V008")
      ) |>
      dplyr::collect() |>
      dplyr::mutate(dplyr::across(
        -dplyr::all_of(c("code_tract", "code_muni")),
        \(x) tidyr::replace_na(as.numeric(x), 0)
      ))

    # Remove "alfabetizacao_" prefix
    nm <- names(lit_data)
    names(lit_data) <- sub("^alfabetizacao_", "", nm)

    lit_data <- dplyr::filter(
      lit_data,
      as.character(.data$code_muni) %in% code_muni
    )

    # Census 2022 age groups (already pre-aggregated):
    # Men total:   V00722(15-19)..V00734(80+)
    # Women total: V00735(15-19)..V00747(80+)
    # Lit men:     V00826(15-19)..V00838(80+)
    # Lit women:   V00839(15-19)..V00851(80+)
    #
    # Age group mapping (V offset from base):
    # 15-19=+0, 20-24=+1, 25-29=+2, 30-34=+3, 35-39=+4,
    # 40-44=+5, 45-49=+6, 50-54=+7, 55-59=+8,
    # 60-64=+9, 65-69=+10, 70-79=+11, 80+=+12

    # 15-19 bracket: use 2/5 proxy for 18-19
    lit_data <- dplyr::mutate(lit_data,
      pop_hom_alf_18_19  = .data$V00826 * 2 / 5,
      pop_hom_nalf_18_19 = pmax(
        (.data$V00722 - .data$V00826) * 2 / 5, 0),
      pop_mul_alf_18_19  = .data$V00839 * 2 / 5,
      pop_mul_nalf_18_19 = pmax(
        (.data$V00735 - .data$V00839) * 2 / 5, 0),
      # 20-24
      pop_hom_alf_20_24  = .data$V00827,
      pop_hom_nalf_20_24 = pmax(
        .data$V00723 - .data$V00827, 0),
      pop_mul_alf_20_24  = .data$V00840,
      pop_mul_nalf_20_24 = pmax(
        .data$V00736 - .data$V00840, 0),
      # 25-29
      pop_hom_alf_25_29  = .data$V00828,
      pop_hom_nalf_25_29 = pmax(
        .data$V00724 - .data$V00828, 0),
      pop_mul_alf_25_29  = .data$V00841,
      pop_mul_nalf_25_29 = pmax(
        .data$V00737 - .data$V00841, 0),
      # 30-39 = 30-34 + 35-39
      pop_hom_alf_30_39  = .data$V00829 + .data$V00830,
      pop_hom_nalf_30_39 = pmax(
        (.data$V00725 + .data$V00726) -
          (.data$V00829 + .data$V00830), 0),
      pop_mul_alf_30_39  = .data$V00842 + .data$V00843,
      pop_mul_nalf_30_39 = pmax(
        (.data$V00738 + .data$V00739) -
          (.data$V00842 + .data$V00843), 0),
      # 40-49 = 40-44 + 45-49
      pop_hom_alf_40_49  = .data$V00831 + .data$V00832,
      pop_hom_nalf_40_49 = pmax(
        (.data$V00727 + .data$V00728) -
          (.data$V00831 + .data$V00832), 0),
      pop_mul_alf_40_49  = .data$V00844 + .data$V00845,
      pop_mul_nalf_40_49 = pmax(
        (.data$V00740 + .data$V00741) -
          (.data$V00844 + .data$V00845), 0),
      # 50-59 = 50-54 + 55-59
      pop_hom_alf_50_59  = .data$V00833 + .data$V00834,
      pop_hom_nalf_50_59 = pmax(
        (.data$V00729 + .data$V00730) -
          (.data$V00833 + .data$V00834), 0),
      pop_mul_alf_50_59  = .data$V00846 + .data$V00847,
      pop_mul_nalf_50_59 = pmax(
        (.data$V00742 + .data$V00743) -
          (.data$V00846 + .data$V00847), 0),
      # 60-69 = 60-64 + 65-69
      pop_hom_alf_60_69  = .data$V00835 + .data$V00836,
      pop_hom_nalf_60_69 = pmax(
        (.data$V00731 + .data$V00732) -
          (.data$V00835 + .data$V00836), 0),
      pop_mul_alf_60_69  = .data$V00848 + .data$V00849,
      pop_mul_nalf_60_69 = pmax(
        (.data$V00744 + .data$V00745) -
          (.data$V00848 + .data$V00849), 0)
    )

    lit_cols <- dplyr::select(lit_data, "code_tract",
      dplyr::starts_with("pop_hom_"),
      dplyr::starts_with("pop_mul_"))
    persons <- dplyr::left_join(persons, lit_cols,
                                by = "code_tract")
  }

  dplyr::select(persons,
    "code_tract", "code_muni",
    dplyr::starts_with("pop")
  ) |>
    as.data.frame()
}
