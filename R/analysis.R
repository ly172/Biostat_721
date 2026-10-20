run_analysis <- function(project_dir = ".", write_outputs = TRUE) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  data_dir <- file.path(project_dir, "data")
  output_dir <- file.path(project_dir, "outputs")

  # malformed are used as NA
  read_source <- function(filename, expected_names) {
    path <- file.path(data_dir, filename)
    if (!file.exists(path)) stop("No file in ", path)
    x <- read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, stringsAsFactors = FALSE,
                  strip.white = FALSE, fileEncoding = "UTF-8-BOM")
    if (!identical(names(x), expected_names)) {
      stop("abnormal col name ", filename)
    }
    x
  }

  strict_date <- function(x, variable) {
    valid_shape <- is.na(x) | grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
    if (any(!valid_shape)) stop("Unexpected date text in ", variable)
    result <- as.Date(x, format = "%m/%d/%Y")
    supplied <- which(!is.na(x))
    pieces <- strsplit(x[supplied], "/", fixed = TRUE)
    expected <- vapply(pieces, function(z) {
      sprintf("%04d-%02d-%02d", as.integer(z[3]), as.integer(z[1]),
              as.integer(z[2]))
    }, character(1))
    if (any(is.na(result[supplied])) ||
        any(format(result[supplied], "%Y-%m-%d") != expected)) {
      stop("abnormal date in ", variable)
    }
    result
  }

  strict_numeric <- function(x, variable) {
    number_pattern <- "^[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?$"
    valid_shape <- is.na(x) | grepl(number_pattern, x)
    if (any(!valid_shape)) stop("invalid numeric text in ", variable)
    result <- as.numeric(x)
    if (any(!is.na(x) & (!is.finite(result) | is.na(result)))) {
      stop("infinitye numeric value in ", variable)
    }
    result
  }

  strict_factor <- function(x, levels, variable) {
    if (any(!is.na(x) & !x %in% levels)) {
      stop("invalid category in ", variable, ": ",
           paste(unique(x[!is.na(x) & !x %in% levels]), collapse = ", "))
    }
    factor(x, levels = levels)
  }

  patient_names <- c("MRN", "DOB", "race", "financialclass", "ethnicity",
                     "hypertension", "CHF", "diabetes")
  encounter_names <- c("MRN", "contact_date", "enc_type", "temp",
                       "distress_score", "WBC", "BMI")
  patient_text <- read_source("PatientData-2.csv", patient_names)
  encounter_text <- read_source("EncounterData-2.csv", encounter_names)
  if (nrow(patient_text) == 0L || nrow(encounter_text) == 0L) {
    stop("Both input files must contain at least one data row.")
  }
  if (anyNA(patient_text$MRN) || anyNA(encounter_text$MRN) ||
      any(!nzchar(patient_text$MRN)) || any(!nzchar(encounter_text$MRN))) {
    stop("Missing MRN value prevents a verified patient-to-encounter.")
  }
  if (anyDuplicated(patient_text$MRN)) stop("Patient MRN is not unique.")

  raw_patients <- patient_text
  raw_patients$DOB <- strict_date(patient_text$DOB, "DOB")
  raw_patients$race <- strict_factor(patient_text$race,
                                    c("White", "Black", "Other"), "race")
  raw_patients$financialclass <- strict_factor(patient_text$financialclass,
                                    c("Medicare", "Private"), "financialclass")
  raw_patients$ethnicity <- strict_factor(patient_text$ethnicity,
                                    c("non-Hispanic", "Hispanic"), "ethnicity")
  history_variables <- c("hypertension", "CHF", "diabetes")
  for (variable in history_variables) {
    raw_patients[[variable]] <- strict_factor(patient_text[[variable]],
                                              c("N", "Y"), variable)
  }
  raw_encounters <- encounter_text
  raw_encounters$contact_date <- strict_date(encounter_text$contact_date,
                                             "contact_date")
  raw_encounters$enc_type <- strict_factor(encounter_text$enc_type,
                    c("Office visit", "Hospital Encounter"), "enc_type")
  for (variable in c("temp", "distress_score", "WBC", "BMI")) {
    raw_encounters[[variable]] <- strict_numeric(encounter_text[[variable]],
                                                variable)
  }
  if (anyNA(raw_encounters$contact_date)) stop("Encounter date is required.")

  patient_index <- match(raw_encounters$MRN, raw_patients$MRN)
  if (anyNA(patient_index)) stop("Some MRNs have no record.")
  if (!setequal(raw_encounters$MRN, raw_patients$MRN)) {
    stop("Some MRNs have no encounter in the encounter file.")
  }
  attach_patients <- function(encounters, patients) {
    joined <- cbind(encounters, patients[patient_index,
                        setdiff(names(patients), "MRN"), drop = FALSE])
    rownames(joined) <- NULL
    joined
  }
  base_analytic_dataset <- attach_patients(raw_encounters, raw_patients)

  dob_at_encounter <- raw_patients$DOB[patient_index]
  age_at_encounter <- as.integer(format(raw_encounters$contact_date, "%Y")) -
    as.integer(format(dob_at_encounter, "%Y")) -
    as.integer(format(raw_encounters$contact_date, "%m%d") <
               format(dob_at_encounter, "%m%d"))
  dob_flag_encounter <- !is.na(age_at_encounter) &
    (age_at_encounter > 120L | age_at_encounter < 0L)
  dob_flag_patient <- raw_patients$MRN %in%
    raw_encounters$MRN[dob_flag_encounter]
  patients <- raw_patients
  patients$DOB[dob_flag_patient] <- as.Date(NA)

  encounters <- raw_encounters
  wbc_floored <- !is.na(encounters$WBC) & encounters$WBC < 1
  bmi_invalid <- !is.na(encounters$BMI) & encounters$BMI <= 0
  encounters$WBC[wbc_floored] <- 1
  encounters$BMI[bmi_invalid] <- NA_real_
  analytic_dataset <- attach_patients(encounters, patients)

  make_wbc_category <- function(wbc) {
    category <- rep("Not Taken", length(wbc))
    category[!is.na(wbc) & wbc < 3.2] <- "Low"
    category[!is.na(wbc) & wbc >= 3.2 & wbc <= 9.8] <- "Normal"
    category[!is.na(wbc) & wbc > 9.8] <- "High"
    factor(category, levels = c("Low", "Normal", "High", "Not Taken"),
           ordered = TRUE)
  }
  analytic_dataset$WBC_category <- make_wbc_category(analytic_dataset$WBC)

  log_rows <- function(entity, rows, mrn, variable, raw, clean, reason) {
    data.frame(entity = rep(entity, length(rows)), source_row = rows,
               MRN = mrn[rows], variable = rep(variable, length(rows)),
               raw_value = as.character(raw[rows]),
               clean_value = as.character(clean[rows]),
               reason = rep(reason, length(rows)), stringsAsFactors = FALSE)
  }
  cleaning_log <- rbind(
    log_rows("patient", which(dob_flag_patient), patients$MRN, "DOB",
             patient_text$DOB, patients$DOB,
             "DOB implies age above 120 or below 0 at an encounter; set missing"),
    log_rows("encounter", which(wbc_floored), encounters$MRN, "WBC",
             encounter_text$WBC, encounters$WBC,
             "Change <1 WBC to 1 as required"),
    log_rows("encounter", which(bmi_invalid), encounters$MRN, "BMI",
             encounter_text$BMI, encounters$BMI,
             "invalid (negative) BMI")
  )
  rownames(cleaning_log) <- NULL

  patient_audit <- data.frame(source_row = seq_len(nrow(patients)),
                MRN = patients$MRN, DOB_raw = patient_text$DOB,
                DOB_clean = patients$DOB, DOB_invalid = dob_flag_patient)
  encounter_audit <- data.frame(source_row = seq_len(nrow(encounters)),
                MRN = encounters$MRN, contact_date = encounters$contact_date,
                DOB_raw = patient_text$DOB[patient_index],
                age_years_from_raw_DOB = age_at_encounter,
                DOB_invalid = dob_flag_encounter,
                WBC_raw = raw_encounters$WBC, WBC_clean = encounters$WBC,
                WBC_floored = wbc_floored,
                BMI_raw = raw_encounters$BMI, BMI_clean = encounters$BMI,
                BMI_invalid = bmi_invalid)


  wbc_counts <- table(analytic_dataset$WBC_category)
  wbc_summary <- data.frame(group = names(wbc_counts),
                            n = as.integer(wbc_counts),
                            percent = 100 * as.integer(wbc_counts) /
                              nrow(analytic_dataset), stringsAsFactors = FALSE)
  wbc_summary$label <- sprintf("%d (%.1f%%)", wbc_summary$n,
                                wbc_summary$percent)
  table1_variables <- c("race", "ethnicity", "financialclass",
                         "hypertension", "CHF", "diabetes")
  table1_parts <- lapply(table1_variables, function(variable) {
    x <- patients[[variable]]
    counts <- table(x, useNA = "ifany")
    categories <- names(counts)
    categories[is.na(categories)] <- "Missing"
    if (variable %in% history_variables) {
      categories[categories == "N"] <- "No"
      categories[categories == "Y"] <- "Yes"
    }
    out <- data.frame(variable = variable, category = categories,
                       n = as.integer(counts),
                       percent = 100 * as.integer(counts) / nrow(patients),
                       stringsAsFactors = FALSE)
    out$label <- sprintf("%d (%.1f%%)", out$n, out$percent)
    out
  })
  table1 <- do.call(rbind, table1_parts)
  rownames(table1) <- NULL
  missingness <- data.frame(variable = names(analytic_dataset),
               n_missing = vapply(analytic_dataset, function(x) sum(is.na(x)),
                                   integer(1)), row.names = NULL)
  missingness$percent_missing <- 100 * missingness$n_missing /
    nrow(analytic_dataset)


  checks <- list()
  add_check <- function(name, pass, observed, expected) {
    checks[[length(checks) + 1L]] <<- data.frame(check = name,
      observed = as.character(observed), expected = as.character(expected),
      pass = isTRUE(pass), stringsAsFactors = FALSE)
  }
  patient_expected_shape <- c(nrow(patient_text), length(patient_names))
  encounter_expected_shape <- c(nrow(encounter_text), length(encounter_names))
  base_expected_shape <- c(nrow(encounter_text),
                            length(encounter_names) + length(patient_names) - 1L)
  final_expected_shape <- base_expected_shape + c(0L, 1L)
  add_check("patient_input_shape", identical(dim(raw_patients), patient_expected_shape),
            paste(dim(raw_patients), collapse = " x "),
            paste(patient_expected_shape, collapse = " x "))
  add_check("encounter_input_shape", identical(dim(raw_encounters), encounter_expected_shape),
            paste(dim(raw_encounters), collapse = " x "),
            paste(encounter_expected_shape, collapse = " x "))
  add_check("patient_source_exact_duplicates", !anyDuplicated(patient_text),
            sum(duplicated(patient_text)), 0L)
  add_check("encounter_source_exact_duplicates", !anyDuplicated(encounter_text),
            sum(duplicated(encounter_text)), 0L)
  encounter_key <- raw_encounters[c("MRN", "contact_date")]
  add_check("encounter_MRN_date_duplicates", !anyDuplicated(encounter_key),
            sum(duplicated(encounter_key)), 0L)
  dates_before_birth <- !is.na(dob_at_encounter) &
    raw_encounters$contact_date < dob_at_encounter
  add_check("encounter_date_not_before_raw_DOB", !any(dates_before_birth),
            sum(dates_before_birth), 0L)
  add_check("patient_MRN_unique", !anyDuplicated(patients$MRN),
            length(unique(patients$MRN)), nrow(patients))
  add_check("all_encounters_matched", !anyNA(patient_index),
            sum(!is.na(patient_index)), nrow(encounters))
  add_check("all_patients_represented", setequal(encounters$MRN, patients$MRN),
            length(unique(encounters$MRN)), nrow(patients))
  add_check("base_analytic_shape", identical(dim(base_analytic_dataset),
                                                base_expected_shape),
            paste(dim(base_analytic_dataset), collapse = " x "),
            paste(base_expected_shape, collapse = " x "))
  add_check("final_analytic_shape", identical(dim(analytic_dataset), final_expected_shape),
            paste(dim(analytic_dataset), collapse = " x "),
            paste(final_expected_shape, collapse = " x "))
  add_check("encounter_order_preserved",
            identical(analytic_dataset$MRN, raw_encounters$MRN) &&
              identical(analytic_dataset$contact_date, raw_encounters$contact_date),
            "MRN and contact_date vectors", "identical to encounter source")
  add_check("date_columns_parsed", inherits(analytic_dataset$DOB, "Date") &&
              inherits(analytic_dataset$contact_date, "Date"),
            paste(class(analytic_dataset$DOB), class(analytic_dataset$contact_date)),
            "Date Date")
  add_check("WBC_minimum_one", all(is.na(analytic_dataset$WBC) |
                                      analytic_dataset$WBC >= 1),
            min(analytic_dataset$WBC, na.rm = TRUE), ">= 1")
  add_check("WBC_only_values_below_one_changed",
            identical(analytic_dataset$WBC[!wbc_floored],
                       raw_encounters$WBC[!wbc_floored]) &&
              all(analytic_dataset$WBC[wbc_floored] == 1),
            sum(wbc_floored), sum(!is.na(raw_encounters$WBC) &
                                   raw_encounters$WBC < 1))
  add_check("WBC_ordered_factor",
            is.ordered(analytic_dataset$WBC_category) &&
              identical(levels(analytic_dataset$WBC_category),
                        c("Low", "Normal", "High", "Not Taken")),
            paste(levels(analytic_dataset$WBC_category), collapse = ", "),
            "Low, Normal, High, Not Taken")
  boundary_check <- as.character(make_wbc_category(c(3.19, 3.2, 9.8, 9.81, NA)))
  add_check("WBC_boundaries_and_missing",
            identical(boundary_check, c("Low", "Normal", "Normal", "High", "Not Taken")),
            paste(boundary_check, collapse = ", "),
            "Low, Normal, Normal, High, Not Taken")
  add_check("WBC_summary_denominator", sum(wbc_summary$n) == nrow(encounters),
            sum(wbc_summary$n), nrow(encounters))
  add_check("Table1_patient_denominators",
            all(tapply(table1$n, table1$variable, sum) == nrow(patients)),
            paste(tapply(table1$n, table1$variable, sum), collapse = ", "),
            paste(nrow(patients), "per variable"))
  add_check("BMI_missingness_accounted_for",
            sum(is.na(analytic_dataset$BMI)) == sum(is.na(raw_encounters$BMI)) +
              sum(bmi_invalid), sum(is.na(analytic_dataset$BMI)),
            sum(is.na(raw_encounters$BMI)) + sum(bmi_invalid))
  add_check("positive_BMI_preserved",
            identical(analytic_dataset$BMI[!bmi_invalid],
                       raw_encounters$BMI[!bmi_invalid]),
            "values outside flagged rows", "identical to encounter source")
  add_check("DOB_flagged_values_missing", all(is.na(patients$DOB[dob_flag_patient])) &&
              identical(patients$DOB[!dob_flag_patient],
                         raw_patients$DOB[!dob_flag_patient]),
            sum(dob_flag_patient), "all flagged values are missing; other values preserved")
  add_check("temperature_and_distress_preserved",
            identical(analytic_dataset$temp, raw_encounters$temp) &&
              identical(analytic_dataset$distress_score, raw_encounters$distress_score),
            "temperature and distress vectors", "identical to encounter source")
  add_check("cleaning_log_complete",
            nrow(cleaning_log) == sum(dob_flag_patient) + sum(wbc_floored) + sum(bmi_invalid),
            nrow(cleaning_log), sum(dob_flag_patient) + sum(wbc_floored) + sum(bmi_invalid))
  quality_checks <- do.call(rbind, checks)
  if (!all(quality_checks$pass)) {
    stop("Quality checks failed: ", paste(quality_checks$check[!quality_checks$pass],
                                         collapse = ", "))
  }

  data_dictionary <- data.frame(variable = names(analytic_dataset),
    type = vapply(analytic_dataset, function(x) paste(class(x), collapse = "/"),
                  character(1)),
    description = c("Patient identifier; repeated across encounters",
      "Encounter date; imported from month/day/year text",
      "Encounter type", "Temperature; unit not documented in source",
      "Distress score; nonmissing observed scores are integers; scale bounds not documented",
      "White blood cell count in 10^3 cells/uL; values below 1 replaced with 1 per assignment",
      "Body mass index; nonpositive values set missing",
      "Date of birth; set missing if any encounter implies age above 120 or below 0",
      "Patient race", "Patient financial class", "Patient ethnicity",
      "Hypertension history: N = No, Y = Yes",
      "Congestive heart failure history: N = No, Y = Yes",
      "Diabetes history: N = No, Y = Yes",
      "Ordered categories: Low (<3.2), Normal (3.2-9.8 inclusive), High (>9.8), Not Taken (missing)"),
    unit_of_observation = rep("encounter", ncol(analytic_dataset)),
    stringsAsFactors = FALSE, row.names = NULL)

  stats <- list(n_patients = nrow(patients), n_encounters = nrow(encounters),
    n_wbc_floored = sum(wbc_floored), n_bmi_invalid = sum(bmi_invalid),
    n_dob_invalid = sum(dob_flag_patient),
    n_encounters_dob_invalid = sum(dob_flag_encounter),
    n_bmi_missing_raw = sum(is.na(raw_encounters$BMI)),
    n_bmi_missing_clean = sum(is.na(encounters$BMI)),
    n_distress_missing = sum(is.na(encounters$distress_score)),
    earliest_encounter_date = min(encounters$contact_date),
    latest_encounter_date = max(encounters$contact_date),
    min_age_raw = min(age_at_encounter, na.rm = TRUE),
    max_age_raw = max(age_at_encounter, na.rm = TRUE))
  result <- list(raw_patients = raw_patients, raw_encounters = raw_encounters,
    base_analytic_dataset = base_analytic_dataset, patients = patients,
    analytic_dataset = analytic_dataset, wbc_summary = wbc_summary,
    table1 = table1, missingness = missingness, cleaning_log = cleaning_log,
    quality_checks = quality_checks, patient_audit = patient_audit,
    encounter_audit = encounter_audit, data_dictionary = data_dictionary,
    stats = stats)

  if (isTRUE(write_outputs)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    csv_outputs <- list(base_analytic_dataset = base_analytic_dataset,
      analytic_dataset = analytic_dataset, patient_clean = patients,
      wbc_summary = wbc_summary, table1 = table1, missingness = missingness,
      cleaning_log = cleaning_log, quality_checks = quality_checks,
      patient_audit = patient_audit, encounter_audit = encounter_audit,
      data_dictionary = data_dictionary)
    for (name in names(csv_outputs)) {
      write.csv(csv_outputs[[name]], file.path(output_dir, paste0(name, ".csv")),
                row.names = FALSE, na = "NA", fileEncoding = "UTF-8")
    }
    saveRDS(analytic_dataset, file.path(output_dir, "analytic_dataset.rds"))
    saveRDS(result, file.path(output_dir, "analysis_results.rds"))
  }
  result
}
