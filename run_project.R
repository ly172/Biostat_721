find_project_root <- function(project_dir = NULL) {
  if (!is.null(project_dir)) {
    return(normalizePath(project_dir, winslash = "/", mustWork = TRUE))
  }

  candidates <- character()
  # source() knows the script path even when the working directory is elsewhere.
  for (frame in rev(sys.frames())) {
    if (is.character(frame$ofile) && length(frame$ofile) == 1L) {
      script_path <- frame$ofile
      is_absolute <- grepl("^([[:alpha:]]:)?[/\\\\]", script_path)
      if (!is_absolute && is.character(frame$owd) && length(frame$owd) == 1L) {
        script_path <- file.path(frame$owd, script_path)
      }
      candidates <- c(candidates, dirname(script_path))
    }
  }
  script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(script_arg)) {
    candidates <- c(candidates, dirname(sub("^--file=", "", script_arg[1])))
  }

  # Run / Ctrl+Enter sends text to the console, so no source() path exists.
  # Ask RStudio for the saved editor file in that case.
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                            error = function(e) "")
    if (length(editor_path) == 1L && !is.na(editor_path) && nzchar(editor_path)) {
      candidates <- c(candidates, dirname(editor_path))
    }
    active_project <- tryCatch(rstudioapi::getActiveProject(),
                              error = function(e) NULL)
    candidates <- c(candidates, active_project)
  }
  candidates <- c(candidates, getwd())
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  is_project <- vapply(candidates, function(path) {
    file.exists(file.path(path, "project_report.Rmd")) &&
      file.exists(file.path(path, "R", "analysis.R"))
  }, logical(1))
  if (any(is_project)) return(candidates[which(is_project)[1]])

  stop("Could not locate the report project. Searched:\n  ",
       paste(candidates, collapse = "\n  "),
       "\nOpen the saved run_project.R in RStudio, or call ",
       'main("word", project_dir = "path/to/project").', call. = FALSE)
}

make_overleaf_zip <- function(tex_file, zip_file) {
  if (.Platform$OS.type == "windows") {
    powershell <- unname(Sys.which("pwsh"))
    if (!nzchar(powershell)) powershell <- unname(Sys.which("powershell"))
    if (!nzchar(powershell)) stop("Powershell error")
    ps_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
    command <- paste("Compress-Archive -LiteralPath", ps_quote(tex_file),
                     "-DestinationPath", ps_quote(zip_file), "-Force -ErrorAction Stop")
    status <- system2(powershell, c("-NoProfile", "-NonInteractive", "-Command",
                                   shQuote(command)))
  } else {
    previous_dir <- setwd(dirname(tex_file))
    on.exit(setwd(previous_dir), add = TRUE)
    # Remove a previous archive so obsolete files cannot remain in it.
    if (file.exists(zip_file) && !file.remove(zip_file)) {
      stop("Could not replace existing flies ", zip_file)
    }
    status <- utils::zip(zipfile = zip_file, files = basename(tex_file), flags = "-j")
  }
  if (is.null(status) || status != 0L || !file.exists(zip_file)) {
    stop("Could not create latex")
  }
  members <- utils::unzip(zip_file, list = TRUE)$Name
  if (!identical(members, "main.tex")) stop("Unexpected files in overleaf.")
}

compile_pdf <- function(tex_file, pdf_file) {
  tectonic <- Sys.getenv("TECTONIC", unset = "")
  if (nzchar(tectonic) && !file.exists(tectonic)) {
    resolved <- unname(Sys.which(tectonic))
    if (!nzchar(resolved)) stop("non-executalbe path")
    tectonic <- resolved
  }
  if (!nzchar(tectonic)) tectonic <- unname(Sys.which("tectonic"))
  pdflatex <- unname(Sys.which("pdflatex"))
  if (!nzchar(tectonic) && !nzchar(pdflatex)) {
    message("please upload the generated zip pack in the current directory to overleaf, owing to no local latex")
    return(invisible(FALSE))
  }

  build_dir <- tempfile("721_latex_")
  dir.create(build_dir)
  if (nzchar(tectonic)) {
    status <- system2(tectonic, c("--keep-logs", "--outdir", shQuote(build_dir),
                                 shQuote(tex_file)))
  } else {
    args <- c("-interaction=nonstopmode", "-halt-on-error", "-file-line-error",
              shQuote(paste0("-output-directory=", build_dir)), shQuote(tex_file))
    status <- system2(pdflatex, args)
    if (status == 0L) status <- system2(pdflatex, args)
  }
  built_pdf <- file.path(build_dir, "main.pdf")
  log_file <- file.path(build_dir, "main.log")
  log_lines <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
  if (file.exists(log_file)) {
    file.copy(log_file, file.path(dirname(pdf_file), "outputs", "latex_compile.log"),
              overwrite = TRUE)
  }
  errors <- grep("^!|Emergency stop|Fatal error|LaTeX Error:", log_lines, value = TRUE)
  if (status != 0L || length(errors) || !file.exists(built_pdf) ||
      file.info(built_pdf)$size == 0) {
    stop("latex compile failed. See the files in ", build_dir,
         if (length(errors)) paste0("\n", paste(errors, collapse = "\n")) else "")
  }
  if (!file.copy(built_pdf, pdf_file, overwrite = TRUE)) {
    stop("The PDF compiled, but could not be copied to ", pdf_file)
  }
  message("Created ", pdf_file)
  invisible(TRUE)
}

main <- function(format = "all", project_dir = NULL) {
  format <- match.arg(format, c("all", "word", "latex"))
  project_dir <- find_project_root(project_dir)
  required <- c("rmarkdown", "knitr")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("package needed install.packages(c(",
         paste(sprintf('"%s"', missing), collapse = ", "), "))")
  }
  input <- file.path(project_dir, "project_report.Rmd")
  template <- file.path(project_dir, "report_template.tex")
  needed <- c(input, file.path(project_dir, "R", "analysis.R"),
              file.path(project_dir, "data", "PatientData-2.csv"),
              file.path(project_dir, "data", "EncounterData-2.csv"),
              if (format != "word") template,
              if (format != "latex") file.path(project_dir, "R", "word_tables.R"))
  missing_files <- needed[!file.exists(needed)]
  if (length(missing_files)) {
    stop("Missing required files in project: ", project_dir, "\n  ",
         paste(missing_files, collapse = "\n  "), call. = FALSE)
  }


  if (!rmarkdown::pandoc_available()) {
    candidates <- c(
      file.path(Sys.getenv("ProgramFiles"), "RStudio", "resources", "app",
                "bin", "quarto", "bin", "tools"),
      file.path(Sys.getenv("ProgramFiles"), "RStudio", "bin", "pandoc")
    )
    found <- candidates[file.exists(file.path(candidates, "pandoc.exe"))]
    if (length(found)) Sys.setenv(RSTUDIO_PANDOC = found[1])
    rmarkdown::find_pandoc(cache = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is required")
  }

  if (format != "latex") {
    rmarkdown::render(
      input = input,
      output_format = rmarkdown::word_document(),
      output_file = "project_report.docx",
      output_dir = project_dir,
      knit_root_dir = project_dir,
      envir = new.env(parent = globalenv()),
      quiet = FALSE
    )
  }
  if (format != "word") {
    overleaf_dir <- file.path(project_dir, "overleaf")
    dir.create(overleaf_dir, showWarnings = FALSE)
    rmarkdown::render(
      input = input,
      output_format = rmarkdown::latex_document(template = template, number_sections = TRUE),
      output_file = "main.tex",
      output_dir = overleaf_dir,
      knit_root_dir = project_dir,
      envir = new.env(parent = globalenv()),
      quiet = FALSE
    )
    tex_file <- file.path(overleaf_dir, "main.tex")
    make_overleaf_zip(tex_file, file.path(project_dir, "721_project_overleaf.zip"))
    compile_pdf(tex_file, file.path(project_dir, "project_report.pdf"))
  }
  message("Completed")
}


args <- if (!interactive() && sys.nframe() == 0L) commandArgs(trailingOnly = TRUE) else character()
main(if (length(args)) args[1] else "all")
