#' Run GPTStudio Chat App
#'
#' This function initializes and runs the Chat GPT Shiny App as a background job
#' in RStudio and opens it in the viewer pane or browser window.
#'
#' @param host A character string specifying the host on which to run the app.
#'   Defaults to the value of `getOption("shiny.host", "127.0.0.1")`.
#'
#' @return This function does not return a value. It runs the Shiny app as a side effect.
#'
#' @details
#' The function performs the following steps:
#' 1. Verifies that RStudio API is available.
#' 2. Finds an available port for the Shiny app.
#' 3. Creates a temporary directory for the app files.
#' 4. Runs the app as a background job in RStudio.
#' 5. Opens the app in the RStudio viewer pane or browser window.
#'
#' @note This function is designed to work within the RStudio IDE and requires
#'   the rstudioapi package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gptstudio_chat()
#' }
gptstudio_chat <- function(host = getOption("shiny.host", "127.0.0.1")) {
  check_installed(c("miniUI", "future"))
  rstudioapi::verifyAvailable()

  port <- find_available_port()
  app_dir <- create_temp_app_dir()

  run_app_background(app_dir, "gptstudio", host, port)

  if (rstudioapi::versionInfo()$mode == "server") {
    Sys.sleep(3)
  }

  open_app_in_viewer(host, port)
}

# Helper functions

find_available_port <- function() {
  safe_ports <- setdiff(3000:8000, c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697))
  sample(safe_ports, 1)
}

create_temp_app_dir <- function() {
  dir <- normalizePath(tempdir(), winslash = "/")
  app_file <- create_temp_app_file()
  file.copy(app_file, file.path(dir, "app.R"), overwrite = TRUE)
  dir
}

create_temp_app_file <- function() {
  temp_file <- tempfile(fileext = ".R")
  ide_colors <- dput(get_ide_theme_info())
  code_theme_url <- get_highlightjs_theme()

  writeLines(
    # nolint start
    glue::glue(
      "ide_colors <- {{paste(deparse(ide_colors), collapse = '\n')}}
      ui <- gptstudio:::mod_app_ui('app', ide_colors, '{{code_theme_url}}')
      server <- function(input, output, session) {
          gptstudio:::mod_app_server('app', ide_colors)
      }
      shiny::shinyApp(ui, server)",
      .open = "{{", .close = "}}"
    ),
    temp_file)
    # nolint end
  temp_file
}

run_app_background <- function(app_dir, job_name, host, port) {
  job_script <- tempfile(fileext = ".R")
  writeLines(glue::glue(
    "shiny::runApp(appDir = '{app_dir}', port = {port}, host = '{host}')"
  ), job_script)

  rstudioapi::jobRunScript(job_script, name = job_name)
  cli::cli_alert_success("{job_name} initialized as background job in RStudio")
}

open_app_in_viewer <- function(host, port) {
  url <- glue::glue("http://{host}:{port}")
  translated_url <- rstudioapi::translateLocalUrl(url, absolute = TRUE)

  if (host == "127.0.0.1") {
    cli::cli_inform(c(
      "i" = "Showing app in 'Viewer' pane",
      "i" = "Run {.run rstudioapi::viewer(\"{url}\")} to see it"
    ))
  } else {
    cli::cli_alert_info("Showing app in browser window")
  }

  wait_for_bg_app(translated_url)

  rstudioapi::viewer(translated_url)
}

wait_for_bg_app <- function(url, max_seconds = 20) {
  start_time <- Sys.time()
  repeat {
    resp <- tryCatch(
      request(url) |> req_perform(),
      error = function(e) e
    )
    # 如果返回对象不是error，或者是403错误，则认为服务已启动
    if (!inherits(resp, "error") || (inherits(resp, "error") && grepl("403", conditionMessage(resp)))) {
      break
    }
    if (as.numeric(Sys.time() - start_time, units = "secs") > max_seconds) {
      cli::cli_alert_danger("Timeout: Service did not start within {max_seconds} seconds.")
      break
    }
    Sys.sleep(0.2)
  }
}