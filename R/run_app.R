#' Launch the gravitypool Interactive Shiny Application
#'
#' Initializes the modern `bslib`-powered consulting dashboard for sizing,
#' comparing, and presenting gravitational vs. battery pool recirculation systems.
#'
#' @param ... Additional arguments passed to `shiny::shinyAppDir()`.
#'
#' @return Runs the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#'   launch_app()
#' }
launch_app <- function(...) {
  app_dir <- system.file("app", package = "gravitypool")
  if (app_dir == "" || !dir.exists(app_dir)) {
    # Development fallback
    app_dir <- file.path("inst", "app")
  }
  if (!dir.exists(app_dir)) {
    stop("Shiny application directory not found. Please install the package or run from root.")
  }

  shiny::shinyAppDir(appDir = app_dir, ...)
}

#' @rdname launch_app
#' @export
run_app <- launch_app
