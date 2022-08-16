test_server <- function(root = NULL, validate = TRUE, ...) {
  skip_if_not_installed("outpack.server")
  if (is.null(root)) {
    root <- outpack::outpack_init(tempfile())
  }
  args <- list(root = root, validate = validate)
  server <- porcelain::porcelain_background$new(
    outpack.server::api, args = args, ...)
  url <- server$url("")
  server$start()
  list(url = url, root = root, server = server)
}
