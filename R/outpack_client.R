outpack_client <- R6::R6Class(
  "outpack_client",

  private = list(
    request = function(verb, path, ..., download = FALSE, progress = FALSE) {
      ## See orderlyweb for a nice pattern here for getting auth
      ## checking and retry on invalid token
      if (!grepl("^/", path)) {
        stop("Expected an absolute path")
      }

      if (download) {
        download <- download_options(progress)
      } else {
        download <- NULL
      }

      r <- verb(paste0(self$url, path), ..., download$options)
      private$response(r, download)
    },

    response = function(response, download = NULL) {
      code <- httr::status_code(response)
      if (code >= 300) {
        if (is_json_response(response)) {
          res <- response_to_json(response)
          stop(outpack_client_error(res$errors[[1]]$message, code, res$errors))
        }
        if (code == 404) {
          stop("endpoint or resource not found")
        } else {
          stop("server returned error code ", code)
        }
      }

      if (!is.null(download)) {
        download$dest
      } else if (is_json_response(response)) {
        response_to_json(response)$data
      } else {
        httr::content(response, "raw")
      }
    }
  ),

  public = list(
    url = NULL,

    initialize = function(url) {
      self$url <- url
      lockBinding("url", self)
    },

    get = function(path, ...) {
      private$request(httr::GET, path, ...)
    }
  ))


outpack_client_error <- function(msg, code, errors) {
  err <- list(message = msg, errors = errors, code = code)
  class(err) <- c("outpack_client_error", "error", "condition")
  err
}


outpack_download_options <- function(progress) {
  dest <- tempfile()
  assert_scalar_logical(progress)
  list(dest = dest,
       options = c(httr::write_disk(dest),
                   httr::accept("application/octet-stream"),
                   if (progress) httr::progress()))
}


response_to_json <- function(r) {
  txt <- httr::content(r, "text", encoding = "UTF-8")
  withCallingHandlers(
    from_json(txt),
    error = function(e) message("Original response:\n\n", txt))
}


is_json_response <- function(r) {
  type <- httr::headers(r)[["Content-Type"]]
  httr::parse_media(type)$complete == "application/json"
}
