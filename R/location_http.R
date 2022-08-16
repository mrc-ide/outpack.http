outpack_location_http <- R6::R6Class(
  "outpack_location_http",

  private = list(
    client = NULL
  ),

  public = list(
    ## Later, we will need to come up with a good model here for
    ## auth. The constraints are:
    ##
    ## * needs to work with both montagu (username/password) and
    ##   standalone (oauth, token) flow
    ## * needs to support a declarative configuration so that a python
    ##   client can use the same thing
    ##
    ## There's a reasonably extensible approach
    initialize = function(url) {
      private$client <- outpack_client$new(url)
    },

    list = function() {
      dat <- private$client$get("/metadata/list")
      if (length(dat) == 0) {
        dat <- data_frame(packet = character(),
                          time = character(),
                          hash = character())
      }
      dat
    },

    metadata = function(packet_ids) {
      lapply(packet_ids, function(id) {
        private$client$get(sprintf("/metadata/%s", id))
      })
    },

    fetch_file = function(hash, progress = FALSE) {
      browser()
      path <- private$client$get(sprintf("/file/%s", hash),
                                 download = TRUE, progress = progress)
      path
    }
  ))
