test_that("Can connect client", {
  srv <- test_server()
  cl <- outpack_client$new(srv$url)
  cl$get("/")
})
