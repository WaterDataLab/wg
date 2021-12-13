# user agent for server
ua <- httr::user_agent("http://github.com/waterdatalab")

f_cdec_api <- function(path, query) {
  url <- httr::modify_url("https://cdec.water.ca.gov", 
                          path  = path, 
                          query = query)
  
  resp <- httr::GET(url, ua)
  
  # check content type and error informatively if response type changes
  if (httr::http_type(resp) != "text/plain") {
    stop("API did not return text/plain", call. = FALSE)
  }
  
  # error handling
  if (httr::status_code(resp) != 200) {
    stop(
      glue::glue("CDEC API request failed [{status_code(resp)}].\n
                 See docs: https://cdec.water.ca.gov/queryTools.html"),
      call. = FALSE
    )
  }
  
  # parse status, headers, and response as an S3 object
  structure(
    list(
      status  = httr::http_status(resp),
      headers = httr::headers(resp),
      content = suppressMessages(readr::read_csv(httr::content(resp)))
    ),
    class = "cdec_api"
  )
}

# S3 print method for api return object
print.f_cdec_api = function(x, ...) {
  cat("Status and headers:\n\n")
  str(c(x$status, x$headers))
  cat("\n\n Content:\n\n")
  print(x$content)
}
