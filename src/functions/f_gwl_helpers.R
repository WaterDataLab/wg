# plotly buttons to remove
buttons_to_remove <- 
  list("zoom2d", "select2d", "lasso2d", "autoscale",
       "hoverClosestCartesian", "hoverCompareCartesian",
       "zoom3d", "pan3d", "resetCameraDefault3d",
       "resetCameraLastSave3d", "hoverClosest3d",
       "orbitRotation", "tableRotation","zoomInGeo", 
       "zoomOutGeo", "resetGeo", "hoverClosestGeo",
       "sendDataToCloud", "pan2d","hoverClosestGl2d",
       "hoverClosestPie","toggleHover","toggleSpikelines",
       "autoScale2d","zoomIn2d","zoomOut2d")

# plotly plugin for leaflet
f_add_deps <- function(dtbl, name, pkg = name) {
  tagList(
    dtbl,
    htmlwidgets::getDependency(name, pkg)
  )
}

# write dashboards
f_write_dashboard <- function(x) {
  dir_out = here::here(glue::glue("content/gsa-{x}"))
  unlink(dir_out, recursive = TRUE)
  dir_create(dir_out)
  rmarkdown::render(input       = here("src/gwl/02_dashboard.Rmd"), 
                    output_file = path(dir_out, "index.html"),
                    params      = list(AOI = x)
  )
}

# encrpyt a file (written dashboard in this case)
f_encrypt_file <- function(x){
  file     = here::here(glue::glue("content/gsa-{x}/index.html"))
  template = here::here("src/etc/password_template.html")
  cmd      = glue::glue("staticrypt {file} password -o {file} -f {template}")
  system(cmd)
}

# copy a local file to s3
f_s3_copy <- function(file_local, file_s3){
  cmd = glue::glue("aws s3 cp {file_local} {file_s3}")
  system(cmd)
}

# generate a presigned url of a file on s3 valid for a default of 
# 604800 seconds (1 week) and return the url as a string 
f_s3_sign <- function(file_s3, file_sign, seconds = 604800){
  cmd = glue::glue("aws s3 presign {file_s3} --expires-in {seconds} > {file_sign}")
  system(cmd)
  presigned_url = readLines(file_sign)
  unlink(file_sign)
  return(presigned_url)
}
