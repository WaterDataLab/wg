# plotly plugin for leaflet
f_add_deps <- function(dtbl, name, pkg = name) {
  tagList(
    dtbl,
    htmlwidgets::getDependency(name, pkg)
  )
}

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

# run download

# loop over preprocessor and write dashboards

# write dashboards
f_write_dashboard <- function(x) {
  dir_out = sprintf("/Users/richpauloo/Documents/GitHub/wg/content/gsa-%s", x)
  unlink(dir_out, recursive = TRUE)
  dir_create(dir_out)
  rmarkdown::render(input       = here("src/gwl/02_dashboard.Rmd"), 
                    output_file = path(dir_out, "index.html"),
                    params      = list(AOI = x)
  )
}
