f_gwl_preprocess <- function(input_basin){
  # ith GSA to loop over
  # input_basin = "sasb"
  
  # filter to selected GSAs
  # gsa_selected = st_transform(filter(gsa, web_name == input_basin), 4326)
  
  b118_selected = st_transform(dplyr::filter(b118, web_name == input_basin), 4326)
  
  # water year type
  mwyt <- wyt %>% 
    # TODO: download and join HUC8 boundaries to filter in the next step
    # filter(HUC8 == names(which.max(table(input_basin$HUC8)))) %>% 
    filter(HUC8 == 18010205) %>% 
    mutate(
      water_year_type = case_when(
        WYT == "Wet" ~ "W",
        WYT == "Above Normal" ~ "AN",
        WYT == "Below Normal" ~ "BN",
        WYT == "Dry" ~ "D",
        WYT == "Critical" ~ "C")
    )
  
  
  # add observation count and time range. site code has no NA vals
  maoi <- gwl %>% 
    filter(web_name == input_basin) %>% 
    group_by(SITE_CODE) %>% 
    mutate(
      # date range over which samples are taken
      SAMPLE_RANGE = paste(range(MSMT_DATE), collapse = " - "),
      # count of samples per site
      SAMPLE_COUNT = n(),
      # percentile of each measurement in the historical record
      PERCENTILE = 1 - percent_rank(GSE_GWE),
      PERCENTILE_BIN = case_when(
        PERCENTILE == 1 ~ "highest recorded",
        PERCENTILE == 0 ~ "lowest recorded",
        PERCENTILE > 0 & PERCENTILE < 0.25 ~ "< 25th percentile",
        PERCENTILE >= 0.25 & PERCENTILE < 0.50 ~ "25-50th percentile",
        PERCENTILE >= 0.50 & PERCENTILE < 0.75 ~ "50-75th percentile",
        PERCENTILE >= 0.75 & PERCENTILE < 1 ~ "75-99th percentile",
      )
    )
    ungroup()
  
  
  # TODO: SPC on groundwater levels (e.g., outlier control)
  
  
  # ggplots of DBGS - IMPT TO SORT!!!
  sc <- unique(maoi$SITE_CODE) %>% sort() 
  ns <- group_by(maoi, SITE_CODE) %>% slice(1) %>% ungroup() %>% arrange(SITE_CODE) 
  ns$lab <- paste0(
    "<p><b>state well num:</b> ", ns$SWN, "</p>",
    "<p><b>coords x:</b> ", ns$LONGITUDE, "</p>",
    "<p><b>coords y:</b> ", ns$LATITUDE, "</p>",
    "<p><b>n samp:</b> ", ns$SAMPLE_COUNT, "</p>",
    "<p><b>t range:</b> ", ns$SAMPLE_RANGE, "</p>",
    "<p><b>depth (ft):</b> ", ns$WELL_DEPTH, "</p>",
    "<p><b>perf int (ft):</b> ", ns$TOP_PRF_INT, " to ", ns$BOT_PRF_INT, "</p>",
    "<p><b>use:</b> ", ns$WELL_USE, "</p>",
    "<p><b>type:</b> ", ns$WELL_TYPE, "</p>",
    "<p><b>agency:</b> ", ns$WLM_ORG_NAME, "</p>",
    "<p><b>GSA:</b> ", ns$GSA_Name, "</p>"
  )
  
  
  # add seasons
  maoi <- maoi %>% 
    mutate(
      season = case_when(
        month(MSMT_DATE) %in% 8:11 ~ "fall",
        month(MSMT_DATE) %in% 3:5  ~ "spring",
        TRUE ~ "other"
      )
    ) %>% 
    # add seasonal range
    group_by(SITE_CODE, season) %>% 
    mutate(
      fall_high   = ifelse(season == "fall",   min(GSE_GWE, na.rm = TRUE), NA),
      fall_low    = ifelse(season == "fall",   max(GSE_GWE, na.rm = TRUE), NA),
      spring_high = ifelse(season == "spring", min(GSE_GWE, na.rm = TRUE), NA),
      spring_low  = ifelse(season == "spring", max(GSE_GWE, na.rm = TRUE), NA)
    ) %>% 
    ungroup() %>% 
    mutate(water_year = f_calculate_water_year(MSMT_DATE)) %>% 
    left_join(mwyt, by = c("water_year" = "WY")) %>% 
    mutate(
      water_year_type = 
        factor(
          water_year_type,
          levels = c("W","AN","BN","D","C"), 
          labels = c("Wet","Above Normal","Below Normal","Dry","Critical")
        ),
      water_year_start = ymd_hms(paste0(water_year-1, "-10-01 00:00:00")), 
      water_year_end   = ymd_hms(paste0((water_year), "-09-30 24:00:00"))
    )
  
  
  # build plotly objects
  p <- vector("list", length(sc))
  for(i in seq_along(p)){
    d <- maoi %>% 
      filter(SITE_CODE == sc[i])
    
    # water year range rectangles - join to WY type data
    wy_rng <- c(min(d$water_year_start, na.rm = TRUE), 
                max(d$water_year_start, na.rm = TRUE),
                min(d$water_year_end, na.rm = TRUE), 
                max(d$water_year_end, na.rm = TRUE))
    wy_rect <- tibble(t0 = seq(wy_rng[1], wy_rng[2], "1 year"),
                      t1 = seq(wy_rng[3], wy_rng[4], "1 year")) %>% 
      mutate(water_year = year(t1)) %>% 
      left_join(mwyt, by = c("water_year" = "WY")) %>% 
      mutate(water_year_type = 
               factor(water_year_type,
                      levels = c("W","AN","BN","D","C"), 
                      labels = c("Wet","Above Normal","Below Normal","Dry","Critical")))
    
    # dumb hack to make ggplotly carry all factor levels
    dummy_vals <- setdiff(levels(wy_rect$water_year_type), unique(wy_rect$water_year_type))
    for(diff in seq_along(dummy_vals)) {
      new_row_i <- nrow(wy_rect) + 1
      wy_rect[new_row_i,] <- wy_rect[1,]
      for (n in names(wy_rect[1,])) {
        wy_rect[new_row_i,][n] = NA
      }
      wy_rect[new_row_i,]$water_year_type <- dummy_vals[diff]
    }
    
    # build ggplots and plotly objects
    p[[i]] <- ggplot() +
      geom_rect(data = wy_rect, 
                mapping = aes(
                  xmin  = t0, 
                  xmax  = t1, 
                  ymin  = min(-d$GSE_GWE, na.rm=TRUE), 
                  ymax  = 0,
                  fill  = water_year_type,
                  WY    = water_year), alpha = 0.5) +
      geom_point(data = d, mapping = aes(MSMT_DATE, -GSE_GWE)) +
      geom_line(data = d, mapping = aes(MSMT_DATE, -GSE_GWE)) +
      geom_hline(data = d, mapping = aes(yintercept = -spring_high), 
                 lwd = 1, linetype = "dotted", color = "blue") +
      geom_hline(data = d, mapping = aes(yintercept = -spring_low),  
                 lwd = 1, linetype = "dotted", color = "cyan") +
      geom_hline(data = d, mapping = aes(yintercept = -fall_high),   
                 lwd = 1, linetype = "dotted", color = "orange") +
      geom_hline(data = d, mapping = aes(yintercept = -fall_low),    
                 lwd = 1, linetype = "dotted", color = "red") +
      scale_fill_brewer(palette = "RdYlBu", direction = -1, drop = FALSE) +
      coord_cartesian(ylim = c(min(-d$GSE_GWE, na.rm=TRUE), 0)) +
      labs(y = "DBGS (FT)", x = NULL,
           fill = "Year Type", title = sc[i])
    p[[i]] <- ggplotly(p[[i]]) %>% 
      as.tags() %>%
      {tags$div(style="width:500px; height:285px;", .)} %>%
      as.character() #%>% 
    # stringr::str_replace("height:200px","height:100%")
  }
  
  # leaflet
  pal <- colorFactor(colormap::colormap(colormap::colormaps$viridis, 
                                        nshades = length(unique(ns$WLM_ORG_NAME))), 
                     domain = ns$WLM_ORG_NAME)
  
  # prep gwl data as points for leaflet
  ns <- st_as_sf(ns, coords = c("LONGITUDE", "LATITUDE"), crs = 4269, remove = FALSE)
  
  # make leaflet
  l <- leaflet() %>% 
    addProviderTiles(providers$CartoDB.Positron,   group = "Light") %>% 
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$OpenStreetMap,      group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery,  group = "World") %>%
    addPolygons(data = b118_selected,
                fillOpacity = 0,
                color = "black") %>%
    addCircleMarkers(data = st_transform(ns, 4326), 
                     color = ~pal(ns$WLM_ORG_NAME), 
                     stroke = FALSE,
                     radius = 4, 
                     fillOpacity = .8,
                     popup = p,
                     #label = lapply(ns$lab, htmltools::HTML),
                     group = ns$WLM_ORG_NAME) %>% 
    addLegend(pal = pal, 
              values = ns$WLM_ORG_NAME,
              title = "Agency",
              position = "topright") %>% 
    addLayersControl(
      baseGroups    = c("Light", "Dark", "Street", "World"),
      overlayGroups = ns$WLM_ORG_NAME,
      options       = layersControlOptions(collapsed = FALSE,
                                           position = "topright")) %>% 
    # JS for polygon popups
    onRender(
      "function(el,x) {
    this.on('popupopen', function() {HTMLWidgets.staticRender(); remove()})
    }"
    ) %>%
    f_add_deps("plotly") %>%
    htmltools::attachDependencies(
      plotly:::plotlyMainBundle(), append = TRUE
    ) %>%
    htmltools::attachDependencies(
      crosstalk::crosstalkLibs(),  append = TRUE
    ) %>%
    browsable()
  
  return(list(l = l, maoi = maoi))
}
