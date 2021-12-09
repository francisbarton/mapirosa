query_api <- function(x, y, zoom, type, crs, user_agent = NULL) {

  # OS Maps API - zxy (instead of WMTS)
  # See https://osdatahub.os.uk/docs/wmts/technicalSpecification
  # for details

  # be good to wrap all this in a safely() or a possibly() eventually

  os_base_url2 <- "https://api.os.uk/maps/raster/v1/zxy/"
  zxy_string <- stringr::str_glue(
    "{str_to_sentence(type)}_{crs}/{zoom}/{x}/{y}.png")


  # path <- file.path(download_dir, type, z)
  #
  # if (xfun::dir_create(path)) {
  #   outfile <- file.path(download_dir, type, zoom, str_glue("{x}_{y}.png"))
  # }

  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")


  default_user_agent <- "Francis Barton fbarton@alwaysdata.net"
  user_agent <- match.arg(user_agent, choices = default_user_agent)

  # using {httr2} for the first time properly!
  request(os_base_url2) %>%
    # replace UA string with package name once made into package
    req_user_agent(user_agent) %>%
    req_url_path_append(zxy_string) %>%
    req_url_query(key = os_data_key) %>%
    req_perform() %>%
    resp_body_raw() %>% # read raw as PNG
    # return: PNG data (don't actually write the files)
    png::readPNG(info = TRUE) # %>%
    # png::writePNG(target = outfile)
}
