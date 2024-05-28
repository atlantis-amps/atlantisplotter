
get_bgm <- function(thisbgmfile, thisfolder){
  fl <- here::here(thisfolder,thisbgmfile)
  # load the file
  bgm <- rbgm::read_bgm(fl)
  # names(bgm)
  emocc_sf <- rbgm::box_sf(bgm)
  sf::st_crs(emocc_sf) <- sf::st_crs(attr(emocc_sf$geometry, "crs")$proj)
  # box_area <- emocc_sf %>% st_set_geometry(NULL) %>%  dplyr::select(box_id,area) %>%
  #   mutate(sqkm=area/1e6)

  return(emocc_sf)
}
