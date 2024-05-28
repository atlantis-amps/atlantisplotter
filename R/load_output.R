load_output <- function(thisfolder, thisoutncfile, timeperiod) {

  atlantis_outputs <- list()

  # load west cost land for mapping
  coaststates <- rnaturalearth::ne_countries(continent="North America",returnclass = 'sf') %>%
    filter(name %in% c('Canada','United States','Mexico'))

  atlantis_outputs[['coaststates']] <- coaststates

  tictoc::tic("Loading files: ")
  # output file
  out_fl <- here::here(thisfolder, thisoutncfile)
  out <- tidync::tidync(out_fl)
  this.nc <- ncdf4::nc_open(out_fl)

  atlantis_outputs[['out']] <- out
  atlantis_outputs[['this.nc']] <- this.nc

  # derived values for output
  # depths <- out %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="dz") %>% dplyr::select(-t)
  # glimpse(depths)

  # volumes of each layer
  volumes <- out %>%
    tidync::hyper_filter(t=t>timeperiod) %>% #tidync::hyper_filter(t=t==0)
    tidync::hyper_tibble(select_var="volume") %>%
    dplyr::select(-t)

  atlantis_outputs[['volumes']] <- volumes

  # time dimension
  ts <- ncdf4::ncvar_get(this.nc,varid = "t") %>%
    as.numeric
  tyrs <- ts/(60*60*24*timeperiod)

  atlantis_outputs[['tyrs']] <- tyrs

  # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
  areas <- volumes %>%
    dplyr::filter(z==max(z)) %>%
    dplyr::select(b,volume) %>% dplyr::rename(area=volume)

  atlantis_outputs[['areas']] <- areas

  # functional group dimensions
  fg_dimensions <- tidync::hyper_grids(out) %>%
    purrr::pluck("grid") %>%
    purrr::map_df(function(x){
      out %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
        dplyr::mutate(grd=x)
    })

  atlantis_outputs[['fg_dimensions']] <- fg_dimensions

  # diets table
  diet_check <- read.table(here::here(thisfolder, outdietfile), as.is = TRUE,header=TRUE,sep=" ")
  tictoc::toc()

  atlantis_outputs[['diet_check']] <- diet_check

  return(atlantis_outputs)
}
