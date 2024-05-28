#' Plot spatial distribution and center-of-gravity of a functional group
#'
#' @param fg
#' @param spatial_plot_type
#' @param grp_list
#' @param atlantis_outputs
#'
#' @return out_plot
#' @export
#'
#' @examples
plot_spatial <- function(fg, spatial_plot_type, grp_list, atlantis_outputs, emocc_sf){

  grps <- grp_list$grps
  tyrs <- atlantis_outputs$tyrs
  fg_dimensions <- atlantis_outputs$fg_dimensions
  areas <- atlantis_outputs$areas
  volumes <- atlantis_outputs$volumes
  coaststates <- atlantis_outputs$coaststates
  this.nc <- atlantis_outputs$this.nc

  # Pull biomass using the same process as the plot_biomass function
  # but we do not average across boxes this time, just across depth
  vn <- c(paste0(fg,"_N"),paste0(fg,"_N1"),paste0(fg,"_N2")) # variable name is functional group + "_N" or "_N1" or "_N2"
  vn <- fg_dimensions$name[which(fg_dimensions$name%in%vn)]
  grd <- fg_dimensions$grd[which(fg_dimensions$name%in%vn)] %>% unique() # find correct grid using the table above
  btype <- grps$BiomassType[which(grps$Name==fg)]
  if(length(btype)==0|length(vn)==0){btype="missing"}

  ## get the data from the netCDF
  if(btype!="missing"){
    Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(this.nc,vn[x])) %>%
      purrr::reduce(`+`)%>%
      setNA(ini.pol, end.pol)
    volumes_arr <- array(data = unlist(volumes),dim = dim(Ndat)[c(1,2)]) # box/layer volumes
    areas_vec <- areas$area
  }

  ## do checks on what kind of critter we're talking about and adjust accordingly
  # get the attributes associated with each functional group
  fg_atts <- grps %>% dplyr::filter(Name==fg)
  fgt <- fg_atts$GroupType # get group type

  # if it's a 3D critter like a fish, or it's plankton-like, biomass is N*volume*(5.7*20/10^9), in metric tons
  if(btype=="vertebrate"|btype=="plankton") {

    totbio_by_box <- apply(Ndat*c(volumes_arr)*(5.7*20/10^9),c(2,3),sum,na.rm=T) %>%
      # multiply by volume of each layer and then sum across layers for each time step and box
      tibble::as_tibble(.name_repair = "minimal") %>%
      suppressMessages() %>%
      purrr::set_names(as.character(tyrs)) %>%
      tidyr::pivot_longer(everything(),names_to="t",values_to="biomass") %>%
      dplyr::mutate(t=as.numeric(t),b=rep(1:dim(Ndat)[2],each=dim(Ndat)[3])) # add box identifier back in

  }

  # if it's a 2D critter like a benthic invert, biomass is N*area*(5.7*20/10^9), in metric tons
  if(btype=="2D") {

    totbio_by_box <- tibble::as_tibble(Ndat*areas_vec*(5.7*20/10^9)) %>% # multiply by area of each box
      purrr::set_names(as.character(tyrs)) %>%
      tidyr::pivot_longer(everything(),names_to="t",values_to="biomass") %>%
      dplyr::mutate(t=as.numeric(t),b=rep(1:dim(Ndat)[1],each=dim(Ndat)[2])) #  add box identifier back in
  }

  # if it's other (like detritus and bacteria), leave it as a density (biomass/total volume), mg N/m^3
  if(btype=="other") {
    # sum_volumes_boxes <- volumes %>% group_by(b) %>% summarise(volume=sum(volume))

    totbio_by_box <- apply(Ndat*c(volumes_arr),c(2,3),sum,na.rm=T) %>%
      # multiply by volume of each layer and then sum across layers for each time step and box
      tibble::as_tibble(.name_repair = "minimal") %>%
      suppressMessages() %>%
      purrr::set_names(as.character(tyrs)) %>%
      tidyr::pivot_longer(everything(),names_to="t",values_to="biomass") %>%
      dplyr::mutate(t=as.numeric(t),b=rep(1:dim(Ndat)[2],each=dim(Ndat)[3])) # add box identifier back in

  }

  # now we have biomass in each box at each time


  # fix an off-by-one identifier issue for box IDs
  totbio_by_box <- totbio_by_box %>% mutate(b=b-1)

  # make either a plot of mean spatial distribution, or a center-of-gravity timeseries
  if(!spatial_plot_type %in% c("spdist","cog")) stop("plot_type must be 'spdist' or 'cog'.")
  if(spatial_plot_type=="spdist"){
    # if we want spatial distribution output, calculate
    # mean proportion of total biomass in each box, across time
    spdist <- totbio_by_box %>%
      dplyr::left_join(areas,by='b') %>%
      dplyr::mutate(total=sum(biomass),
                    dens_sqkm=biomass/area*1e06) %>%
      dplyr::group_by(b) %>%
      dplyr::summarise(meandens=mean(dens_sqkm)) %>% # relative proportion in each box
      dplyr::ungroup()

    # add spatial info and plot
    spdist_sf <- spdist %>%
      dplyr::left_join(emocc_sf,by=c('b'='box_id')) %>%
      sf::st_as_sf()

    # bounding box
    bbox <- sf::st_bbox(spdist_sf)
    out_plot <- ggplot2::ggplot()+
      ggplot2::geom_sf(data=spdist_sf,ggplot2::aes(fill=meandens),col=NA)+
      viridis::scale_fill_viridis()+
      ggplot2::geom_sf(data=coaststates,fill='gray50')+
      ggplot2::coord_sf(xlim=c(bbox[1],bbox[3]),ylim=c(bbox[2],bbox[4]))+
      ggplot2::labs(fill="Biomass Density\n(tons/sq. km)",title=paste0(fg_atts$`Long Name`,"\nDistribution"))
  }

  # if we want COG timeseries, do a slightly different calculation
  if(spatial_plot_type=="cog"){
    # if we want center of gravity
    # assign biomass in each time to the "inside points" from Atlantis, then calc a weighted mean COG
    cogts <- totbio_by_box %>%
      # join spatial data
      dplyr::left_join(emocc_sf,by=c('b'='box_id')) %>%
      dplyr::select(t,b,biomass,insideX,insideY) %>%

      # group by time step and calculate a COG weighted by biomass
      dplyr::group_by(t) %>%
      dplyr::summarise(cogX=weighted.mean(insideX,w=biomass),cogY=weighted.mean(insideY,w=biomass))
    # filter(t>0)

    # plot as non-spatial time series
    # calculate total range in north and east directions to add as an annotation to the plot
    out_plot <- cogts %>%
      tidyr::pivot_longer(contains('cog'),names_to="direction",values_to="meters") %>%
      dplyr::mutate(direction=ifelse(direction=="cogX","eastings","northings")) %>%
      ggplot2::ggplot(ggplot2::aes(t,meters/1000))+
      ggplot2::geom_line()+
      ggplot2::labs(x="Year",y="Km",title=paste0(fg_atts$`Long Name`,"\nCenter-of-Gravity"))+
      ggplot2::facet_wrap(~direction,ncol=1,scales="free_y")

  }
  return(out_plot)
}

