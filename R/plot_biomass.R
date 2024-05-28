plot_biomass <- function(fg, grp_list, atlantis_outputs, ini.pol, end.pol){

  grps <- grp_list$grps
  fg_dimensions <- atlantis_outputs$fg_dimensions
  tyrs <- atlantis_outputs$tyrs
  volumes <- atlantis_outputs$volumes
  this.nc <- atlantis_outputs$this.nc
  areas <- atlantis_outputs$areas

  vn <- c(paste0(fg,"_N"),paste0(fg,"_N1"),paste0(fg,"_N2")) # variable name is functional group + "_N" or "_N1" or "_N2"
  vn <- fg_dimensions$name[which(fg_dimensions$name%in%vn)]
  btype <- grps$BiomassType[which(grps$Name==fg)]
  if(length(btype)==0|length(vn)==0){btype="missing"}

  ## get the data from the netCDF
  if(btype!="missing"){
    Ndat <- purrr::map(1:length(vn), function(x) ncdf4::ncvar_get(this.nc,vn[x])) %>%
      reduce(`+`) %>%
      setNA(ini.pol, end.pol)
    volumes_arr <- array(data = unlist(volumes),dim = dim(Ndat)[c(1,2)]) # box/layer volumes
    areas_vec <- areas$area
  }


  #
  ## do checks on what kind of critter we're talking about and adjust accordingly
  # get the attributes associated with each functional group
  fg_atts <- grps %>% dplyr::filter(Name==fg)
  fgt <- fg_atts$GroupType # get group type


  # if it's a 3D critter like a fish, or it's plankton-like, biomass is N*volume*(5.7*20/10^9), in metric tons
  if(btype=="vertebrate"|btype=="plankton") {

    totbio <- apply(Ndat*c(volumes_arr)*(5.7*20/10^9),3,sum,na.rm=T) # multiply by volume of each layer and then sum across boxes and layers for each time step

    Ndat2 <- tibble::tibble(t=tyrs,biomass=totbio)

  }

  # if it's a 2D critter like a benthic invert, biomass is N*area*(5.7*20/10^9), in metric tons
  if(btype=="2D") {

    totbio <- apply(Ndat*areas_vec*(5.7*20/10^9),2,sum,na.rm=T) # multiply by area of each layer and then sum across boxes and layers for each time step

    Ndat2 <- tibble::tibble(t=tyrs,biomass=totbio)
  }

  # if it's other (like detritus and bacteria), leave it as a density (biomass/total volume), mg N/m^3
  if(btype=="other") {

    sum_volumes <- sum(volumes$volume)

    totbio <- apply(Ndat*c(volumes_arr),3,sum,na.rm=T)/sum_volumes

    Ndat2 <- tibble::tibble(t=tyrs,biomass=totbio)
  }
  # if it's missing (i.e., group is turned off), leave it as an empty df
  if(btype=="missing") {

    Ndat2 <- tibble::tibble(t=tyrs,biomass=NA)
  }

  # make the plot
  ylabel <- switch(btype, "other" = "mg N/m^3", "vertebrate" = "Metric Tons", "2D" = "Metric Tons","plankton" = "Metric Tons","missing"="Missing")
  plot_out <- Ndat2 %>%
    dplyr::mutate(btype=btype) %>%
    # filter(t>0) %>% # remove plotting artifact
    ggplot2::ggplot(ggplot2::aes(t,biomass))+
    ggplot2::geom_line()+
    ggplot2::labs(y=ylabel,x="Year",title=paste0(fg_atts$`Long Name`,"\nBiomass"))

  return(plot_out)

}
