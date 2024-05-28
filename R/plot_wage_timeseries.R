#' Plot weight-at-age time series
#'
#' @param fg
#' @param grp_list
#' @param atlantis_outputs
#'
#' @return plot_out
#' @export
#'
#' @examples
plot_wage_timeseries <- function(fg, grp_list, atlantis_outputs){

  grps <- grp_list$grps
  tyrs <- atlantis_outputs$tyrs
  this.nc <- atlantis_outputs$this.nc
  out <- atlantis_outputs$out


  # get the attributes associated with each functional group
  fg_atts <- grps %>% dplyr::filter(Name==fg)

  if(fg_atts$BiomassType!="vertebrate") stop("weight at age only for vertebrates.")

  #Extract from the output .nc file the appropriate reserve N time series variables
  resN_vars <- tidync::hyper_vars(out) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_ResN",name)) %>% # filter for abundance variables
    dplyr::filter(grepl(fg,name)) # filter for specific functional group

  #Extract from the output .nc file the appropriate structural N time series variables
  strucN_vars <- tidync::hyper_vars(out) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_StructN",name)) %>% # filter for abundance variables
    dplyr::filter(grepl(fg,name)) # filter for specific functional group

  # Get numbers by box
  abun_vars <- tidync::hyper_vars(out) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_Nums",name)) %>% # filter for abundance variables
    dplyr::filter(grepl(fg,name)) # filter for specific functional group

  if(nrow(resN_vars)==0) {return("no data.")}
  else {
    # # Actually pull the data from the .nc
    resN <- purrr::map(resN_vars$name,ncdf4::ncvar_get,nc=this.nc)
    strucN <- purrr::map(strucN_vars$name,ncdf4::ncvar_get,nc=this.nc)
    nums <-purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) #numbers by age group,box,layer,time
    totnums <-nums %>% purrr::map(apply,MARGIN=3,FUN=sum) # total numbers by age group, time
    relnums <- purrr::map2(nums,totnums,sweep,MARGIN=3,FUN=`/`) # divide nums by totnums along the time axis to get relative annual nums per age group/box/layer

    # add the two matrices to get total nitrogen weight
    rnsn <- purrr::map2(resN,strucN,`+`)

    # multiply and sum to get abundance-weighted mean weight at age
    rnsn_summ <- purrr::map2(rnsn,relnums,`*`) %>%
      purrr::map(apply,MARGIN=3,FUN=sum) %>% # mean total N by time
      dplyr::bind_cols() %>% # bind age groups elements together
      suppressMessages() %>%
      purrr::set_names(resN_vars$name) %>%
      mutate(t=tyrs) %>%
      # pivot to long form
      tidyr::pivot_longer(cols = tidyselect::contains(fg_atts$Name),names_to = 'age_group',values_to = 'totN') %>%
      dplyr::mutate(age=readr::parse_number(age_group)) %>%
      mutate(weight=totN*20*5.7/1000000) # convert totN to weight/individual in kg
    # dplyr::filter(t>0)

    # plot
    plot_out <- rnsn_summ %>%
      ggplot2::ggplot(ggplot2::aes(t,weight,col=factor(age)))+
      ggplot2::geom_line()+
      ggplot2::labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year",title=paste0(fg_atts$`Long Name`,"\nWeight-at-Age"))

    return(plot_out)
  }
}
