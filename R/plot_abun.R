#' Plot functional group abundance
#'
#' @param fg
#' @param grp_list
#' @param atlantis_outputs
#' @param ini.pol
#' @param end.pol
#'
#' @return plot_out
#' @export
#'
#' @examples
plot_abun <- function(fg, grp_list, atlantis_outputs, ini.pol, end.pol){

  grps <- grp_list$grps
  out <- atlantis_outputs$out
  this.nc <- atlantis_outputs$this.nc

  tyrs <- atlantis_outputs$tyrs
  # if the species is TURNED OFF, return an empty plot
  if(grps$IsTurnedOn[grps$Name==fg]==0) return(tibble(t=0,abun=0) %>% ggplot2::ggplot(aes(t,abun))+geom_blank())
  # get the attributes associated with each functional group
  fg_atts <- grps %>% dplyr::filter(Name==fg)
  #Extract from the output .nc file the appropriate time series variables
  abun_vars <- tidync::hyper_vars(out) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_Nums",name)) %>% # filter for abundance variables
    dplyr::filter(grepl(fg,name)) # filter for specific functional group
  # Actually pull the data from the .nc
  abun1 <- purrr::map(abun_vars$name,ncdf4::ncvar_get,nc=this.nc) %>%
    lapply(setNA, ini.pol, end.pol) %>%
    purrr::map(apply,MARGIN=3,FUN=sum,na.rm=T) %>%
    dplyr::bind_cols() %>%
    suppressMessages() %>%
    purrr::set_names(abun_vars$name) %>%
    dplyr::mutate(t=tyrs)

  abun2 <- abun1 %>%
    tidyr::pivot_longer(cols = contains(fg_atts$Name),names_to = 'age_group',values_to = 'abun') %>%
    dplyr::mutate(age=readr::parse_number(age_group))

  # plot
  plot_out <- abun2 %>%
    # filter(t>0) %>% # remove plotting artifact
    ggplot2::ggplot(ggplot2::aes(t,abun/1e6,col=factor(age)))+
    ggplot2::geom_line()+
    ggplot2::labs(col="Age Group",y="Numbers (Millions)",x="Year",title=paste0(fg_atts$`Long Name`,"\nNumbers-at-Age"))

  return(plot_out)


}
