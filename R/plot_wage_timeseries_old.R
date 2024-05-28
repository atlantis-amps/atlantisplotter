#' deprecated (old) weight at age calculation
#' @description
#' just keeping it here in case we need to revert for some reason
#'
#' @param fg
#' @param atlantis_outputs
#' @param grp_list
#'
#' @return plot_out
#' @export
#'
#' @examples
plot_wage_timeseries_old <- function(fg, atlantis_outputs, grp_list){

  grps <- grp_list$grps
  # get the attributes associated with each functional group
  fg_atts <- grps %>% filter(Name==fg)
  tyrs <- atlantis_outputs$tyrs
  this.nc <- atlantis_outputs$this.nc
  out <- atlantis_outputs$out


  if(fg_atts$BiomassType!="vertebrate") stop("weight at age only for vertebrates.")

  #Extract from the output .nc file the appropriate reserve N time series variables
  resN_vars <- tidync::hyper_vars(out) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_ResN",name)) %>% # filter for abundance variables
    dplyr::filter(grepl(fg,name)) # filter for specific functional group

  if(nrow(resN_vars)==0) {return("no data.")}
  else {
    # # Actually pull the data from the .nc
    # resN1 <- out %>% hyper_tibble(select_var=resN_vars$name)
    resN1 <- purrr::map(resN_vars$name,ncdf4::ncvar_get,nc=this.nc) %>%
      lapply(na_if,y=0) %>%
      lapply(setNA, ini.pol, end.pol) %>%
      purrr::map(apply,MARGIN=3,FUN=mean,na.rm=T) %>% # mean reserve N by time
      dplyr::bind_cols() %>%
      suppressMessages() %>%
      purrr::set_names(resN_vars$name) %>%
      dplyr::mutate(t=tyrs)
    resN2 <- resN1 %>%
      tidyr::pivot_longer(cols = contains(fg_atts$Name),names_to = 'age_group',values_to = 'resN') %>%
      dplyr::mutate(age=readr::parse_number(age_group)) %>%
      dplyr::mutate(weight=resN*20*5.7*(3.65/2.65)/1000000)   # convert ResN to weight/individual
    # dplyr::filter(t>0)

    # plot
    plot_out <- resN2 %>%
      ggplot2::ggplot(ggplot2::aes(t,weight,col=factor(age)))+
      ggplot2::geom_line()+
      ggplot2::labs(col="Age Group",y="Wet Weight per Individual (kg)",x="Year",title=paste0(fg_atts$`Long Name`,"\nWeight-at-Age"))

    return(plot_out)
  }

}
