#' Set options
#'
#' @return
#' @export
#'
#' @examples
set_options <- function(){

  # ggplot theme
  plot_theme <-   ggplot2::theme_minimal()+
    ggplot2::theme(text=ggplot2::element_text(family="sans",size=10,color="black"),
                   legend.text = ggplot2::element_text(size=10),
                   axis.title=ggplot2::element_text(family="sans",size=14,color="black"),
                   axis.text=ggplot2::element_text(family="sans",size=8,color="black"),
                   panel.grid.major = ggplot2::element_line(color="gray50",linetype=3))
  ggplot2::theme_set(plot_theme)

  options(dplyr.summarise.inform = FALSE)
  sf::sf_use_s2(FALSE)
}
