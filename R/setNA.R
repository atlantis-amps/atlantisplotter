#' Set values to NA
#'
#' @param mat
#'
#' @return
#' @export
#'
#' @examples
setNA <- function(mat, ini.pol, end.pol) {
  mat2 <- mat
  if(length(dim(mat2))==3) mat2[,c(1,ini.pol:end.pol),]<-NA
  if(length(dim(mat2))==2) mat2[c(1,ini.pol:end.pol),] <- NA
  mat2
}
