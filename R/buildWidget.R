#' Create an HTML widget
#' 
#' @param widget_name Character string indicating what type of widget to build
#' @param data Data as specified by Vis function
#' @param settings Settings as specified by Vis function
#'
#' @export
#' 
buildWidget <- function(widget_name, data, settings, width = NULL, height = NULL){
  
  x <- list(data=data, settings = settings)

  htmlwidgets::createWidget(widget_name, x, width = width, height = height, package="healthvis")

}