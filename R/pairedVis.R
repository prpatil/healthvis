#' Create a paired scatterplot matrix visualization
#'
#' \code{pairedVis} takes a data.frame input and creates a matrix of
#' paired plots for all numeric columns. Points are colored by
#' categorical columns in the data, which the user can cycle through
#' using the drop-down menu (points are all one color otherwise). Users
#' can highlight points in one plot of the matrix to see them highlighted
#' in all of the other plots.
#'
#' @param data A data frame to visualize
#' @param plot.title The title of the plot to appear on the HTML page
#' @export
#' @examples
#' # Let's use the iris data, but add an extra categorical column
#' test_data <- iris
#' test_data$content <- sample(c("High", "Med", "Low", "None"), nrow(test_data), replace=T)
#' pairedVis(test_data)

pairedVis <- function(data, plot.title="My Scatterplot Matrix"){
  
  if(class(data) != "data.frame"){
	stop("Data must be a data frame")
  }
  classes <- sapply(data, class)
  
  nr <- nrow(data)
  cn <- colnames(data)
  ll <- vector("list", length=nr)
  for (irow in 1:nr) ll[[irow]] <- data[irow,]
  
  dropouts <- cn[classes != "numeric"]

  levels <- toJSON(sapply(dropouts, function(x){
			l <- levels(data[[x]])
			if(is.null(l)){
				return( names(table(data[[x]])) )
			} else {
				return(l)
			}
		}, simplify=F))

  n <- sum(classes=="numeric")
  js <- toJSON(ll)

  if (length(dropouts) == 1) dropouts <- list(dropouts)
  if (length(dropouts) == 0) dropouts <- list("None")
  
  # Form input types and ranges/options
  #varType <- c("factor")
  menu.type <- c("factor")
  
  var.list <- list("Category" = dropouts)
  
  data <- js
  settings <- list(levels = levels, dropouts = dropouts, n = n)
  
  inputlist <- list()
  for(i in 1:length(menu.type)){
		if(menu.type[i] == "continuous"){
			inputlist[[i]] <- sliderInput(names(var.list)[i],
					names(var.list)[i],
					min = var.list[[i]][1],
					max = var.list[[i]][2],
					value = 0)
		} else {
			inputlist[[i]] <- selectInput(names(var.list)[i],
					names(var.list)[i],
					choices = var.list[[i]],
					selected = var.list[[i]][1])

		}
 }

  ui <- fluidPage(

		# Application title
		titlePanel(plot.title),

		# Sidebar
		sidebarLayout(
			sidebarPanel("Covariates", inputlist),

			# Show a plot of the generated distribution
			mainPanel(
				pairedVisOutput("survival")
			)
		)
		
	)
  
	server <- function(input, output, session) {

		output$survival <- renderPairedVis({
			buildWidget("paired", data, settings)
		})
		observe({
			covarlist <- list()
			for(i in 1:length(menu.type)){
				covarlist[[i]] <- list("name" = names(var.list)[i], "value" = input[[names(var.list)[i]]])
			}
			session$sendCustomMessage(type="handler", covarlist)
		})
	}
 
	shinyApp(ui, server)  
}

#' @export
pairedVisOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "paired", width, height, package = "healthvis")
}
#' @export
renderPairedVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, pairedVisOutput, env, quoted = TRUE)
}
