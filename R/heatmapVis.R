#' Create a sortable heatmap.
#'
#' \code{heatmapVis} creates an interactive, sortable heatmap. Visualizes a matrix of subjects
#' by observations, and uses an accompanying matrix of subjects by additional covariates to sort.
#'
#' @param data Matrix to be displayed in heatmap from (rows are observations/subjects/etc.)
#' Row and column names are used in the figure.
#' @param sort.by Dataframe of outcomes to sort on (same number of rows as data)
#' @param colors Vector of colors that the heatmap should range through (3 colors: low, medium, high)
#' @param plot.title The title of the plot to be created
#' @export
#' @note Currently, matricies with >100 rows or columns are currently a bit slow, and larger matrices may hit a data passing limit.
#' @examples
#' # Create a matrix of random values for a set of subjects
#' nsubj = 40
#' nobs = 25
#' data1 <- matrix(rnorm(nsubj*nobs), nsubj, nobs)
#' rownames(data1) <- sapply(1:nsubj, function(x){paste("S", x, sep="")})
#' colnames(data1) <- sapply(1:nobs, function(x){paste("V", x, sep="")})
#'
#' # Create a random set of discrete and continuous covariates to sort by
#' sort.by1 <- data.frame("Treatment"=rbinom(nsubj, 1, 0.4), "Age"=rpois(nsubj, 30))
#'
#' heatmapVis(data=data1, sort.by=sort.by1) 
#' 

heatmapVis <- function(data, sort.by, colors = c("#D33F6A","#E99A2C","#E2E6BD"), plot.title="Heatmap Visualization"){
	if(is.null(dim(data)) | is.null(dim(sort.by))){
		stop("data and sort.by must be matrices and/or data.frames.")
	}

	if(nrow(data) != nrow(sort.by)){
		stop("sort.by must have same number of rows as data.")
	}

	if(length(colors) != 3){
		stop("need exactly three colors: low, medium, high.")
	}	

	if(is.null(colnames(sort.by))){
		stop("please provide column names for your sorting variables.")
	}

	ordering <- apply(sort.by, 2, order) # Gets ordering for each outcome
#	data.sort <- apply(sort.by, 2, function(x){x[order(x)]}) # For the future

	if(is.null(rownames(data))){
		rownames <- 1:nrow(data)
	} else {
		rownames <- rownames(data)
	}

	if(is.null(colnames(data))){
		colnames <- 1:ncol(data)
	} else {
		colnames <- colnames(data)
	}
	
	vlist <- list("Sort By"=c("None", colnames(sort.by)))
	medians <- apply(data, 2, median)
	names(medians) <- NULL
	
	settings <- list(rownames=rownames, colnames=colnames, ordering=ordering, ordnames=colnames(sort.by), colors=colors, nsubj=nrow(data), nobs=ncol(data), ncov=ncol(ordering), medians=medians)

	ui <- fluidPage(

		# Application title
		titlePanel(plot.title),

		# Sidebar
		sidebarLayout(
			selectInput("sortby",
					"Sort By",
					choices = vlist,
					selected = vlist[1]),

			# Show a plot of the generated distribution
			mainPanel(
				heatmapVisOutput("heatmap")
			)
		)
	
	)
  
	server <- function(input, output, session) {

		output$heatmap <- renderSurvivalVis({
			buildWidget("heatmap", t(data), settings)
		})
		observe({
			session$sendCustomMessage(type="handler", list(input$sortby))
		})
	}

	shinyApp(ui, server)  
}

#' @export
heatmapVisOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "heatmap", width, height, package = "healthvis")
}
#' @export
renderHeatmapVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, heatmapVisOutput, env, quoted = TRUE)
}
