#' Create an accuracy table visualization
#'
#' \code{accuracyTableVis} allows users to examine the effect of changing
#' a test's sensitivity and specificity or adjusting the prevalence of the
#' disease being tested. Changes in the number of true positives (TP), false
#' positives (FP), true negatives (TN), and false negatives (FN) 
#' are reactively displayed.
#' 
#' @param plot.title The title of the plot to be created
#' @param colors The colors used (two colors, first for TP/TN)
#'
#' @export
#'
#' @examples
#' accuracyTableVis()
#' 


accuracyTableVis <- function(plot.title="Sensitivity/Specificity Plot", colors = c("deepskyblue", "orangered")){

	  if(length(colors) != 2 | class(colors) != "character"){
		stop("Please specify two colors: the first for TP/TN,the second from FP/FN.")
	  }
	  
	  data <- ""
	  settings <- list(colors=colors)

	ui <- fluidPage(

		# Application title
		titlePanel(plot.title),

		# Sidebar with a slider input for the number of bins
		sidebarLayout(
			sidebarPanel("Input Parameters",
			sliderInput("sens",
						"Sensitivity:",
						min = 0,
						max = 1,
						value = 0.3),
			sliderInput("spec",
						"Specificty:",
						min = 0,
						max = 1,
						value = 0.3),
			sliderInput("prev",
						"Prevalence:",
						min = 0,
						max = 1,
						value = 0.3)
			),

			# Show a plot of the generated distribution
			mainPanel(
				accuracyTableVisOutput("acc_table")
			)
		)
	)
  
	server <- function(input, output, session) {
		output$acc_table <- renderAccuracyTableVis({
			buildWidget("accuracy_table", data, settings)
		})
		observe({session$sendCustomMessage(type="handler", c(input$sens, input$spec, input$prev))})
	}
 
	shinyApp(ui, server)
}

#' @export
accuracyTableVisOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "accuracy_table", width, height, package = "healthvis")
}
#' @export
renderAccuracyTableVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, accuracyTableVisOutput, env, quoted = TRUE)
}