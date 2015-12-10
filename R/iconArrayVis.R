#' Create an icon array.
#'
#' \code{iconArrayVis} creates an interactive icon array. Allows for specification of a multinomial
#' logit regression to estimate probability of each group based on a set of covariates. This concept is
#' based on research by Zikmund-Fisher et. al. (http://www.iconarray.com/)
#' 
#' @param mobj Result of multinomial logit regression. Defaults to NULL, meaning groups are manually specified.
#' @param colors List of colors to use for each group. Should be length of levels of outcome or length of group.names.
#' @export
#' @examples
#' # To create a simple icon array image
#' iconArrayVis()
#' # To display the results of a multinomial logit (quine data from MASS package)
#' library(MASS)
#' library (nnet) # contains function multinom
#' mobj <- multinom(Age~Eth+Sex+Lrn+Days, data=quine)
#' iconArrayVis(mobj, data=quine, colors=c("deepskyblue", "orangered"), plot.title="School Absenteeism")


iconArrayVis <- function(mobj=NULL, data=NULL, group.colors=c("deepskyblue", "orangered"), init.color="lightgray", plot.title="Icon Array"){

		obj.flag = 1
		if(is.null(data)){
			stop("Please provide the dataset used for the regression.")
		}
		
		coefs <- summary(mobj)$coefficients
		row.num <- dim(coefs)[1]
		col.num <- dim(coefs)[2]
		cats <- colnames(coefs)

		grouping.var <- all.vars(mobj$terms)[1]
		vars <- all.vars(mobj$terms)[-1] # Ignore the outcome

	    menu.type <- vector("character", length(vars))
		var.list <- list()
#		ref.cats <- c()
  
		for(i in 1:length(vars)){
			cur <- data[[vars[i]]]
			print(class(cur))
			if(class(cur) == "numeric" || class(cur) == "integer"){
				if(length(table(cur)) < 5){
					warning("Variable ", vars[[i]], " has fewer than 5 unique values; treating as continuous, but should this be a factor?")
				}
				menu.type[i] <- "continuous"
				var.list[[vars[i]]] <- range(cur)
			} else if(class(cur) == "factor"){
				if(length(levels(cur)) > 10){
					stop("Variable ", vars[[i]], " has too many levels (>10).")
				}
				menu.type[i] <- "factor"
				var.list[[vars[i]]] <- levels(cur)
				#ref.cats <- c(ref.cats, paste(vars[i], levels(cur)[1], sep=""))
			}
		}

		gr.l <- var.list
		varType <- menu.type
		group.names <- levels(data[[grouping.var]])
		if(length(group.colors) != length(group.names)){
			print(grouping.var)
			stop("Not enough colors for the categories of your outcome.")
		}
	
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


	 data <- ""
	 settings <- list(obj_flag = obj.flag, color_array=rep(init.color, 100), group_colors=group.colors, group_names=group.names, coefs=coefs, rows=row.num, cols=col.num, cats=cats, vtype=varType)
  
	  ui <- fluidPage(

		# Application title
		titlePanel(plot.title),

		# Sidebar
		sidebarLayout(
			sidebarPanel("Covariates", inputlist),

			# Show a plot of the generated distribution
			mainPanel(
				iconArrayVisOutput("icon_array")
			)
		)
		
	)
  
	server <- function(input, output, session) {

		output$icon_array <- renderIconArrayVis({
			buildWidget("icon_array", data, settings)
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
iconArrayVisOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "icon_array", width, height, package = "healthvis")
}
#' @export
renderIconArrayVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, iconArrayVisOutput, env, quoted = TRUE)
}


