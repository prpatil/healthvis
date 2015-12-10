#' Create a interactive plot of a cox proportional hazards model
#'
#' \code{survivalVis} plots a cox proportional hazards model and uses
#' the covariate information to update the figure in real time. Users
#' can specify a grouping covariate to display a group comparison.
#' 
#'
#' @param cobj An object created with a call to \code{\link{coxph}}
#' @param data The dataset used to creat the coxph object. 
#' @param group The covariate in model by which to group. If NULL, no grouping.
#' @param group.names Names of groups to appear on legend. Default empty string for no grouping.
#' @param line.col Vector of names of colors to use for each group. Length must equal number of groups. Default single string for no grouping.
#' @param plot.title The title of the plot to appear on the HTML page
#' @export
#' @note Interaction terms are not fully supported currently. The figure will plot, but transitions may be inaccurate.
#' @examples
#' # Uses the survival package
#' library(survival)
#' # Set trt and prior as factors so they are treated as such
#' veteran$trt <- as.factor(veteran$trt)
#' veteran$prior <- as.factor(veteran$prior)
#' cobj <- coxph(Surv(time, status)~trt+age+celltype+prior, data=veteran)
#' survivalVis(cobj, data=veteran, plot.title="Veteran Survival Data", group="trt", group.names=c("Treatment", "No Treatment"), line.col=c("#E495A5","#39BEB1"))

survivalVis <- function(cobj, data, group=NULL, group.names="", line.col="deepskyblue", plot.title="Survival Plot",day.max=1000){
  
  if(class(cobj) != "coxph"){
    stop("Object not of class 'coxph'")
  }

  tmp.data <- data # In case we have to modify
  
  formula <- cobj$formula
  
  vars <- all.vars(formula[[3]]) # This is a list of all variables on RHS

  if(!is.null(group)){
	  if(!(group %in% vars)){
		stop("Grouping variable not in dataset")
	  }

	  vars <- setdiff(vars, group) # Remove grouping cat from variables for form
  }
  
  day.max <- max(data[[all.vars(formula[[2]])[1]]]) # This might need to be updated
  
  if(length(vars) > 10){
    stop("Number of covariates exceeds 10")
  }
  
  # Figure out what kind of menus are needed
  menu.type <- vector("character", length(vars))
  var.list <- list()
  ref.cats <- c()
  
  for(i in 1:length(vars)){
    cur <- tmp.data[[vars[i]]]
    if(class(cur) == "numeric" || class(cur) == "integer"){
      if(length(table(cur)) < 5){
        warning("Variable ", vars[[i]], " has fewer than 5 unique values; treating as continuous, but should this be a factor?")
      }
      menu.type[i] <- "continuous"
      var.list[[vars[i]]] <- range(cur, na.rm=T)
    } else if(class(cur) == "factor"){
      if(length(levels(cur)) > 10){
        stop("Variable ", vars[[i]], " has too many levels (>10).")
      }
      menu.type[i] <- "factor"
      var.list[[vars[i]]] <- levels(cur)
      ref.cats <- c(ref.cats, paste(vars[i], levels(cur)[1]))
    }
  }

  if(is.null(group)){

	 ds <- tmp.data[1,] # Take the first row of data
	 ds <- baseline(ds)
	 so <- survfit(cobj, newdata=ds)

 	 djs <- data_assign(so)
 
  } else {
        lvls <- unique(data[[group]])

        glen <- length(lvls)

	  if(length(group.names) != glen){
		stop("Number of group names does not match number of groups.")
	  }

	  if(length(line.col) != glen){
		stop("Number of line colors does not match number of groups.")
	  }

	  djs <- vector("character", glen)
	  for(i in 1:glen){
		ds <- tmp.data[1,]
		ds <- baseline(ds)
	      ds[[group]] <- lvls[i]
		so <- survfit(cobj, newdata=ds)
		djs[i] <- data_assign(so)
	  }

  }

  data <- paste("[", paste(djs, collapse=","), "]")
  
  c.sort <- c(cobj$coef, sapply(ref.cats, function(x){assign(x, 0)}))
  c.sort <- c.sort[sort(names(c.sort))]
  
  settings <- list(csort=c.sort, cnames=names(c.sort), vars=vars, menutype=menu.type, daymax=day.max, linecol=line.col, group_names=group.names)
  
  # Generate input elements for ui
 
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
				survivalVisOutput("survival")
			)
		)
		
	)
  
	server <- function(input, output, session) {

		output$survival <- renderSurvivalVis({
			buildWidget("survival", data, settings)
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
survivalVisOutput <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "survival", width, height, package = "healthvis")
}
#' @export
renderSurvivalVis <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, survivalVisOutput, env, quoted = TRUE)
}

baseline <- function(ds){
  for(i in 1:length(ds)){
    if(class(ds[1,i]) == "factor"){
      ds[1,i] <- levels(ds[1,i])[1]
    } else {
      ds[1,i] <- 0
    }
  }
  ds
}

data_assign <- function(so){
  data <- cbind(so$time, -log(so$surv))
  colnames(data) <- c("time", "haz")
  data <- apply(data, 1, as.list)
  rjson::toJSON(data)
}