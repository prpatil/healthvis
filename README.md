# healthvis
Interactive health visualizations in R

This package offers a few d3 visualizations which can be applied to different forms of health data: survival, heatmap matricies, sensitivity/specificity, etc. 
The visualizations are written in [d3](http://d3js.org/), which is rendered through [htmlWidgets](http://www.htmlwidgets.org/). All of this is contained in a [shiny](http://shiny.rstudio.com/) 
application which provides form input to update the visualizations based on covariate/sorting/grouping choices. The future goal is to streamline the conversion 
process from a d3 figure that relies on form input to a healthvis figure by generalizing the structure.

# Installation

- Have devtools installed
- install_github("prpatil/healthvis")

# Dependencies

shiny, htmlWidgets, rjson

# Author

Prasad Patil