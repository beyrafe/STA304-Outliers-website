# ==============================================================================
# STA304 Outliers: survey website
#
# Entry point of the Shiny app. Run it with "Run App" in RStudio, or
# shiny::runApp() from the project folder. Posit Connect Cloud uses this file
# as the primary file.
#
# Shiny sources every file in R/ before it runs this one, so the graph modules
# (careerUI(), personalServer(), ...) and the layout functions from R/layout.R
# are already defined here. No source() calls are needed.
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)

DATA_FILE <- "STA304_Outliers_Data_Collection - Form Responses 1.csv"


# ---- Data --------------------------------------------------------------------
# The raw form responses. The four POSt groups (post_group) are added by the
# filter module, and each graph module computes its own scores. If
# R/data_prep.R ever defines prepare_data(raw), it is applied here first.
survey <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
if (exists("prepare_data", mode = "function")) {
  survey <- prepare_data(survey)
}


# ---- Demographic filter (R/filters.R) ----------------------------------------
#   filtersUI(id)            the gender identity / POSt controls
#   filtersServer(id, data)  returns a reactive data frame: the rows of `data`
#                            that match the controls, with post_group added
# Without that file the site runs unfiltered and says so in the sidebar.
has_filters <- exists("filtersUI", mode = "function") &&
  exists("filtersServer", mode = "function")


# ---- App ---------------------------------------------------------------------
ui <- layoutUI(
  survey,
  filters_ui = if (has_filters) filtersUI("filters")
)

server <- function(input, output, session) {
  filtered_data <- if (has_filters) {
    filtersServer("filters", survey)
  } else {
    reactive(survey)
  }

  # Every graph module gets the same filtered data.
  start_graph_modules(filtered_data)

  layoutServer(input, output, session, survey, filtered_data)
}

shinyApp(ui, server)
