
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinycssloaders)

fluidPage(sidebarLayout(
  shiny::sidebarPanel(
    selectizeInput("coach", "Coach", choices = NULL)
  ),
  shiny::mainPanel(withSpinner(plotOutput("coach_history"), type = 8, color = "#333333"),DT::dataTableOutput("ranking_table"))
))
