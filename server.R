
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(nufflytics)
library(shinycssloaders)

ratings <- readRDS("data/final_rating.rds")
CCL_dates <- readRDS("data/CCL_dates.rds")

as.data.frame.player <- function(x) {
  data.frame(
    name = attr(x, 'name'), 
    rating = x$rating,
    deviation = x$deviation,
    volatility = x$volatility,
    tot_games = x$tot_games,
    period_games = x$period_games,
    inactivity = x$inactivity,
    stringsAsFactors = F
  )
}

as_data_frame.player <- function(x) {
  as_data_frame(as.data.frame(x))
}
ratings_table <- ratings$rating %>% map_df(as_data_frame) %>% mutate(ranking = rating - deviation) %>% arrange(desc(ranking)) %>% select(name, ranking, rating, deviation, tot_games, inactivity)

deviation_density <- function(d) {
  d <- mutate(d, rating = case_when(inactivity > 30 ~ NA_real_, T~rating)) %>%
    filter(!is.na(rating))
  
  data_frame(name = d$name, period = d$period, rating = d$rating, deviation = d$deviation, y = map2(d$rating, d$deviation, ~seq(.x-.y*3, .x+.y*3, length.out = 200))) %>% 
    unnest %>% 
    mutate(dens = dnorm(y, mean=rating, sd=deviation)) %>% 
    group_by(period,name)
}

filter_inactive <- function(d) {
  mutate(d, rating = case_when(inactivity > 30 ~ NA_real_, T~rating))
}

filter_unplayed <- function(d) filter(d, period_games > 0)

CCL_backdrop <- function() {
  list(
    geom_rect(data = CCL_dates, aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf, fill = as.numeric(CCL)%%2==0), alpha = 0.2, inherit.aes = F),
    geom_text(data = CCL_dates, aes(x=lubridate::as_date(min + lubridate::as.duration(max-min)/2), y = Inf, label = CCL), vjust = 2),
    scale_fill_brewer(palette="Set1", guide = "none")
  )
}

plot_coach <- function(coach) {
  rating <- ratings$rating[[coach]]
  
  historical_ratings <- ratings$history %>% 
    filter(name %in% coach, tot_games > 10) %>% 
    mutate(period = lubridate::as_date(period))
  
  historical_ratings %>% 
    ggplot(aes(x = period, y = rating)) +
    CCL_backdrop() +
    geom_tile(data = deviation_density, aes(y=y, alpha = dens, height = (max(y)-min(y))/60)) +
    geom_line(data = filter_inactive, colour = "white") +
    geom_rug(data = filter_unplayed, aes(colour = period_games), sides = "b") +
    scale_color_viridis_c("Games Played") +
    scale_alpha_continuous(range = c(0, 0.2), guide = "none") +
    coord_cartesian(xlim = range(historical_ratings$period)) +
    xlab(NULL) + 
    ylab("Rating") + 
    ggtitle(coach, glue::glue("Rating: {round(rating$rating, digits=2)}, Deviation: {round(rating$deviation, digits=2)}"))
}




shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "coach", choices = ratings_table$name)
  
  output$coach_history <- renderPlot({
    plot_coach(input$coach) + theme_nufflytics()
  })
  
  output$ranking_table <- DT::renderDataTable(DT::datatable(
    ratings_table,
    rownames = F,
    colnames = c("Coach", "Ranking", "Rating", "Rating Deviation", "Games Played", "Days Inactive"),
    selection = "none",
    filter = "top",
    escape = F,
    class = "display compact"
  ) %>% DT::formatRound(2:4))
  
})
