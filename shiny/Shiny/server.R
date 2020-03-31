# Chargement des librairies
library(tidyverse)
library(knitr)
library(rmarkdown)
library(markdown)
library(data.table)
library(plotly)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(shiny)
library(shinydashboard)

# Chargement des dataset
df_anime <- fread(file='../../datasets/anime_filtered.csv')
df_users <- fread(file='../../datasets/users_filtered.csv')

# Création des variables necessaire au premier graph
users <- df_users[df_users$gender %in% c('Female','Male') & !is.null(df_users$birth_date)] %>% 
    select(username, gender, user_completed, user_days_spent_watching, birth_date)
users$age <- as.period(interval(start=users$birth_date, end=as.Date(now())))$year

# Création des variables necessaire au second graph
date_data <- df_anime %>% 
  rowwise() %>% 
  mutate(aired = str_extract_all(aired, "(None|[0-9]*-[0-9]+-[0-9]+)")[[1]][1]) %>% 
  ungroup() %>%
  mutate(
    year = as.integer(if_else(nchar(aired) == 10, substr(aired, 1, 4), NULL)),
    month = as.integer(if_else(nchar(aired) == 10, substr(aired, 6, 7), NULL)),
    season = if_else(month %in% c(1:3), 'Winter',
                     if_else(month %in% c(4:6), 'Spring',
                             if_else(month %in% c(7:9), 'Summer', 
                                     if_else(month %in% c(10:12), 'Autumn', NULL)))),
    year_season = paste(year, season),
    decade = as.factor(if_else(!is.na(year), 
                               paste(substr(year, 1, 3), "0's", sep = ''), NULL))
  ) %>% 
  arrange(year, month)



shinyServer(function(input, output) {
    # Création du premier graph
    output$plot1 <- renderPlot({
        users %>% filter(gender %in% input$checkGroup1) %>%
            ggplot(aes(x = age, y = user_days_spent_watching, color = gender)) +
            geom_point(alpha = 0.5) +
            theme_ipsum() +
            theme(
                legend.position = 'none',
                axis.title.x = element_text(size=14),
                axis.title.y = element_text(size=14),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()
            ) +
            scale_x_continuous(
                expand = c(0, 0),
                limits = c(-1, 81),
                breaks = seq(0, 80, 5)
            ) +
            scale_y_continuous(
                expand = c(0, 0),
                limits = c(-1, 1001),
                breaks = seq(0, 1000, 200)
            ) +
            ggtitle("Anime watchers' age")
    })
    
    # Création du second graph
    output$plot2 <- renderPlot({
      date_data %>% filter(decade %in% input$checkGroup2) %>% select(decade, type, score) %>% 
        filter(!is.na(decade), type == 'TV', score != 0) %>% 
        ggplot(aes(score, group = decade, fill = decade)) +
        geom_density(adjust = 1.25, alpha = .75) +
        theme_ipsum() +
        theme(
          legend.position='top',
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
        ) +
        scale_x_continuous(
          limits = c(0, 10),
          breaks = seq(0, 10, 2.5)
        ) +
        scale_y_continuous(
          expand = c(0, 0),
          limits = c(0, 1),
          breaks = seq(0, 1, 0.25)
        ) +
        ggtitle('TV anime score density by decade')
    })

})
