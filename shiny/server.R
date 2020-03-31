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
df_anime <- fread(file='../datasets/anime_filtered.csv')
df_users <- fread(file='../datasets/users_filtered.csv')

# Colonne age des utilisateurs
users <- df_users[df_users$gender %in% c('Female', 'Male') & !is.null(df_users$birth_date)] %>% 
  select(username, gender, user_completed, user_days_spent_watching, birth_date)

users$age <- as.period(interval(start = users$birth_date, end = as.Date(now())))$year

# Dataset et nouvelles colonnes pour le graphe de densité par décennie
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

# Valeur médiane pour les boxplots
median_val <- df_anime %>% 
  select(scored_by, score) %>% 
  filter(scored_by > 99) %>% 
  summarize(median_sc = median(score))


shinyServer(function(input, output) {
  
  output$nb_box <- renderValueBox({
    valueBox(
      nrow(df_anime), icon = icon("database"),
      "Animes (TV, movies, ONA, OVA, etc.)"
    )
  })
  
  # Bubble chart rang et popularité
  output$bbl_chart_pop <- renderPlotly({
    df_anime[df_anime$popularity != 0] %>% 
      arrange(rank) %>% head(n = 100) %>%
      select(popularity, type, rank, title, scored_by, favorites, score) %>%
      filter(popularity <= input$slider_popularity, type == 'TV') %>%
      mutate(point = (as.numeric(scored_by) * as.numeric(favorites) * as.numeric(score)) / 10^10) %>% 
      ggplot(aes(x = rank, y = popularity, size = point, color = popularity, text = title)) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(1.4, 19)) +
      scale_color_viridis() +
      theme_ipsum() +
      theme(
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 10)
      ) +
      scale_y_continuous(
        limits = c(-5, 100),
        breaks = seq(0, 100, 10)
      ) +
      labs(title = 'In Terms Of Rank And Popularity TOP 100 Animes', x = 'Rank', y = 'Popularity')
  })
  
  # Point plot rang et popularité
  output$point_plot_pop <- renderPlotly({
    df_anime[df_anime$popularity != 0] %>%
      arrange(popularity) %>% head(n = input$slider_popularity) %>% 
      select(title, popularity, type, rank) %>%
      filter(type == "TV") %>%
      ggplot(aes(x = popularity, y=rank)) +
      geom_line(color = 'black') +
      geom_point(size = 2, color = 'red') +
      theme_ipsum() +
      theme(axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(-1, 101),
        breaks = seq(0, 100, 5)
      ) +
      scale_y_continuous(
        breaks = seq(0, 3000, 500),
        limits = c(0, 3000)
      ) +
      ggtitle('Top 100 anime with their rank score')
  })
  
  # Boxplot score par source
  output$score_source <- renderPlotly({
    df_anime %>% 
      filter(airing == F & scored_by > 99) %>%
      select(score, scored_by, source) %>%
      ggplot(aes(x = source, y = score)) +
      geom_boxplot(fill = 'deepskyblue') + 
      geom_hline(aes(yintercept = median_val[[1]], linetype = 'median')) +
      scale_linetype_manual(name = '', values = c(median = 'dashed')) +
      coord_flip() + 
      theme_ipsum() +
      theme(
        axis.title.x = element_text(size=14),
        axis.title.y = element_blank(),
      ) +
      scale_y_continuous(limits = c(0, 10)) +
      ggtitle('Score Distribution by Source')
  })
  
  
  # Boxplot sore par type
  output$score_type <- renderPlotly({
    df_anime %>% 
      filter(airing == F & scored_by > 99) %>%
      select(score, scored_by, type) %>%
      ggplot(aes(x = type, y = score)) +
      geom_boxplot(fill = 'deepskyblue') + 
      geom_hline(aes(yintercept = median_val[[1]], linetype = 'median')) +
      scale_linetype_manual(name = '', values = c(median = 'dashed')) +
      coord_flip() + 
      theme_ipsum() +
      theme(
        axis.title.x = element_text(size=14),
        axis.title.y = element_blank(),
      ) +
      scale_y_continuous(limits = c(0, 10)) +
      ggtitle('Score Distribution by Type')
  })
  
  # Relation entre le score et le nombre de notes attribuées
  output$rs_score_notes <- renderPlotly({
    df_anime %>%
      filter(airing == F & scored_by > 99) %>%
      ggplot(aes(x = scored_by, y = score)) +
      stat_bin_hex(bins = 50) +
      theme_ipsum() +
      scale_fill_viridis() +
      stat_smooth(
        method = 'lm',
        color = 'red',
        formula = y ~ log(x)
      ) +
      theme(
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14)
      ) +
      scale_y_continuous(
        limits = c(2, 9.5),
        breaks = seq(2, 9.5)
      ) +
      ggtitle('Relationship between Score and Scored_by')
  })
  
  # Graphe densité de la distribution des notes par décennie
  output$score_density_dec <- renderPlotly({
    date_data %>% filter(decade %in% input$checkGroup2) %>% select(decade, type, score) %>% 
      filter(!is.na(decade), type == 'TV', score != 0) %>% 
      ggplot(aes(score, group = decade, fill = decade)) +
      geom_density(adjust = 1.25, alpha = .7) +
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
  
  # Graphe densité de la distribution des notes par décennie
  output$score_density_movie <- renderPlotly({
    date_data %>% filter(decade %in% input$checkGroup3) %>% select(decade, type, score) %>% 
      filter(!is.na(decade), type == 'Movie', score != 0) %>% 
      ggplot(aes(score, group = decade, fill = decade)) +
      geom_density(adjust = 1.25, alpha = .7) +
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
      ggtitle('Movies score density by decade')
  })
  
  # Liste anime
  output$data_anime <- DT::renderDataTable({
    df_anime %>%
      select(rank, title, type, episodes, aired_string, duration, popularity, score, scored_by, studio)
  })
  
  # Liste utilisateurs
  output$data_users <- DT::renderDataTable({ users })
  
})