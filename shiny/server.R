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
library(highcharter)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

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

genres <- unique(unlist(str_split(df_anime$genre, ", ")))
bar_genres = table("Genre" = genres) %>% as.data.frame() %>% arrange(-Freq)


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
  
  output$nb_tv_box <- renderValueBox({
    valueBox(
      paste(format(nrow(df_anime %>% filter(type == 'TV'))), 'TV animes'), icon = icon("play"),
      "Number of anime series"
    )
  })
  
  output$nb_movie_box <- renderValueBox({
    valueBox(
      paste(format(nrow(df_anime %>% filter(type == 'Movie'))), 'Movies'), icon = icon("film"),
      "Number of anime movies"
    )
  })
  
  output$nb_genres <- renderValueBox({
    valueBox(
      length(unique(unlist(str_split(df_anime$genre, ", ")))), icon = icon("list"),
      color = "yellow", "Number of genres"
    )
  })
  
  output$nb_genres <- renderValueBox({
    valueBox(
      length(unique(unlist(str_split(df_anime$genre, ", ")))), icon = icon("list"),
      color = "yellow", "Number of genres"
    )
  })
  
  # Pie chart type
  output$pie_type <- renderHighchart({
    data_pie_type <- df_anime %>% 
      filter(type != "Unknown") %>% 
      group_by("Type" = type) %>% 
      summarise(Freq = n())
    
    highchart() %>% 
      hc_add_series( data_pie_type, hcaes(x = Type,y = Freq, color = Type), type = "pie") %>%
      hc_tooltip(borderWidth = 1.5, headerFormat = "",
                 pointFormat = paste("<b>Type: {point.Type}</b> ({point.percentage:.1f}%)<br><b>Count:</b> {point.y}"))
  })
  
  
  # Bubble chart rang et popularité
  output$bbl_chart_pop <- renderPlotly({
    df_anime[df_anime$popularity != 0] %>% 
      arrange(rank) %>% head(n = 100) %>%
      select(popularity, rank, title, scored_by, favorites, score) %>%
      filter(popularity <= 100) %>%
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
        breaks = seq(0, 100, 25)
      ) +
      labs(x = 'Rank', y = 'Popularity')
  })
  
  # Point plot rang et popularité
  output$point_plot_pop <- renderPlotly({
    df_anime[df_anime$popularity != 0] %>%
      arrange(popularity) %>% head(n = 100) %>% 
      select(title, popularity, rank) %>%
      ggplot(aes(x = popularity, y = rank)) +
      geom_line(color = 'black') +
      geom_point(size = 2, color = 'red') +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(-1, 101),
        breaks = seq(0, 100, 5)
      ) +
      scale_y_continuous(
        breaks = seq(0, 3000, 500),
        limits = c(0, 3000)
      )
  })
  
  # Line chart nombre d'anime par année
  output$line_nb_year <- renderPlotly({
    date_data %>% 
      filter(!is.na(year)) %>% 
      group_by("Year" = year) %>% 
      summarise(Freq = n()) %>%
      ggplot(aes(x = Year, y = Freq)) +
      geom_line(color = 'red', size = 1) +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(1910, 2017),
        breaks = seq(1910, 2017, 25)
      ) +
      scale_y_continuous(
        breaks = seq(0, 1000, 250),
        limits = c(0, 1000)
      )
  })
  
  
  # Line chart moyenne notes TV par année
  output$score_year_tv <- renderPlotly({
    date_data %>% 
      filter(score != 0, scored_by > 10, type == 'TV') %>% 
      group_by("Year" = year) %>% 
      summarise(avg = round(mean(score, na.rm = T), 2)) %>%
      ggplot(aes(x = Year, y = avg)) +
      geom_line(color = 'red', size = 1) +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(1960, 2017),
        breaks = seq(1960, 2017, 10)
      ) +
      scale_y_continuous(
        breaks = seq(6, 7.5, 0.2),
        limits = c(6, 7.5)
      )
  })
  
  # Line chart moyenne notes films par année
  output$score_year_movie <- renderPlotly({
    date_data %>% 
      filter(score != 0, scored_by > 10, type == 'Movie') %>% 
      group_by("Year" = year) %>% 
      summarise(avg = round(mean(score, na.rm = T), 2)) %>%
      ggplot(aes(x = Year, y = avg)) +
      geom_line(color = 'red', size = 1) +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(1930, 2017),
        breaks = seq(1930, 2017, 25)
      ) +
      scale_y_continuous(
        breaks = seq(4.5, 7.5, 0.5),
        limits = c(4.5, 7.5)
      )
  })
  
  # Evolution de la classification TV au fil des décennies
  output$ratings_tv_decade <- renderPlotly({
    date_data %>%
      filter(!is.na(decade), type == "TV") %>% 
      group_by("Decade" = decade, rating, .drop = F) %>% 
      summarise(Freq = n()) %>%
      ggplot(aes(x = Decade, y = Freq, group = rating, shape = rating, color = rating)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_y_continuous(
        breaks = seq(0, 1250, 250),
        limits = c(0, 1250)
      )
  })
  
  # Evolution de la classification des films au fil des décennies
  output$ratings_movie_decade <- renderPlotly({
    date_data %>%
      filter(!is.na(decade), type == "Movie") %>% 
      group_by("Decade" = decade, rating, .drop = F) %>% 
      summarise(Freq = n()) %>%
      ggplot(aes(x = Decade, y = Freq, group = rating, shape = rating, color = rating)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      theme_ipsum() +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      scale_y_continuous(
        breaks = seq(0, 300, 50),
        limits = c(0, 300)
      )
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
      scale_y_continuous(limits = c(0, 10))
  })
  
  
  # Boxplot score par type
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
      scale_y_continuous(limits = c(0, 10))
  })
  
  # Boxplot age utilisateurs par genre
  output$box_gender_age <- renderPlotly({
    users %>% 
      ggplot(aes(x = gender, y = age, fill = gender)) +
      geom_boxplot() +
      theme_ipsum() +
      theme(
        legend.position='none',
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14)
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(-1, 60),
        breaks = seq(0, 60, 5)
      )
  })
  
  # Boxplot temps de visio utilisateurs par genre
  output$box_gender_spent <- renderPlotly({
    users %>%
      ggplot(aes(x = gender, y = user_days_spent_watching, fill = gender)) +
        geom_boxplot() +
        theme_ipsum() +
        theme(
          legend.position = 'none',
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14)
        ) +
        scale_y_continuous(
          expand = c(0, 0),
          limits = c(-1, 301),
          breaks = seq(0, 300, 50)
        )
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
      )
  })
  
  # Graphe densité de la distribution des notes par décennie (TV animes)
  output$score_density_dec <- renderPlotly({
    date_data %>% filter(decade %in% input$checkGroup2) %>% select(decade, type, score) %>% 
      filter(!is.na(decade), type == 'TV', score != 0) %>% 
      ggplot(aes(score, group = decade, fill = decade), height = 700) +
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
      )
  })
  
  # Graphe densité de la distribution des notes par décennie (films)
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
      )
  })
  
  # Nuage de points age utilisateurs
  output$scatter_age <- renderPlotly({
    users %>%
      ggplot(aes(x = age, y = user_days_spent_watching, color = gender)) +
      stat_bin_hex(bins = 75, alpha = 0.6)  +
      scale_color_manual(values= c('red', 'blue')) +
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
      )
  })
  
  
  # Liste anime
  output$data_anime <- DT::renderDataTable({
    df_anime %>%
      select(rank, title, type, episodes, aired_string, duration, popularity, score, scored_by, studio)
  })
  
  # Liste utilisateurs
  output$data_users <- DT::renderDataTable({ users })
  
  # Top anime populaires
  output$top_anime_pop <- DT::renderDataTable({
    df_anime %>%
      select(title, popularity, rank, score) %>%
      filter(popularity != 0) %>% 
      arrange(popularity)
  })
  
  # Top anime score
  output$top_anime_score <- DT::renderDataTable({
    df_anime %>%
      select(title, rank, score, popularity) %>%
      filter(rank != 0) %>% 
      arrange(rank)
  })
  
  output$anime_downloadCSV <- downloadHandler(
    filename <- "anime_data.csv",
    content <- function(file) {
      write.csv(df_anime, file)
    },
    contentType = "text/csv"
  )
  
  output$users_downloadCSV <- downloadHandler(
    filename <- "users_data.csv",
    content <- function(file) {
      write.csv(df_users, file)
    },
    contentType = "text/csv"
  )
  
  
})