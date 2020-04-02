library(shiny)
library(shinydashboard)
# library(shinydashboardPlus)

shinyUI(fluidPage(theme = "bootstrap.css",
    ui <- dashboardPage(
        
        # Header avec titre et sidebar
        dashboardHeader(title = "MyAnimeList"),
        sidebar <- dashboardSidebar(
            sidebarMenu(
                style = "overflow: visible;",
                menuItem("Overview", tabName = "overview", icon = icon("eye")),
                menuItem("TV Anime", tabName = "tv_anime", icon = icon("tv")),
                menuItem("Movies", tabName = "movies", icon = icon("film")),
                menuItem("Users", tabName = "users", icon = icon("user")),
                menuItem("Data", tabName = "list_anime", icon = icon("table"))
            )
        ),
        
        # Body
        dashboardBody(
            tabItems(
                
                tabItem(tabName = "overview",
                        
                        # Value box
                        fluidRow(
                            valueBoxOutput("nb_box"),
                            valueBoxOutput("nb_genres")
                        ),
                        
                        fluidRow(
                            box(
                                title = tagList(shiny::icon("chart-line"), " Number of anime per year"),
                                width = 6,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("line_nb_year")
                            ),
                            box(
                                title = tagList(shiny::icon("chart-pie"), " Anime type repartition"),
                                width = 6,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("pie_type")
                            ),
                        ),
                        
                        # Top popularité et score
                        fluidRow(
                            box(
                                title = tagList(shiny::icon("fire"), " Most popular anime"),
                                status = "primary",
                                solidHeader = TRUE,
                                DT::dataTableOutput("top_anime_pop"),
                            ),
                            
                            box(
                                title = tagList(shiny::icon("heart"), " Most liked anime"),
                                status = "primary",
                                solidHeader = TRUE,
                                DT::dataTableOutput("top_anime_score")
                            )
                        ),
                        
                        # Affichage du graphe de relation entre le score et le nombre de notes attribuées
                        fluidRow(
                            box(
                                title = tagList(shiny::icon("chart-scatter"), " Relationship between Score and Scored_by"),
                                width = 12,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("rs_score_notes")
                            ),
                            
                            
                        ),
                        
                        # Boxplots
                        fluidRow(
                            # Score par source
                            box(
                                title = "Score Distribution by Source",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("score_source")
                            ),
                            
                            # Score par type
                            box(
                                title = "Score Distribution by Type",
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("score_type")
                            )
                        ),
                        
                ),
                
                tabItem(tabName = "tv_anime", 
                        
                    # Value box
                    fluidRow(
                        valueBoxOutput("nb_tv_box")
                    ),
                    
                    # Score moyen par année + classification par décennie
                    fluidRow(
                        # Score moyen par année
                        box(
                            title = tagList(shiny::icon("chart-line"), " Average score per year"),
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("score_year_tv")
                        ),
                        
                        # Classification par décennie
                        box(
                            title = tagList(shiny::icon("chart-line"), " Ratings evolution during decades"),
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("ratings_tv_decade")
                        ),
                        
                    ),
    
                    # Bubble chart, point plot + panel controles
                    fluidRow(
                        # Affichage du bubble chart et du point plot via 2 onglets
                        tabBox(
                            title = "TOP 100 anime", width = 8,
                            tabPanel(tagList(shiny::icon("chart-scatter"), "Bubble chart"), plotlyOutput("bbl_chart_pop")),
                            tabPanel(tagList(shiny::icon("chart-line"), "Line plot"), plotlyOutput("point_plot_pop"))
                        ),
                        
                        # Slider input pour modifier les graphes en fonction de la popularité
                        box(
                            title = tagList(shiny::icon("gear"), "Controls"),
                            width = 4,
                            status = "primary",
                            solidHeader = TRUE,
                            "Popularity:", # br(), "More box content",
                            sliderInput("slider_popularity", "Slider input:", 1, 100, 100, 10)
                        )
                    ),
                        
                    
                    fluidRow(
                        # Graphe densité de la distribution des notes par décennie
                        box(
                            title = tagList(shiny::icon("chart-area"), " TV anime score density by decade"),
                            width = 8,
                            status = "warning",
                            collapsible = TRUE,
                            plotlyOutput("score_density_dec")
                        ),
                        
                        # Checkboxes pour chaque décennie
                        box(
                            title = tagList(shiny::icon("gear"), "Controls"),
                            width = 4,
                            status = "warning",
                            solidHeader = TRUE,
                            "Choose decade to show", # br(), "More box content",
                            checkboxGroupInput(
                                "checkGroup2", 
                                "Checkbox group:",
                                choices = list(
                                    "1960's" = "1960's",
                                    "1970's" = "1970's", 
                                    "1980's" = "1980's", 
                                    "1990's" = "1990's", 
                                    "2000's" = "2000's",
                                    "2010's" = "2010's"
                                ),
                                selected = c("1960's","1970's","1980's","1990's","2000's","2010's")
                            )
                        )
                    ),
                
                ),
                
                
                tabItem(tabName = "movies",
                        
                        # Value box
                        fluidRow(
                            valueBoxOutput("nb_movie_box")
                        ),
                        
                        # Score moyen par année + classification par décennie
                        fluidRow(
                            # Score moyen par année
                            box(
                                title = tagList(shiny::icon("chart-line"), " Average score per year"),
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("score_year_movie")
                            ),
                            
                            # Classification par décennie
                            box(
                                title = tagList(shiny::icon("chart-line"), " Ratings evolution during decades"),
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("ratings_movie_decade")
                            ),
                            
                        ),
                        
                        # Graphe densité score par décennie + panel controles
                        fluidRow(
                            # Graphe densité de la distribution des notes par décennie
                            box(
                                title = tagList(shiny::icon("chart-area"), " Movies score density by decade"), 
                                width = 8, 
                                status = "success", 
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("score_density_movie")
                            ),
                        
                        # Checkboxes pour chaque décennie
                            box(
                                title = tagList(shiny::icon("gear"), "Controls"),
                                width = 4,
                                status = "success",
                                solidHeader = TRUE,
                                "Choose decade to show", # br(), "More box content",
                                checkboxGroupInput(
                                    "checkGroup3", 
                                    "Checkbox group:",
                                    choices = list(
                                        "1910's" = "1910's",
                                        "1920's" = "1920's", 
                                        "1930's" = "1930's", 
                                        "1940's" = "1940's", 
                                        "1950's" = "1950's",
                                        "1960's" = "1960's",
                                        "1970's" = "1970's", 
                                        "1980's" = "1980's", 
                                        "1990's" = "1990's", 
                                        "2000's" = "2000's",
                                        "2010's" = "2010's"
                                    ),
                                    selected = c("1910's", "1920's", "1930's", "1940's", "1950's", 
                                                 "1960's", "1970's", "1980's", "1990's", "2000's","2010's")
                                )
                            )
                        
                        )
                ),
                
                
                
                tabItem(tabName = "users",
                        
                        # Boxplots
                        fluidRow(
                            tabBox(
                                title = "Age and days spent by gender", width = 4,
                                tabPanel("Age", plotlyOutput("box_gender_age")),
                                tabPanel("Days spent", plotlyOutput("box_gender_spent"))
                            ),
                            
                            # Nuage de points age utilisateurs
                            box(
                                title = tagList(shiny::icon("chart-scatter"), " Anime watchers' age"),
                                width = 8,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("scatter_age")
                            ),
                        ),
                ),
                
            
        
                tabItem(tabName = "list_anime",
                    # 2 onglets, 1 pour chaque liste à afficher (animes, utilisateurs)
                    fluidRow(
                        tabBox(
                            title = "Data", width = 12,
                            tabPanel(tagList(shiny::icon("list"), "Anime list"), DT::dataTableOutput("data_anime")),
                            tabPanel(tagList(shiny::icon("list"), "User list"), DT::dataTableOutput("data_users"))
                        )
                    )
                )
                
            )
            
        )
    )
))