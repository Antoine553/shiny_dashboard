library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    
    # Header avec titre et sidebar
    dashboardHeader(title = "MyAnimeList dashboard", style = "position: fixed; overflow: visible;"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            style = "position: fixed; overflow: visible;",
            menuItem("TV Anime", tabName = "tv_anime", icon = icon("tv")),
            menuItem("Movies", tabName = "movies", icon = icon("film")),
            menuItem("List of anime", tabName = "list_anime", icon = icon("table"))
        )
    ),
    
    # Body
    dashboardBody(
        tabItems(
            tabItem(tabName = "tv_anime", 
                h2("Dashboard tab content"),
                
                # Value box
                fluidRow(
                    valueBoxOutput("nb_box")
                ),
                
                # Bubble chart, point plot + panel controles
                fluidRow(
                    # Affichage du bubble chart et du point plot via 2 onglets
                    tabBox(
                        title = "TOP 100 anime", width = 8,
                        tabPanel("Bubble chart", plotlyOutput("bbl_chart_pop")),
                        tabPanel("Line plot", plotlyOutput("point_plot_pop"))
                    ),
                    
                    # Slider input pour modifier les graphes en fonction de la popularité
                    box(
                        title = "Controls",
                        width = 4,
                        status = "warning",
                        solidHeader = TRUE,
                        "Popularity:", # br(), "More box content",
                        sliderInput("slider_popularity", "Slider input:", 1, 100, 100, 10)
                    )
                ),
                
                # Affichage du graphe de relation entre le score et le nombre de notes attribuées
                fluidRow(
                    box(
                        title = "plot",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("rs_score_notes")
                    )
                ),
                
                # Boxplots
                fluidRow(
                    # Score par source
                    box(
                        title = "plot",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("score_source")
                    ),
                    
                    # Score par type
                    box(
                        title = "plot",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("score_type")
                    )
                ),
                
                
                # Graphe densité score par décennie + panel controles
                fluidRow(
                    # Graphe densité de la distribution des notes par décennie
                    box(
                        title = "Title plot 2", 
                        width = 8, 
                        status = "primary", 
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotlyOutput("score_density_dec")
                    ),
                    
                    # Checkboxes pour chaque décennie
                    box(
                        title = "Controls",
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
                    h2("Movies dashboard"),
                    
                    # Graphe densité score par décennie + panel controles
                    fluidRow(
                        # Graphe densité de la distribution des notes par décennie
                        box(
                            title = "Title plot 2", 
                            width = 8, 
                            status = "primary", 
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            plotlyOutput("score_density_movie")
                        ),
                    
                    # Checkboxes pour chaque décennie
                        box(
                            title = "Controls",
                            width = 4,
                            status = "warning",
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
            
            
    
            tabItem(tabName = "list_anime",
                h2("List of anime"),
                
                # 2 onglet, 1 pour chaque liste à afficher (animes, utilisateurs)
                fluidRow(
                    tabBox(
                        title = "Data", width = 12,
                        tabPanel("Anime list", DT::dataTableOutput("data_anime")),
                        tabPanel("User list", DT::dataTableOutput("data_users"))
                    )
                )
            )
            
        )
        
    )
)