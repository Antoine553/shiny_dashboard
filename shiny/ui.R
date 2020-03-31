library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    
    # Header avec titre et sidebar
    dashboardHeader(title = "INFO0808"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("List of anime", icon = icon("table"), tabName = "list_anime")
        )
    ),
    
    # Body
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    h2("Dashboard tab content")
            ),
            
            tabItem(tabName = "list_anime",
                    h2("List of anime")
            )
        ),
        
        
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