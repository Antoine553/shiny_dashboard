library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    # Creation d'un header avec uniquement un titre et une sidebar vide
    dashboardHeader(title = "INFO0808"),
    dashboardSidebar(),
    dashboardBody(
        # Création de la premiere ligne pour le premier graph et ses controls
        fluidRow(
            # Chargement du premier graph "plot1"
            box(plotOutput("plot1", height = 250)),
            # Chargement des controls avec deux option preselectionner pour filtrer le graph
            box(
                title = "Controls",
                checkboxGroupInput(
                    "checkGroup1", 
                    h3("Checkbox group"), 
                    choices = list(
                        "Homme" = "Male", 
                        "Femme" = "Female"
                        ),
                    selected = c("Male","Female")
                )
            )
        ),
        # Création de la seconde ligne pour le second graph et ses controls
        fluidRow(
            # Chargement du second graph "plot2"
            box(plotOutput("plot2", height = 250)),
            # Chargement des controls avec deux option preselectionner pour filtrer le graph
            box(
                title = "Controls",
                checkboxGroupInput(
                    "checkGroup2", 
                    h3("Checkbox group"), 
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
        )
    )
)