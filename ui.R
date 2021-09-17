# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: ui.R for a Shiny app that shows times from Idaho Interscholastic
# Cycling League races.

# Define UI
ui <- fluidPage(
    titlePanel("Idaho Interscholastic Cycling League Race Results"),
    setBackgroundImage(src = "wp3064754-idaho-wallpapers.jpg"),
    
    # Javascript to get the height of the window
    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '))
    # tags$head(tags$style("#error{color: gray;
    #                              font-size: 20px
    #                              }"
    # ))
    ,sidebarLayout(
        sidebarPanel(
            selectizeInput(
                "races"
                ,label = "Races"
                ,choices = race_list
                ,multiple = TRUE
                ,options = list(placeholder = "Select races"
                                ,maxItems = 9)
            )
            ,selectizeInput(
                "division"
                ,label = "Division"
                ,choices = division_list
                ,multiple = TRUE
                ,options = list(
                    placeholder = "Select a race division"
                    ,maxItems = 1)
            )
            ,uiOutput("foo")
            ,conditionalPanel(
                condition = "input.select_method == 'Name'"
                ,selectizeInput(
                    "riders"
                    ,label = "Names"
                    ,choices = NULL
                    ,selected = NULL
                    ,multiple = TRUE
                    ,options = list(placeholder = "Select names"
                                    ,maxItems = 10)
                ))
            ,conditionalPanel(
                condition = "input.select_method == 'Place'"
                ,selectizeInput(
                    "places"
                    ,label = "Places"
                    ,choices = place_list
                    ,multiple = TRUE
                    ,options = list(placeholder = "Select places"
                                    ,maxItems = 10)
                )
            )
            ,actionButton("submit" ,label = "Submit")
            ,br()
            ,br()
            ,textOutput("error")
        )
        
        ,mainPanel(
            plotlyOutput(
                "figure"
                ,width = "100%"
                )
        )
    )
)
