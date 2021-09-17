# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: server.R for a Shiny app that shows times from Idaho
# Interscholastic Cycling League races.

# TODO:
# Set to automatically update. Maybe every time the app starts it looks for new race data posted to the results_home page? If there's new stuff, it scrapes it and adds to existing. See how long this takes to do. If it's too long, maybe only update weekly--e.g., Sunday at 11:55pm. Maybe set update so that it only happens on for the first user of a day?
# Maybe save a scraped copy of the races table. For first user each day/week/whatever, compare this to a newly scraped version. If there are additional rows, do scraping for them. Add a progress bar if this takes too long.
# Speed up deployment to polished by using wide format, dropping unused columns. So conversion to long would happen on app load.
# Commit to git & github

# Define server logic
server <- function(input, output, session) {
    
    output$foo <- renderUI({
        
        radioButtons(
            "select_method"
            ,label = "Select riders by:"
            ,choices = c("Name", "Place")
        )
    })
    
    # Whenever radio buttons are clicked, restore riders and places to default
    observeEvent(input$select_method, {
        updateSelectizeInput(
            session
            ,"riders"
            ,label = "Names"
            ,choices = rider_list
            ,selected = NULL
            ,server = TRUE
            ,options = list(
                placeholder = "Select names"
                ,maxItems = 10)
            )
        updateSelectizeInput(
            session
            ,"places"
            ,label = "Places"
            ,choices = place_list
            ,options = list(
                placeholder = "Select places"
                ,maxItems = 10)
            )
        })
    
    # Update rider list from server
    updateSelectizeInput(session
                         ,"riders"
                         ,choices = rider_list
                         ,server = TRUE)
    
    # data, n_races & n_riders are reactives that update whenever Submit is
    # clicked.
    data <- eventReactive(input$submit, {

        # Apply filters
        if(length(input$races) > 0) {
            df <- df %>% filter(Race %in% input$races)
        }
        if(length(input$riders) > 0) {
            df <- df %>% filter(Name %in% input$riders)
        }
        if(length(input$division) > 0) {
            df <- df %>% filter(Division %in% input$division)
        }
        if(length(input$places) > 0) {
            df <- df %>% filter(Place %in% input$places)
        }
        df
    })
    
    # n_races is used to determine the number of facet columns
    n_races <- eventReactive(input$submit, {
        data() %>%
            pull(Race) %>%
            unique() %>%
            length()
        })
    
    # n_riders is used to determine error messages
    n_riders <- eventReactive(input$submit, {
        data() %>%
            pull(Name) %>%
            unique() %>%
            length()
        })
    
    # Display error if there's no data
    output$error <- renderText({
        if(n_riders() == 0) {
            "There are no data for the current filter settings. Please remove filters and resubmit." }
        else if(n_riders() > max_riders) {
            "The current filter settings result in data for more than ten riders, which leads to cluttered figures. Please adjust your filters and resubmit." }
        else { NULL }
    })
    
    # Plotly figure
    output$figure <- renderPlotly({
        if (n_riders() != 0 & n_riders() <= max_riders) {

            # Compute ncol. Default is 1.
            ncol <- case_when(
                n_races() %in% 1:3 ~ 1,
                n_races() %in% 4:6 ~ 2,
                n_races() %in% 7:9 ~ 3,
                n_races() > 9 ~ 4)

            # Horizontal jitter to reduce overplotting
            jittered <-  position_dodge(width = .2)

            # Start with ggplot
            plot <- ggplot(
                data = data()
                ,mapping = aes(
                    x = as_factor(Lap)
                    ,y = time_min
                    ,group = Name
                    ,color = Name
                    ,Division = Division
                    ,Place = Place
                    ,`Lap Time` = `Lap Time`
                )) +
                geom_line(position = jittered) +
                geom_point(position = jittered, size = 1.5) +
                scale_x_discrete(name = "Lap") +
                scale_y_continuous(name = "Time") +
                scale_color_brewer(palette = "Paired") +
                facet_wrap( ~ Race, ncol = ncol) +
                theme(legend.title = element_blank())
            
            # Convert to plotly. Height depends on javascript bit in ui.R.
            ggplotly(
                plot
                ,tooltip = c("Division", "Place", "Lap Time")
                ,height = 0.85 * as.numeric(input$dimension[2])) %>%
            layout(margin = list(l = 80, r = 30, b = 30, t = 40, pad = 0)) }
        else { NULL }
    })
}
