# Author : Satyabrat Borgohain
# Image references : http://www.freepik.com, https://www.flaticon.com/

# Required libraries ----------------------------------------------------------------------------
library('ggplot2')
library('shiny')
library('DT')
library('leaflet')

# Reading the dataset ---------------------------------------------------------------------------
dataset <- read.csv('assignment-02-formatted-data.csv')

# Cleaning --------------------------------------------------------------------------------------
colnames(dataset)[1] <- 'year'
dataset$bleaching <- gsub("%", "", dataset$bleaching)
dataset$bleaching <- as.numeric(dataset$bleaching)
dataset$longitude <- as.numeric(dataset$longitude)
dataset$latitude <- as.numeric(dataset$latitude)

# Reordering the dataset on the basis of decreasing latitude (refactoring) ----------------------
dataset.ordered <-
  dataset[order(dataset$latitude, decreasing = TRUE), ]
dd <- unique(dataset.ordered$site)
dataset$site = factor(dataset$site, levels = c(as.character(dd)))

# UI component of the Shiny App -----------------------------------------------------------------
ui <- navbarPage(
  # Title of the app
  "Bleaching Analyzer",
  # Main tab for all the visualizations
  tabPanel("Visualization",
           
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 p(
                   "An interactive visualization showcasing 'bleaching %' vs 'year' of corals, at sites located
                   in the Great Barrier Reef (as shown in the map). The sites are arranged (in descending order) by their latitude."
                 ),
                 p("Use the following parameters to customize the visualization :"),
                 br(),
                 # Input for the type of coral
                 selectInput(
                   inputId = "coral",
                   label = "Select type of Coral :",
                   choices = c(
                     "All" = "all",
                     "Blue corals" = "blue corals",
                     "Hard corals" = "hard corals",
                     "Sea pens" = "sea pens",
                     "Soft corals" = "soft corals",
                     "Sea fans" = "sea fans "
                   ),
                   selected = "all"
                 ),
                 hr(),
                 # Input for selecting the color coding
                 radioButtons(
                   inputId = "colorcoding",
                   label = "Select color coding :",
                   c(
                     "Bleaching" = "bleaching",
                     "Sites (also affects the map)" = "sites"
                   ),
                   selected = "bleaching"
                 ),
                 hr(),
                 # Input slider for the choice of smoother
                 sliderInput(
                   inputId = "smoother",
                   label = "Select smoother :",
                   min = 0,
                   max = 2,
                   value = 0
                 ),
                 fluidRow(verbatimTextOutput("value")),
                 width = 3
                 ),
               # Main panel for displaying outputs
               mainPanel(fluidRow(
                 # The main dynamic tabular visualization
                 column(
                   8,
                   plotOutput(
                     outputId = "tabViz",
                     dblclick = "tab_dblclick",
                     brush = brushOpts(id = "tab_brush",
                                       resetOnNew = TRUE)
                   )
                 ),
                 br(),
                 # The leaflet map
                 column(
                   4,
                   tags$style(type = "text/css", "#map {height: 550px !important;}"),
                   # Added to increase the height of map
                   leafletOutput(outputId = "map")
                 )
               ),
               width = 9)
             )
           )),
  # Secondary tab for viewing the original data
  tabPanel("Data", DT::dataTableOutput("table")),
  # Tab containing information about the developer/author
  tabPanel("About",
           fluidRow(verbatimTextOutput("about")))
)

# Server component of the Shiny App ----------------------------------------------------------------
server <- function(input, output) {
  # Variables
  # For zooming functionality
  ranges <- reactiveValues(x = NULL, y = NULL)
  index <- c("0", "1", "2")
  # Types of smoothers
  smoothers <- c("None", "lm", "loess")
  names(smoothers) <- index
  # Alpha value for the color
  color.alpha <- 0.1
  # Custom colors for the site
  site.colors <-
    c(
      site01 = "#39B600",
      site02 = "#9633FF",
      site03 = "#9CFF33",
      site04 = "#E2FF33",
      site05 = "#F8BC6D",
      site06 = "#00B7FD",
      site07 = "#E3DB71",
      site08 = "#6DEFF8"
    )
  
  # Tabular plot output
  output$tabViz <- renderPlot({
    # Storing user selected smoother
    input.smoother <- as.character(input$smoother)
    # Condition if displaying 'All' corals
    if (input$coral == "all") {
      # Condition to check user input for color as all sites
      if (input$colorcoding == "sites") {
        tab.plot <-
          ggplot(data = dataset, aes(x = year, y = bleaching)) + ggtitle("All corals") + theme(plot.title = element_text(hjust = 0.5)) + 
          ylab(label = "bleaching %")
        # Defining unique colors for the sites
        dataset.color <-
          unique(dataset[, c('site', 'type', 'year', 'bleaching')])
        # Tabular plot
        tab.plot <-
          tab.plot + geom_rect(
            data = dataset.color,
            aes(fill = site),
            xmin = -Inf,
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            alpha = color.alpha
          ) + geom_point() + facet_grid(site ~ type) + scale_fill_manual(values = site.colors)
      }
      # Condition for showing color coding for bleaching
      else{
        # Tabular plot
        tab.plot <-
          ggplot(data = dataset, aes(
            x = year,
            y = bleaching,
            color = bleaching
          )) + geom_point() + ggtitle("All corals") + theme(plot.title = element_text(hjust = 0.5)) + facet_grid(site ~ type) +
          scale_color_gradient(high = 'orange', low = 'blue') + ylab(label = "bleaching %")
      }
      # Choice of smoother applied
      if (smoothers[input.smoother] == "None") {
        tab.plot + coord_cartesian(xlim = ranges$x,
                                   ylim = ranges$y,
                                   expand = TRUE)
      }
      else{
        tab.plot + geom_smooth(method = smoothers[input.smoother]) + coord_cartesian(xlim = ranges$x,
                                                                                     ylim = ranges$y,
                                                                                     expand = TRUE)
      }
    }
    # Condition when selecting a specific coral
    else{
      # Variable to store the type of coral given by the user
      coral.type <- input$coral
      sub.dataset <- subset(dataset, type == coral.type)
      # Condition to check user input for color as all sites
      if (input$colorcoding == "sites") {
        trend.plot <-
          ggplot(data =  sub.dataset, aes(x = year, y = bleaching)) +
          ggtitle(coral.type) + theme(plot.title = element_text(hjust = 0.5)) + ylab(label = "bleaching %")
        # To select the sites uniquely
        dataset.color <-
          unique(sub.dataset[, c('site', 'type', 'year', 'bleaching')])
        trend.plot <-
          trend.plot + geom_rect(
            data = dataset.color,
            aes(fill = site),
            xmin = -Inf,
            xmax = Inf,
            ymin = -Inf,
            ymax = Inf,
            alpha = color.alpha
          ) + geom_point(size = 2) + scale_fill_manual(values = site.colors) + facet_grid(site ~ .)
      }
      # Condition for showing color coding for bleaching
      else{
        trend.plot <-
          ggplot(data =  sub.dataset, aes(
            x = year,
            y = bleaching,
            color = bleaching
          )) + geom_point(size = 2) +
          ggtitle(coral.type) + theme(plot.title = element_text(hjust = 0.5)) + scale_color_gradient(high = 'orange', low = 'blue') + 
          ylab(label = "bleaching %")
        trend.plot <- trend.plot + facet_grid(site ~ .)
      }
      # Choice of smoother applied
      if (smoothers[input.smoother] == "None") {
        trend.plot + coord_cartesian(xlim = ranges$x,
                                     ylim = ranges$y,
                                     expand = TRUE)
      }
      else{
        trend.plot + geom_smooth(method = smoothers[input.smoother]) + coord_cartesian(xlim = ranges$x,
                                                                                       ylim = ranges$y,
                                                                                       expand = TRUE)
      }
    }
    
  },
  height = 600)
  
  # For zooming functionality
  observeEvent(input$tab_dblclick, {
    brush <- input$tab_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    # Icons for site color coding
    site.icons <-
      iconList(
        site01 = makeIcon(
          "39B600.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site02 = makeIcon(
          "9633FF.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site03 = makeIcon(
          "9cff33.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site04 = makeIcon(
          "E2FF33.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site05 = makeIcon(
          "F8BC6D.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site06 = makeIcon(
          "00B7FD.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site07 = makeIcon(
          "E3DB71.png",
          iconWidth = 32,
          iconHeight = 32
        ),
        site08 = makeIcon(
          "6deff8.png",
          iconWidth = 32,
          iconHeight = 32
        )
      )
    # Condition if all is selected for coral type
    if (input$coral == "all") {
      # If color coding is for sites
      if (input$colorcoding == "sites") {
        map <- leaflet(data = dataset) %>%
          addTiles() %>%
          # color coding
          addMarkers(
            lng = ~ longitude,
            lat = ~ latitude,
            icon = ~ site.icons[site],
            label = ~ site ,
            popup = ~ site
          )
      }
      else{
        map <- leaflet(data = dataset) %>%
          addTiles() %>%
          # color coding
          addMarkers(
            lng = ~ longitude,
            lat = ~ latitude,
            label = ~ site ,
            popup = ~ site
          )
      }
    }
    # Condition if a specific coral type is selected
    else{
      sub.dataset <- subset(dataset, type == input$coral)
      if (input$colorcoding == "sites") {
        map <- leaflet(data = sub.dataset) %>%
          addTiles() %>%
          addMarkers(
            lng = ~ longitude,
            lat = ~ latitude,
            icon = ~ site.icons[site],
            label = ~ site ,
            popup = ~ site
          )
      }
      else{
        map <- leaflet(data = sub.dataset) %>%
          addTiles() %>%
          addMarkers(
            lng = ~ longitude,
            lat = ~ latitude,
            label = ~ site ,
            popup = ~ site
          )
      }
    }
    # Display the map
    map
  })
  
  # Display the type of the smoother used to the user
  output$value <-
    renderText({
      paste("Smoother used : ", smoothers[as.character(input$smoother)])
    })
  
  # Display the datatable showing the original dataset
  output$table <- DT::renderDataTable(DT::datatable({
    dataset
  }))
  
  # Information about the author
  output$about <-
    renderText({
      "
      Created by :
      Satyabrat Borgohain\n
      Student ID :
      29300800\n
      Unit :
      FIT 5147 (Data Exploration & Visualization)\n
      Assignment :
      2
      "
    })
    }

# Create the Shiny App ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)