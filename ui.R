library(shiny)

# Define UI for application that plots random distributions 
shinyUI(
    pageWithSidebar(
    
        # Application title
        headerPanel("MAP and ALTITUDE PROFILE PLOTTER"),
    
        # Sidebar with some information and high level input
        sidebarPanel(
            
            # Explanation for user
            h5("With this app, you can upload a *gpx file of your run or hike, and the app will show the track on a map and plot 
               an altitude profile."),
            
            h5("If you select demo mode, a map and altitude profie is shown for a hike in Scotland."),
            
            h5("For the application to work, GPX files must have one route or track per file only, 
               and the elevation must be stored in the GPX log."),
            
           # add a link to website with more gpx tracks.        
            tags$h5(class="header", checked=NA,
                     tags$p("For more trails with GPX tracks that can be downloaded, use the following link:"),
                     tags$a(href="http://www.walkhighlands.co.uk/long-distance-routes.shtml", "Scotland's Great Trails")
            ),
            
           # user can choose demo mode, or select a file. default is demo mode.
            radioButtons("SelectFile", label = h4("GPX Choice"),
                choices = list("Show me a Demo" = 1, "Let me upload a file from my PC" = 2),selected = 1),
            
           # This section is only shown if user chose to select a file from the pc
            conditionalPanel(
                condition = "input.SelectFile == 2",
                fileInput('gpxfile', 'Choose a *.gpx File',accept=c('.gpx')))),
    
    # Show a title, plot on map, another title and then the altitude profile
        mainPanel(
            h3(textOutput("caption")),
            plotOutput("distPlot",width="500px",height="500px"),
            h3(textOutput("caption2")),
            plotOutput("altPlot",width="500px",height="200px")
        )
    )
)