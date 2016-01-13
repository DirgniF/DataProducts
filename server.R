# This server UI creates 2 outputs:
# 1) distPlot: a plot of the track on a google map
# 2) altPlot: an altitude profile
# Next to that, there are also 2 output titles that update dynamically.
# 
# For both of these outputs, it is necessary to parse the information stored in the GPX file
# for distPlot, we need latitude and longitude.
# for altPlot. we need lat, long and elevation.
# 
# Some of the calculations that happen based on changes to the input of the users are used by both output functions.(eg. name of track, lat/long info)
# To make those calculations only once, we use 2 reactive conductors.
# For more information on how this works: see http://shiny.rstudio.com/articles/reactivity-overview.html
# First Reactive Conductor:
# MODE: this function returns, as a string, the operating mode of the application, which will govern what the output function will display
#   "Demo": app will show demo information
#   "noFile": app will show nothing, user is in process to select a file to upload
#   "localFile": app should show information stored in the file the user selected on his PC.
# 
# Second Reactive Conductor: curGPXInfo: this function returns the GPXInfo of the relevant file (governed by MODE above)
# GPXInfo is a user-defined function returns a list with 2 elements:
# # [["name"]]: the first element is a string, storing the name of the track/route
# [[coords]]: the second element is a data frame, with 3 columns: "lat", "long" and "ele".
# 
# Further information can be found close to the relevant pieces of code.


library(shiny)
library(RCurl)
library(XML)
library(RgoogleMaps)
library(ggplot2)

# file to plot if in demo-mode.
urlDemo<-"http://www.walkhighlands.co.uk/fortwilliam/profiles/fort-william-glenfinnan.gpx"

# User-defined function
# For the altitude profile, we need the distance between consecutive points, while the gpx file stores lat and long only
# the "Haversine" formula calculcated this distance, given latitude and longitude of 2 points, 
# see e.g. https://en.wikipedia.org/wiki/Haversine_formula
haverDist<-function(aLong,aLat,bLong,bLat){
    dLat<-2*pi*(bLat-aLat)/360.0
    dLon<-2*pi*(bLong-aLong)/360.0
    a<-(sin(dLat/2))^2+cos(2*pi*aLat/360)*cos(2*pi*bLat/360)*(sin(dLon/2)^2)
    6371*2*atan2(sqrt(a),sqrt(1-a))
}

# Define server logic required to generate both plots and correct titles
shinyServer(function(input, output) {
    
    ## 1st Reactive conductor:
   
    # Determine if we are in demoMode or if a file was selected
    Mode<-reactive({
        if(input$SelectFile==1)
        {"Demo"}
        else
        {if(length(input$gpxfile$datapath)==0)
        {"noFile"}
            else
            {"localFile"}
        }
    })
    
    # User Defined Function
     GPXInfo<-function(gpxParsed){
            
            # extract route name from GPX file. If there are several names, extract first    
            routeName<-xmlValue(xpathSApply(gpxParsed,"//name")[[1]])
        
            # Check if information is stored as track or as route
            # Depending on what you find, extract point information stored in tracks or stored in routes
            storedAsRte<-FALSE
            storedAsTrk<-FALSE
            if(length(getNodeSet(gpxParsed,"//trk"))>0)
            {
                storedAsTrk<-TRUE
                points<-xpathSApply(gpxParsed,"//trk//trkpt")
                alts<-xpathSApply(gpxParsed,"//trk//trkpt/ele")
            }else{
                if(length(getNodeSet(gpxParsed,"//rte"))>0) 
                {
                    storedAsRte<-TRUE
                    points<-xpathSApply(gpxParsed,"//rte//rtept")
                    alts<-xpathSApply(gpxParsed,"//rte//rtept/ele")
                }
            }
            
            # Above xpathApply functions returned lists. Code below simplifies it to vectors, and for the points,
            # store lat and lon separately
            
            lat<-sapply(points,function(x){as.numeric(xmlGetAttr(x,"lat"))})
            lon<-sapply(points,function(x){as.numeric(xmlGetAttr(x,"lon"))})
            ele<-sapply(alts,function(x){(as.numeric(xmlValue(x)))})
            
            # output of the GPXInfo, a list with all information needed by both output functions (dispPlot and altPlot)
            list(name=routeName,coords=data.frame(lat=lat,lon=lon,ele=ele))
    }
    
     ## 2nd Reactive conductor:
     # determine name, alt, lat and lon
    curGPXInfo<-reactive({if (Mode()=="noFile")
        {return()}
        else
            if(Mode()=="Demo")
                {GPXInfo(htmlParse(getURL(urlDemo)))} 
            else 
                {GPXInfo(htmlParse(input$gpxfile$datapath))}})
           
    # Output of 2 titles
    
    output$caption <- renderText(paste0(ifelse(Mode()=="Demo","DEMO - ",""),curGPXInfo()[["name"]]))
    output$caption2 <- renderText({
        if (Mode()=="noFile") return() 
        paste0(ifelse(Mode()=="Demo","DEMO - ",""),"Altitude Profile")})
    
    # output plot on map
    output$distPlot <- renderPlot({
        if (Mode()=="noFile") return()
        ## plot on Map
        lat<-curGPXInfo()[["coords"]]$lat
        lon<-curGPXInfo()[["coords"]]$lon
        
        # create a little border and chose desired plot level
        extra<-c(-0.005,0.005)
        rangeLon<-range(lon)+extra
        rangeLat<-range(lat)+extra
        center = c(mean(rangeLat), mean(rangeLon))
        zoom <- min(MaxZoom(range(rangeLat), range(rangeLon)));
        
        # get the google map and plot the line
        MyMap <- GetMap(center=center, zoom=zoom,maptype="terrain")
        pic = PlotOnStaticMap(MyMap)
        PlotOnStaticMap(MyMap, lat = lat, lon = lon, lwd=3,col='red', FUN = lines, add=TRUE)
    
      })
    
        
    # output altitude profile    
    output$altPlot<-renderPlot({
        if (Mode()=="noFile") return()
        # store info in easily accessible vectors
        lat<-curGPXInfo()[["coords"]]$lat
        lon<-curGPXInfo()[["coords"]]$lon
        ele<-curGPXInfo()[["coords"]]$ele
        # calculate distance between difference points, using haverDist
        dist<-0
        for (i in 2:length(lat)) {
            dist[i]<-dist[i-1]+haverDist(lon[i],lat[i],lon[i-1],lat[i-1])
        }
        # store information to plot
        toPlot<-data.frame(dist=dist,ele=ele) 
       
        # create plot
        h<-ggplot(toPlot,aes(x=dist))
        h+geom_ribbon(aes(ymin=0,ymax=ele),fill = 'steelblue2',alpha=.8)+
            labs(x = "Distance (KM)",y = "Altitude (m)")+
            theme(axis.text.x = element_text( size=12))+
            theme(axis.text.y = element_text( size=12))+
            theme(plot.background = element_rect(fill="grey"))+
            theme(panel.background = element_rect(fill="grey"))+
            theme(axis.ticks = element_blank())
    })    
})