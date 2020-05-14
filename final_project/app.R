library(xts)
library(shiny)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(tigris)

gv <- readRDS("GV_GeoData.rds")

gv_df <- read.csv(paste0("https://raw.githubusercontent.com/josephsimone/Data_608/master/final_project/gv_archive.csv"))


states <- states(cb=T)


sb_state <- gv_df %>%
    group_by(State) %>%
    summarize(total=n())%>% 
    rename(state=State)


states_merged_sb <- geo_join(states, sb_state, "NAME", "state")



bins <- c(0, 10, 20, 50, 60)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)


states_merged_sb <- subset(states_merged_sb, !is.na(total))


popup_sb <- paste0("Total Incidents: ", as.character(states_merged_sb$total))

ui <- navbarPage("US Gun Violence Application", id="nav",
                 
                 tabPanel("About",
                          div(class="outer",
                              tags$head(
                                  includeCSS("Assets/styles.css")),
                              
                          fluidRow(
                              column(12,
                                     h3("Application Overview"),
                                     p("This Shiny Application's purpose is to aid in the understanding and vizualization of this archive's data. 
                                       This application maps the Gun Violence Crisis that has plagued our country's recent history from November 2018 - March 2020. 
                                       Encompassed within are an animated and interative map showing which parts of the country are most affected. "),
                                     h3("Data Source"),
                                     div(HTML("<a href='http://www.gunviolencearchive.org/methodology'>The Gun Violence Archive</a>")),
                                     p("The Gun Violence Archive is an online archive of gun violence incidents collected from over 7,500  law enforcement, media, government and commercial sources daily in an effort to provide near-real time data about the results of gun violence. GVA is an independent data collection and research group with no affiliation with any advocacy organization."),
                                    
                                     h3("Gun Violence Archive Mission Statement"),
                                     p("Gun Violence Archive (GVA) is a not for profit corporation formed in 2013 to provide online public access to accurate information
                        about gun-related violence in the United States. GVA will collect and check for accuracy, comprehensive information about gun-related violence in the 
                        U.S. and then post and disseminate it online, primarily if not exclusively on this website and summary ledgers at www.facebook.com/gunviolencearchive. 
                        It is hoped that this information will inform and assist those engaged in discussions and activities concerning gun violence, including analysis of 
                        proposed regulations or legislation relating to gun safety usage. All we ask is to please provide proper credit for use of Gun Violence Archive data and 
                        advise us of its use.
                      
                        GVA is not, by design an advocacy group. The mission of GVA is to document incidents of gun violence and gun crime nationally to provide independent, 
                        verified data to those who need to use it in their research, advocacy or writing.
                      "),
                                     h3("Why are GVA Mass Shooting numbers higher than some other sources?"),
                                     p("The Gun Violence Archive uses a purely statistical threshold to define mass shooting based only on the numeric value of 4 or more shot 
                      or killed, not including the shooter. GVA does not parse the definition to remove any subcategory of shooting. To 
                      that end we don't exclude, set apart, caveat, or differentiate victims based upon the circumstances in which they were shot."),
                                    
                                     h4("Application Source Files:"),
                                     div(HTML("<a href='https://github.com/josephsimone/Data_608/tree/master/final_project'>GitHub</a>")),
                                     h4("References:"),
                                     div(HTML("<a href='https://andrewbtran.github.io/NICAR/2017/maps/leaflet-r.html#how_to_put_the_map_online'>Mapping with R</a>")),
                                     div(HTML("<a href='https://rstudio.github.io/leaflet/shiny.html'>Leaflet For R</a>")),
                                     div(HTML("<a href='https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example'>Shiny-R SuperZip Example</a>"))
                              )))),
                          
                  tabPanel("Animated Map",
                          div(class="outer",
                              tags$head(
                                  includeCSS("Assets/styles.css")
                              ),
                              leafletOutput("map", width="100%", height="100%"), 
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("US Gun Violence"),
                                            h4("November 2018 - March 2020"),
                                            radioButtons("incidentweight", "Incident Factor:",
                                                         c("Killed"="Killed", "Injured"="Injured"), selected = "Killed", inline=TRUE),
                                            sliderInput(inputId = "date", label = "Animate", min = min(gv$Date), 
                                                        max = max(gv$Date),
                                                        value = max(gv$Date),
                                                        ticks = F,
                                                        step=365/12, 
                                                        animate = animationOptions(interval = 1000,
                                                                                   playButton = icon('play', "fa-3x"),
                                                                                   pauseButton = icon('pause', "fa-3x"))),
                                            textOutput("counts")
                              ),
                              tags$div(id="cite",
                                       'Data Source:  The Gun Violence Archive.'
                              )
                          )
                 ),
                 tabPanel("Interative Map",
                     div(class="outer",
                         tags$head(
                             includeCSS("Assets/styles.css")
                         ),
                         leafletOutput("mymap", width="100%", height="100%"), 
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 330, height = "auto",
                                       h2("US Gun Violence"),
                                       h4("November 2018 - March 2020")
                         ),
                         tags$div(id="cite",
                                   'Data Source:  The Gun Violence Archive.')
                     )
                     )

                 
)


server <- function(input, output, session) {
    history <- reactive({
        gv %>%
            filter(Date <= input$date)
    })
    
    color <- reactive({
        if (input$incidentweight == "Killed") {
            col = "OrRd"
        } else {
            col = "YlGn"
        }
    })
    
    sc <- reactiveVal(7000)
    
    observeEvent(input$incidentweight, {
        if (input$incidentweight == "Killed") {
            newValue <- 7000
            sc(newValue)
        } else {
            newValue <- 4000
            sc(newValue)
        }
    })
    
    name <- reactive({
        if (input$incidentweight == "Killed") {
            nam = "Killed"
        } else {
            nam = "Injured"
        }
    })
    
    output$counts <- renderText({
        c <- sum(history()[[input$incidentweight]])
        paste("Total ", name(), ": ", c)
    })
    
    colorpal <- reactive({
        colorNumeric(color(), gv[[input$incidentweight]])
    })
    
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                     attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%  # Add default OpenStreetMap map tiles%>%
            addLegend(position = "bottomright",
                      pal = colorpal(), values = gv[[input$incidentweight]],
                      title = name()) %>%
            setView(lng = -83.7129, lat = 37.0902, zoom = 4)
    })
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-98.483330, 38.712046, zoom = 4) %>% 
            addPolygons(data = states_merged_sb , 
                        fillColor = ~pal(states_merged_sb$total), 
                        fillOpacity = 0.7, 
                        weight = 0.2, 
                        smoothFactor = 0.2, 
                        popup = ~popup_sb) %>%
            addLegend(pal = pal, 
                      values = states_merged_sb$total, 
                      position = "bottomright", 
                      title = "Total<br />Incidents<br />per</br> State")
    })
    observe({
        pal <- colorpal()
        proxy <- leafletProxy("map", data = history()) 
        proxy %>%
            clearShapes() %>%
            addCircles(lng = ~lon,
                       lat = ~lat,
                       radius = ~history()[[input$incidentweight]] * sc(),
                       weight = 1,
                       popup= ~Content,
                       color = "#777777",
                       fillColor = ~pal(history()[[input$incidentweight]]),
                       stroke = F, 
                       fillOpacity = 0.7,
                       data = history()
            ) 
    })
}

shinyApp(ui, server)