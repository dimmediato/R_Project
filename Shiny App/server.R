function(input, output, session){
  
  map_data <- reactive({switch(input$yearInput, 
                               "2023"=geo2$Year_2023,"2022"=geo2$Year_2022,"2021"=geo2$Year_2021,
                               "2020"=geo2$Year_2020,"2019"=geo2$Year_2019,"2018"=geo2$Year_2018,"2017"=geo2$Year_2017,
                               "2016"=geo2$Year_2016,"2015"=geo2$Year_2015,"2014"=geo2$Year_2014,"2013"=geo2$Year_2013,
                               "2012"=geo2$Year_2012,"2011"=geo2$Year_2011,"2010"=geo2$Year_2010,"2009"=geo2$Year_2009,
                               "2008"=geo2$Year_2008,"2007"=geo2$Year_2007,"2006"=geo2$Year_2006,"2005"=geo2$Year_2005,
                               "2004"=geo2$Year_2004,"2003"=geo2$Year_2003,"2002"=geo2$Year_2002,"2001"=geo2$Year_2001,
                               "2000"=geo2$Year_2000,"1999"=geo2$Year_1999,"1998"=geo2$Year_1998,"1997"=geo2$Year_1997,
                               "1996"=geo2$Year_1996,"1995"=geo2$Year_1995,"1994"=geo2$Year_1994,"1993"=geo2$Year_1993,
                               "1992"=geo2$Year_1992,"1991"=geo2$Year_1991,"1990"=geo2$Year_1990,"1989"=geo2$Year_1989,
                               "1988"=geo2$Year_1988,"1987"=geo2$Year_1987,"1986"=geo2$Year_1986,"1985"=geo2$Year_1985,
                               "1984"=geo2$Year_1984,"1983"=geo2$Year_1983)
  })
  
  pal <-colorNumeric(palette = c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2"),
                     domain=c(0,.09, .10,.19, .20,.29, .30,.39, .40,.49, .50,.59, .60,.69, .70,.79, .80,.89, .90,1))
  mytext <- paste(
    "Country: ", geo2$NAME,"<br/>", 
    "Democracy Index: ", round(geo2$Year_2023, 2), 
    sep="") %>%
    lapply(htmltools::HTML)
  
  output$map = renderLeaflet({ 
    leaflet(geo2) %>%
      setView( lat=10, lng=0 , zoom=2)%>%
      addLegendNumeric(pal = pal, values = c(0,1), title = 'Democracy Index',
                       orientation = 'horizontal', fillOpacity = .7, width = 150,
                       height = 20, position = 'bottomright', group = 'Symbols',
                       data = geo2) })
  
  observe({
    leafletProxy("map", data=map_data()) %>% 
      addPolygons(data = geo2 , 
                  fillColor = ~pal(map_data()), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2,
                  label = paste(
                    "Country: ", geo2$NAME,"<br/>", 
                    "Democracy Index: ", map_data(), 
                    sep="") %>%
                    lapply(htmltools::HTML),
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  ))
  })
  
  
}