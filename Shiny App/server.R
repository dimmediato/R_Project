function(input, output, session){
  
  # Server Map ----  
  
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
  
  output$map = renderLeaflet({ 
    leaflet(geo2) %>%
      setView( lat=10, lng=0 , zoom=2)%>%
      addLegendNumeric(pal = colorNumeric(palette = "RdYlBu", domain=NULL), values = c(0,1), title = 'Democracy Index',
                       orientation = 'horizontal', fillOpacity = .7, width = 150,
                       height = 20, position = 'bottomright', group = 'Symbols',
                       data = geo2) })
  
  observe({
    leafletProxy("map", data=map_data()) %>% 
      addPolygons(data = geo2 , 
                  fillColor = ~colorNumeric(palette = "RdYlBu", domain=NULL)(map_data()), 
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
  
 # Democracy Index Plots, Grouped by Countries ---- 
  
  output$plot1 <- renderPlot({
    data <- V_Modified %>% filter(country_name %in% input$checkGroup1)
    (ggplot(NULL, aes_string(x= "year", y = "v2x_libdem", group = "country_name")) + 
      geom_line(data = data, aes(colour = country_name)) + 
      labs(color='Country', x="Year", y= "Democracy Index"))
 
  })
  
  output$info <- renderPrint({
    req(input$plot_click1)
    x <- round(input$plot_click1$x, 0)
    y <- round(input$plot_click1$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  output$plot2 <- renderPlot({
    data2 <- V_Modified %>% filter(country_name %in% input$checkGroup2)
    (ggplot(NULL, aes_string(x= "year", y = "v2x_libdem", group = "country_name")) + 
        geom_line(data = data2, aes(colour = country_name)) + 
        labs(color='Country', x="Year", y= "Democracy Index"))
  })
  
  output$info2 <- renderPrint({
    req(input$plot_click1)
    x <- round(input$plot_click1$x, 0)
    y <- round(input$plot_click1$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  output$plot3 <- renderPlot({
    data3 <- V_Modified %>% filter(country_name %in% input$checkGroup3)
    (ggplot(NULL, aes_string(x= "year", y = "v2x_libdem", group = "country_name")) + 
        geom_line(data = data3, aes(colour = country_name)) + 
        labs(color='Country', x="Year", y= "Democracy Index"))
  })
  
  output$info3 <- renderPrint({
    req(input$plot_click1)
    x <- round(input$plot_click1$x, 0)
    y <- round(input$plot_click1$y, 2)
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  # Party Regression Plots and Trend Bar Plot ---- 
  
  output$partytrend <- renderPlot({
    yvarnames <- c("Populism" = "v2xpa_popul",
                  "Anti-Pluralism" = "v2xpa_antiplural")
    ggplot(merged_data, aes(x= year, y = .data[[input$radio]], fill = year)) + 
      geom_bar(stat="identity") +
      scale_fill_viridis_c(option = "plasma") + 
      labs(fill='Year', x="Year", y= names(yvarnames[which(yvarnames == input$radio)]))
  })
  
  filtered1 <- reactive({
    merged_data %>% 
      filter(v2pagovsup==input$govsupportcheck)
  })
  output$partygraph <- renderPlot({
    ggplot(merged_data, aes(x = log(v2xpa_popul), y = v2x_libdem)) +
      geom_point(data = filtered1(), aes(group = v2pagovsup)) + stat_smooth(data = filtered1(), aes(group = v2pagovsup, fill = v2pagovsup),method = "lm", 
                                 formula = y ~ x, 
                                 geom = "smooth", color = "red") + 
      labs(fill = "Support Index", x="Party Populism", y= "Democracy Index")
  })
  
  output$partyinfo <- renderPrint({
    req(input$plot_click2)
    if (input$govsupportcheck == "0") {
    summary(lm(v2x_libdem~ log(v2xpa_popul), merged_data %>% 
                 filter(v2pagovsup == "0")))} else
    if (input$govsupportcheck == "1") {
      summary(lm(v2x_libdem~ log(v2xpa_popul), merged_data %>% 
                   filter(v2pagovsup == "1")))} else
                     if (input$govsupportcheck == "2") {
                       summary(lm(v2x_libdem~ log(v2xpa_popul), merged_data %>% 
                                    filter(v2pagovsup == "2")))} else
                                      if (input$govsupportcheck == "3") {
                                        summary(lm(v2x_libdem~ log(v2xpa_popul), merged_data %>% 
                                                     filter(v2pagovsup == "3")))}
                     
  })
  
  output$partygraph2 <- renderPlot({
    ggplot(merged_data, aes(x = log(v2xpa_antiplural), y = v2x_libdem)) +
      geom_point(data = filtered1(), aes(group = v2pagovsup)) + stat_smooth(data = filtered1(), aes(group = v2pagovsup, fill = v2pagovsup),method = "lm", 
                                                                            formula = y ~ x, 
                                                                            geom = "smooth", color = "red") + 
      labs(fill = "Support Index", x="Party Anti-Pluralism", y= "Democracy Index")
  })

output$partyinfo2 <- renderPrint({
  req(input$plot_click3)
  if (input$govsupportcheck == "0") {
    summary(lm(v2x_libdem~ log(v2xpa_antiplural), merged_data %>% 
                 filter(v2pagovsup == "0")))} else
                   if (input$govsupportcheck == "1") {
                     summary(lm(v2x_libdem~ log(v2xpa_antiplural), merged_data %>% 
                                  filter(v2pagovsup == "1")))} else
                                    if (input$govsupportcheck == "2") {
                                      summary(lm(v2x_libdem~ log(v2xpa_antiplural), merged_data %>% 
                                                   filter(v2pagovsup == "2")))} else
                                                     if (input$govsupportcheck == "3") {
                                                       summary(lm(v2x_libdem~ log(v2xpa_antiplural), merged_data %>% 
                                                                    filter(v2pagovsup == "3")))}
  
})

}