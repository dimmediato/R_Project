geo2 = geojson_read("merged.geojson", what = "sp")
V_Modified = readRDS("V_Modified.RDS")
merged_data = readRDS("merged_data.RDS")
average_merge = readRDS("average_merge.RDS")
merged_GINI = readRDS("merged_GINI.RDS")
Eastern_Europe = readRDS("Eastern_Europe.RDS")
Latin_America = readRDS("Latin_America.RDS")
Middle_East = readRDS("Middle_East.RDS")
Africa = readRDS("Africa.RDS")
Western_Europe = readRDS("Western_Europe.RDS")
Asia = readRDS("Asia.RDS")
Eastern_Europe_GINI = readRDS("Eastern_Europe_GINI.RDS")
Latin_America_GINI = readRDS("Latin_America_GINI.RDS")
Middle_East_GINI = readRDS("Middle_East_GINI.RDS")
Africa_GINI = readRDS("Africa_GINI.RDS")
Western_Europe_GINI = readRDS("Western_Europe_GINI.RDS")
Asia_GINI = readRDS("Asia_GINI.RDS")

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
  
  # Party Trend Bar Plot ---- 
  
  output$partytrend <- renderPlot({
    yvarnames <- c("Populism" = "v2xpa_popul",
                  "Anti-Pluralism" = "v2xpa_antiplural")
    ggplot(average_merge, aes(x= year, y = .data[[input$radio]], fill = year)) + 
      geom_bar(stat="identity") +  geom_line() +
      scale_fill_viridis_c(option = "plasma") + coord_cartesian(ylim = c(.2, .6)) +
      labs(fill='Year', x="Year", y= names(yvarnames[which(yvarnames == input$radio)]))
  })
  
  output$trend <- renderPrint({
    req(input$party_trendclick)
    x <- round(input$party_trendclick$x, 0)
    y <- input$party_trendclick$y
    cat("[", x, ", ", y, "]", sep = "")
  })
  
  # Party Regression Plots ----
  
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
  
  observe({
    if( is.null(input$govsupportcheck) ){
      updateCheckboxGroupInput(session, "govsupportcheck", selected ="0")
    }
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

# Polarization Trend Bar Plot ---- 

output$polarizationtrend <- renderPlot({
polarizationdata <- V_Modified %>% filter(country_name %in% input$polarizationpicker)
polarizationdata2 <- V_Modified %>% filter(country_name %in% input$polarizationpicker2)
polarizationdata3 <- V_Modified %>% filter(country_name %in% input$polarizationpicker3)
polarizationdata4 <- V_Modified %>% filter(country_name %in% input$polarizationpicker4)
polarizationdata5 <- V_Modified %>% filter(country_name %in% input$polarizationpicker5)
polarizationdata6 <- V_Modified %>% filter(country_name %in% input$polarizationpicker6)

ggplot(V_Modified, aes(x=year, y=v2cacamps_osp)) +
  geom_smooth(data = polarizationdata, method = loess, formula = y ~ x, aes(colour = country_name)) + 
  geom_smooth(data = polarizationdata2, method = loess, formula = y ~ x, aes(colour = country_name)) + 
  geom_smooth(data = polarizationdata3, method = loess, formula = y ~ x, aes(colour = country_name)) + 
  geom_smooth(data = polarizationdata4, method = loess, formula = y ~ x, aes(colour = country_name)) +
  geom_smooth(data = polarizationdata5, method = loess, formula = y ~ x, aes(colour = country_name)) + 
  geom_smooth(data = polarizationdata6, method = loess, formula = y ~ x, aes(colour = country_name)) +
  labs(color='Country', x="Year", y= "Political Polarization")
})

output$polarizationinfo <- renderPrint({
  req(input$polarization_click)
  x <- round(input$polarization_click$x, 0)
  y <- round(input$polarization_click$y, 2)
  cat("[", x, ", ", y, "]", sep = "")
})

# Polarization Regression Plots ---- 

filtered2 <- reactive({
  V_Modified %>% 
    filter(e_regionpol_6C==input$radio2)
})
output$polarizationregression <- renderPlot({ 
 ggplot(V_Modified, aes(x = v2cacamps_osp, y = v2x_libdem)) +
    geom_point(data = filtered2(), aes(group = e_regionpol_6C)) + geom_smooth(data = filtered2(), aes(group = e_regionpol_6C, fill = e_regionpol_6C),method = "lm", 
                                                                          formula = y ~ x, 
                                                                          color = input$radio2) + 
    labs(fill = "Region Index", x="Political Polarization", y= "Democracy Index")
})

output$polarizationinfo2 <- renderPrint({
  req(input$polarization_click2)
  if (input$radio2 == "0") {
    summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                 filter(e_regionpol_6C== "0")))} else
                   if (input$radio2 == "1") {
                     summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                                  filter(e_regionpol_6C == "1")))} else
                                    if (input$radio2 == "2") {
                                      summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                                                   filter(e_regionpol_6C == "2")))} else
                                                     if (input$radio2 == "3") {
                                                       summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                                                                    filter(e_regionpol_6C == "3")))} else if (input$radio2 == "4") {
                                                                      summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                                                                                   filter(e_regionpol_6C == "4")))}
  else if (input$radio2 == "5") {
    summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                 filter(e_regionpol_6C == "5")))}
  else if (input$radio2 == "6") {
    summary(lm(v2x_libdem~ v2cacamps_osp, V_Modified %>% 
                 filter(e_regionpol_6C == "6")))}
})

output$GINItrend <- renderPlot({
  GINIdata <- merged_GINI %>% filter(country_name %in% input$GINIpicker)
  GINIdata2 <- merged_GINI %>% filter(country_name %in% input$GINIpicker2)
  GINIdata3 <- merged_GINI %>% filter(country_name %in% input$GINIpicker3)
  GINIdata4 <- merged_GINI %>% filter(country_name %in% input$GINIpicker4)
  GINIdata5 <- merged_GINI %>% filter(country_name %in% input$GINIpicker5)
  GINIdata6 <- merged_GINI %>% filter(country_name %in% input$GINIpicker6)
  GINI_all <- rbind(GINIdata, GINIdata2, GINIdata3, GINIdata4, GINIdata5, GINIdata6)
  
  ggplot(merged_GINI, aes(x=year) )  +
    geom_smooth(data=GINI_all, method = loess, formula = y ~ x, aes(y=Value, color = "GINI Score")) + 
    geom_smooth(data=GINI_all, method = loess, formula = y ~ x, aes(y=v2x_libdem*100, color = "Democracy Index")) + 
    scale_colour_manual(name="Legend", values=c("darkblue", "darkgreen")) +
    labs(x="Year", y= "Dual GINI and Democracy Levels") 
})

# GINI Regression Plots ---- 

filtered3 <- reactive({
  merged_GINI %>% 
    filter(e_regionpol_6C==input$radio3)
})
output$GINIregression <- renderPlot({
  ggplot(merged_GINI, aes(x = Value, y = v2x_libdem)) +
    geom_point(data = filtered3(), aes(group = e_regionpol_6C)) + geom_smooth(data = filtered3(), aes(group = e_regionpol_6C, fill = e_regionpol_6C),method = "loess", 
                                                                              formula = y ~ x, 
                                                                              color = input$radio3) + 
    labs(fill = "Region Index", x="GINI Coef", y= "Democracy Index")
})

output$GINIinfo2 <- renderPrint({
  req(input$GINI_regression)
  if (input$radio3 == "0") {
    summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                 filter(e_regionpol_6C== "0")))} else
                   if (input$radio3 == "1") {
                     summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                                  filter(e_regionpol_6C == "1")))} else
                                    if (input$radio3 == "2") {
                                      summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                                                   filter(e_regionpol_6C == "2")))} else
                                                     if (input$radio3 == "3") {
                                                       summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                                                                    filter(e_regionpol_6C == "3")))} else if (input$radio3 == "4") {
                                                                      summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                                                                                   filter(e_regionpol_6C == "4")))}
  else if (input$radio3 == "5") {
    summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                 filter(e_regionpol_6C == "5")))}
  else if (input$radio3 == "6") {
    summary(lm(v2x_libdem~ Value, merged_GINI %>% 
                 filter(e_regionpol_6C == "6")))}
})

}