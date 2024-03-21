function(input, output, session){
  
  map_data <- reactive({
  })
  
  pal <- colorNumeric("RdYlBu", domain=geo2$Democracy_Index)
  popup_sb <- paste0("Total: ", as.character(geo2$Democracy_Index))
  
  output$map = renderLeaflet({ 
    map_data() %>%
      leaflet(geo2) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView( lat=10, lng=0 , zoom=2) %>% 
      addPolygons(data = geo2 , 
                  fillColor = ~pal(geo2$Democracy_Index), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb)})}