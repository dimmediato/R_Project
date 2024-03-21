# Load packages ----
library(shiny)
# User interface ----
ui <- navbarPage("Democratic Backsliding",
                 tabPanel("World Map",
                          titlePanel("Democratic Backsliding: The Impossible Becoming Reality"),
                          sidebarPanel(width = 3, selectInput
                                       ("yearInput", label = h4("Liberal Democracy Index and the World"),
                                         choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                                                     "2013","2012","2011","2010","2009","2008","2007","2006","2005","2004",
                                                     "2003","2002","2001","2000","1999","1998","1997","1996","1995","1994",
                                                     "1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983")))),
                 mainPanel(tabsetPanel(
                   type = "tabs",
                   tabPanel("Map",
                            id = "panel1",
                            leafletOutput("map")),
                   tabPanel("Description", id = "panel2", 
                            sidebarPanel(width = 15, p("Democratic backsliding is the erosion or deterioration of democratic governance overtime. It is hopefully the goal of most countries to achieve the ideal liberal democracy, achieving the 1 score. 
             However, year by year, the institutions that used to uphold the liberal principle of democracy begin to fade, being replaced with a new tyranny of the state, dragging them further to total autocracy, the 0 score. 
                            At its worst, backsliding can result in complete autocratization without even one realizing it. At best, one might see a reversal of the trend.", 
                                                       p("Unfortunately, the mid-2000s has now shown that even the most developed of democracies are falling victim to this trend.
                                          The Varieties of Democracy Institute generously provided data to visualize these trends, with the hope of pin-pointing a cause and a way to prevent further degradation."),
                                                       a("Full Codebook: https://github.com/dimmediato/R_Project/blob/master/Data/Full_Codebook.pdf"),
                                                       br(),
                                                       br(),
                                                       a("Party Codebook: https://github.com/dimmediato/R_Project/blob/master/Data/V-Dem_V-Party_Codebook_V2.pdf"),
                                                       br(),
                                                       br(),
                                                       a("V-Dem Website: https://v-dem.net/")))))
                 ))