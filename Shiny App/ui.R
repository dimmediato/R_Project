# Load packages ----
library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(ggplot2)
library(geojsonio)
library(tidyverse)
library(leaflegend)
library(shinyWidgets)
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
# User interface ----
ui <- navbarPage(title = "Democratic Backsliding",
                 tabPanel(title = "World Map",
                          titlePanel("Democratic Backsliding: The Impossible Becoming Reality"),
                          sidebarPanel(width = 3, selectInput
                                       ("yearInput", label = h4("Liberal Democracy Index and the World"),
                                         choices = c("2023","2022","2021","2020","2019","2018","2017","2016","2015","2014",
                                                     "2013","2012","2011","2010","2009","2008","2007","2006","2005","2004",
                                                     "2003","2002","2001","2000","1999","1998","1997","1996","1995","1994",
                                                     "1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983"))),
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
                 )),
                 
                 # Trends Tab ---- 
                 
                 tabPanel(title = "Trends",
                   titlePanel("It Can Happen Here?"),
                   
                   sidebarLayout(
                     sidebarPanel(fluidRow(column(4,pickerInput("checkGroup1",
                                                     h5("Democracies"),
                                                     multiple = T,
                                                     options = list(`actions-box` = TRUE),
                                                     choices = c("Armenia" = "Armenia",
                                                                 "Austria" = "Austria",
                                                                    "Botswana" = "Botswana",
                                                                    "Brazil" = "Brazil",
                                                                    "Croatia" = "Croatia",
                                                                    "Cyprus" = "Cyprus",
                                                                    "Guatemala" = "Guatemala",
                                                                    "Guyana" = "Guyana",
                                                                    "Ghana" = "Ghana",
                                                                    "Greece" = "Greece",
                                                                    "Israel" = "Israel",
                                                                    "Lithuania" = "Lithuania",
                                                                    "Mexico" = "Mexico",
                                                                    "Mongolia" = "Mongolia",
                                                                    "Peru" = "Peru",
                                                                    "Poland" = "Poland",
                                                                    "Portugal" = "Portugal",
                                                                    "Romania" = "Romania",
                                                                    "Senegal" = "Senegal",
                                                                    "Slovakia" = "Slovakia",
                                                                    "Slovenia" = "Slovenia",
                                                                    "South Korea" = "South Korea",
                                                                    "Trinidad and Tobago" = "Trinidad and Tobago",
                                                                    "USA" = "United States of America"),
                                                     selected = c("Greece", "Ghana", "Poland", "Botswana", "United States of America"))),
                                           column(
                                             4,pickerInput("checkGroup2",
                                                                  h5("Electoral Autocracies"),
                                                                  multiple = T,
                                                           options = list(`actions-box` = TRUE),
                                                                  choices = c("Belarus" = "Belarus",
                                                                                 "Benin" = "Benin",
                                                                                 "Cambodia" = "Cambodia",
                                                                                 "CAR" = "Central African Republic",
                                                                                 "Comoros" = "Comoros",
                                                                                 "El Salvador" = "El Salvador",
                                                                                 "Hungary" = "Hungary",
                                                                                 "India" = "India",
                                                                                 "Kyrgyzstan" = "Kyrgyzstan",
                                                                                 "Lebanon" = "Lebanon",
                                                                                 "Mauritania" = "Mauritania",
                                                                                 "Mauritius" = "Mauritius",
                                                                                 "Nicaragua" = "Nicaragua",
                                                                                 "Niger" = "Niger",
                                                                                 "Nigeria" = "Nigeria",
                                                                                 "Pakistan" = "Pakistan",
                                                                                 "Philippines" = "Philippines",
                                                                                 "Serbia" = "Serbia",
                                                                                 "Sierra Leone" = "Sierra Leone",
                                                                                 "Tunisia" = "Tunisia",
                                                                                 "Turkey" = "Turkey",
                                                                                 "Ukraine" = "Ukraine",
                                                                              "USA" = "United States of America"),
                                                           selected = c("Comoros", "El Salvador", "Hungary", "India", "Mauritius", "Niger", "Philippines", "Serbia", "United States of America")
                                                           
                                                                                 )),
                                           column(
                                             4,pickerInput("checkGroup3",
                                                                  h5("Closed Autocracies"),
                                                                  multiple = T,
                                                           options = list(`actions-box` = TRUE),
                                                           selected = c("Afghanistan", "Burkina Faso", "Burma/Myanmar", "Chad", "Guinea", "Haiti", "Iran", "Libya", "Mali", "Sudan", "United States of America"),
                                                                  choices = c("Afghanistan" = "Afghanistan",
                                                                                 "Burkina Faso" = "Burkina Faso",
                                                                                 "Chad" = "Chad",
                                                                                 "Guinea" = "Guinea",
                                                                                 "Haiti" = "Haiti",
                                                                                 "Iran" = "Iran",
                                                                                 "Libya" = "Libya",
                                                                                 "Myanmar" = "Burma/Myanmar",
                                                                                 "Sudan" = "Sudan",
                                                                              "USA" = "United States of America",
                                                                                 "Uzbekistan" = "Uzbekistan",
                                                                                 "Yemen" = "Yemen")))),
                       ),
                     
                     # Trends Subtabset ---- 
                     
                     mainPanel(tabsetPanel(
                       type = "tabs",
                       tabPanel("Democracies",
                                id = "panel3", plotOutput("plot1", click = "plot_click1"), verbatimTextOutput("info")),
                       tabPanel("Electoral Autocracies",
                                id = "panel4",
                                plotOutput("plot2", click = "plot_click1"), verbatimTextOutput("info2")),
                       tabPanel("Closed Autocracies",
                                id = "panel5",
                                plotOutput("plot3", click = "plot_click1"), verbatimTextOutput("info3")),
                       tabPanel("Description",
                                id = "panel6",
                                sidebarPanel(width = 15, p("The V-Dem Project categorizes countries by their regime type, based on the democracy index. 
                                                           Democracies are those that reached the threshold of above .5, 
                                                           while an \"electoral autocracy\" is a regime that is .2 to the grey area of becoming a democracy, 
                                                           while a \"closed autocracy\" is a score of .2 and below."),
                                             p("Displayed is a graph of selected countries that either A) are moving towards an autocracy since 2013,
                                               or B) are currently in said autocratization, as of 2023. There are a total of 42 countries currently in a downward trend, and 36 countries deemed as moving towards autocracy since 2013. 20 are doing both."),
                                               
                                               p("Selected are those countries that are both moving towards autocracy and in a current downward trend. 
                                               The US, a liberal democracy that recently had a period of backsliding, was included as a contrast for each."))))
                     ))
                   ),
                 
                 # Populism Tab ---- 
                 
                 tabPanel(title = "Populism",
                          titlePanel("Words That Can Move Mountains"),
                          sidebarLayout( sidebarPanel (width =2, conditionalPanel(
                            condition = "input.example == 1 || input.example == 2 || input.example == 3", checkboxGroupInput("govsupportcheck", h3("Government Support?"),
                                                               choices = list("Senior Partner (0)" = "0", "Junior Partner (1)" = "1",
                                                                              "In Gov., but not Represented (2)" = "2",
                                                                              "In Opposition (3)" = "3"),selected = "0")),
                            conditionalPanel(condition = "input.example == 4", radioButtons(
                              "radio", h3("Type of Trend"), 
                              choices = list("Populism" = "v2xpa_popul",
                                             "Anti-Pluralism" = "v2xpa_antiplural")))),
                            
                            # Populism Subtabset ---- 
                            
                          mainPanel(tabsetPanel(
                            type = "tabs",
                            id = "example",
                            tabPanel("Trends",
                                     value = 4,
                                     plotOutput("partytrend", click = "party_trendclick"), verbatimTextOutput("trend")),
                            tabPanel("Populism",
                                     value = 1,
                                     plotOutput("partygraph", click = "plot_click2"), verbatimTextOutput("partyinfo")),
                            tabPanel("Anti-Pluralism",
                                       value = 2,
                                       plotOutput("partygraph2", click = "plot_click3"), verbatimTextOutput("partyinfo2")),
                            tabPanel("Description",
                                     value = 3,sidebarPanel(width = 15,  
                                     p("Populism is suggested to be the largest, most recent causes of democratic backsliding.
                                                                                        Populism is a rhetoric strategy, where the party incorporates anti-establishment and anti-elite speech into their rhetoric. They also speak of reducing political corruption. 
                                                                                        Populist parties can be pluralist or anti-pluralist, which is whether or not they commit to democratic norms."),
                                     p("First shown is a general trend in a steady increase of populist rhetoric of political parties in all countries,
                                     but anti-pluralist rhetoric has generally stayed the same on average. The V-Dem Project suggests that the two may be correlated."),
                                                                          p("Following the trends are two semi-log graphs, with x-log for the purpose of increasing R-squared. The Party Populism graph shows that, as populist rhetoric in countries increases, autocratization increases (or rather, the liberal democracy level decreases).
                                                                            Those not represented have the least impact in regards to populism, while those in opposition have the most impact."),
                                                                          p("For anti-pluralism, this trend is seen to be much worse. If these parties have an anti-pluralist rhetoric, then the autocratization is much more likely.
                                                                          However, different results than the populist trends were seen. Its strongest impact occurred when the party was in the government, and its weakest impact was, like with the populist trend, when in the government but not represented.
                                                                          No category here was statistically insignificant."))))
))),

# Polarization Tab ---- 

tabPanel(title = "Polarization",
         titlePanel("Torn Apart From the Inside"), 
         sidebarLayout(sidebarPanel(width =2, conditionalPanel( condition = "input.polarizationsubtab == 10",
           pickerInput("polarizationpicker",
                       "Eastern Europe:", choices= unique(Eastern_Europe$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, select = "Poland"),
           pickerInput("polarizationpicker2",
                       "Latin America:", choices= unique(Latin_America$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, "Bolivia"),
           pickerInput("polarizationpicker3",
                       "Middle East:", choices= unique(Middle_East$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, "Turkey"),
           pickerInput("polarizationpicker4",
                       "Africa:", choices= unique(Africa$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, "Zimbabwe"),
           pickerInput("polarizationpicker5",
                       "Western Europe:", choices= unique(Western_Europe$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, "United States of America"),
           pickerInput("polarizationpicker6",
                       "Asia:", choices= unique(Asia$country_name), 
                       options = pickerOptions(actionsBox = TRUE, liveSearch=T),  multiple = T, "Hong Kong")),
           conditionalPanel( condition = "input.polarizationsubtab == 11", radioButtons(
             "radio2", h3("Region"), 
             choices = list("Eastern Europe" = "1",
                            "Latin America" = "2",
                            "Middle East" = "3",
                            "Africa" = "4",
                            "Western Europe" = "5",
                            "Asia" = "6")))), 
           
           # Polarization Subtabset ----            
           
           mainPanel(tabsetPanel(
             type = "tabs",
             id = "polarizationsubtab",
             tabPanel("Polarization Trend",
                      value = 10,
                      plotOutput("polarizationtrend", click = "polarization_click"), verbatimTextOutput("polarizationinfo"),
                      ),
             tabPanel("Polarization Regression",
                      value = 11,
                      plotOutput("polarizationregression", click = "polarization_click2"), verbatimTextOutput("polarizationinfo2"),
             ),
             tabPanel("Description",
                       value = 11,sidebarPanel(width = 15,p("The V-Dem Project determined that political polarization has remained a significant factor in contributing to the decline in democracy level.
                                                            The trend here is different from the previous, showing an almost universal rise in polarization in the world (this was tested both on a country-level and on a world-level).
                                                            Highlighted are the most egregious examples in the \"political regions\" of the world, with the highlighted countries being the ones with the highest level of political polarization."),
                                               p("Next are the linear regressions for each region. Unlike for populism, log was not needed to enhance R-Squared. Also, all regions were statistically significant.
                                                 Here, Latin America and Western Europe seemed to have political polarization be the strongest as a factor, while in the Middle East political polarization seemed to matter the least.
                                                 This is likely blamed on the *lack* of polarization possible. Considering in the Middle East most countries are autocracies.")))
             )))
           
         # GINI Tab ---- 
),
tabPanel(title = "Inequality",
         titlePanel("Economic Inequality and Democratic Collapse"), 
         sidebarLayout(sidebarPanel(width =2, conditionalPanel( condition = "input.GINIsubtab == 20", pickerInput("GINIpicker",
                                                "Eastern Europe:", choices= unique(Eastern_Europe_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T),
                                    pickerInput("GINIpicker2",
                                                "Latin America:", choices= unique(Latin_America_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T, "Colombia"),
                                    pickerInput("GINIpicker3",
                                                "Middle East:", choices= unique(Middle_East_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T),
                                    pickerInput("GINIpicker4",
                                                "Africa:", choices= unique(Africa_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T),
                                    pickerInput("GINIpicker5",
                                                "Western Europe:", choices= unique(Western_Europe_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T),
                                    pickerInput("GINIpicker6",
                                                "Asia:", choices= unique(Asia_GINI$country_name), 
                                                options = pickerOptions(title= "Select One", actionsBox = TRUE, liveSearch=T),  multiple = T)),
                                    conditionalPanel( condition = "input.GINIsubtab == 21", radioButtons(
                                      "radio3", h3("Region"), 
                                      choices = list("Eastern Europe (1)" = "1",
                                                     "Latin America (2)" = "2",
                                                     "Middle East (3)" = "3",
                                                     "Africa (4)" = "4",
                                                     "Western Europe (5)" = "5",
                                                     "Asia (6)" = "6")))),
                       
                       # GINI Subtabset ---- 
                       
                       mainPanel(tabsetPanel(
                         type = "tabs",
                         id = "GINIsubtab",
                         tabPanel("Inequality Trend",
                                  value = 20,
                                  plotOutput("GINItrend", click = "GINI_click"), verbatimTextOutput("GINIinfo")),
                         tabPanel("Inequality Regression",
                                  value = 21,
                                  plotOutput("GINIregression", click = "GINI_regression"), verbatimTextOutput("GINIinfo2")),
                         tabPanel("Description",
                                  value = 21,
                                  sidebarPanel(width = 15, p("A claimed background cause to democratic backsliding is wealth inequality, most often measured by the Gini coefficient: the degree of income inequality in the distribution of wealth.
                                  The V-Dem Project had Gini as a background cause, but it was removed from the codebook. So, an alternative dataset was used. The results were mixed. With all countries combined, the regression has an extremely weak R-Squared, although is still statistically significant."),
                                               
                                  p("Separating by region is a similiar story, with R-Squared being minimal across the board. While the Asian region is the only region not statistically significant, 
                                    this factor stands out as the only one with no filter have a statistical significance < 2e-16 (Western Europe had the highest R-Squared and lowest statistical significance, though).
                                    The implication is that wealth may have an effect, but only if the effect can be noticed. Take Colombia for example, a country with fluctuating democracy index AND fluctuating wealth inequality, and how they reflect off of each other.")))
                         
                         ))))
                       

)
                 