library(shiny)

shinyUI(fluidPage(
        #Title
        titlePanel("US Baby Name Explorer"), 
        br(),
        
        #Sidebar
        sidebarLayout(
            sidebarPanel(
                uiOutput("choose_babyname"),
                br(),
                sliderInput("range", "2. Period : ", min = 1960, max = 2008, value = c(1960, 2008), sep=""),              
                br(),
                uiOutput("choose_states"),     
                actionButton(inputId = "select_all", label = "Select All", icon = icon("check-square-o")) 
            ),
            
            mainPanel(
              tabsetPanel(
                # for info
                tabPanel(
                    p(icon("bookmark"), "About App"),
                    includeMarkdown("include.md")              
                ),
                
                # by state
                tabPanel(
                    p(icon("map-marker"), "By State"),
                    br(),
                    "Info : the result(%) of each state is scaled by division of total number in US.",
                    plotOutput("ByState"),
                    "p.s : Alaska and Hawaii are excluded from the map." 
                ),
                
                # by year
                tabPanel(
                    p(icon("line-chart"), "By Year"),
                    br(),
                    "The trends of name by states are presented in the chart below.",
                    plotOutput("ByYear")       
                )
                
                
              )  
            )
        )
))
   