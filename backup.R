library(shiny)
#library(shiny)
library(shinydashboard)
# Define UI for data upload app ----



ui <-dashboardPage(#skin = "blue",
  
  #Header for the App, The sidebar and the menu items.
  dashboardHeader(title = "Recruitment Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(tags$head(tags$style(HTML('.content{
                                          background-color: white;
                                          } 
                                          .skin-blue .main-header .navbar{
                                          background-color:#003da5}
                                          .skin-blue .main-header .logo{
                                          background-color:#003da5                                  
                                          }
                                          .skin-blue .sidebar-menu > li.active > a, .skin-blue .sidebar-menu > li:hover > a{
                                          border-left-color:#003da5                                        
                                          }
                                          h1{
                                          font-family:"Cambria"
                                          }'))),
  
                tabItem(tabName = "Pop",
                        tags$h1("DSM+ Data Upload"),
                        fluidRow(
                          box(
                            
                            # Input: Select a file ----
                            fileInput("file1", "Upload New Demand Data",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))
                            
                            
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                            
                            # Output: Data file ----
                            tableOutput("contents")
                            
                          )
                          
                        )
                )))

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath
    )
    
    return(head(df))
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)