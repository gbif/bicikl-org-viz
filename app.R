#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Input data preparation
# Read 14 BiCKL organisation list for 
org_list <- read.csv(file = 'data/org_list.csv')
org_key_value  <- setNames(as.list(org_list$org_id),org_list$org)

get_dataframe <- function(org_id = 0,level = 1) {
    # Create the file iname
    file_name <- paste('data/org_',org_id,'.',level,'.csv', sep = '')
    
    #PENDING IF FILE NOT FOUND
    return(read.csv(file_name))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GBIF Organization Relationships"),

    # Sidebar with filter options
    sidebarLayout(
        sidebarPanel(
            selectInput('filter_org_list','Select an Organization',org_key_value),
            actionButton('btn_visualize','Visualize')
        ),

        # Show the selected organization name and ID
        mainPanel(
            dataTableOutput('relationships_table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$btn_visualize, {
        output$relationships_table <- renderDataTable(get_dataframe(input$filter_org_list))
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
