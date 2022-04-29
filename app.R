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
    
    if (file.exists(file_name)) {
        # Read the preloaded dataframe with the desire level of depth
        return(read.csv(file_name))
        
    } else {
        return(NULL)
    }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Custom CSS
    includeCSS('www/style.css'),

    # Application title
    titlePanel("GBIF Organization Relationships"),

    # Sidebar with filter options
    sidebarLayout(
        sidebarPanel(
            selectInput('filter_org_list','Select an Organization',org_key_value),
            selectInput('filter_depth','Select the Depth',c('First level' = 1,'Second level' = 2)),
            actionButton('btn_visualize','Visualize')
        ),

        # Show the selected organization name and ID
        mainPanel(
            dataTableOutput('relationships_table')
        )
    ),
    includeHTML('www/footer.html')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$btn_visualize, {
        dataTable <- get_dataframe(input$filter_org_list,input$filter_depth)
        
        # Remove unnecessary columns from the dataframe
        dataTable$X <- NULL
        
        # Prepare dataTable depending of the depth level
        if (input$filter_depth == 1) {
            dataTable$organisation_id.x <- NULL
            dataTable$organisation_name.x <- NULL
            dataTable$organisation_abbr.x <- NULL
            
            # Sorting by related organization name
            dataTable <- dataTable[order(dataTable$organisation_name.y),]
            
            # Create valid organization URL in Y
            dataTable$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.y,'" target="_blank">',dataTable$organisation_name.y,'</a>', sep='')
            
            # Renaming column names
            colnames(dataTable) <- c('Related To', 'ID', 'Abbr')
            
        } else {
            # Sorting by related organization name
            dataTable <- dataTable[order(dataTable$organisation_name.x,dataTable$organisation_name.y),]
            
            # Create valid organization URL in X and Y
            dataTable$organisation_name.x <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.x,'" target="_blank">',dataTable$organisation_name.x,'</a>', sep='')
            dataTable$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.y,'" target="_blank">',dataTable$organisation_name.y,'</a>', sep='')
            
            # Renaming column names
            colnames(dataTable) <- c('From', 'From ID', 'From Abbr', 'To', 'To ID', 'To Abbr')            
        }
        
        # Rendering the final table
        output$relationships_table <- renderDataTable(dataTable,escape = FALSE)
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
