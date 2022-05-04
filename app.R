#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(igraph)
library(arcdiagram)
library(dplyr)

# Input data preparation
# Read 14 BiCKL organisation list for 
org_list <- read.csv(file = 'data/org_list.csv')
org_key_value  <- setNames(as.list(org_list$org_id),org_list$org)
org_key_value  <- append(c('Select an Organization' = 0),org_key_value)

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
            selectInput('filter_org_list','Selected Organization',org_key_value),
            selectInput('filter_depth','Depth Level',c('First level' = 1,'Second level' = 2)),
            actionButton('btn_visualize','Visualize')
        ),
        
        # Show the selected organization name and ID
        mainPanel(
            htmlOutput("h3_org"),
            tabsetPanel(id = "tabs", type = "tabs",
                        tabPanel("Data table",
                                 withSpinner(dataTableOutput('relationships_table'),
                                             type = 4,
                                             color = "#4787fb",
                                             size = 1
                                 )
                        ),
                        tabPanel("Graph",
                                 withSpinner(plotOutput('org_plot'),
                                             type = 4,
                                             color = "#4787fb",
                                             size = 1
                                 )
                        )
            )
        )
    ),
    includeHTML('www/footer.html')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    hideTab(inputId = "tabs", target = "Data table")
    hideTab(inputId = "tabs", target = "Graph")
    
    observeEvent(input$btn_visualize, {
        if (input$filter_org_list != 0) {
            showTab(inputId = "tabs", target = "Data table")
            showTab(inputId = "tabs", target = "Graph")
            
            # Get the corresponding data for the selected organization
            dataTable <- get_dataframe(input$filter_org_list,input$filter_depth)
            
            output$h3_org <- renderText(paste("<p>Organization: ",input$filter_org_list," (",nrow(dataTable)," relationships)</p>", sep = ""))
            
            # Create the arc plot
            dset<-select(dataTable,organisation_abbr.x,organisation_abbr.y)
            un_graphe<-as.matrix(dset)
            output$org_plot<-renderPlot(arcplot(un_graphe, col.arcs = palette("ggplot2")))
            
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
            
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
