#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinycssloaders)
library(igraph)
library(dplyr)
library(arcdiagram)

# Input data preparation
# Read 14 BiCKL organisation list for 
org_list <- read.csv(file = 'data/org_list.csv')
org_key_value  <- setNames(as.list(org_list$org_id),org_list$org)
org_key_value  <- append(c('Select an Organization' = 0),org_key_value)

get_dataframe <- function(org_id = 0,level = 1) {
    
    if (org_id != 0) {
        # Create the file iname
        file_name <- paste('data/org_',org_id,'.',level,'.csv', sep = '')
        
        if (file.exists(file_name)) {
            # Read the preloaded dataframe with the desire level of depth
            return(read.csv(file_name))
            
        } else {
            return(NULL)
        }   
    } else {
        return(NULL)
    }
}

prep_dataheading <- function(org_id = 0, org_data = NULL) {
    if (!is.null(org_data) && (org_id != 0)) { 
        org_name <- org_data$organisation_name.x[org_data$organisation_id.x == org_id][1]
        org_relations <- nrow(org_data)
        return (paste("<h3>",org_name,"</h3><p><strong>(",org_relations," relationships)</strong></p>", sep = ""))
    } else {
        return ('<p>No organization selected.</p>')
    }
}

prep_dataplot <- function(org_data = NULL) {
    if (!is.null(org_data)) {
        dset<-select(dataTable,organisation_abbr.x,organisation_abbr.y)
        data_matrix<-as.matrix(dset)
        return(data_matrix)
    } else {
        return(rbind(c('No','Organization'),c('Organization','Selected')))
    }
}

prep_datatable <- function(org_data = NULL, depth = 1) {
    if (! is.null(org_data)) {
        # Prepare dataTable depending of the depth level
        if (depth == 1) {
            org_data$organisation_id.x <- NULL
            org_data$organisation_name.x <- NULL
            org_data$organisation_abbr.x <- NULL
            
            # Sorting by related organization name
            org_data <- org_data[order(org_data$organisation_name.y),]
            
            # Create valid organization URL in Y
            org_data$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',org_data$organisation_id.y,'" target="_blank">',org_data$organisation_name.y,'</a>', sep='')
            
            # Renaming column names
            colnames(org_data) <- c('Related To', 'ID', 'Abbr')
            
        } else {
            # Sorting by related organization name
            org_data <- org_data[order(org_data$organisation_name.x,org_data$organisation_name.y),]
            
            # Create valid organization URL in X and Y
            org_data$organisation_name.x <- paste('<a href="https://fairsharing.org/organisations/',org_data$organisation_id.x,'" target="_blank">',org_data$organisation_name.x,'</a>', sep='')
            org_data$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',org_data$organisation_id.y,'" target="_blank">',org_data$organisation_name.y,'</a>', sep='')
            
            # Renaming column names
            colnames(org_data) <- c('From', 'From ID', 'From Abbr', 'To', 'To ID', 'To Abbr')            
        }
        
        return(org_data)
        
    } else {
        # Sets a default blank dataframe
        default <- data.frame(nodata = c('No Organization data available'))
        colnames(default) <- c('No Data')
        
        return(default)
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
            selectInput('filter_org_list','Organization',org_key_value),
            selectInput('filter_depth','Depth Level',c('First level' = 1,'Second level' = 2))
        ),
        
        # Show the selected organization name and ID
        mainPanel(
            htmlOutput("h3_org"),
            tabsetPanel(id = "tabs", type = "tabs",
                        tabPanel("Relationships",
                                 withSpinner(DT::dataTableOutput('relationships_table'),
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

server <- function(input, output) {
    # Get the corresponding data for the selected organization. If no selection then NULL
    dataSet <- reactive({get_dataframe(input$filter_org_list,input$filter_depth)})
    
    output$h3_org <- renderText(prep_dataheading(input$filter_org_list,dataSet()))
    
    dataPlot <- reactive({prep_dataplot(dataSet())})
    output$org_plot<-renderPlot(arcplot(dataPlot(), col.arcs = palette("ggplot2")))
        
    dataTable <- reactive({prep_datatable(dataSet(),input$filter_depth)})
    output$relationships_table <- DT::renderDataTable({datatable(dataTable(),
                                                                 escape = FALSE,
                                                                 selection = "single",
                                                                 options = list(
                                                                     columnDefs = list(
                                                                         list(visible = FALSE,
                                                                              targets=c(0))
                                                                     )
                                                                 )
    )})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
