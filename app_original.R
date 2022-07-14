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
library(arcdiagram)
library(igraph)
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
        return(default)
    }   
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Custom CSS
    includeCSS('www/style.css'),
    
    # Application title
    includeHTML('www/header.html'),
               
    
    
    # Sidebar with filter options
    sidebarLayout(
        sidebarPanel(
            selectInput('filter_org_list','Organization',org_key_value),
            selectInput('filter_depth','Depth Level',c('First level' = 1,'Second level' = 2)),
            actionButton('btn_visualize','Visualize'),
            HTML(paste('<div class="credits">',
                       '<p style="text-align:center;">',
                       '<a href="https://fairsharing.org/" target="_blank">',
                       '<img src="https://api.fairsharing.org/img/fairsharing-attribution.svg" alt="FAIRsharing Logo" width="50%" height="50%">',
                       '</a></p>',
                sep = "")),
            HTML(paste("<p>",
                       "Working under the auspices of the ",
                       tags$a(href="https://www.allianceforbio.org/", target="_blank", "alliance for biodiversity knowledge"),
                       ", GBIF is responsible for producing a deliverable that identifies potential stakeholders and partners relevant to BiCIKL.</p>",
                       sep=""
                       )),
            HTML(paste("<p>",
                       '<a href="https://fairsharing.org/" target="_blank">',
                       "<strong>FAIRsharing.org</strong></a> ",
                       "has provided critical assistance to this work by supplying curated information on open data resources, standards and policies. This data, which GBIF and the BiCIKL partners are both contributing to and drawing from, has served as a core information resource for a network graph analysis that reveals potential target communities for BiCIKL's outreach and educational activities.</p>",
                       sep=""
                       )),
            HTML(paste(
                "<p>",
                '<a href="https://doi.org/10.1038/s41587-019-0080-8" target="_blank">',
                "Learn more about <strong>FAIRsharing.org</strong>.</a>",
                "</div>",
                sep = ""))
            
        ),
        
        # Show the selected organization name and ID
        mainPanel(
            htmlOutput("h3_org"),
            tabsetPanel(id = "tabs", type = "tabs",
                        tabPanel("Data table",
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
    hideTab(inputId = "tabs", target = "Data table")
    hideTab(inputId = "tabs", target = "Graph")
    
    #dataTable <- reactive({get_dataframe(input$filter_org_list,input$filter_depth)})
    
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
            
            # Remove the first unnecessary column from the dataframe
            dataTable[1] <- NULL
            
            # Prepare dataTable depending of the depth level
            if (input$filter_depth == 1) {
                dataTable<- select(dataTable,organisation_name.y,organisation_id.y,organisation_abbr.y)

                # Sorting by related organization name
                dataTable <- arrange(dataTable,organisation_name.y)
                
                # Create valid organization URL in Y
                dataTable$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.y,'" target="_blank">',dataTable$organisation_name.y,'</a>', sep='')
                
                # Renaming column names
                colnames(dataTable) <- c('Related To', 'ID', 'Abbr')
                
            } else {
                # Sorting by related organization name
                dataTable <- arrange(dataTable,organisation_name.y,dataTable$organisation_name.y)
                
                # Create valid organization URL in X and Y
                dataTable$organisation_name.x <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.x,'" target="_blank">',dataTable$organisation_name.x,'</a>', sep='')
                dataTable$organisation_name.y <- paste('<a href="https://fairsharing.org/organisations/',dataTable$organisation_id.y,'" target="_blank">',dataTable$organisation_name.y,'</a>', sep='')
                
                # Renaming column names
                colnames(dataTable) <- c('From', 'From ID', 'From Abbr', 'To', 'To ID', 'To Abbr')            
            }
            
            # Rendering the final table
            output$relationships_table <- DT::renderDataTable({datatable(dataTable,
                                                                         extensions = 'Buttons', options = list(
                                                                             dom = 'Blfrtip',
                                                                             buttons = 
                                                                                 list(list(
                                                                                     extend = 'collection',
                                                                                     buttons = c('csv', 'excel', 'pdf'),
                                                                                     text = 'Download'
                                                                                 ))),
                                                                         escape = FALSE,
                                                                         selection = "single"
                                                                         )})
            
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
