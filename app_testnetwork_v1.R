library(shiny)
library(DT)
library(shinycssloaders)
library(arcdiagram)
library(igraph)
library(dplyr)
library(tidyverse) # tidy style
library(sigmajs) # visualize network: http://sigmajs.john-coene.com/
library(skimr) # describe data
library(readr) # read data
library(abbreviate) # to get unique abbreviations
library(threejs) # another visu

# Input data preparation
# Read 14 BiCKL organisation list for
org_list <- read.csv(file = 'data/org_list.csv')
org_key_value  <- setNames(as.list(org_list$org_id), org_list$org)
org_key_value  <-
  append(c('Select an Organization' = 0), org_key_value)

get_dataframe <- function(org_id = 0, level = 2, exclutions = TRUE) {
  # Create the file iname
  file_name <- paste('data/org_', org_id, '.', '2.csv', sep = '')
  
  if (file.exists(file_name)) {
    # Read the preloaded dataframe with the desire level of depth
    t <- read.csv(file_name)
    if (exclutions == TRUE){
      t <- t[t$relation.x != 'undefined',]
      t <- t[t$relation.x != 'funds',]
      t <- t[t$relation.y != 'undefined',]
      t <- t[t$relation.y != 'funds',]
    }
    if (level == 1) {
      t <- t[t$org_id.x == org_id,]
    }
    return(t)
    
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
      selectInput('filter_org_list', 'Organization', org_key_value),
      selectInput(
        'filter_depth',
        'Depth Level',
        c('First level' = 1, 'Second level' = 2)
      ),
      checkboxInput("chk_exclutions","Exclude undefined and fund type relationships",value = TRUE),
      actionButton('btn_visualize', 'Visualize'),
      HTML(
        paste(
          '<div class="credits">',
          '<p style="text-align:center;">',
          '<a href="https://fairsharing.org/" target="_blank">',
          '<img src="https://api.fairsharing.org/img/fairsharing-attribution.svg" alt="FAIRsharing Logo" width="50%" height="50%">',
          '</a></p>',
          sep = ""
        )
      ),
      HTML(
        paste(
          "<p>",
          "Working under the auspices of the ",
          tags$a(
            href = "https://www.allianceforbio.org/",
            target = "_blank",
            "alliance for biodiversity knowledge"
          ),
          ", GBIF is responsible for producing a deliverable that identifies potential stakeholders and partners relevant to BiCIKL.</p>",
          sep = ""
        )
      ),
      HTML(
        paste(
          "<p>",
          '<a href="https://fairsharing.org/" target="_blank">',
          "<strong>FAIRsharing.org</strong></a> ",
          "has provided critical assistance to this work by supplying curated information on open data resources, standards and policies. This data, which GBIF and the BiCIKL partners are both contributing to and drawing from, has served as a core information resource for a network graph analysis that reveals potential target communities for BiCIKL's outreach and educational activities.</p>",
          sep = ""
        )
      ),
      HTML(
        paste(
          "<p>",
          '<a href="https://doi.org/10.1038/s41587-019-0080-8" target="_blank">',
          "Learn more about <strong>FAIRsharing.org</strong>.</a>",
          "</div>",
          sep = ""
        )
      )
      
    ),
    # Show the selected organization name and I
    mainPanel(
      htmlOutput("h3_org"),
      withSpinner(
        DT::dataTableOutput('relationships_table'),
        type = 4,
        color = "#4787fb",
        size = 1
      ),
      withSpinner(
        sigmajsOutput('org_plot'),
        type = 4,
        color = "#4787fb",
        size = 1
      )
    )
    
    
  ),
  includeHTML('www/footer.html'), 
  position = c("left", "right"),
  fluid = TRUE
  
)


server <- function(input, output) {
  #dataTable <- reactive({get_dataframe(input$filter_org_list,input$filter_depth)})
  
  observeEvent(input$btn_visualize, {
    if (input$filter_org_list != 0) {
      # Get the corresponding data for the selected organization
      dataTable <-
        get_dataframe(input$filter_org_list, input$filter_level, input$chk_exclutions)
      
      output$h3_org <-
        renderText(
          paste(
            "<p>Organization: ",
            input$filter_org_list,
            " (",
            nrow(dataTable),
            " relationships)</p>",
            sep = ""
          )
        )
      
      # Create the arc plot
      dset <-
        select(dataTable, org_id.x, org_abbr.x, org_id.y, org_abbr.y)
      nodes <- dset %>%
        group_by(org_id.y) %>%
        tally() %>%
        mutate(id = org_id.x,
               size = n) %>%
        select(id, size)
      edges <- dset %>%
        mutate(from = org_id.x,
               to = org_id.y) %>%
        select(from, to)
      network <-
        graph_from_data_frame(d = edges,
                              directed = F,
                              vertices = nodes)
      color_pal2 <- rainbow(2, alpha = .5)
      output$org_plot <- renderSigmajs(
        sigmajs() %>%
          sg_from_igraph(network) %>%
          #sg_settings(drawLabels = TRUE, drawEdgeLabels = FALSE) %>%
          sg_layout(layout = igraph::layout_nicely) %>%
          sg_cluster(colors = color_pal2)  %>%
          sg_cluster(colors = hcl.colors(10, "Set 2"))  %>%
          sg_settings(
            minNodeSize = 1,
            maxNodeSize = 5.0,
            edgeColor = "default",
            defaultEdgeColor = "#d3d3d3",
            labelThreshold = 5,
            labelThreshold = 3
          ) %>%
          sg_neighbours()
      )
      
      # Remove the first unnecessary column from the dataframe
      dataTable[1] <- NULL
      
      # Prepare dataTable depending of the depth level
      if (input$filter_depth == 1) {
        dataTable <-
          dataTable <-
          select(dataTable, org_name.y, org_id.y, org_abbr.y)
        
        # Sorting by related organization name
        dataTable <- arrange(dataTable, org_name.y)
        
        # Create valid organization URL in Y
        dataTable$org_name.y <-
          paste(
            '<a href="https://fairsharing.org/organisations/',
            dataTable$org_id.y,
            '" target="_blank">',
            dataTable$org_name.y,
            '</a>',
            sep = ''
          )
        
        # Renaming column names
        colnames(dataTable) <- c('Related To', 'ID', 'Abbr')
        
      } else {
        # Sorting by related organization name
        dataTable <-
          arrange(dataTable,
                  org_name.y,
                  dataTable$org_name.y)
        
        # Create valid organization URL in X and Y
        dataTable$org_name.x <-
          paste(
            '<a href="https://fairsharing.org/organisations/',
            dataTable$org_id.x,
            '" target="_blank">',
            dataTable$org_name.x,
            '</a>',
            sep = ''
          )
        dataTable$org_name.y <-
          paste(
            '<a href="https://fairsharing.org/organisations/',
            dataTable$org_id.y,
            '" target="_blank">',
            dataTable$org_name.y,
            '</a>',
            sep = ''
          )
        
        # Renaming column names
        colnames(dataTable) <-
          c('From', 'From ID', 'From Abbr', 'To', 'To ID', 'To Abbr')
      }
      
      # Rendering the final table
      output$relationships_table <-
        DT::renderDataTable({
          datatable(
            dataTable,
            extensions = 'Buttons',
            options = list(dom = 'Blfrtip',
                           buttons =
                             list(
                               list(
                                 extend = 'collection',
                                 buttons = c('csv', 'excel', 'pdf'),
                                 text = 'Download'
                               )
                             )),
            escape = FALSE,
            selection = "single"
          )
        })
      
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
