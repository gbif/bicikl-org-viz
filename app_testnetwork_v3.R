library(shiny)
library(shinyjs)
library(DT)
library(shinycssloaders)
library(igraph)
library(dplyr)
library(tidyverse) # tidy style
library(sigmajs) # visualize network: http://sigmajs.john-coene.com/
library(skimr) # describe data
library(readr) # read data
library(abbreviate) # to get unique abbreviations
library(threejs) # another visual

# Input data preparation
# Read 14 BiCKL organisation list for
org_list <- read.csv(file = 'data/org_list.csv')
org_key_value  <- setNames(as.list(org_list$org_id), org_list$org)
org_key_value  <-
  append(c('Select an Organization' = 0), org_key_value)

full_org_list <- read_csv("data/full_org_list.csv") %>% 
  mutate(abbr = abbreviate_text(name,minlength = 5),
         label = abbr)

get_dataframe <- function(org_id = 0, level = 2, exclutions = TRUE) {
  # Read the dataset for the selected organization with two levels of relationships
  file_name <- paste('data/org_', org_id, '.', '2.csv', sep = '')
  
  if (file.exists(file_name)) {
    # Read the preloaded dataframe with the desire level of depth
    t <- read.csv(file_name)
    if (exclutions == TRUE) {
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
    return(NULL)
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Use ShinyJS to enable UX interactions
  useShinyjs(),
  
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
      ),
      width = 3 # Sidebar width, by default is 4 (1/3 of 12 units)
    ),
    
    mainPanel(
      htmlOutput("h3_org"), # Show the selected organization name and Id
      #withSpinner(
      DT::dataTableOutput('relationships_table'),
      #  type = 4,
      #  color = "#4787fb",
      #  size = 1
      #)
      #,
      #withSpinner(
      sigmajsOutput('org_plot'),
      #  type = 4,
      #  color = "#4787fb",
      #  size = 1
      #)
      #actionButton('btn_update', 'Update Graph'),
      width = 12 # Mainpanel width, by default is 8 (2/3 of 12 units)
    )
    
  ),
  includeHTML('www/footer.html'), 
  position = c("left", "right"),
  fluid = TRUE
)


server <- function(input, output) {
  #dataTable <- reactive({get_dataframe(input$filter_org_list,input$filter_depth)})
  
  shinyjs::hide("btn_update")
  
  observeEvent(input$btn_visualize, {
    if (input$filter_org_list != 0) {
      # Get the corresponding data for the selected organization
      dataTable <- get_dataframe(org_id = input$filter_org_list, 
                                 level = input$filter_depth, 
                                 exclutions = input$chk_exclutions)
      
      if (!is.null(dataTable)) {
        selected_org_name <- dataTable$org_name.x[dataTable$org_id.x == input$filter_org_list][1]
        output$h3_org <-
          renderText(
            paste(
              "<p><strong>",
              selected_org_name,
              " </strong>(",
              nrow(dataTable),
              " relationships)</p>",
              sep = ""
            )
          )
        
        
        # Remove the first unnecessary column from the dataframe
        dataTable[1] <- NULL
        
        # Sort the table by the organization name. If depth level is 1, by the related organization, if 2, by the origin organization
        if (input$filter_depth == 1) {
          dataTable <- arrange(dataTable, org_name.y)
        } else {
          dataTable <- arrange(dataTable, org_name.x)
        }
        
        # Remove underscore character (_) from the relation text: "collaborates_on" to "collaborates on"
        dataTable$relation.x <- sub("_"," ",dataTable$relation.x)
        dataTable$relation.y <- sub("_"," ",dataTable$relation.y)
        
        # Create valid organization URL in X
        dataTable$org_name.x <-
          paste(
            '<a href="https://fairsharing.org/organisations/',
            dataTable$org_id.x,
            '" target="_blank" data-toggle="tooltip" title="Visit the Org. page at Fairsharing.org">',
            dataTable$org_name.x,
            '</a>',
            sep = ''
          )
        
        # Create valid organization URL in Y
        dataTable$org_name.y <-
          paste(
            '<a href="https://fairsharing.org/organisations/',
            dataTable$org_id.y,
            '" target="_blank" data-toggle="tooltip" title="Visit the Org. page at Fairsharing.org">',
            dataTable$org_name.y,
            '</a>',
            sep = ''
          )
        
        # Create valid fairshring project URL
        dataTable$fr_abbr <- paste(
          '<a href="https://fairsharing.org/',
          dataTable$fr_id,
          '" target="_blank" data-toggle="tooltip" title="',
          dataTable$fr_name,
          '">',
          dataTable$fr_abbr,
          '</a>',
          sep = ''
        )
        
        # dataTable$org_id.x <- NULL
        # dataTable$org_abbr.x <- NULL
        # dataTable$org_id.y <- NULL
        # dataTable$org_abbr.y <- NULL
        
        dataTable$fr_id <- NULL
        dataTable$fr_name <- NULL
        dataTable$fr_subjects <- NULL
        
        # Rearranging column
        dataTable <- relocate(dataTable,org_abbr.x,org_name.x,relation.x,fr_abbr,relation.y,org_abbr.y,org_name.y, org_id.x, org_id.y)
        dataTable <- dataTable %>% 
          mutate(relation.x = as.factor(relation.x),
                 relation.y = as.factor(relation.y),
                 org_abbr.x = as.factor(org_abbr.x),
                 org_abbr.y = as.factor(org_abbr.y))
        
        # Renaming column names
        #colnames(dataTable) <- c('Origin', 'related to', 'FR Project', 'related to', 'Destination', 'org_id.x', 'org_id.y')
        
        # Rendering the final table
        output$relationships_table <-
          DT::renderDataTable({
            datatable(
              select(in_react_data_frame(), c(org_abbr.x,org_name.x,relation.x,fr_abbr,relation.y,org_abbr.y,org_name.y)),
              colnames = c('Abbr','Origin', 'related to', 'FR Project', 'related to', 'Abbr','Destination'),
              filter = 'top',
              extensions = 'Buttons',
              options = list(pageLength = 5,
                             dom = 'Blrtip',
                             buttons =
                               list(
                                 list(
                                   extend = 'collection',
                                   buttons = c('csv', 'excel', 'pdf'),
                                   text = 'Download'
                                 )
                               ))
              ,
              escape = FALSE,
              selection = "single"
            )
            
          })
        shinyjs::show("btn_update")
        
        # Getting the filtered table
        
        in_react_data_frame <- reactiveVal(dataTable)
        
        filtered_dataTable <- reactive({
          table <- req(in_react_data_frame())
          indexes <- req(input$relationships_table_rows_all)
          
          table[indexes,]
        })
        
        # Creates the graph
        
        edges <- reactive({
          filtered_dataTable() %>%
            mutate(from = org_id.x,
                   to = org_id.y) %>%
            select(from, to)
        })
        
        # Hacer un vector con todos los nodos (valores únicos en  from y to de 
        # edges). Este es el componente id del data.frame nodes
        
        nodes_from <- reactive({
          edges() %>% 
            select(from) 
        })
        
        
        nodes_from_vector <- reactive({
          edges() %>% 
            select(from) %>% 
            pull()
        })
        
        names_from <- reactive({
          filtered_dataTable() %>% 
            filter(org_id.x %in% nodes_from_vector()) %>% 
            mutate(id = org_id.x,
                   label = org_abbr.x,
                   organization = org_name.x) %>% 
            select(id,label,organization)
        })
        
        nodes_to  <- reactive({
          edges() %>% 
            select(to)
        })
        
        nodes_to_vector  <- reactive({
          edges() %>% 
            select(to) %>% 
            pull()
        })
        
        names_to <- reactive({
          filtered_dataTable() %>% 
            filter(org_id.y %in% nodes_to_vector()) %>% 
            mutate(id = org_id.y,
                   label = org_abbr.y,
                   organization = org_name.y) %>% 
            select(id,label,organization)
        })
        
        # Para calcular size contar cuantas veces cada nodo aparece como to en 
        #`edges.
        
        # También podemos calcular como from
        
        size_from <- reactive({
          nodes_from() %>%
            group_by(from) %>% 
            tally()  
        })
        
        size_to <- reactive({
          nodes_to() %>%
            group_by(to) %>% 
            tally()  
        })  
        
        # nodos unicos:
        
        nodes_id <- reactive({
          c(nodes_from_vector(), nodes_to_vector()) %>% 
            unique() %>% 
            as_tibble()
        })
        
        
        # id + size
        
        node_size <- reactive({
          left_join(nodes_id(),size_from(),by = c("value" = "from" )) %>% 
            rename(id = value,
                   size = n) %>%
            mutate(size = ifelse(is.na(size), 0, size))
        })
        
        # labels:
        
        names_all <- reactive({
          bind_rows(names_to(),names_from()) %>% 
            distinct()
        })
        
        # Para obtener label, hacer un join con los dato 
        
        
        # id + size + label
        
        
        
        node_size_label <- reactive({
          left_join(node_size(), names_all(),
                    by = c("id" = "id"))
        })
        
        
        # cat(file = stderr(), "org_id.x", filtered_dataTable()$org_id.x[1] , "\n")
        # cat(file = stderr(), "org_abbr.x", filtered_dataTable()$org_abbr.x[1] , "\n")
        # cat(file = stderr(), "org_name.x", filtered_dataTable()$org_name.x[1] , "\n")
        # cat(file = stderr(), "node_size_label", node_size_label()[1,4] , "\n")
        
        network <- reactive({
          graph_from_data_frame(d = edges(),
                                vertices = node_size_label(),
                                directed = FALSE)
        })
        
        color_pal2 <- rainbow(2, alpha = .5)
        
        output$org_plot <- renderSigmajs(
          
          sigmajs() %>%
            sg_from_igraph(network()) %>%
            sg_settings(drawLabels = TRUE, drawEdgeLabels = FALSE) %>%
            sg_layout(layout = igraph::layout_nicely) %>%
            sg_cluster(colors = color_pal2)  %>%
            sg_cluster(colors = hcl.colors(10, "Set 2"))  %>%
            sg_settings(
              minNodeSize = 3,
              maxNodeSize = 8.0,
              edgeColor = "default",
              defaultEdgeColor = "#d3d3d3",
              labelThreshold = 5           
            )
        )
        
      } else {
        output$h3_org <-
          renderText("<p>No organization selected. ")
      }
      
    }
  }) 
  
}

# Run the application
shinyApp(ui = ui, server = server)
