#V 0.2.2
library(tictoc)
library(httr)
library(jsonlite)
library(dplyr)
library(abbreviate)
library(googleCloudStorageR)
library(googleErrorReportingR)

# Base working directory
base_dir <- '/home/technology/gbif-org-relationship-visualization/'

# Base API domain
base_api <- 'https://api.fairsharing.org/'

# Google Cloud Storage setup. Access json available at /secrets
gcs_global_bucket("bicikl_org_data")

# Initialize the error message component
message <- format_error_message()

# Set any of the message components to your own value
message$serviceContext$service <- "GBIF Data Extraction"
message$serviceContext$version <- "v0.3.1"


# Get bearen token using user credentials
login <- function(username = '', userpassword = '') {
  #Get user credentials and return a valide bearer token 
  #If success returns a Bearer token, else, returns an error, a message or an empty string
  
  
  #Initialize the credentials array using input
  credentials <-
    list(user = list(login = username, password = userpassword))
  
  #Fairshare API: Sign In method
  api_sign_in <- paste(base_api, 'users/sign_in', sep = "")
  
  #Try to login
  response <- tryCatch({
    userdata <-
      POST(api_sign_in, body = credentials, encode = 'json', verbose())
    
    #Parse the result from endpoint
    userdata_txt  <- content(userdata, "text")
    userdata_json <- fromJSON(userdata_txt, flatten = TRUE)
    
    #Get the login success flag from the result. TRUE is a valid login
    login <- userdata_json[["success"]]
    
    if ((!is.null(login)) && (login == TRUE)) {
      #Parse and asemble the bearer token for the session
      token <- userdata_json[["jwt"]]
      btoken <- paste("Bearer ", token, sep = "")
    }
    
  },error = function(err) { 
    message$message <- paste("Login: Use of login API failed.", err)
    googleErrorReportingR::report_error(message)
    
    return(NA)
    }
  )
  
  if (!is.na(response) && !is.null(login) && (login == TRUE)) {
    return(btoken)
    
  } else {
    if (length(userdata_json[["message"]]) > 0) { 
      response <- userdata_json[["message"]]
    } else {
      if (length(userdata_json[["error"]]) > 0) { 
        response <- userdata_json[["error"]]
      }
    }
    return(NA)
  }
  
}


# Get the full list of organisations available from the API
get_organisations <- function(token = '') {
  api_link <- paste(base_api, 'search/organisations', sep = '')
  
  #Call the API link
  entity <- POST(api_link, add_headers(Authorization = token))
  
  #Parse the API result on JSON format and get attributes list
  entity_txt  <- content(entity, "text")
  entity_json <- fromJSON(entity_txt, flatten = TRUE)
  
  #Extract the organization IDs and name in a new dataframe
  df_orgs <- data.frame(id = entity_json$data$id, name = entity_json$data$attributes.name)
  
  #Extract the alternative name list in alt column 
  df_orgs$alt <- ifelse(df_orgs$id == entity_json$data$id, as.character(entity_json$data$attributes.alternative_names[1]), NA)
  
  #Create the abbr if not alternative name available
  for (row in 1:nrow(df_orgs)) {
    if (identical("character(0)",df_orgs$alt[row])) {
      
      id <- df_orgs$id[row]
      # Compare the ID of the organization with the list of partners. Create a pseudo abbr if not
      df_orgs$abbr[row] <-  case_when(
        id == '302' ~ 'BGBM',
        id == '908' ~ 'EMBL-EBI',
        id == '947' ~ 'CERN',
        id == '1166' ~ 'GBIF',
        id == '2682' ~ 'SIB',
        id == '3380' ~ 'CETAF',
        id == '3379' ~ 'MBG',
        id == '2094' ~ 'Naturalis',
        id == '2305' ~ 'Pensoft',
        id == '3378' ~ 'Plazi',
        id == '2614' ~ 'Sp2000',
        id == '3385' ~ 'TDWG',
        id == '3132' ~ 'TU',
        TRUE ~ create_abbr(df_orgs$name[row])
        )
      
    } else {
      df_orgs$abbr[row] <- df_orgs$alt[row]
    }
  }

  return(unique(df_orgs))
}


# Create an organization abbreviation
create_abbr <- function(name = '') {

  if (grepl('\\(|\\)',name)) {
    abbr <- gsub(".*\\((.+)\\).*","\\1",name)
  } else {
    #abbr <- gsub('\\b(\\pL)\\pL{1,}|.','\\U\\1',name,perl = TRUE)
    abbr <- as.character(abbreviate_text(c(name), minlength = 5))
  }
  return(abbr)
}


# Get the relationships from an organization or a project
get_entity_relationships <-
  function(entity_id = '0', entity_type = 'org', token = '') {
    #Get the organisations_links array from an organization or fairsharing_record depending of entity_type
    
    if (entity_type == 'org') {
      api_link <- paste(base_api, 'organisations/', entity_id, sep = "")
    } else {
      #Only two types of entities are supported: organisations and fairsharing_records
      api_link <-
        paste(base_api, 'fairsharing_records/', entity_id, sep = "")
    }
    
    #Call the API link
    entity <- GET(api_link, add_headers(Authorization = token))
    
    #Parse the API result on JSON format and get attributes list
    entity_txt  <- content(entity, "text")
    entity_json <- fromJSON(entity_txt, flatten = TRUE)
    
    #If a valid request was made and the API get valid results 
    if ((is.null(entity_json$error)) && (is.null(entity_json$message))) {
      
      #Get the organisation_links dataframe from the organization
      entity_links <- entity_json$data$attributes$organisation_links
      
      if (entity_type == 'org') { 
        #Extract organisation alternative name
        if (length(entity_json$data$attributes$alternative_names) > 0) {
          entity_links$org_abbr <- as.character(entity_json$data$attributes$alternative_names[1])
        } else {
          entity_links$org_abbr <- NA
        }
        #Clean unnecessary columns from organisation_links dataframe
        entity_links$organisation_countries <- NULL
        
      } else {
        entity_links$fr_name <- entity_json$data$attributes$metadata$name
        if (!is.null(entity_json$data$attributes$metadata$abbreviation)){
          entity_links$fr_abbr <- entity_json$data$attributes$metadata$abbreviation
        } else {
          entity_links$fr_abbr <- abbreviate(entity_json$data$attributes$metadata$name)
        }
        # Geth Subjects ontology for this project
        entity_links$fr_subjects <- paste(entity_json$data$attributes$subjects, collapse = '| ', sep = '| ')
        
        # Geth Domains ontology for this project
        #entity_links$fr_domains <- paste(entity_json$data$attributes$domains,collapse = '|', sep = '|')
        
        # Geth Project abbreviation
        entity_links$org_abbr <- NA
      }
      
      #Clean unnecessary columns from organisation_links dataframe
      entity_links$link_id <- NULL
      entity_links$grant_id <- NULL
      entity_links$grant_name <- NULL
      entity_links$organisation_types <- NULL
      
      #entity_links$relation <- NULL  #Type of relation between organization and fairsharing_record
      
      #Rename columns
      entity_links <- rename(entity_links, 
                             org_name = organisation_name, 
                             org_id = organisation_id, 
                             fr_id = fairsharing_record_id) %>% 
        relocate(any_of(c("org_id","org_name","org_abbr","relation","fr_id","fr_name","fr_abbr")))
      
      return(unique(entity_links))
      
    } else {
      #An error occurred
      return(NULL) 
    }
  }

# Get the next level of relationships from an organization
get_org_pairs <- function(id_vector = c(), final_df = NULL, token = '') {
  #id_vector is a vector of organisation ids to get fairsharing records
  #final_df is the dataframe were the new relationships are going to be included
  #token is the bearer token to access de Fairsharing API
  
  #Remove duplicates before loop
  org_id_vector <- unique(id_vector)
  
  for (org_id in org_id_vector) {
    if ((is.null(final_df$org_id.y)) ||
        (any(org_id != final_df$org_id.x))) {
      #If the final_df is NULL is the first level of relationships, continue
      #If the current org_id is already in the organisation_id.x vector, the organization had already been queried
      org_links <- get_entity_relationships(org_id, 'org', token)
      
      if (!is.null(org_links)) {
        #If the org_links return a valid dataframe
        
        for (row in 1:nrow(org_links)) {
          #from_org_id <- org_links[row, 'organisation_id']
          #from_org_name <- org_links[row, 'organisation_name']
          
          #Get the fairsharing_record id for the current row
          fr_id <- org_links[row, 'fr_id']
          
          if ((is.null(final_df$fr_id)) ||
              (any(fr_id != final_df$fr_id))) {
              #If the final_df is NULL is the first level of relationships, continue
              #If the current fr_id is already in the fairsharing_record_id vector, the record had already been queried
            
            fr_links <- get_entity_relationships(fr_id, 'fr', token)
            
            if (!is.null(fr_links)) {
              #Data join using fairsharing_record_id as key
              new_df <- merge(org_links, fr_links, by = 'fr_id')
              
              #Append the new data to the cumulative dataset
              tryCatch({
                final_df <- rbind(final_df, new_df)
              }, error= function(e){
                print(paste("Failed FR:", fr_id))
                print("New DF:")
                colnames(new_df)
                print("Final DF:")
                colnames(final_df)
              })
              
            } else {
              #An error occurred during the get_entity_relationships call
            }
            
          } else {
            #The fairsharing_record had been already queried. Skip the API call
            print(paste("FR repetido:", fr_id))
          }
        }
        
      } else {
        #An error occurred during the get_entity_relationships call
      }
    }
  }
  
  #return the relationships with other than itself
  return(final_df[which(final_df$org_id.x != final_df$org_id.y),])
  
}

tic("Total time")
currentDate <- Sys.Date()
print(paste("Current date:", currentDate))

tic("Login")
#TESTING with fixed credentials
token <- login("cpravia", "ExtendoPrivado!!")
toc(log = TRUE)

if (length(token) > 0) {
  
  #Get full organisation list with abbr
  tic("Update Full Organization List")
  full_org_list <- get_organisations(token)
  result <- tryCatch({
    full_org_filename <- paste(base_dir,'data/full_org_list.csv', sep = "")
    write.csv(full_org_list, full_org_filename)
    
  },warning = function(war) {
    message$message <- paste("Warning: Writing Full Organization List:", err)
    googleErrorReportingR::report_error(message)
    return(NA)
    
  }, error = function(err) {
    message$message <- paste("Error: Writing Full Organization List:", err)
    googleErrorReportingR::report_error(message)
    return(NULL)
  } )
  
  if (!is.na(result) && !is.null(result)) {
    # Get local file date for comparison
    full_org_file_cdate <- as.Date(file.info(full_org_filename)$ctime)
    
    if (full_org_file_cdate == currentDate) {
      gcs_upload(full_org_filename, name='csv/full_org_list.csv', predefinedAcl='bucketLevel')
    } else {
      message$message <- paste("GCS Upload failed for", full_org_filename)
      googleErrorReportingR::report_error(message)
    }
  }
  
  toc(log = TRUE)
  
  #Read 14 BiCKL organisation list for 
  tic("Read BiCKL Organization List")
  organisation_list <- read.csv(file = paste(base_dir,'data/org_list.csv', sep=''))
  toc(log = TRUE)
  
  for (row in 1:nrow(organisation_list)) {
    #Get the organisation_id from list
    org_id <- organisation_list[row,'org_id']
    
    #First level of relationships
    tic(paste("Procesing Organization:", org_id, "1st level"))
    org_df_1st <- get_org_pairs(c(org_id), NULL, token)
    toc(log = TRUE)
    
    #Second level of relationships
    tic(paste("Procesing Organization:", org_id, "2nd level"))
    org_df_2nd <-
      get_org_pairs(org_df_1st$org_id.y, org_df_1st, token) %>% 
      relocate(fr_id, .before = fr_name)
    toc(log = TRUE)
    
    #Data cleaning and de-duplication
    #org_df_2nd$fairsharing_record_id <- NULL
    tic(paste("Clean 2nd level, let only unique rows for Organization:", org_id))
    org_df2_clean <- unique(org_df_2nd)
    toc(log = TRUE)
    
    tic(paste("Find Organization Abbreviations from master list ", org_id))
    for (org_row in 1:nrow(org_df2_clean)) {
      org_df2_clean$org_abbr.x[org_row] <- full_org_list$abbr[full_org_list$id == org_df2_clean$org_id.x[org_row]]
      org_df2_clean$org_abbr.y[org_row] <- full_org_list$abbr[full_org_list$id == org_df2_clean$org_id.y[org_row]]
    }
    toc(log = TRUE)
    
    # Write the file locally
    tic(paste("Write CSV:", org_id))
    csv_filename <- paste(base_dir,'data/org_',org_id,'.2.csv',sep = '')
    write.csv(org_df2_clean,csv_filename)
    toc(log = TRUE)
    
    # Upload the file to Cloud Storage
    tic(paste("Upload CSV:", org_id))
    gcs_filename <- paste('csv/org_',org_id,'.2.csv',sep = '')
    
    # Get local file date for comparison
    file_cdate <- as.Date(file.info(csv_filename)$ctime)
    
    if (file_cdate == currentDate) {
      gcs_upload(csv_filename, name=gcs_filename, predefinedAcl='bucketLevel')
    } else {
      message$message <- paste("GCS Upload failed for", gcs_filename)
      googleErrorReportingR::report_error(message)
    }
    toc(log = TRUE)    
  }
}

toc(log = TRUE)
