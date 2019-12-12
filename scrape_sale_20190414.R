# See if I can scrape daft
# This one does rented accommodation

# Boiler plate code -------------------------------------------------------

rm(list = ls())
#devtools::install_github('andrewcparnell/auf')
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest')

# Get today's date
today = format(Sys.time(), "%Y%m%d")

# Example scrape ----------------------------------------------------------

# Take main page and extract the links
url = "https://www.daft.ie/ireland/property-for-sale/?s%5Badvanced%5D=1&searchSource=sale"
daft_page <- read_html(url) %>%
  html_nodes(".PropertyImage__mainImageContainer > a") %>%
  html_attr('href')

# Now visit each link and see if I can get the details
property_url = paste0("https://www.daft.ie/", daft_page[7])
property_html = read_html(property_url)

# Want to extract price, number of bedrooms/bathrooms,
# address, details, date entered, ber etc

# Price
property_html %>%
  html_nodes(".PropertyInformationCommonStyles__costAmountCopy") %>%
  html_text %>%
  parse_number

# Eircode
property_html %>%
  html_nodes(".PropertyMainInformation__eircode") %>%
  html_text %>%
  str_squish

# Address
property_html %>%
  html_nodes(".PropertyMainInformation__address") %>%
  html_text()

# No of bedrooms
property_html %>%
  html_nodes(".QuickPropertyDetails__iconCopy") %>%
  html_text() %>%
  parse_number

# No of bathrooms
property_html %>%
  html_nodes(".QuickPropertyDetails__iconCopy--WithBorder") %>%
  html_text() %>%
  parse_number

# Type of house
property_html %>%
  html_node(".QuickPropertyDetails__propertyType") %>%
  html_text(trim = TRUE)

# Lat and long?
property_html %>%
  html_text(trim = TRUE) %>%
  str_match("MultiMaps.init(.*?)zoom") %>%
  '[['(1) %>%
  str_split("latitude") %>%
  unlist %>%
  str_split("longitude") %>%
  '[['(2) %>%
  parse_number

# Sale type
property_html %>%
  html_node(".PropertyOverview__propertyOverviewDetails") %>%
  html_text(trim = TRUE) %>%
  str_split('\\n') %>%
  unlist %>%
  '['(1)

# Floor area
property_html %>%
  html_node(".PropertyOverview__propertyOverviewDetails") %>%
  html_text(trim = TRUE)

# Full description
property_html %>%
  html_node(".PropertyDescription__propertyDescription") %>%
  html_text(trim = TRUE) %>%
  str_squish

# Features?
property_html %>%
  html_node(".PropertyFacilities__facilitiesList") %>%
  html_text(trim = TRUE) %>%
  str_squish

# Date enterred/reviewed
property_html %>%
  html_node(".PropertyStatistics__iconData") %>%
  html_text(trim = TRUE) %>%
  readr::parse_date(format = "%d.%m.%Y")

# Mumber of views
property_html %>%
  html_nodes(".PropertyStatistics__iconData") %>%
  html_text(trim = TRUE) %>%
  '['(2) %>%
  parse_number

# Agent
property_html %>%
  html_nodes(".AgentDetails__agentHeader") %>%
  html_text(trim = TRUE)

# BER
property_html %>%
  html_nodes("img") %>%
  html_attr('src') %>%
  str_match("ber\\_(.*?)\\.svg") %>%
  na.omit %>%
  '['(1,2)

# Number of images
property_html %>%
  html_nodes(".PropertyImage__picturesAmountCopy") %>%
  html_text(trim = TRUE) %>%
  parse_number

# YES!

# Full scrape -------------------------------------------------------------

# Full data frame will have columns:
# url
# price
# eircode
# address
# bedrooms
# bathrooms
# type
# lat/long
# sale type
# floor area
# Description
# features
# date entered_reviewed
# views
# agent
# ber
# images


all_data = vector(mode = 'list')

# Need to keep going until we reach the end
still_going = TRUE
count = 1

while(still_going) {
  print(count)
  
  # It does 20 per page and the offset is the key
  offset = ifelse(count == 1, 0, offset + length(main_page_all))
  curr_url = paste0("https://www.daft.ie/ireland/property-for-sale/?s%5Badvanced%5D=1&searchSource=sale&offset=", offset)
  
  # Get all the links
  main_page = read_html(curr_url) %>%
    html_nodes(".PropertyImage__mainImageContainer > a") %>%
    html_attr('href')
  main_page2 = read_html(curr_url) %>%
    html_nodes(".PropertyImage__mainImageContainerStandard > a") %>%
    html_attr('href')
  main_page_all = c(main_page, main_page2) %>% unique

  # Break out if we've reached the end
  if(length(main_page_all) == 0) {
  #if(count == 5) {
    still_going = FALSE
  } else {
    # Set everything up
    all_data[[count]] = data.frame(
      "url" = paste0("https://www.daft.ie", main_page_all),
      price = NA,
      eircode = NA,
      address = NA,
      bedrooms = NA,
      bathrooms = NA,
      type = NA,
      lat = NA,
      long = NA,
      sale_type = NA,
      floor_area = NA,
      description = NA,
      features = NA,
      date_entered_reviewed = as.Date(Sys.time()),
      views = NA,
      agent = NA,
      ber = NA,
      num_images = NA,
      stringsAsFactors = FALSE)
    
    pb = progress::progress_bar$new(
      format = "Progress [:bar]  :current/:total (:percent)",
      width = 60, total = length(main_page_all))
    
    # Now loop through each url 
    for(j in 1:length(main_page_all)) {
      pb$tick()
      
      curr_property_url = paste0("https://www.daft.ie", main_page_all[j])
      
      # Load it in
      property_html = read_html(curr_property_url) 
      
      # Price
      all_data[[count]]$price[j] = property_html %>% 
        html_nodes(".PropertyInformationCommonStyles__costAmountCopy") %>%
        html_text
      
      # Eircode
      eircode = property_html %>% 
        html_nodes(".PropertyMainInformation__eircode") %>%
        html_text %>%
        parse_character %>%
        str_squish
      all_data[[count]]$eircode[j] = ifelse(is.null(eircode),
                                            NA,
                                            eircode)
      
      # Address
      all_data[[count]]$address[j] = property_html %>% 
        html_nodes(".PropertyMainInformation__address") %>%
        html_text()
      
      # No of bedrooms
      bedrooms = property_html %>% 
        html_nodes(".QuickPropertyDetails__iconCopy") %>%
        html_text() %>%
        parse_number
      all_data[[count]]$bedrooms[j]= ifelse(is.null(bedrooms),
                                            NA,
                                            bedrooms)
      
      # No of bathrooms
      bathrooms = property_html %>% 
        html_nodes(".QuickPropertyDetails__iconCopy--WithBorder") %>%
        html_text() %>%
        parse_number
      all_data[[count]]$bathrooms[j] = ifelse(is.null(bathrooms),
                                              NA,
                                              bathrooms)
      
      # Type of house
      all_data[[count]]$type[j] = property_html %>% 
        html_node(".QuickPropertyDetails__propertyType") %>%
        html_text(trim = TRUE)
      
      # Lat and long?
      lat_long = property_html %>% 
        html_text(trim = TRUE) %>%
        str_match("MultiMaps.init(.*?)zoom") %>%
        '[['(1) %>%
        str_split("latitude") %>%
        unlist %>%
        str_split("longitude") %>%
        '[['(2) %>%
        parse_number
      all_data[[count]]$lat[j] = lat_long[1]
      all_data[[count]]$long[j] = lat_long[2]
      
      # Sale type
      all_data[[count]]$sale_type[j] = property_html %>% 
        html_node(".PropertyOverview__propertyOverviewDetails") %>%
        html_text(trim = TRUE) %>% 
        str_split('\\n') %>%
        unlist %>%
        '['(1)
      
      # Floor area
      all_data[[count]]$floor_area[j] = property_html %>% 
        html_node(".PropertyOverview__propertyOverviewDetails") %>%
        html_text(trim = TRUE)
      
      # Full description
      all_data[[count]]$description[j] = property_html %>% 
        html_node(".PropertyDescription__propertyDescription") %>%
        html_text(trim = TRUE) %>%
        str_squish
      
      # Features?
      all_data[[count]]$features[j] = property_html %>% 
        html_node(".PropertyFacilities__facilitiesList") %>%
        html_text(trim = TRUE) %>%
        str_squish
      
      # Date enterred/reviewed
      all_data[[count]]$date_entered_reviewed[j] = property_html %>% 
        html_node(".PropertyStatistics__iconData") %>%
        html_text(trim = TRUE) %>%
        readr::parse_date(format = "%d.%m.%Y")
      
      # Mumber of views
      all_data[[count]]$views[j] = property_html %>% 
        html_nodes(".PropertyStatistics__iconData") %>%
        html_text(trim = TRUE) %>%
        '['(2) %>%
        parse_number
      
      # Agent
      agent = property_html %>% 
        html_nodes(".AgentDetails__agentHeader") %>%
        html_text(trim = TRUE)
      all_data[[count]]$agent[j] = ifelse(is.null(agent),
                                          NA,
                                          agent)
      
      # BER  
      ber = property_html %>% 
        html_nodes("img") %>%
        html_attr('src') %>%
        str_match("ber\\_(.*?)\\.svg") %>%
        na.omit
      all_data[[count]]$ber[j] = ifelse(nrow(ber) == 0,
                                        NA,
                                        ber[1,2])
      
      # Number of images
      all_data[[count]]$num_images[j] = property_html %>% 
        html_nodes(".PropertyImage__picturesAmountCopy") %>%
        html_text(trim = TRUE) %>%
        parse_number

    }
    
    # Write it out to a file as we go to save re-doing this
    write_csv(all_data[[count]], 
              path = paste0('daft_sale_',today,'.csv'), 
              append = count!=1)
    count = count + 1
  }
}

# Turn into rds
daft_all = do.call(rbind, all_data)
saveRDS(daft_all, file = paste0('daft_sale_',today,'.rds'))
system(paste("rm",paste0('daft_sale_',today,'.csv')))

# Test whether it worked
#daft = readRDS(paste0('daft_sale_',today,'.rds'))
#tibble::glimpse(daft)

