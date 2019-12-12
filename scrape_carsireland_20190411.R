# Some code to scrape data from the carsireland website

# Boiler plate code -------------------------------------------------------

rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest')

# Get today's date
today = 20190411#format(Sys.time(), "%Y%m%d")

# Example scrape ----------------------------------------------------------

url = "https://www.carsireland.ie/search-results.php?location_id[]=p2&min_price=10000&min_year=2016&max_year=2019&per_page=50&page=4#listings-top"
ci <- getURL(url)

urls_raw = str_match_all(ci, "<a href=\"(.*?)\">more")[[1]][,2]
urls_raw[1]

new_url = paste0("https://www.carsireland.ie/", urls_raw[1])

# Extract all the characters after the string "results"

ci_by_car = read_html(new_url) %>% html_table()
# YES!

# Full scrape -------------------------------------------------------------

# Full data frame will have columns:
# ID
# url
# num_images
# make
# model
# title
# features
# mileageKm
# engine
# price
# year
# reg
# location

# First find the maximuum number of pages
url_1 = "https://www.carsireland.ie/search-results.php?location_id[]=p2&min_price=10000&min_year=2016&max_year=2019&per_page=50&page=0#listings-top"
#ci = getURL(url)

# Extract out the maximum number of pages
ci_max = getURL(url_1)
max_cars = str_match(ci_max, "Displaying 1 to 50 of (.*?) used cars")[,2]
final_max_pages = floor(as.numeric(max_cars)/50)

# Now create the blank data frame
all_data = vector('list', length = final_max_pages)

# # Function to extract price
extract_price = function(ll) {
  ll %>% str_match_all("[0-9]+") %>% unlist %>% paste0(collapse = '') %>% as.numeric
}
# 
# extract_mileage = function(ll) {
#   ll$mileageKm %>% str_match_all("[0-9]+") %>% unlist %>% paste0(collapse = '') %>% as.numeric
# }

for(i in 1:final_max_pages) {
  #pb$tick()
  print(i)
  #i = 499; stop()
  
  # Create the url
  curr_url = paste0("https://www.carsireland.ie/search-results.php?location_id[]=p2&min_price=10000&min_year=2016&max_year=2019&per_page=50&page=",i-1,"#listings-top")
  ci = getURL(curr_url)
  
  # Extract all the pages of the files
  car_urls = str_match_all(ci, "<a href=\"(.*?)\">more")[[1]][,2]
  
  all_data[[i]] = data.frame(
    "url" = paste0("https://www.carsireland.ie/", car_urls),
    "make" = NA,
    "model" = NA,
    "year" = NA,
    "price" = NA,
    "mileage_km" = NA,
    "fuel_type" = NA,
    "engine_size" = NA,
    "colour" = NA,
    "body_type" = NA,
    "owners" = NA,
    "transmission" = NA,
    "seats" = NA,
    "tax" = NA,
    "economy" = NA,
    "last_update" = NA,
    "dealer" = NA,
    "features" = NA,
    stringsAsFactors = FALSE)
  
  pb = progress::progress_bar$new(
    format = "Progress [:bar]  :current/:total (:percent)",
    width = 60, total = length(car_urls))
  
  # Now loop through each url 
  for(j in 1:length(car_urls)) {
    pb$tick()
    curr_car_url = paste0("https://www.carsireland.ie/", car_urls[j])
    curr_ci_car = read_html(curr_car_url) %>% html_table()
    rownames(curr_ci_car[[1]]) = curr_ci_car[[1]][,1]
    rownames(curr_ci_car[[2]]) = curr_ci_car[[2]][,1]
    desc = read_html(curr_car_url) %>% html_nodes("p") %>% html_text()
    addr = read_html(curr_car_url) %>% html_nodes("address") %>% html_text()
    
    # Extract out the info
    all_data[[i]]$make[j] = curr_ci_car[[1]]["Make:",2]
    all_data[[i]]$model[j] = curr_ci_car[[1]]["Model:",2]
    all_data[[i]]$year[j] = curr_ci_car[[1]]["Year:",2]
    all_data[[i]]$price[j] = extract_price(curr_ci_car[[1]]["Price:",2])
    all_data[[i]]$mileage_km[j] = curr_ci_car[[1]]["Odometer:",2]
    all_data[[i]]$fuel_type[j] = curr_ci_car[[1]]["Fuel type:",2]
    all_data[[i]]$engine_size[j] = curr_ci_car[[1]]["Engine size:",2]
    all_data[[i]]$colour[j] = curr_ci_car[[1]]["Colour:",2]
    all_data[[i]]$body_type[j] = curr_ci_car[[1]]["Body:",2]
    all_data[[i]]$owners[j] = curr_ci_car[[1]]["Owners:",2]
    all_data[[i]]$transmission[j] = curr_ci_car[[1]]["Transmission:",2]
    all_data[[i]]$seats[j] = curr_ci_car[[1]]["Seats:",2]
    all_data[[i]]$tax[j] = curr_ci_car[[1]]["Annual road tax:",2]
    all_data[[i]]$economy[j] = curr_ci_car[[1]]["Fuel economy:",2]
    all_data[[i]]$last_update[j] = curr_ci_car[[1]]["Last update:",2]
    all_data[[i]]$dealer[j] = addr
    all_data[[i]]$features[j] = desc[2]
  }
  
  # Write it out to a file as we go to save re-doing this
  write_csv(all_data[[i]], 
            path = paste0('carsireland/carsireland_',today,'.csv'), 
            append = i!=1)
}
