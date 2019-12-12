# Scrape the information on which stations are used/available

# Boiler plate code -------------------------------------------------------

rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest', 'tidykml')

# Get today's date
now = format(Sys.time(), "%Y%m%d%H%M")

# Chagen definition of table to somethign sensible
table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)

# Get main details --------------------------------------------------------

# Want to extract all the current sites and find out the details about them

url = "https://esb.ie/ECARS/kml/charging-locations.kml"
ecars_page = read_html(url) %>% 
  html_children

# Get names
names = ecars_page %>% 
  html_nodes("name") %>% 
  html_text

# Descriptions - this gives more or less everything else!
descriptions = ecars_page %>%
  html_nodes("description") %>%
  html_text %>% 
  tolower

# Proper scrape -----------------------------------------------------------

# Should create a final data frame with:
all_data = data.frame(
  time_stamp = rep(now, length(names)),
  name = names,# Name
  lat = NA,# Lat
  long = NA,# Long
  chademo = NA,# ChaDeMo Yes/No
  chademo_pwr = NA,# ChaDemo power
  chademo_status = NA, # Find status
  ccs_combo = NA, # CCS Combo Yes/No
  ccs_combo_pwr = NA,# CCS Combo power
  ccs_combo_status = NA, # Find status
  ac43 = NA,# AC43 Yes/No
  ac43_pwr = NA,# AC43 power
  ac43_status = NA, # Find status
  type2ac = NA,# Type 2 ac Yes/No
  type2ac_pwr = NA,# AC43 power
  t2ac_any_available = NA, # Find status - there's often more than one charger here so just find if any are available
  total_chargers = NA, # total number of chargers at this site
  hr24 = NA,# 24 hour access  yes/no
  free_parking = NA,# Free parking yes/no
  details = NA# Other details (phone number etc)
)

# Extract lat and long
latlong = descriptions %>%
  str_match("long:(.*?)google") %>% 
  '['(,2) %>% 
  str_split(',') %>% 
  do.call(rbind, .)
all_data$lat = latlong %>% '['(,1) %>% parse_number
all_data$long = latlong %>% '['(,2) %>% parse_number
# Check
# qplot(long, lat, data = all_data)

# Find out whether its chademo or not
chademo = descriptions %>%
  str_locate("chademo") 
all_data$chademo = !is.na(chademo[,1])
table(all_data$chademo)

# Find the chademo power
all_data$chademo_pwr = descriptions %>%
  str_match("chademo(.*?)\\(") %>% 
  '['(,2) %>% 
  parse_number
  # Check
with(all_data, table(chademo_pwr,chademo))

# Find its status
all_data$chademo_status = descriptions %>%
  str_match("chademo(.*?)\\)") %>% 
  '['(,2) %>% 
  str_split("\\(") %>%
  do.call(rbind, .) %>% 
  '['(,2)
table(all_data$chademo_status)

# Find out whether its ccs combo or not
ccs_combo = descriptions %>%
  str_locate("combo dc") 
all_data$ccs_combo = !is.na(ccs_combo[,1])
table(all_data$ccs_combo)

# Find the power
all_data$ccs_combo_pwr = descriptions %>%
  str_match("combo dc(.*?)\\(") %>% 
  '['(,2) %>% 
  parse_number
# Check
with(all_data, table(ccs_combo_pwr,ccs_combo))

# Find its status
all_data$ccs_combo_status = descriptions %>%
  str_match("combo dc(.*?)\\)") %>% 
  '['(,2) %>% 
  str_split("\\(") %>%
  do.call(rbind, .) %>% 
  '['(,2)
table(all_data$ccs_combo_status)

# Finally for AC43
ac43 = descriptions %>%
  str_locate("fast ac") 
all_data$ac43 = !is.na(ac43[,1])
table(all_data$ac43)

# Find the power
all_data$ac43_pwr = descriptions %>%
  str_match("fast ac \\(type\\-2\\)(.*?)kw") %>% 
  '['(,2) %>% 
  parse_number
# Check
with(all_data, table(ac43_pwr,ac43))

# Find its status
all_data$ac43_status = descriptions %>%
  str_match("fast ac \\(type\\-2\\)(.*?)\\)") %>% 
  '['(,2) %>% 
  str_split("\\(") %>%
  do.call(rbind, .) %>% 
  '['(,2)
table(all_data$ac43_status)

# Finally finally the slower Type 2 AC
t2ac = descriptions %>%
  str_locate("type\\-2 ac socket") 
all_data$t2ac = !is.na(t2ac[,1])
table(all_data$t2ac)

# Find the power
all_data$t2ac_pwr = descriptions %>%
  str_match("type\\-2 ac socket(.*?)kw") %>% 
  '['(,2) %>% 
  parse_number
# Check
with(all_data, table(t2ac_pwr,t2ac))

# Find its status
t2ac_status = descriptions %>%
  str_match("type\\-2 ac socket(.*?)\\)") %>% 
  '['(,2) %>% 
  str_locate("available")
all_data$t2ac_any_available = !is.na(t2ac_status[,1])

# Now find total number of chargers at the site
total_t2ac = descriptions %>%
  str_match("type\\-2 ac socket(.*?)\\)") %>% 
  '['(,2) %>% 
  str_match("\\((.*?)\\,") %>% 
  '['(,2) %>% 
  parse_number %>%
  replace_na(0)
table(total_t2ac)

# Get the total number of chargers at a site
all_data$total_chargers = with(all_data,
                               total_t2ac + chademo + ccs_combo + ac43)
# Fix thoose with a t2ac charger but not multiple ones
all_data$total_chargers[all_data$t2ac == TRUE 
                        & all_data$total_chargers == 0] = 1
table(all_data$total_chargers)

# 24 hour access
hr24 = descriptions %>%
  str_locate("24 hour")
all_data$hr24 = !is.na(hr24[,1])
table(all_data$hr24)

# Free parking
free_parking = descriptions %>%
  str_locate("free parking")
all_data$free_parking = !is.na(free_parking[,1])
table(all_data$free_parking)

all_data$details = descriptions %>% str_squish

saveRDS(all_data, file = paste0('ecar_scrape_',now,'.rds'))
