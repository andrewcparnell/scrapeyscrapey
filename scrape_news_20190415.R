# Scrape some news sites

# Boiler plate code
rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest', 'xml2')

# Get today's date
today = format(Sys.time(), "%Y%m%d%H")

# Set up container for all data
all_data = vector(mode = 'list')

# Set all the urls here to make is easier:
url_df = data.frame(
  name = c('Irish Times', 
           'Irish Independent',
           'RTE',
           'Irish Mirror',
           'Breaking News',
           'The Journal',
           'Irish Examiner',
           'Irish Sun',
           'The Herald'),
  url = c("https://www.irishtimes.com/cmlink/news-1.1319192",
          "https://www.independent.ie/breaking-news/rss/",
          "https://www.rte.ie/news/rss/news-headlines.xml",
          "https://www.irishmirror.ie/rss.xml",
          "http://feeds.breakingnews.ie/bntopstories?format=xml",
          "http://www.thejournal.ie/feed/?x=1",
          "http://feeds.examiner.ie/ietopstories",
          "https://www.thesun.ie/feed",
          "https://www.herald.ie/rss"),
  stringsAsFactors = FALSE
)

# 1. Irish Times ----------------------------------------------------------

num = 1
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 2. Irish Indep ----------------------------------------------------------

num = 2
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 3. RTE ------------------------------------------------------------------

num = 3
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)


# 4. Irish Mirror ---------------------------------------------------------

num = 4
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 5 Breaking news ---------------------------------------------------------

num = 5
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 6. The Journal ----------------------------------------------------------

num = 6
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 7. Irish Examiner -------------------------------------------------------

num = 7
url = url_df[num,2]
news_page = read_html(url)

# Time/Date
time = news_page %>% 
  html_nodes("entry updated") %>% 
  html_text %>%
  parse_datetime(format = "%Y-%m-%dT%H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% 
  html_nodes("entry link") %>% 
  html_attr("href")

# Headline
head = news_page %>% 
  html_nodes("entry title") %>%
  html_text

# Description
desc = news_page %>% 
  html_nodes("entry summary") %>%
  html_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# 8. Irish Sun ------------------------------------------------------------

num = 8
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# # 9. NewsTalk -------------------------------------------------------------
# 
# NEWSTALK DON"T HAVE A VALID PAGE ANYMORE!
# num = 9
# url = url_df[num,2]
# news_page = read_xml(url)
# 
# # Time/Date
# time = news_page %>% xml_nodes('item pubDate') %>%
#   xml_text %>%
#   parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
#                  locale = locale(tz = 'GB'))
# 
# # URL
# link = news_page %>% xml_nodes("item link") %>%
#   xml_text
# 
# # Headline
# head = news_page %>% xml_nodes("item title") %>%
#   xml_text
# 
# # Description
# desc = news_page %>% xml_nodes("item description") %>%
#   xml_text
# 
# all_data[[num]] = data.frame(
#   site = rep(url_df[num,1],length(time)),
#   time = time,
#   link = link,
#   head = head,
#   desc = desc,
#   stringsAsFactors = FALSE
# )

# 10. Herald --------------------------------------------------------------

num = 9
url = url_df[num,2]
news_page = read_xml(url)

# Time/Date
time = news_page %>% xml_nodes('item pubDate') %>%
  xml_text %>%
  parse_datetime(format = "%+ %d %b %Y %H:%M:%S %Z",
                 locale = locale(tz = 'GB'))

# URL
link = news_page %>% xml_nodes("item link") %>%
  xml_text

# Headline
head = news_page %>% xml_nodes("item title") %>%
  xml_text

# Description
desc = news_page %>% xml_nodes("item description") %>%
  xml_text

all_data[[num]] = data.frame(
  site = rep(url_df[num,1],length(time)),
  time = time,
  link = link,
  head = head,
  desc = desc,
  stringsAsFactors = FALSE
)

# Tidy up and save --------------------------------------------------------

# Turn into rds
news_all = do.call(rbind, all_data)
saveRDS(news_all, file = paste0('news',today,'.rds'))

# Test whether it worked
#news = readRDS(paste0('news',2019041716,'.rds'))
#tibble::glimpse(news)

# auf::packages('tidyverse')
# ggplot(news, aes(x = site, y = nchar(head))) +
#   geom_boxplot() +
#   coord_flip()

