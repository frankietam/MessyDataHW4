## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Crime_Library.R

source('crime_library.R')


## Retrieve data from urls
crime.data.full <- foreach(i=1:nrow(url.data), .combine='rbind') %dopar% {
  
  # Retrieve html content
  content <- content_to_parsed_html(get_site_content(url.data$url[i]))
  # Retrieve crime type
  crime.type <- extract_all_crime_type(content)
  # Retrieve crime hour
  crime.date <- extract_all_crime_date(content)
  # Create crime neighborhood
  crime.neighborhood <- rep_len(url.data$neighborhood[i], length(crime.type))
  
  crime.data.temp <- cbind(crime.type, crime.date, crime.neighborhood)
  
  crime.data.temp
}

crime.data <- crime.data.full
# Convert to tibble
crime.data <- as.tibble(crime.data)

# Remove white space and \n in crime type
crime.data <- crime.data %>% mutate(crime.type=str_trim(crime.type))

# Convert to date string to POSIXct and extract hour from date
crime.data <- crime.data %>% mutate(crime.date=mdy_hm(crime.date)) %>% mutate(hour=hour(crime.date))

# Remove crime.date column
crime.data <- select(crime.data, -crime.date)

# Rename columns
names(crime.data) <- c("crime", "neighborhood", "hour")
