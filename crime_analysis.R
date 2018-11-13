## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Crime_analysis.R

source('crime_library.R')

## Part A

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

## Part B

#visualize the list of crimes to easily spot spelling mistakes
unique.crime <- sort(unique(crime.data$crime))

#remove rows with empty "crime" value
crime.data <- crime.data %>%  filter(crime.data$crime != "")

#Fix spelling errors for "Shooting" and "Assault" crime types
crime.data <- crime.data %>% mutate(crime = case_when(
  crime=="Shootin" ~ "Shooting",
  crime=="Shoting" ~ "Shooting",
  crime=="Shotting" ~ "Shooting",
  crime=="shooting" ~ "Shooting",
  crime=="Assaul" ~ "Assault",
  TRUE ~ crime
))

#observe the 5 most common crime types accross all neighborhoods and hours
crime.data %>% count(crime) %>% arrange(desc(n)) %>% slice(1:5)

## Part C

#count number of total crimes by hour
crime.by.hour <- crime.data %>% group_by(hour) %>% count(hour)

#plot total number of crimes by hour
ggplot(crime.by.hour, aes(x = hour, y = n)) + 
  geom_col() + xlab(NULL) + coord_flip()

## Part D

#keep only observations for the 5 most common crime types
top.5.crimes <- crime.data %>% filter(crime.data$crime == "Shooting" | 
                                        crime.data$crime == "Gunfire"| 
                                        crime.data$crime == "Murder" | 
                                        crime.data$crime == "Stabbing" | 
                                        crime.data$crime == "Illegal gun possession" )

#count number of crimes by crime and by hour
top.5.crimes.by.hour <-  top.5.crimes %>% group_by(crime) %>% count(hour)

#plot total number of crimes by hour, for each of the 5 most common crime types
ggplot(data=top.5.crimes.by.hour, aes(x=hour, y=n, group = crime, colour = crime)) +
  geom_line() 

## Part E

#keep only observations for the neighborhoods of "Dorchester" and "Downtown"
question.E <- crime.data %>% filter(crime.data$neighborhood == "Dorchester" | 
                                      crime.data$neighborhood == "Downtown")

#count number of crimes by neighborhood and by hour
question.E.by.hour <- question.E %>% group_by(neighborhood) %>% count(hour)

#plot total number of crimes by hour, for each of the two selected neighborhoods
ggplot(data=question.E.by.hour, aes(x=hour, y=n, group = neighborhood, colour = neighborhood)) +
  geom_line() 

