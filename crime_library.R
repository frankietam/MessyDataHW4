## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Crime_Library.R

library(httr)
library(xml2)
library(rvest)
library(stringr)
library(tidyverse)
library(foreach)
library(lubridate)

## URLs of crime pages and neigborhood
# urls <- read.table("urls.txt", header = FALSE)
# names(urls) <- c("url");

url <- c("https://www.universalhub.com/crime/allston.html",
          "https://www.universalhub.com/crime/back-bay.html",
          "https://www.universalhub.com/crime/beacon-hill.html",
          "https://www.universalhub.com/crime/brighton.html",
          "https://www.universalhub.com/crime/charlestown.html",
          "https://www.universalhub.com/crime/dorchester.html",
          "https://www.universalhub.com/crime/dorchester.html?page=1",
          "https://www.universalhub.com/crime/downtown.html",
          "https://www.universalhub.com/crime/east-boston.html",
          "https://www.universalhub.com/crime/fenway.html",
          "https://www.universalhub.com/crime/hyde-park.html",
          "https://www.universalhub.com/crime/jamaica-plain.html",
          "https://www.universalhub.com/crime/mattapan.html",
          "https://www.universalhub.com/crime/mission-hill",
          "https://www.universalhub.com/crime/north-end.html",
          "https://www.universalhub.com/crime/roslindale.html",
          "https://www.universalhub.com/crime/roxbury.html",
          "https://www.universalhub.com/crime/south-boston.html",
          "https://www.universalhub.com/crime/south-end.html",
          "https://www.universalhub.com/crime/west-roxbury.html"
          )

neighborhood <- c("Allston",
                  "Back Bay",
                  "Beacon Hill",
                  "Brighton",
                  "Charlestown",
                  "Dorchester",
                  "Dorchester",
                  "Downtown",
                  "East Boston",
                  "Fenway",
                  "Hyde Park",
                  "Jamaica Plain",
                  "Mattapan",
                  "Mission Hill",
                  "North End",
                  "Roslindale",
                  "Roxbury",
                  "South Boston",
                  "South End",
                  "West Roxbury")

url.data <- tibble(url, neighborhood)

## Extract from UHubScript.R

## FUNCTIONS TO GET THE XML PARSED HTML FROM SITE
get_site_content <- function( url ){
  require( httr )
  # get the site response
  response <- httr::GET( url )
  # extract the content
  content <- httr::content( x = response, as = 'text', encoding = 'utf-8' )
  # return 
  return( content )
}

content_to_parsed_html <- function( content ){
  require( xml2 )
  # parse the html with xml2
  parsed_html <- xml2::read_html( content )
  # return
  return( parsed_html )
}

## Extract crime type
extract_all_crime_type <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  crime_type_els <- rvest::html_nodes( x = parsed_html, xpath = '//td[contains(@class,"field-name")]' )
  # extract the text from the cell
  crime_types <- rvest::html_text( crime_type_els )
  # return
  return( crime_types )
}

## Extract crime date
extract_all_crime_date <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  crime_date_els <- rvest::html_nodes( x = parsed_html, xpath = '//td[contains(@class,"crime-date")]' )
  # extract the text from the cell
  crime_dates <- rvest::html_text( crime_date_els )
  
  # return
  return( crime_dates )
}
