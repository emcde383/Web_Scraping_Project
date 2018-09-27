library(shinythemes)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(shiny)
library(tidytext)
library(wordcloud)
library(reshape2)
library(stringr)
library(tidyr)

ca_data <- read.csv("./data/output/ca_reviews.csv")
winemag_data <- read.csv("./data/winemag-data.csv")
attribute_data <- read.csv("./data/attributes.csv")
award_cat <- read.csv("./data/award_cat.csv")

sf_data14 <- read.csv("./data/sf_reviews_14.csv")
sf_data14$competition_year = 2014
sf_data15 <- read.csv("./data/sf_reviews_15.csv")
sf_data15$competition_year = 2015
sf_data16 <- read.csv("./data/sf_reviews_16.csv")
sf_data16$competition_year = 2016
sf_data17 <- read.csv("./data/sf_reviews_17.csv")
sf_data17$competition_year = 2017
sf_data18 <- read.csv("./data/sf_reviews_18.csv")
sf_data18$competition_year = 2018
sf_data18$price <- as.factor(sf_data18$price)

sf_data <- bind_rows(sf_data14, sf_data15, sf_data16, sf_data17, sf_data18)

f1 = function(s) {
  t = strsplit(s, "\\. ")[[1]][2]
  gsub(" APPELLATIONS", "", t)
}

f2 = function(s) {
  gsub(" APPELLATION", "", s)
}

levels(ca_data$region) <- as.vector(sapply(levels(ca_data$region), f1))
levels(ca_data$region) <- as.vector(sapply(levels(ca_data$region), f2))

ca_data <- ca_data %>% 
  filter(is.na(ca_data$region) == FALSE, grape != "", grape != " ")

#capitalize first letter in string
toproper <- function(x) {

  return(paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)), collapse = " "))

}
ca_data$region <- sapply(ca_data$region, toproper)

#remove dollar sign from price
remove_dollar = function(s) {
  gsub("\\$", "", as.character(s))
}
ca_data$price <- as.numeric(sapply(ca_data$price, remove_dollar))

ca_data$ln_price = log(ca_data$price)
winemag_data$ln_price = log(winemag_data$price)

sf_data$price <- as.numeric(sapply(sf_data$price, remove_dollar))
sf_data$ln_price <- log(sf_data$price)

t1 <- ca_data %>% 
  select(score, ln_price) %>% 
  mutate(data = "California Competition", ca = 1)

t2 <- winemag_data %>%
  mutate(score = points, data = "Wine Enthusiast",
         ca = ifelse(country == "US" & province == "California", 1, 0)) %>% 
  select(score, ln_price, data, ca)

ca_winemag_data <- bind_rows(t1, t2) %>% 
  mutate(ca = factor(ca, labels = c(0, 1)))

#remove spaces from string
striptext = function(x) {
  
  y <- gsub(pattern = "[[:punct:]]", replacement = "", x)
  return(gsub(" ", "", y))
  
}