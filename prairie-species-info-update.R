# Adds in taxonomic information on prairie species. Web scapes from () describing
# flower structure. Also stores information on pollen grain sizes

# Import libraries----
library(taxize)
library(tidyverse)
library(httr) #
library(rvest)
library(purrr)

# Import csv ----
df = read.csv("data/prairie.species.info.csv")

# Separate out Genus and species ----
df = df %>%
  separate(scientific.name, into = c("genus", "species"), sep = " ", remove = F)

# Create new columns with Family, Order, and Common Name

## Use 2 for MOFI
df$family = tax_name(df$scientific.name, get = "family", db = 'itis')$family
df$order = tax_name(df$scientific.name, get = "order", db = 'itis')$order
df$common.name = sci2comm(df$scientific.name, db = 'itis', simplify = T)
df$common.name <- sapply(df$common.name, function(names) {
  paste(names, collapse = ", ")
})

# Web scrape plant information from Prairie Moon Nursery ----

# Create function that scrapes information from Details table
pull_plant_info = function(url) {
  
  # If url is missing skip over plant
  if (url == "") {
    
    life.cycle = NA
    germ.code = NA
    bloom.time = NA
    
  } else {
    
    # Get HTML from webpage
    html = read_html(as.character(url))
    
    # Extract first column
    terms = html %>%
      html_nodes(".g-product-details__item-term") %>% 
      html_text(trim = TRUE)
    
    # Extract second column
    descriptions = html %>%
      html_nodes(".g-product-details__item-description") %>% 
      html_text(trim = TRUE)
    
    # Select life cycle information
    if ('Life Cycle' %in% terms) {
      life.cycle =  descriptions[which(terms == "Life Cycle")]
    } else {
      life.cycle = "NA"
    }
  
    # Select germination code
    if ('Germination Code' %in% terms) {
      germ.code =  descriptions[which(terms == "Germination Code")]
    } else {
      germ.code = "NA"
    }
  
    # Select bloom time
    if ('Bloom Time' %in% terms) {
      bloom.time =  descriptions[which(terms == "Bloom Time")]
    } else {
      bloom.time = "NA"
    }
  }
  return(list(life.cycle = life.cycle, germ.code = germ.code, bloom.time = bloom.time))
}

# Use map_df to apply function to urls and add to df dataframe
prairie.moon.dat  = map_df(df$prairie.moon.website, pull_plant_info)
df = cbind(df, prairie.moon.dat)

# Export
cols = c("code", "common.name", "order", "family", "genus", "species", 
         "scientific.name", "life.cycle", "germ.code", "bloom.time", 
         "prairie.moon.website") 
export = df[cols]
write_csv(export, "data/processedData/output.csv")
