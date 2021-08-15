## Load libraries
library(dplyr, warn.conflicts = FALSE)
library(readr)

## Set work directory
setwd('D:/Robson/data/cites/citesTrade/')

## read all the data sets and merge it
citesTrade <- list.files(
        path = './data',
        full.names = TRUE
) %>%
        lapply(read_csv) %>%
        bind_rows

write.csv2(
        citesTrade,
        'D:/Robson/data/cites/citesTrade/output/citesTrade.csv',
        row.names = FALSE
)

## Read the new data set
citesTrade <- read_csv2('./output/citesTrade.csv')
head(citesTrade)
tail(citesTrade)
