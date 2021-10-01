## Load libraries
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)


## read all the data sets and merge it
# citesTrade <- list.files(
#         path = './data',
#         full.names = TRUE
# ) %>%
#         lapply(read_csv) %>%
#         bind_rows
# 
# write.csv2(
#         citesTrade,
#         './output/citesTrade.csv',
#         row.names = FALSE
# )

## Read the new data set
citesTrade <- read_csv('./output/citesTrade.csv')

head(citesTrade)
tail(citesTrade)
str(citesTrade) ## Investigate the structure of the data set
as.vector(unique(citesTrade$Year))

## Read the codes of countries
codCountries <- read.csv2('./output/CodCountries.csv')

## join
t <- citesTrade %>%
        left_join(codCountries[, 2:3], by = 'Exporter')
        left_join(codCountries[, 2:3], by = 'Importer')

t %>% select(16:22)
glimpse(t)

## Most commercialized families
citesFamily <- citesTrade %>%
        filter(!is.na(Family)) %>%
        count(Family, sort = TRUE) %>%
        slice_max(n, n = 10)

citesFamily$Family <- with(citesFamily, reorder(Family, n)) 

familyPlot <- ggplot(citesFamily, aes(Family, n)) +
        geom_col(fill = 'steelblue') + coord_flip() +
        geom_text(aes(
                label = format(n, big.mark = ','), hjust = 1), color = 'white'
        ) +
        theme(
                axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_text(hjust = 0.5)
        ) +
        labs(
                title = 'Top 10 Families CITES Trade - 1975-2020',
                x = '', y = ''
        )

familyPlot

## Most commercialized Genus
citesGenus <- citesTrade %>%
        filter(!is.na(Genus)) %>%
        count(Genus, sort = TRUE) %>%
        slice_max(n, n = 10)

citesGenus$Genus <- with(citesGenus, reorder(Genus, n))

genusPlot <- ggplot(citesGenus, aes(Genus, n)) +
        geom_col(fill = 'steelblue') + coord_flip() +
        geom_text(aes(label = format(n, big.mark = ','),
                      hjust = 1), color = 'white') +
        theme(
                axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_text(hjust = 0.5)
        ) +
        labs(
                title = 'Top 10 Genus CITES Trade - 1975-2020',
                x = '', y = ''
        )

genusPlot

## Most commercialized species (taxon)
citesTaxon <- citesTrade %>%
        filter(!is.na(Taxon)) %>%
        count(Taxon, sort = TRUE) %>%
        slice_max(n, n = 10)

citesTaxon$Taxon <- with(citesTaxon, reorder(Taxon, n))

taxonPlot <- ggplot(citesTaxon, aes(Taxon, n)) +
        geom_col(fill = 'steelblue') + coord_flip() +
        geom_text(aes(label = format(n, big.mark = ','),
                      hjust = 1), color = 'white') +
        theme(
                axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_text(hjust = 0.5)
        ) +
        labs(
                title = 'Top 10 Species CITES Trade - 1975-2020',
                x = '', y = ''
        )

taxonPlot

## Licenses by year
citesLicenses <- citesTrade %>%
        filter(!is.na(Year)) %>%
        filter(Year != 2021) %>%
        #group_by(Year) %>%
        count(Year)

#citesLicenses$Year <- with(citesLicenses, reorder(Year, n))

licensesPlot <- ggplot(citesLicenses, aes(Year, n)) +
        geom_col(fill = 'steelblue') +
        scale_y_continuous(labels = scales::label_number(big.mark = ',')) +
        annotate('text', x = 2020, y = 8298, 
                 label = '8298', angle = 90, hjust = -0.5) +
        theme(
                axis.title = element_text(hjust = 0.5)
        ) +
        labs(
                title = 'CITES Licenses by Year - 1975-2020',
                x = '', y = ''
        )

licensesPlot

## Main Importer Countries
citesImporter <- citesTrade %>%
        select(Year, Taxon, Family, Importer, Exporter) %>%
        group_by(Year, Importer, Exporter) %>%
        tidyr::drop_na() %>%
        top_n(Importer, 5)

## Main Exporter Countries

## Types of CITES products
