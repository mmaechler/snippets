## get list of currently available packages on CRAN
library(XML)
pkgs <- readHTMLTable(readLines('https://cran.r-project.org/src/contrib/'), which = 1, stringsAsFactors = FALSE)

## we love data.table
library(data.table)
setDT(pkgs)

## drop directories
pkgs <- pkgs[Size != '-']

## package name should contain only (ASCII) letters, numbers and dot
pkgs[, name := sub('^([a-zA-Z0-9\\.]*).*', '\\1', Name)]
pkgs[, date := as.Date(`Last modified`, format = '%d-%b-%Y')]

## keep date and name
pkgs <- pkgs[, .(name, date)]

## list of packages with at least one archived version
archives <- readHTMLTable(readLines('https://cran.r-project.org/src/contrib/Archive/'), which = 1, stringsAsFactors = FALSE)
setDT(archives)

## keep directories
archives <- archives[grep('/$', Name)]

## add packages not found in current list of R packages
archives[, Name := sub('/$', '', Name)]
pkgs <- rbind(pkgs, archives[!Name %in% pkgs$name, .(name = Name)], fill = TRUE)

## NA date of packages with archived versions
pkgs[name %in% archives$Name, date := NA]

## reorder pkg in alphabet order
setorder(pkgs, name)

## lookup release date of first version
pkgs[is.na(date), date := {

    cat(name, '\n')

    ## download archive page
    page <- readLines(sprintf('https://cran.rstudio.com/src/contrib/Archive/%s/', name))

    ## extract date with regexp as HTML parsing can be slow :)
    date <- sub('.*([0-9]{2}-[A-Za-z]{3}-[0-9]{4}).*', '\\1', page[10])

    ## return YYYY-mm-dd format
    as.Date(date, format = '%d-%b-%Y')

}, by = name]

## order by date & alphabet
setorder(pkgs, date, name)
pkgs[, index := .I]
pkgs[c(250, 500, (1:9)*1000)]