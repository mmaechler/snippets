## original idea & report by Henrik Bengtsson at
## https://stat.ethz.ch/pipermail/r-devel/2016-February/072388.html

## This script downloads the list of currently published R packages
## from CRAN and also looks at all the archived package versions to
## combine these into a list of all R packages ever published on
## CRAN with the date of first release.

## CRAN mirror to use
CRAN_page <- function(...) {
    file.path('https://cran.rstudio.com/src/contrib', ...)
}

## get list of currently available packages on CRAN
library(XML)
pkgs <- readHTMLTable(readLines(CRAN_page()),
                                which = 1, stringsAsFactors = FALSE)

## we love data.table
library(data.table)
setDT(pkgs)

## drop directories
pkgs <- pkgs[Size != '-']
## drop files that does not seem to be R packages
pkgs <- pkgs[grep('tar.gz$', Name)]

## package name should contain only (ASCII) letters, numbers and dot
pkgs[, name := sub('^([a-zA-Z0-9\\.]*).*', '\\1', Name)]

## grab date from last modified timestamp
pkgs[, date := as.POSIXct(`Last modified`, format = '%d-%b-%Y %H:%M')]
pkgs[, date := as.character(date)]

## keep date and name
pkgs <- pkgs[, .(name, date)]

## list of packages with at least one archived version
archives <- readHTMLTable(readLines(CRAN_page('Archive')),
                          which = 1, stringsAsFactors = FALSE)
setDT(archives)

## keep directories
archives <- archives[grep('/$', Name)]

## add packages not found in current list of R packages
archives[, Name := sub('/$', '', Name)]
pkgs <- rbind(pkgs,
              archives[!Name %in% pkgs$name, .(name = Name)],
              fill = TRUE)

## reorder pkg in alphabet order
setorder(pkgs, name)

## number of versions released is 1 for published packages
pkgs[, versions := 0]
pkgs[!is.na(date), versions := 1]

## mark archived pacakges
pkgs[, archived := FALSE]
pkgs[name %in% archives$Name, archived := TRUE]

## NA date of packages with archived versions
pkgs[archived == TRUE, date := NA]

## lookup release date of first version & number of releases
pkgs[is.na(date), c('date', 'versions') := {

    cat(name, '\n')

    ## download archive page
    page <- readLines(CRAN_page('Archive', name))

    ## extract date with regexp as HTML parsing can be slow :)
    date <- sub('.*([0-9]{2}-[A-Za-z]{3}-[0-9]{4} [0-9]{2}:[0-9]{2}).*', '\\1', page[10])

    ## convert to YYYY-mm-dd format
    date <- as.POSIXct(date, format = '%d-%b-%Y %H:%M')

    ## number of previous releases
    archived_versions <- length(page) - 9 - 4

    ## return
    list(as.character(date), versions + archived_versions)

}, by = name]

## rename cols
setnames(pkgs, 'date', 'first_release')

## order by date & alphabet
setorder(pkgs, first_release, name)
pkgs[, index := .I]
pkgs[c(250, 500, (1:9)*1000)]

##                 name       first_release versions archived index
##  1:          pls.pcr 2003-03-31 12:44:00       13     TRUE   250
##  2:            MEMSS 2005-02-25 08:07:00       12     TRUE   500
##  3: signalextraction 2007-03-15 18:50:00        4     TRUE  1000
##  4:         ORIClust 2009-09-18 20:18:00        2     TRUE  2000
##  5:           MAPLES 2011-04-26 17:36:00        1    FALSE  3000
##  6:            Bclim 2012-06-22 05:42:00        3     TRUE  4000
##  7:    RadialPlotter 2013-03-21 06:53:00        9     TRUE  5000
##  8:             ltsk 2014-02-06 20:35:00        5     TRUE  6000
##  9:             matR 2014-10-23 09:50:00        1    FALSE  7000
## 10:            CompR 2015-07-01 14:06:00        1    FALSE  8000
## 11:       ggcorrplot 2016-01-12 22:12:00        1    FALSE  9000

## plot trend
library(ggplot2)
ggplot(pkgs, aes(as.Date(first_release), index)) +
    geom_line(size = 2) +
    scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
    scale_y_continuous(breaks = seq(0, 9000, 1000)) +
    xlab('') + ylab('') + theme_bw() +
    ggtitle('Number of R packages ever published on CRAN')

## store report
write.csv(pkgs, 'pkgs.csv', row.names = FALSE)
