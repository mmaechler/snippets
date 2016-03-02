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
pkgs[, date := as.Date(`Last modified`, format = '%d-%b-%Y')]

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
pkgs <- rbind(pkgs, archives[!Name %in% pkgs$name, .(name = Name)], fill = TRUE)

## NA date of packages with archived versions
pkgs[name %in% archives$Name, date := NA]

## reorder pkg in alphabet order
setorder(pkgs, name)

## lookup release date of first version
pkgs[is.na(date), date := {

    cat(name, '\n')

    ## download archive page
    page <- readLines(CRAN_page('Archive', name))

    ## extract date with regexp as HTML parsing can be slow :)
    date <- sub('.*([0-9]{2}-[A-Za-z]{3}-[0-9]{4}).*', '\\1', page[10])

    ## return YYYY-mm-dd format
    as.Date(date, format = '%d-%b-%Y')

}, by = name]

## order by date & alphabet
setorder(pkgs, date, name)
pkgs[, index := .I]
pkgs[c(250, 500, (1:9)*1000)]

##            name       date index
##  1:   polspline 2003-03-19   250
##  2:     micEcon 2005-02-21   500
##  3: cairoDevice 2007-03-11  1000
##  4:     maticce 2009-09-10  2000
##  5:     SPECIES 2011-04-24  3000
##  6:       HIBAG 2012-06-21  4000
##  7:    Rgnuplot 2013-03-20  5000
##  8:       bilan 2014-02-05  6000
##  9:      glmvsd 2014-10-22  7000
## 10:      gkmSVM 2015-06-30  8000
## 11:     dChipIO 2016-01-12  9000

## plot trend
library(ggplot2)
ggplot(pkgs, aes(date, index)) + geom_line(size = 2) +
    scale_x_date(date_breaks = '2 year', date_labels = '%Y') +
    scale_y_continuous(breaks = seq(0, 9000, 1000)) +
    xlab('') + ylab('') + theme_bw() +
    ggtitle('Number of R packages ever published on CRAN')

## store report
write.csv(pkgs, 'pkgs.csv', row.names = FALSE)
