#### Only definitions -- mainly of diverse implementations of ifelse()

## base :: ifelse ----(since r68595 | hornik | 2015-06-28), as of 2016-08-08
##M: ~/R/D/r-devel/R/src/library/base/R/ifelse.R

ifelseR <- function (test, yes, no)
{
    if(is.atomic(test)) { # do not lose attributes
        if (typeof(test) != "logical")
            storage.mode(test) <- "logical"
        ## quick return for cases where 'ifelse(a, x, y)' is used
        ## instead of 'if (a) x else y'
        if (length(test) == 1 && is.null(attributes(test))) {
            if (is.na(test)) return(NA)
            else if (test) {
                if (length(yes) == 1 && is.null(attributes(yes)))
                    return(yes)
            }
            else if (length(no) == 1 && is.null(attributes(no)))
                return(no)
        }
    }
    else ## typically a "class"; storage.mode<-() typically fails
	test <- if(isS4(test)) methods::as(test, "logical") else as.logical(test)
    ans <- test
    ok <- !(nas <- is.na(test))
    if (any(test[ok]))
	ans[test & ok] <- rep(yes, length.out = length(ans))[test & ok]
    if (any(!test[ok]))
	ans[!test & ok] <- rep(no, length.out = length(ans))[!test & ok]
    ans[nas] <- NA
    ans
}

## base R 1.0.1 's version   [file/svn date: May 9, 1999] :
ifelseR101 <- function (test, yes, no)
{
    ans <- test
    test <- as.logical(test)
    nas <- is.na(test)
    if (any(test[!nas])) {
        ans[test] <- rep(yes, length = length(ans))[test]
    }
    if (any(!test[!nas])) {
        ans[!test] <- rep(no, length = length(ans))[!test]
    }
    ans[nas] <- NA
    ans
}

## base R 0.63.3's version [file/svn date: Sep 10, 1998] :

ifelseR0633 <- function (test, yes, no)
{
    ans <- test
    test <- as.logical(test)
    nas <- is.na(test)
    ans[ test] <- rep(yes, length = length(ans))[ test]
    ans[!test] <- rep(no,  length = length(ans))[!test]
    ans[nas] <- NA
    ans
}



## Hadley's  dplyr::if_else (with *simplified*  check_*)

##M: /usr/local.nfs/app/R/R_local/src/dplyr/R/if_else.R

#' Vectorised if.
#'
#' Compared to the base \code{\link{ifelse}()}, this function is more strict.
#' It checks that \code{true} and \code{false} are the same type. This
#' strictness makes the output type more predictable, and makes it somewhat
#' faster.
#'
#' @param condition Logical vector
#' @param true,false Values to use for \code{TRUE} and \code{FALSE} values of
#'   \code{condition}. They must be either the same length as \code{condition},
#'   or length 1. They must also be the same type: \code{if_else} checks that
#'   they have the same type and same class. All other attributes are
#'   taken from \code{true}.
#' @param missing If not \code{NULL}, will be used to replace missing
#'   values.
#' @return Where \code{condition} is \code{TRUE}, the matching value from
#'   \code{true}, where it's \code{FALSE}, the matching value from \code{false},
#'   otherwise \code{NA}.
#' @export
#' @examples
#' x <- c(-5:5, NA)
#' if_else(x < 0, NA_integer_, x)
#' if_else(x < 0, "negative", "positive", "missing")
#'
#' # Unlike ifelse, if_else preserves types
#' x <- factor(sample(letters[1:5], 10, replace = TRUE))
#' ifelse(x %in% c("a", "b", "c"), x, factor(NA))
#' if_else(x %in% c("a", "b", "c"), x, factor(NA))
#' # Attributes are taken from the `true` vector,
ifelseHW <- function(condition, true, false, missing = NULL) {
    if (!is.logical(condition))
        stop("`condition` must be logical", call. = FALSE)
    out <- true[rep(NA_integer_, length(condition))]
    out <- replace_with(out, condition & !is.na(condition), true, "`true`")
    out <- replace_with(out, !condition & !is.na(condition), false, "`false`")
           replace_with(out, is.na(condition), missing, "`missing`")
}

##M: /usr/local.nfs/app/R/R_local/src/dplyr/R/utils-replace-with.R

##' Simplified, more tolerant, standalone version of dplyr::replace_with()
replace_with <- function(x, i, val, name) {
    if (is.null(val))
        return(x)
    n <- length(x) ## + stop() below  <==> check_length(val, x, name)
    ## check_type(val, x, name) ## <==> stopifnot(typeof(val) == typeof(x))
    ## check_class(val, x, name)## <==> if(is.object(x))
    ##				stopifnot(identical(class(x),class(template)))
    x[i] <- if (length(val) == 1L) val
            else if(length(val) == n) val[i]
            else stop("no recycling here: length(val) must be 1 or length(x)")
    x
}

##'--------- Jonathan Hosking's ifthen(): .. sent by private E-mail, Aug.2016
ifelseJH <- function(test, yes, no, warn = TRUE) {
    ##
    ##  Variant of ifelse(). Class and mode of result are those of 'c(yes,no)'.
    ##  Thus the Warning in the help of ifelse() does not apply, and if
    ##  'yes' and 'no' have the same class then the result also has this class.
    ##    Names and dimensions of result are those of 'test'.
    ##
    ##  Example:
    ##
    ##    d1 <- Sys.Date() + (1:3)              # A "Date" object
    ##    d2 <- Sys.Date() + (31:33)            # Another "Date" object
    ##    ifelse(c(TRUE, FALSE, TRUE), d1, d2)  # Returns a numeric vector(!)
    ##    ifthen(c(TRUE, FALSE, TRUE), d1, d2)  # Returns a "Date" object
    ##
    n <- length(test)
    len <- c(length(yes),length(no))
                                        # if (getRversion() < "2.11.0") {  # getRversion() and its "<" method are slow!
    if (inherits(yes,"POSIXlt")) len[1] <- length(yes[[1]])  # Accommodates R 2.10.x and earlier, probably not needed now
    if (inherits( no,"POSIXlt")) len[2] <- length( no[[1]])
                                        # }
    if (!all(len == n)) {
        if (warn && !all(is.element(len,c(1,n))))
            warning("lengths of 'yes' and 'no' are not both either 1 or the same as the length of 'test'")
        yes <- rep(yes,length.out = n)
        no <- rep(no,length.out = n)
    }
    tt <- as.logical(test)
    out <- c(yes,no)[seq_along(tt)+n*(!tt)]
    if (inherits(out,"POSIXt")) {
        tz <- attr(yes,"tzone")
        if (!is.null(tz) && identical(tz,attr(no,"tzone"))) attr(out,"tzone") <- tz
    }
    if (inherits(out,"POSIXlt")) return(out)
    dim(out) <- dim(test)
    if (is.null(dim(test))) names(out) <- names(test)
    else dimnames(out) <- dimnames(test)
    out
}


##' Martin Maechler, 14. Nov 2016 --- taking into account Duncan M. and Hadley's
##' ideas in the R-devel thread starting at (my mom's 86th birthday):
##' https://stat.ethz.ch/pipermail/r-devel/2016-August/072970.html
ifelse2 <- function (test, yes, no, NA. = NA) {
    if(!is.logical(test)) {
        if(is.atomic(test))
            storage.mode(test) <- "logical"
        else ## typically a "class"; storage.mode<-() typically fails
            test <- if(isS4(test)) methods::as(test, "logical") else as.logical(test)
    }

    ## No longer optimize the  "if (a) x else y"  cases:
    ## Only "non-good" R users use ifelse(.) instead of if(.) in these cases.

    ans <-
	tryCatch(if(identical(class(yes), class(no))) {
		     ## as c(o) or o[0] may not work for the class
		     if(length(yes) == length(test))
			 yes # keep attributes such as dim(.)
		     else
			 rep(yes, length.out = length(test))
		 }
		 else rep(c(yes[0], no[0]), length.out = length(test)),
		 error = function(e) structure(e, class = c("ifelse2_error", class(e))))
    if(inherits(ans, "ifelse2_error")) { ## -> asymmetric  yes-leaning
	ans <- yes
	ans[!test] <- no[!test] # (potentially lots of recycling here)
	if(anyNA(test))
	    ans[is.na(test)] <- NA.
    }
    else {
	ok <- !(nas <- is.na(test))
	if (any(test[ok]))
	    ans[test & ok] <- rep(yes, length.out = length(ans))[test & ok]
	if (any(!test[ok]))
	    ans[!test & ok] <- rep(no, length.out = length(ans))[!test & ok]
	ans[nas] <- NA. # possibly coerced to class(ans)
    }
    ans
}

## Suharto Anggono - on R-devel@..., Nov.26, 2016
## A concrete version of 'ifelse2' that starts the result from 'yes':
ifelseSA1 <- function(test, yes, no, NA. = NA) {
    if(!is.logical(test))
        test <- if(isS4(test)) methods::as(test, "logical") else as.logical(test)
    n <- length(test)
    ans <- rep(yes, length.out = n)
    ans[!test & !is.na(test)] <- rep(no, length.out = n)[!test & !is.na(test)]
    ans[is.na(test)] <- rep(NA., length.out = n)[is.na(test)]
    ans
}
## It requires 'rep' method that is compatible with subsetting. It also works
## with "POSIXlt" in R 2.7.2, when 'length' gives 9, and gives an appropriate
## result if time zones are the same.

## For coercion of 'test', there is no need of keeping attributes. So, it
## doesn't do
## 	storage.mode(test) <- "logical"
## and goes directly to 'as.logical'.

## It relies on subassignment for silent coercions of
##     logical < integer < double < complex .
## Unlike 'ifelse', it never skips any subassignment. So, phenomenon as in "example of different return modes" in ?ifelse doesn't happen.

## Suharto Anggono - on R-devel@..., Nov.26, 2016
## Another version, for keeping attributes as pointed out by Duncan Murdoch:
ifelseSA2 <- function(test, yes, no, NA. = NA) {
    if(!is.logical(test))
        test <- if(isS4(test)) methods::as(test, "logical") else as.logical(test)
    n <- length(test)
    n.yes <- length(yes); n.no <- length(no)
    if (n.yes != n) {
        if (n.no == n) {  # swap yes <-> no
            test <- !test
            ans <- yes; yes <- no; no <- ans
            n.no <- n.yes
        } else yes <- yes[rep_len(seq_len(n.yes), n)]
    }
    ans <- yes
    if (n.no == 1L)
        ans[!test] <- no
    else
        ans[!test & !is.na(test)] <- no[
            if (n.no == n) !test & !is.na(test)
            else rep_len(seq_len(n.no), n)[!test & !is.na(test)]]
    stopifnot(length(NA.) == 1L) ## << MM: I have been assuming this in all cases
    ans[is.na(test)] <- NA.
    ans
}
## Note argument evaluation order: 'test', 'yes', 'no', 'NA.'.

## First, it chooses the first of 'yes' and 'no' that has the same length as
## the result. If none of 'yes' and 'no' matches the length of the result, it
## chooses recycled (or truncated) 'yes'.

## It uses 'rep' on the index and subsetting as a substitute for 'rep' on the
## value.
## It requires 'length' method that is compatible with subsetting.

## Additionally, it uses the same idea as dplyr::if_else, or more precisely
## the helper function 'replace_with'. It doesn't use 'rep' if the length of
## 'no' is 1 or is the same as the length of the result. For subassignment
## with value of length 1, recycling happens by itself and NA in index is OK.

## It limits 'NA.' to be of length 1, considering 'NA.' just as a label for NA.
