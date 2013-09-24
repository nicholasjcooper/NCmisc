\name{NCmisc-package}
\alias{NCmisc-package}
\alias{NCmisc}
\docType{package}
\title{
Miscellaneous one line functions
}
\description{
A set of useful functions, for instance, a versatile one line progress bar,
 a one line detailed function timer, a waiting function, text histogram, 
CRAN package search, simpler package installer, Linux queries, a Mode function, and more
}
\details{
\tabular{ll}{
Package: \tab NCmisc\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-07-25\cr
License: \tab GPL (>= 2)\cr
}
A package of general purpose functions that are habitually used in my R sessions. 
 Some of these functions are similar to existing functions but are simpler to use
 or have  more features (e.g, timeit and loop.tracker reduce an initialisation, 
 'during' and close three-line call structure, to a single function call.
Also, some of these functions are useful for building packages and pipelines,
 for instance: Header(), to provide strong visual deliniation between 
 procedures in console output, by an ascii bordered heading; loop.tracker()
to track the progress of loops (called with only 1 line of code), with the
 option to periodically backup a key object during the loop; estimate.memory()
 to determine whether the object may exceed some threshold before creating it,
 timeit(), a one line wrapper for proftools which gives a detailed breakdown of
 time taken, and time within each function called during a procedure; and 
 check.linux.install() to verify installation status of terminal commands before 
 using system().
\itemize{
 \item{check.linux.install }{ Check whether a given system command is installed (e.g, bash)}
 \item{estimate.memory }{ Estimate the memory required for an object}
 \item{get.distinct.cols }{ Return up to 22 distinct colours}
 \item{hlist }{ A good way to preview large lists}
 \item{Header }{ Print heading text with a border}
 \item{loop.tracker }{ Creates a progess bar within a loop with only 1 line}
 \item{Mode }{ Find the mode(s) of a vector}
 \item{must.use.package }{ Do everything possible to load an R package}
 \item{narm }{ Return an object with missing values removed}
 \item{pad.left }{ Print a vector with appropriate padding so each has equal char length}
 \item{pctile }{ Find data thresholds corresponding to percentiles}
 \item{Rfile.index }{ Create an index file for an R function file}
 \item{rmv.spc }{ Remove leading and trailing spaces (or other character)}
 \item{search.cran }{ Search all CRAN packages for those containing keyword(s)}
 \item{spc }{ Print a character a specified number of times}
 \item{standardize }{ Convert a numeric vector to Z-scores}
 \item{textogram }{ Make an ascii histogram in the console}
 \item{timeit }{ Times an expression, with breakdown of time spent in functions}
 \item{toheader }{ Return a string with each first letter of each word in upper case}
 \item{Unlist }{ Unlist a list, starting only from a set depth}
 \item{wait }{ Wait for a period of time}
}
}
\author{
Nicholas Cooper

Maintainer: 
Nicholas Cooper <nick.cooper@cimr.cam.ac.uk>
}
\keyword{ iteration }
\keyword{ color }
\keyword{ utilities }
\keyword{ package }
\seealso{
package 'reader' (not yet submitted)
}
\examples{
#text histogram
textogram(rnorm(10000),range=c(-3,3))
# wait 3 seconds
wait(0.2,silent=FALSE) 
# see whether a system command is installed
check.linux.install("sed") 
# a nice progress bar
max <- 100; for (cc in 1:max) { loop.tracker(cc,max); wait(0.004,"s") }
# nice header
Header(c("SPACE","The final frontier"))
# memory req'd for proposed or actual object
estimate.memory(matrix(rnorm(100),nrow=10)) 
# a mode function (there isn't one included as part of base)
Mode(c(1,2,3,3,4,4,4))
# search for packages containing text, eg, 'misc'
search.cran("misc",repos="http://cran.ma.imperial.ac.uk/") 
# breakdown of processing time using proftools
# not run:  timeit(wait(2,"s") ,total.time=TRUE) 
}

