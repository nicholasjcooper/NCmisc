\name{NCmisc-package}
\alias{NCmisc-package}
\alias{NCmisc}
\docType{package}
\title{
Miscellaneous one line functions
}
\description{
A set of handy functions. Includes: versatile one line progress bar, one 
 line function timer with detailed output, time delay function, text histogram, object 
 preview, CRAN package search, simpler package installer, Linux command install check, 
 a flexible Mode function, top function, simulation of correlated data, and more
}
\details{
\tabular{ll}{
Package: \tab NCmisc\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-07-25\cr
License: \tab GPL (>= 2)\cr
}
A package of general purpose functions that might save time or help tidy up code. 
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
 using system(), top() to examine current memory and CPU usage [using the system
 'top' command]. prv() is useful for debugging as it allows a detailed preview of
 objects, and is as easy as placing print statements within loops/functions but
 gives more information, and gives compact output for large objects. For testing
 sim.cor() provides a simple way to simulate a correlated data matrix, as often
 this is more realistic than completely random data. Otherwise summarise.r.datasets
 gives a list of all available datasets and their structure and dimensionality.

List of key functions:
\itemize{
 \item{\emph{check.linux.install }}{ Check whether a given system command is installed (e.g, bash)}
 \item{\emph{cor.with }}{ simulate a variable with a specified correlation to an existing variable}
 \item{\emph{Dim }}{ same as dim() function but works for more objects, including vectors }
 \item{\emph{estimate.memory }}{ Estimate the memory required for an object}
 \item{\emph{exists.not.function }}{ same as exists() function but ignores functions }
 \item{\emph{get.distinct.cols }}{ Return up to 22 distinct colours}
 \item{\emph{hlist }}{ A good way to preview large lists}
 \item{\emph{Header }}{ Print heading text with a border}
 \item{\emph{loop.tracker }}{ Creates a progess bar within a loop with only 1 line}
 \item{\emph{Mode }}{ Find the mode(s) of a vector}
 \item{\emph{must.use.package }}{ Do everything possible to load an R package}
 \item{\emph{narm }}{ Return an object with missing values removed}
 \item{\emph{packages.loaded }}{ quietly test whether packages are loaded without using require }
 \item{\emph{pad.left }}{ Print a vector with appropriate padding so each has equal char length}
 \item{\emph{pctile }}{ Find data thresholds corresponding to percentiles}
 \item{\emph{preview }}{ same as prv, but enter arguments as strings}
 \item{\emph{prv.large }}{ tidy representation for large matrices/data.frames}
 \item{\emph{prv }}{ compact preview of objects (more complete than 'print')}
 \item{\emph{Rfile.index }}{ Create an index file for an R function file}
 \item{\emph{rmv.spc }}{ Remove leading and trailing spaces (or other character)}
 \item{\emph{search.cran }}{ Search all CRAN packages for those containing keyword(s)}
 \item{\emph{sim.cor }}{ simulate a correlated dataset}
 \item{\emph{spc }}{ Print a character a specified number of times}
 \item{\emph{standardize }}{ Convert a numeric vector to Z-scores}
 \item{\emph{Substitute }}{ multivariable version of substitute (base)}
 \item{\emph{summarise.r.datasets }}{ show and summarise all available example datasets }
 \item{\emph{textogram }}{ Make an ascii histogram in the console}
 \item{\emph{timeit }}{ Times an expression, with breakdown of time spent in functions}
 \item{\emph{toheader }}{ Return a string with each first letter of each word in upper case}
 \item{\emph{top }}{ report on CPU and memory usage, overall or by process }
 \item{\emph{Unlist }}{ Unlist a list, starting only from a set depth}
 \item{\emph{wait }}{ Wait for a period of time}
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
\code{\link[reader:reader-package]{reader}} ~~

}
\examples{
#text histogram
textogram(rnorm(10000),range=c(-3,3))
# wait 0.2 seconds
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
# simulate a correlated dataset
corDat <- sim.cor(200,5)
cor(corDat) # show correlation matrix
prv(corDat) # show preview of matrix
# Dim() versus dim()
Dim(1:10); dim(1:10)
# check whether package is loaded (when not required or dependency)
repos <- "http://cran.ma.imperial.ac.uk/" # or repos <- getOption("repos")
packages.loaded("bigmemory",repos=repos)
}

