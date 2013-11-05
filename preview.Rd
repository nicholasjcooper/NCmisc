\name{preview}
\alias{preview}
\alias{prv}
\title{Output variable states within functions during testing/debugging}
\usage{
  preview(varlist, labels = NULL, counts = NULL)
  prv(..., counts = NULL)
}
\arguments{
  \item{varlist}{character vector, the list of variable(s)
  to report, which will trigger automatic labelling of the
  variable name, otherwise if entered as the variable value
  (ie.  without quotes, then will by default be displayed
  as 'unknown variable')}

  \item{...}{for prv() equivalent to varlist for preview()
  the list of objects to preview, which will trigger automatic
  labelling of the variable name.}

  \item{labels,}{will label 'unknown variables' (see above)
  if entered as variables without quotes}

  \item{counts}{a list of array index values; so if calling
  during a counting loop, the value can be reported each
  iteration, also printing the count index; if the list is
  named the name will also appear, e.g, variable[count=1].
  This list must be the same length as varlist (and labels
  if not NULL), and each element [[i]] must contain as many
  values as the original corresponding varlist[i] has
  dimensions}
}
\description{
  A versatile function to compactly display most common R
  objects. Will return the object name, type, dimension,
  and a compact representation of object contents, for
  instance using prv.large() to display matrices, so as to
  not overload the console for large objects. Useful for
  debugging, can be placed inside loops and functions to
  track values, dimensions, and data types. Particularly
  when debugging complex code, the automatic display of the
  variable name prevents confusion versus using regular
  print statements. By listing variables to track as
  character(), provides 'cat()' output of compact and
  informative variable state information, e.g, variable
  name, value, datatype and dimension. Can also specify
  array or list elements, or custom labels. prv() is the
  same as preview() except it can take objects without
  using double quotes and has no 'labels' command (and
  doesn't need one).
  prv() is the same as preview() but has no labels command, 
  and input is without quotes and must be plain variable 
  names of existing variables (no indices, args, etc) 
}
\examples{
# create variables of different types to show output styles #
testvar1 <- 193
testvar2 <- "Atol"
testvar3 <- c(1:10)
testvar4 <- matrix(rnorm(100),nrow=25)
testvar5 <- list(first="test",second=testvar4,third=100:110)
preview("testvar1"); prv(testvar1)
preview("testvar4")
preview(paste("testvar",1:5,sep=""))
# prv() works without using a character array:
prv(testvar1,testvar2,testvar3,testvar4,testvar5)
preview(testvar1,"myvarname")
preview(testvar1)
# examples with loops and multiple dimensions / lists
for (cc in 1:4) {
 for (dd in 1:4) { preview("testvar4",counts=list(cc,dd)) }}

for (dd in 1:3) { preview("testvar5",counts=list(dd=dd)) }
}
\seealso{
  \code{\link{Dim}}
}

