% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Misc.R
\name{out}
\alias{out}
\title{Customary function for printing output and simultaneously writing to a logfile}
\usage{
out(..., logpath = getOption("StandardPaths")[["TextOutput"]])
}
\arguments{
\item{...}{Objects to print/write to file}

\item{logpath}{Filename(s) of file(s) to print to, beside console. NULL if you just want to print to console.}
}
\description{
Character vectors are printed using cat instead of print.\cr
Multiple arguments are accepted, as seperate calls\cr
Any output is first evaluated (in its entirety), then printed to files, finally to console
}
