% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterstream.R
\name{import2mongo}
\alias{import2mongo}
\title{Importing a filterstream file to mongoDB.}
\usage{
import2mongo(fileid, connection = "filterstream", createLog = TRUE)
}
\arguments{
\item{fileid}{Nnumber of the file to import}

\item{connection}{A mongo db connection}

\item{createLog}{If set to FALSE output is send to the
console instead of the logfile (logfile_import.txt)}
}
\value{
none
}
\description{
Used to import fiterstream files to mongo
in a seperate R process. Can be used to import tweets
manually for times of MongoDB downtime. The file to import
must be located in the directory set in sys.setenv("wd" =)
}
\examples{
#import2mongo(2)

}

