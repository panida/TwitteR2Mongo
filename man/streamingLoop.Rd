% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterstream.R
\name{streamingLoop}
\alias{streamingLoop}
\title{StreamingLoop}
\usage{
streamingLoop(timeout = 30, type, createLog = TRUE)
}
\arguments{
\item{timeout}{Time until the streaming is stoped and restarted with updated parameters}

\item{type}{The type of stream which is started. (1=Based on hometimeline) (2=Based on query_url)}

\item{createLog}{If set to FALSE output is send to the console instead of the logfile
(logfile_filterstream.txt)}
}
\description{
StreamingLoop is usually started by \link{startStreaming} in a seperate instance of R.
If you start StreamingLoop manually, you will not be able to stop it using R. StreamingLoop
can be triggered to stop after timeout using \link{stopStreaming}.
}
\examples{
#do not run
#streamingLoop()

}

