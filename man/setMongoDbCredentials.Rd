% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mongoConnection.R
\name{setMongoDbCredentials}
\alias{setMongoDbCredentials}
\title{Set MongoDB Credentials}
\usage{
setMongoDbCredentials(type, collection, db, url)
}
\arguments{
\item{type}{Type to set the MongoDB credentials for: hometimeline or filterstream}

\item{collection}{The collection to store the tweets in}

\item{db}{The database to store the tweets in}

\item{url}{The path to the MongoDB}
}
\value{
None
}
\description{
Function to set MongoDB credentials. Type can be hometimeline or filterstream.
Creates either hometimeline or filterstream as a connection element.
}
\examples{
setMongoDbCredentials (type = "hometimeline", collection = "hometimeline",
db = "twitter", url = "mongodb://localhost")

}

