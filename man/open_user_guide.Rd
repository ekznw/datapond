\name{open_user_guide}
\alias{open_user_guide}
\title{Open the DataPond user guide}
\usage{
open_user_guide(browse = interactive())
}
\arguments{
\item{browse}{Whether to open the guide in a browser. Use \code{FALSE} to obtain its path without opening it.}
}
\value{The normalized path to the offline HTML guide, invisibly.}
\description{Opens the bundled, self-contained HTML guide in the default browser. Reading it does not require Quarto, an internet connection or a running app. The guide covers installation, pond setup, metadata, lineage, review and frozen releases.}
\examples{
guide <- open_user_guide(browse = FALSE)
}
