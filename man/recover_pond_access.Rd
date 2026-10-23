\name{recover_pond_access}
\alias{recover_pond_access}
\title{Recover a local DataPond account from the trusted R console}
\usage{recover_pond_access(data_pond, username)}
\arguments{
\item{data_pond}{Existing pond path.}
\item{username}{Active or invited account to recover.}
}
\description{Run from the trusted local R console, outside a Shiny session.
Revokes existing sessions and replaces any previous activation/recovery code.
Disabled and pending accounts cannot use console recovery.}
\value{A private, single-use recovery code valid for 24 hours.}
\seealso{\code{\link{setup_pond_access}}}
