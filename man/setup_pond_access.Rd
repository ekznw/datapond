\name{setup_pond_access}
\alias{setup_pond_access}
\title{Set up the first local DataPond administrator}
\usage{
setup_pond_access(data_pond, username, person_id = NULL,
  first_name = NULL, last_name = NULL, email = "")
}
\arguments{
\item{data_pond}{Existing pond path.}
\item{username}{Administrator's login name.}
\item{person_id}{Existing metadata person ID, or NULL for a new person.}
\item{first_name, last_name}{Name for a new person.}
\item{email}{Optional contact email.}
}
\description{Run once from the trusted local R console, outside a Shiny session.
Creates an invited administrator without changing historical attribution.
Existing account stores cannot be replaced by this function.}
\value{A private, single-use activation code valid for 24 hours. Use it in
the app's Activate / recover access tab to set a passphrase.}
\seealso{\code{\link{recover_pond_access}}}
