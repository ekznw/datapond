\name{check_document_tools}
\alias{check_document_tools}
\alias{setup_document_tools}
\title{Check or set up Data Pond document tools}
\usage{
check_document_tools(verify = TRUE, quiet = FALSE)

setup_document_tools(install = TRUE, quiet = FALSE)
}
\arguments{
\item{verify}{Render metadata PDF and development PDF, Word and HTML examples.
If FALSE, only executable checks run and full readiness remains unverified.}
\item{quiet}{Suppress printing checks and installation guidance.}
\item{install}{Allow TinyTeX and its required packages to be downloaded.
Use FALSE for offline verification only.}
}
\value{Invisibly, a list containing \code{ready}, a \code{checks} data frame,
executable probe results and \code{verified}. Readiness requires all checks,
including successful renders.}
\description{Checks Quarto, XeLaTeX and Python 3.9+, and renders temporary
examples using both bundled EKZNW themes. No dataset is opened or changed.
Checking never installs tools or TeX packages. Word-template editing needs none
of these tools.}
\details{
Explicit setup can download TinyTeX through Quarto and provision its TeX
packages. Existing system TeX installations are tested but never modified.
Missing Quarto and Python installations receive official installation links.
Setup is not run at package installation, package load or application startup.

\code{DATAPOND_QUARTO} (or \code{QUARTO_PATH}) can select Quarto;
\code{DATAPOND_RELEASE_PYTHON} can select Python. Configure these in
\file{.Renviron} for subsequent sessions. TinyTeX installed by Quarto need not
be on PATH. Renders use temporary files and disabled code execution.
Installations need internet access; after successful setup ordinary rendering
does not install packages. Failed renders include diagnostics in the result.
}
