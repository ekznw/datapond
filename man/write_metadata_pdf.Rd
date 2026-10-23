\name{write_metadata_pdf}
\alias{write_metadata_pdf}
\title{Write an EKZNW-branded dataset metadata PDF}
\usage{write_metadata_pdf(dataset_id, base_path, directory)}
\arguments{
\item{dataset_id}{Dataset identifier.}
\item{base_path}{Data Pond root.}
\item{directory}{New destination directory outside the Data Pond. Its parent must exist.}
}
\description{Reads a saved metadata snapshot through a read-only connection and renders
an EKZNW-branded metadata PDF using Quarto and XeLaTeX. Does not execute development
documents, change the pond, or certify readiness for external distribution.}
\value{Named paths to metadata.pdf, metadata.qmd and citation.txt. The output
directory also contains the portable theme. Existing directories are never overwritten.}
