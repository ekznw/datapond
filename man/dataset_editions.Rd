\name{dataset_editions}
\alias{set_dataset_edition}
\alias{public_draft_files}
\alias{public_draft_fields}
\alias{create_public_dataset_draft}
\alias{record_dataset_publication}
\alias{dataset_edition_family}
\alias{public_edition_choices}
\alias{preview_public_dataset_draft}
\alias{apply_public_dataset_draft}
\alias{set_public_metadata_defaults}
\title{Manage linked in-house and public dataset editions}
\description{Designate separate dataset editions, derive a public draft from a verified frozen source, and record local publication history.}
\usage{
set_dataset_edition(dataset_id, base_path, user_id,
  edition = c("in_house", "public"), source_release_id = NULL,
  preparation_method = "")
public_draft_files(release_id, base_path)
public_draft_fields(release_id, base_path, files)
create_public_dataset_draft(release_id, base_path, user_id, title, version,
  files, fields = list(), preparation_method)
record_dataset_publication(release_id, base_path, user_id,
  action = c("published", "withdrawn"), location, note)
dataset_edition_family(dataset_id, base_path)
public_edition_choices(release_id, base_path, dataset_id = NULL)
preview_public_dataset_draft(release_id, base_path, user_id, files,
  fields = list(), dataset_id = NULL)
apply_public_dataset_draft(preview, title, version, preparation_method,
  decisions = list(), edits = list())
set_public_metadata_defaults(dataset_id, base_path, user_id,
  excluded_fields = character(), organisation_id = NULL, person_id = NULL)
}
\arguments{
\item{dataset_id}{The selected dataset's local catalogue ID.}
\item{base_path}{Path to the local pond.}
\item{user_id}{Active local application user ID.}
\item{edition}{In-house or public designation.}
\item{source_release_id, release_id}{Exact frozen release UUID.}
\item{preparation_method}{Description of public selection, transformation and documentation preparation.}
\item{files}{Explicit relative paths returned by \code{public_draft_files()}.}
\item{fields}{Named list keyed by selected file path. Each value is a named list of retained column names keyed by \code{table} for CSV/TSV or by layer name for GeoPackage/GeoJSON. Omitted files copy intact. Omitted vector layers are excluded; geometry is retained.}
\item{title, version}{New public title and independent major.minor.patch version.}
\item{action}{Record completed external publication or withdrawal locally.}
\item{location}{Publication location or URL.}
\item{note}{Reason for the publication event.}
\item{preview}{An unchanged object from \code{preview_public_dataset_draft()}. Its temporary staging is removed on successful application; callers should remove \code{preview$stage} when cancelling.}
\item{decisions}{Named list keyed by preview conflict keys, with values \code{current} or \code{source}. Every conflict requires a choice.}
\item{edits}{Named list of descriptive entry values keyed as \code{entry:abstract}, for example. Rights are reviewed separately.}
\item{excluded_fields}{Descriptive metadata keys to omit from frozen-source public defaults; rights cannot be excluded.}
\item{organisation_id, person_id}{Optional organisation and its nominated public contact. A null person means organisation contact only.}
}
\value{Designation returns the relationship invisibly. File inspection returns a data frame; field inspection returns nested named lists. Draft creation returns the new dataset ID, UUID, folder and source release. Publication returns true invisibly. Family inspection returns edition and release summaries.}
\details{Public drafts inherit descriptive metadata, rights and definitions for retained fields, with a new identity and fresh review. Automatic scanning registers the resulting files and attributes. Updates compare previous source, current public and new source metadata, reuse selections, preserve public edits and require conflict decisions. An update keeps the public UUID and saves a full rollback checkpoint accessible with \code{dataset_release_checkpoints()}. Public transport omits private comparison baselines and internal-field policies; imported public datasets therefore require manual preparation. Publication recording does not upload or remove remote files.}
