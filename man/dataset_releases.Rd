\name{dataset_releases}
\alias{preview_dataset_release}
\alias{create_dataset_release}
\alias{dataset_release_history}
\alias{verify_dataset_release}
\alias{archive_dataset_release}
\alias{restore_dataset_release}
\alias{dataset_release_checkpoints}
\alias{restore_dataset_checkpoint}
\alias{recover_dataset_release_operations}
\alias{cache_dataset_release}
\alias{verify_cached_dataset_release}
\title{Freeze, restore and cache verified dataset releases}
\description{Creates versioned local archive ZIPs with saved metadata documents,
restores files and metadata together, and manages recoverable lifecycle operations.
Freezing does not publish a release or grant distribution permission.}
\usage{
preview_dataset_release(dataset_id, base_path, user_id)
create_dataset_release(dataset_id, base_path, user_id, notes, preview,
  close_working = FALSE, iso_policy = c("none", "pinned", "datapond"))
dataset_release_history(dataset_id, base_path)
verify_dataset_release(release_id, base_path, extract_to = NULL)
archive_dataset_release(release_id, base_path, user_id, note)
restore_dataset_release(release_id, base_path, user_id, note)
dataset_release_checkpoints(dataset_id, base_path)
restore_dataset_checkpoint(operation_id, base_path, user_id, note)
recover_dataset_release_operations(dataset_id, base_path, user_id)
cache_dataset_release(release_id, base_path, cache, keep_zip = FALSE)
verify_cached_dataset_release(directory)
}
\arguments{
\item{dataset_id}{Saved local dataset ID.}
\item{base_path}{Local Data Pond root.}
\item{user_id}{Active application user ID for the lifecycle audit.}
\item{notes, note}{Nonempty release notes or reason for a lifecycle operation.}
\item{preview}{Unmodified result of a fresh preview. Changed files or metadata invalidate it.}
\item{close_working}{Whether to retire the working folder after verifying the release.}
\item{iso_policy}{Optional ISO validation policy; a requested validation failure aborts freezing.}
\item{release_id}{Registered frozen-release UUID.}
\item{extract_to}{Optional new extraction directory whose parent exists.}
\item{operation_id}{Committed working-draft checkpoint identifier.}
\item{cache}{Consumer cache root outside the Data Pond.}
\item{keep_zip}{Keep the transport ZIP in addition to the extracted cache. Default false.}
\item{directory}{One cached-release directory returned by the cache helper.}
}
\details{Before previewing, explicitly save/download and review the working PDF
using Human-readable metadata in the app. Preparation requires Quarto and XeLaTeX;
release creation requires Python 3 and preserves the prepared PDF and citation
without rendering them again. Missing, modified or stale documents prevent
freezing. All dataset files
are previewed and hashed. Development documents are copied without execution.
Close succeeds only if the working data and metadata still match the release.
Restore retains dataset identity and checkpoints an existing draft. Shared
reference changes must be resolved before restoration. Interrupted operations
require recovery before another lifecycle action. Releases and checkpoints have
no automatic expiry. The cache helper reads local releases; remote publication,
Flow download and QGIS integration are not implemented by these functions.}
\value{Preview returns the source file inventory and metadata fingerprint.
Creation and archive closure return a release row; history and checkpoint
listing return data frames. Verification invisibly returns verified paths and
the release record. Restoration returns dataset ID, new draft path, checkpoint
directory and release ID. Recovery returns status messages. Caching returns the
new cached-release directory; cache verification invisibly returns its receipt.}
