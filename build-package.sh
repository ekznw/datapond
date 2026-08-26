#!/usr/bin/env bash

set -euo pipefail

project_dir="$(
  cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
  pwd
)"
artifact_dir="${1:-${project_dir}/dist}"
r_command="${R_COMMAND:-R}"
rscript_command="${RSCRIPT_COMMAND:-Rscript}"

cd -- "${project_dir}"

if ! command -v "${r_command}" >/dev/null 2>&1; then
  echo "Error: ${r_command} is not available on PATH." >&2
  exit 1
fi

if ! command -v "${rscript_command}" >/dev/null 2>&1; then
  echo "Error: ${rscript_command} is not available on PATH." >&2
  exit 1
fi

package_name="$(${rscript_command} --vanilla -e \
  'cat(read.dcf("DESCRIPTION", fields = "Package")[[1]])' \
  2>/dev/null)"
package_version="$(${rscript_command} --vanilla -e \
  'cat(read.dcf("DESCRIPTION", fields = "Version")[[1]])' \
  2>/dev/null)"

archive_path="${artifact_dir}/${package_name}_${package_version}.tar.gz"
check_path="${artifact_dir}/${package_name}.Rcheck"
check_log="${check_path}/00check.log"

mkdir -p "${artifact_dir}"

echo "Building ${package_name} ${package_version}..."
(
  cd -- "${artifact_dir}"
  "${r_command}" CMD build "${project_dir}"
)

if [[ ! -f "${archive_path}" ]]; then
  echo "Error: expected package archive was not created: ${archive_path}" >&2
  exit 1
fi

echo "Checking ${archive_path}..."
(
  cd -- "${artifact_dir}"
  "${r_command}" CMD check --no-manual "${archive_path}"
)

check_status="$(awk '
  /^Status:/ {
    sub(/^Status:[[:space:]]*/, "")
    print
    exit
  }
' "${check_log}")"

echo
echo "Package: ${archive_path}"
echo "Check:   ${check_path}"

if [[ "${check_status}" != "OK" ]]; then
  echo "Check status: ${check_status:-unknown}" >&2
  exit 1
fi

echo "Check status: OK"
