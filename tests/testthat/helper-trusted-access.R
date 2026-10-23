# Existing module tests isolate business behaviour using trusted fixture users.
# Authentication and real browser boundaries are exercised without these mocks
# in test-access-control.R. Keep this opt-in and scoped to the individual test.
local_trusted_access_fixture <- function(.env = parent.frame()) {
  original_admin <- datapond:::is_user_admin
  original_reviewer <- datapond:::review_require_user
  testthat::local_mocked_bindings(
    access_allowed = function(...) TRUE,
    require_active_user_for_write = function(state, ...) datapond:::has_active_user_id(state$active_user_id),
    is_user_admin = function(user_id, base_path) shiny::withReactiveDomain(NULL, original_admin(user_id, base_path)),
    review_require_user = function(...) shiny::withReactiveDomain(NULL, original_reviewer(...)),
    .package = "datapond", .env = .env
  )
}
