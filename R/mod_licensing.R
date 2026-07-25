# mod_licensing.R
#
# Guided dataset sharing, rights and licensing assessment.


licensing_rights_basis_choices <- c(
  "Select the rights basis" = "",
  "EKZNW-created or EKZNW-owned" =
    "ekznw_owned",
  "External organisation \u2014 existing licence applies" =
    "external_licence",
  "External organisation \u2014 written redistribution permission" =
    "external_permission",
  "Mixed or derived from several rights holders" =
    "mixed_rights",
  "Rights or permission unclear" =
    "rights_unknown",
  "Existing record \u2014 rights basis needs review" =
    "review_required"
)


licensing_access_level_choices <- c(
  "Select intended access" = "",
  "Public" =
    "public",
  "Public after request or registration" =
    "public_request",
  "Approved partners" =
    "approved_partners",
  "EKZNW internal" =
    "internal",
  "Embargoed" =
    "embargoed",
  "Not distributable" =
    "not_distributable"
)


licensing_redistribution_choices <- c(
  "Select redistribution permission" = "",
  "Unrestricted reuse" =
    "unrestricted",
  "Permitted with attribution" =
    "permitted_with_attribution",
  "Permitted with additional conditions" =
    "permitted_with_conditions",
  "Approved recipients only" =
    "approved_recipients_only",
  "Redistribution prohibited" =
    "prohibited",
  "Unknown" =
    "unknown"
)


licensing_sensitivity_choices <- c(
  "No sensitive content" =
    "none",
  "Sensitive content removed or generalised" =
    "removed_or_generalised",
  "Sensitive \u2014 approved recipients only" =
    "approved_recipients_only",
  "Confidential or contractually restricted" =
    "confidential",
  "Sensitivity review required" =
    "review_required"
)


licensing_template_redistribution <- function(
  identifier
) {
  switch(
    as.character(identifier),

    "CC0-1.0" =
      "unrestricted",

    "CC-BY-4.0" =
      "permitted_with_attribution",

    "CC-BY-SA-4.0" =
      "permitted_with_conditions",

    "CC-BY-NC-4.0" =
      "permitted_with_conditions",

    "ODbL-1.0" =
      "permitted_with_conditions",

    "PROVIDER-LICENCE" =
      "permitted_with_conditions",

    "REDISTRIBUTION-PERMISSION" =
      "permitted_with_conditions",

    "EKZNW-PARTNER" =
      "approved_recipients_only",

    "EKZNW-INTERNAL" =
      "prohibited",

    "NO-LICENCE" =
      "prohibited",

    ""
  )
}


licensing_template_requires_attribution <- function(
  identifier
) {
  as.character(identifier) %in%
    c(
      "CC-BY-4.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "EKZNW-PARTNER"
    )
}


licensing_allowed_template_identifiers <- function(
  rights_basis_code,
  access_level_code
) {
  all_identifiers <- c(
    "CC-BY-4.0",
    "CC0-1.0",
    "CC-BY-SA-4.0",
    "CC-BY-NC-4.0",
    "ODbL-1.0",
    "PROVIDER-LICENCE",
    "REDISTRIBUTION-PERMISSION",
    "EKZNW-PARTNER",
    "EKZNW-INTERNAL",
    "CUSTOM-TERMS",
    "NO-LICENCE"
  )

  rights_identifiers <- switch(
    as.character(
      rights_basis_code
    ),

    ekznw_owned = c(
      "CC-BY-4.0",
      "CC0-1.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "EKZNW-PARTNER",
      "EKZNW-INTERNAL",
      "CUSTOM-TERMS",
      "NO-LICENCE"
    ),

    external_licence = c(
      "PROVIDER-LICENCE",
      "CC-BY-4.0",
      "CC0-1.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "CUSTOM-TERMS"
    ),

    external_permission = c(
      "REDISTRIBUTION-PERMISSION",
      "CUSTOM-TERMS"
    ),

    mixed_rights = c(
      "CUSTOM-TERMS",
      "EKZNW-PARTNER",
      "EKZNW-INTERNAL",
      "NO-LICENCE"
    ),

    rights_unknown = "NO-LICENCE",

    review_required =
      all_identifiers,

    all_identifiers
  )

  access_identifiers <- switch(
    as.character(
      access_level_code
    ),

    public = c(
      "CC-BY-4.0",
      "CC0-1.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "PROVIDER-LICENCE",
      "REDISTRIBUTION-PERMISSION",
      "CUSTOM-TERMS"
    ),

    public_request = c(
      "CC-BY-4.0",
      "CC0-1.0",
      "CC-BY-SA-4.0",
      "CC-BY-NC-4.0",
      "ODbL-1.0",
      "PROVIDER-LICENCE",
      "REDISTRIBUTION-PERMISSION",
      "CUSTOM-TERMS"
    ),

    approved_partners = c(
      "EKZNW-PARTNER",
      "PROVIDER-LICENCE",
      "REDISTRIBUTION-PERMISSION",
      "CUSTOM-TERMS",
      "NO-LICENCE"
    ),

    internal = c(
      "EKZNW-INTERNAL",
      "PROVIDER-LICENCE",
      "CUSTOM-TERMS",
      "NO-LICENCE"
    ),

    not_distributable = c(
      "NO-LICENCE",
      "EKZNW-INTERNAL",
      "CUSTOM-TERMS"
    ),

    # An embargo is temporary and may coexist with
    # public or controlled terms after the embargo.
    embargoed =
      all_identifiers,

    all_identifiers
  )

  intersect(
    rights_identifiers,
    access_identifiers
  )
}


licensing_recommended_identifier <- function(
  rights_basis_code,
  access_level_code
) {
  rights_basis_code <- as.character(
    rights_basis_code
  )

  access_level_code <- as.character(
    access_level_code
  )

  if (
    identical(
      rights_basis_code,
      "rights_unknown"
    ) ||
    identical(
      access_level_code,
      "not_distributable"
    )
  ) {
    return(
      "NO-LICENCE"
    )
  }

  if (identical(
    rights_basis_code,
    "external_licence"
  )) {
    return(
      "PROVIDER-LICENCE"
    )
  }

  if (identical(
    rights_basis_code,
    "external_permission"
  )) {
    return(
      "REDISTRIBUTION-PERMISSION"
    )
  }

  if (identical(
    rights_basis_code,
    "mixed_rights"
  )) {
    return(
      "CUSTOM-TERMS"
    )
  }

  if (identical(
    access_level_code,
    "internal"
  )) {
    return(
      "EKZNW-INTERNAL"
    )
  }

  if (identical(
    access_level_code,
    "approved_partners"
  )) {
    return(
      "EKZNW-PARTNER"
    )
  }

  if (
    identical(
      rights_basis_code,
      "ekznw_owned"
    ) &&
    access_level_code %in%
      c(
        "public",
        "public_request",
        "embargoed"
      )
  ) {
    return(
      "CC-BY-4.0"
    )
  }

  NA_character_
}


mod_licensing_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      "Sharing rights and licence"
    ),

    shiny::tags$div(
      style = paste(
        "padding:10px;",
        "background:#F1E9DA;",
        "border:1px solid #B99572;",
        "margin-bottom:14px;"
      ),

      shiny::tags$p(
        style = "margin:0;",

        paste(
          "Work through the questions below rather than choosing",
          "a licence from an unexplained list. Folder placement may",
          "provide context, but src/, dev/ or arc/ does not by itself",
          "determine ownership or redistribution rights."
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        shiny::h4(
          "1. Sharing authority"
        ),

        shiny::selectInput(
          ns("rights_basis_code"),
          "Who controls the sharing rights?",
          choices =
            licensing_rights_basis_choices,
          selected = ""
        ),

        shiny::checkboxInput(
          ns("use_dataset_owner_as_rights_holder"),
          paste(
            "Use the dataset owning organisation",
            "as the rights holder"
          ),
          value = TRUE
        ),

        shiny::uiOutput(
          ns("dataset_owner_sync_status")
        ),

        shiny::selectizeInput(
          ns("rights_holder_organisation_id"),
          "Rights holder organisation",
          choices = NULL,
          selected = character(),
          multiple = FALSE,
          options = list(
            placeholder =
              "Select an organisation where available"
          )
        ),

        shiny::textInput(
          ns("rights_holder_other"),
          "Other or unlisted rights holder",
          placeholder = paste(
            "Use only when the rights holder is not",
            "available in the organisation register."
          )
        ),

        shiny::hr(),

        shiny::h4(
          "2. Intended access"
        ),

        shiny::selectInput(
          ns("access_level_code"),
          "Who may receive the dataset?",
          choices =
            licensing_access_level_choices,
          selected = ""
        ),

        shiny::selectInput(
          ns("redistribution_permission_code"),
          "What may recipients do?",
          choices =
            licensing_redistribution_choices,
          selected = ""
        ),

        shiny::hr(),

        shiny::h4(
          "3. Licence or permission basis"
        ),

        shiny::uiOutput(
          ns("recommended_licence")
        ),

        shiny::checkboxInput(
          ns("show_advanced_licences"),
          "Show advanced licence options",
          value = FALSE
        ),

        shiny::selectInput(
          ns("license_template_id"),
          "Licence or sharing terms",
          choices = character(),
          selected = character(0)
        ),

        shiny::uiOutput(
          ns("selected_licence_summary")
        ),

        shiny::conditionalPanel(
          condition = sprintf(
            paste0(
              "input['%s'] == 'external_licence'",
              " || input['%s'] == 'external_permission'"
            ),
            ns("rights_basis_code"),
            ns("rights_basis_code")
          ),

          shiny::textAreaInput(
            ns("permission_reference"),
            "Licence or permission reference",
            placeholder = paste(
              "Identify the provider licence, agreement,",
              "memorandum, letter, email or supporting document",
              "that establishes the sharing rights."
            ),
            rows = 3
          )
        ),

        shiny::hr(),

        shiny::h4(
          "4. Attribution"
        ),

        shiny::checkboxInput(
          ns("required_attribution"),
          "Acknowledgement or attribution is required",
          value = FALSE
        ),

        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] == true",
            ns("required_attribution")
          ),

          shiny::tags$div(
            style = paste(
              "padding:10px;",
              "background:#F8F4EA;",
              "border:1px solid #ddd;",
              "margin-bottom:10px;"
            ),

            shiny::strong(
              "Suggested dataset citation"
            ),

            shiny::uiOutput(
              ns("licensing_citation_preview")
            )
          ),

          shiny::checkboxInput(
            ns("use_suggested_citation"),
            "Use the suggested citation as the attribution statement",
            value = TRUE
          ),

          shiny::textAreaInput(
            ns("attribution_text"),
            "Required acknowledgement or attribution text",
            placeholder = paste(
              "State the exact wording that must accompany",
              "redistribution or reuse."
            ),
            rows = 4
          )
        ),

        shiny::hr(),

        shiny::h4(
          "5. Sensitive information"
        ),

        shiny::selectInput(
          ns("sensitivity_handling_code"),
          "Sensitivity handling",
          choices =
            licensing_sensitivity_choices,
          selected = "none"
        ),

        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] != 'none'",
            ns("sensitivity_handling_code")
          ),

          shiny::textAreaInput(
            ns("sensitivity_reason"),
            "Sensitivity reason and handling instructions",
            placeholder = paste(
              "Describe the sensitive content, mitigation,",
              "approved recipients and any handling requirements."
            ),
            rows = 4
          )
        ),

        shiny::hr(),

        shiny::h4(
          "Additional terms"
        ),

        shiny::textAreaInput(
          ns("use_constraints"),
          "Additional use or redistribution conditions",
          placeholder = paste(
            "Record conditions not already represented by",
            "the selected licence or access classification."
          ),
          rows = 4
        ),

        shiny::dateInput(
          ns("terms_review_date"),
          "Terms review date",
          value = NULL,
          format = "yyyy-mm-dd"
        ),

        shiny::textAreaInput(
          ns("licence_notes"),
          "Internal licensing notes",
          placeholder = paste(
            "Record internal interpretation, unresolved questions",
            "or follow-up actions. Do not use this field as the",
            "public licence statement."
          ),
          rows = 4
        )
      ),

      shiny::column(
        width = 5,

        shiny::uiOutput(
          ns("sharing_assessment_summary")
        ),

        shiny::br(),

        shiny::actionButton(
          ns("save_licensing"),
          "Save sharing assessment"
        ),

        shiny::tags$small(
          style = paste(
            "color:#666;",
            "display:block;",
            "margin-top:6px;"
          ),

          paste(
            "The main Save Dataset button also saves this",
            "sharing assessment."
          )
        )
      )
    )
  )
}


mod_licensing_server <- function(
  id,
  state,
  org_mod = NULL,
  citation_text = NULL
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      pending_template_id <-
        shiny::reactiveVal(NULL)

      loading_record <-
        shiny::reactiveVal(FALSE)

      safe_text <- function(
        x,
        default = ""
      ) {
        if (
          is.null(x) ||
          length(x) == 0L ||
          is.na(x[1]) ||
          !nzchar(trimws(as.character(x[1])))
        ) {
          return(default)
        }

        trimws(
          as.character(x[1])
        )
      }

      safe_date <- function(x) {
        value <- safe_text(
          x
        )

        if (!nzchar(value)) {
          return(NULL)
        }

        parsed <- suppressWarnings(
          as.Date(value)
        )

        if (is.na(parsed)) {
          return(NULL)
        }

        parsed
      }

      citation_value <- shiny::reactive({
        if (
          is.null(citation_text) ||
          !is.function(citation_text)
        ) {
          return("")
        }

        safe_text(
          citation_text()
        )
      })

      templates <- shiny::reactive({
        base_path <- state$data_pond

        if (
          is.null(base_path) ||
          length(base_path) == 0L ||
          is.na(base_path[1]) ||
          !nzchar(trimws(base_path[1]))
        ) {
          return(
            data.frame()
          )
        }

        get_license_templates(
          base_path
        )
      })

      organisations <- shiny::reactive({
        state$refresh_org

        base_path <- state$data_pond

        if (
          is.null(base_path) ||
          length(base_path) == 0L ||
          is.na(base_path[1]) ||
          !nzchar(trimws(base_path[1]))
        ) {
          return(
            data.frame()
          )
        }

        get_organisations(
          base_path
        )
      })


      # Dataset organisation and owner helpers ----

      normalise_dataset_organisation_role <- function(x) {
        x <- as.character(x)

        x[is.na(x)] <- ""

        x <- tolower(
          trimws(x)
        )

        x <- gsub(
          pattern = "[_-]+",
          replacement = " ",
          x = x
        )

        x <- gsub(
          pattern = "\\s+",
          replacement = " ",
          x = x
        )

        data.table::fcase(
          x %in% c(
            "owner",
            "owning organisation",
            "owning organization",
            "dataset owner",
            "data owner",
            "resource owner"
          ),
          "owner",

          x %in% c(
            "rights holder",
            "rightsholder",
            "rights-holder",
            "intellectual property rights holder",
            "ip rights holder"
          ),
          "rights_holder",

          x %in% c(
            "licensor",
            "license holder",
            "licence holder"
          ),
          "licensor",

          default = x
        )
      }


      dataset_organisation_rows <- shiny::reactive({
        # The dataset-organisation module may return only the
        # selected organisation ID and its dataset role.
        selected <- if (
          is.null(org_mod) ||
          !is.list(org_mod) ||
          !"orgs" %in% names(org_mod) ||
          !is.function(org_mod$orgs)
        ) {
          NULL
        } else {
          org_mod$orgs()
        }

        if (
          is.null(selected) ||
          nrow(as.data.frame(selected)) == 0L
        ) {
          return(
            data.table::data.table()
          )
        }

        selected <- data.table::as.data.table(
          data.table::copy(selected)
        )

        # Support either ID name returned by the organisation module.
        if (
          !"id" %in% names(selected) &&
          "organisation_id" %in% names(selected)
        ) {
          data.table::setnames(
            selected,
            old = "organisation_id",
            new = "id"
          )
        }

        if (!"id" %in% names(selected)) {
          warning(
            paste(
              "The dataset organisation module returned records",
              "without an id or organisation_id column."
            )
          )

          return(
            data.table::data.table()
          )
        }

        if (!"role" %in% names(selected)) {
          selected[
            ,
            role := NA_character_
          ]
        }

        selected[
          ,
          id :=
            suppressWarnings(
              as.integer(id)
            )
        ]

        selected[
          ,
          role :=
            as.character(role)
        ]

        selected[
          ,
          role_normalised :=
            normalise_dataset_organisation_role(
              role
            )
        ]

        # Retrieve the full organisation register. This contains the
        # authoritative names, short names and organisation codes.
        register <- organisations()

        if (
          is.null(register) ||
          nrow(as.data.frame(register)) == 0L
        ) {
          register <- data.table::data.table()
        } else {
          register <- data.table::as.data.table(
            data.table::copy(register)
          )
        }

        if (
          nrow(register) > 0L &&
          !"id" %in% names(register) &&
          "organisation_id" %in% names(register)
        ) {
          data.table::setnames(
            register,
            old = "organisation_id",
            new = "id"
          )
        }

        if (
          nrow(register) > 0L &&
          "id" %in% names(register)
        ) {
          register[
            ,
            id :=
              suppressWarnings(
                as.integer(id)
              )
          ]
        }

        selected_column <- function(
          candidate_names
        ) {
          matching_field <- candidate_names[
            candidate_names %in%
              names(selected)
          ]

          if (length(matching_field) == 0L) {
            return(
              rep(
                NA_character_,
                nrow(selected)
              )
            )
          }

          value <- as.character(
            selected[[matching_field[1]]]
          )

          value[
            is.na(value) |
              !nzchar(
                trimws(value)
              )
          ] <- NA_character_

          value
        }


        register_column <- function(
          candidate_names
        ) {
          if (
            nrow(register) == 0L ||
            !"id" %in% names(register)
          ) {
            return(
              rep(
                NA_character_,
                nrow(selected)
              )
            )
          }

          matching_field <- candidate_names[
            candidate_names %in%
              names(register)
          ]

          if (length(matching_field) == 0L) {
            return(
              rep(
                NA_character_,
                nrow(selected)
              )
            )
          }

          match_index <- match(
            selected$id,
            register$id
          )

          value <- as.character(
            register[[matching_field[1]]][
              match_index
            ]
          )

          value[
            is.na(value) |
              !nzchar(
                trimws(value)
              )
          ] <- NA_character_

          value
        }


        prefer_first_non_empty <- function(
          primary,
          fallback
        ) {
          primary <- as.character(
            primary
          )

          fallback <- as.character(
            fallback
          )

          primary_missing <- (
            is.na(primary) |
              !nzchar(
                trimws(primary)
              )
          )

          primary[
            primary_missing
          ] <- fallback[
            primary_missing
          ]

          primary
        }


        selected_full_name <- selected_column(
          c(
            "organisation_name_full",
            "organisation_name",
            "name",
            "label"
          )
        )

        registered_full_name <- register_column(
          c(
            "organisation_name_full",
            "organisation_name",
            "name"
          )
        )

        selected_short_name <- selected_column(
          c(
            "organisation_name_short",
            "short_name",
            "short"
          )
        )

        registered_short_name <- register_column(
          c(
            "organisation_name_short",
            "short_name",
            "short"
          )
        )

        selected_code <- selected_column(
          c(
            "organisation_code",
            "code"
          )
        )

        registered_code <- register_column(
          c(
            "organisation_code",
            "code"
          )
        )

        selected_address <- selected_column(
          c(
            "organisation_address",
            "address"
          )
        )

        registered_address <- register_column(
          c(
            "organisation_address",
            "address"
          )
        )

        selected[
          ,
          organisation_name_full :=
            prefer_first_non_empty(
              selected_full_name,
              registered_full_name
            )
        ]

        selected[
          ,
          organisation_name_short :=
            prefer_first_non_empty(
              selected_short_name,
              registered_short_name
            )
        ]

        selected[
          ,
          organisation_code :=
            prefer_first_non_empty(
              selected_code,
              registered_code
            )
        ]

        selected[
          ,
          organisation_address :=
            prefer_first_non_empty(
              selected_address,
              registered_address
            )
        ]

        selected[]
      })


      dataset_owner_candidates <- shiny::reactive({
        rows <- dataset_organisation_rows()

        if (
          is.null(rows) ||
          nrow(rows) == 0L
        ) {
          return(
            data.table::data.table()
          )
        }

        rows[
          role_normalised %in%
            c(
              "owner",
              "rights_holder",
              "licensor"
            )
        ]
      })


      primary_dataset_owner <- shiny::reactive({
        candidates <- dataset_owner_candidates()

        if (
          is.null(candidates) ||
          nrow(candidates) != 1L
        ) {
          return(NULL)
        }

        candidates[
          1L
        ]
      })


      organisation_row_label <- function(
        row,
        default = "Unnamed organisation"
      ) {
        if (
          is.null(row) ||
          nrow(as.data.frame(row)) == 0L
        ) {
          return(default)
        }

        row <- as.data.frame(
          row,
          stringsAsFactors = FALSE
        )

        row_value <- function(
          candidate_names,
          default_value = ""
        ) {
          matching_field <- candidate_names[
            candidate_names %in%
              names(row)
          ]

          if (length(matching_field) == 0L) {
            return(default_value)
          }

          value <- row[[matching_field[1]]]

          if (
            is.null(value) ||
            length(value) == 0L ||
            is.na(value[1]) ||
            !nzchar(
              trimws(
                as.character(
                  value[1]
                )
              )
            )
          ) {
            return(default_value)
          }

          trimws(
            as.character(
              value[1]
            )
          )
        }

        full_name <- row_value(
          c(
            "organisation_name_full",
            "organisation_name",
            "name",
            "label"
          )
        )

        short_name <- row_value(
          c(
            "organisation_name_short",
            "short_name",
            "short"
          )
        )

        code <- row_value(
          c(
            "organisation_code",
            "code"
          )
        )

        organisation_id <- row_value(
          c(
            "id",
            "organisation_id"
          )
        )

        label <- if (nzchar(full_name)) {
          full_name
        } else if (nzchar(short_name)) {
          short_name
        } else if (nzchar(code)) {
          code
        } else if (nzchar(organisation_id)) {
          paste(
            "Organisation",
            organisation_id
          )
        } else {
          default
        }

        # Add the short code only when it is not already the
        # complete display label.
        if (
          nzchar(code) &&
          !identical(
            tolower(
              trimws(label)
            ),
            tolower(
              trimws(code)
            )
          )
        ) {
          label <- paste0(
            label,
            " [",
            code,
            "]"
          )
        }

        label
      }

      output$dataset_owner_sync_status <- shiny::renderUI({
        candidates <- dataset_owner_candidates()

        use_owner <- isTRUE(
          input$use_dataset_owner_as_rights_holder
        )

        if (!isTRUE(use_owner)) {
          return(
            shiny::tags$small(
              style = paste(
                "color:#666;",
                "display:block;",
                "margin-bottom:8px;"
              ),

              paste(
                "Automatic owner synchronisation is disabled.",
                "Select the rights holder manually."
              )
            )
          )
        }

        if (
          is.null(candidates) ||
          nrow(candidates) == 0L
        ) {
          return(
            shiny::tags$div(
              style = paste(
                "padding:7px;",
                "background:#E9DFB5;",
                "border:1px solid #C3A35B;",
                "margin-bottom:8px;"
              ),

              shiny::strong(
                "No owning organisation identified: "
              ),

              paste(
                "add an organisation with the role Owner,",
                "Rights holder or Licensor in Dataset overview,",
                "or select the rights holder manually."
              )
            )
          )
        }

        if (nrow(candidates) > 1L) {
          labels <- vapply(
            seq_len(nrow(candidates)),
            function(i) {
              organisation_row_label(
                candidates[i]
              )
            },
            character(1)
          )

          return(
            shiny::tags$div(
              style = paste(
                "padding:7px;",
                "background:#E9DFB5;",
                "border:1px solid #C3A35B;",
                "margin-bottom:8px;"
              ),

              shiny::strong(
                "Several owning organisations are recorded: "
              ),

              paste(
                labels,
                collapse = "; "
              ),

              shiny::tags$br(),

              shiny::tags$small(
                paste(
                  "Select the appropriate rights holder manually",
                  "or refine the organisation roles."
                )
              )
            )
          )
        }

        owner <- candidates[1]

        shiny::tags$div(
          style = paste(
            "padding:7px;",
            "background:#D1DDC7;",
            "border:1px solid #526657;",
            "margin-bottom:8px;"
          ),

          shiny::strong(
            "Dataset owner: "
          ),

          organisation_row_label(
            owner
          )
        )
      })

      organisation_choices <- shiny::reactive({
        orgs <- organisations()

        if (
          is.null(orgs) ||
          nrow(as.data.frame(orgs)) == 0L
        ) {
          return(
            stats::setNames(
              character(),
              character()
            )
          )
        }

        orgs <- as.data.frame(
          orgs,
          stringsAsFactors = FALSE
        )

        full_name <- if (
          "organisation_name_full" %in%
            names(orgs)
        ) {
          orgs$organisation_name_full
        } else {
          rep("", nrow(orgs))
        }

        short_name <- if (
          "organisation_name_short" %in%
            names(orgs)
        ) {
          orgs$organisation_name_short
        } else {
          rep("", nrow(orgs))
        }

        code <- if (
          "organisation_code" %in%
            names(orgs)
        ) {
          orgs$organisation_code
        } else {
          rep("", nrow(orgs))
        }

        full_name[is.na(full_name)] <- ""
        short_name[is.na(short_name)] <- ""
        code[is.na(code)] <- ""

        labels <- ifelse(
          nzchar(trimws(short_name)),
          short_name,
          full_name
        )

        has_code <- nzchar(
          trimws(code)
        )

        labels[has_code] <- paste0(
          labels[has_code],
          " [",
          code[has_code],
          "]"
        )

        missing_label <- !nzchar(
          trimws(labels)
        )

        labels[missing_label] <- paste(
          "Organisation",
          orgs$id[missing_label]
        )

        stats::setNames(
          as.character(orgs$id),
          labels
        )
      })

      shiny::observe({
        if (
          isTRUE(
            loading_record()
          ) ||
          !isTRUE(
            input$use_dataset_owner_as_rights_holder
          )
        ) {
          return()
        }

        rights_basis <- safe_text(
          input$rights_basis_code
        )

        # Automatic owner selection only applies where an
        # identified rights holder is meaningful.
        if (
          !rights_basis %in%
            c(
              "ekznw_owned",
              "external_licence",
              "external_permission"
            )
        ) {
          return()
        }

        owner <- primary_dataset_owner()

        if (
          is.null(owner) ||
          nrow(as.data.frame(owner)) != 1L ||
          is.na(owner$id[1])
        ) {
          return()
        }

        owner_id <- as.character(
          owner$id[1]
        )

        current_id <- isolate(
          input$rights_holder_organisation_id
        )

        if (
          is.null(current_id) ||
          length(current_id) == 0L ||
          !identical(
            as.character(current_id[1]),
            owner_id
          )
        ) {
          shiny::updateSelectizeInput(
            session,
            "rights_holder_organisation_id",
            selected = owner_id
          )
        }

        # Avoid recording both a registered organisation
        # and an unlisted free-text rights holder.
        if (nzchar(
          safe_text(
            isolate(
              input$rights_holder_other
            )
          )
        )) {
          shiny::updateTextInput(
            session,
            "rights_holder_other",
            value = ""
          )
        }
      })

      selected_holder_organisation <- shiny::reactive({
        selected_id <- suppressWarnings(
          as.integer(
            input$rights_holder_organisation_id
          )
        )

        orgs <- organisations()

        if (
          is.na(selected_id) ||
          is.null(orgs) ||
          nrow(as.data.frame(orgs)) == 0L
        ) {
          return(NULL)
        }

        row <- orgs[
          as.integer(orgs$id) ==
            selected_id,
          ,
          drop = FALSE
        ]

        if (nrow(row) == 0L) {
          return(NULL)
        }

        row[1, , drop = FALSE]
      })

      rights_holder_name <- shiny::reactive({
        org_row <-
          selected_holder_organisation()

        other <- safe_text(
          input$rights_holder_other
        )

        if (!is.null(org_row)) {
          full_name <- if (
            "organisation_name_full" %in%
              names(org_row)
          ) {
            safe_text(
              org_row$organisation_name_full
            )
          } else {
            ""
          }

          short_name <- if (
            "organisation_name_short" %in%
              names(org_row)
          ) {
            safe_text(
              org_row$organisation_name_short
            )
          } else {
            ""
          }

          if (nzchar(full_name)) {
            return(full_name)
          }

          if (nzchar(short_name)) {
            return(short_name)
          }
        }

        other
      })

      filtered_templates <- shiny::reactive({
        dt <- templates()

        if (
          is.null(dt) ||
          nrow(as.data.frame(dt)) == 0L
        ) {
          return(
            data.table::data.table()
          )
        }

        dt <- data.table::as.data.table(
          data.table::copy(dt)
        )

        allowed <- licensing_allowed_template_identifiers(
          rights_basis_code =
            input$rights_basis_code,
          access_level_code =
            input$access_level_code
        )

        dt <- dt[
          license_identifier %in%
            allowed
        ]

        retained_id <- pending_template_id() %||%
          isolate(
            input$license_template_id
          )

        if (!isTRUE(
          input$show_advanced_licences
        )) {
          dt <- dt[
            data.table::fcoalesce(
              as.integer(is_advanced),
              0L
            ) != 1L |
              as.character(id) ==
                as.character(retained_id)
          ]
        }

        data.table::setorder(
          dt,
          sort_order,
          license_identifier
        )

        dt[]
      })

      selected_template <- shiny::reactive({
        selected_id <- suppressWarnings(
          as.integer(
            input$license_template_id
          )
        )

        dt <- templates()

        if (
          is.na(selected_id) ||
          is.null(dt) ||
          nrow(as.data.frame(dt)) == 0L
        ) {
          return(NULL)
        }

        row <- dt[
          as.integer(dt$id) ==
            selected_id,
          ,
          drop = FALSE
        ]

        if (nrow(row) == 0L) {
          return(NULL)
        }

        row[1, , drop = FALSE]
      })

      shiny::observe({
        choices <-
          organisation_choices()

        current <- isolate(
          input$rights_holder_organisation_id
        )

        if (
          is.null(current) ||
          length(current) == 0L ||
          !as.character(current[1]) %in%
            unname(choices)
        ) {
          current <- character()
        }

        shiny::updateSelectizeInput(
          session,
          "rights_holder_organisation_id",
          choices = choices,
          selected = current,
          server = FALSE
        )
      })

      shiny::observe({
        dt <- filtered_templates()

        if (
          is.null(dt) ||
          nrow(dt) == 0L
        ) {
          shiny::updateSelectInput(
            session,
            "license_template_id",
            choices = character(),
            selected = character(0)
          )

          return()
        }

        groups <- unique(
          dt$ui_group
        )

        choices <- stats::setNames(
          lapply(
            groups,
            function(group_name) {
              rows <- dt[
                ui_group == group_name
              ]

              stats::setNames(
                as.character(
                  rows$id
                ),
                rows$ui_label
              )
            }
          ),
          groups
        )

        valid_ids <- unlist(
          choices,
          use.names = FALSE
        )

        pending <- pending_template_id()

        current <- isolate(
          input$license_template_id
        )

        recommended_identifier <-
          licensing_recommended_identifier(
            input$rights_basis_code,
            input$access_level_code
          )

        recommended_row <- dt[
          license_identifier ==
            recommended_identifier
        ]

        recommended_id <- if (
          nrow(recommended_row) > 0L
        ) {
          as.character(
            recommended_row$id[1]
          )
        } else {
          character()
        }

        selected <- if (
          !is.null(pending) &&
          as.character(pending) %in%
            valid_ids
        ) {
          as.character(pending)
        } else if (
          !is.null(current) &&
          length(current) > 0L &&
          as.character(current[1]) %in%
            valid_ids
        ) {
          as.character(current[1])
        } else {
          recommended_id
        }

        shiny::updateSelectInput(
          session,
          "license_template_id",
          choices = choices,
          selected = selected
        )

        if (!is.null(pending)) {
          pending_template_id(NULL)
        }
      })

      shiny::observeEvent(
        input$license_template_id,
        {
          template <-
            selected_template()

          if (
            is.null(template) ||
            nrow(as.data.frame(template)) == 0L
          ) {
            return()
          }

          identifier <- safe_text(
            template$license_identifier
          )

          redistribution <-
            licensing_template_redistribution(
              identifier
            )

          if (nzchar(redistribution)) {
            shiny::updateSelectInput(
              session,
              "redistribution_permission_code",
              selected = redistribution
            )
          }

          if (
            licensing_template_requires_attribution(
              identifier
            )
          ) {
            shiny::updateCheckboxInput(
              session,
              "required_attribution",
              value = TRUE
            )
          } else if (
            identical(
              identifier,
              "CC0-1.0"
            )
          ) {
            shiny::updateCheckboxInput(
              session,
              "required_attribution",
              value = FALSE
            )
          }
        },
        ignoreInit = TRUE
      )

      shiny::observe({
        use_citation <- isTRUE(
          input$required_attribution
        ) &&
          isTRUE(
            input$use_suggested_citation
          )

        citation <- citation_value()

        if (
          isTRUE(use_citation) &&
          nzchar(citation)
        ) {
          shiny::updateTextAreaInput(
            session,
            "attribution_text",
            value = citation
          )
        }
      })

      output$licensing_citation_preview <- shiny::renderUI({
        citation <- citation_value()

        if (!nzchar(citation)) {
          return(
            shiny::tags$small(
              style = "color:#666;",

              paste(
                "Complete the citation fields before",
                "using the generated citation as attribution."
              )
            )
          )
        }

        shiny::tags$div(
          class =
            "suggested-citation",
          citation
        )
      })

      output$recommended_licence <- shiny::renderUI({
        identifier <- licensing_recommended_identifier(
            input$rights_basis_code,
            input$access_level_code
          )

        if (
          is.na(identifier) ||
          !nzchar(identifier)
        ) {
          return(
            shiny::tags$small(
              style = "color:#666;",

              paste(
                "Select the sharing authority and intended",
                "access to receive a recommendation."
              )
            )
          )
        }

        dt <- templates()

        row <- dt[
          dt$license_identifier ==
            identifier,
          ,
          drop = FALSE
        ]

        label <- if (nrow(row) > 0L) {
          safe_text(
            row$ui_label,
            default = identifier
          )
        } else {
          identifier
        }

        shiny::tags$div(
          style = paste(
            "padding:8px;",
            "background:#D1DDC7;",
            "border:1px solid #526657;",
            "margin-bottom:8px;"
          ),

          shiny::strong(
            "Recommended: "
          ),

          label
        )
      })

      output$selected_licence_summary <- shiny::renderUI({
        template <- selected_template()

        if (
          is.null(template) ||
          nrow(as.data.frame(template)) == 0L
        ) {
          return(NULL)
        }

        shiny::tags$div(
          style = paste(
            "padding:8px;",
            "background:#F8F4EA;",
            "border:1px solid #ddd;",
            "margin-bottom:10px;"
          ),

          shiny::strong(
            safe_text(
              template$license_name
            )
          ),

          shiny::tags$p(
            style = "margin:5px 0;",

            safe_text(
              template$description
            )
          ),

          if (nzchar(
            safe_text(
              template$notes
            )
          )) {
            shiny::tags$small(
              safe_text(
                template$notes
              )
            )
          }
        )
      })

      values <- shiny::reactive({
        org_row <-
          selected_holder_organisation()

        holder_org_id <- if (
          is.null(org_row)
        ) {
          NA_integer_
        } else {
          as.integer(
            org_row$id[1]
          )
        }

        terms_review_date <- if (
          is.null(input$terms_review_date) ||
          length(input$terms_review_date) == 0L ||
          is.na(input$terms_review_date[1])
        ) {
          NA_character_
        } else {
          format(
            as.Date(
              input$terms_review_date[1]
            ),
            "%Y-%m-%d"
          )
        }

        list(
          license_template_id =
            suppressWarnings(
              as.integer(
                input$license_template_id
              )
            ),

          rights_basis_code =
            safe_text(
              input$rights_basis_code,
              default = NA_character_
            ),

          rights_holder_organisation_id =
            holder_org_id,

          license_holder =
            safe_text(
              rights_holder_name(),
              default = NA_character_
            ),

          access_level_code =
            safe_text(
              input$access_level_code,
              default = NA_character_
            ),

          redistribution_permission_code =
            safe_text(
              input$redistribution_permission_code,
              default = NA_character_
            ),

          permission_reference =
            safe_text(
              input$permission_reference,
              default = NA_character_
            ),

          required_attribution =
            isTRUE(
              input$required_attribution
            ),

          attribution_text =
            safe_text(
              input$attribution_text,
              default = NA_character_
            ),

          use_constraints =
            safe_text(
              input$use_constraints,
              default = NA_character_
            ),

          sensitivity_handling_code =
            safe_text(
              input$sensitivity_handling_code,
              default = "none"
            ),

          sensitivity_reason =
            safe_text(
              input$sensitivity_reason,
              default = NA_character_
            ),

          terms_review_date =
            terms_review_date,

          licence_notes =
            safe_text(
              input$licence_notes,
              default = NA_character_
            )
        )
      })

      has_any_values <- shiny::reactive({
        vals <- values()

        rights_basis_code <- safe_text(
          vals$rights_basis_code,
          default = ""
        )

        license_holder <- safe_text(
          vals$license_holder,
          default = ""
        )

        access_level_code <- safe_text(
          vals$access_level_code,
          default = ""
        )

        redistribution_permission_code <- safe_text(
          vals$redistribution_permission_code,
          default = ""
        )

        permission_reference <- safe_text(
          vals$permission_reference,
          default = ""
        )

        attribution_text <- safe_text(
          vals$attribution_text,
          default = ""
        )

        use_constraints <- safe_text(
          vals$use_constraints,
          default = ""
        )

        sensitivity_handling_code <- safe_text(
          vals$sensitivity_handling_code,
          default = "none"
        )

        sensitivity_reason <- safe_text(
          vals$sensitivity_reason,
          default = ""
        )

        terms_review_date <- safe_text(
          vals$terms_review_date,
          default = ""
        )

        licence_notes <- safe_text(
          vals$licence_notes,
          default = ""
        )

        template_selected <- (
          !is.null(
            vals$license_template_id
          ) &&
            length(
              vals$license_template_id
            ) > 0L &&
            !is.na(
              vals$license_template_id[1]
            )
        )

        holder_organisation_selected <- (
          !is.null(
            vals$rights_holder_organisation_id
          ) &&
            length(
              vals$rights_holder_organisation_id
            ) > 0L &&
            !is.na(
              vals$rights_holder_organisation_id[1]
            )
        )

        isTRUE(
          any(
            c(
              nzchar(
                rights_basis_code
              ),

              nzchar(
                license_holder
              ),

              holder_organisation_selected,

              nzchar(
                access_level_code
              ),

              nzchar(
                redistribution_permission_code
              ),

              template_selected,

              nzchar(
                permission_reference
              ),

              isTRUE(
                vals$required_attribution
              ),

              nzchar(
                attribution_text
              ),

              !identical(
                sensitivity_handling_code,
                "none"
              ),

              nzchar(
                sensitivity_reason
              ),

              nzchar(
                use_constraints
              ),

              nzchar(
                terms_review_date
              ),

              nzchar(
                licence_notes
              )
            )
          )
        )
      })

      validate <- function(
        strict = FALSE
      ) {
        vals <- values()

        # ------------------------------------------------------------
        # Normalise all scalar values
        # ------------------------------------------------------------

        rights_basis_code <- safe_text(
          vals$rights_basis_code,
          default = ""
        )

        access_level_code <- safe_text(
          vals$access_level_code,
          default = ""
        )

        redistribution_permission_code <- safe_text(
          vals$redistribution_permission_code,
          default = ""
        )

        sensitivity_handling_code <- safe_text(
          vals$sensitivity_handling_code,
          default = "none"
        )

        license_holder <- safe_text(
          vals$license_holder,
          default = ""
        )

        permission_reference <- safe_text(
          vals$permission_reference,
          default = ""
        )

        attribution_text <- safe_text(
          vals$attribution_text,
          default = ""
        )

        use_constraints <- safe_text(
          vals$use_constraints,
          default = ""
        )

        sensitivity_reason <- safe_text(
          vals$sensitivity_reason,
          default = ""
        )

        problems <- character()
        missing <- character()

        # ------------------------------------------------------------
        # Resolve the selected rights-holder organisation
        # ------------------------------------------------------------

        holder_id_value <-
          vals$rights_holder_organisation_id

        selected_org_id <- if (
          is.null(holder_id_value) ||
          length(holder_id_value) == 0L ||
          is.na(holder_id_value[1])
        ) {
          NA_integer_
        } else {
          suppressWarnings(
            as.integer(
              holder_id_value[1]
            )
          )
        }

        other_holder <- safe_text(
          input$rights_holder_other,
          default = ""
        )

        if (
          !is.na(selected_org_id) &&
          nzchar(other_holder)
        ) {
          problems <- c(
            problems,
            paste(
              "Select a registered rights-holder organisation",
              "or enter an unlisted rights holder, not both."
            )
          )
        }

        # ------------------------------------------------------------
        # Resolve the selected licence template
        # ------------------------------------------------------------

        template <- selected_template()

        identifier <- if (
          is.null(template) ||
          nrow(as.data.frame(template)) == 0L
        ) {
          ""
        } else {
          safe_text(
            template$license_identifier,
            default = ""
          )
        }

        allowed_identifiers <-
          licensing_allowed_template_identifiers(
            rights_basis_code =
              rights_basis_code,
            access_level_code =
              access_level_code
          )

        if (
          nzchar(identifier) &&
          !identifier %in%
            allowed_identifiers
        ) {
          problems <- c(
            problems,
            paste(
              "The selected licence is inconsistent with",
              "the sharing authority or intended access."
            )
          )
        }

        # ------------------------------------------------------------
        # Check the owning-organisation relationship
        # ------------------------------------------------------------

        owner <- primary_dataset_owner()

        if (
          identical(
            rights_basis_code,
            "ekznw_owned"
          ) &&
          !is.null(owner) &&
          nrow(as.data.frame(owner)) == 1L &&
          "id" %in% names(owner) &&
          !is.na(owner$id[1]) &&
          !is.na(selected_org_id) &&
          as.integer(selected_org_id) !=
            as.integer(owner$id[1])
        ) {
          problems <- c(
            problems,
            paste(
              "The selected rights holder differs from",
              "the organisation recorded as the dataset owner."
            )
          )
        }

        # ------------------------------------------------------------
        # Rights and access consistency
        # ------------------------------------------------------------

        if (
          identical(
            rights_basis_code,
            "rights_unknown"
          ) &&
          access_level_code %in%
            c(
              "public",
              "public_request",
              "approved_partners"
            )
        ) {
          problems <- c(
            problems,
            paste(
              "A dataset with unclear rights cannot be",
              "marked for public or partner redistribution."
            )
          )
        }

        if (
          identical(
            redistribution_permission_code,
            "prohibited"
          ) &&
          access_level_code %in%
            c(
              "public",
              "public_request"
            )
        ) {
          problems <- c(
            problems,
            paste(
              "Public access is inconsistent with",
              "prohibited redistribution."
            )
          )
        }

        if (
          identical(
            access_level_code,
            "not_distributable"
          ) &&
          redistribution_permission_code %in%
            c(
              "unrestricted",
              "permitted_with_attribution",
              "permitted_with_conditions"
            )
        ) {
          problems <- c(
            problems,
            paste(
              "A dataset marked as not distributable cannot",
              "also permit redistribution."
            )
          )
        }

        # ------------------------------------------------------------
        # Sensitivity and access consistency
        # ------------------------------------------------------------

        restricted_sensitivity <- (
          sensitivity_handling_code %in%
            c(
              "approved_recipients_only",
              "confidential",
              "review_required"
            )
        )

        public_access <- (
          access_level_code %in%
            c(
              "public",
              "public_request"
            )
        )

        if (
          isTRUE(
            restricted_sensitivity
          ) &&
          isTRUE(
            public_access
          )
        ) {
          problems <- c(
            problems,
            paste(
              "Sensitive or review-pending data cannot be",
              "marked as publicly accessible."
            )
          )
        }

        # ------------------------------------------------------------
        # Complete-record requirements
        # ------------------------------------------------------------

        if (isTRUE(strict)) {
          if (
            !nzchar(
              rights_basis_code
            ) ||
            identical(
              rights_basis_code,
              "review_required"
            )
          ) {
            missing <- c(
              missing,
              "Sharing authority"
            )
          }

          if (!nzchar(
            license_holder
          )) {
            missing <- c(
              missing,
              "Rights holder"
            )
          }

          if (!nzchar(
            access_level_code
          )) {
            missing <- c(
              missing,
              "Intended access"
            )
          }

          if (!nzchar(
            redistribution_permission_code
          )) {
            missing <- c(
              missing,
              "Redistribution permission"
            )
          }

          if (!nzchar(
            identifier
          )) {
            missing <- c(
              missing,
              "Licence or sharing terms"
            )
          }

          if (
            rights_basis_code %in%
              c(
                "external_licence",
                "external_permission"
              ) &&
            !nzchar(
              permission_reference
            )
          ) {
            missing <- c(
              missing,
              "Licence or permission reference"
            )
          }

          if (
            isTRUE(
              vals$required_attribution
            ) &&
            !nzchar(
              attribution_text
            )
          ) {
            missing <- c(
              missing,
              "Attribution text"
            )
          }

          if (
            !identical(
              sensitivity_handling_code,
              "none"
            ) &&
            !nzchar(
              sensitivity_reason
            )
          ) {
            missing <- c(
              missing,
              "Sensitivity reason"
            )
          }

          if (
            identical(
              rights_basis_code,
              "mixed_rights"
            ) &&
            !nzchar(
              use_constraints
            )
          ) {
            missing <- c(
              missing,
              "Mixed-rights conditions"
            )
          }
        }

        problems <- unique(
          problems
        )

        missing <- unique(
          missing
        )

        all_issues <- c(
          problems,

          if (length(missing) > 0L) {
            paste0(
              "Missing: ",
              paste(
                missing,
                collapse = "; "
              )
            )
          } else {
            character()
          }
        )

        list(
          valid = (
            length(problems) == 0L &&
              length(missing) == 0L
          ),

          problems =
            problems,

          missing =
            missing,

          message = if (
            length(all_issues) == 0L
          ) {
            ""
          } else {
            paste(
              all_issues,
              collapse = "\n"
            )
          }
        )
      }

      output$sharing_assessment_summary <-
        shiny::renderUI({
          vals <- values()

          if (!isTRUE(
            has_any_values()
          )) {
            return(
              shiny::tags$div(
                style = paste(
                  "padding:12px;",
                  "background:#F8F4EA;",
                  "border:1px solid #ccc;"
                ),

                shiny::strong(
                  "Sharing assessment not started"
                ),

                shiny::tags$p(
                  style =
                    "margin:6px 0 0 0;",

                  paste(
                    "Complete the guided questions to record",
                    "who controls the rights and how the",
                    "dataset may be shared."
                  )
                )
              )
            )
          }

          check <- validate(
            strict = TRUE
          )

          rights_basis_code <- safe_text(
            vals$rights_basis_code,
            default = ""
          )

          access_level_code <- safe_text(
            vals$access_level_code,
            default = ""
          )

          redistribution_permission_code <-
            safe_text(
              vals$redistribution_permission_code,
              default = ""
            )

          sensitivity_handling_code <-
            safe_text(
              vals$sensitivity_handling_code,
              default = "none"
            )

          rights_basis_label <- switch(
            rights_basis_code,

            ekznw_owned =
              "EKZNW-created or owned",

            external_licence =
              "External provider licence",

            external_permission =
              "External redistribution permission",

            mixed_rights =
              "Mixed or derived rights",

            rights_unknown =
              "Rights unclear",

            review_required =
              "Rights basis needs review",

            "Not recorded"
          )

          access_label <- switch(
            access_level_code,

            public =
              "Public",

            public_request =
              "Public after request or registration",

            approved_partners =
              "Approved partners",

            internal =
              "EKZNW internal",

            embargoed =
              "Embargoed",

            not_distributable =
              "Not distributable",

            "Not recorded"
          )

          redistribution_label <- switch(
            redistribution_permission_code,

            unrestricted =
              "Unrestricted reuse",

            permitted_with_attribution =
              "Permitted with attribution",

            permitted_with_conditions =
              "Permitted with additional conditions",

            approved_recipients_only =
              "Approved recipients only",

            prohibited =
              "Redistribution prohibited",

            unknown =
              "Unknown",

            "Not recorded"
          )

          sensitivity_label <- switch(
            sensitivity_handling_code,

            none =
              "No sensitive content",

            removed_or_generalised =
              "Sensitive content removed or generalised",

            approved_recipients_only =
              "Sensitive \u2014 approved recipients only",

            confidential =
              "Confidential or contractually restricted",

            review_required =
              "Sensitivity review required",

            "Not recorded"
          )

          template <- selected_template()

          template_label <- if (
            is.null(template) ||
            nrow(as.data.frame(template)) == 0L
          ) {
            "Not selected"
          } else {
            safe_text(
              template$ui_label,
              default = safe_text(
                template$license_name,
                default = "Not selected"
              )
            )
          }

          rights_holder_label <- safe_text(
            vals$license_holder,
            default = "Not recorded"
          )

          appearance <- if (
            isTRUE(
              check$valid
            )
          ) {
            list(
              background = "#D1DDC7",
              border = "#526657",
              title =
                "Sharing assessment complete"
            )
          } else {
            list(
              background = "#E9DFB5",
              border = "#C3A35B",
              title =
                "Sharing assessment needs attention"
            )
          }

          summary_row <- function(
            label,
            value
          ) {
            display_value <- safe_text(
              value,
              default = "Not recorded"
            )

            shiny::tags$p(
              style = "margin:4px 0;",

              shiny::strong(
                paste0(
                  label,
                  ": "
                )
              ),

              display_value
            )
          }

          issue_items <- c(
            check$problems,
            check$missing
          )

          issue_items <- issue_items[
            !is.na(issue_items) &
              nzchar(
                trimws(
                  issue_items
                )
              )
          ]

          shiny::tags$div(
            style = paste(
              "padding:12px;",

              paste0(
                "background:",
                appearance$background,
                ";"
              ),

              paste0(
                "border:1px solid ",
                appearance$border,
                ";"
              )
            ),

            shiny::h4(
              style = "margin-top:0;",
              appearance$title
            ),

            summary_row(
              "Sharing authority",
              rights_basis_label
            ),

            summary_row(
              "Rights holder",
              rights_holder_label
            ),

            summary_row(
              "Access",
              access_label
            ),

            summary_row(
              "Licence or terms",
              template_label
            ),

            summary_row(
              "Redistribution",
              redistribution_label
            ),

            summary_row(
              "Sensitivity",
              sensitivity_label
            ),

            if (
              !isTRUE(
                check$valid
              ) &&
              length(issue_items) > 0L
            ) {
              shiny::tags$div(
                style = paste(
                  "margin-top:10px;",
                  "padding-top:8px;",
                  "border-top:1px solid #C3A35B;"
                ),

                shiny::strong(
                  "Required review:"
                ),

                shiny::tags$ul(
                  lapply(
                    issue_items,
                    shiny::tags$li
                  )
                )
              )
            }
          )
        })

      clear <- function() {
        pending_template_id(NULL)

        shiny::updateSelectInput(
          session,
          "rights_basis_code",
          selected = ""
        )

        shiny::updateCheckboxInput(
          session,
          "use_dataset_owner_as_rights_holder",
          value = TRUE
        )

        shiny::updateSelectizeInput(
          session,
          "rights_holder_organisation_id",
          selected = character()
        )

        shiny::updateTextInput(
          session,
          "rights_holder_other",
          value = ""
        )

        shiny::updateSelectInput(
          session,
          "access_level_code",
          selected = ""
        )

        shiny::updateSelectInput(
          session,
          "redistribution_permission_code",
          selected = ""
        )

        shiny::updateSelectInput(
          session,
          "license_template_id",
          selected = character(0)
        )

        shiny::updateCheckboxInput(
          session,
          "show_advanced_licences",
          value = FALSE
        )

        shiny::updateTextAreaInput(
          session,
          "permission_reference",
          value = ""
        )

        shiny::updateCheckboxInput(
          session,
          "required_attribution",
          value = FALSE
        )

        shiny::updateCheckboxInput(
          session,
          "use_suggested_citation",
          value = TRUE
        )

        shiny::updateTextAreaInput(
          session,
          "attribution_text",
          value = ""
        )

        shiny::updateSelectInput(
          session,
          "sensitivity_handling_code",
          selected = "none"
        )

        shiny::updateTextAreaInput(
          session,
          "sensitivity_reason",
          value = ""
        )

        shiny::updateTextAreaInput(
          session,
          "use_constraints",
          value = ""
        )

        shiny::updateDateInput(
          session,
          "terms_review_date",
          value = NULL
        )

        shiny::updateTextAreaInput(
          session,
          "licence_notes",
          value = ""
        )

        invisible(TRUE)
      }

      load <- function(dataset_id) {
        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          clear()
          return(
            invisible(FALSE)
          )
        }

        record <- get_dataset_license(
          dataset_id = as.integer(
            dataset_id[1]
          ),
          base_path = state$data_pond
        )

        if (
          is.null(record) ||
          nrow(as.data.frame(record)) == 0L
        ) {
          clear()
          return(
            invisible(FALSE)
          )
        }

        record_value <- function(
          field,
          default = ""
        ) {
          if (
            !field %in% names(record)
          ) {
            return(default)
          }

          safe_text(
            record[[field]],
            default = default
          )
        }

        rights_basis_code <-
          record_value(
            "rights_basis_code",
            default = if (
              nzchar(
                record_value(
                  "license_identifier"
                )
              )
            ) {
              "review_required"
            } else {
              ""
            }
          )

        access_level_code <-
          record_value(
            "access_level_code"
          )

        if (!nzchar(access_level_code)) {
          legacy_access <- tolower(
            record_value(
              "access_level"
            )
          )

          access_level_code <- if (
            grepl(
              "partner",
              legacy_access
            )
          ) {
            "approved_partners"
          } else if (
            grepl(
              "internal",
              legacy_access
            )
          ) {
            "internal"
          } else if (
            grepl(
              "public",
              legacy_access
            )
          ) {
            "public"
          } else {
            ""
          }
        }

        redistribution_code <-
          record_value(
            "redistribution_permission_code",
            default =
              licensing_template_redistribution(
                record_value(
                  "license_identifier"
                )
              )
          )

        sensitivity_code <-
          record_value(
            "sensitivity_handling_code",
            default = if (
              "sensitive_data" %in%
                names(record) &&
              !is.na(
                record$sensitive_data[1]
              ) &&
              as.integer(
                record$sensitive_data[1]
              ) == 1L
            ) {
              "review_required"
            } else {
              "none"
            }
          )

        holder_org_id <- if (
          "rights_holder_organisation_id" %in%
            names(record)
        ) {
          suppressWarnings(
            as.integer(
              record$rights_holder_organisation_id[1]
            )
          )
        } else {
          NA_integer_
        }

        owner <- primary_dataset_owner()

        saved_holder_is_dataset_owner <- (
          !is.na(holder_org_id) &&
            !is.null(owner) &&
            nrow(as.data.frame(owner)) == 1L &&
            !is.na(owner$id[1]) &&
            as.integer(holder_org_id) ==
              as.integer(owner$id[1])
        )

        holder_other <- if (
          is.na(holder_org_id)
        ) {
          record_value(
            "license_holder"
          )
        } else {
          ""
        }

        required_attribution <- if (
          "required_attribution" %in%
            names(record) &&
          !is.na(
            record$required_attribution[1]
          )
        ) {
          as.integer(
            record$required_attribution[1]
          ) == 1L
        } else {
          licensing_template_requires_attribution(
            record_value(
              "license_identifier"
            )
          )
        }

        attribution_text <-
          record_value(
            "attribution_text"
          )

        current_citation <-
          citation_value()

        use_citation <- (
          nzchar(attribution_text) &&
          nzchar(current_citation) &&
          identical(
            attribution_text,
            current_citation
          )
        )

        template_id <- if (
          "license_template_id" %in%
            names(record) &&
          !is.na(
            record$license_template_id[1]
          )
        ) {
          as.character(
            record$license_template_id[1]
          )
        } else {
          dt <- templates()

          row <- dt[
            dt$license_identifier ==
              record_value(
                "license_identifier"
              ),
            ,
            drop = FALSE
          ]

          if (nrow(row) > 0L) {
            as.character(
              row$id[1]
            )
          } else {
            NULL
          }
        }

        loading_record(TRUE)

        pending_template_id(
          template_id
        )

        shiny::updateSelectInput(
          session,
          "rights_basis_code",
          selected = rights_basis_code
        )

        shiny::updateCheckboxInput(
          session,
          "use_dataset_owner_as_rights_holder",
          value =
            saved_holder_is_dataset_owner
        )

        shiny::updateSelectizeInput(
          session,
          "rights_holder_organisation_id",
          selected = if (
            is.na(holder_org_id)
          ) {
            character()
          } else {
            as.character(
              holder_org_id
            )
          }
        )

        shiny::updateTextInput(
          session,
          "rights_holder_other",
          value = holder_other
        )

        shiny::updateSelectInput(
          session,
          "access_level_code",
          selected = access_level_code
        )

        shiny::updateSelectInput(
          session,
          "redistribution_permission_code",
          selected = redistribution_code
        )

        shiny::updateTextAreaInput(
          session,
          "permission_reference",
          value = record_value(
            "permission_reference"
          )
        )

        shiny::updateCheckboxInput(
          session,
          "required_attribution",
          value = required_attribution
        )

        shiny::updateCheckboxInput(
          session,
          "use_suggested_citation",
          value = use_citation
        )

        shiny::updateTextAreaInput(
          session,
          "attribution_text",
          value = attribution_text
        )

        shiny::updateSelectInput(
          session,
          "sensitivity_handling_code",
          selected = sensitivity_code
        )

        shiny::updateTextAreaInput(
          session,
          "sensitivity_reason",
          value = record_value(
            "sensitivity_reason"
          )
        )

        shiny::updateTextAreaInput(
          session,
          "use_constraints",
          value = record_value(
            "use_constraints"
          )
        )

        shiny::updateDateInput(
          session,
          "terms_review_date",
          value = safe_date(
            record_value(
              "terms_review_date"
            )
          )
        )

        shiny::updateTextAreaInput(
          session,
          "licence_notes",
          value = record_value(
            "licence_notes"
          )
        )

        session$onFlushed(
          function() {
            loading_record(FALSE)
          },
          once = TRUE
        )

        invisible(TRUE)
      }

      save <- function(
        dataset_id =
          state$current_dataset_id,
        strict = FALSE
      ) {
        if (
          is.null(dataset_id) ||
          length(dataset_id) == 0L ||
          is.na(dataset_id[1])
        ) {
          stop(
            "Select or save a dataset before saving the sharing assessment."
          )
        }

        if (!has_any_values()) {
          return(
            invisible(FALSE)
          )
        }

        check <- validate(
          strict = strict
        )

        if (!isTRUE(check$valid)) {
          stop(
            check$message
          )
        }

        user_person_id <-
          get_active_user_person_id(
            active_user_id =
              state$active_user_id,
            base_path =
              state$data_pond
          )

        save_dataset_license(
          dataset_id =
            as.integer(dataset_id[1]),
          template_row =
            selected_template(),
          data =
            values(),
          base_path =
            state$data_pond,
          user_person_id =
            user_person_id
        )

        invisible(TRUE)
      }

      shiny::observeEvent(
        input$save_licensing,
        {
          tryCatch(
            {
              result <- save(
                strict = FALSE
              )

              if (isTRUE(result)) {
                state$refresh_datasets <-
                  state$refresh_datasets + 1L

                shiny::showNotification(
                  "Sharing assessment saved.",
                  type = "message"
                )
              } else {
                shiny::showNotification(
                  "No sharing assessment values were entered.",
                  type = "warning"
                )
              }
            },
            error = function(e) {
              shiny::showNotification(
                conditionMessage(e),
                type = "error",
                duration = 8
              )
            }
          )
        }
      )

      list(
        values = values,
        validate = validate,
        save = save,
        load = load,
        clear = clear,
        has_any_values =
          has_any_values
      )
    }
  )
}
