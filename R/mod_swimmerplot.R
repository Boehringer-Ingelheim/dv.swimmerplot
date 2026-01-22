MODULE_IDS <- pack_of_constants( # nolint
  SWIMMERPLOT = "swimmerplot",
  GROUP_VARS = "group_vars",
  SORT_VARS = "sort_vars",
  SORT_DIRECTION = "sort_direction",
  FILTER = "filter",
  X_RNG = "x_rng",
  SWIMMER_PLOT = "swimmer_plot"
)

#' Swimmer Plot Module UI
#'
#' (For use outside of the DaVinci framework)\cr
#' Places the Swimmer Plot module UI at the call site of this function. A matching call to [swimmerplot_server()]
#' is necessary.\cr
#'
#' @param id `[character(1)]` Unique shiny ID. Must match the ID provided to [swimmerplot_server()].
#' @param group_by_vars `[character(n)]` Variables available for grouping subjects.
#' @param sort_by_vars `[character(n)]` Variables available for sorting subjects.
#' @param jumping_enabled `[logical(1)]` Whether clicking on a subject should enable navigation to detail view.
#'
#' @return Shiny UI.
#'
#' @seealso [mod_swimmerplot()] and [swimmerplot_server()]
#' 
#' @export
swimmerplot_UI <- function(id, group_by_vars = NULL, sort_by_vars = NULL, jumping_enabled = FALSE) { # nolint
  checkmate::assert_string(id)
  checkmate::assert_character(group_by_vars, null.ok = TRUE)
  checkmate::assert_character(sort_by_vars, null.ok = TRUE)
  checkmate::assert_logical(jumping_enabled, len = 1)
  
  ns <- shiny::NS(namespace = id)
  
  ui_elements <- list(
    shiny::tags$div(
      shiny::selectInput(
        inputId = ns(MODULE_IDS$GROUP_VARS),
        label = "Group subjects by:",
        choices = group_by_vars,
        multiple = TRUE
      )
    ),
    shiny::tags$div(
      shiny::selectInput(
        inputId = ns(MODULE_IDS$SORT_VARS),
        label = "Sort subjects by:",
        choices = sort_by_vars,
        selected = NULL,
        multiple = TRUE
      )
    ),
    shiny::tags$div(
      shiny::radioButtons(
        inputId = ns(MODULE_IDS$SORT_DIRECTION),
        label = "Sort direction:",
        choices = c("Ascending" = "asc", "Descending" = "desc"),
        selected = "asc",
        inline = TRUE
      )
    ),
    shiny::tags$div(
      shiny::selectInput(
        inputId = ns(MODULE_IDS$FILTER),
        label = "Select filter:",
        choices = NULL,
        multiple = TRUE
      )
    ),
    shiny::tags$div(
      shinyWidgets::numericRangeInput(
        inputId = ns(MODULE_IDS$X_RNG),
        label = "X Range:",
        value = c(NA, NA)
      )
    )
  )
  
  if (jumping_enabled) {
    ui_elements <- c(
      ui_elements,
      list(
        shiny::tags$div(
          style = "margin-bottom: 10px; color: #0072B2; font-style: italic;",
          shiny::icon("info-circle"), 
          "Click on a subject, treatment duration bar, 
          or response point to view detailed information in the linked module."
        )
      )
    )
  }
  
  drop_menu <- do.call(
    shinyWidgets::dropMenu,
    c(
      list(
        tag = shiny::actionButton(
          inputId = ns("plot_options"),
          label = "Plot Options",
          icon = shiny::icon("gear")
        )
      ),
      ui_elements
    )
  )

  scroll_id <- ns("swimmer_scroll")
  legend_wrap_id <- ns("legend_wrap")

  legend_menu <- shinyWidgets::dropMenu(
    tag = shiny::actionButton(
      inputId = ns("legend_toggle"),
      label = "Legend",
      icon = shiny::icon("list")
    ),
    shiny::tags$div(
      style = "padding: 6px 10px; max-width: 260px;",
      shiny::uiOutput(outputId = ns("plot_legend"))
    )
  )

  legend_script <- shiny::tags$script(
    shiny::HTML(
      paste0(
        "document.addEventListener('DOMContentLoaded', function(){",
        "function bindLegend(){",
        "var el=document.getElementById('", scroll_id, "');",
        "var legend=document.getElementById('", legend_wrap_id, "');",
        "if(!el||!legend){return;}",
        "if(el.dataset.legendBound){return;}",
        "el.dataset.legendBound='1';",
        "var toggle=function(){legend.style.display=el.scrollTop>0?'block':'none';};",
        "el.addEventListener('scroll', toggle);",
        "toggle();",
        "}",
        "bindLegend();",
        "setInterval(bindLegend, 1000);",
        "});"
      )
    )
  )

  shiny::tags$div(
    id = scroll_id,
    style = "height: 100%; overflow-y: auto;",
    shiny::tags$div(
      style = "position: sticky; top: 0; z-index: 1000; background: #fff;",
      drop_menu,
      shiny::tags$div(
        id = legend_wrap_id,
        style = "display: none;",
        legend_menu
      )
    ),
    ggiraph::girafeOutput(
      outputId = ns(MODULE_IDS$SWIMMER_PLOT),
      width = "100%",
      height = NULL
    ),
    legend_script
  )
}

#' Swimmer Plot Module server
#'
#' (For use outside of the DaVinci framework)\cr
#' Runs the server that populates the UI returned by [swimmerplot_UI()].\cr
#' Requires a matching call to that function.
#'
#' @inheritParams swimmerplot_UI
#' @inheritParams mod_swimmerplot
#' @param subject_level_dataset `[reactive(data.frame)]` Subject-level dataset containing baseline information.
#' @param exposure_dataset `[reactive(data.frame)]` Dataset containing treatment exposure records.
#' @param response_dataset `[reactive(data.frame)]` Dataset containing response/event records. Optional - 
#' can be a reactive that returns NULL to create a plot without response data.
#' @param afmm `[list]` Arguments from Module Manager containing datasets, utilities and module communication channels.
#' @param filter_var `[character(1)]` Name of the variable to use for filtering subjects.
#' @param filter_default_vals `[character(n)]` Default selected values for the filter variable upon initialization.
#'
#' @seealso [mod_swimmerplot()] and [swimmerplot_UI()]
#' 
#' @export
swimmerplot_server <- function(
  id,
  subject_level_dataset,
  exposure_dataset,
  response_dataset,
  subjid_var,
  trt_start_day_var,
  trt_end_day_var,
  trt_ongoing_var,
  trt_tooltip_vars = NULL,
  trt_group_var = NULL,
  trt_legend_label = "Treatment",
  color_palette = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  trt_annotation_vars = NULL,
  trt_annotation_x = NULL,
  result_annotation_vars = NULL,
  result_cat_var = NULL,
  result_legend_label = "Response",
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = "Study Day",
  plot_y_label = "Subject ID",
  plot_width = NULL,
  plot_height = NULL,
  sort_by_vars = NULL,
  sort_direction = "asc",
  receiver_id = NULL,
  afmm = NULL,
  filter_dataset = NULL,
  filter_on_exposure = FALSE,
  filter_on_response = FALSE,
  filter_var = "ARM",
  filter_values = NULL,
  filter_default_vals = NULL
) {
  checkmate::assert_string(id)
  checkmate::assert_class(subject_level_dataset, "reactive")
  checkmate::assert_class(exposure_dataset, "reactive")
  checkmate::assert_class(response_dataset, "reactive")
  
  checkmate::assert_string(subjid_var)
  checkmate::assert_string(trt_start_day_var)
  checkmate::assert_string(trt_end_day_var)
  checkmate::assert_string(trt_ongoing_var, null.ok = TRUE)
  checkmate::assert_character(trt_tooltip_vars, null.ok = TRUE)
  checkmate::assert_string(trt_group_var, null.ok = TRUE)
  checkmate::assert_string(trt_legend_label)
  checkmate::assert(
    checkmate::check_character(color_palette, names = "named", null.ok = TRUE),
    checkmate::check_character(color_palette, min.len = 0, null.ok = TRUE)
  )
  
  checkmate::assert_string(result_study_day_var, null.ok = TRUE)
  checkmate::assert_character(result_tooltip_vars, null.ok = TRUE)
  checkmate::assert_string(result_cat_var, null.ok = TRUE)
  checkmate::assert_string(result_legend_label, null.ok = TRUE)
  checkmate::assert_numeric(shape_mapping, null.ok = TRUE)
  
  checkmate::assert_string(plot_title, null.ok = TRUE)
  checkmate::assert_string(plot_subtitle, null.ok = TRUE)
  checkmate::assert_string(plot_x_label)
  checkmate::assert_string(plot_y_label)
  checkmate::assert_number(plot_width, lower = 0, null.ok = TRUE)
  checkmate::assert_number(plot_height, lower = 0, null.ok = TRUE)
  
  checkmate::assert_character(sort_by_vars, null.ok = TRUE)
  checkmate::assert_choice(sort_direction, c("asc", "desc"))
  
  checkmate::assert_string(receiver_id, null.ok = TRUE)
  checkmate::assert_class(filter_dataset, "reactive", null.ok = TRUE)
  checkmate::assert_flag(filter_on_exposure)
  checkmate::assert_flag(filter_on_response)
  checkmate::assert_character(filter_values, min.chars = 1, any.missing = FALSE, unique = TRUE, min.len = 1, null.ok = TRUE)
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      selected_subject <- shiny::reactiveVal(NULL)
      filter_initialized <- shiny::reactiveVal(FALSE)

      bookmark_key <- paste0("dv.swimmerplot.", id, ".", MODULE_IDS$FILTER)
      restored_filter_sel <- shiny::reactiveVal(NULL)
      restore_nonce <- shiny::reactiveVal(0)
      restore_applied_nonce <- shiny::reactiveVal(0)

      filter_source <- shiny::reactive({
        if (!is.null(filter_dataset)) {
          filter_dataset()
        } else {
          subject_level_dataset()
        }
      })

      shiny::onBookmark(function(state) {
        state$values[[bookmark_key]] <- list(value = input[[MODULE_IDS$FILTER]])
      })

      shiny::onRestore(function(state) {
        if (!is.null(state$values[[bookmark_key]])) {
          restored_filter_sel(state$values[[bookmark_key]]$value)
        } else {
          restored_filter_sel(state$input[[MODULE_IDS$FILTER]])
        }
        restore_nonce(shiny::isolate(restore_nonce() + 1))
      })

      shiny::observeEvent(list(filter_source(), restore_nonce()), {
        data <- filter_source()
        if (is.null(filter_var) || is.null(data) || !(filter_var %in% names(data))) {
          return()
        }

        filter_vals <- unique(stats::na.omit(data[[filter_var]]))

        is_restore <- restore_nonce() > restore_applied_nonce()
        selected <- if (is_restore) {
          sel <- restored_filter_sel()
          if (is.null(sel) || length(sel) == 0) {
            character(0)
          } else {
            intersect(sel, filter_vals)
          }
        } else if (!is.null(filter_values)) {
          intersect(filter_values, filter_vals)
        } else {
          if (is.null(filter_default_vals)) filter_vals else intersect(filter_default_vals, filter_vals)
        }

        shiny::updateSelectInput(
          session = session,
          inputId = MODULE_IDS$FILTER,
          label = if (is.null(filter_var)) "Select filter:" else paste0("Select filter (", filter_var, "):"),
          choices = filter_vals,
          selected = selected
        )

        if (is_restore) {
          restore_applied_nonce(restore_nonce())
        }

        filter_initialized(TRUE)
      })

      filtered_subject_level <- shiny::reactive({
        data <- subject_level_dataset()
        source_data <- filter_source()
        if (is.null(filter_var) || is.null(source_data) || !(filter_var %in% names(source_data))) {
          return(data)
        }

        if (!(subjid_var %in% names(source_data))) {
          return(data)
        }

        sel_vals <- input[[MODULE_IDS$FILTER]]

        if (!isTRUE(filter_initialized())) {
          return(data)
        }

        if (is.null(sel_vals) || length(sel_vals) == 0) {
          return(data[0, , drop = FALSE])
        }

        filtered_source <- source_data[source_data[[filter_var]] %in% sel_vals, , drop = FALSE]
        subj_ids <- unique(filtered_source[[subjid_var]])

        if (length(subj_ids) == 0) {
          return(data[0, , drop = FALSE])
        }

        data[data[[subjid_var]] %in% subj_ids, , drop = FALSE]
      })
      
      filtered_subj_ids <- shiny::reactive({
        subj_data <- filtered_subject_level()
        if (is.null(subj_data) || nrow(subj_data) == 0) {
          return(character())
        }
        subj_data[[subjid_var]]
      })

      filtered_exposure <- shiny::reactive({
        ex <- exposure_dataset()
        subj_ids <- filtered_subj_ids()
        if (length(subj_ids) == 0) {
          return(ex[0, , drop = FALSE])
        }
        ex_filtered <- ex[ex[[subjid_var]] %in% subj_ids, , drop = FALSE]

        if (!isTRUE(filter_on_exposure)) {
          return(ex_filtered)
        }

        if (is.null(filter_var) || !(filter_var %in% names(ex_filtered))) {
          return(ex_filtered)
        }

        if (!isTRUE(filter_initialized())) {
          return(ex_filtered)
        }

        sel_vals <- input[[MODULE_IDS$FILTER]]
        if (is.null(sel_vals) || length(sel_vals) == 0) {
          return(ex_filtered[0, , drop = FALSE])
        }

        ex_filtered[ex_filtered[[filter_var]] %in% sel_vals, , drop = FALSE]
      })

      filtered_response <- shiny::reactive({
        rs <- response_dataset()
        if (is.null(rs)) {
          return(NULL)
        }
        subj_ids <- filtered_subj_ids()
        if (length(subj_ids) == 0) {
          return(rs[0, , drop = FALSE])
        }
        rs_filtered <- rs[rs[[subjid_var]] %in% subj_ids, , drop = FALSE]

        if (!isTRUE(filter_on_response)) {
          return(rs_filtered)
        }

        if (is.null(filter_var) || !(filter_var %in% names(rs_filtered))) {
          return(rs_filtered)
        }

        if (!isTRUE(filter_initialized())) {
          return(rs_filtered)
        }

        sel_vals <- input[[MODULE_IDS$FILTER]]
        if (is.null(sel_vals) || length(sel_vals) == 0) {
          return(rs_filtered[0, , drop = FALSE])
        }

        rs_filtered[rs_filtered[[filter_var]] %in% sel_vals, , drop = FALSE]
      })
      
      output[[MODULE_IDS$SWIMMER_PLOT]] <- ggiraph::renderGirafe({
        subj_data <- filtered_subject_level()
        n_subjects <- nrow(subj_data)

        shiny::validate(
          shiny::need(
            n_subjects > 0, 
            "No data available for swimmer plot. Please check your filters or data source."
          )
        )

        if (is.null(plot_height)) {       
          plot_height <- max((n_subjects * 0.3) + 3, 6)
        }
        
        selected_sort_vars <- input[[MODULE_IDS$SORT_VARS]]
        
        if (length(selected_sort_vars) == 0) {
          selected_sort_vars <- NULL
        }
        
        response_data <- filtered_response()

        x_rng <- input[[MODULE_IDS$X_RNG]]
        x_rng_lower <- NULL
        x_rng_upper <- NULL
        if (!is.null(x_rng) && length(x_rng) == 2) {
          if (!is.na(x_rng[[1]])) {
            x_rng_lower <- x_rng[[1]]
          }
          if (!is.na(x_rng[[2]])) {
            x_rng_upper <- x_rng[[2]]
          }
        }

        swimmerplot(
          subject_level_dataset = subj_data,
          exposure_dataset = filtered_exposure(),
          response_dataset = response_data,
          subjid_var = subjid_var,
          group_by_vars = input[[MODULE_IDS$GROUP_VARS]],
          sort_by_vars = selected_sort_vars,
          sort_direction = input[[MODULE_IDS$SORT_DIRECTION]],
          trt_start_day_var = trt_start_day_var,
          trt_end_day_var = trt_end_day_var,
          trt_ongoing_var = trt_ongoing_var,
          trt_tooltip_vars = trt_tooltip_vars,
          trt_group_var = trt_group_var,
          trt_legend_label = trt_legend_label,
          color_palette = color_palette,
          result_study_day_var = result_study_day_var,
          result_tooltip_vars = result_tooltip_vars,
          trt_annotation_vars = trt_annotation_vars,
          trt_annotation_x = trt_annotation_x,
          result_annotation_vars = result_annotation_vars,
          result_cat_var = result_cat_var,
          result_legend_label = result_legend_label,
          shape_mapping = shape_mapping,
          plot_title = plot_title,
          plot_subtitle = plot_subtitle,
          plot_x_label = plot_x_label,
          plot_y_label = plot_y_label,
          plot_width = plot_width,
          plot_height = plot_height,
          x_rng_lower = x_rng_lower,
          x_rng_upper = x_rng_upper,
          enable_jumping = !is.null(receiver_id)
        )      
      })

      output[["plot_legend"]] <- shiny::renderUI({
        ex <- filtered_exposure()
        rs <- filtered_response()

        trt_vals <- character()
        if (!is.null(trt_group_var) && !is.null(ex) && nrow(ex) > 0 && trt_group_var %in% names(ex)) {
          trt_raw <- stats::na.omit(ex[[trt_group_var]])
          trt_vals <- sort(unique(as.character(trt_raw)))
        }

        rs_vals <- character()
        if (!is.null(result_cat_var) && !is.null(rs) && nrow(rs) > 0 && result_cat_var %in% names(rs)) {
          rs_raw <- stats::na.omit(rs[[result_cat_var]])
          rs_vals <- sort(unique(as.character(rs_raw)))
        }

        if (!is.null(color_palette) && !is.null(names(color_palette))) {
          trt_vals <- trt_vals[trt_vals %in% names(color_palette)]
        }

        if (!is.null(shape_mapping) && !is.null(names(shape_mapping))) {
          rs_vals <- rs_vals[rs_vals %in% names(shape_mapping)]
        }

        if (length(trt_vals) == 0 && length(rs_vals) == 0) {
          return(NULL)
        }

        shape_symbol <- function(val) {
          key <- as.character(val)
          if (key == "16") return("●")
          if (key == "17") return("▲")
          if (key == "15") return("■")
          if (key == "18") return("◆")
          if (key == "3") return("+")
          if (key == "4") return("×")
          if (key == "1") return("○")
          if (key == "2") return("△")
          if (key == "0") return("□")
          "●"
        }

        hue_pal <- function(n) {
          if (n == 0) return(character())
          h <- seq(15, 375, length.out = n + 1)
          grDevices::hcl(h[-(n + 1)], l = 65, c = 100)
        }

        shape_pal <- function(n) {
          vals <- c(16, 17, 15, 3, 7, 8)
          rep(vals, length.out = n)
        }

        make_item <- function(label, symbol, color) {
          shiny::tags$div(
            style = "display:flex;align-items:center;gap:8px;line-height:1.4;",
            shiny::tags$span(
              style = paste0(
                "color:", color, ";font-weight:700;",
                "display:inline-block;width:14px;text-align:center;font-size:14px;"
              ),
              symbol
            ),
            shiny::tags$span(style = "font-size:13px;", label)
          )
        }

        trt_cols <- character()
        if (length(trt_vals) > 0) {
          if (!is.null(color_palette) && !is.null(names(color_palette))) {
            trt_cols <- unname(color_palette[trt_vals])
          } else {
            trt_cols <- hue_pal(length(trt_vals))
          }
        }

        rs_shapes <- character()
        if (length(rs_vals) > 0) {
          if (!is.null(shape_mapping) && !is.null(names(shape_mapping))) {
            rs_shapes <- as.character(shape_mapping[rs_vals])
          } else {
            rs_shapes <- as.character(shape_pal(length(rs_vals)))
          }
        }

        trt_items <- lapply(seq_along(trt_vals), function(i) {
          make_item(as.character(trt_vals[[i]]), "■", trt_cols[[i]])
        })

        rs_items <- lapply(seq_along(rs_vals), function(i) {
          make_item(as.character(rs_vals[[i]]), shape_symbol(rs_shapes[[i]]), "#333333")
        })

        shiny::tags$div(
          if (length(trt_items) > 0) shiny::tags$div(
            shiny::tags$div(style = "font-weight:600;margin:4px 0;", trt_legend_label),
            trt_items
          ),
          if (length(rs_items) > 0) shiny::tags$div(
            style = "margin-top:8px;",
            shiny::tags$div(style = "font-weight:600;margin:4px 0;", result_legend_label),
            rs_items
          )
        )
      })
      
      shiny::observeEvent(input$swimmer_plot_selected, {
        if (!is.null(input$swimmer_plot_selected) && length(input$swimmer_plot_selected) > 0) {
          selected_subject(input$swimmer_plot_selected)
          
          if (!is.null(receiver_id) && !is.null(afmm)) {
            afmm[["utils"]][["switch2mod"]](receiver_id)
          }
        }
      })
      
      list(subj_id = shiny::reactive(selected_subject()))
    }
  )
}

#' Swimmer Plot Module
#'
#' @description
#'
#' `mod_swimmerplot` is a DaVinci Shiny module that displays subject-level treatment exposures and outcomes over time.
#' It consists of an interactive visualization with the following features:
#' \itemize{
#'   \item Each subject is represented as a horizontal "swim lane"
#'   \item Treatment exposures are shown as horizontal bars along the timeline
#'   \item Clinical outcomes or responses are shown as points along the timeline (optional)
#'   \item Subjects can be grouped by categorical variables
#'   \item Interactive tooltips provide additional information on hover
#'   \item Optional linking to other modules (e.g., Patient Profile) when clicking on subjects, 
#' treatment duration bars, or response points
#' }
#'
#' @param module_id `[character(1)]` Unique ID for the module.
#' @param subject_level_dataset_name `[character(1)]` Name of the dataset containing subject-level dataset.
#' @param exposure_dataset_name `[character(1)]` Name of the dataset containing exposure data.
#' @param response_dataset_name `[character(1)]` Name of the dataset containing response data. Set to NULL to 
#' create a plot without response data.
#' @param subjid_var `[character(1)]` Name of the variable representing subject ID.
#' @param group_by_vars `[character(n)]` Name of the variables to group subjects by in the plot.
#' @param sort_by_vars `[character(n)]` Name of the variables to sort by in the plot.
#' @param sort_direction `[character(1)]` Direction to sort ("asc" for ascending, "desc" for descending).
#' @param trt_start_day_var `[character(1)]` Name of the variable representing the start of exposure.
#' @param trt_end_day_var `[character(1)]` Name of the variable representing the end of exposure.
#' @param trt_ongoing_var `[character(1)]` Name of the variable indicating ongoing exposure.
#' @param trt_tooltip_vars `[character(n)]` Character vector of variables for tooltip information on exposure.
#'   Optionally named to add labels in tooltips (e.g., c("Dose: " = "EXDOSE")).
#' @param trt_group_var `[character(1)]` Name of the variable representing the type of exposure.
#' @param trt_legend_label `[character(1)]` Legend title for the exposure type.
#' @param color_palette `[character(n)]` Named character vector mapping exposure types to colors 
#' (e.g., c("Drug A" = "#FF0000", "Drug B" = "#00FF00")).
#' @param result_study_day_var `[character(1)]` Name of the variable representing the response time. 
#' Only used when response_dataset_name is provided.
#' @param result_tooltip_vars `[character(n)]` Character vector of variables for tooltip information on response.
#'   Optionally named to add labels in tooltips (e.g., c("Response: " = "RSORRES")). 
#'   Only used when response_dataset_name is provided.
#' @param trt_annotation_vars `[character(n)]` Character vector of variables for fixed text annotation on exposure.
#' @param trt_annotation_x `[numeric(1)]` X-axis position for a left-aligned fixed exposure annotation column.
#'   If NULL, the exposure annotation is placed after each exposure bar end.
#' @param result_annotation_vars `[character(n)]` Character vector of variables for fixed text annotation on response points.
#' @param result_cat_var `[character(1)]` Name of the variable representing the type of response. 
#' Only used when response_dataset_name is provided.
#' @param result_legend_label `[character(1)]` Legend title for the response type. 
#' Only used when response_dataset_name is provided.
#' @param shape_mapping `[numeric(n)]` Named numeric vector mapping response types to point shapes 
#' (e.g., c("CR" = 16, "PR" = 17)). Only used when response_dataset_name is provided.
#' @param plot_title `[character(1)]` Title of the plot.
#' @param plot_subtitle `[character(1)]` Subtitle of the plot.
#' @param plot_x_label `[character(1)]` Label for the x-axis.
#' @param plot_y_label `[character(1)]` Label for the y-axis.
#' @param plot_width `[numeric(1)]` Width of the plot in inches.
#' @param plot_height `[numeric(1)]` Height of the plot in inches. 
#' If NULL, height will be calculated automatically based on the number of subjects.
#' @param receiver_id `[character(1)]` ID of the module to communicate with (e.g., Patient Profile module ID). 
#' Set to NULL to disable the communication functionality.
#' @param filter_var `[character(1)]` Name of the variable to use for filtering subjects.
#' @param filter_default_vals `[character(n)]` Default selected values for the filter variable upon initialization.
#'
#' @return A list composed of the following elements:
#' \itemize{
#'   \item{`ui`}: Shiny UI function.
#'   \item{`server`}: Shiny server function.
#'   \item{`module_id`}: Shiny unique identifier.
#' }
#'
#' @seealso [swimmerplot_UI()], [swimmerplot_server()]
#' 
#' @export
mod_swimmerplot <- function(
  module_id,
  subject_level_dataset_name = "dm",
  exposure_dataset_name = "ex",
  response_dataset_name = NULL,
  subjid_var = "USUBJID",
  group_by_vars = c("SEX", "RACE"),
  sort_by_vars = NULL,
  sort_direction = "asc",
  trt_start_day_var = "EXSTDY",
  trt_end_day_var = "EXENDY",
  trt_ongoing_var = NULL,
  trt_tooltip_vars = NULL,
  trt_group_var = "EXTRT",
  trt_legend_label = NULL,
  color_palette = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  trt_annotation_vars = NULL,
  trt_annotation_x = NULL,
  result_annotation_vars = NULL,
  result_cat_var = NULL,
  result_legend_label = NULL,
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = NULL,
  plot_y_label = NULL,
  plot_width = NULL,
  plot_height = NULL,
  receiver_id = NULL,
  filter_data = subject_level_dataset_name,
  filter_var = "ARM",
  filter_values = NULL,
  filter_default_vals = NULL
) {
  checkmate::assert_string(module_id)
  checkmate::assert_string(subject_level_dataset_name)
  checkmate::assert_string(exposure_dataset_name)
  checkmate::assert_string(response_dataset_name, null.ok = TRUE)
  checkmate::assert_string(subjid_var)
  checkmate::assert_character(group_by_vars, null.ok = TRUE)
  checkmate::assert_character(sort_by_vars, null.ok = TRUE)
  checkmate::assert_choice(sort_direction, choices = c("asc", "desc"))
  checkmate::assert_string(trt_start_day_var)
  checkmate::assert_string(trt_end_day_var)
  checkmate::assert_string(trt_ongoing_var, null.ok = TRUE)
  checkmate::assert_character(trt_tooltip_vars, null.ok = TRUE)
  checkmate::assert_string(trt_group_var)
  checkmate::assert_string(trt_legend_label, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_character(color_palette, names = "named", null.ok = TRUE),
    checkmate::check_character(color_palette, min.len = 0, null.ok = TRUE)
  )
  checkmate::assert_string(result_study_day_var, null.ok = TRUE)
  checkmate::assert_character(result_tooltip_vars, null.ok = TRUE)
  checkmate::assert_character(trt_annotation_vars, null.ok = TRUE)
  checkmate::assert_number(trt_annotation_x, null.ok = TRUE)
  checkmate::assert_character(result_annotation_vars, null.ok = TRUE)
  checkmate::assert_string(result_cat_var, null.ok = TRUE)
  checkmate::assert_string(result_legend_label, null.ok = TRUE)
  checkmate::assert(
    checkmate::check_numeric(shape_mapping, names = "named", null.ok = TRUE),
    checkmate::check_numeric(shape_mapping, min.len = 0, null.ok = TRUE)
  )
  checkmate::assert_string(plot_title, null.ok = TRUE)
  checkmate::assert_string(plot_subtitle, null.ok = TRUE)
  checkmate::assert_string(plot_x_label, null.ok = TRUE)
  checkmate::assert_string(plot_y_label, null.ok = TRUE)
  checkmate::assert_number(plot_width, lower = 0, null.ok = TRUE)
  checkmate::assert_number(plot_height, lower = 0, null.ok = TRUE)
  checkmate::assert_string(receiver_id, null.ok = TRUE)
  checkmate::assert_string(filter_data, null.ok = TRUE)
  checkmate::assert_string(filter_var, null.ok = TRUE)
  checkmate::assert_character(
    filter_values,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1,
    null.ok = TRUE
  )
  checkmate::assert_character(
    filter_default_vals,
    min.chars = 1,
    any.missing = FALSE,
    unique = TRUE,
    min.len = 1,
    null.ok = TRUE
  )
  
  mod <- list(
    ui = function(mod_id) {
      swimmerplot_UI(
        id = mod_id, 
        group_by_vars = group_by_vars, 
        sort_by_vars = sort_by_vars,
        jumping_enabled = !is.null(receiver_id)
      )
    },
    
    server = function(afmm) {
      subject_level_dataset <- shiny::reactive(
        afmm[["filtered_dataset"]]()[[subject_level_dataset_name]]
      )
      exposure_dataset <- shiny::reactive(
        afmm[["filtered_dataset"]]()[[exposure_dataset_name]]
      )
      
      response_dataset <- if (is.null(response_dataset_name)) {
        shiny::reactive(NULL)
      } else {
        shiny::reactive(
          afmm[["filtered_dataset"]]()[[response_dataset_name]]
        )
      }

      filter_dataset <- shiny::reactive({
        data <- afmm[["filtered_dataset"]]()
        if (is.null(filter_data) || is.null(data) || !(filter_data %in% names(data))) {
          return(NULL)
        }
        data[[filter_data]]
      })

      filter_on_exposure <- !is.null(filter_data) && identical(filter_data, exposure_dataset_name)
      filter_on_response <- !is.null(response_dataset_name) && !is.null(filter_data) && 
        identical(filter_data, response_dataset_name)
      
      swimmerplot_server(
        id = module_id,
        subject_level_dataset = subject_level_dataset,
        exposure_dataset = exposure_dataset,
        response_dataset = response_dataset,      
        subjid_var = subjid_var,
        trt_start_day_var = trt_start_day_var,
        trt_end_day_var = trt_end_day_var,
        trt_ongoing_var = trt_ongoing_var,
        trt_tooltip_vars = trt_tooltip_vars,
        trt_group_var = trt_group_var,
        trt_legend_label = trt_legend_label,
        color_palette = color_palette,
        result_study_day_var = result_study_day_var,
        result_tooltip_vars = result_tooltip_vars,
        trt_annotation_vars = trt_annotation_vars,
        trt_annotation_x = trt_annotation_x,
        result_annotation_vars = result_annotation_vars,
        result_cat_var = result_cat_var,
        result_legend_label = result_legend_label,
        shape_mapping = shape_mapping,
        plot_title = plot_title,
        plot_subtitle = plot_subtitle,
        plot_x_label = plot_x_label,
        plot_y_label = plot_y_label,
        plot_width = plot_width,
        plot_height = plot_height,
        sort_by_vars = sort_by_vars,
        sort_direction = sort_direction,
        receiver_id = receiver_id,
        afmm = afmm,
        filter_dataset = filter_dataset,
        filter_on_exposure = filter_on_exposure,
        filter_on_response = filter_on_response,
        filter_var = filter_var,
        filter_values = filter_values,
        filter_default_vals = filter_default_vals
      )
    },
    
    module_id = module_id
  )
  
  mod
}
