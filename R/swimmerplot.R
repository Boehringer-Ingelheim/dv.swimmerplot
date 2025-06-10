#' Create an interactive swimmer plot
#'
#' @description
#' Generates an interactive swimmer plot showing subject-level treatment exposure periods and response events.
#' Response data is optional - when not provided, the plot will only show subject-level and exposure data.
#'
#' @param subject_level_dataset `[data.frame]` Dataset containing subject-level data.
#' @param subjid_var `[character(1)]` Name of the variable representing subject ID.
#' @param group_by_vars `[character(n)]` Name of the variables to group subjects by in the plot.
#' @param sort_by_vars `[character(n)]` Name of the variables to sort by in the plot.
#' @param sort_direction `[character(1)]` Direction to sort ("asc" for ascending, "desc" for descending).
#' @param exposure_dataset `[data.frame]` Dataset containing treatment exposure data.
#' @param trt_start_day_var `[character(1)]` Name of the variable representing the start of exposure.
#' @param trt_end_day_var `[character(1)]` Name of the variable representing the end of exposure.
#' @param trt_ongoing_var `[character(1)]` Name of the variable indicating ongoing exposure.
#' @param trt_tooltip_vars `[character(n)]` Character vector of variables for tooltip information on exposure. 
#'   Optionally named to add labels in tooltips (e.g., c("Dose: " = "EXDOSE")).
#' @param trt_group_var `[character(1)]` Name of the variable representing the type of exposure.
#' @param trt_legend_label `[character(1)]` Legend title for the exposure type.
#' @param color_palette `[character(n)]` Named character vector mapping exposure types to colors 
#' (e.g., c("Drug A" = "#FF0000", "Drug B" = "#00FF00")).
#' @param response_dataset `[data.frame]` Dataset containing response/event data. 
#' Optional - set to NULL to create a plot without response data.
#' @param result_study_day_var `[character(1)]` Name of the variable representing the response time. 
#' Only used when response_dataset is provided.
#' @param result_tooltip_vars `[character(n)]` Character vector of variables for tooltip information on response.
#'   Optionally named to add labels in tooltips (e.g., c("Response: " = "RSORRES")). 
#'   Only used when response_dataset is provided.
#' @param result_cat_var `[character(1)]` Name of the variable representing the type of response. 
#' Only used when response_dataset is provided.
#' @param result_legend_label `[character(1)]` Legend title for the response type. 
#' Only used when response_dataset is provided.
#' @param shape_mapping `[numeric(n)]` Named numeric vector mapping response types to point shapes 
#' (e.g., c("CR" = 16, "PR" = 17)). Only used when response_dataset is provided.
#' @param plot_title `[character(1)]` Title of the plot.
#' @param plot_subtitle `[character(1)]` Subtitle of the plot.
#' @param plot_x_label `[character(1)]` Label for the x-axis.
#' @param plot_y_label `[character(1)]` Label for the y-axis.
#' @param plot_width `[numeric(1)]` Width of the graphics region in inches. 
#' @param plot_height `[numeric(1)]` Height of the graphics region in inches.
#' @param enable_jumping `[logical(1)]` Whether to enable jumping behavior on hover.
#' @param interactive_plot `[logical(1)]` Whether to return an interactive girafe object (TRUE) or 
#' a ggplot2 object (FALSE).
#'
#' @return A ggiraph interactive plot object if interactive_plot=TRUE, otherwise a ggplot2 object
#'
#' @importFrom ggplot2 .data
#' 
#' @keywords internal
swimmerplot <- function(
  subject_level_dataset,
  subjid_var,
  group_by_vars = NULL,
  sort_by_vars = NULL,
  sort_direction = "asc",
  exposure_dataset,
  trt_start_day_var,
  trt_end_day_var,
  trt_ongoing_var,
  trt_tooltip_vars = NULL,
  trt_group_var = NULL,
  trt_legend_label = "Treatment",
  color_palette = NULL,
  response_dataset = NULL,
  result_study_day_var = NULL,
  result_tooltip_vars = NULL,
  result_cat_var = NULL,
  result_legend_label = "Response",
  shape_mapping = NULL,
  plot_title = NULL,
  plot_subtitle = NULL,
  plot_x_label = "Study Day",
  plot_y_label = "Subject ID",
  plot_width = NULL,
  plot_height = NULL,
  enable_jumping = FALSE,
  interactive_plot = TRUE
) {
  checkmate::assert_data_frame(subject_level_dataset)
  checkmate::assert_data_frame(exposure_dataset)
  checkmate::assert_data_frame(response_dataset, null.ok = TRUE)
  
  checkmate::assert_character(subjid_var, len = 1)
  checkmate::assert_character(group_by_vars, null.ok = TRUE)
  checkmate::assert_character(sort_by_vars, null.ok = TRUE)
  checkmate::assert_choice(sort_direction, c("asc", "desc"))
  
  checkmate::assert_character(trt_start_day_var, len = 1)
  checkmate::assert_character(trt_end_day_var, len = 1)
  checkmate::assert_character(trt_ongoing_var, len = 1)
  checkmate::assert_character(trt_tooltip_vars, null.ok = TRUE)
  checkmate::assert_character(trt_group_var, len = 1, null.ok = TRUE)
  checkmate::assert_character(trt_legend_label, len = 1)
  checkmate::assert_character(color_palette, null.ok = TRUE)
  
  has_response_data <- !is.null(response_dataset) && nrow(response_dataset) > 0
  
  if (has_response_data) {
    checkmate::assert_character(result_study_day_var, len = 1)
    checkmate::assert_character(result_tooltip_vars, null.ok = TRUE)
    checkmate::assert_character(result_cat_var, len = 1, null.ok = TRUE)
    checkmate::assert_character(result_legend_label, len = 1)
    checkmate::assert_numeric(shape_mapping, null.ok = TRUE)
    
    checkmate::assert_subset(c(subjid_var, result_study_day_var), colnames(response_dataset))
    if (!is.null(result_cat_var)) {
      checkmate::assert_subset(result_cat_var, colnames(response_dataset))
    }
    if (!is.null(result_tooltip_vars)) {
      checkmate::assert_subset(result_tooltip_vars, colnames(response_dataset))
    }
    
    if (!is.null(result_tooltip_vars)) {
      if (!is.null(names(result_tooltip_vars))) {
        checkmate::assert_names(names(result_tooltip_vars))
      }
    }
  }
  
  checkmate::assert_character(plot_title, len = 1, null.ok = TRUE)
  checkmate::assert_character(plot_subtitle, len = 1, null.ok = TRUE)
  checkmate::assert_character(plot_x_label, len = 1)
  checkmate::assert_character(plot_y_label, len = 1)
  checkmate::assert_numeric(plot_width, len = 1)
  checkmate::assert_numeric(plot_height, len = 1)
  checkmate::assert_logical(enable_jumping, len = 1)
  checkmate::assert_logical(interactive_plot, len = 1)
  
  checkmate::assert_subset(subjid_var, colnames(subject_level_dataset))
  if (!is.null(group_by_vars)) {
    checkmate::assert_subset(group_by_vars, colnames(subject_level_dataset))
  }
  if (!is.null(sort_by_vars)) {
    checkmate::assert_subset(sort_by_vars, colnames(subject_level_dataset))
  }
  
  checkmate::assert_subset(
    c(subjid_var, trt_start_day_var, trt_end_day_var, trt_ongoing_var), 
    colnames(exposure_dataset)
  )
  if (!is.null(trt_group_var)) {
    checkmate::assert_subset(trt_group_var, colnames(exposure_dataset))
  }
  if (!is.null(trt_tooltip_vars)) {
    checkmate::assert_subset(trt_tooltip_vars, colnames(exposure_dataset))
  }
  
  if (!is.null(trt_tooltip_vars)) {
    if (!is.null(names(trt_tooltip_vars))) {
      checkmate::assert_names(names(trt_tooltip_vars))
    }
  }
  
  if (!is.null(trt_group_var) && trt_group_var %in% colnames(exposure_dataset)) {
    exposure_dataset[[trt_group_var]] <- as.character(exposure_dataset[[trt_group_var]])
  }
  
  if (has_response_data && !is.null(result_cat_var) && result_cat_var %in% colnames(response_dataset)) {
    response_dataset[[result_cat_var]] <- as.character(response_dataset[[result_cat_var]])
  }
  
  if (!is.null(sort_by_vars)) {
    decreasing <- sort_direction == "desc"
    
    if (length(sort_by_vars) == 1) {
      sorted_data <- subject_level_dataset[order(subject_level_dataset[[sort_by_vars]], decreasing = decreasing), ]
    } else {
      sort_vars <- lapply(sort_by_vars, function(var) {
        if (decreasing) {
          -xtfrm(subject_level_dataset[[var]])
        } else {
          xtfrm(subject_level_dataset[[var]])
        }
      })
      
      sorted_data <- subject_level_dataset[do.call(order, sort_vars), ]
    }
    
    sorted_subjects <- sorted_data[[subjid_var]]
    
    subject_level_dataset[[subjid_var]] <- factor(
      subject_level_dataset[[subjid_var]], 
      levels = sorted_subjects
    )
    exposure_dataset[[subjid_var]] <- factor(
      exposure_dataset[[subjid_var]], 
      levels = sorted_subjects
    )
    
    if (has_response_data) {
      response_dataset[[subjid_var]] <- factor(
        response_dataset[[subjid_var]], 
        levels = sorted_subjects
      )
    }
  }
  
  exposure_dataset$tooltip_trt <- generate_tooltip(exposure_dataset, trt_tooltip_vars)
  
  if (has_response_data) {
    response_dataset$tooltip_point <- generate_tooltip(response_dataset, result_tooltip_vars)
  }
  
  plot_obj <- ggplot2::ggplot() +
    ggiraph::geom_segment_interactive(
      data = exposure_dataset[exposure_dataset[[trt_ongoing_var]] == FALSE, ],
      mapping = ggplot2::aes(
        x = .data[[trt_start_day_var]],
        xend = .data[[trt_end_day_var]],
        y = .data[[subjid_var]],
        yend = .data[[subjid_var]],
        color = .data[[trt_group_var]],
        tooltip = .data[["tooltip_trt"]],
        data_id = .data[[subjid_var]]
      ),
      linewidth = 2
    ) +
    ggiraph::geom_segment_interactive(
      data = exposure_dataset[exposure_dataset[[trt_ongoing_var]] == TRUE, ],      
      mapping = ggplot2::aes(
        x = .data[[trt_start_day_var]],
        xend = .data[[trt_end_day_var]],
        y = .data[[subjid_var]],
        yend = .data[[subjid_var]],
        color = .data[[trt_group_var]],
        tooltip = .data[["tooltip_trt"]],
        data_id = .data[[subjid_var]]
      ),
      linewidth = 1,
      arrow = grid::arrow(length = grid::unit(0.01, "npc"))
    )
  
  if (has_response_data) {
    plot_obj <- plot_obj +
      ggiraph::geom_point_interactive(
        data = response_dataset,
        mapping = ggplot2::aes(
          x = .data[[result_study_day_var]],
          y = .data[[subjid_var]],
          shape = .data[[result_cat_var]],
          tooltip = .data[["tooltip_point"]],
          data_id = .data[[subjid_var]]
        ),
        size = 3
      )
  }
  
  shape_legend <- if (has_response_data) result_legend_label else NULL
  
  plot_obj <- plot_obj +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      color = trt_legend_label,
      shape = shape_legend
    ) +
    ggplot2::xlab(label = plot_x_label) +
    ggplot2::ylab(label = plot_y_label) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top", legend.box = "vertical") +
    ggplot2::scale_y_discrete(limits = rev)
  
  if (!is.null(group_by_vars)) {
    plot_obj <- plot_obj + 
      ggforce::facet_col(
        facets = group_by_vars, 
        scales = "free_y", 
        space = "free"
      )
  }
  
  if (!is.null(color_palette)) {
    plot_obj <- plot_obj + 
      ggplot2::scale_color_manual(values = color_palette)
  }
  
  if (has_response_data && !is.null(shape_mapping)) {
    plot_obj <- plot_obj + 
      ggplot2::scale_shape_manual(values = shape_mapping)
  }
  
  plot_obj <- plot_obj +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(color = "black")
    )
  
  if (!interactive_plot) {
    return(plot_obj)
  }
  
  hover_css <- if (enable_jumping) "fill:orange;cursor:pointer;" else "fill:orange;"
  
  ggiraph::girafe(
    ggobj = plot_obj, 
    width_svg = plot_width, 
    height_svg = plot_height,
    options = list(
      ggiraph::opts_hover(css = hover_css),
      ggiraph::opts_selection(css = "fill:red;stroke:black;", type = "single")
    )
  )
}

#' Generate tooltips for interactive plot elements
#'
#' @param dataset `[data.frame]` A data frame containing the variables to be used in tooltips
#' @param tooltip_vector `[character(n)]` A named character vector where names are labels and values are column names
#'
#' @return A character vector of HTML-formatted tooltip strings
#' 
#' @keywords internal
generate_tooltip <- function(dataset, tooltip_vector) {
  if (is.null(tooltip_vector) || length(tooltip_vector) == 0) {
    return(rep("", nrow(dataset)))
  }
  
  has_names <- !is.null(names(tooltip_vector)) && any(names(tooltip_vector) != "")
  if (!has_names) {
    return(rep("", nrow(dataset)))
  }
  
  valid_cols <- tooltip_vector[tooltip_vector %in% colnames(dataset)]
  if (length(valid_cols) == 0) {
    return(rep("", nrow(dataset)))
  }
  
  labels <- names(valid_cols)
  
  formatted_data <- dataset
  for (col in valid_cols) {
    if (is.factor(dataset[[col]])) {
      formatted_data[[col]] <- as.character(dataset[[col]])
    } else if (is.numeric(dataset[[col]])) {
      formatted_data[[col]] <- format(dataset[[col]], scientific = FALSE, trim = TRUE)
    }
  }
  
  tooltips <- apply(formatted_data, 1, function(row) {
    tooltip_parts <- sapply(seq_along(valid_cols), function(j) {
      col <- valid_cols[j]
      label <- labels[j]
      val <- row[col]
      
      if (is.na(val)) {
        return(NULL)
      }
      
      paste0(label, val)
    })
    
    tooltip_parts <- tooltip_parts[!sapply(tooltip_parts, is.null)]
    if (length(tooltip_parts) == 0) {
      return("")
    }
    
    paste(tooltip_parts, collapse = "<br>")
  })
  
  tooltips
}
