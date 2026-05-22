make_sort_data <- function() {
  dm <- data.frame(
    USUBJID = c(
      "01-001-0001", 
      "01-001-0002", 
      "01-001-0003",
      "01-001-0004", 
      "01-001-0005", 
      "01-001-0006"
    ),
    AGE = c(45, 32, 58, 27, 61, 39),
    SEX = c("M", "F", "M", "F", "M", "F"),
    stringsAsFactors = FALSE
  )
  ex <- data.frame(
    USUBJID = dm$USUBJID,
    SEX = dm$SEX,
    AGE = dm$AGE,
    EXSTDY = rep(1L, 6),
    ex_end = c(30, 20, 50, 15, 60, 25),
    ex_trt = rep("Drug A", 6),
    ex_ongoing = rep(FALSE, 6),
    stringsAsFactors = FALSE
  )
  list(dm = dm, ex = ex)
}

get_y_levels <- function(plot_obj) {
  levels(plot_obj$layers[[1]]$data[["USUBJID"]])
}

make_swimmerplot <- function(dm, 
                             ex,
                             sort_by_vars  = NULL,
                             sort_direction = "asc",
                             group_by_vars  = NULL) {
  dv.swimmerplot:::swimmerplot(
    subject_level_dataset = dm,
    subjid_var = "USUBJID",
    sort_by_vars = sort_by_vars,
    sort_direction = sort_direction,
    group_by_vars = group_by_vars,
    exposure_dataset = ex,
    trt_start_day_var = "EXSTDY",
    trt_end_day_var = "ex_end",
    trt_ongoing_var = "ex_ongoing",
    trt_group_var = "ex_trt",
    plot_height = 6,
    interactive_plot = FALSE
  )
}

test_that(
  "sort_direction: no grouping, no sorting variable" |> vdoc[["add_spec"]](specs$sort_direction),
  {
    d <- make_sort_data()
    p_asc  <- make_swimmerplot(d$dm, d$ex, sort_direction = "asc")
    p_desc <- make_swimmerplot(d$dm, d$ex, sort_direction = "desc")
    expect_equal(get_y_levels(p_asc), rev(get_y_levels(p_desc)))
  }
)

test_that(
  "sort_direction: with grouping, no sorting variable" |> vdoc[["add_spec"]](specs$sort_direction),
  {
    d <- make_sort_data()
    p_asc  <- make_swimmerplot(d$dm, d$ex, sort_direction = "asc",  group_by_vars = "SEX")
    p_desc <- make_swimmerplot(d$dm, d$ex, sort_direction = "desc", group_by_vars = "SEX")
    expect_equal(get_y_levels(p_asc), rev(get_y_levels(p_desc)))
  }
)

test_that(
  "sort_direction: with grouping and sorting variable" |> vdoc[["add_spec"]](specs$sort_direction),
  {
    d <- make_sort_data()
    p_asc  <- make_swimmerplot(d$dm, d$ex, sort_by_vars = "AGE", sort_direction = "asc",  group_by_vars = "SEX")
    p_desc <- make_swimmerplot(d$dm, d$ex, sort_by_vars = "AGE", sort_direction = "desc", group_by_vars = "SEX")
    expect_equal(get_y_levels(p_asc), rev(get_y_levels(p_desc)))
  }
)
