test_that(
  "swimmerplot_UI() generates UI with controls" |> 
    vdoc[["add_spec"]](specs$ui_customization), 
  {
  ui <- swimmerplot_UI(
    id = "testmod",
    group_by_vars = c("SEX", "ARM"),
    sort_by_vars = c("AGE", "SEX"),
    jumping_enabled = TRUE
  )
  
  expect_s3_class(ui, "shiny.tag.list")
  
  expect_true(any(grepl("group_vars", as.character(ui))))
  
  expect_true(any(grepl("sort_vars", as.character(ui))))
  
  expect_true(any(grepl("sort_direction", as.character(ui))))
  
  expect_true(any(grepl("testmod-swimmer_plot", as.character(ui))))
  
  expect_true(any(grepl("Click on a subject", as.character(ui))))
})
