test_that(
  "Module is compatible with the DaVinci framework Module Manager" |> 
    vdoc[["add_spec"]](specs$integration$davinci_compatibility), {
      app_path <- testthat::test_path("apps", "mock_swimmerplot_mm")
      app <- shinytest2::AppDriver$new(app_dir = app_path, name = "mock_swimmerplot_mm")
      
      app$set_inputs(`global_filter-vars` = "SEX")
      app$set_inputs(`global_filter-SEX` = "M")
      app$wait_for_idle(duration = 1000)

      plot_output <- app$get_value(output = "swimmer1-swimmer_plot")
      plot_output_list <- jsonlite::fromJSON(plot_output)[["x"]][["html"]] |> 
        xml2::read_xml() |> 
        xml2::as_list()
      
      unlisted_data <- unlist(plot_output_list$svg$g)
      subject_ids <- unlisted_data[grep("^01-", unlisted_data)]
      
      sex <- pharmaversesdtm::dm[pharmaversesdtm::dm$USUBJID %in% subject_ids, ]$SEX
      expect_true(all(sex == "M"))
      
      app$stop()
    }
)
