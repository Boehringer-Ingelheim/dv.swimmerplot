test_that(
  "mock swimmerplot MM app integrates with Module Manager" |> 
    vdoc[["add_spec"]](specs$mm_integration), {
      app_path <- testthat::test_path("apps", "mock_swimmerplot_mm")
      app <- shinytest2::AppDriver$new(app_dir = app_path, name = "mock_swimmerplot_mm")
      
      app$wait_for_idle()
      dataset_list_name <- app$get_value(input = "selector")
      filter_json <- sprintf('{"filters":{"datasets_filter":{"children":[]},"subject_filter":{"children":[{"kind":"row_operation","operation":"and","children":[{"kind":"filter","dataset":"dm","operation":"select_subset","variable":"SEX","values":["M"],"include_NA":false}]}]}},"dataset_list_name":"%s"}', dataset_list_name)
      app$run_js(sprintf("dv_filter.request_dataset_filter_state({id:\"filter\", state:`%s`})", filter_json))
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
