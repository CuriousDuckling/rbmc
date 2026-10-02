library(shinytest2)
library(pdftools)

report_test <- function(clino_cat,
                        num_highr,
                        num_mediumr,
                        num_lowr,
                        result_risk) {
  app <- AppDriver$new(
    test_path("../.."),
    variant = platform_variant(),
    name = "rbmc",
    height = 631,
    width = 979,
    timeout = 60000
  )
  
  app$set_inputs(clino_cat = clino_cat)
  
  # Get the risk table from the *.csv used by the app and sample risks to mark as applicable
  texttab <- read.csv(test_path("../../texttable.csv"), header = TRUE)
  # category -> match sidebar tab names with app variables/items
  tab_map <- c(
    "I. Participant"  = "part",
    "II. Design"      = "desi",
    "III. Safety"     = "safe",
    "IV. Intervention" = "inte",
    "V. Management"   = "mana",
    "VI. Data"        = "data"
  )
  #choose number of random risks to mark as applicable (Plus 1 will be 'other risk')
  set.seed(123)
  chosen <- texttab[sample(nrow(texttab), sum(num_highr, num_mediumr, num_lowr) -
                             1), ]
  chosen_medium <- chosen$ref[seq_len(num_mediumr)]
  chosen_high <- tail(chosen$ref, num_highr)
  risks <- split(chosen$ref, factor(unname(tab_map[chosen$category]), levels = unname(tab_map)))
  risks <- risks[lengths(risks) > 0]
  
  # Loop through the tabs and set the inputs for the chosen risks
  for (tab in names(risks)) {
    message("Visiting tab: ", tab)
    # visit the tab so its renderUI() runs and the inputs exist
    app$click(selector = sprintf("a[data-value='%s']", tab))
    app$wait_for_idle()
    for (r in risks[[tab]]) {
      message("Setting applicable: ", r)
      app$set_inputs(!!paste0(r, "_appl") := "1")
      app$wait_for_idle()
      if (r %in% chosen_medium) {
        message("Setting medium risk: ", r)
        app$set_inputs(!!paste0(r, "_imp") := "1")
        app$set_inputs(!!paste0(r, "_occ") := "1")
        app$set_inputs(!!paste0(r, "_det") := "1")
        app$wait_for_idle()
      } else if (r %in% chosen_high) {
        message("Setting high risk: ", r)
        app$set_inputs(!!paste0(r, "_imp") := "2")
        app$set_inputs(!!paste0(r, "_occ") := "2")
        app$set_inputs(!!paste0(r, "_det") := "2")
        app$wait_for_idle()
      }
    }
  }
  
  # Other risks: add one
  app$click("addOtherInput")
  app$click(selector = "a[data-value='othe']")
  app$wait_for_idle()
  app$set_inputs(other1_tx = "OtherRisk")
  
  # Report: render report page and generate PDF
  app$click(selector = "a[data-value='repo']")
  app$wait_for_value(output = "report_table")
  pdf_file <- app$get_download("report")
  
  # Extract PDF text and collapse line breaks / repeated spaces
  pdf_text <- pdftools::pdf_text(pdf_file) |>
    paste(collapse = " ") |>
    gsub(pattern = "\\s+", replacement = " ")
  
  # Check that the PDF text contains the expected content
  # suggested risk strategy: check that risk correspond to given numbers and resulting risk
  expect_match(
    pdf_text,
    paste0(
      "Swiss risk category: ",
      clino_cat,
      " Based on the assumptions fed into the RBM score calculator, the study risk is ",
      result_risk,
      " and we thus recommend applying the monitoring strategy for ",
      result_risk,
      "-risk trials."
    )
  )
  # summary table: check that risk correspond to given numbers
  expect_match(
    pdf_text,
    paste0(
      "Number of risks High ",
      num_highr,
      " Medium ",
      num_mediumr,
      " Low ",
      num_lowr
    )
  )
}

# ClinO Cat A Tests
test_that("CatA Trial with 5 medium risks and 10 low risks", {
  report_test(
    clino_cat = "A",
    num_highr = 0,
    num_mediumr = 5,
    num_lowr = 10,
    result_risk = "low"
  )
})

test_that("CatA Trial with 6 medium risks and 9 risks", {
  report_test(
    clino_cat = "A",
    num_highr = 0,
    num_mediumr = 6,
    num_lowr = 9,
    result_risk = "low"
  )
})

test_that("CatA Trial with 12 medium risks and 3 low risks", {
  report_test(
    clino_cat = "A",
    num_highr = 0,
    num_mediumr = 12,
    num_lowr = 3,
    result_risk = "low"
  )
})

test_that("CatA Trial with 1 high risks and 14 low risks", {
  report_test(
    clino_cat = "A",
    num_highr = 1,
    num_mediumr = 0,
    num_lowr = 14,
    result_risk = "low"
  )
})

test_that("CatA Trial with 13 medium risks and 2 low risks", {
  report_test(
    clino_cat = "A",
    num_highr = 0,
    num_mediumr = 13,
    num_lowr = 2,
    result_risk = "medium"
  )
})

test_that("CatA Trial with 2 high risks and 13 low risks", {
  report_test(
    clino_cat = "A",
    num_highr = 2,
    num_mediumr = 0,
    num_lowr = 13,
    result_risk = "medium"
  )
})


# ClinO Cat B Tests
test_that("CatB Trial with 5 medium risks and 10 low risks", {
  report_test(
    clino_cat = "B",
    num_highr = 0,
    num_mediumr = 5,
    num_lowr = 10,
    result_risk = "low"
  )
})

test_that("CatB Trial with 6 medium risks and 9 risks", {
  report_test(
    clino_cat = "B",
    num_highr = 0,
    num_mediumr = 6,
    num_lowr = 9,
    result_risk = "medium"
  )
})

test_that("CatB Trial with 12 medium risks and 3 low risks", {
  report_test(
    clino_cat = "B",
    num_highr = 0,
    num_mediumr = 12,
    num_lowr = 3,
    result_risk = "medium"
  )
})

test_that("CatB Trial with 1 high risks and 14 low risks", {
  report_test(
    clino_cat = "B",
    num_highr = 1,
    num_mediumr = 0,
    num_lowr = 14,
    result_risk = "medium"
  )
})

test_that("CatB Trial with 13 medium risks and 2 low risks", {
  report_test(
    clino_cat = "B",
    num_highr = 0,
    num_mediumr = 13,
    num_lowr = 2,
    result_risk = "high"
  )
})

test_that("CatB Trial with 2 high risks and 13 low risks", {
  report_test(
    clino_cat = "B",
    num_highr = 2,
    num_mediumr = 0,
    num_lowr = 13,
    result_risk = "high"
  )
})


# ClinO Cat C Tests
test_that("CatC Trial with 5 medium risks and 10 low risks", {
  report_test(
    clino_cat = "C",
    num_highr = 0,
    num_mediumr = 5,
    num_lowr = 10,
    result_risk = "medium"
  )
})

test_that("CatC Trial with 6 medium risks and 9 risks", {
  report_test(
    clino_cat = "C",
    num_highr = 0,
    num_mediumr = 6,
    num_lowr = 9,
    result_risk = "high"
  )
})

test_that("CatC Trial with 12 medium risks and 3 low risks", {
  report_test(
    clino_cat = "C",
    num_highr = 0,
    num_mediumr = 12,
    num_lowr = 3,
    result_risk = "high"
  )
})

test_that("CatC Trial with 1 high risks and 14 low risks", {
  report_test(
    clino_cat = "C",
    num_highr = 1,
    num_mediumr = 0,
    num_lowr = 14,
    result_risk = "high"
  )
})

test_that("CatC Trial with 13 medium risks and 2 low risks", {
  report_test(
    clino_cat = "C",
    num_highr = 0,
    num_mediumr = 13,
    num_lowr = 2,
    result_risk = "high"
  )
})

test_that("CatC Trial with 2 high risks and 13 low risks", {
  report_test(
    clino_cat = "C",
    num_highr = 2,
    num_mediumr = 0,
    num_lowr = 13,
    result_risk = "high"
  )
})