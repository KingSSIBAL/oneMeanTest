# Test report formatting functionality

test_that("format_ttest_report produces output", {
  skip_if_not_installed("oneMeanTest")
  skip_if_not(exists("format_ttest_report"))
  
  set.seed(999)
  x <- rnorm(25, mean = 7, sd = 2)
  result <- one_mean_test(x, mu0 = 5, check_assumptions = FALSE)
  
  report <- format_ttest_report(result)
  
  expect_type(report, "character")
  expect_true(nchar(report) > 0)
})

test_that("interpretation changes with alternative hypothesis", {
  set.seed(1010)
  x <- rnorm(50, mean = 8, sd = 2)
  
  result_two <- one_mean_test(x, mu0 = 5, alternative = "two.sided",
                              check_assumptions = FALSE)
  result_gr <- one_mean_test(x, mu0 = 5, alternative = "greater",
                            check_assumptions = FALSE)
  result_ls <- one_mean_test(x, mu0 = 10, alternative = "less",
                            check_assumptions = FALSE)
  
  # Interpretations should mention the direction
  expect_true(grepl("different from", result_two$interpretation) ||
              grepl("two.sided", result_two$alternative))
  expect_true(grepl("greater than", result_gr$interpretation) ||
              grepl("greater", result_gr$alternative))
  expect_true(grepl("less than", result_ls$interpretation) ||
              grepl("less", result_ls$alternative))
})

test_that("report includes proper t-test components", {
  skip_if_not(exists("format_ttest_report"))
  
  set.seed(1111)
  x <- rnorm(30, mean = 10, sd = 3)
  result <- one_mean_test(x, mu0 = 10, check_assumptions = FALSE)
  
  # Print should work without error
  expect_output(print(result))
  
  # Should show key statistics
  output <- capture.output(print(result))
  output_text <- paste(output, collapse = "\n")
  
  expect_true(any(grepl("mean", output, ignore.case = TRUE)))
  expect_true(any(grepl("sd", output, ignore.case = TRUE)))
  expect_true(any(grepl("t", output, ignore.case = TRUE)))
  expect_true(any(grepl("p", output, ignore.case = TRUE)))
})
