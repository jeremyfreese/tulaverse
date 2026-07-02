# Assertions on .detect_var_type() in R/tab_helpers.R, the type dispatcher
# behind tulatab().

test_that(".detect_var_type classifies the common types", {
  expect_equal(.detect_var_type(1:3), "numeric")
  expect_equal(.detect_var_type(c(1.5, 2.5)), "numeric")
  expect_equal(.detect_var_type(letters[1:3]), "character")
  expect_equal(.detect_var_type(factor(c("a", "b"))), "factor")
  expect_equal(.detect_var_type(ordered(c("lo", "hi"))), "ordered")
})

test_that(".detect_var_type checks haven_labelled before numeric", {
  skip_if_not_installed("haven")
  v <- haven::labelled(c(1, 2), labels = c(no = 1, yes = 2))
  expect_equal(.detect_var_type(v), "haven_labelled")
})

# Logical vectors are supported by normalising them to a two-level factor at
# ingestion (.coerce_tab_logical), so .detect_var_type() then sees a factor.
# This was previously a gap where tulatab() errored on logical input.
test_that(".coerce_tab_logical turns a logical into a FALSE/TRUE factor", {
  f <- .coerce_tab_logical(c(TRUE, FALSE, TRUE))
  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("FALSE", "TRUE"))
  expect_equal(as.integer(table(f)), c(1L, 2L))
})

test_that(".coerce_tab_logical drops the unobserved level and passes others through", {
  expect_equal(levels(.coerce_tab_logical(c(TRUE, TRUE))), "TRUE")
  expect_identical(.coerce_tab_logical(1:3), 1:3)
  expect_identical(.coerce_tab_logical(letters[1:2]), letters[1:2])
})

test_that(".coerce_tab_logical preserves a haven-style label attribute", {
  v <- structure(c(TRUE, FALSE), label = "is manual")
  expect_equal(attr(.coerce_tab_logical(v), "label"), "is manual")
})
