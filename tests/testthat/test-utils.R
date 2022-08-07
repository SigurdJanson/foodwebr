a <- function() {
  x <- utils::getAnywhere("x")
  y <- exp(mean(1:10))
  z <- rlang::sym("x")
  zz <- get_called_functions(a)
  TRUE
}
bb <- function() seq_len(3)
cc <- function() { 3 |> seq_len() }
dd <- function() 1:3 |> mean()



test_that("`get_calls()` detects calls correctly", {
  cls <- get_calls(body(a))
  cls <- unlist(cls)
  expect_identical(cls[[2]], quote(utils::getAnywhere))
  expect_identical(cls[[3]], quote(exp))
  expect_identical(cls[[4]], quote(mean))
})


test_that("`get_calls(bb)`: with `bb` being a single call to other function without brackets", {
  # act
  cls <- get_calls(body(bb))
  cls <- unlist(cls)

  # assert
  expect_identical(cls[[1]], quote(seq_len))
})


test_that("`get_calls(cc)` with `cc` including a |> (pipe)", {
  # act
  cls <- get_calls(body(cc))
  cls <- unlist(cls)

  # assert
  expect_identical(cls[[1]], quote(`{`))
  expect_identical(cls[[2]], quote(seq_len))
})

test_that("`get_calls()`:  no function brackets; pipe", {
  # act
  cls <- get_calls(body(dd))
  cls <- unlist(cls)

  # assert
  expect_identical(cls[[2]], quote(`:`))
  expect_identical(cls[[1]], quote(mean))
})




test_that("`get_called_functions()` captures the right functions", {
  expect_equal(
    get_called_functions(a),
    c("utils::getAnywhere", "rlang::sym", "foodwebr::get_called_functions")
  )
})