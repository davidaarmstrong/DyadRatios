library(testthat)
library(DyadRatios)

# ---- helpers ---------------------------------------------------------------
make_synthetic_data <- function(n_items = 3, n_years = 20,
                                seed = 42, start_year = 1980) {
  set.seed(seed)
  years <- seq(start_year, start_year + n_years - 1)
  true_mood <- 50 + cumsum(rnorm(n_years, 0, 1))

  items <- paste0("item_", letters[seq_len(n_items)])
  rows <- list()
  for (item in items) {
    sign <- sample(c(1, -1), 1)
    noise <- rnorm(n_years, 0, 3)
    index <- 50 + sign * (true_mood - mean(true_mood)) / sd(true_mood) * 8 + noise
    rows[[item]] <- data.frame(
      varname = item,
      date    = as.Date(paste0(years, "-07-01")),
      index   = index,
      n       = sample(800:1200, n_years, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# ============================================================================

test_that("extract returns correct structure", {
  dat <- make_synthetic_data()
  res <- extract(dat, n_col = "n", smoothing = FALSE)

  expect_s3_class(res, "extract")
  expect_true(is.numeric(res$mood))
  expect_equal(length(res$mood), res$n_periods)
  expect_true(is.data.frame(res$loadings))
  expect_true(is.data.frame(res$iterations))
  expect_true(is.data.frame(res$periods))
  expect_true("dim1" %in% names(res$loadings))
  expect_equal(nrow(res$loadings), res$n_series)
})

test_that("mood has no NAs", {
  dat <- make_synthetic_data()
  res <- extract(dat, n_col = "n", smoothing = FALSE)
  expect_false(anyNA(res$mood))
})

test_that("smoothing = TRUE runs without error", {
  dat <- make_synthetic_data(n_years = 25)
  expect_no_error(
    extract(dat, n_col = "n", smoothing = TRUE)
  )
})

test_that("n_dim = 2 works and returns two mood vectors", {
  dat <- make_synthetic_data(n_items = 5, n_years = 25)
  res <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  expect_equal(length(res$mood),      res$n_periods)
  expect_equal(length(res$mood_dim2), res$n_periods)
  expect_false(anyNA(res$mood))
  expect_false(anyNA(res$mood_dim2))
  expect_true("dim2" %in% names(res$loadings))
})

test_that("variance_explained is in (0, 1]", {
  dat <- make_synthetic_data()
  res <- extract(dat, n_col = "n", smoothing = FALSE)
  expect_true(res$variance_explained > 0 && res$variance_explained <= 1)
})

test_that("loadings are in [-1, 1]", {
  dat <- make_synthetic_data()
  res <- extract(dat, n_col = "n", smoothing = FALSE)
  expect_true(all(abs(res$loadings$dim1) <= 1 + 1e-6))
})

test_that("quarterly aggregation produces correct number of periods", {
  dat <- make_synthetic_data(n_years = 10)
  dat2 <- dat
  dat2$date <- dat$date + 90
  dat2$date[dat2$date > max(dat$date)] <- max(dat$date)
  combined <- rbind(dat, dat2)
  combined$date <- as.Date(paste0(format(combined$date, "%Y-"),
                                  c("01","04","07","10")[ceiling(as.integer(format(combined$date,"%m"))/3)],
                                  "-01"))
  expect_no_error(
    extract(combined, n_col = "n",
            agg_interval = "quarterly", smoothing = FALSE)
  )
})

test_that("start_date / end_date filtering works", {
  dat <- make_synthetic_data(n_years = 30, start_year = 1970)
  res_full <- extract(dat, n_col = "n", smoothing = FALSE)
  res_trim <- extract(dat, n_col = "n", smoothing = FALSE,
                      start_date = "1980-01-01",
                      end_date   = "1989-12-31")
  expect_true(res_trim$n_periods < res_full$n_periods)
  expect_true(res_trim$settings$start_date >= as.Date("1980-01-01"))
})

test_that("print and summary do not error", {
  dat <- make_synthetic_data()
  res <- extract(dat, n_col = "n", smoothing = FALSE)
  expect_no_error(print(res))
  expect_no_error(summary(res))
})

test_that("missing n_col uses default weight of 1000", {
  dat <- make_synthetic_data()
  dat$n <- NULL
  res <- expect_no_error(extract(dat, smoothing = FALSE))
  expect_s3_class(res, "extract")
})

# ============================================================================
# boot_dr tests
# ============================================================================

test_that("boot_dr returns a list with correct structure", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 1)
  expect_s3_class(boot, "boot_dr")
  expect_true(all(c("estimates", "samples") %in% names(boot)))
  expect_s3_class(boot$estimates, "data.frame")
  expect_true(is.matrix(boot$samples))
  expect_true(all(c("period", "year", "mood", "lower", "upper") %in% names(boot$estimates)))
})

test_that("boot_dr estimates has one row per period", {
  dat  <- make_synthetic_data(n_years = 15)
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 2)
  expect_equal(nrow(boot$estimates), res$n_periods)
})

test_that("boot_dr samples matrix has correct dimensions", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 2)
  expect_equal(nrow(boot$samples), res$n_periods)
  expect_equal(ncol(boot$samples), attr(boot, "R"))
})

test_that("boot_dr mood column matches original extract", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 3)
  expect_equal(boot$estimates$mood, res$mood)
})

test_that("boot_dr confidence bounds bracket the point estimate", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 50, seed = 4)
  expect_true(all(boot$estimates$lower <= boot$estimates$mood + 1e-9))
  expect_true(all(boot$estimates$upper >= boot$estimates$mood - 1e-9))
})

test_that("boot_dr R attribute records successful replications", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 30, seed = 5)
  expect_true(attr(boot, "R") <= 30)
  expect_true(attr(boot, "R") > 0)
})

test_that("plot.boot_dr runs without error", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 6)
  expect_no_error(plot(boot))
})

# ============================================================================
# boot_dr two-dimension tests
# ============================================================================

test_that("boot_dr n_dim=2 returns samples_dim2 matrix", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 7)
  expect_true("samples_dim2" %in% names(boot))
  expect_true(is.matrix(boot$samples_dim2))
  expect_equal(nrow(boot$samples_dim2), res$n_periods)
  expect_equal(ncol(boot$samples_dim2), attr(boot, "R"))
})

test_that("boot_dr n_dim=2 estimates has dim2 CI columns", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 8)
  expect_true(all(c("mood_dim2", "lower_dim2", "upper_dim2") %in% names(boot$estimates)))
  expect_false(anyNA(boot$estimates$mood_dim2))
})

test_that("boot_dr n_dim=2 mood_dim2 matches original extract", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 9)
  expect_equal(boot$estimates$mood_dim2, res$mood_dim2)
})

test_that("boot_dr n_dim=2 dim2 CI bounds bracket point estimate", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 50, seed = 10)
  expect_true(all(boot$estimates$lower_dim2 <= boot$estimates$mood_dim2 + 1e-9))
  expect_true(all(boot$estimates$upper_dim2 >= boot$estimates$mood_dim2 - 1e-9))
})

test_that("boot_dr n_dim=2 n_dim attribute is 2", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 11)
  expect_equal(attr(boot, "n_dim"), 2L)
})

test_that("plot.boot_dr dim=2 runs without error", {
  dat  <- make_synthetic_data(n_items = 5, n_years = 25)
  res  <- extract(dat, n_col = "n", n_dim = 2, smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 12)
  expect_no_error(plot(boot, dim = 2))
})

test_that("plot.boot_dr dim=2 errors on 1-dim object", {
  dat  <- make_synthetic_data()
  res  <- extract(dat, n_col = "n", smoothing = FALSE)
  boot <- boot_dr(res, dat, R = 20, seed = 13)
  expect_error(plot(boot, dim = 2))
})
