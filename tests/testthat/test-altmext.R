test_that("altmext has altmejd's columns and types", {
  expect_identical(names(altmext), names(altmejd))
  expect_identical(
    vapply(altmext, class, character(1)),
    vapply(altmejd, class, character(1))
  )
  expect_identical(levels(altmext$replicate), levels(altmejd$replicate))
  expect_false(anyNA(altmext))
  expect_false(anyDuplicated(altmext$eid) > 0)
})

test_that("altmext holds the eight projects with their outcomes", {
  n <- table(altmext$pid)
  success <- tapply(altmext$replicate == "success", altmext$pid, sum)
  want_n <- c(
    eerp = 18,
    holz = 26,
    ml1 = 13,
    ml2 = 24,
    ml3 = 10,
    rpp = 90,
    score = 92,
    ssrp = 21
  )
  want_success <- c(
    eerp = 11,
    holz = 14,
    ml1 = 11,
    ml2 = 13,
    ml3 = 3,
    rpp = 31,
    score = 46,
    ssrp = 13
  )
  expect_equal(as.vector(n[names(want_n)]), unname(want_n))
  expect_equal(as.vector(success[names(want_success)]), unname(want_success))
})

test_that("altmejd723's rows are unchanged in altmext", {
  old <- altmejd[altmejd$pid != "ssrp", ]
  new <- altmext[match(old$eid, altmext$eid), ]
  rownames(old) <- rownames(new) <- NULL
  expect_identical(new, old)
})

test_that("the per-project data sets make up altmext", {
  parts <- list(ssrp1026, holz1026, mltwo1026, score1026)
  for (part in parts) {
    rows <- altmext[match(part$eid, altmext$eid), ]
    expect_false(anyNA(rows$eid))
    expect_equal(rows$effect_size.o, part$effect_size.o)
    expect_equal(rows$p_value.o, part$p_value.o)
    expect_equal(rows$n.o, as.integer(part$n.o))
    expect_equal(as.integer(rows$replicate == "success"), part$replicate)
  }
})

test_that("power.o follows from effect_size.o and n.o in the rebuilt rows", {
  rows <- altmext[!altmext$eid %in% altmejd723$eid, ]
  n <- rows$n.o
  r <- rows$effect_size.o
  t_crit <- stats::qt(0.025, df = n - 2, lower.tail = FALSE)
  r_crit <- sqrt(t_crit^2 / (t_crit^2 + n - 2))
  z_r <- atanh(r) + r / (2 * (n - 1))
  power <- stats::pnorm((z_r - atanh(r_crit)) * sqrt(n - 3)) +
    stats::pnorm((-z_r - atanh(r_crit)) * sqrt(n - 3))
  expect_equal(rows$power.o, power)
})
