#' Prepare the per-project figure, checking the arguments on the way
#'
#' Does all the work of draw_logo_cv() that draws nothing, in the order the
#' drawing needs it. Its callers run it before par() or pdf(), so an argument
#' it rejects stops before a device or a file is opened. Returns the pieces
#' draw_logo_cv() takes, and in `facts`, what it returns.
#' @noRd
prepare_logo_cv <- function(x, metric, levels, baseline, ylab, add_keys) {
  # a factor would pick the bands' column by its code
  metric <- as.character(metric)
  estimates <- read_metric_rows(x, metric)
  levels <- sort(levels)

  intervals <- compute_bands(x$ci, estimates$project, metric, levels)
  bands <- intervals$bands

  reference <- find_reference(metric, baseline, estimates$base_rate)

  values <- c(estimates$estimate, unlist(bands), reference$values)
  data_range <- range(values, na.rm = TRUE)
  padding <- max(diff(data_range), 0.05) * 0.14
  ylim <- c(data_range[1] - padding, data_range[2] + padding * 2)
  if (!all(is.finite(ylim))) {
    stop("need finite 'ylim' values", call. = FALSE)
  }

  # ticks only where the metric can go: the headroom for the n labels is not
  # a value
  bounds <- if (metric == "mcc") c(-1, 1) else c(0, 1)

  banded <- !is.null(x$ci) && any(!vapply(bands, is.null, logical(1)))
  key_bands <- add_keys && banded
  key_reference <- add_keys && nzchar(reference$label)

  read_widest <- function(i, bound) {
    if (is.null(bands[[i]])) NA_real_ else bands[[i]][bound, length(levels)]
  }
  positions <- seq_len(nrow(estimates))
  facts <- data.frame(
    project = estimates$project,
    n = estimates$n,
    estimate = estimates$estimate,
    lower = vapply(positions, read_widest, numeric(1), bound = 1L),
    upper = vapply(positions, read_widest, numeric(1), bound = 2L),
    rejected = intervals$rejection_rate,
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  list(
    estimates = estimates,
    levels = levels,
    bands = bands,
    reference = reference,
    data_range = data_range,
    padding = padding,
    ylim = ylim,
    ylab = ylab,
    bounds = bounds,
    key_bands = key_bands,
    key_reference = key_reference,
    facts = facts
  )
}

#' Draw the per-project estimates and bands; return what was drawn
#'
#' Shared by plot.logo_cv() and tutplot_logocv(), which differ in margins,
#' title, keys and how they label the projects and their sizes. Takes the
#' output of prepare_logo_cv().
#' @noRd
draw_logo_cv <- function(
  figure,
  main = NULL,
  label_project = identity,
  italic_n = FALSE
) {
  estimates <- figure$estimates
  levels <- figure$levels
  bands <- figure$bands
  reference <- figure$reference
  data_range <- figure$data_range
  padding <- figure$padding
  ylim <- figure$ylim
  bounds <- figure$bounds
  n_projects <- nrow(estimates)
  positions <- seq_len(n_projects)

  # by name, so a project has the color tutplot_logoscheme() gives it
  colors <- unname(color_projects(estimates$project)[estimates$project])

  graphics::plot(
    NULL,
    xlim = c(0.5, n_projects + 0.5),
    ylim = ylim,
    xaxt = "n",
    yaxt = "n",
    xlab = "",
    ylab = figure$ylab,
    main = main
  )
  graphics::axis(1, at = positions, labels = label_project(estimates$project))
  # round: pretty() returns 1 as 1.0000000000000002, which the filter drops
  ticks <- round(pretty(ylim), 10)
  graphics::axis(
    2,
    at = ticks[
      ticks >= max(ylim[1], bounds[1]) & ticks <= min(ylim[2], bounds[2])
    ]
  )

  if (any(is.finite(reference$values))) {
    graphics::segments(
      positions - 0.42,
      reference$values,
      positions + 0.42,
      reference$values,
      col = "gray55",
      lty = 2,
      lwd = 2
    )
  }

  widths <- seq(9, 2.2, length.out = length(levels))
  opacities <- seq(1, 0.32, length.out = length(levels))
  for (i in positions) {
    band <- bands[[i]]
    if (is.null(band)) {
      next
    }
    for (k in seq_along(levels)) {
      graphics::segments(
        i,
        band[1, k],
        i,
        band[2, k],
        lwd = widths[k],
        col = grDevices::adjustcolor(colors[i], alpha.f = opacities[k])
      )
    }
  }

  # halo first, so the estimate stays legible against the darkest band
  graphics::points(
    positions,
    estimates$estimate,
    pch = 19,
    cex = 1.6,
    col = "white"
  )
  graphics::points(
    positions,
    estimates$estimate,
    pch = 19,
    cex = 1.15,
    col = colors
  )

  # fold size drives the band widths, so it belongs on the plot
  n_labels <- if (italic_n) {
    # paste() keeps the plain " = " and its spaces; `==` and `~` set it
    # tighter and add the Symbol font
    as.expression(lapply(
      estimates$n,
      \(size) bquote(paste(italic(n), " = ", .(size)))
    ))
  } else {
    paste0("n = ", estimates$n)
  }
  graphics::text(
    positions,
    data_range[2] + padding * 1.5,
    n_labels,
    cex = 0.75,
    col = "gray30"
  )

  # keys go in the margin rather than over the data
  if (figure$key_bands) {
    graphics::mtext(
      paste0(
        "Bootstrap intervals: ",
        paste0(round(100 * levels), "%", collapse = ", ")
      ),
      side = 3,
      line = 0.25,
      adj = 1,
      cex = 0.72,
      col = "gray35"
    )
  }
  if (figure$key_reference) {
    graphics::mtext(
      reference$label,
      side = 3,
      line = 0.25,
      adj = 0,
      cex = 0.72,
      col = "gray35"
    )
  }

  figure$facts
}

#' Take one metric's rows of the long table, projects in name order; stop if
#' it has none
#' @noRd
read_metric_rows <- function(x, metric) {
  long <- as.data.frame(x)
  estimates <- long[long$metric == metric, , drop = FALSE]
  if (nrow(estimates) == 0L) {
    stop("no estimates for metric `", metric, "`.", call. = FALSE)
  }
  estimates[order(estimates$project), , drop = FALSE]
}

#' Compute each project's bands from its bootstrap draws
#'
#' Also returns the share of resamples each bootstrap rejected.
#' @noRd
compute_bands <- function(ci, projects, metric, levels) {
  bands <- vector("list", length(projects))
  rejection_rate <- rep(0, length(projects))
  for (i in seq_along(projects)) {
    project_ci <- ci[[projects[i]]]
    draws <- attr(project_ci, "draws")
    if (is.null(draws) || !metric %in% colnames(draws)) {
      next
    }
    # from the draws, so any level can be asked for later
    bands[[i]] <- vapply(
      levels,
      function(level) {
        tail_prob <- (1 - level) / 2
        stats::quantile(
          draws[, metric],
          c(tail_prob, 1 - tail_prob),
          na.rm = TRUE
        )
      },
      numeric(2)
    )
    n_dropped <- project_ci$drop[1]
    rejection_rate[i] <- n_dropped /
      (n_dropped + attr(project_ci, "n_resample"))
  }
  list(bands = bands, rejection_rate = rejection_rate)
}

#' Find the reference line and its key
#' @noRd
find_reference <- function(metric, baseline, base_rate) {
  n_projects <- length(base_rate)
  if (!identical(baseline, "auto")) {
    if (length(baseline) == 1L && is.na(baseline)) {
      return(list(values = rep(NA_real_, n_projects), label = ""))
    }
    return(list(
      values = rep(as.numeric(baseline), n_projects),
      label = paste0("Dashed line: ", signif(baseline, 3))
    ))
  }
  # what a useless model scores: 0.5 for auroc and bacc, 0 for mcc, always
  # predicting the larger class for acc, the base rate for the precision
  # metrics (auprc, ppv, patk) and f1
  if (metric %in% c("auroc", "bacc")) {
    list(values = rep(0.5, n_projects), label = "Dashed line: chance (0.5)")
  } else if (metric == "mcc") {
    list(values = rep(0, n_projects), label = "Dashed line: no association (0)")
  } else if (metric == "acc") {
    list(
      values = pmax(base_rate, 1 - base_rate),
      label = "Dashed line: majority-class rate"
    )
  } else if (
    metric %in% c("auprc", "ppv", "f1") || grepl("^patk(_|$)", metric)
  ) {
    list(values = base_rate, label = "Dashed line: base rate of each project")
  } else {
    list(values = rep(NA_real_, n_projects), label = "")
  }
}
