#' Check `cores` and return how many to use
#' @noRd
choose_cores <- function(cores) {
  if (
    !is.numeric(cores) ||
      length(cores) != 1L ||
      !is.finite(cores) ||
      cores < 1 ||
      cores != round(cores)
  ) {
    stop("`cores` must be one whole number, 1 or more.", call. = FALSE)
  }
  cores <- as.integer(cores)
  # more processes than cores bring no gain; an unknown count is NA
  available <- count_cores()
  if (!is.na(available) && cores > available) {
    message(
      sprintf(
        paste(
          "Specified `cores = %d`, but this computer has only %d %s.",
          "The run continues with `cores = %d`."
        ),
        cores,
        available,
        if (available == 1L) "core" else "cores",
        available
      )
    )
    cores <- as.integer(available)
  }
  cores
}

#' Count the cores of this computer, a seam for tests
#' @noRd
count_cores <- function() {
  parallel::detectCores()
}

#' Read the operating system type, a seam for tests
#' @noRd
read_os_type <- function() {
  .Platform$OS.type
}

#' Run tasks over `cores`, forked or on a socket cluster; keep their order
#'
#' @param globals The plain values of this session the tasks read. Only
#'   socket workers need it, so it is evaluated only there.
#' @param report A function of the number of tasks done, run in this session
#'   after each task on one core and after each batch on several. Without it,
#'   the tasks go out in one batch.
#' @noRd
run_tasks <- function(
  n_tasks,
  run_task,
  cores = 1L,
  globals = list(),
  report = NULL
) {
  if (cores == 1L || n_tasks == 1L) {
    if (is.null(report)) {
      return(lapply(seq_len(n_tasks), run_task))
    }
    return(lapply(seq_len(n_tasks), function(k) {
      result <- run_task(k)
      report(k)
      result
    }))
  }
  n_workers <- min(cores, n_tasks)
  batches <- split_batches(n_tasks, n_workers, batched = !is.null(report))
  if (read_os_type() == "windows") {
    # before any worker starts, so a stop leaves none behind
    force(globals)
    return(run_socket_tasks(
      n_tasks,
      run_task,
      n_workers,
      globals,
      batches = batches,
      report = report
    ))
  }

  # each mclapply() call restarts the L'Ecuyer-CMRG streams at the session's
  # seed, so start each batch where one call would be: task k draws stream k
  shifted <- length(batches) > 1L && RNGkind()[1L] == "L'Ecuyer-CMRG"
  if (shifted) {
    # as mclapply() does when the session has no seed yet
    if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      sample.int(1L)
    }
    seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", seed, envir = globalenv()), add = TRUE)
    stream <- seed
  }
  parts <- vector("list", length(batches))
  for (b in seq_along(batches)) {
    batch <- batches[[b]]
    if (shifted) {
      assign(".Random.seed", stream, envir = globalenv())
      for (i in seq_along(batch)) {
        stream <- parallel::nextRNGStream(stream)
      }
    }
    # a fresh fork per task: tasks differ in cost (depth, largest n_iter), and
    # handing them out as cores free up balances the load
    parts[[b]] <- parallel::mclapply(
      batch,
      run_task,
      mc.cores = min(n_workers, length(batch)),
      mc.preschedule = FALSE
    )
    if (!is.null(report)) {
      report(batch[length(batch)])
    }
  }
  # checked after all batches, so the first error in task order wins, as in
  # a single batch
  results <- do.call(c, parts)
  for (result in results) {
    if (inherits(result, "try-error")) {
      stop(conditionMessage(attr(result, "condition")), call. = FALSE)
    }
  }
  # a worker that was killed, as for lack of memory, returns NULL
  if (any(vapply(results, is.null, logical(1)))) {
    stop(
      "A parallel worker returned no result. Rerun with fewer `cores`.",
      call. = FALSE
    )
  }
  results
}

#' Split the task numbers into batches, one for each report of progress
#' @noRd
split_batches <- function(n_tasks, n_workers, batched, per_worker = 16L) {
  tasks <- seq_len(n_tasks)
  if (!batched) {
    return(list(tasks))
  }
  # a batch waits for its slowest task; 16 per worker keep that idle time to a
  # few seconds in 100, and smaller batches move the bar more often
  size <- n_workers * per_worker
  batches <- unname(split(tasks, (tasks - 1L) %/% size))
  # mclapply() runs a batch of one task in this session, not on a fork
  last <- length(batches)
  if (last > 1L && length(batches[[last]]) == 1L) {
    batches[[last - 1L]] <- c(batches[[last - 1L]], batches[[last]])
    batches[[last]] <- NULL
  }
  batches
}

#' Run tasks on a socket cluster of new R processes; keep their order
#'
#' @param batches,report As in run_tasks().
#' @noRd
run_socket_tasks <- function(
  n_tasks,
  run_task,
  n_workers,
  globals,
  batches = list(seq_len(n_tasks)),
  report = NULL
) {
  # without the user's profile: a worker's state comes from this session
  cl <- parallel::makePSOCKcluster(n_workers, rscript_args = "--no-init-file")
  on.exit(stop_workers(cl), add = TRUE)

  # their enclosures and source references point into this package and would
  # load the installed adatutor on a worker before prepare_worker() chooses
  prepare <- utils::removeSource(prepare_worker)
  environment(prepare) <- baseenv()
  read <- utils::removeSource(read_code)
  environment(read) <- baseenv()
  session <- read_session()
  prepared <- tryCatch(
    parallel::clusterCall(cl, prepare, session, read),
    # a worker that died while it loaded breaks its connection
    error = function(e) NULL
  )
  if (is.null(prepared)) {
    stop(
      "A parallel worker returned no result. Rerun with fewer `cores`.",
      call. = FALSE
    )
  }
  check_workers(prepared, session)

  parts <- vector("list", length(batches))
  for (b in seq_along(batches)) {
    part <- tryCatch(
      {
        if (b == 1L) {
          parallel::clusterExport(
            cl,
            names(globals),
            envir = list2env(globals)
          )
          # once per worker, not once per task: it carries the data and the
          # fit
          parallel::clusterCall(cl, store_task, run_task)
        }
        parallel::clusterApplyLB(cl, batches[[b]], call_task)
      },
      # a worker that died, as for lack of memory, breaks its connection
      error = function(e) NULL
    )
    if (is.null(part)) {
      stop(
        "A parallel worker returned no result. Rerun with fewer `cores`.",
        call. = FALSE
      )
    }
    parts[[b]] <- part
    if (!is.null(report)) {
      report(batches[[b]][length(batches[[b]])])
    }
  }
  # checked after all batches, so the first error in task order wins, as in
  # a single batch
  results <- do.call(c, parts)
  for (result in results) {
    if (inherits(result, "task_error")) {
      stop(result$message, call. = FALSE)
    }
  }
  results
}

#' Read what a socket worker needs to match this session
#' @noRd
read_session <- function() {
  path <- getNamespaceInfo("adatutor", "path")
  learners <- intersect(list_learners(), loadedNamespaces())
  list(
    lib_paths = .libPaths(),
    # the kinds a task's set.seed() runs under, and the collation that orders
    # factor levels
    rng_kind = RNGkind(),
    collate = Sys.getlocale("LC_COLLATE"),
    # missing values and factor coding in a refit, and which of its warnings
    # are errors
    options = lapply(
      stats::setNames(
        nm = c(
          "contrasts",
          "na.action",
          "warn",
          "warnPartialMatchArgs",
          "warnPartialMatchAttr",
          "warnPartialMatchDollar",
          "xgboost.strict_mode"
        )
      ),
      getOption
    ),
    paths = c(
      adatutor = path,
      vapply(learners, \(package) getNamespaceInfo(package, "path"), "")
    ),
    # pkgload::load_all() runs a source folder, which has no Meta folder
    installed = file.exists(file.path(path, "Meta", "package.rds")),
    # the files in `path` can change after this session loaded them
    code = read_code()
  )
}

#' List the learners' packages, which socket workers load as this session did
#' @noRd
list_learners <- function() {
  c("randomForest", "rpart", "xgboost")
}

#' Deparse the adatutor namespace, to compare the code two R processes run
#' @noRd
read_code <- function() {
  # runs on a socket worker with the base environment as its enclosure

  # deparse() writes numbers by `scipen`, which the workers do not share
  old <- options(scipen = 0)
  on.exit(options(old), add = TRUE)
  namespace <- asNamespace("adatutor")
  # tests replace read_os_type() to reach the socket path
  names <- setdiff(ls(namespace, all.names = TRUE), "read_os_type")
  # sorted the same in any locale
  objects <- mget(sort(names, method = "radix"), envir = namespace)
  # environments change as the package runs
  objects <- objects[!vapply(objects, is.environment, logical(1))]
  # deparse() leaves out source references and byte code
  lapply(objects, deparse)
}

#' Load this session's adatutor, learners and settings on a socket worker
#'
#' @param read_code The session's read_code().
#' @return Where the worker loaded each package from and the adatutor code it
#'   loaded, or the message of the error that stopped it.
#' @noRd
prepare_worker <- function(session, read_code) {
  # both run with the base environment as their enclosure
  tryCatch(
    {
      .libPaths(session$lib_paths)
      do.call(RNGkind, as.list(session$rng_kind))
      Sys.setlocale("LC_COLLATE", session$collate)
      for (package in setdiff(names(session$paths), "adatutor")) {
        loadNamespace(package, lib.loc = dirname(session$paths[[package]]))
      }
      # the source under pkgload, so the workers never run an older install
      if (session$installed) {
        loadNamespace(
          "adatutor",
          lib.loc = dirname(session$paths[["adatutor"]])
        )
      } else {
        pkgload::load_all(
          session$paths[["adatutor"]],
          export_all = FALSE,
          helpers = FALSE,
          attach_testthat = FALSE,
          quiet = TRUE
        )
      }
      # last, so `warn` cannot turn a warning while loading into an error
      options(session$options)
      list(
        paths = vapply(
          names(session$paths),
          \(package) getNamespaceInfo(package, "path"),
          ""
        ),
        code = read_code()
      )
    },
    error = function(e) list(error = conditionMessage(e))
  )
}

#' Stop unless every socket worker loaded what this session loaded
#' @noRd
check_workers <- function(results, session) {
  expected <- normalizePath(session$paths, winslash = "/", mustWork = FALSE)
  for (result in results) {
    if (!is.null(result$error)) {
      stop(
        "A parallel worker could not load what the tasks need: ",
        result$error,
        call. = FALSE
      )
    }
    loaded <- normalizePath(result$paths, winslash = "/", mustWork = FALSE)
    # by position: normalizePath() drops the names
    moved <- which(loaded != expected)
    if (length(moved)) {
      stop(
        "A parallel worker loaded ",
        names(session$paths)[moved[1L]],
        " from ",
        loaded[moved[1L]],
        ", not from ",
        expected[moved[1L]],
        " as this session did.",
        call. = FALSE
      )
    }
    if (!identical(result$code, session$code)) {
      stop(
        "adatutor changed since this session loaded it, so a parallel ",
        "worker would run different code. Reload adatutor or restart R.",
        call. = FALSE
      )
    }
  }
}

# the task closure of the current run_tasks() call, on a socket worker
worker_tasks <- new.env(parent = emptyenv())

#' Keep the task closure on a socket worker
#' @noRd
store_task <- function(run_task) {
  worker_tasks$run_task <- run_task
  invisible()
}

#' Run one stored task on a socket worker; an error comes back as its message
#' @noRd
call_task <- function(k) {
  tryCatch(
    worker_tasks$run_task(k),
    # not a try-error, which clusterApplyLB() turns into an error of its own
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "task_error")
    }
  )
}

#' Stop each worker on its own, so a dead one leaves no other running
#' @noRd
stop_workers <- function(cl) {
  for (i in seq_along(cl)) {
    stopped <- tryCatch(
      {
        parallel::stopCluster(cl[i])
        TRUE
      },
      error = function(e) FALSE
    )
    # stopCluster() closes the connection only after it sent "DONE", which
    # can fail for a dead worker
    if (!stopped) {
      try(close(cl[[i]]$con), silent = TRUE)
    }
  }
}

#' List the plain values a refit on socket workers reads; stop at other code
#'
#' @return A named list of the values to send to the workers.
#' @noRd
list_globals <- function(fit, data) {
  # an XGBoost refit runs from stored parameters, with no formula or call
  if (inherits(fit, "xgboost")) {
    return(list())
  }
  formula <- read_formula(fit)
  places <- list(formula = read_names(formula))
  # model.frame() reads the formula's variables from the data first, and `.`
  # stands for the data's other columns
  places$formula$used <- setdiff(places$formula$used, c(names(data), "."))
  # an AdaBoost refit grows its trees from the formula alone
  if (!inherits(fit, "adaboost")) {
    places$call <- read_names(read_refit_call(fit))
  }
  # a name found in a safe home resolves alike on a worker; a plain value on
  # the search path is sent, and code anywhere else stops the run
  safe <- list_safe_homes()
  # a worker has a global environment and search path of its own, without
  # what this session's hold
  search_path <- lapply(seq_along(search()), as.environment)
  check_enclosures(environment(formula), search_path)
  globals <- list()
  for (place in names(places)) {
    found <- places[[place]]
    for (value in found$values) {
      if (!is_plain_value(value) && !is_safe_function(value, safe)) {
        stop_for_workers(NULL, place)
      }
    }
    # a call skips non-functions, so `sd(x) * sd` calls stats' sd() and reads a
    # number `sd`; a string may name either, so it is looked up both ways
    strings <- unique(unlist(lapply(found$values, read_strings)))
    lookups <- list(
      list(names = found$called, kind = "function", quoted = FALSE),
      list(names = found$used, kind = "any", quoted = FALSE),
      list(names = strings, kind = "function", quoted = TRUE),
      list(names = strings, kind = "any", quoted = TRUE)
    )
    for (lookup in lookups) {
      for (name in lookup$names) {
        home <- find_binding(name, environment(formula), lookup$kind)
        if (is.null(home) || any(vapply(safe, identical, logical(1), home))) {
          next
        }
        attached <- any(vapply(search_path, identical, logical(1), home))
        package <- read_package(home, attached)
        if (!attached && !is.null(package)) {
          stop_for_workers(name, place, package = package, attached = FALSE)
        }
        # an active binding runs code each time it is read
        if (bindingIsActive(name, home)) {
          stop_for_workers(name, place, quoted = lookup$quoted)
        }
        # its frame travels with the formula, the argument still missing
        if (is_empty_argument(name, home)) {
          next
        }
        value <- get(name, envir = home, mode = lookup$kind, inherits = FALSE)
        if (!is_plain_value(value)) {
          stop_for_workers(name, place, value, package, quoted = lookup$quoted)
        }
        # any other frame travels with the formula
        if (attached) {
          globals[name] <- list(value)
        }
      }
    }
  }
  globals
}

#' Stop when a formula's frames lead into an attached environment
#' @noRd
check_enclosures <- function(env, search_path) {
  # a worker gets the frames by value up to these, which it has of its own,
  # but an attached one, as sys.source() into attach() makes, by name only
  while (
    !identical(env, globalenv()) &&
      !identical(env, baseenv()) &&
      !identical(env, emptyenv()) &&
      !isNamespace(env)
  ) {
    label <- attr(env, "name", exact = TRUE)
    if (
      any(vapply(search_path, identical, logical(1), env)) ||
        is.character(label) &&
          length(label) == 1L &&
          startsWith(label, "package:")
    ) {
      stop(
        "The fit's formula looks up its objects through the attached ",
        "environment `",
        environmentName(env),
        "`. On Windows, parallel workers get attached environments without ",
        "their objects. Set `cores = 1`, or make the fit at the top level.",
        call. = FALSE
      )
    }
    env <- parent.env(env)
  }
}

#' Read a fit's call as a refit evaluates it
#' @noRd
read_refit_call <- function(fit) {
  fit_call <- stats::getCall(fit)
  fit_call$formula <- NULL
  fit_call$data <- NULL
  # refit_score.rpart() replaces the control list and the function the call
  # runs, and refit_score.randomForest() the function
  if (inherits(fit, "rpart")) {
    fit_call[[1L]] <- quote(rpart::rpart)
    fit_call$control <- NULL
  }
  if (inherits(fit, "randomForest")) {
    fit_call[[1L]] <- quote(randomForest::randomForest)
  }
  fit_call
}

#' Read the names an expression evaluates, by how R looks them up
#'
#' @return A list: `called`, the names it calls, which R looks up among
#'   functions only; `used`, the other names; and `values`, what it holds
#'   itself, not by name, such as the function do.call() puts in a call.
#' @noRd
read_names <- function(expr) {
  found <- list(called = character(), used = character(), values = list())
  if (is.name(expr)) {
    # the empty name of a missing argument, as in `x[, 1]`, names nothing
    found$used <- setdiff(as.character(expr), "")
    return(found)
  }
  if (!is.call(expr)) {
    found$values <- list(expr)
    return(found)
  }
  callee <- expr[[1L]]
  # unclass(): as.list() keeps a formula's class, and `[` has a method for it
  parts <- unclass(as.list(expr))[-1L]
  if (!is.name(callee)) {
    return(bind_names(lapply(c(list(callee), parts), read_names)))
  }
  called <- as.character(callee)
  if (called %in% c("::", ":::")) {
    # `pkg::name` takes its object from the namespace
    parts <- list()
  } else if (called %in% c("$", "@")) {
    # the element of `x$name` is not looked up
    parts <- parts[1L]
  }
  if (called == "function") {
    # its own arguments are not looked up where the function is made. A call
    # skips an argument that holds no function, so called names stay.
    arguments <- as.list(expr[[2L]])
    found <- bind_names(lapply(c(arguments, list(expr[[3L]])), read_names))
    found$used <- setdiff(found$used, names(arguments))
  } else {
    found <- bind_names(lapply(parts, read_names))
  }
  found$called <- union(called, found$called)
  found
}

#' Bind the names read from several expressions
#' @noRd
bind_names <- function(parts) {
  list(
    called = unique(as.character(unlist(lapply(parts, `[[`, "called")))),
    used = unique(as.character(unlist(lapply(parts, `[[`, "used")))),
    values = do.call(c, lapply(parts, `[[`, "values"))
  )
}

#' Read the strings a value holds, in lists too, that could name an object
#' @noRd
read_strings <- function(value) {
  if (is.list(value)) {
    return(unlist(lapply(value, read_strings)))
  }
  if (!is.character(value)) {
    return(character())
  }
  # R allows no empty name, and none longer than 10000 bytes
  value[
    !is.na(value) & nzchar(value) & nchar(value, type = "bytes") <= 10000L
  ]
}

#' Find the environment where a lookup from `env` finds `name`
#'
#' @param kind A mode of exists().
#' @noRd
find_binding <- function(name, env, kind = "any") {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, mode = kind, inherits = FALSE)) {
      return(env)
    }
    env <- parent.env(env)
  }
  NULL
}

#' Tell whether `name` in `env` is an argument left out with no default
#' @noRd
is_empty_argument <- function(name, env) {
  left_out <- tryCatch(
    eval(call("missing", as.name(name)), env),
    # missing() refuses a name that is no argument
    error = \(e) FALSE
  )
  # also TRUE for one left out with a default, which get() reads; get() fails
  # without one, also for one handed on, left out, from another frame
  if (!isTRUE(left_out)) {
    return(FALSE)
  }
  # a failed read of an argument handed on leaves it marked, and R warns at
  # the next read, as when the check runs again
  restarted <- gettext(
    "restarting interrupted promise evaluation",
    domain = "R"
  )
  value <- withCallingHandlers(
    tryCatch(get(name, envir = env, inherits = FALSE), error = identity),
    warning = function(w) {
      if (identical(conditionMessage(w), restarted)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  inherits(value, "error")
}

#' List the environments where a lookup ends alike on a socket worker
#' @noRd
list_safe_homes <- function() {
  packages <- c(list_default_packages(), "adatutor", list_learners())
  namespaces <- lapply(Filter(isNamespaceLoaded, packages), asNamespace)
  c(
    list(baseenv(), .BaseNamespaceEnv),
    read_default_homes(),
    namespaces,
    lapply(namespaces, parent.env)
  )
}

#' List the packages a new R process attaches, in their search order
#' @noRd
list_default_packages <- function() {
  c("stats", "graphics", "grDevices", "utils", "datasets", "methods")
}

#' Read the default packages' environments, if attached as in a new R process
#' @noRd
read_default_homes <- function() {
  packages <- list_default_packages()
  homes <- paste0("package:", packages)
  # this variable can give a worker other packages
  if (
    nzchar(Sys.getenv("R_DEFAULT_PACKAGES")) ||
      !identical(search()[search() %in% homes], homes)
  ) {
    return(list())
  }
  envs <- lapply(homes, as.environment)
  real <- vapply(
    seq_along(envs),
    \(i) identical(read_package(envs[[i]], attached = TRUE), packages[i]),
    logical(1)
  )
  if (!all(real)) {
    return(list())
  }
  envs
}

#' Read the package an environment belongs to, or NULL
#'
#' @param attached Whether `env` is on the search path: `FALSE` reads a
#'   namespace or its imports, `TRUE` a package attached from its namespace.
#' @noRd
read_package <- function(env, attached) {
  if (!attached && isNamespace(env)) {
    return(unname(getNamespaceName(env)))
  }
  label <- attr(env, "name", exact = TRUE)
  prefix <- if (attached) "package:" else "imports:"
  if (
    !is.character(label) || length(label) != 1L || !startsWith(label, prefix)
  ) {
    return(NULL)
  }
  package <- substring(label, nchar(prefix) + 1L)
  # attach() can give a list a package's name, but not its path
  if (
    attached &&
      !(isNamespaceLoaded(package) &&
        identical(
          attr(env, "path", exact = TRUE),
          getNamespaceInfo(package, "path")
        ))
  ) {
    return(NULL)
  }
  package
}

#' Tell whether a value holds data only, in its attributes too
#' @noRd
is_plain_value <- function(value) {
  if (is.null(value)) {
    return(TRUE)
  }
  # a function, a call, a formula or an environment is neither
  if (isS4(value) || !is.atomic(value) && !is.list(value)) {
    return(FALSE)
  }
  parts <- c(if (is.list(value)) unclass(value), attributes(value))
  all(vapply(parts, is_plain_value, logical(1)))
}

#' Tell whether a function is base's or from a namespace in `safe`
#' @noRd
is_safe_function <- function(value, safe) {
  is.primitive(value) ||
    is.function(value) &&
      isNamespace(environment(value)) &&
      any(vapply(safe, identical, logical(1), environment(value)))
}

#' Stop a socket run at what a refit uses from this session
#'
#' @param name The object's name, or `NULL` for code the formula or call
#'   holds itself.
#' @param place `"formula"` or `"call"`.
#' @param package The package the object comes from: attached here, or with
#'   `attached = FALSE`, a namespace the workers do not check.
#' @param quoted Whether a string names the object.
#' @noRd
stop_for_workers <- function(
  name,
  place,
  value = NULL,
  package = NULL,
  attached = TRUE,
  quoted = FALSE
) {
  way_out <- if (place == "formula") {
    "compute the column first and use it in the formula."
  } else {
    "compute the value first and use it in the call."
  }
  reason <- if (is.null(name)) {
    c(
      "The fit's ",
      place,
      " holds a function or other code itself, not by its name, as ",
      "do.call() puts it there. On Windows, parallel workers start empty and ",
      "may not get what that code uses. Set `cores = 1`, or fit the model ",
      "with a call that names its functions."
    )
  } else if (!is.null(package) && attached) {
    c(
      "The fit uses `",
      name,
      "` from the attached package ",
      package,
      ". On Windows, parallel workers load packages but do not attach them. ",
      "Set `cores = 1`, or write `",
      package,
      "::",
      name,
      if (is.function(value)) "()",
      "`."
    )
  } else if (!is.null(package)) {
    c(
      "The fit uses `",
      name,
      "` from the package ",
      package,
      ". On Windows, parallel workers may not load ",
      package,
      " as this session did. Set `cores = 1`."
    )
  } else if (quoted && is.function(value)) {
    c(
      "The fit names `",
      name,
      "` in a string, and this session has a function of that name. On ",
      "Windows, parallel workers start empty and may not get it. Set ",
      "`cores = 1`, or rename the function in this session."
    )
  } else if (quoted) {
    c(
      "The fit names `",
      name,
      "` in a string, and this session has an object of that name that ",
      "holds code. On Windows, parallel workers start empty and may not get ",
      "that code. Set `cores = 1`, or rename the object in this session."
    )
  } else if (is.function(value)) {
    c(
      "The fit uses `",
      name,
      "` from this session. On Windows, parallel workers start empty and ",
      "may not get it. Set `cores = 1`, or ",
      way_out
    )
  } else {
    c(
      "The fit uses `",
      name,
      "` from this session, an object that holds code. On Windows, parallel ",
      "workers start empty and may not get that code. Set `cores = 1`, or ",
      way_out
    )
  }
  stop(paste(reason, collapse = ""), call. = FALSE)
}
