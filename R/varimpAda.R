#' @title Calculate Feature Importance for Custom AdaBoost
#'
#' @description Extracts the relative importance of the predictor variables from
#'   an ensemble fitted with \code{\link[adatutor]{trainAda}}. Each tree
#'   contributes its model weight \eqn{a_t}, distributed across the variables it
#'   splits on in proportion to the improvement each split achieves. A decision
#'   stump splits once, so it passes its whole weight \eqn{a_t} to that single
#'   variable; deeper trees spread it over every splitting variable rather than
#'   only the one at the root.
#'
#' @param fit A trained AdaBoost model from \code{\link[adatutor]{trainAda}}:
#'   a list of trees and their corresponding model weights.
#'
#' @return A sorted data frame of variables and their relative importance (%).
#'
#' @export
varimpAda <- function(fit) {
  # Initialize an empty list to store our weight tallies
  var_wght <- list()

  # Loop through every single tree in the model
  for (t in seq_along(fit)) {
    tree <- fit[[t]]$h
    wght <- fit[[t]]$a

    # rpart records an improvement-based importance for every variable the tree
    # splits on. It is NULL for a tree that never split (a bare leaf) and has a
    # single entry for a stump.
    # TODO: Input check for rpart object
    imp <- tree$variable.importance

    # Safety check: ensure the tree actually made a split
    if (is.null(imp) || sum(imp) == 0) next

    # Distribute this tree's model weight across the variables it split on. A
    # stump has one entry, so it receives the full weight.
    share <- wght * imp / sum(imp)

    for (v in names(share)) {
      if (is.null(var_wght[[v]])) {
        var_wght[[v]] <- share[[v]]
      } else {
        var_wght[[v]] <- var_wght[[v]] + share[[v]]
      }
    }
  }

  # No tree in the ensemble split on anything
  if (length(var_wght) == 0) {
    return(data.frame(variable = character(0), importance = numeric(0),
                      stringsAsFactors = FALSE))
  }

  # Convert tallied list into 'clean' data frame
  imp_df <- data.frame(
    variable = names(var_wght),
    importance = unlist(var_wght),
    stringsAsFactors = FALSE
  )

  # normalize to [0,1] and scale to 100%: easier to read
  imp_df$importance <- (imp_df$importance / sum(imp_df$importance)) * 100

  # sort from most important to least important
  imp_df <- imp_df[order(-imp_df$importance), ]
  rownames(imp_df) <- NULL

  return(imp_df)
}
