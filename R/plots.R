#' Visualize importance scores
#' @param object A tibble of results from [importance_perm()].
#' @param metric A character vector or `NULL` for which metric to plot. By
#' default, all metrics will be shown via facets. Possible options are
#' the entries in `.metric` column of the object.
#' @param eval_time For censored regression models, a vector of time points at
#' which the survival probability is estimated.
#' @param top An integer for how many terms to show. The rankings of predictors
#' are computed across metrics.
#' @param type A character value. The default is `"importance"` which shows the
#' overall signal-to-noise ration (i.e., mean divided by standard error).
#' Alternatively, `"direction"` shows the mean difference value with standard
#' error bounds.
#' @param ... Not used.
#' @return A `ggplot2` object.
#' @export
autoplot.importance_perm <- function(object, top = Inf, metric = NULL,
                                     eval_time = NULL, type = "importance", ...) {
  type <- rlang::arg_match(type, values = c("importance", "difference"))
  if (!is.null(metric)) {
    object <- object[object$.metric %in% metric,]
    if (nrow(object) == 0) {
      cli::cli_abort("No data left when filtering over {.val {metric}}.")
    }
  }
  overall_rank <-
    object |>
    dplyr::mutate(ranking = rank(-importance)) |>
    dplyr::summarize(
      ranking = mean(ranking),
      .by = c(predictor)
    ) |>
    dplyr::arrange(ranking)

  num_pred <- vctrs::vec_unique_count(object$predictor)
  if (top < num_pred) {
    overall_rank <- overall_rank[overall_rank$ranking <= top, ]
    object <- object[object$predictor %in% unique(overall_rank$predictor), ]
  }
  object$predictor <- factor(object$predictor, levels = rev(overall_rank$predictor))

  p <-
    ggplot2::ggplot(object, ggplot2::aes(y = predictor)) +
    ggplot2::geom_vline(xintercept = 0, col = "red", lty = 2) +
    ggplot2::labs(y = NULL, x = "Permutation Importance Score")
  if (length(unique(object$.metric)) > 1) {
    p <- p + ggplot2::facet_wrap(~ .metric)
  }
  if (type == "importance") {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = importance))
  } else if (type == "difference") {
    # TODO add alpha level
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = mean)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = mean - 1.96 * std_err, xmax = mean + 1.96 * std_err),
        width = 0)
  }
  p
}

predictions <- function(wflow, new_data, type, eval_time) {
  if (type == "original") {
    preds <- augment(wflow, new_data = new_data, eval_time = eval_time)
  } else {
    preds <-
      wflow |>
      extract_fit_parsnip() |>
      augment(new_data = new_data, eval_time = eval_time)
    use_post <- has_postprocessor(wflow)
    if (use_post) {
      post_proc <- extract_postprocessor(wflow)
      preds <- predict(post_proc, preds)
    }
  }
  preds
}
