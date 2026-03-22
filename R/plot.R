#' @title Plot spatial outputs from a solution or solution set
#'
#' @description
#' Plot spatial information from a `Solution` object or from one or more runs of a
#' `SolutionSet` object.
#'
#' Supported views are:
#' \itemize{
#'   \item \code{"pu"}: selected planning units.
#'   \item \code{"actions"}: selected actions in space.
#'   \item \code{"features"}: spatial distribution of features using baseline,
#'   benefit, or final values.
#' }
#'
#' @param x A `Solution` or `SolutionSet` object.
#' @param what Character. One of `"pu"`, `"actions"`, or `"features"`.
#' @param runs Optional integer vector of run ids. If `NULL`, a `Solution` is used
#'   directly, and for a `SolutionSet` the first run is used.
#' @param subset Optional character vector used to filter actions or features.
#' @param value Character. Only used when `what = "features"`. One of
#'   `"final"`, `"baseline"`, or `"benefit"`.
#' @param layout Character. One of `"single"` or `"facet"`. For actions, the
#'   default is `"single"`. For features, the default is `"facet"`.
#' @param max_facets Maximum number of facets shown when `subset = NULL`.
#' @param only_selected Logical. Only used when `what = "pu"`.
#' @param ... Reserved for future extensions.
#' @param base_alpha Numeric in `[0,1]`. Alpha used for the base PU layer.
#' @param selected_alpha Numeric in `[0,1]`. Alpha used for highlighted layers.
#' @param base_fill Fill color for the base PU layer.
#' @param base_color Border color for the base PU layer.
#' @param selected_color Border color for highlighted layers.
#' @param draw_borders Logical. If `FALSE`, borders are not drawn. This is faster
#'   for large spatial datasets.
#' @param show_base Logical. If `TRUE`, draw the base PU layer underneath the
#'   highlighted layer.
#' @param fill_values Optional named vector of colors for discrete maps.
#' @param fill_na Fill color for missing values.
#' @param use_viridis Logical.
#'
#' @return Invisibly returns a `ggplot` object.
#' @export
plot_spatial <- function(
    x,
    what = c("pu", "actions", "features"),
    runs = NULL,
    subset = NULL,
    value = c("final", "baseline", "benefit"),
    layout = NULL,
    max_facets = 4L,
    only_selected = FALSE,
    ...,
    base_alpha = 0.10,
    selected_alpha = 0.90,
    base_fill = "grey92",
    base_color = NA,
    selected_color = NA,
    draw_borders = FALSE,
    show_base = TRUE,
    fill_values = NULL,
    fill_na = "grey80",
    use_viridis = TRUE
) {
  what <- match.arg(what)
  value <- match.arg(value)

  if (is.null(layout)) {
    layout <- if (identical(what, "features")) "facet" else "single"
  }
  layout <- match.arg(layout, c("single", "facet"))

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("plot_spatial() requires the 'sf' package.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_spatial() requires the 'ggplot2' package.", call. = FALSE)
  }
  has_viridis <- requireNamespace("viridis", quietly = TRUE)

  if (!isTRUE(draw_borders)) {
    base_color <- NA
    selected_color <- NA
  }

  # -------------------------------------------------------------------
  # resolve runs / solutions
  # -------------------------------------------------------------------
  if (inherits(x, "Solution")) {
    if (!is.null(runs)) {
      runs <- as.integer(runs)
      if (length(runs) != 1L || is.na(runs[1]) || runs[1] != 1L) {
        stop("For a Solution object, `runs` must be NULL or 1.", call. = FALSE)
      }
    }
    runs <- 1L
    solutions <- list(x)
    names(solutions) <- "1"
  } else if (inherits(x, "SolutionSet")) {
    all_solutions <- x$solution$solutions %||% NULL
    if (is.null(all_solutions) || !is.list(all_solutions) || length(all_solutions) == 0L) {
      stop("SolutionSet has no solutions in x$solution$solutions.", call. = FALSE)
    }

    if (is.null(runs)) {
      runs <- 1L
    } else {
      runs <- as.integer(runs)
      if (any(!is.finite(runs)) || any(is.na(runs)) || any(runs < 1L)) {
        stop("`runs` must contain positive integers.", call. = FALSE)
      }
      if (any(runs > length(all_solutions))) {
        stop(
          "`runs` contains values larger than the number of available solutions (",
          length(all_solutions), ").",
          call. = FALSE
        )
      }
      runs <- unique(runs)
    }

    solutions <- all_solutions[runs]
    names(solutions) <- as.character(runs)
  } else {
    stop("x must be a Solution or SolutionSet.", call. = FALSE)
  }

  multi_runs <- length(solutions) > 1L

  # restrictions for multi-run plotting
  if (isTRUE(multi_runs) && identical(what, "actions") && identical(layout, "facet")) {
    stop(
      "When plotting multiple runs for `what = 'actions'`, use layout = 'single'. ",
      "Faceting by both run and action is not supported.",
      call. = FALSE
    )
  }

  if (isTRUE(multi_runs) && identical(what, "features")) {
    if (is.null(subset) || length(subset) != 1L) {
      stop(
        "When plotting multiple runs for `what = 'features'`, `subset` must specify exactly one feature.",
        call. = FALSE
      )
    }
  }

  # use first solution to obtain shared geometry/problem
  sol0 <- solutions[[1]]
  pr <- sol0$problem %||% NULL
  if (is.null(pr) || !inherits(pr, "Problem")) {
    stop("Solution does not contain a valid problem object.", call. = FALSE)
  }

  pu_sf <- pr$data$pu_sf %||% NULL
  if (is.null(pu_sf) || !inherits(pu_sf, "sf")) {
    stop("No PU geometry found in solution$problem$data$pu_sf.", call. = FALSE)
  }
  if (!("id" %in% names(pu_sf))) {
    stop("PU geometry must contain an 'id' column.", call. = FALSE)
  }
  pu_sf$id <- as.integer(pu_sf$id)
  pu_sf_min <- pu_sf[, "id", drop = FALSE]

  make_base_plot <- function() {
    p <- ggplot2::ggplot()
    if (isTRUE(show_base)) {
      p <- p +
        ggplot2::geom_sf(
          data = pu_sf_min,
          fill = base_fill,
          color = base_color,
          alpha = base_alpha
        )
    }
    p + ggplot2::theme_minimal()
  }

  # -------------------------------------------------------------------
  # PU
  # -------------------------------------------------------------------
  if (identical(what, "pu")) {
    pu_list <- vector("list", length(solutions))

    for (i in seq_along(solutions)) {
      sol_i <- solutions[[i]]
      run_i <- as.integer(names(solutions)[i])

      pu_tbl <- get_pu(sol_i, only_selected = FALSE)
      if (!all(c("id", "selected") %in% names(pu_tbl))) {
        stop("PU summary must contain 'id' and 'selected'.", call. = FALSE)
      }

      pu_tbl$id <- as.integer(pu_tbl$id)
      g_i <- merge(pu_sf_min, pu_tbl[, c("id", "selected")], by = "id", all.x = TRUE)
      g_i$selected[is.na(g_i$selected)] <- 0L
      g_i$run_id <- run_i
      pu_list[[i]] <- g_i
    }

    g <- do.call(rbind, pu_list)
    g <- sf::st_as_sf(g)
    g_sel <- g[g$selected %in% 1L, , drop = FALSE]

    if (isTRUE(only_selected) && nrow(g_sel) == 0L) {
      stop("No selected PUs to plot.", call. = FALSE)
    }

    p <- make_base_plot() +
      ggplot2::geom_sf(
        data = g_sel,
        fill = "#2C7FB8",
        color = selected_color,
        alpha = selected_alpha
      ) +
      ggplot2::labs(title = "Selected planning units")

    if (isTRUE(multi_runs)) {
      p <- p + ggplot2::facet_wrap(~run_id)
    }

    print(p)
    return(invisible(p))
  }

  # -------------------------------------------------------------------
  # ACTIONS
  # -------------------------------------------------------------------
  if (identical(what, "actions")) {
    act_list <- vector("list", length(solutions))

    for (i in seq_along(solutions)) {
      sol_i <- solutions[[i]]
      run_i <- as.integer(names(solutions)[i])

      act_tbl <- get_actions(sol_i, only_selected = TRUE)

      if (!all(c("pu", "action", "selected") %in% names(act_tbl))) {
        stop("Action summary must contain 'pu', 'action', and 'selected'.", call. = FALSE)
      }

      act_tbl$pu <- as.integer(act_tbl$pu)
      act_tbl$action <- as.character(act_tbl$action)

      if (!is.null(subset)) {
        keep <- .pa_resolve_action_subset(sol_i$problem, subset = subset)
        keep_ids <- as.character(keep$id)
        act_tbl <- act_tbl[act_tbl$action %in% keep_ids, , drop = FALSE]
      }

      if (nrow(act_tbl) == 0L) next

      act_tbl$run_id <- run_i
      act_list[[i]] <- act_tbl
    }

    act_list <- Filter(Negate(is.null), act_list)
    if (length(act_list) == 0L) {
      stop("No selected actions to plot.", call. = FALSE)
    }

    act_tbl <- do.call(rbind, act_list)
    acts <- unique(act_tbl$action)

    if (!isTRUE(multi_runs) && identical(layout, "facet")) {
      if (is.null(subset) && length(acts) > max_facets) {
        warning(
          "Showing only the first ", max_facets,
          " actions. Use subset=... or increase max_facets.",
          call. = FALSE
        )
        keep_acts <- acts[seq_len(max_facets)]
        act_tbl <- act_tbl[act_tbl$action %in% keep_acts, , drop = FALSE]
      }

      names(act_tbl)[names(act_tbl) == "pu"] <- "id"
      g <- merge(pu_sf_min, act_tbl[, c("id", "action")], by = "id", all.x = FALSE)
      g <- sf::st_as_sf(g)

      p <- make_base_plot() +
        ggplot2::geom_sf(
          data = g,
          ggplot2::aes(fill = action),
          color = selected_color,
          alpha = selected_alpha
        ) +
        ggplot2::facet_wrap(~action) +
        ggplot2::labs(title = "Selected actions", fill = "")

      if (!is.null(fill_values)) {
        p <- p + ggplot2::scale_fill_manual(values = fill_values, na.value = fill_na)
      } else if (isTRUE(use_viridis) && has_viridis) {
        p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
      }

      print(p)
      return(invisible(p))
    }

    # single layout (also used for multi-run)
    lab_list <- split(act_tbl, act_tbl$run_id)

    lab_out <- lapply(names(lab_list), function(rr) {
      dd <- lab_list[[rr]]
      tmp <- stats::aggregate(
        action ~ pu,
        data = dd,
        FUN = function(z) {
          z <- unique(as.character(z))
          if (length(z) > 1L) {
            warning(
              "More than one action detected in at least one PU. Labels were collapsed using '+'.",
              call. = FALSE
            )
          }
          paste(sort(z), collapse = "+")
        }
      )
      names(tmp)[names(tmp) == "pu"] <- "id"
      tmp$id <- as.integer(tmp$id)
      tmp$action <- as.character(tmp$action)
      tmp$run_id <- as.integer(rr)
      tmp
    })

    lab_act <- do.call(rbind, lab_out)
    g <- merge(pu_sf_min, lab_act, by = "id", all.x = FALSE)
    g <- sf::st_as_sf(g)

    if (nrow(g) == 0L) {
      stop("No geometry matched the plotted action labels.", call. = FALSE)
    }

    p <- make_base_plot() +
      ggplot2::geom_sf(
        data = g,
        ggplot2::aes(fill = action),
        color = selected_color,
        alpha = selected_alpha
      ) +
      ggplot2::labs(title = "Selected actions", fill = "")

    if (isTRUE(multi_runs)) {
      p <- p + ggplot2::facet_wrap(~run_id)
    }

    if (!is.null(fill_values)) {
      p <- p + ggplot2::scale_fill_manual(values = fill_values, na.value = fill_na)
    } else if (isTRUE(use_viridis) && has_viridis) {
      p <- p + viridis::scale_fill_viridis(discrete = TRUE, option = "C")
    }

    print(p)
    return(invisible(p))
  }

  # -------------------------------------------------------------------
  # FEATURES
  # -------------------------------------------------------------------
  distf <- pr$data$dist_features %||% NULL
  feats <- pr$data$features %||% NULL

  if (is.null(distf) || !inherits(distf, "data.frame")) {
    stop("Missing problem$data$dist_features.", call. = FALSE)
  }
  if (is.null(feats) || !inherits(feats, "data.frame")) {
    stop("Missing problem$data$features.", call. = FALSE)
  }
  if (!all(c("pu", "feature", "amount") %in% names(distf))) {
    stop("dist_features must contain 'pu', 'feature', and 'amount'.", call. = FALSE)
  }

  feat_map <- feats[, intersect(c("id", "name"), names(feats)), drop = FALSE]
  if (!("name" %in% names(feat_map))) feat_map$name <- as.character(feat_map$id)

  feature_frames <- vector("list", length(solutions))

  for (i in seq_along(solutions)) {
    sol_i <- solutions[[i]]
    run_i <- as.integer(names(solutions)[i])

    base_tbl <- stats::aggregate(
      amount ~ pu + feature,
      data = distf,
      FUN = sum
    )
    names(base_tbl)[names(base_tbl) == "amount"] <- "baseline"

    eff <- sol_i$problem$data$dist_effects_model %||% sol_i$problem$data$dist_effects %||% NULL
    ben_tbl <- NULL

    if (!is.null(eff) &&
        inherits(eff, "data.frame") &&
        all(c("pu", "feature", "action", "benefit") %in% names(eff))) {

      act_sel <- get_actions(sol_i, only_selected = TRUE)

      if (all(c("pu", "action") %in% names(act_sel))) {
        key_sel <- paste(act_sel$pu, act_sel$action, sep = "||")
        key_eff <- paste(eff$pu, eff$action, sep = "||")

        eff2 <- eff[key_eff %in% key_sel, , drop = FALSE]

        if (nrow(eff2) > 0L) {
          ben_tbl <- stats::aggregate(
            benefit ~ pu + feature,
            data = eff2,
            FUN = sum
          )
          names(ben_tbl)[names(ben_tbl) == "benefit"] <- "benefit"
        }
      }
    }

    if (is.null(ben_tbl)) {
      ben_tbl <- data.frame(
        pu = integer(0),
        feature = numeric(0),
        benefit = numeric(0)
      )
    }

    ff <- merge(base_tbl, ben_tbl, by = c("pu", "feature"), all = TRUE)
    ff$baseline[is.na(ff$baseline)] <- 0
    ff$benefit[is.na(ff$benefit)] <- 0
    ff$final <- ff$baseline + ff$benefit

    ff <- merge(ff, feat_map, by.x = "feature", by.y = "id", all.x = TRUE)
    ff$feature_label <- as.character(ff$name)
    ff$run_id <- run_i

    if (!is.null(subset)) {
      subset_chr <- as.character(subset)
      ff <- ff[ff$feature_label %in% subset_chr | as.character(ff$feature) %in% subset_chr, , drop = FALSE]
    }

    feature_frames[[i]] <- ff
  }

  ff <- do.call(rbind, feature_frames)

  if (nrow(ff) == 0L) {
    stop("No features available to plot for the requested subset.", call. = FALSE)
  }

  if (!isTRUE(multi_runs)) {
    feat_levels <- unique(ff$feature_label)
    if (identical(layout, "facet") && is.null(subset) && length(feat_levels) > max_facets) {
      warning(
        "Showing only the first ", max_facets,
        " features. Use subset=... or increase max_facets.",
        call. = FALSE
      )
      keep_feats <- feat_levels[seq_len(max_facets)]
      ff <- ff[ff$feature_label %in% keep_feats, , drop = FALSE]
    }
  }

  names(ff)[names(ff) == "pu"] <- "id"
  g <- merge(pu_sf_min, ff[, c("id", "feature_label", "run_id", value)], by = "id", all.y = TRUE)
  g <- sf::st_as_sf(g)

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = g,
      ggplot2::aes(fill = .data[[value]]),
      color = selected_color,
      alpha = 1
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Spatial", value, "by feature"), fill = value)

  if (isTRUE(multi_runs)) {
    p <- p + ggplot2::facet_wrap(~run_id)
  } else if (identical(layout, "facet")) {
    p <- p + ggplot2::facet_wrap(~feature_label)
  }

  if (isTRUE(use_viridis) && has_viridis) {
    p <- p + viridis::scale_fill_viridis(option = "C", na.value = fill_na)
  }

  print(p)
  invisible(p)
}


#' @title Plot trade-offs from a multi-objective solution set
#'
#' @description
#' Plot pairwise trade-offs among objective values stored in a `SolutionSet`.
#'
#' If exactly two objectives are selected, the function returns a single scatterplot.
#' If three or more objectives are selected, it returns all pairwise combinations
#' using facets.
#'
#' @param x A `SolutionSet` object.
#' @param objectives Optional character vector of objective aliases to display.
#'   These should match the suffixes of the `value_*` columns in `x$solution$runs`.
#'   If `NULL`, all available objectives are used.
#' @param color_by Optional character scalar used to color points. It may refer to
#'   one of the selected objective aliases (e.g. `"benefit"`) or to one of the
#'   run-level columns `"run_id"`, `"status"`, `"runtime"`, or `"gap"`.
#' @param all_pairs Logical. If `TRUE`, allow plotting all pairwise combinations
#'   even when more than four objectives are present. If `NULL`, it is treated as
#'   `FALSE`.
#' @param connect Logical. If `TRUE`, connect points by `run_id` within each panel.
#' @param label_runs Logical. If `TRUE`, add run labels to points.
#' @param point_size Numeric point size.
#' @param line_alpha Numeric alpha for connecting lines.
#' @param text_size Numeric size for run labels.
#' @param ... Reserved for future extensions.
#'
#' @return Invisibly returns a `ggplot` object.
#' @export
plot_tradeoff <- function(
    x,
    objectives = NULL,
    color_by = NULL,
    all_pairs = NULL,
    connect = FALSE,
    label_runs = FALSE,
    point_size = 2.5,
    line_alpha = 0.6,
    text_size = 3,
    ...
) {
  if (!inherits(x, "SolutionSet")) {
    stop("plot_tradeoff() requires a SolutionSet object.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot_tradeoff() requires the 'ggplot2' package.", call. = FALSE)
  }

  runs <- x$solution$runs %||% NULL
  if (is.null(runs) || !inherits(runs, "data.frame") || nrow(runs) == 0L) {
    stop("No run table found in x$solution$runs.", call. = FALSE)
  }

  value_cols <- grep("^value_", names(runs), value = TRUE)
  if (length(value_cols) < 2L) {
    stop("At least two objective value columns ('value_*') are required.", call. = FALSE)
  }

  available_obj <- sub("^value_", "", value_cols)

  if (is.null(objectives)) {
    objectives <- available_obj
  } else {
    objectives <- as.character(objectives)
    objectives <- unique(objectives[!is.na(objectives) & nzchar(objectives)])
    if (length(objectives) < 2L) {
      stop("`objectives` must contain at least two objective names.", call. = FALSE)
    }
    bad <- setdiff(objectives, available_obj)
    if (length(bad) > 0L) {
      stop(
        "Unknown objective(s): ", paste(bad, collapse = ", "),
        ". Available objectives are: ", paste(available_obj, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  all_pairs <- isTRUE(all_pairs)

  if (length(objectives) > 4L && !all_pairs) {
    stop(
      "More than four objectives were selected (", length(objectives), "). ",
      "Please specify a smaller set in `objectives = ...` or use `all_pairs = TRUE`.",
      call. = FALSE
    )
  }

  if (!is.null(color_by)) {
    color_by <- as.character(color_by)[1]
    if (is.na(color_by) || !nzchar(color_by)) {
      stop("`color_by` must be a non-empty string or NULL.", call. = FALSE)
    }

    valid_color_vars <- c(objectives, "run_id", "status", "runtime", "gap")
    if (!color_by %in% valid_color_vars) {
      stop(
        "`color_by` must be one of: ",
        paste(valid_color_vars, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------
  # helper to build pairwise plotting data
  # -------------------------------------------------------------------
  pair_mat <- utils::combn(objectives, 2)
  pair_df <- vector("list", ncol(pair_mat))

  for (i in seq_len(ncol(pair_mat))) {
    ox <- pair_mat[1, i]
    oy <- pair_mat[2, i]

    cx <- paste0("value_", ox)
    cy <- paste0("value_", oy)

    dd <- data.frame(
      run_id = runs$run_id %||% seq_len(nrow(runs)),
      status = if ("status" %in% names(runs)) runs$status else NA_character_,
      runtime = if ("runtime" %in% names(runs)) runs$runtime else NA_real_,
      gap = if ("gap" %in% names(runs)) runs$gap else NA_real_,
      obj_x = ox,
      obj_y = oy,
      x = as.numeric(runs[[cx]]),
      y = as.numeric(runs[[cy]]),
      pair = paste0(ox, " vs ", oy),
      stringsAsFactors = FALSE
    )

    if (!is.null(color_by)) {
      if (color_by %in% objectives) {
        dd$color_value <- runs[[paste0("value_", color_by)]]
      } else {
        dd$color_value <- runs[[color_by]]
      }
    }

    pair_df[[i]] <- dd
  }

  plot_df <- do.call(rbind, pair_df)

  # -------------------------------------------------------------------
  # single pair
  # -------------------------------------------------------------------
  if (ncol(pair_mat) == 1L) {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y))

    if (isTRUE(connect)) {
      p <- p + ggplot2::geom_line(
        ggplot2::aes(group = 1),
        alpha = line_alpha
      )
    }

    if (is.null(color_by)) {
      p <- p + ggplot2::geom_point(size = point_size)
    } else {
      p <- p + ggplot2::geom_point(
        ggplot2::aes(color = color_value),
        size = point_size
      )
    }

    if (isTRUE(label_runs)) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_text_repel(
          ggplot2::aes(label = run_id),
          size = text_size
        )
      } else {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = run_id),
          size = text_size,
          vjust = -0.5
        )
      }
    }

    p <- p +
      ggplot2::labs(
        x = unique(plot_df$obj_x),
        y = unique(plot_df$obj_y),
        color = if (!is.null(color_by)) color_by else NULL
      ) +
      ggplot2::theme_minimal()

    print(p)
    return(invisible(p))
  }

  # -------------------------------------------------------------------
  # multiple pairs
  # -------------------------------------------------------------------
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y))

  if (isTRUE(connect)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(group = 1),
      alpha = line_alpha
    )
  }

  if (is.null(color_by)) {
    p <- p + ggplot2::geom_point(size = point_size)
  } else {
    p <- p + ggplot2::geom_point(
      ggplot2::aes(color = color_value),
      size = point_size
    )
  }

  if (isTRUE(label_runs)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        ggplot2::aes(label = run_id),
        size = text_size,
        show.legend = FALSE
      )
    } else {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = run_id),
        size = text_size,
        vjust = -0.5,
        show.legend = FALSE
      )
    }
  }

  p <- p +
    ggplot2::facet_wrap(~pair, scales = "free") +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      color = if (!is.null(color_by)) color_by else NULL
    ) +
    ggplot2::theme_minimal()

  print(p)
  invisible(p)
}
