#' @title qgraph-Compatible Network Plotting
#' @description Network visualization that exactly replicates qgraph's visual logic.
#' @name sonplot
NULL

#' Plot Network with qgraph-Compatible Visuals
#'
#' Creates a network visualization using base R graphics with visual output
#' that exactly replicates qgraph's appearance. Uses qgraph's formulas for
#' node sizing, edge width scaling, and boundary calculations while maintaining
#' sonnet's snake_case parameter naming convention.
#'
#' @param x Network input. Can be:
#'   - A square numeric matrix (adjacency/weight matrix)
#'   - A data frame with edge list (from, to, optional weight columns)
#'   - An igraph object
#'   - A sonnet_network object
#' @param layout Layout algorithm: "circle", "spring", "groups", or a matrix
#'   of x,y coordinates, or an igraph layout function. Also supports igraph
#'   two-letter codes: "kk", "fr", "drl", "mds", "ni", etc.
#' @param directed Logical. Force directed interpretation. NULL for auto-detect.
#' @param seed Random seed for deterministic layouts. Default 42.
#'
#' @section Node Aesthetics:
#' @param node_size Node size(s). NULL uses qgraph adaptive formula: 8*exp(-n/80)+1.
#'   Single value or vector.
#' @param node_size2 Secondary node size for ellipse/rectangle height.
#' @param node_shape Node shape(s): "circle", "square", "triangle", "diamond",
#'   "pentagon", "hexagon", "star", "heart", "ellipse", "cross", or any custom
#'   SVG shape registered with register_svg_shape().
#' @param node_fill Node fill color(s).
#' @param node_border_color Node border color(s).
#' @param node_border_width Node border width(s).
#' @param node_alpha Node transparency (0-1). Default 1.
#' @param labels Node labels: TRUE (use node names/indices), FALSE (none),
#'   or character vector.
#' @param label_size Label character expansion factor.
#' @param label_color Label text color.
#'
#' @section Pie/Donut Nodes:
#' @param pie_values List of numeric vectors for pie chart nodes.
#' @param donut_fill Numeric value (0-1) for donut fill proportion.
#' @param donut_values Deprecated. Use donut_fill.
#' @param donut_inner_ratio Inner radius ratio for donut (0-1). Default 0.5.
#'
#' @section Edge Aesthetics:
#' @param edge_color Edge color(s). If NULL, uses positive_color/negative_color.
#' @param edge_width Edge width(s). If NULL, scales by weight using qgraph formula.
#' @param esize Base edge size for weight scaling. NULL uses qgraph adaptive formula:
#'   15*exp(-n/90)+1 (halved for directed networks).
#' @param cut qgraph-style cutoff: 0 = continuous scaling (default), positive = threshold.
#' @param minimum Minimum weight threshold. Edges below this are hidden.
#' @param maximum Maximum weight for scaling. NULL for auto.
#' @param edge_alpha Edge transparency (0-1). Default 0.8.
#' @param curvature Edge curvature. 0 for straight, positive/negative for curves.
#' @param curves Curve mode: TRUE = reciprocal edges curved, FALSE = all straight,
#'   "force" = all edges curved.
#' @param arrow_size Arrow head size.
#' @param show_arrows Logical or vector: show arrows on directed edges?
#' @param positive_color Color for positive weights.
#' @param negative_color Color for negative weights.
#'
#' @section Edge Labels:
#' @param edge_labels Edge labels: TRUE (show weights), FALSE (none), or character vector.
#' @param edge_label_size Edge label size.
#' @param edge_label_position Position along edge (0=start, 0.5=middle, 1=end). Default 0.5.
#'
#' @section Plot Settings:
#' @param title Plot title.
#' @param margins Margins as c(bottom, left, top, right).
#' @param background Background color.
#' @param rescale Logical: rescale layout to [-1, 1]?
#' @param aspect Logical: maintain aspect ratio?
#'
#' @section Output:
#' @param filetype Output format: "default" (screen), "png", "pdf", "svg", "jpeg", "tiff".
#' @param filename Output filename (without extension).
#' @param width Output width in inches.
#' @param height Output height in inches.
#' @param res Resolution in DPI for raster outputs. Default 600.
#' @param ... Additional arguments passed to layout functions.
#'
#' @return Invisibly returns the sonnet_network object.
#'
#' @export
#'
#' @examples
#' # Basic network from adjacency matrix
#' adj <- matrix(c(0, 1, 1, 0,
#'                 0, 0, 1, 1,
#'                 0, 0, 0, 1,
#'                 0, 0, 0, 0), 4, 4, byrow = TRUE)
#' sonplot(adj)
#'
#' # Weighted network with automatic sizing
#' w_adj <- matrix(c(0, 0.5, -0.3, 0,
#'                   0.8, 0, 0.4, -0.2,
#'                   0, 0, 0, 0.6,
#'                   0, 0, 0, 0), 4, 4, byrow = TRUE)
#' sonplot(w_adj)
#'
#' # Compare node sizes with qgraph
#' # For n=4 nodes: 8*exp(-4/80)+1 = 8.61
#' # sonplot uses same formula, so visuals should match qgraph
#'
sonplot <- function(
    x,
    layout = "spring",
    directed = NULL,
    seed = 42,

    # Node aesthetics (sonnet syntax)
    node_size = NULL,
    node_size2 = NULL,
    node_shape = "circle",
    node_fill = NULL,
    node_border_color = NULL,
    node_border_width = 1,
    node_alpha = 1,
    labels = TRUE,
    label_size = NULL,
    label_color = "black",

    # Pie/Donut
    pie_values = NULL,
    pie_colors = NULL,
    donut_fill = NULL,
    donut_values = NULL,
    donut_color = NULL,
    donut_inner_ratio = 0.5,
    donut_bg_color = "gray90",

    # Edge aesthetics (sonnet syntax, qgraph behavior)
    edge_color = NULL,
    edge_width = NULL,
    esize = NULL,
    cut = 0,
    minimum = 0,
    maximum = NULL,
    edge_alpha = 0.8,
    edge_labels = FALSE,
    edge_label_size = 0.8,
    edge_label_color = "gray30",
    edge_label_bg = "white",
    edge_label_position = 0.5,
    edge_style = 1,
    curvature = 0,
    curve_scale = TRUE,
    curve_shape = 0,
    curve_pivot = 0.5,
    curves = TRUE,
    arrow_size = 1,
    show_arrows = TRUE,
    bidirectional = FALSE,
    loop_rotation = NULL,

    # Weight handling
    threshold = 0,
    positive_color = "#2E7D32",
    negative_color = "#C62828",

    # Plot settings
    title = NULL,
    title_size = 1.2,
    margins = c(0.1, 0.1, 0.1, 0.1),
    background = "white",
    rescale = TRUE,
    layout_margin = 0.15,
    aspect = TRUE,
    usePCH = FALSE,

    # Legend
    legend = FALSE,
    legend_position = "topright",
    legend_size = 0.8,
    legend_edge_colors = TRUE,
    groups = NULL,

    # Output
    filetype = "default",
    filename = "sonplot",
    width = 7,
    height = 7,
    res = 600,
    ...
) {

  # ============================================
  # 1. INPUT PROCESSING
  # ============================================

  # Set seed for deterministic layouts
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Convert to sonnet_network if needed
  network <- ensure_sonnet_network(x, layout = layout, seed = seed, ...)

  nodes <- network$network$get_nodes()
  edges <- network$network$get_edges()
  layout_coords <- network$network$get_layout()

  n_nodes <- nrow(nodes)
  n_edges <- if (!is.null(edges)) nrow(edges) else 0

  # Determine if directed
  if (is.null(directed)) {
    directed <- network$network$is_directed
  }

  # Determine if weighted
  weighted <- !is.null(edges) && "weight" %in% names(edges) &&
              any(edges$weight != 1, na.rm = TRUE)

  # ============================================
  # 2. LAYOUT HANDLING
  # ============================================

  if (is.null(layout_coords)) {
    stop("Layout coordinates not available", call. = FALSE)
  }

  layout_mat <- as.matrix(layout_coords[, c("x", "y")])

  # Rescale to [-1, 1]
  if (rescale) {
    layout_mat <- as.matrix(rescale_layout(layout_mat, mar = 0.1))
  }

  # ============================================
  # 3. QGRAPH-STYLE NODE SIZING
  # ============================================

  # Use qgraph adaptive formula if node_size not specified
  if (is.null(node_size)) {
    node_size <- qgraph_default_vsize(n_nodes)
  }

  # Get qgraph scale constants
  qscale <- QGRAPH_SCALE

  # Convert to user coordinates using qgraph-matched scale factor
  vsize_usr <- recycle_to_length(node_size, n_nodes) * qscale$vsize_factor
  vsize2_usr <- if (!is.null(node_size2)) {
    recycle_to_length(node_size2, n_nodes) * qscale$vsize_factor
  } else {
    vsize_usr
  }

  # Node shapes
  shapes <- recycle_to_length(node_shape, n_nodes)

  # Node colors
  node_colors <- resolve_node_colors(node_fill, n_nodes, nodes, groups)

  # Apply alpha to node colors
  if (node_alpha < 1) {
    node_colors <- sapply(node_colors, function(c) adjust_alpha(c, node_alpha))
  }

  # Border colors
  if (is.null(node_border_color)) {
    node_border_color <- sapply(node_colors, function(c) {
      tryCatch(adjust_brightness(c, -0.3), error = function(e) "black")
    })
  }
  border_colors <- recycle_to_length(node_border_color, n_nodes)

  # Border widths
  border_widths <- recycle_to_length(node_border_width, n_nodes)

  # Labels
  node_labels <- resolve_labels(labels, nodes, n_nodes)

  # Label sizes - qgraph default is based on node size
  if (is.null(label_size)) {
    label_cex <- pmin(1, vsize_usr * 8)
  } else {
    label_cex <- recycle_to_length(label_size, n_nodes)
  }
  label_colors <- recycle_to_length(label_color, n_nodes)

  # ============================================
  # 4. QGRAPH-STYLE EDGE PROCESSING
  # ============================================

  # Use minimum threshold or explicit threshold
  effective_threshold <- max(threshold, minimum)

  if (n_edges > 0) {
    # Filter by minimum weight (threshold)
    edges <- filter_edges_by_weight(edges, effective_threshold)
    n_edges <- nrow(edges)
  }

  # ============================================
  # 4a. QGRAPH-STYLE DOUBLE CURVED EDGES FOR UNDIRECTED NETWORKS
  # ============================================
  # For undirected networks with curves enabled, duplicate edges to create
  # double curved visual representation (like qgraph)
  if (n_edges > 0 && !directed && (identical(curves, TRUE) || identical(curves, "mutual"))) {
    # Duplicate each edge with reversed direction
    reverse_edges <- edges
    reverse_edges$from <- edges$to
    reverse_edges$to <- edges$from
    edges <- rbind(edges, reverse_edges)
    n_edges <- nrow(edges)
  }

  if (n_edges > 0) {
    # Edge colors
    edge_colors <- resolve_edge_colors(edges, edge_color, positive_color, negative_color)

    # Apply edge alpha
    if (edge_alpha < 1) {
      edge_colors <- sapply(edge_colors, function(c) adjust_alpha(c, edge_alpha))
    }

    # qgraph-style edge width scaling
    # Use qgraph adaptive esize if not specified
    if (is.null(esize)) {
      esize <- qgraph_default_esize(n_nodes, weighted, directed)
    }

    # Scale esize down to reasonable lwd values (qgraph's internal units != lwd)
    # Use 0.25 factor to match splot/soplot edge_width_range (max ~4 lwd)
    esize_scaled <- esize * 0.25

    # Edge widths
    if (!is.null(edge_width)) {
      edge_widths <- recycle_to_length(edge_width, n_edges)
    } else if ("weight" %in% names(edges)) {
      # Use qgraph-exact scaling formula with scaled esize
      edge_widths <- qgraph_scale_edge_widths(
        weights = edges$weight,
        minimum = effective_threshold,
        maximum = maximum,
        cut = cut,
        esize = esize_scaled
      )
    } else {
      edge_widths <- rep(qscale$esize_unweighted, n_edges)
    }

    # Line types
    ltys <- recycle_to_length(edge_style, n_edges)

    # Handle curves mode
    curves_vec <- recycle_to_length(curvature, n_edges)
    is_reciprocal <- rep(FALSE, n_edges)

    # Identify reciprocal pairs
    for (i in seq_len(n_edges)) {
      from_i <- edges$from[i]
      to_i <- edges$to[i]
      if (from_i == to_i) next
      for (j in seq_len(n_edges)) {
        if (j != i && edges$from[j] == to_i && edges$to[j] == from_i) {
          is_reciprocal[i] <- TRUE
          break
        }
      }
    }

    # Default curvature for mutual/reciprocal edges (qgraph-style)
    # Increased from 0.3 to 0.5 for better visibility
    default_curve <- 0.5

    # Calculate network center for curve direction
    center_x <- mean(layout_mat[, 1])
    center_y <- mean(layout_mat[, 2])

    if (identical(curves, TRUE) || identical(curves, "mutual")) {
      for (i in seq_len(n_edges)) {
        if (is_reciprocal[i] && curves_vec[i] == 0) {
          # Calculate edge midpoint
          from_idx <- edges$from[i]
          to_idx <- edges$to[i]
          mid_x <- (layout_mat[from_idx, 1] + layout_mat[to_idx, 1]) / 2
          mid_y <- (layout_mat[from_idx, 2] + layout_mat[to_idx, 2]) / 2

          # Calculate perpendicular direction (for curve)
          dx <- layout_mat[to_idx, 1] - layout_mat[from_idx, 1]
          dy <- layout_mat[to_idx, 2] - layout_mat[from_idx, 2]

          # Perpendicular vector (rotated 90 degrees)
          perp_x <- -dy
          perp_y <- dx

          # Check if positive curve moves toward or away from center
          test_x <- mid_x + perp_x * 0.1
          test_y <- mid_y + perp_y * 0.1
          dist_to_center_pos <- sqrt((test_x - center_x)^2 + (test_y - center_y)^2)
          dist_to_center_orig <- sqrt((mid_x - center_x)^2 + (mid_y - center_y)^2)

          # Lower index node gets the curve pointing AWAY from center (outer curve)
          # Higher index node gets the curve pointing TOWARD center (inner curve)
          if (edges$from[i] < edges$to[i]) {
            # This edge should curve OUTWARD (away from center)
            curves_vec[i] <- if (dist_to_center_pos > dist_to_center_orig) default_curve else -default_curve
          } else {
            # This edge should curve INWARD (toward center)
            curves_vec[i] <- if (dist_to_center_pos < dist_to_center_orig) default_curve else -default_curve
          }
        }
      }
    } else if (identical(curves, "force")) {
      for (i in seq_len(n_edges)) {
        if (edges$from[i] == edges$to[i]) next
        if (curves_vec[i] == 0) {
          curves_vec[i] <- default_curve
        }
      }
    }

    curve_pivots <- recycle_to_length(curve_pivot, n_edges)
    curve_shapes <- recycle_to_length(curve_shape, n_edges)

    # Arrows
    if (is.logical(show_arrows) && length(show_arrows) == 1) {
      arrows_vec <- rep(directed && show_arrows, n_edges)
    } else {
      arrows_vec <- recycle_to_length(show_arrows, n_edges)
    }

    # Arrow size - qgraph style
    asize_scaled <- arrow_size * qscale$arrow_factor
    arrow_sizes <- recycle_to_length(asize_scaled, n_edges)

    # Bidirectional
    bidirectionals <- recycle_to_length(bidirectional, n_edges)

    # Loop rotation
    loop_rotations <- resolve_loop_rotation(loop_rotation, edges, layout_mat)

    # Edge labels
    edge_labels_vec <- resolve_edge_labels(edge_labels, edges, n_edges)
  }

  # ============================================
  # 5. DEVICE SETUP
  # ============================================

  # Handle file output
  if (filetype != "default") {
    full_filename <- paste0(filename, ".", filetype)

    if (filetype == "png") {
      grDevices::png(full_filename, width = width, height = height,
                     units = "in", res = res)
    } else if (filetype == "pdf") {
      grDevices::pdf(full_filename, width = width, height = height)
    } else if (filetype == "svg") {
      grDevices::svg(full_filename, width = width, height = height)
    } else if (filetype == "jpeg" || filetype == "jpg") {
      grDevices::jpeg(full_filename, width = width, height = height,
                      units = "in", res = res, quality = 100)
    } else if (filetype == "tiff") {
      grDevices::tiff(full_filename, width = width, height = height,
                      units = "in", res = res, compression = "lzw")
    } else {
      stop("Unknown filetype: ", filetype, call. = FALSE)
    }

    on.exit(grDevices::dev.off(), add = TRUE)
  }

  # Set up plot area
  old_mar <- graphics::par("mar")
  on.exit(graphics::par(mar = old_mar), add = TRUE)

  # Margins
  title_space <- if (!is.null(title)) 0.5 else 0
  graphics::par(mar = c(margins[1], margins[2], margins[3] + title_space, margins[4]))

  # Calculate plot limits
  x_range <- range(layout_mat[, 1], na.rm = TRUE)
  y_range <- range(layout_mat[, 2], na.rm = TRUE)

  # Add margin to limits
  x_margin <- diff(x_range) * layout_margin
  y_margin <- diff(y_range) * layout_margin

  xlim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  ylim <- c(y_range[1] - y_margin, y_range[2] + y_margin)

  # Create plot
  graphics::plot(
    1, type = "n",
    xlim = xlim,
    ylim = ylim,
    axes = FALSE,
    ann = FALSE,
    asp = if (aspect) 1 else NA,
    xaxs = "i", yaxs = "i"
  )

  # Background
  if (!is.null(background) && background != "transparent") {
    graphics::rect(
      xleft = xlim[1] - 1, ybottom = ylim[1] - 1,
      xright = xlim[2] + 1, ytop = ylim[2] + 1,
      col = background, border = NA
    )
  }

  # Title
  if (!is.null(title)) {
    graphics::title(main = title, cex.main = title_size)
  }

  # ============================================
  # 6. RENDER EDGES
  # ============================================

  if (n_edges > 0) {
    sonplot_render_edges(
      edges = edges,
      layout = layout_mat,
      node_sizes = vsize_usr,
      shapes = shapes,
      edge_color = edge_colors,
      edge_width = edge_widths,
      edge_style = ltys,
      curvature = curves_vec,
      curve_shape = curve_shapes,
      curve_pivot = curve_pivots,
      show_arrows = arrows_vec,
      arrow_size = arrow_sizes,
      bidirectional = bidirectionals,
      loop_rotation = loop_rotations,
      edge_labels = edge_labels_vec,
      edge_label_size = edge_label_size,
      edge_label_color = edge_label_color,
      edge_label_bg = edge_label_bg,
      edge_label_position = edge_label_position
    )
  }

  # ============================================
  # 7. RENDER NODES
  # ============================================

  # Handle donut nodes (similar to splot)
  effective_donut_values <- donut_values
  if (!is.null(donut_fill)) {
    if (!is.list(donut_fill)) {
      fill_vec <- recycle_to_length(donut_fill, n_nodes)
      effective_donut_values <- as.list(fill_vec)
    } else {
      effective_donut_values <- donut_fill
    }
  }

  effective_donut_colors <- NULL
  effective_bg_color <- donut_bg_color

  if (!is.null(donut_color)) {
    if (length(donut_color) == 2) {
      effective_donut_colors <- as.list(rep(donut_color[1], n_nodes))
      effective_bg_color <- donut_color[2]
    } else if (length(donut_color) == 1) {
      effective_donut_colors <- as.list(rep(donut_color, n_nodes))
    } else {
      cols <- recycle_to_length(donut_color, n_nodes)
      effective_donut_colors <- as.list(cols)
    }
  } else if (!is.null(effective_donut_values)) {
    effective_donut_colors <- as.list(rep("lightgray", n_nodes))
  }

  render_nodes_splot(
    layout = layout_mat,
    node_size = vsize_usr,
    node_size2 = vsize2_usr,
    node_shape = shapes,
    node_fill = node_colors,
    node_border_color = border_colors,
    node_border_width = border_widths,
    pie_values = pie_values,
    pie_colors = pie_colors,
    pie_border_width = NULL,
    donut_values = effective_donut_values,
    donut_colors = effective_donut_colors,
    donut_border_color = NULL,
    donut_border_width = NULL,
    donut_inner_ratio = donut_inner_ratio,
    donut_bg_color = effective_bg_color,
    donut_shape = "circle",
    donut_show_value = FALSE,
    donut_value_size = 0.8,
    donut_value_color = "black",
    donut_value_fontface = "bold",
    donut_value_fontfamily = "sans",
    donut_value_digits = 2,
    donut_value_prefix = "",
    donut_value_suffix = "",
    donut2_values = NULL,
    donut2_colors = NULL,
    donut2_inner_ratio = 0.4,
    labels = node_labels,
    label_size = label_cex,
    label_color = label_colors,
    label_position = "center",
    label_fontface = "plain",
    label_fontfamily = "sans",
    label_hjust = 0.5,
    label_vjust = 0.5,
    label_angle = 0,
    usePCH = usePCH
  )

  # ============================================
  # 8. LEGEND
  # ============================================

  if (legend) {
    has_pos_edges <- FALSE
    has_neg_edges <- FALSE
    if (n_edges > 0 && "weight" %in% names(edges)) {
      has_pos_edges <- any(edges$weight > 0, na.rm = TRUE)
      has_neg_edges <- any(edges$weight < 0, na.rm = TRUE)
    }

    render_legend_splot(
      groups = groups,
      node_names = NULL,
      nodes = nodes,
      node_colors = node_colors,
      position = legend_position,
      cex = legend_size,
      show_edge_colors = legend_edge_colors,
      positive_color = positive_color,
      negative_color = negative_color,
      has_pos_edges = has_pos_edges,
      has_neg_edges = has_neg_edges,
      show_node_sizes = FALSE,
      node_size = vsize_usr
    )
  }

  # ============================================
  # 9. RETURN
  # ============================================

  invisible(network)
}


#' Render Edges for sonplot (qgraph-compatible)
#'
#' Edge rendering using qgraph-compatible geometry calculations.
#'
#' @keywords internal
sonplot_render_edges <- function(edges, layout, node_sizes, shapes,
                                  edge_color, edge_width, edge_style, curvature,
                                  curve_shape, curve_pivot, show_arrows, arrow_size,
                                  bidirectional, loop_rotation, edge_labels,
                                  edge_label_size, edge_label_color, edge_label_bg,
                                  edge_label_position = 0.5) {

  m <- nrow(edges)
  if (m == 0) return(invisible())

  n <- nrow(layout)

  # Calculate network center for inward curve direction
  center_x <- mean(layout[, 1])
  center_y <- mean(layout[, 2])

  # Get render order (weakest to strongest)
  order_idx <- get_edge_order(edges)

  # Storage for label positions
  label_positions <- vector("list", m)

  for (i in order_idx) {
    from_idx <- edges$from[i]
    to_idx <- edges$to[i]

    x1 <- layout[from_idx, 1]
    y1 <- layout[from_idx, 2]
    x2 <- layout[to_idx, 1]
    y2 <- layout[to_idx, 2]

    # Self-loop
    if (from_idx == to_idx) {
      draw_self_loop_base(
        x1, y1, node_sizes[from_idx],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        rotation = loop_rotation[i],
        arrow = show_arrows[i],
        asize = arrow_size[i]
      )

      loop_dist <- node_sizes[from_idx] * 2.5
      label_positions[[i]] <- list(
        x = x1 + loop_dist * cos(loop_rotation[i]),
        y = y1 + loop_dist * sin(loop_rotation[i])
      )
      next
    }

    # Calculate edge endpoints using qgraph-compatible formula
    angle_to <- splot_angle(x1, y1, x2, y2)
    angle_from <- splot_angle(x2, y2, x1, y1)

    start <- qgraph_cent_to_edge_simple(x1, y1, angle_to, node_sizes[from_idx], shapes[from_idx])
    end <- qgraph_cent_to_edge_simple(x2, y2, angle_from, node_sizes[to_idx], shapes[to_idx])

    # Use the curvature value directly (already set correctly for mutual edges)
    curve_i <- curvature[i]

    # Draw edge
    if (abs(curve_i) > 1e-6) {
      draw_curved_edge_base(
        start$x, start$y, end$x, end$y,
        curve = curve_i,
        curvePivot = curve_pivot[i],
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i]
      )
    } else {
      draw_straight_edge_base(
        start$x, start$y, end$x, end$y,
        col = edge_color[i],
        lwd = edge_width[i],
        lty = edge_style[i],
        arrow = show_arrows[i],
        asize = arrow_size[i],
        bidirectional = bidirectional[i]
      )
    }

    # Store label position
    label_positions[[i]] <- get_edge_label_position(
      start$x, start$y, end$x, end$y,
      position = edge_label_position,
      curve = curve_i,
      curvePivot = curve_pivot[i],
      label_offset = 0  # On the line, not offset
    )
  }

  # Draw edge labels
  if (!is.null(edge_labels)) {
    for (i in seq_len(m)) {
      if (!is.null(edge_labels[i]) && !is.na(edge_labels[i]) && edge_labels[i] != "") {
        pos <- label_positions[[i]]
        draw_edge_label_base(
          pos$x, pos$y,
          label = edge_labels[i],
          cex = edge_label_size,
          col = edge_label_color,
          bg = edge_label_bg,
          font = 1
        )
      }
    }
  }
}
