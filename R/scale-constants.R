#' @title Scaling Constants
#' @description Central scaling constants for parameter alignment between splot/soplot.
#' @name scale-constants
#' @keywords internal
NULL

#' Sonnet Scaling Constants
#'
#' Central location for all scaling factors used in splot() and soplot().
#' These constants are calibrated to produce similar visual output to qgraph
#' when using equivalent parameter values.
#'
#' @details
#' The default scaling mode uses values calibrated to match qgraph visual appearance:
#' - `node_size = 6` in Sonnet should look similar to `vsize = 6` in qgraph
#' - `label_size = 1` uses cex-style multiplier (independent of node size)
#' - `arrow_size = 1` produces consistent arrows between splot and soplot
#'
#' Legacy mode preserves the original Sonnet v1.x behavior where:
#' - Node sizes used a 0.04 scale factor
#' - Label sizes were coupled to node size (vsize * 8)
#' - Arrow sizes differed between splot (0.03) and soplot (0.015)
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{node_factor}{Scale factor applied to node_size parameter}
#'   \item{node_default}{Default node size when not specified}
#'   \item{label_default}{Default label size (cex multiplier)}
#'   \item{label_coupled}{Whether label size is coupled to node size}
#'   \item{edge_base}{Base edge width}
#'   \item{edge_scale}{Edge width scale factor}
#'   \item{edge_default}{Default edge width}
#'   \item{arrow_factor}{Scale factor for arrow sizes}
#'   \item{arrow_default}{Default arrow size}
#' }
#'
#' @keywords internal
SONNET_SCALE <- list(
  # Node sizing: node_size=6 should look like qgraph vsize=6
  # Calibrated: 6 * 0.015 = 0.09 user coords (similar visual size to qgraph)
  node_factor = 0.015,
  node_default = 6,

  # Label sizing: independent of node, cex-style
  # label_size=1 is the baseline (like cex=1 in base R)
  label_default = 1,
  label_coupled = FALSE,

  # Edge sizing
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = 1,


  # Arrow sizing - unified between splot and soplot
  # Calibrated to produce visually balanced arrows

  arrow_factor = 0.02,
  arrow_default = 1,

  # soplot-specific: NPC coordinates
  # When converting node_size for soplot (NPC coords), use this factor
  soplot_node_factor = 0.008
)

#' Legacy Scaling Constants (Pre-v2.0 Behavior)
#'
#' Scaling constants that preserve the original Sonnet v1.x behavior.
#' Use `scaling = "legacy"` to enable these values.
#'
#' @format A list with the same structure as \code{SONNET_SCALE}
#' @keywords internal
SONNET_SCALE_LEGACY <- list(
  # Original splot values
  node_factor = 0.04,
  node_default = 3,

  # Label size coupled to node size (vsize * 8)
  label_default = NULL,
  label_coupled = TRUE,

  # Edge sizing (unchanged)
  edge_base = 0.5,
  edge_scale = 3,
  edge_default = NULL,

  # Original arrow factors
  # splot used 0.03, soplot used 0.015
  arrow_factor = 0.03,
  arrow_factor_soplot = 0.015,
  arrow_default = 1,

  # soplot-specific (original behavior)
  soplot_node_factor = 0.01
)

#' Get Scaling Constants
#'
#' Returns the appropriate scaling constants based on the scaling mode.
#'
#' @param scaling Character: "default" for qgraph-matched scaling,
#'   "legacy" for pre-v2.0 behavior.
#' @return A list of scaling constants.
#' @keywords internal
get_scale_constants <- function(scaling = "default") {
  if (identical(scaling, "legacy")) {
    SONNET_SCALE_LEGACY
  } else {
    SONNET_SCALE
  }
}
