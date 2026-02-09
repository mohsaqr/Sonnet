# Create PowerPoint presentation for cograph
# Requires: officer package

library(officer)

# Create a new PowerPoint presentation
ppt <- read_pptx()

# Define colors
primary_blue <- "#1e3a5f"
accent_blue <- "#2563eb"
accent_purple <- "#7c3aed"
light_gray <- "#f8fafc"
dark_text <- "#334155"
code_bg <- "#0f172a"

# ============================================================================
# Slide 1: Title
# ============================================================================
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "cograph", location = ph_location_type("ctrTitle"))
ppt <- ph_with(ppt, value = "Modern Network Visualization for R\n\nVersion 1.5.2\n\nMohammed Saqr\nUniversity of Eastern Finland",
               location = ph_location_type("subTitle"))

# ============================================================================
# Slide 2: What is cograph?
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "What is cograph?", location = ph_location_type("title"))

content_2 <- "A modern R package for creating publication-ready network visualizations with minimal code.

Key Features:
\u2022 Universal input: matrices, igraph, tna, edge lists, statnet
\u2022 One function: splot(data) is all you need
\u2022 Smart defaults: auto-detect directed, scale by weight
\u2022 TNA-first: built for Transition Network Analysis
\u2022 Statistical: bootstrap, permutation, stability plots
\u2022 Extensible: custom layouts, shapes, themes

Simplest Usage:
  splot(my_matrix)
  splot(igraph_object)
  splot(tna_model)

Dependencies: R6, grid, grDevices, ggplot2
Install: install.packages(\"cograph\")"

ppt <- ph_with(ppt, value = content_2, location = ph_location_type("body"))

# ============================================================================
# Slide 3: Design Philosophy
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Design Philosophy", location = ph_location_type("title"))

content_3 <- "Core Principle: Simplicity First
Network visualization should be as simple as splot(matrix) while offering deep customization when needed.

1. Progressive Complexity
   \u2022 Start with splot(data)
   \u2022 Add parameters as needed
   \u2022 Full control available

2. Consistent API
   \u2022 All snake_case naming
   \u2022 Predictable patterns
   \u2022 sn_*() for transforms, plot_*() for plots

3. Pipe-Friendly
   \u2022 Works with |>
   \u2022 Immutable transforms
   \u2022 Chain operations

4. TNA-First Design
   \u2022 Built with Transition Network Analysis as primary use case
   \u2022 Preserves all TNA metadata: weights, initial probabilities, labels

5. Statistical Plotting
   \u2022 Validation results directly plottable
   \u2022 Run bootstrap(), then splot() the result"

ppt <- ph_with(ppt, value = content_3, location = ph_location_type("body"))

# ============================================================================
# Slide 4: Section - Naming Convention
# ============================================================================
ppt <- add_slide(ppt, layout = "Section Header", master = "Office Theme")
ppt <- ph_with(ppt, value = "Naming Convention", location = ph_location_type("title"))
ppt <- ph_with(ppt, value = "Consistent, discoverable, memorable function and parameter names",
               location = ph_location_type("body"))

# ============================================================================
# Slide 5: Function Naming Patterns
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Function Naming Patterns", location = ph_location_type("title"))

content_5 <- "Pattern         | Purpose              | Examples
------------|---------------------|--------------------
splot*()       | Primary plotting     | splot(), soplot()
sn_*()         | Pipe transforms      | sn_layout(), sn_nodes(), sn_edges()
plot_*()       | Specialized plots    | plot_compare(), plot_heatmap()
to_*()         | Export conversion    | to_igraph(), to_matrix(), to_df()
as_*()         | Import conversion    | as_cograph()
get_*()        | Accessors            | get_nodes(), get_edges()
n_*()          | Counts               | n_nodes(), n_edges()

Discoverability:
Type sn_ + Tab in RStudio to see all transformation functions
Type plot_ + Tab for all specialized plots

The \"sn\" prefix stands for \"simple network\" or \"splot network\""

ppt <- ph_with(ppt, value = content_5, location = ph_location_type("body"))

# ============================================================================
# Slide 6: Parameter Naming
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Parameter Naming: snake_case Throughout", location = ph_location_type("title"))

content_6 <- "Node Parameters:
  node_size, node_fill, node_shape, node_border_color
  node_border_width, node_alpha
  label_size, label_color, label_position

Edge Parameters:
  edge_color, edge_width, edge_style, edge_alpha
  edge_labels, edge_label_size
  arrow_size, curvature

Comparison with Other Packages:
Concept      | qgraph    | igraph        | cograph (clear!)
-----------|---------|-------------|------------------
Node size    | vsize     | vertex.size   | node_size
Node color   | color     | vertex.color  | node_fill
Edge width   | esize     | edge.width    | edge_width
Donut chart  | pie (list)| N/A           | donut_fill (0-1)"

ppt <- ph_with(ppt, value = content_6, location = ph_location_type("body"))

# ============================================================================
# Slide 7: Section - splot()
# ============================================================================
ppt <- add_slide(ppt, layout = "Section Header", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot()", location = ph_location_type("title"))
ppt <- ph_with(ppt, value = "The main plotting function - comprehensive parameter reference",
               location = ph_location_type("body"))

# ============================================================================
# Slide 8: splot() Overview
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot() - Function Overview", location = ph_location_type("title"))

content_8 <- "What splot() Does:
\u2022 Accepts any input: matrix, igraph, tna, data.frame, network, qgraph
\u2022 Auto-detects: directedness from matrix symmetry
\u2022 Auto-scales: edge widths by weight magnitude
\u2022 Auto-colors: positive (green) vs negative (red) edges
\u2022 Computes layout: spring, circle, or custom
\u2022 Renders: using fast base R graphics

Three Rendering Backends:
  splot()     - Base R    - Returns NULL (plots to device)
  soplot()    - Grid      - Returns grob object
  sn_ggplot() - ggplot2   - Returns ggplot object

Basic Signature:
  splot(x, layout = \"spring\", directed = NULL, seed = 42, theme = \"classic\",
        node_size, node_fill, node_shape, labels, label_size,
        edge_color, edge_width, edge_style, edge_labels,
        edge_ci, edge_ci_scale, title, mar, ...)

Key insight: All parameters have defaults. Only x is required."

ppt <- ph_with(ppt, value = content_8, location = ph_location_type("body"))

# ============================================================================
# Slide 9: splot() Node Parameters
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot() - Node Parameters", location = ph_location_type("title"))

content_9 <- "Basic Node Styling:
  node_size (default: 3)        - Size (scalar or per-node vector)
  node_size2 (default: NULL)    - Secondary size for ellipse height
  node_shape (default: circle)  - Shape name or vector
  node_fill (default: #4A90D9)  - Fill color(s)
  node_border_color (#2C5AA0)   - Border color(s)
  node_border_width (1)         - Border thickness
  node_alpha (1)                - Transparency (0-1)

Available Shapes:
  circle, square, triangle, diamond, pentagon, hexagon, star, heart, ellipse, cross

Label Parameters:
  labels (TRUE)             - TRUE, FALSE, or character vector
  label_size (10)           - Font size (cex)
  label_color (black)       - Text color
  label_position (center)   - center, above, below, left, right
  label_fontface (plain)    - plain, bold, italic, bold.italic

Example:
  splot(net, node_shape = \"hexagon\", node_fill = c(\"coral\", \"steelblue\"),
        node_size = 8, labels = TRUE, label_color = \"white\")"

ppt <- ph_with(ppt, value = content_9, location = ph_location_type("body"))

# ============================================================================
# Slide 10: Donut & Pie Chart Nodes
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot() - Donut & Pie Chart Nodes", location = ph_location_type("title"))

content_10 <- "Donut Charts (qgraph-style):
Simple proportional fill using a single value (0-1) per node.

  donut_fill (NULL)         - Fill proportion 0-1 per node
  donut_color (maroon)      - Fill color(s)
  donut_bg_color (gray90)   - Unfilled portion color
  donut_inner_ratio (0.5)   - Inner radius (0-1)
  donut_show_value (FALSE)  - Show value in center

Example - TNA initial probabilities as donuts:
  splot(tna_model, donut_fill = tna_model$inits,
        donut_color = \"steelblue\", donut_inner_ratio = 0.6)

Pie Chart Nodes:
Multiple segments per node using a list of vectors.

  pie_values    - List of numeric vectors (one per node)
  pie_colors    - List of color vectors for segments

Example:
  splot(net, pie_values = list(c(0.4, 0.6), c(0.3, 0.3, 0.4)),
        pie_colors = list(c(\"red\", \"blue\"), c(\"red\", \"green\", \"blue\")))

Tip: A simple numeric vector passed to pie_values auto-converts to donut_fill."

ppt <- ph_with(ppt, value = content_10, location = ph_location_type("body"))

# ============================================================================
# Slide 11: Edge Parameters
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot() - Edge Parameters", location = ph_location_type("title"))

content_11 <- "Basic Edge Styling:
  edge_color (NULL)           - Color(s). NULL = auto by weight sign
  edge_width (NULL)           - Width(s). NULL = scale by weight
  edge_style (1)              - 1=solid, 2=dashed, 3=dotted...
  edge_alpha (0.8)            - Transparency (0-1)
  edge_width_range (c(0.5,4)) - Output width range
  edge_scale_mode (linear)    - linear, log, sqrt, rank

Default Color Convention:
  Positive: #2E7D32 (green)
  Negative: #C62828 (red)
  Zero/NA:  gray50

Arrows & Curvature:
  show_arrows (auto)    - TRUE for directed networks
  arrow_size (0.015)    - Arrow head size
  curvature (0)         - Edge bend (0 = straight)
  curves (TRUE)         - Auto-curve reciprocal edges
  loop_rotation (auto)  - Self-loop angle (radians)

Example:
  splot(net, edge_color = \"weight\", edge_width = \"weight\",
        edge_scale_mode = \"sqrt\", curvature = 0.2)"

ppt <- ph_with(ppt, value = content_11, location = ph_location_type("body"))

# ============================================================================
# Slide 12: Edge Labels & CI
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "splot() - Edge Labels & Confidence Intervals", location = ph_location_type("title"))

content_12 <- "Edge Label Parameters:
  edge_labels (FALSE)         - TRUE, FALSE, or character vector
  edge_label_size (0.8)       - Font size
  edge_label_color (black)    - Text color
  edge_label_bg (NA)          - Background (NA = transparent)
  edge_label_halo (TRUE)      - White outline for readability
  edge_label_position (0.5)   - Position along edge (0-1)
  edge_label_fontface (plain) - plain, bold, italic

Example:
  splot(net, edge_labels = TRUE, edge_label_size = 0.7,
        edge_label_halo = TRUE, edge_label_bg = NA)

Confidence Interval Visualization:
Show uncertainty as translucent edge underlays.

  edge_ci (NULL)         - CI widths vector (0-1 scale)
  edge_ci_scale (2)      - Width multiplier for underlay
  edge_ci_alpha (0.15)   - Underlay transparency
  edge_ci_color (NA)     - Underlay color (NA = edge color)
  edge_priority (NULL)   - Render order (higher = on top)

Example - Visualize bootstrap CIs:
  splot(net, edge_ci = boot$ci_upper - boot$ci_lower,
        edge_ci_scale = 3, edge_priority = ifelse(sig, 2, 1))"

ppt <- ph_with(ppt, value = content_12, location = ph_location_type("body"))

# ============================================================================
# Slide 13: Section - Statistical Plotting
# ============================================================================
ppt <- add_slide(ppt, layout = "Section Header", master = "Office Theme")
ppt <- ph_with(ppt, value = "Statistical Plotting", location = ph_location_type("title"))
ppt <- ph_with(ppt, value = "Validation results are directly plottable with automatic styling",
               location = ph_location_type("body"))

# ============================================================================
# Slide 14: Bootstrap Visualization
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Bootstrap Visualization", location = ph_location_type("title"))

content_14 <- "How It Works:
Run bootstrap() on a tna model, then pass the result directly to splot().
Styling is automatic - significant edges prominent, non-significant subtle.

  boot <- bootstrap(tna_model, iter = 1000, level = 0.05)
  splot(boot)  # Styling is automatic!

What bootstrap() Returns:
  $weights      - Original weight matrix
  $weights_sig  - Significant weights only
  $p_values     - Edge p-values matrix
  $ci_lower     - Lower confidence bounds
  $ci_upper     - Upper confidence bounds
  $significant  - Logical significance matrix

Auto Styling:
                   Significant    Non-Significant
  Color            #003355        #E091AA (pink)
  Style            Solid          Dashed
  Labels           0.45***        0.12
  Alpha            0.7            0.4
  CI scale         1.0            3.0
  Priority         On top         Behind

Stars indicate p-value:  *** p < 0.001, ** p < 0.01, * p < 0.05"

ppt <- ph_with(ppt, value = content_14, location = ph_location_type("body"))

# ============================================================================
# Slide 15: Other Validation Functions
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Other Validation Functions", location = ph_location_type("title"))

content_15 <- "permutation_test() - Network Comparison
Statistical comparison of two networks with p-values for edge and centrality differences.
  perm <- permutation_test(tna1, tna2, iter = 1000)
  plot(perm)

estimate_cs() - Centrality Stability
CS-coefficient: how stable are centrality rankings when dropping cases?
  cs <- estimate_cs(tna_model, measures = c(\"InStrength\", \"Betweenness\"))
  plot(cs)  # Stability curves
  # CS > 0.5 acceptable, > 0.7 good

disparity_filter() - Backbone Extraction
Extract statistically significant edges using Serrano et al. (2009) method.
  backbone <- disparity_filter(mat, level = 0.05)
  splot(backbone)

plot_compare() - Visual Difference
Visualize element-wise difference between networks (x - y).
  plot_compare(net1, net2)
  # Green edges: net1 > net2 (positive diff)
  # Red edges: net1 < net2 (negative diff)

Enabling validation for non-TNA networks:
  set_raw_data(net, data)  # Attach raw sequence data"

ppt <- ph_with(ppt, value = content_15, location = ph_location_type("body"))

# ============================================================================
# Slide 16: Pipe-Friendly Workflow
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Pipe-Friendly Workflow", location = ph_location_type("title"))

content_16 <- "The sn_* Functions:
All sn_*() functions take a network, modify it, and return a modified copy.
This enables fluent chaining with the pipe operator.

  sn_layout(net, layout, seed)         - Set layout algorithm
  sn_theme(net, theme)                 - Apply visual theme
  sn_palette(net, palette, target, by) - Apply color palette
  sn_nodes(net, fill, size, ...)       - Set node aesthetics
  sn_edges(net, color, width, ...)     - Set edge aesthetics
  sn_save(net, file, width, height)    - Save to file

Complete Example:
  cograph(mat) |>
    sn_layout(\"circle\", seed = 42) |>
    sn_theme(\"dark\") |>
    sn_nodes(fill = \"coral\", size = 0.08, shape = \"hexagon\") |>
    sn_edges(color = \"weight\", width = \"weight\", alpha = 0.7) |>
    sn_palette(\"viridis\", target = \"nodes\", by = \"degree\") |>
    splot(title = \"Styled Network\")

Note: Each function returns a new object. The original network is never modified."

ppt <- ph_with(ppt, value = content_16, location = ph_location_type("body"))

# ============================================================================
# Slide 17: Built-in Themes
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Built-in Themes", location = ph_location_type("title"))

content_17 <- "Available Themes:
  classic    - White bg, blue nodes (default)
  dark       - Dark bg, light elements
  minimal    - Subtle, clean styling
  colorblind - CVD-friendly colors
  gray       - Print-friendly B&W
  nature     - Earth tones
  viridis    - Viridis palette
  + custom   - register_theme()

Using Themes:
  # Direct in splot()
  splot(net, theme = \"dark\")

  # Or via pipe
  cograph(mat) |> sn_theme(\"colorblind\") |> splot()

Register Custom Theme:
  register_theme(\"mytheme\", list(
    background = \"#1a1a2e\",
    node_fill = \"#e94560\",
    node_border = \"#ffffff\",
    edge_color = \"#0f3460\"
  ))

Built-in Palettes:
  palette_viridis(n), palette_colorblind(n), palette_pastel(n),
  palette_rainbow(n), palette_blues(n), palette_diverging(n)"

ppt <- ph_with(ppt, value = content_17, location = ph_location_type("body"))

# ============================================================================
# Slide 18: Layout Algorithms
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Layout Algorithms", location = ph_location_type("title"))

content_18 <- "Built-in Layouts:
  spring    (fr)       - Force-directed (Fruchterman-Reingold)
  circle               - Circular arrangement
  oval      (ellipse)  - Elliptical arrangement
  groups               - Grouped circular (by cluster)
  grid                 - Regular grid
  star                 - Center node with ring
  bipartite            - Two-column layout
  random               - Random positions

igraph Layout Codes:
  kk   - Kamada-Kawai
  drl  - DrL (large graphs)
  mds  - Multidimensional scaling
  ni   - Nicely (auto-select)
  tr   - Tree / Reingold-Tilford

Using Layouts:
  splot(net, layout = \"spring\", seed = 42)  # In splot()
  cograph(mat) |> sn_layout(\"circle\") |> splot()  # Via pipe
  splot(net, layout = my_coords)  # Custom n\u00d72 matrix
  splot(net, layout = igraph::layout_with_fr)  # igraph function

Register Custom Layout:
  register_layout(\"spiral\", function(network, ...) { ... })

Tip: Always set seed for reproducible layouts."

ppt <- ph_with(ppt, value = content_18, location = ph_location_type("body"))

# ============================================================================
# Slide 19: TNA Integration
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "TNA Integration", location = ph_location_type("title"))

content_19 <- "TNA-First Design:
cograph is built with the tna (Transition Network Analysis) package as primary use case.
All TNA metadata is preserved for seamless round-trip compatibility.

Basic TNA Plotting:
  library(tna)
  model <- tna(engagement_data)
  splot(model)  # Direct plotting - just works!

  splot(model, donut_fill = model$inits,  # Initial probs as donuts
        edge_labels = TRUE)

group_tna Support:
  group_model <- group_tna(data, group = \"cluster\")
  splot(group_model, i = 1)  # Plot individual groups
  splot(group_model, i = \"high_achievers\")
  plot_compare(group_model, i = 1, j = 2)  # Compare groups
  bootstrap(group_model, i = 1)  # Bootstrap per group

TNA Helper Functions:
  is_tna_network(net)     - Check if TNA-based
  get_tna_model(net)      - Retrieve original tna object
  get_raw_data(net)       - Get sequence data
  set_raw_data(net, data) - Attach data for validation

Preserved TNA Fields: $weights, $nodes$inits, $labels"

ppt <- ph_with(ppt, value = content_19, location = ph_location_type("body"))

# ============================================================================
# Slide 20: Specialized Plot Functions
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Specialized Plot Functions", location = ph_location_type("title"))

content_20 <- "plot_heatmap() - Network as Heatmap
Visualize adjacency/weight matrix with clustering support.
  plot_heatmap(net, colors = \"viridis\", show_values = TRUE)

plot_mlna() - Multilevel Networks
3D perspective view with stacked layers.
  plot_mlna(mat, layer_list = list(Macro = n1, Meso = n2, Micro = n3))

plot_htna() - Bipartite Networks
Heterogeneous/bipartite layout.
  plot_htna(net, groups = list(Students = s_nodes, Resources = r_nodes))

tna_animate() - Animated Networks
Create animated GIF of network evolution.
  tna_animate(data, window_size = 100, step = 50, output = \"network.gif\")

More Specialized Functions:
  plot_comparison_heatmap() - Side-by-side heatmap comparison
  plot_mtna()               - Multi-cluster network visualization
  plot_mcml()               - Multi-cluster multilevel combination
  plot_ml_heatmap()         - Multilayer network as heatmap"

ppt <- ph_with(ppt, value = content_20, location = ph_location_type("body"))

# ============================================================================
# Slide 21: Summary
# ============================================================================
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = "Summary: Why cograph?", location = ph_location_type("title"))

content_21 <- "Simple
  \u2022 splot(data) is enough
  \u2022 Smart defaults for everything
  \u2022 No mandatory configuration
  \u2022 Works with any format

Consistent
  \u2022 snake_case everywhere
  \u2022 Predictable patterns
  \u2022 Discoverable API
  \u2022 Clear documentation

Powerful
  \u2022 Full customization available
  \u2022 Statistical validation built-in
  \u2022 TNA-first design
  \u2022 Extensible registries

Key Functions to Remember:
  splot(x)      - Main plotting function
  cograph(x)    - Create network with layout
  as_cograph(x) - Convert to cograph format
  sn_*()        - Pipe-friendly transforms
  bootstrap()   - Edge significance testing
  plot_compare()- Network difference visualization

Getting Started:
  install.packages(\"cograph\")
  library(cograph)
  splot(your_matrix)  # That's it!"

ppt <- ph_with(ppt, value = content_21, location = ph_location_type("body"))

# ============================================================================
# Slide 22: Thank You
# ============================================================================
ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
ppt <- ph_with(ppt, value = "Thank You", location = ph_location_type("ctrTitle"))
ppt <- ph_with(ppt, value = "github.com/mohsaqr/cograph\n\nQuestions?\n\nMohammed Saqr\nmohammed.saqr@uef.fi",
               location = ph_location_type("subTitle"))

# ============================================================================
# Save the presentation
# ============================================================================
print(ppt, target = "docs/cograph-presentation.pptx")

cat("PowerPoint presentation created: docs/cograph-presentation.pptx\n")
