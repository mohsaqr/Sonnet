# Multi-Cluster TNA Network Plot (mtna)

Visualize multiple network clusters with summary edges between clusters and individual edges within clusters.

---

## Overview

The `mtna()` / `plot_mtna()` function creates a cluster-based visualization:

- Each **cluster** is displayed as a shape (circle, square, diamond, triangle)
- **Summary edges** show aggregated connections between clusters
- **Within-cluster edges** show individual node connections inside each cluster
- Clusters can be arranged in circle, grid, horizontal, or vertical layouts

---

## Basic Syntax

```r
mtna(x, cluster_list, ...)
plot_mtna(x, cluster_list, ...)  # same function
```

---

## Arguments

### Required Arguments

| Argument | Description |
|----------|-------------|
| `x` | A tna object or weight matrix |
| `cluster_list` | Named list of character vectors defining clusters |

### Layout Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `layout` | `"circle"` | Cluster arrangement: `"circle"`, `"grid"`, `"horizontal"`, `"vertical"` |
| `spacing` | `3` | Distance between cluster centers |
| `shape_size` | `1.2` | Size of each cluster shape (shell radius) |
| `node_spacing` | `0.5` | Radius for node placement within shapes (0-1) |
| `layout_margin` | `0.15` | Margin around the layout as fraction of range |

### Appearance Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `colors` | `NULL` | Vector of colors for each cluster (auto-generated if NULL) |
| `shapes` | `NULL` | Vector of shapes for each cluster |
| `edge_colors` | `NULL` | Vector of edge colors by source cluster |
| `node_size` | `2` | Size of nodes inside shapes |
| `curvature` | `0.3` | Edge curvature |

### Edge Control Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `summary_edges` | `TRUE` | Show aggregated summary edges between clusters |
| `within_edges` | `TRUE` | Show individual edges within each cluster |
| `bundle_edges` | `TRUE` | Bundle inter-cluster edges through channels |
| `bundle_strength` | `0.8` | How tightly to bundle edges (0-1) |

### Display Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `show_border` | `TRUE` | Draw a border around each cluster |
| `legend` | `TRUE` | Show legend |
| `legend_position` | `"topright"` | Position for legend |

---

## Cluster List Format

The `cluster_list` must be a **named list** of character vectors:

```r
cluster_list <- list(
  Cluster1 = c("node1", "node2", "node3"),
  Cluster2 = c("node4", "node5", "node6"),
  Cluster3 = c("node7", "node8", "node9"),
  Cluster4 = c("node10", "node11", "node12")
)
```

### Rules

- Minimum 2 clusters required
- Node names must match the matrix row/column names
- **No overlap**: Each node can only belong to one cluster

---

## Layout Options

### `"circle"` (default)

Clusters arranged in a circle.

```r
mtna(m, clusters, layout = "circle")
```

### `"grid"`

Clusters arranged in a grid pattern.

```r
mtna(m, clusters, layout = "grid")
```

### `"horizontal"`

Clusters arranged in a horizontal row.

```r
mtna(m, clusters, layout = "horizontal")
```

### `"vertical"`

Clusters arranged in a vertical column.

```r
mtna(m, clusters, layout = "vertical")
```

---

## Available Shapes

Shapes cycle through clusters automatically:

| Shape | Description |
|-------|-------------|
| `"circle"` | Circular shell |
| `"square"` | Square shell |
| `"diamond"` | Diamond shape |
| `"triangle"` | Triangle pointing up |
| `"pentagon"` | Pentagon |
| `"hexagon"` | Hexagon |
| `"star"` | Star shape |
| `"cross"` | Cross/plus |

---

## Default Color Palettes

### Cluster Colors (fills)

```r
c("#ffd89d", "#a68ba5", "#7eb5d6", "#98d4a2",
  "#f4a582", "#92c5de", "#d6c1de", "#b8e186",
  "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9")
```

### Edge Colors

```r
c("#e6a500", "#7a5a7a", "#4a90b8", "#5cb85c",
  "#d9534f", "#5bc0de", "#9b59b6", "#8bc34a",
  "#ff7043", "#78909c", "#ab47bc", "#aed581")
```

---

## Examples

### Basic Usage

```r
# Create sample data
set.seed(42)
nodes <- paste0("N", 1:20)
m <- matrix(runif(400, 0, 0.3), 20, 20)
diag(m) <- 0
colnames(m) <- rownames(m) <- nodes

# Define 4 clusters
clusters <- list(
  North = paste0("N", 1:5),
  East  = paste0("N", 6:10),
  South = paste0("N", 11:15),
  West  = paste0("N", 16:20)
)

# Basic plot with summary edges
mtna(m, clusters)
```

### Different Layouts

```r
# Circle layout (default)
mtna(m, clusters, layout = "circle")

# Grid layout
mtna(m, clusters, layout = "grid")

# Horizontal row
mtna(m, clusters, layout = "horizontal")

# Vertical column
mtna(m, clusters, layout = "vertical")
```

### Customized Appearance

```r
mtna(m, clusters,
     layout = "circle",
     spacing = 4,
     shape_size = 1.5,
     node_spacing = 0.6,
     node_size = 3,
     curvature = 0.4)
```

### Custom Colors and Shapes

```r
mtna(m, clusters,
     colors = c("coral", "steelblue", "forestgreen", "purple"),
     shapes = c("circle", "square", "diamond", "triangle"),
     edge_colors = c("darkred", "darkblue", "darkgreen", "darkorchid"))
```

### Edge Display Options

```r
# Summary edges only (no within-cluster edges)
mtna(m, clusters,
     summary_edges = TRUE,
     within_edges = FALSE)

# All individual edges (no summary)
mtna(m, clusters,
     summary_edges = FALSE)

# Both summary and within-cluster edges (default)
mtna(m, clusters,
     summary_edges = TRUE,
     within_edges = TRUE)
```

### Edge Bundling

```r
# Strong bundling (nodes positioned by inter-cluster connections)
mtna(m, clusters,
     bundle_edges = TRUE,
     bundle_strength = 0.9)

# No bundling (nodes evenly distributed)
mtna(m, clusters,
     bundle_edges = FALSE)
```

### Minimal Display

```r
mtna(m, clusters,
     show_border = FALSE,
     legend = FALSE)
```

---

## Summary Edges Explained

When `summary_edges = TRUE` (default):

1. **Inter-cluster edges**: Aggregated (summed) weights between clusters
   - Displayed as thick curved arrows between cluster shells
   - Line width proportional to total edge weight
   - Labels show the aggregated weight value

2. **Intra-cluster edges**: Individual node-to-node edges within each cluster
   - Only shown if `within_edges = TRUE`
   - Displayed inside each cluster shell

### Example Output

```
Cluster A ──(2.5)──> Cluster B
    │                    │
    │  (individual       │  (individual
    │   edges inside)    │   edges inside)
    │                    │
```

---

## Typical Use Cases

### 1. Regional Network Analysis

```r
clusters <- list(
  NorthAmerica = c("USA", "Canada", "Mexico"),
  Europe       = c("UK", "France", "Germany", "Spain"),
  Asia         = c("China", "Japan", "Korea", "India"),
  Oceania      = c("Australia", "NewZealand")
)
mtna(trade_matrix, clusters)
```

### 2. Organizational Departments

```r
clusters <- list(
  Engineering = c("Dev1", "Dev2", "QA1", "QA2"),
  Marketing   = c("Mkt1", "Mkt2", "Sales1"),
  Operations  = c("Ops1", "Ops2", "Support1"),
  Finance     = c("Fin1", "Fin2", "HR1")
)
mtna(communication_matrix, clusters)
```

### 3. Learning Activity Types

```r
clusters <- list(
  Reading    = c("Textbook", "Article", "Notes"),
  Practice   = c("Exercise", "Quiz", "Lab"),
  Social     = c("Discussion", "GroupWork", "PeerReview"),
  Assessment = c("Exam", "Project", "Presentation")
)
mtna(learning_transitions, clusters)
```

### 4. Biological Systems

```r
clusters <- list(
  Proteins  = c("P1", "P2", "P3", "P4"),
  Enzymes   = c("E1", "E2", "E3"),
  Receptors = c("R1", "R2", "R3"),
  Signals   = c("S1", "S2", "S3", "S4")
)
mtna(interaction_matrix, clusters)
```

---

## Comparison with Other Functions

| Feature | `mtna()` | `mlna()` | `htna()` |
|---------|----------|----------|----------|
| Visual style | 2D cluster shells | 3D stacked layers | 2D polygon |
| Edge summary | Yes (aggregated) | No | No |
| Best for | Cluster overview | Hierarchical levels | Group interactions |
| Complexity reduction | High | Low | Low |

---

## Tips

1. **Use summary edges** for dense networks to reduce visual clutter

2. **Adjust spacing** based on number of clusters:
   - 2-3 clusters: `spacing = 2-3`
   - 4-6 clusters: `spacing = 3-4`
   - 7+ clusters: `spacing = 4-5`

3. **Shape size**: Increase `shape_size` if clusters have many nodes

4. **Edge bundling**: Enable for cleaner inter-cluster edge routing

5. **Layout choice**:
   - `"circle"`: Best for 3-6 clusters
   - `"grid"`: Best for 4, 6, 9+ clusters
   - `"horizontal"`: Best for 2-4 clusters with flow
   - `"vertical"`: Best for hierarchical comparison

---

## Return Value

Returns `NULL` invisibly when `summary_edges = TRUE`, or the `plot_tna` result otherwise.

---

## See Also

- `mlna()` / `plot_mlna()` - Stacked 3D layer visualization
- `htna()` / `plot_htna()` - Polygon/bipartite layout
- `splot()` - Standard network plotting
- `detect_communities()` - Automatic community detection
