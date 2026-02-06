# TNA Animation Functions

Create animated visualizations of Transition Network Analysis (TNA) networks evolving over time.

## Overview

The TNA animation feature allows you to visualize how transition patterns change across sequential data. It works by:

1. Taking wide-format sequence data (rows = individuals, columns = time points)
2. Creating rolling windows over the time points
3. Estimating a TNA model for each window
4. Rendering each model as a frame with smooth layout transitions
5. Combining frames into an animated GIF

## Functions

### `tna_windows()` - Generate Windowed TNA Models

Creates a series of TNA models from rolling windows over sequence data.

```r
tna_windows(
  x,
 window_size = 5,
  step = 1,
  na_threshold = 0.5
)
```

#### Arguments

| Argument | Description |
|----------|-------------|
| `x` | Wide data.frame where rows are sequences and columns are time points. Cell values should be states (character or factor). |
| `window_size` | Number of time points per window (default: 5). Use 2 for single transitions. |
| `step` | Step size between windows. 1 = sliding windows, `window_size` = tumbling/non-overlapping (default: 1). |
| `na_threshold` | Maximum NA proportion allowed in a window. Stops when exceeded (default: 0.5). |

#### Returns

A list with components:

| Component | Description |
|-----------|-------------|
| `windows` | List of `tna` objects, one per valid window |
| `start_times` | Integer vector of window start column indices |
| `end_times` | Integer vector of window end column indices |
| `na_proportions` | Numeric vector of NA proportions per window |
| `stopped_early` | Logical indicating if stopped due to NA threshold |
| `stopped_at` | Window index where it stopped (if stopped early) |

#### Example

```r
library(tna)
data(group_regulation)

# Create windowed TNA models
result <- tna_windows(group_regulation, window_size = 5, step = 1)

# Check results
length(result$windows)
# [1] 9

result$start_times
# [1] 1 2 3 4 5 6 7 8 9

# Single transitions (window_size = 2)
result2 <- tna_windows(group_regulation, window_size = 2)
length(result2$windows)
# [1] 13
```

---

### `tna_animate()` - Create Animated GIF

Generates an animated GIF showing TNA network evolution over time.
```r
tna_animate(
  x,
  window_size = 5,
  step = 1,
  fps = 2,
  loop = 0,
  na_threshold = 0.5,
  output = "tna_animation.gif",
  width = 600,
  height = 600,
  title_template = "Time %d-%d",
  ...
)
```

#### Arguments

| Argument | Description |
|----------|-------------|
| `x` | Wide data.frame (rows = sequences, columns = time points) |
| `window_size` | Time points per window (default: 5) |
| `step` | Step between windows (default: 1) |
| `fps` | Frames per second (default: 2) |
| `loop` | Loop count, 0 = infinite (default: 0) |
| `na_threshold` | Max NA proportion before stopping (default: 0.5) |
| `output` | Output GIF file path (default: "tna_animation.gif") |
| `width` | GIF width in pixels (default: 600) |
| `height` | GIF height in pixels (default: 600) |
| `title_template` | sprintf template for frame titles (default: "Time %d-%d") |
| `...` | Additional arguments passed to `tplot()` |

#### Returns

Invisibly returns the path to the output GIF file.

#### Example

```r
library(tna)
data(group_regulation)

# Basic animation
tna_animate(group_regulation, output = "regulation.gif")

# Single transitions, slower
tna_animate(
  group_regulation,
  window_size = 2,
  fps = 1,
  output = "transitions.gif"
)

# Customize appearance
tna_animate(
  group_regulation,
  window_size = 5,
  layout = "circle",
  theme = "colorblind",
  vsize = 6,
  output = "custom.gif"
)
```

---

### `layout_spring()` - Animation-Ready Spring Layout

The Fruchterman-Reingold spring layout now supports animation with smooth transitions between frames.

```r
layout_spring(
  network,
  iterations = 500,
  cooling = 0.95,
  repulsion = 1,
  attraction = 1,
  seed = NULL,
  initial = NULL,
  max_displacement = NULL,
  anchor_strength = 0
)
```

#### New Animation Parameters

| Parameter | Description |
|-----------|-------------|
| `initial` | Previous frame's layout coordinates. Nodes start from these positions instead of random. |
| `max_displacement` | Maximum distance a node can move from initial position (e.g., 0.03-0.1). Prevents large jumps. |
| `anchor_strength` | Force pulling nodes toward initial positions (e.g., 0.5-2.0). Higher = more stable. |

#### Animation Workflow

```r
# 1. Compute initial layout (first frame)
first_net <- CographNetwork$new(tna_models[[1]]$weights)
layout <- layout_spring(first_net, seed = 42, iterations = 1000)

# 2. For each subsequent frame, use previous layout as starting point
for (i in 2:n_frames) {
  frame_net <- CographNetwork$new(tna_models[[i]]$weights)

  layout <- layout_spring(
    frame_net,
    initial = layout,           # Start from previous positions
    iterations = 50,            # Fewer iterations needed
    max_displacement = 0.03,    # Limit movement to 3% of canvas
    anchor_strength = 0.8       # Strong pull toward previous positions
  )

  # Render frame with this layout
  tplot(tna_models[[i]], layout = layout)
}
```

---

## Complete Animation Example

```r
library(cograph)
library(tna)

# Load data
data(group_regulation)

# Generate windowed TNA models
result <- tna_windows(
  group_regulation,
  window_size = 2,      # Single transitions
  step = 1,
  na_threshold = 0.5
)

n_frames <- length(result$windows)

# Setup
tmp_dir <- tempdir()
frame_files <- character(n_frames)

# Compute initial layout
first_net <- CographNetwork$new(result$windows[[1]]$weights)
current_layout <- layout_spring(first_net, seed = 42, iterations = 1000)

# Render each frame
for (i in seq_len(n_frames)) {
  frame_file <- file.path(tmp_dir, sprintf("frame_%04d.png", i))
  frame_files[i] <- frame_file

  # Update layout with smooth transition
  if (i > 1) {
    frame_net <- CographNetwork$new(result$windows[[i]]$weights)
    current_layout <- layout_spring(
      frame_net,
      initial = current_layout,
      iterations = 50,
      max_displacement = 0.025,
      anchor_strength = 1.0
    )
  }

  png(frame_file, width = 700, height = 700, res = 150)
  tplot(
    result$windows[[i]],
    title = sprintf("T%d → T%d", result$start_times[i], result$end_times[i]),
    layout = current_layout,
    theme = "colorblind",
    vsize = 5
  )
  dev.off()
}

# Combine into GIF
gifski::gifski(
  png_files = frame_files,
  gif_file = "~/Downloads/regulation_animation.gif",
  width = 700,
  height = 700,
  delay = 0.25,
  loop = 0
)
```

---

## Window Types

### Sliding Windows (step = 1)
Each window overlaps with the previous, creating smooth transitions.

```
Time:    1  2  3  4  5  6  7  8  9  10
Window 1: [--------]
Window 2:    [--------]
Window 3:       [--------]
...
```

### Tumbling Windows (step = window_size)
Non-overlapping windows for distinct time periods.

```
Time:    1  2  3  4  5  6  7  8  9  10
Window 1: [--------]
Window 2:             [--------]
```

### Single Transitions (window_size = 2)
One frame per consecutive time point pair.

```
Time:    1  2  3  4  5
Window 1: [--]
Window 2:    [--]
Window 3:       [--]
Window 4:          [--]
```

---

## Dependencies

- **tna**: Required for TNA model estimation
- **gifski**: Required for GIF creation

Both are suggested packages (not required for other cograph functionality).

```r
install.packages(c("tna", "gifski"))
```

---

## Tips

1. **NA Handling**: Use `na_threshold` to control when animation stops. Lower values = stricter, fewer frames.

2. **Smooth Animations**: Use the spring layout with `initial`, `max_displacement`, and `anchor_strength` for fluid transitions.

3. **Frame Rate**: Lower fps (1-2) for presentations, higher (4-5) for quick previews.

4. **Window Size**:
   - `window_size = 2`: Individual transitions (most frames)
   - `window_size = 5-10`: Aggregated patterns (smoother, fewer frames)

5. **Layout Stability**: Higher `anchor_strength` (0.8-1.5) keeps nodes more stable; lower `max_displacement` (0.02-0.05) limits per-frame movement.
