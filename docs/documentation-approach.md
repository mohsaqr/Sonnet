# Documentation Approach

A guide for organizing R package documentation to maximize usability and discoverability.

## Core Principles

1. **Plotting functions first**: The main plotting function is the package's primary interface—document it immediately after Quick Start
2. **User-first hierarchy**: Primary user-facing functions appear prominently; internal/advanced functions are de-emphasized
3. **Logical grouping**: Functions organized by purpose, not alphabetically
4. **Progressive disclosure**: Basic usage upfront, advanced details available but not intrusive
5. **Minimal redundancy**: Aliases mentioned once, not documented separately

## Document Structure

### 1. Quick Start
- Immediate working example (3-5 lines)
- Shows the most common use case
- No explanation needed—code speaks

### 2. Plotting Functions (First Priority)

Plotting is the primary output—document immediately after Quick Start:

| Subsection | Content |
|------------|---------|
| **Core plots** | Main plotting function and variants |
| **Specialized plots** | Multi-group, bipartite, multilevel visualizations |
| **Comparison plots** | Difference networks, heatmaps |

### 3. Other Primary Functions (by Category)

Group remaining functions by purpose:

| Category | Content |
|----------|---------|
| **Input/Output** | Import and export functions |
| **Analysis** | Computation and statistics |
| **Workflow helpers** | Piping, chaining, convenience functions |

For each category:
- Brief description (1 sentence)
- Table of functions with one-line descriptions
- Code example showing typical usage

### 4. Integration Sections
- Document compatibility with other packages
- Show conversion workflows
- Include working examples

### 5. Customization Reference
- List available options (themes, palettes, etc.)
- Tables over prose for scannability
- Brief examples

### 6. Advanced/Internal Functions
Use collapsible sections (`<details>`) for:
- Low-level accessors and setters
- Registry/extension functions
- Verification utilities

These are documented but don't clutter the main reference.

### 7. Quick Reference Table
End with a summary table mapping categories to key functions.

## Formatting Guidelines

- **Tables** for function lists (scannable)
- **Code blocks** for examples (copy-paste ready)
- **Horizontal rules** between major sections
- **No emojis** in technical documentation
- **Consistent column widths** in tables

## What NOT to Document Prominently

- Aliases (mention once, refer to canonical name)
- Internal helpers (prefix with `.` and don't export, or collapse)
- Deprecated functions (brief note with replacement)
- R6 classes (users interact via functions, not classes)

## Example Section Template

```markdown
## Category Name

Brief description of what these functions do.

| Function | Description |
|----------|-------------|
| `main_fn()` | Primary function for X |
| `variant_fn()` | Variant for Y situations |

\```r
# Typical usage
result <- main_fn(input)
result <- variant_fn(input, option = TRUE)
\```
```

## Maintenance

- Review quarterly for deprecated/new functions
- Keep examples runnable (use `eval = FALSE` if dependencies are heavy)
- Update Quick Reference table when adding new categories
