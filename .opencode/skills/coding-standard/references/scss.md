# SCSS Coding Standard

## Spacing Scale

Always use spacing variables, never hardcoded pixels:

```scss
// ✅ GOOD
.card {
  padding: $space-lg;
  margin-bottom: $space-md;
  gap: $space-sm;
}

// ❌ BAD
.card {
  padding: 24px;
  margin-bottom: 16px;
  gap: 8px;
}
```

| Variable | Value | Use For |
|----------|-------|---------|
| `$space-xs` | 4px | Icon margins, tight gaps |
| `$space-sm` | 8px | Small gaps, button padding |
| `$space-md` | 16px | Standard gaps, section padding |
| `$space-lg` | 24px | Large gaps, card padding |
| `$space-xl` | 32px | Section margins |
| `$space-2xl` | 48px | Major section separations |

## Colors

Use CSS variables for all colors to support theming:

```scss
// ✅ GOOD
.card {
  background: var(--color-surface);
  color: var(--color-text);
  border: 1px solid var(--color-border);
  
  &:hover {
    border-color: var(--color-primary);
  }
}

// ❌ BAD
.card {
  background: #f7f3e3;
  color: #1a1a1a;
  border: 1px solid #e8e0c8;
}
```

| Variable | Light | Dark | Use For |
|----------|-------|------|---------|
| `--color-bg` | #fffcf0 | #1a1a2e | Page background |
| `--color-text` | #1a1a1a | #e2e8f0 | Primary text |
| `--color-primary` | #3b82f6 | #60a5fa | Links, accents |
| `--color-secondary` | #64748b | #94a3b8 | Meta text |
| `--color-surface` | #f7f3e3 | #252540 | Cards, elevated |
| `--color-border` | #e8e0c8 | #374151 | Borders |

## Placeholders for Reuse

Extract shared patterns into placeholders:

```scss
// ✅ GOOD: Define once, extend everywhere
%chip {
  display: inline-block;
  padding: $space-xs $space-sm;
  background: var(--color-surface);
  border: 1px solid var(--color-border);
  border-radius: 4px;
  
  &:hover {
    color: var(--color-primary);
    border-color: var(--color-primary);
  }
}

.tag { @extend %chip; }
.friend-link { @extend %chip; }

// ❌ BAD: Copy-paste
.tag {
  display: inline-block;
  padding: 4px 8px;
  // ... duplicate styles
}
.friend-link {
  display: inline-block;
  padding: 4px 8px;
  // ... same styles again
}
```

## BEM-like Naming

Use `&-` for child elements:

```scss
// ✅ GOOD
.post-list {
  &-year { ... }
  &-item { ... }
  &-title { ... }
  &-date { ... }
}

// ❌ BAD (disconnected naming)
.post-list { ... }
.year-section { ... }
.item { ... }
```

## Media Queries

Use breakpoint variables:

```scss
// ✅ GOOD
@media (max-width: $breakpoint-md) {
  .nav { ... }
}

// ❌ BAD
@media (max-width: 768px) {
  .nav { ... }
}
```

| Variable | Value |
|----------|-------|
| `$breakpoint-sm` | 480px |
| `$breakpoint-md` | 768px |
| `$breakpoint-lg` | 1024px |

## Hover States

Always use `--color-primary` for interactive states:

```scss
// ✅ GOOD
.link {
  color: var(--color-text);
  
  &:hover {
    color: var(--color-primary);
  }
}

.card {
  border: 1px solid var(--color-border);
  
  &:hover {
    border-color: var(--color-primary);
    transform: translateY(-2px);
  }
}

// ❌ BAD (inconsistent hover colors)
.link:hover { color: blue; }
.card:hover { border-color: #ff0000; }
```

## File Organization

```
static/scss/
├── _variables.scss   # Design tokens (spacing, breakpoints)
├── _components.scss  # All component styles
├── _syntax.scss      # Code syntax highlighting
└── style.scss        # Base styles, theme setup, imports
```

## Section Comments

Mark major sections with comment headers:

```scss
// ============================================================================
// Navigation
// ============================================================================

.nav { ... }
.nav-links { ... }

// ============================================================================
// Post List
// ============================================================================

.post-list { ... }
```

## Validation Commands

```bash
# Check for hardcoded pixels (excluding media queries)
grep -E "[0-9]+px" static/scss/*.scss | grep -v "//" | grep -v "@media"

# Check for hardcoded colors
grep -E "#[0-9a-fA-F]{3,6}" static/scss/*.scss | grep -v "//"

# Both should return minimal/no results
```
