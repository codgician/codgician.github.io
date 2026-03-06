# Content Search for codgician.github.io

## TL;DR

> **Quick Summary**: Add self-contained, offline content search using Pagefind (build-time indexer) with an always-visible search input in the nav bar, dropdown results panel, and optional Chrome Prompt API semantic re-ranking as a progressive enhancement.
> 
> **Deliverables**:
> - Pagefind integrated into Nix build pipeline (post-build indexing)
> - Always-visible search input in the nav bar with dropdown results
> - Custom search UI matching site's minimalist dark/light theme
> - Bilingual search (auto-separated en/zh indexes)
> - Progressive semantic re-ranking via Chrome Prompt API (graceful fallback)
> 
> **Estimated Effort**: Medium
> **Parallel Execution**: YES - 4 waves
> **Critical Path**: Task 1 → Task 3 → Task 5 → Task 7 → Task 8 → Final Verification

---

## Context

### Original Request
Add content search to this Hakyll-based bilingual static site with requirements:
- Self-contained: No external services, works offline on local machine
- Efficient: No bloat to repository or built artifacts
- Nice-to-have: Leverage modern browser AI capability for semantic search

### Interview Summary
**Key Discussions**:
- Search library: **Pagefind** selected after comparing 6 options (Lunr.js, Fuse.js, MiniSearch, Pagefind, Stork, TinySearch). Pagefind wins on CJK support, index efficiency, and zero-Hakyll-code-change integration.
- UI style: **Always-visible search input in nav bar** (even on mobile), with **dropdown results panel** below. Custom UI via Pagefind's JS API (not PagefindUI).
- Semantic enhancement: **Chrome Prompt API** (Gemini Nano) for re-ranking keyword results. Zero additional download. Progressive enhancement with graceful fallback.
- Nix integration: `pkgs.pagefind` confirmed available in nixpkgs-unstable. Follows existing subprocess pattern (KaTeX, Mermaid, dart-sass).

**Research Findings**:
- Site already has `<html lang="$lang$">` — Pagefind auto-detects languages
- Pagefind creates chunked indexes: ~6KB JS core + chunks on-demand (~100KB total search session)
- Pagefind has built-in `debouncedSearch()` and `preload()` — no custom debounce needed
- Chrome Prompt API renamed to `LanguageModel` global (not `window.ai`)
- Gemini Nano has 1024 token/prompt limit — re-ranking must use titles only, max 5 results
- Model download requires user activation — must skip AI if model not already `'available'`

### Metis Review
**Identified Gaps** (addressed):
- Chrome AI API surface was outdated — corrected to `LanguageModel` global
- Token limit constrains re-ranking strategy — revised to titles-only, max 5 results
- CJK IME composition events need handling — added `compositionend` event directive
- Pagefind sub_results provide section-level linking — incorporated as free win
- Model download activation requirement — revised to skip AI unless already available

---

## Work Objectives

### Core Objective
Add a fully self-contained, offline-capable content search system that supports bilingual (en/zh) content with optional browser AI semantic enhancement.

### Concrete Deliverables
- `flake.nix`: Pagefind added to build tools and build phase
- `templates/partials/nav.html`: Search input element added
- `static/js/search.js`: Custom search logic using Pagefind JS API + Chrome Prompt API
- `static/scss/_components.scss`: Search input and results dropdown styles
- `templates/default.html`: search.js script included
- Content templates: `data-pagefind-body` / `data-pagefind-ignore` attributes

### Definition of Done
- [ ] `nix build` succeeds and output contains `pagefind/` directory
- [ ] Search input visible in nav bar on both desktop and mobile
- [ ] Typing English query returns relevant English posts in dropdown
- [ ] Typing Chinese query returns relevant Chinese posts in dropdown
- [ ] Results link to correct post URLs (section-level when available)
- [ ] Search works fully offline (no external network requests)
- [ ] Dark/light theme switch correctly styles search components
- [ ] Chrome Prompt API re-ranking works when available, degrades silently when not

### Must Have
- Pagefind CLI runs during `nix build` as post-Hakyll step
- Always-visible search input in nav bar
- Dropdown results panel with highlighted excerpts
- Per-language search (en searches en content, zh searches zh content)
- Keyboard accessibility (Escape closes, arrow keys navigate)
- CJK IME composition event handling
- Works offline with zero external service dependencies

### Must NOT Have (Guardrails)
- No external search services (Algolia, ElasticSearch, etc.)
- No PagefindUI default component — custom UI only
- No JS bundler/build tool — raw JS files matching existing pattern
- No heavy ML model downloads in browser (>5MB)
- No UI indicator showing "AI mode" vs "keyword mode" — transparent to user
- No model download prompts — if Gemini Nano not already available, skip silently
- No server-side search infrastructure
- No `as any`, `@ts-ignore`, or suppressed type errors in JS
- No hardcoded colors — use existing CSS variables
- No changes to existing post content files

---

## Verification Strategy

> **ZERO HUMAN INTERVENTION** — ALL verification is agent-executed. No exceptions.

### Test Decision
- **Infrastructure exists**: YES (Haskell hspec tests, nix flake checks)
- **Automated tests**: Tests-after (for Haskell pipeline changes only)
- **Framework**: Existing hspec for Haskell; JS verified via QA scenarios
- **Approach**: If any Hakyll rules change in Site.hs, add/update corresponding hspec tests

### QA Policy
Every task MUST include agent-executed QA scenarios.
Evidence saved to `.sisyphus/evidence/task-{N}-{scenario-slug}.{ext}`.

- **Frontend/UI**: Use Playwright (playwright skill) — Navigate, interact, assert DOM, screenshot
- **Build pipeline**: Use Bash — Run `nix build`, verify output structure
- **Search functionality**: Use Playwright — Type queries, verify results, test keyboard nav

---

## Execution Strategy

### Parallel Execution Waves

```
Wave 1 (Foundation — Nix + Pagefind build integration):
├── Task 1: Add Pagefind to Nix flake build pipeline [quick]
├── Task 2: Add data-pagefind-body/ignore attributes to templates [quick]

Wave 2 (Search UI — all independent of each other):
├── Task 3: Add search input to nav bar HTML [quick]
├── Task 4: Style search input + results dropdown in SCSS [visual-engineering]

Wave 3 (Search Logic — depends on Wave 1+2):
├── Task 5: Implement search.js with Pagefind JS API [unspecified-high]
├── Task 6: Implement Chrome Prompt API re-ranking [deep]

Wave 4 (Integration + Polish):
├── Task 7: Wire everything together + keyboard accessibility [unspecified-high]
├── Task 8: GitHub Actions build integration [quick]

Wave FINAL (Verification — 4 parallel):
├── Task F1: Plan compliance audit (oracle)
├── Task F2: Code quality review (unspecified-high)
├── Task F3: Real manual QA (unspecified-high)
├── Task F4: Scope fidelity check (deep)

Critical Path: Task 1 → Task 2 → Task 5 → Task 7 → Task 8 → Final
Parallel Speedup: ~50% faster than sequential
Max Concurrent: 2 (Waves 2, 3)
```

### Dependency Matrix

| Task | Depends On | Blocks |
|------|-----------|--------|
| 1 | — | 2, 5, 8 |
| 2 | 1 (needs pagefind to verify) | 5 |
| 3 | — | 7 |
| 4 | — | 7 |
| 5 | 1, 2 | 6, 7 |
| 6 | 5 | 7 |
| 7 | 3, 4, 5, 6 | 8 |
| 8 | 7 | F1-F4 |
| F1-F4 | 8 | — |

### Agent Dispatch Summary

- **Wave 1**: 2 tasks — T1 → `quick`, T2 → `quick`
- **Wave 2**: 2 tasks — T3 → `quick`, T4 → `visual-engineering`
- **Wave 3**: 2 tasks — T5 → `unspecified-high`, T6 → `deep`
- **Wave 4**: 2 tasks — T7 → `unspecified-high`, T8 → `quick`
- **FINAL**: 4 tasks — F1 → `oracle`, F2 → `unspecified-high`, F3 → `unspecified-high`, F4 → `deep`

---

## TODOs

- [ ] 1. Add Pagefind to Nix flake build pipeline

  **What to do**:
  - Add `pkgs.pagefind` to the `buildTools` list in `flake.nix` (around line 50)
  - In the `buildPhase` of the `website` derivation, add `pagefind --site _site` AFTER `site build` (after line 130)
  - In the `siteWrapper` script, add `pagefind` to the PATH or call it after `site` commands for dev workflow
  - Verify `nix build` produces `_site/pagefind/` directory with index files
  - For the dev shell: ensure `pagefind` is available so `nix run . -- watch` can be followed by manual `pagefind --site _site`

  **Must NOT do**:
  - Do not add pagefind via npm/npx — use the Nix package `pkgs.pagefind`
  - Do not modify the Hakyll Haskell source (src/) — pagefind runs on HTML output only
  - Do not add a separate build script — keep everything in `flake.nix`

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Single-file Nix configuration change with clear insertion points
  - **Skills**: []
    - No special skills needed — this is a Nix flake edit
  - **Skills Evaluated but Omitted**:
    - `git-master`: No git operations in this task

  **Parallelization**:
  - **Can Run In Parallel**: NO (must complete before Task 2)
  - **Parallel Group**: Wave 1 (sequential with Task 2)
  - **Blocks**: Tasks 2, 5, 8
  - **Blocked By**: None (can start immediately)

  **References**:

  **Pattern References**:
  - `flake.nix:50-55` — Existing `buildTools` list where `pkgs.pagefind` should be added (alongside `pkgs.dart-sass`, `pkgs.nodePackages.katex`, etc.)
  - `flake.nix:113-132` — Existing `buildPhase` where `pagefind --site _site` should be added after `site build` (line 130)
  - `flake.nix:164-172` — `devTools` list where `pkgs.pagefind` should also be added for development
  - `flake.nix:175-187` — `siteWrapper` script pattern — Pagefind needs to be available here for `nix run . -- watch` workflow

  **API/Type References**:
  - Pagefind CLI: `pagefind --site <dir>` — the only command needed. Output goes to `<dir>/pagefind/`

  **External References**:
  - Pagefind CLI docs: https://pagefind.app/docs/config-options/ — CLI flags and config options
  - Pagefind multilingual docs: https://pagefind.app/docs/multilingual/ — auto-detects `<html lang="">`

  **WHY Each Reference Matters**:
  - `buildTools` is where ALL external CLI tools go — this is the canonical pattern for adding tools
  - `buildPhase` runs `site build` then copies vendor assets — pagefind must run AFTER `site build` but BEFORE `installPhase`
  - `devTools` provides tools in the dev shell — pagefind needed for manual indexing during development
  - `siteWrapper` is the dev entry point — pagefind should be on PATH but NOT automatically run (it's a separate manual step after `site watch`)

  **Acceptance Criteria**:
  - [ ] `pkgs.pagefind` appears in `buildTools` list in flake.nix
  - [ ] `pagefind --site _site` runs in buildPhase after `site build`
  - [ ] `nix build` succeeds and `result/pagefind/` directory exists
  - [ ] `result/pagefind/pagefind.js` file exists
  - [ ] `nix develop` shell has `pagefind` command available

  **QA Scenarios:**

  ```
  Scenario: Nix build produces pagefind index
    Tool: Bash
    Preconditions: Clean checkout, no previous build artifacts
    Steps:
      1. Run `nix build`
      2. Run `ls result/pagefind/`
      3. Assert `pagefind.js` exists in output
      4. Assert at least one `.pf_index` or `.pf_meta` file exists
    Expected Result: Build succeeds, pagefind directory contains JS and index files
    Failure Indicators: Build fails, pagefind/ directory missing, no index files
    Evidence: .sisyphus/evidence/task-1-nix-build-pagefind.txt

  Scenario: Dev shell has pagefind available
    Tool: Bash
    Preconditions: None
    Steps:
      1. Run `nix develop --command pagefind --version`
      2. Assert output contains version number (e.g., "pagefind 1.")
    Expected Result: pagefind command available and reports version
    Failure Indicators: Command not found
    Evidence: .sisyphus/evidence/task-1-dev-shell-pagefind.txt
  ```

  **Commit**: YES
  - Message: `feat(build): add Pagefind to Nix build pipeline`
  - Files: `flake.nix`
  - Pre-commit: `nix build`

- [ ] 2. Add data-pagefind attributes to content templates

  **What to do**:
  - Add `data-pagefind-body` attribute to the `<main>` element in `templates/default.html` (line 8) — this tells Pagefind to index the main content area
  - Add `data-pagefind-ignore` attribute to navigation (`templates/partials/nav.html` line 1), footer (`templates/partials/footer.html`), and actions (`templates/partials/actions.html` line 1) — excludes non-content from index
  - In `templates/post.html`: ensure post title/metadata region has `data-pagefind-meta="title"` if not auto-detected from `<h1>`
  - Add `data-pagefind-ignore` to the future search results dropdown (so search results don't index themselves)
  - Verify that after rebuilding, Pagefind indexes only content (not nav text, footer, etc.)

  **Must NOT do**:
  - Do not modify any Haskell source files
  - Do not change content markdown files
  - Do not add `data-pagefind-body` to non-content areas (slides, homepage hero, etc. are fine to include)

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Simple HTML attribute additions across a few template files
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `visual-engineering`: No visual changes, just HTML attributes

  **Parallelization**:
  - **Can Run In Parallel**: NO (needs Task 1 to verify indexing works)
  - **Parallel Group**: Wave 1 (after Task 1)
  - **Blocks**: Task 5
  - **Blocked By**: Task 1

  **References**:

  **Pattern References**:
  - `templates/default.html:8` — `<main class="main">` — add `data-pagefind-body` here
  - `templates/default.html:7` — Nav partial include — nav already separate, add `data-pagefind-ignore` to nav.html root
  - `templates/default.html:11` — Footer partial include — add `data-pagefind-ignore` to footer.html root
  - `templates/partials/nav.html:1` — `<nav class="nav">` — add `data-pagefind-ignore`
  - `templates/partials/actions.html:1` — `<div class="actions">` — add `data-pagefind-ignore`
  - `templates/partials/footer.html` — footer root element — add `data-pagefind-ignore`

  **API/Type References**:
  - Pagefind: `data-pagefind-body` marks indexable content regions
  - Pagefind: `data-pagefind-ignore` excludes elements from index
  - Pagefind: `data-pagefind-meta="key:value"` adds custom metadata to search results

  **External References**:
  - Pagefind indexing docs: https://pagefind.app/docs/indexing/

  **WHY Each Reference Matters**:
  - `<main>` is the content wrapper — marking it as `data-pagefind-body` ensures only post/page content is indexed
  - Nav/footer/actions contain repeated text across all pages — indexing them would pollute results

  **Acceptance Criteria**:
  - [ ] `<main>` element in default.html has `data-pagefind-body` attribute
  - [ ] `<nav>` element has `data-pagefind-ignore` attribute
  - [ ] Footer element has `data-pagefind-ignore` attribute
  - [ ] `nix build` succeeds
  - [ ] Pagefind index contains post content but not nav/footer text

  **QA Scenarios:**

  ```
  Scenario: Pagefind indexes only content, not nav/footer
    Tool: Bash
    Preconditions: Task 1 complete (pagefind in build)
    Steps:
      1. Run `nix build`
      2. Serve result/ with a static server
      3. In a temporary Node script or browser console, load pagefind and search for site title or nav link text
      4. Assert: searching for a known post title returns results
      5. Assert: searching for nav-only text (e.g., exact nav link label) returns zero or very few results
    Expected Result: Post content is indexed; nav/footer boilerplate is not
    Failure Indicators: Nav text appears as search results, or post content missing from index
    Evidence: .sisyphus/evidence/task-2-pagefind-content-scope.txt
  ```

  **Commit**: YES (groups with Task 1)
  - Message: `feat(build): add Pagefind search indexing to Nix build pipeline`
  - Files: `flake.nix`, `templates/default.html`, `templates/partials/nav.html`, `templates/partials/footer.html`, `templates/partials/actions.html`
  - Pre-commit: `nix build`

---

- [ ] 3. Add search input to nav bar HTML template

  **What to do**:
  - Add a search input element to the nav bar in `templates/partials/nav.html` — positioned between `.nav-links` and the `.actions` partial include
  - HTML structure:
    ```html
    <div class="nav-search" data-pagefind-ignore>
      <i class="icon-search"></i>
      <input type="search" class="nav-search-input" placeholder="Search..." autocomplete="off" />
      <div class="nav-search-results" hidden></div>
    </div>
    ```
  - The `placeholder` text should be translatable — use Hakyll context variable `$searchPlaceholder$` if available, or hardcode with `Search...` / `搜索...` based on `$lang$`
  - Add `data-pagefind-ignore` to the search container (so results dropdown content doesn't get indexed)
  - The search icon should use the existing Lucide icon font (check `static/vendor/lucide/` for available icons — `icon-search` if available)
  - Add the `<script src="/js/search.js"></script>` tag to `templates/default.html` — place it after the existing scripts (after line 14)

  **Must NOT do**:
  - Do not use Pagefind's default UI component (PagefindUI)
  - Do not add a bundler or build step for JS
  - Do not import search.js as a module — use plain `<script>` tag matching existing pattern

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Small HTML template changes across 2 files
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `visual-engineering`: Template structure only, styling is Task 4

  **Parallelization**:
  - **Can Run In Parallel**: YES (parallel with Task 4)
  - **Parallel Group**: Wave 2 (with Task 4)
  - **Blocks**: Task 7
  - **Blocked By**: None (HTML structure independent of Pagefind build)

  **References**:

  **Pattern References**:
  - `templates/partials/nav.html:1-9` — Current nav structure: logo → nav-links → actions. Search input goes between nav-links and actions.
  - `templates/partials/actions.html:1-22` — Actions pattern: flex container with icon buttons. Search container follows similar DOM nesting.
  - `templates/default.html:12-14` — Existing script includes: theme.js, nav.js, toc.js. search.js goes after these.
  - `static/vendor/lucide/` — Lucide icon font with `icon-*` CSS classes. Check for `icon-search` availability.

  **API/Type References**:
  - `config.yaml` — Check if translated strings mechanism exists for search placeholder text
  - `src/Context.hs` — Check `transStr` helper for how i18n strings are provided to templates

  **External References**:
  - Lucide icons reference: check which icons are available in the bundled lucide font subset

  **WHY Each Reference Matters**:
  - Nav template is the insertion point — must understand existing structure to place search input correctly
  - Actions pattern shows how interactive elements are structured in this codebase
  - Script includes show the pattern for adding new JS files (plain script tags, no modules)

  **Acceptance Criteria**:
  - [ ] Search input element exists in nav.html between nav-links and actions
  - [ ] `data-pagefind-ignore` attribute on search container
  - [ ] `search.js` script tag in default.html
  - [ ] Search input has appropriate placeholder text
  - [ ] Search results container div exists (hidden by default)
  - [ ] HTML validates (no nesting errors)

  **QA Scenarios:**

  ```
  Scenario: Search input visible in nav bar
    Tool: Playwright (playwright skill)
    Preconditions: Site built and served locally
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Assert `.nav-search-input` element is visible in viewport
      3. Assert `.nav-search-input` has placeholder text
      4. Navigate to http://127.0.0.1:8000/zh/
      5. Assert `.nav-search-input` element is visible
    Expected Result: Search input visible on both language versions
    Failure Indicators: Element missing, not visible, or no placeholder
    Evidence: .sisyphus/evidence/task-3-search-input-visible.png

  Scenario: Search input visible on mobile viewport
    Tool: Playwright (playwright skill)
    Preconditions: Site served locally
    Steps:
      1. Set viewport to 375x667 (iPhone SE)
      2. Navigate to http://127.0.0.1:8000/en/
      3. Assert `.nav-search-input` element is visible
      4. Assert nav bar is not overflowing horizontally
    Expected Result: Search input fits within mobile nav without overflow
    Failure Indicators: Horizontal scroll, input hidden, nav items overlapping
    Evidence: .sisyphus/evidence/task-3-search-input-mobile.png
  ```

  **Commit**: YES
  - Message: `feat(search): add search input to nav bar template`
  - Files: `templates/partials/nav.html`, `templates/default.html`
  - Pre-commit: `nix build`

- [ ] 4. Style search input and results dropdown in SCSS

  **What to do**:
  - Add search-related styles to `static/scss/_components.scss` — add a new section `// Search` following the existing pattern of commented section headers
  - Style `.nav-search` container: flex layout, positioned between nav-links and actions in the nav flex row, with appropriate sizing
  - Style `.nav-search-input`: matches site's minimalist design — transparent or `var(--color-surface)` background, `var(--color-border)` border, `var(--color-text)` text, rounded corners matching action buttons (0.375rem), appropriate padding
  - Style `.nav-search-results` dropdown: absolute positioned below the search input, `var(--color-surface)` background, `var(--color-border)` border, rounded corners, shadow for depth, z-index above content but below nav (use z-index relative to nav's 1000)
  - Each result item: title, URL/breadcrumb, excerpt with `<mark>` highlight styling
  - `<mark>` within results: highlight with `var(--color-primary)` or accent color
  - Hover/focus states on result items: subtle background change
  - Active/selected result item: visual indicator for keyboard navigation
  - Mobile responsive: on small screens, results dropdown should be full-width of viewport
  - Dark/light theme: ALL colors must use CSS variables — zero hardcoded colors
  - Use existing spacing scale: `$space-xs`, `$space-sm`, `$space-md`, `$space-lg`
  - Max height on results dropdown with overflow-y: auto for scrolling

  **Must NOT do**:
  - No hardcoded color values — CSS variables only
  - No Pagefind default CSS — fully custom styles
  - No new SCSS file — add to existing `_components.scss`
  - No pixels for spacing — use `$space-*` variables

  **Recommended Agent Profile**:
  - **Category**: `visual-engineering`
    - Reason: UI/styling task requiring design sensibility for the minimalist aesthetic
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `frontend-design`: This is incremental CSS, not a new design

  **Parallelization**:
  - **Can Run In Parallel**: YES (parallel with Task 3)
  - **Parallel Group**: Wave 2 (with Task 3)
  - **Blocks**: Task 7
  - **Blocked By**: None

  **References**:

  **Pattern References**:
  - `static/scss/_variables.scss` — All CSS variables (`--color-bg`, `--color-text`, `--color-primary`, `--color-surface`, `--color-border`, `--color-secondary`) and spacing scale (`$space-xs` through `$space-xl`)
  - `static/scss/_components.scss:27-103` — `.actions` and `.actions-btn` styles — match button sizing (2.25rem), border style, border-radius (0.375rem), transition patterns
  - `static/scss/_components.scss:125-202` — `.nav` styles — flex layout, mobile breakpoint handling, z-index (1000)
  - `static/scss/_components.scss:59-102` — `.actions-lang-menu` dropdown — reference for dropdown positioning pattern (absolute, top: 100%, border, shadow, transition)

  **External References**:
  - Pagefind JS API response format: results have `title`, `excerpt` (with `<mark>` tags), `url`, `sub_results[]`

  **WHY Each Reference Matters**:
  - Variables file defines the ENTIRE color/spacing system — mandatory to use for theme consistency
  - Actions/nav styles define the visual language (border radius, transitions, sizing) — search must match
  - Lang dropdown is the closest existing pattern to a dropdown panel — reuse positioning approach

  **Acceptance Criteria**:
  - [ ] `.nav-search` styles added to _components.scss
  - [ ] Search input matches site's visual language (border radius, colors, sizing)
  - [ ] Results dropdown positioned correctly below input
  - [ ] `<mark>` highlight styling defined
  - [ ] Hover/focus/active states on result items
  - [ ] Zero hardcoded colors — all CSS variables
  - [ ] Responsive: dropdown works on mobile (375px viewport)
  - [ ] Dark theme: search elements styled correctly
  - [ ] Light theme: search elements styled correctly

  **QA Scenarios:**

  ```
  Scenario: Search UI matches site theme (dark mode)
    Tool: Playwright (playwright skill)
    Preconditions: Site served locally
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Ensure dark theme is active (check `data-theme="dark"` on html)
      3. Screenshot `.nav-search` area
      4. Assert `.nav-search-input` has appropriate dark theme colors (no white backgrounds)
      5. Click on search input, type "test"
      6. If results appear, screenshot the dropdown
      7. Assert dropdown background uses surface color, text uses text color
    Expected Result: All search elements use theme-appropriate colors
    Failure Indicators: White backgrounds in dark mode, unreadable text, no visible borders
    Evidence: .sisyphus/evidence/task-4-search-dark-theme.png

  Scenario: Search UI matches site theme (light mode)
    Tool: Playwright (playwright skill)
    Preconditions: Site served locally
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Toggle to light theme (click theme toggle button)
      3. Screenshot `.nav-search` area
      4. Assert search elements use light theme colors
    Expected Result: Search elements adapt to light theme
    Failure Indicators: Dark backgrounds in light mode, invisible elements
    Evidence: .sisyphus/evidence/task-4-search-light-theme.png
  ```

  **Commit**: YES
  - Message: `feat(search): style search input and results dropdown`
  - Files: `static/scss/_components.scss`
  - Pre-commit: `nix build`

- [ ] 5. Implement search.js with Pagefind JS API

  **What to do**:
  - Create `static/js/search.js` — the core search logic file
  - Dynamic import: `const pagefind = await import('/pagefind/pagefind.js')` on first search interaction
  - Initialize Pagefind with `await pagefind.options({ excerptLength: 20 })` after import
  - On search input `input` event: call `pagefind.preload(query)` for index warming
  - On search input: call `pagefind.debouncedSearch(query, {}, 200)` for actual results
  - Handle `null` return from `debouncedSearch` (means a newer search superseded this one — ignore)
  - For each result, call `await result.data()` to get title, excerpt, url, sub_results
  - CJK IME handling: track composition state with `compositionstart`/`compositionend` events
    - During composition (`isComposing = true`): only call `preload()`, NOT `debouncedSearch()`
    - On `compositionend`: trigger the actual `debouncedSearch()` with final composed text
  - Render results into `.nav-search-results` container:
    - Each result: title (as link), excerpt with `<mark>` highlights (Pagefind provides these)
    - Use `sub_results[0].url` when available for section-level linking (deep link to matching heading)
  - Show/hide results dropdown: show when results exist and input focused, hide on blur (with delay for click) or Escape
  - Empty state: show "No results found" message when search returns 0 results
  - Empty input: hide results dropdown entirely
  - Loading state: show brief indicator while Pagefind loads for the first time

  **Must NOT do**:
  - Do not use PagefindUI component — custom rendering only
  - Do not implement custom debounce — use Pagefind’s built-in `debouncedSearch()`
  - Do not use ES modules syntax — match existing pattern of plain script files (IIFE or top-level)
  - Do not make any external network requests
  - Do not add console.log in production code

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Non-trivial JS with async patterns, CJK handling, and DOM manipulation
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `frontend-design`: This is logic, not design
    - `playwright`: QA only, not needed for implementation

  **Parallelization**:
  - **Can Run In Parallel**: NO (needs Wave 1 complete)
  - **Parallel Group**: Wave 3 (parallel with Task 6 partially — Task 6 depends on this)
  - **Blocks**: Tasks 6, 7
  - **Blocked By**: Tasks 1, 2

  **References**:

  **Pattern References**:
  - `static/js/theme.js` — Example of existing JS file pattern: IIFE or DOMContentLoaded, DOM queries, event listeners. search.js must follow the same style.
  - `static/js/nav.js` — Shows how nav interactions are handled (scroll hide/show). Search should coexist without conflict.
  - `static/js/toc.js` — Shows toggle pattern (show/hide panel). Results dropdown follows similar show/hide logic.

  **API/Type References**:
  - Pagefind JS API: `import('/pagefind/pagefind.js')` returns module with `.search()`, `.debouncedSearch()`, `.preload()`, `.options()`
  - `debouncedSearch(query, options, timeoutMs)` — returns `{ results: [{ id, data: () => Promise<{url, title, excerpt, sub_results}> }] }` or `null`
  - `preload(term)` — pre-fetches index chunks, no return value
  - `result.data()` — async, returns `{ url, excerpt, title, sub_results: [{url, title, excerpt}] }`
  - Excerpt contains `<mark>` tags around matching terms

  **External References**:
  - Pagefind JS API docs: https://pagefind.app/docs/api/
  - Pagefind search options: https://pagefind.app/docs/search-config/

  **WHY Each Reference Matters**:
  - Existing JS files define the coding style — search.js must match (no TypeScript, no modules, same DOM patterns)
  - Pagefind API docs specify the exact response shape — essential for rendering results correctly
  - `debouncedSearch` returning `null` is a critical edge case that must be handled

  **Acceptance Criteria**:
  - [ ] `static/js/search.js` exists and follows existing JS file patterns
  - [ ] Pagefind dynamically imported on first interaction (not on page load)
  - [ ] `preload()` called on every input event for index warming
  - [ ] `debouncedSearch()` used (not custom debounce)
  - [ ] `null` returns from debouncedSearch handled (ignored)
  - [ ] CJK composition events handled (`compositionstart`/`compositionend`)
  - [ ] Results rendered with title, excerpt (with `<mark>`), and correct URL
  - [ ] Sub-results used for section-level linking when available
  - [ ] Results dropdown shows/hides appropriately
  - [ ] Empty state message shown for zero results
  - [ ] No console.log statements in final code

  **QA Scenarios:**

  ```
  Scenario: English keyword search returns results
    Tool: Playwright (playwright skill)
    Preconditions: Site built with pagefind, served at http://127.0.0.1:8000
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Click on `.nav-search-input`
      3. Type a known post title keyword (e.g., first word of an existing post title)
      4. Wait up to 3s for `.nav-search-results` to become visible
      5. Assert at least 1 result item is present in the dropdown
      6. Assert result item contains a title and an excerpt with `<mark>` element
      7. Click the first result
      8. Assert URL changed to a valid post path (matches `/en/posts/*/`)
    Expected Result: Search returns relevant results, clicking navigates to post
    Failure Indicators: No results appear, results lack titles/excerpts, navigation fails
    Evidence: .sisyphus/evidence/task-5-english-search.png

  Scenario: Chinese keyword search returns Chinese results
    Tool: Playwright (playwright skill)
    Preconditions: Site built with pagefind, served at http://127.0.0.1:8000
    Steps:
      1. Navigate to http://127.0.0.1:8000/zh/
      2. Click on `.nav-search-input`
      3. Type a Chinese keyword from a known Chinese post title
      4. Wait up to 3s for `.nav-search-results` to become visible
      5. Assert at least 1 result item is present
      6. Assert result URL matches `/zh/posts/*/`
    Expected Result: Chinese search returns Chinese-language results
    Failure Indicators: No results, English results shown on Chinese page
    Evidence: .sisyphus/evidence/task-5-chinese-search.png

  Scenario: Empty search shows no results
    Tool: Playwright (playwright skill)
    Preconditions: Site served locally
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Click on `.nav-search-input`
      3. Type "xyznonexistentquery12345"
      4. Wait 500ms
      5. Assert `.nav-search-results` shows "No results" message or is empty
    Expected Result: No results shown for nonsense query
    Failure Indicators: Stale results shown, error displayed, dropdown stays hidden
    Evidence: .sisyphus/evidence/task-5-empty-search.png
  ```

  **Commit**: YES
  - Message: `feat(search): implement Pagefind search logic`
  - Files: `static/js/search.js`
  - Pre-commit: `nix build`

- [ ] 6. Implement Chrome Prompt API semantic re-ranking

  **What to do**:
  - Add Chrome Prompt API integration to `static/js/search.js` as a progressive enhancement layer
  - Feature detection: check `'LanguageModel' in self` (NOT `window.ai` — API was renamed)
  - Availability check: `await LanguageModel.availability()` — only proceed if result is `'available'`
    - If `'downloadable'`, `'downloading'`, or `'unavailable'` — silently skip AI. NEVER trigger model download.
  - Session creation: lazily create on first actual search execution (not on page load or input focus)
    - `const session = await LanguageModel.create({ initialPrompts: [{ role: 'system', content: 'You rerank search results by relevance. Given a query and numbered titles, return the numbers in order of relevance, comma-separated.' }] })`
  - Re-ranking flow:
    1. Pagefind returns keyword results (show these IMMEDIATELY)
    2. Take top 5 result titles
    3. Prompt: `"Query: '{query}'. Rank these by relevance (return comma-separated numbers): 1. {title1} 2. {title2} ..."` 
    4. Parse response for ordering (extract numbers via regex)
    5. Re-order displayed results based on AI ranking
    6. If parse fails or is ambiguous, keep original keyword order
  - Timeout: 2-second AbortController on `session.prompt()` — abort and keep keyword order if slow
  - Error handling: any error → silently fall back to keyword order. No UI error states for AI.
  - Token budget: ~1024 tokens per prompt. Titles only (no excerpts). Max 5 results.
  - NO UI indicator about AI mode — transparent to user

  **Must NOT do**:
  - Do not use `window.ai` or `self.ai` — use `LanguageModel` global (renamed API)
  - Do not trigger model downloads — only use if `availability === 'available'`
  - Do not show any UI indication of AI usage
  - Do not block result display waiting for AI — show keyword results immediately, re-rank async
  - Do not send excerpts/full content to the model — titles only (token limit)
  - Do not add any external AI libraries or SDKs
  - Do not create AI session on page load or input focus

  **Recommended Agent Profile**:
  - **Category**: `deep`
    - Reason: Novel browser API integration with nuanced error handling, token budgeting, and async re-ranking pattern
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `claude-api`: This is Chrome's built-in AI, not Anthropic API

  **Parallelization**:
  - **Can Run In Parallel**: NO (builds on Task 5's search.js)
  - **Parallel Group**: Wave 3 (after Task 5)
  - **Blocks**: Task 7
  - **Blocked By**: Task 5

  **References**:

  **Pattern References**:
  - `static/js/search.js` (from Task 5) — This task EXTENDS the same file with AI re-ranking layer
  - `static/js/theme.js` — Feature detection pattern: check capability, apply enhancement, graceful degrade

  **API/Type References**:
  - `LanguageModel.availability()` → `'available' | 'downloadable' | 'downloading' | 'unavailable'`
  - `LanguageModel.create({ initialPrompts: [{role, content}] })` → `Promise<session>`
  - `session.prompt(text, { signal: AbortController.signal })` → `Promise<string>`
  - Token limit: ~1024 per prompt, ~4096 per session

  **External References**:
  - Chrome Prompt API docs: https://developer.chrome.com/docs/ai/prompt-api
  - Feature detection: https://developer.chrome.com/docs/ai/prompt-api#feature_detection

  **WHY Each Reference Matters**:
  - The API was recently renamed — many online examples use the OLD `window.ai` syntax. The references above are authoritative.
  - Token limits are hard constraints — exceeding them will cause silent failures
  - Availability states determine whether AI can be used without triggering downloads

  **Acceptance Criteria**:
  - [ ] Feature detection uses `'LanguageModel' in self` (not `window.ai`)
  - [ ] Only proceeds when `availability === 'available'`
  - [ ] Never triggers model download
  - [ ] Keyword results shown immediately, AI re-ranking applied async
  - [ ] Re-ranking uses titles only, max 5 results
  - [ ] 2-second timeout on prompt with AbortController
  - [ ] Parse failures fall back to keyword order silently
  - [ ] No UI indicator of AI mode
  - [ ] Session created lazily on first search, not on page load

  **QA Scenarios:**

  ```
  Scenario: Search works when Chrome AI is unavailable
    Tool: Playwright (playwright skill)
    Preconditions: Site served locally, using Firefox or Chrome without Gemini Nano
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Open browser console, verify no errors related to LanguageModel
      3. Type a search query in `.nav-search-input`
      4. Wait for results
      5. Assert results appear (keyword-ordered)
      6. Assert no console errors
    Expected Result: Search works normally without AI, no errors
    Failure Indicators: Console errors about LanguageModel, search fails entirely
    Evidence: .sisyphus/evidence/task-6-no-ai-fallback.png

  Scenario: No model download triggered
    Tool: Playwright (playwright skill) with Chrome
    Preconditions: Site served locally, Chrome with Prompt API flag but model not downloaded
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Monitor network requests
      3. Type a search query
      4. Wait for results
      5. Assert no model download was initiated (no large file transfers)
      6. Assert search returns keyword results normally
    Expected Result: AI is silently skipped, no download prompt or large transfers
    Failure Indicators: Download dialog, large network transfer, hanging search
    Evidence: .sisyphus/evidence/task-6-no-download-trigger.txt
  ```

  **Commit**: YES (groups with Task 5)
  - Message: `feat(search): implement Pagefind search logic with Chrome AI re-ranking`
  - Files: `static/js/search.js`
  - Pre-commit: `nix build`

- [ ] 7. Wire everything together + keyboard accessibility

  **What to do**:
  - Integrate all pieces: HTML (Task 3) + CSS (Task 4) + JS (Tasks 5+6) into a cohesive search experience
  - Keyboard accessibility:
    - `Escape` key: close results dropdown and clear input (or just close dropdown)
    - `ArrowDown` / `ArrowUp`: navigate between result items in dropdown
    - `Enter` on a focused result: navigate to that result's URL
    - `Ctrl+K` or `Cmd+K` (platform-aware): focus search input from anywhere on page
    - Tab order: search input → first result → next result (natural tab flow)
  - Click outside: close results dropdown
  - Scroll behavior: when results dropdown is open and user scrolls the page, close dropdown
  - ARIA attributes:
    - `role="search"` on the search container
    - `role="listbox"` on results container
    - `role="option"` on each result item
    - `aria-expanded="true/false"` on input based on dropdown state
    - `aria-activedescendant` on input pointing to currently focused result
    - `aria-label="Search"` on input
  - Ensure no conflicts with existing nav.js (scroll hide/show) and toc.js (panel toggle)
  - Test the full flow: type query → see results → navigate with keyboard → click result → land on post

  **Must NOT do**:
  - Do not break existing keyboard shortcuts or tab order
  - Do not add focus trapping (dropdown is not a modal)
  - Do not prevent default browser search (Ctrl+F) behavior
  - Do not add unnecessary animations — keep minimal

  **Recommended Agent Profile**:
  - **Category**: `unspecified-high`
    - Reason: Integration task touching multiple files with accessibility requirements
  - **Skills**: [`playwright`]
    - `playwright`: Needed for QA scenarios to test keyboard interactions
  - **Skills Evaluated but Omitted**:
    - `visual-engineering`: Functionality focus, not visual

  **Parallelization**:
  - **Can Run In Parallel**: NO (integration of all prior tasks)
  - **Parallel Group**: Wave 4 (sequential)
  - **Blocks**: Task 8
  - **Blocked By**: Tasks 3, 4, 5, 6

  **References**:

  **Pattern References**:
  - `static/js/nav.js` — Shows existing keyboard/scroll event handling. Search must not conflict.
  - `static/js/toc.js` — Shows toggle panel pattern. Escape key handling may need coordination (if toc also uses Escape).
  - `static/js/search.js` (from Tasks 5+6) — Add keyboard handling and ARIA to this file
  - `templates/partials/nav.html` (from Task 3) — May need ARIA attribute additions

  **External References**:
  - WAI-ARIA combobox pattern: https://www.w3.org/WAI/ARIA/apg/patterns/combobox/ — the standard pattern for search input + results dropdown

  **WHY Each Reference Matters**:
  - Existing JS files may have Escape key handlers — must check for conflicts
  - ARIA combobox pattern is the W3C standard for this exact UI pattern

  **Acceptance Criteria**:
  - [ ] Escape closes results dropdown
  - [ ] Arrow keys navigate results in dropdown
  - [ ] Enter on focused result navigates to URL
  - [ ] Cmd+K / Ctrl+K focuses search input
  - [ ] Click outside closes dropdown
  - [ ] ARIA attributes present (role, aria-expanded, aria-activedescendant)
  - [ ] No conflicts with existing nav.js or toc.js
  - [ ] Tab order is logical

  **QA Scenarios:**

  ```
  Scenario: Keyboard navigation through search results
    Tool: Playwright (playwright skill)
    Preconditions: Site served, search functional
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Press Ctrl+K (or Meta+K on Mac)
      3. Assert `.nav-search-input` is focused
      4. Type a known post keyword
      5. Wait for results to appear
      6. Press ArrowDown
      7. Assert first result item has focus indicator (class or aria-selected)
      8. Press ArrowDown again
      9. Assert second result item has focus indicator
      10. Press Enter
      11. Assert page navigated to a post URL
    Expected Result: Full keyboard flow works without mouse
    Failure Indicators: Focus doesn't move, Enter doesn't navigate, Ctrl+K doesn't focus
    Evidence: .sisyphus/evidence/task-7-keyboard-nav.png

  Scenario: Escape key closes results
    Tool: Playwright (playwright skill)
    Preconditions: Site served, search showing results
    Steps:
      1. Navigate to http://127.0.0.1:8000/en/
      2. Click `.nav-search-input`, type a query, wait for results
      3. Assert `.nav-search-results` is visible
      4. Press Escape
      5. Assert `.nav-search-results` is hidden
    Expected Result: Escape dismisses results dropdown
    Failure Indicators: Results remain visible after Escape
    Evidence: .sisyphus/evidence/task-7-escape-close.png

  Scenario: Click outside closes results
    Tool: Playwright (playwright skill)
    Preconditions: Site served, search showing results
    Steps:
      1. Search for something, results appear
      2. Click on the page body outside the search area
      3. Assert `.nav-search-results` is hidden
    Expected Result: Clicking outside dismisses dropdown
    Failure Indicators: Dropdown stays open
    Evidence: .sisyphus/evidence/task-7-click-outside.png
  ```

  **Commit**: YES
  - Message: `feat(search): add keyboard accessibility and ARIA`
  - Files: `static/js/search.js`, `templates/partials/nav.html`
  - Pre-commit: `nix build`

- [ ] 8. GitHub Actions build integration

  **What to do**:
  - Verify that the existing `.github/workflows/build.yml` works with the updated `flake.nix` (Pagefind runs inside `nix build`)
  - Since Pagefind is now part of the Nix derivation's buildPhase, no separate CI step is needed — just verify `nix build` succeeds in CI
  - If `pagefind` binary is not available in the Nix sandbox (check: `pkgs.pagefind` may need network access), add `pagefind` to the CI step or adjust the derivation
  - Run a full `nix build` locally and verify the output structure matches what GitHub Pages expects
  - Verify `result/pagefind/` directory is included in the artifact upload (it should be, since `cp -r _site/* $out/` copies everything)

  **Must NOT do**:
  - Do not add separate CI steps for Pagefind — it should be part of `nix build`
  - Do not cache pagefind index separately
  - Do not modify the deploy job

  **Recommended Agent Profile**:
  - **Category**: `quick`
    - Reason: Verification task — the CI likely works already since Pagefind is in `nix build`
  - **Skills**: []
  - **Skills Evaluated but Omitted**:
    - `git-master`: No git operations needed

  **Parallelization**:
  - **Can Run In Parallel**: NO (final integration check)
  - **Parallel Group**: Wave 4 (after Task 7)
  - **Blocks**: Final verification
  - **Blocked By**: Task 7

  **References**:

  **Pattern References**:
  - `.github/workflows/build.yml:1-35` — Current CI: checkout → install nix → `nix build` → upload `result/`. Pagefind output is already inside result/ since it runs in buildPhase.
  - `flake.nix:134-139` — `installPhase` copies `_site/*` to `$out/`. Since pagefind writes to `_site/pagefind/`, it's automatically included.

  **WHY Each Reference Matters**:
  - The CI pipeline does `nix build` — if pagefind works in `nix build` locally, it should work in CI
  - The install phase copies everything — no special handling needed for pagefind output

  **Acceptance Criteria**:
  - [ ] `nix build` succeeds (this is the CI command)
  - [ ] `result/pagefind/` directory present in build output
  - [ ] No modifications to build.yml needed (or minimal if pagefind needs special sandbox treatment)
  - [ ] Build output deployable to GitHub Pages (HTML + pagefind/ coexist)

  **QA Scenarios:**

  ```
  Scenario: Full nix build produces deployable output
    Tool: Bash
    Preconditions: All previous tasks complete
    Steps:
      1. Run `nix build`
      2. Assert exit code 0
      3. Run `ls result/pagefind/pagefind.js`
      4. Assert file exists
      5. Run `ls result/en/posts/` 
      6. Assert post directories exist
      7. Run `ls result/zh/posts/`
      8. Assert post directories exist
      9. Verify result/ contains index.html at root
    Expected Result: Complete site with pagefind index ready for GitHub Pages
    Failure Indicators: Build fails, pagefind directory missing, posts missing
    Evidence: .sisyphus/evidence/task-8-full-build.txt

  Scenario: Pagefind index contains both language indexes
    Tool: Bash
    Preconditions: nix build succeeded
    Steps:
      1. Run `ls result/pagefind/`
      2. Assert files for English index exist (look for `en` in filenames or index structure)
      3. Assert files for Chinese index exist (look for `zh` in filenames or index structure)
    Expected Result: Separate index chunks for en and zh languages
    Failure Indicators: Only one language indexed, missing language-specific chunks
    Evidence: .sisyphus/evidence/task-8-bilingual-index.txt
  ```

  **Commit**: YES (groups with Task 7 if needed)
  - Message: `feat(search): integrate search + keyboard accessibility + CI verification`
  - Files: `.github/workflows/build.yml` (only if changes needed)
  - Pre-commit: `nix build`

---

## Final Verification Wave (MANDATORY — after ALL implementation tasks)

> 4 review agents run in PARALLEL. ALL must APPROVE. Rejection → fix → re-run.

- [ ] F1. **Plan Compliance Audit** — `oracle`
  Read the plan end-to-end. For each "Must Have": verify implementation exists (read file, run command). For each "Must NOT Have": search codebase for forbidden patterns — reject with file:line if found. Check evidence files exist in .sisyphus/evidence/. Compare deliverables against plan.
  Output: `Must Have [N/N] | Must NOT Have [N/N] | Tasks [N/N] | VERDICT: APPROVE/REJECT`

- [ ] F2. **Code Quality Review** — `unspecified-high`
  Run `nix build` + `nix flake check`. Review all changed files for: empty catches, console.log in prod, commented-out code, unused imports. Check AI slop: excessive comments, over-abstraction, generic names (data/result/item/temp). Verify JS follows existing patterns in `static/js/`.
  Output: `Build [PASS/FAIL] | Checks [PASS/FAIL] | Files [N clean/N issues] | VERDICT`

- [ ] F3. **Real Manual QA** — `unspecified-high` (+ `playwright` skill)
  Start from clean `nix build`. Serve site locally. Execute EVERY QA scenario from EVERY task — follow exact steps, capture evidence. Test cross-task integration: search in en → switch lang → search in zh. Test edge cases: empty query, very long query, special characters, rapid typing, offline mode. Save to `.sisyphus/evidence/final-qa/`.
  Output: `Scenarios [N/N pass] | Integration [N/N] | Edge Cases [N tested] | VERDICT`

- [ ] F4. **Scope Fidelity Check** — `deep`
  For each task: read "What to do", read actual diff. Verify 1:1 — everything in spec was built, nothing beyond spec was built. Check "Must NOT do" compliance: no PagefindUI usage, no hardcoded colors, no external services, no bundler introduced. Flag unaccounted changes.
  Output: `Tasks [N/N compliant] | Must NOT violations [CLEAN/N issues] | Unaccounted [CLEAN/N files] | VERDICT`

---

## Commit Strategy

- **Wave 1**: `feat(build): add Pagefind search indexing to Nix build pipeline` — flake.nix, templates with data-pagefind attributes
- **Wave 2**: `feat(search): add search input to nav bar with dropdown UI` — nav.html, _components.scss
- **Wave 3**: `feat(search): implement Pagefind search logic with Chrome AI re-ranking` — search.js
- **Wave 4**: `feat(search): integrate search + keyboard accessibility + CI` — default.html, build.yml, search.js updates

---

## Success Criteria

### Verification Commands
```bash
nix build                    # Expected: succeeds, result/ contains pagefind/ directory
ls result/pagefind/          # Expected: pagefind.js, pagefind-*.js, *.pf_index, etc.
ls result/en/                # Expected: posts with searchable content
ls result/zh/                # Expected: posts with searchable content
```

### Final Checklist
- [ ] `nix build` passes with zero errors
- [ ] `result/pagefind/` directory exists with index files
- [ ] Search input visible in nav on desktop and mobile
- [ ] English search returns English results
- [ ] Chinese search returns Chinese results  
- [ ] Results dropdown shows highlighted excerpts
- [ ] Clicking result navigates to correct post
- [ ] Escape key closes results dropdown
- [ ] Dark/light theme correctly styles all search elements
- [ ] No external network requests during search
- [ ] Chrome Prompt API re-ranking works in Chrome 138+ (when model available)
- [ ] Graceful fallback to keyword-only when Prompt API unavailable
- [ ] All "Must NOT Have" items verified absent
