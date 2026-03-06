# Draft: Content Search for codgician.github.io

## Requirements (confirmed)
- Self-contained: No external services, works offline on local machine
- Efficient: Should not bloat repository or built artifacts
- Nice-to-have: Leverage browser AI for semantic search (progressive enhancement)

## Site Architecture (researched)
- **Generator**: Hakyll (Haskell) with Nix-managed builds
- **Bilingual**: en/zh, files named `index.{lang}.md`, routed to `/{lang}/...`
- **Template**: `<html lang="$lang$">` already set in `default.html` (line 2)
- **Frontend**: Raw JS files (no bundler), SCSS compiled via dart-sass
- **Build**: `nix build` → runs `site build` → output to `_site/` → `cp -r _site/* $out/`
- **Deploy**: GitHub Pages via GitHub Actions (`build.yml`)
- **External tools**: Already uses subprocess pattern (KaTeX CLI, Mermaid CLI, dart-sass)
- **Vendor assets**: Managed through Nix, symlinked/copied into `static/vendor/`

## Research Findings

### Search Library Comparison
| Library | CJK | Index Size (100 posts) | Multilingual | Status | Verdict |
|---------|-----|----------------------|--------------|--------|---------|
| **Pagefind** | ✅ Built-in segmentation | ~20-50KB chunked | ✅ Auto per-lang | Active (v1.4, Sep 2025) | ⭐ RECOMMENDED |
| MiniSearch | ⚠️ Custom tokenizer | ~150-300KB | Manual | Active | Runner-up |
| Fuse.js | ⚠️ No segmentation | ~150-300KB raw JSON | None | Active | Acceptable |
| Lunr.js | ❌ Poor CJK | ~200-400KB | Plugin-based | Stale (2020) | Avoid |
| Stork | ❌ None | ~100-200KB | None | Discontinued | Avoid |
| TinySearch | ❌ None | ~50-100KB bloom filter | None | Minimal | Avoid |

### Pagefind Key Facts
- Runs as CLI post-build step on HTML output
- Auto-detects `<html lang="">` for per-language indexes
- Chunked index: ~6KB JS core + chunks loaded on-demand (~100KB total session)
- Chinese UI translations: ✅ | Chinese word stemming: "See below" (built-in segmentation)
- Used by: many Hugo/Jekyll/Astro sites, proven in production
- Available as npm package or standalone Rust binary

### Browser AI Capabilities (researched)
1. **Chrome Prompt API (Gemini Nano)**: Chrome 138+ origin trial
   - Text generation/summarization only — NO embedding generation API
   - Feature detect: `'ai' in self && 'languageModel' in self.ai`
   - Chrome-only, requires model download (~1.7GB, managed by Chrome)
   
2. **Transformers.js (Hugging Face)**: Runs ONNX models in browser
   - `all-MiniLM-L6-v2`: ~22MB English embeddings
   - `gte-tiny`: ~23MB multilingual (smallest viable)
   - `jina-embeddings-v2-base-zh`: ~162MB Chinese+English (too large)
   - WebGPU backend: fast but not universal; WASM fallback available
   - Problem: 23MB+ model download per visitor — heavy for a blog

3. **Practical assessment for semantic search**:
   - Full in-browser embedding: impractical (model download too heavy)
   - Pre-computed embeddings + browser query model: still 23MB download
   - **Chrome Prompt API for result re-ranking**: Most practical enhancement
     - Zero additional download (model already in Chrome)
     - Use to "understand" query intent and re-rank keyword results
     - Graceful fallback to keyword-only for non-Chrome browsers

## Technical Decisions

### Primary search: Pagefind
- Rationale: Best CJK support, automatic multilingual, smallest payload, zero Hakyll code changes, follows project's "external tools via subprocess" philosophy
- Integration: Post-build CLI step, add `pagefind` to Nix flake

### Semantic enhancement: Chrome Prompt API for re-ranking (nice-to-have)
- Rationale: Zero download overhead, progressive enhancement, graceful degradation
- Approach: After Pagefind returns keyword results, use Prompt API to re-rank by semantic relevance
- Fallback: If API unavailable, keyword results shown as-is

### Nix integration approach
- Add `pagefind` package to `buildTools` in flake.nix  
- Run `pagefind --site .` after `site build` in `buildPhase`
- Pagefind outputs go into `_site/pagefind/` alongside other built files

## Open Questions (RESOLVED)
1. UI placement: **Always-visible search input in nav bar** (even on mobile)
2. Results display: **Dropdown panel below search input** (user stays on page)
3. Styling: **Custom UI using Pagefind JS API** matching site design language
4. Semantic search: **Include in plan** as later-wave progressive enhancement
5. Test strategy: **Haskell tests for pipeline changes**, JS verified via QA scenarios

## Scope Boundaries
- INCLUDE: Pagefind integration, search UI (nav bar input + dropdown results), Nix build integration, bilingual support, Chrome Prompt API re-ranking
- EXCLUDE: Server-side search, external service dependencies, heavy ML model downloads, JS test framework setup