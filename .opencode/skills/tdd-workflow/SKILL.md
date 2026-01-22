---
name: tdd-workflow
description: Test-driven development workflow adapted for Hakyll. Use when implementing features or fixing bugs. Write verification first, watch it fail, implement, verify passes.
---

# TDD Workflow for Hakyll

Adapted TDD principles for a static site generator where "tests" are verification commands.

**Core principle:** Define expected behavior BEFORE implementing. Verify failure. Implement. Verify success.

## The Cycle

```
1. DEFINE   → Write verification command (what should be true?)
2. FAIL     → Run it, confirm it fails (proves test is valid)
3. IMPLEMENT → Write minimal code to pass
4. PASS     → Run verification, confirm success
5. REFACTOR → Clean up, verify still passes
```

## Verification Types

Since Hakyll doesn't have HSpec tests, verification happens via:

| Type | Tool | Use For |
|------|------|---------|
| Build | `nix build` | Compilation, type errors |
| Output | `grep` on `_site/` | Content generation |
| Structure | `ls`, `find` | File/route creation |
| Content | `grep`, `head` | Specific output |

## Example: Adding RSS Feed

### 1. DEFINE (Expected Behavior)

```bash
# RSS feed should exist for each language
test -f _site/en/feed.xml && echo "EN feed exists"
test -f _site/zh/feed.xml && echo "ZH feed exists"

# Feed should contain recent posts
grep -c "<entry>" _site/en/feed.xml  # Should be > 0
```

### 2. FAIL (Verify Test Works)

```bash
nix build
test -f result/en/feed.xml && echo "EN feed exists" || echo "FAIL: No EN feed"
# Output: FAIL: No EN feed
```

✅ Good - failure is expected (feature doesn't exist yet)

### 3. IMPLEMENT (Minimal Code)

```haskell
-- In src/Site.hs
rssFeeds :: SiteConfig -> Rules ()
rssFeeds cfg = forM_ (languages cfg) $ \lang -> do
  let langStr = T.unpack $ langCode lang
  create [fromFilePath $ langStr </> "feed.xml"] $ do
    route idRoute
    compile $ do
      posts <- take 20 <$> loadAllPostsForLang cfg langStr
      renderAtom (feedConfiguration cfg langStr) feedCtx posts
```

### 4. PASS (Verify Success)

```bash
nix build
test -f result/en/feed.xml && echo "✅ EN feed exists"
test -f result/zh/feed.xml && echo "✅ ZH feed exists"
grep -c "<entry>" result/en/feed.xml  # Output: 20
```

### 5. REFACTOR (Clean Up)

- Extract helpers
- Remove duplication
- Re-run verification

## Quick Reference

| Feature | Verification Command |
|---------|---------------------|
| New route | `ls result/path/to/file.html` |
| Post list | `grep -c "post-list-item" result/en/posts/index.html` |
| Image copied | `ls result/en/slides/*/image.png` |
| Template applied | `grep "expected-class" result/page.html` |
| RSS items | `grep -c "<entry>" result/lang/feed.xml` |
| Sitemap entry | `grep "expected/url/" result/sitemap.xml` |

## Workflow Checklist

- [ ] Wrote verification command BEFORE implementing
- [ ] Verification failed initially (proves it tests the right thing)
- [ ] Wrote minimal code to pass
- [ ] `nix build` succeeds
- [ ] Verification passes
- [ ] Refactored without breaking verification

## Common Patterns

### Testing Route Creation

```bash
# Before: Should fail
ls result/en/new-feature/index.html 2>/dev/null || echo "EXPECTED: Route doesn't exist"

# After: Should succeed
ls result/en/new-feature/index.html && echo "✅ Route created"
```

### Testing Content Generation

```bash
# Verify specific content appears
grep -q "expected-text" result/page.html && echo "✅ Content found" || echo "FAIL"

# Count items
[ $(grep -c "list-item" result/index.html) -ge 10 ] && echo "✅ 10+ items"
```

### Testing Fallback Behavior

```bash
# Post list should show all posts (including fallbacks)
EXPECTED=27  # Known number of unique slugs
ACTUAL=$(grep -c "post-list-item" result/en/posts/index.html)
[ "$ACTUAL" -ge "$EXPECTED" ] && echo "✅ All posts shown" || echo "FAIL: Only $ACTUAL"
```

## Red Flags

- Implementing before defining verification
- Verification passes immediately (not testing the change)
- Skipping "nix build" step
- Not running verification after refactoring

## When to Skip Formal TDD

- Typo fixes
- Documentation updates
- Comment changes
- Config file tweaks

Even then, run `nix build` to verify no breakage.

## Integration with Other Skills

1. Use `coding-standard` when implementing
2. Use `code-review` after TDD cycle completes
3. Validator confirms all verifications pass
