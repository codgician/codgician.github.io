---
name: new-blog
description: Create a new blog post following project conventions
---

# New Blog Post

Create a new blog post with proper structure and frontmatter.

## Usage

```
/new-blog <topic>
```

## Workflow

1. **Get topic from user** - Ask what the post is about if not provided
2. **Determine slug** - Create URL-friendly slug from topic (lowercase, hyphens)
3. **Create directory** - `content/posts/{slug}/`
4. **Ask for primary language** - Which language is canonical (en or zh)?
5. **Create both language files** with proper frontmatter

## File Structure

```
content/posts/{slug}/
├── index.en.md    # English version
└── index.zh.md    # Chinese version
```

## Frontmatter Template

```yaml
---
title: "{title}"
date: { YYYY-MM-DD }
language: { en|zh }
canonical: { true for source language, false for translation }
tags:
  - { tag1 }
  - { tag2 }
math: false
toc: false
draft: true
---
```

## Field Descriptions

| Field       | Description                                                      |
| ----------- | ---------------------------------------------------------------- |
| `title`     | Post title (can differ between languages)                        |
| `date`      | Publication date (YYYY-MM-DD format, use today)                  |
| `language`  | `en` or `zh`                                                     |
| `canonical` | `true` for the original/source language, `false` for translation |
| `tags`      | List of relevant tags (lowercase, hyphenated)                    |
| `math`      | Set `true` if post contains LaTeX math                           |
| `toc`       | Set `true` to show table of contents                             |
| `draft`     | Set `true` initially, change to `false` when ready to publish    |

## Example

User: `/new-blog Introduction to Monads`

Creates:

- `content/posts/introduction-to-monads/index.en.md`
- `content/posts/introduction-to-monads/index.zh.md`

With appropriate frontmatter and placeholder content.

## Instructions for LLM

1. Ask user for the topic if not provided
2. Ask which language is the primary/canonical version
3. Generate a URL-friendly slug
4. Suggest relevant tags based on the topic
5. Create both files with:
   - Proper frontmatter (canonical=true for primary, false for other)
   - A brief placeholder introduction
   - Section headers as starting structure
6. Set `draft: true` so it won't be published until ready
7. Set `math: true` if topic involves mathematics
8. Report the created files and next steps
