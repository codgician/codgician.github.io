# AGENTS.md Best Practices Guide

**A comprehensive resource for writing effective AGENTS.md files for AI coding assistants**

---

## 📚 What's Included

This guide provides everything you need to create and maintain high-quality AGENTS.md files for your projects.

### Core Documentation

- **[AGENTS_MD_BEST_PRACTICES.md](./AGENTS_MD_BEST_PRACTICES.md)** - Complete guide covering:
  - What AGENTS.md is and why it matters
  - Recommended structure and content
  - Templates for different project sizes
  - Nested AGENTS.md strategy for monorepos
  - What NOT to include + common anti-patterns
  - Real-world examples from major projects
  - Decision frameworks and checklists

- **[QUICK_REFERENCE.md](./QUICK_REFERENCE.md)** - TL;DR version with:
  - Section checklists
  - Do's and don'ts
  - Decision matrices
  - Quick start template
  - Priority guidelines

### Ready-to-Use Templates

- **[TEMPLATE_MINIMAL.md](./TEMPLATE_MINIMAL.md)** - For small projects (< 5 contributors)
  - Essential sections only
  - < 100 lines recommended
  - Single language/framework

- **[TEMPLATE_STANDARD.md](./TEMPLATE_STANDARD.md)** - For most projects (5-20 contributors)
  - Comprehensive structure
  - 100-300 lines recommended
  - Multiple subsystems supported

- **[TEMPLATE_MONOREPO_ROOT.md](./TEMPLATE_MONOREPO_ROOT.md)** - For monorepo root level
  - Project overview
  - Global commands
  - Subsystem navigation
  - Points to nested files

- **[TEMPLATE_MONOREPO_NESTED.md](./TEMPLATE_MONOREPO_NESTED.md)** - For subsystems in monorepos
  - Context-specific guidance
  - Subsystem conventions
  - Local commands
  - Specialized testing

---

## 🚀 Quick Start

### 1. Choose Your Template

| Project Type | Template | Lines |
|--------------|----------|-------|
| Small project, standard setup | [TEMPLATE_MINIMAL.md](./TEMPLATE_MINIMAL.md) | < 100 |
| Medium project, custom conventions | [TEMPLATE_STANDARD.md](./TEMPLATE_STANDARD.md) | 100-300 |
| Monorepo root | [TEMPLATE_MONOREPO_ROOT.md](./TEMPLATE_MONOREPO_ROOT.md) | < 200 |
| Monorepo subsystem | [TEMPLATE_MONOREPO_NESTED.md](./TEMPLATE_MONOREPO_NESTED.md) | < 300 |

### 2. Customize for Your Project

1. Copy the appropriate template to your repository as `AGENTS.md`
2. Replace all `[bracketed placeholders]` with your project details
3. Delete sections that don't apply
4. Add project-specific sections as needed
5. Remove the usage notes at the bottom

### 3. Keep It Current

- Update AGENTS.md in the same PR that changes relevant code/config
- Review quarterly to catch drift
- Test commands to ensure they're still valid

---

## 📖 What is AGENTS.md?

**AGENTS.md** is an open standard maintained by the Agentic AI Foundation (Linux Foundation) that provides a predictable location for AI coding assistants to find project-specific instructions.

### Key Facts

- **Format**: Simple Markdown with **no required fields**
- **Used by**: 60,000+ open-source projects
- **Supported by**: 20+ AI coding tools (GitHub Copilot, Cursor, Windsurf, Zed, Jules, Aider, etc.)
- **Purpose**: Acts as "README for agents" - provides context that would clutter README.md
- **Official site**: https://agents.md

### Why Use AGENTS.md?

✅ **Helps AI assistants**:
- Understand your codebase structure
- Follow your coding conventions
- Run build/test/lint commands correctly
- Navigate your architecture

✅ **Benefits your team**:
- Standardizes development workflows
- Documents tribal knowledge
- Onboards new contributors faster
- Works across all AI coding tools (open standard)

---

## 🎯 Core Principles

1. **Intentional Flexibility** - No required sections; adapt to your needs
2. **Complement, Don't Duplicate** - Don't repeat README.md content
3. **Agent-First Perspective** - Write for AI tools that need to code
4. **Actionable Over Descriptive** - Concrete commands > vague principles
5. **Proximity Principle** - In monorepos, place AGENTS.md close to relevant code

---

## 📋 Common Sections

Most AGENTS.md files include these sections (none are required):

1. **Tech Stack** - Languages, frameworks, versions
2. **Repository Structure** - Directory layout
3. **Development Commands** - Build, test, lint commands
4. **Coding Standards** - Language-specific conventions
5. **Testing Strategy** - How to write and run tests
6. **Common Workflows** - Adding features, migrations, etc.

See [AGENTS_MD_BEST_PRACTICES.md](./AGENTS_MD_BEST_PRACTICES.md) for detailed guidance on each section.

---

## ✅ Do's and ❌ Don'ts

### ✅ DO

- Provide exact, copy-pasteable commands
- Explain project-specific conventions
- Include file paths relative to repo root
- Update when changing code/config
- Use nested files for complex monorepos

### ❌ DON'T

- Include sensitive info (keys, credentials)
- Duplicate README.md content
- Use vague advice ("follow best practices")
- List all dependencies (that's package.json)
- Let it become outdated

---

## 🔍 What Goes Where?

| Content | AGENTS.md | README.md |
|---------|-----------|-----------|
| Build/test commands | ✅ | Maybe |
| Coding conventions | ✅ | ❌ |
| Project structure details | ✅ | Maybe |
| Testing strategy | ✅ | Brief |
| Project description | ❌ | ✅ |
| Installation for users | ❌ | ✅ |
| Quick start guide | ❌ | ✅ |

---

## 🌳 Nested AGENTS.md for Monorepos

For complex monorepos with multiple tech stacks:

```
my-monorepo/
├── AGENTS.md                    # Overview, global commands
├── backend/
│   └── AGENTS.md               # Python-specific guidance
├── frontend/
│   └── AGENTS.md               # React-specific guidance
└── docs/
    └── AGENTS.md               # Docs-specific guidance
```

**Rule**: AI agents read the AGENTS.md closest to the file being edited.

See [TEMPLATE_MONOREPO_ROOT.md](./TEMPLATE_MONOREPO_ROOT.md) and [TEMPLATE_MONOREPO_NESTED.md](./TEMPLATE_MONOREPO_NESTED.md) for examples.

---

## 🎓 Real-World Examples

### Minimal: agents.md Official Repo
- **Size**: 44 lines
- **Project**: https://github.com/agentsmd/agents.md
- **Approach**: Minimal viable documentation
- **When to emulate**: Small, simple projects

### Standard: Streamlit
- **Size**: 82 lines (root) + 7 nested files
- **Project**: https://github.com/streamlit/streamlit
- **Approach**: Overview with nested specialization
- **When to emulate**: Monorepos, mixed tech stacks

### Comprehensive: LangChain.js
- **Size**: 451 lines
- **Project**: https://github.com/langchain-ai/langchainjs
- **Approach**: Exhaustive development manual
- **When to emulate**: Large teams, complex architecture

---

## 🔗 Related Files

AGENTS.md works alongside tool-specific configurations:

| File | Scope |
|------|-------|
| `AGENTS.md` | **Tool-agnostic** (open standard) |
| `.cursor/rules/*.mdc` | Cursor-specific |
| `.aider.conf.yml` | Aider-specific |
| `.github/copilot-instructions.md` | GitHub Copilot-specific |

**Best practice**: Use AGENTS.md for general guidance, tool-specific files for tool features.

---

## 📚 Additional Resources

### Official Documentation
- **AGENTS.md Homepage**: https://agents.md
- **GitHub Repository**: https://github.com/agentsmd/agents.md

### Community Examples
- [Search GitHub for AGENTS.md files](https://github.com/search?q=path%3AAGENTS.md&type=code)
- Look for repos with high stars and active maintenance

---

## 🤝 Contributing to This Guide

This guide was created based on research of:
- Official AGENTS.md documentation
- 60,000+ repositories using AGENTS.md
- Real-world examples from major open-source projects (Streamlit, LangChain.js)

Found an issue or have suggestions? Please open an issue or PR!

---

## 📄 License

This guide is provided as-is for community benefit. AGENTS.md is an open standard maintained by the Agentic AI Foundation under the Linux Foundation.

---

**Last Updated**: 2025-01-14

**Version**: 1.0

---

## 🎯 Next Steps

1. **Read** [AGENTS_MD_BEST_PRACTICES.md](./AGENTS_MD_BEST_PRACTICES.md) for comprehensive guidance
2. **Skim** [QUICK_REFERENCE.md](./QUICK_REFERENCE.md) for quick lookups
3. **Choose** a template that fits your project
4. **Customize** it for your specific needs
5. **Keep it updated** as your project evolves

Happy coding with AI assistants! 🤖✨
