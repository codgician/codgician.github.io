---
title: "LLM Agent Trajectory Analysis"
subtitle: | 
  Unofficial paper reading:  
  [A Survey for LLM Agent Trajectory Analysis From Failure Attribution to Enhancement](https://www.researchgate.net/publication/401193207_A_Survey_for_LLM_Agent_Trajectory_Analysis_From_Failure_Attribution_to_Enhancement) 
author: codgician
date: 2026-04-27
math: true
tikz: true
theme: white
highlight-style: pygments
width: 1440
height: 900
---

# From stack traces to trajectories

::: {.slide-claim}
Much traditional debugging is **code/config/runtime-location** oriented; agent debugging is often **outcome-to-trajectory** oriented.
:::

::: {.split}
:::: {.comparison-card}
**Traditional software**

- **Evaluation signal:** a test failure, exception, crash, or violated specification.

- **Debugging object:** source locations, stack traces, breakpoints, and conventional logs.

- **Repair target:** code paths, data structures, configuration, or dependency behavior.
::::

:::: {.comparison-card}
**LLM agent systems**

- **Evaluation signal:** a task mismatch, unsafe action, inefficient run, or unrecoverable state.

- **Debugging object:** trajectories with prompts, messages, tool calls, observations, handoffs, and state.

- **Repair target:** policy, prompt/context, tool interface, workflow graph, verifier, memory, or supervisor.
::::
:::

::: {.builder-implication}
Final-output scoring is not enough for agents; rich traces become the main evidence for attribution, repair, and regression testing.
:::

::: { .notes }
Open with first principles: why do we even need a survey on agent debugging? Because the *unit of evidence* changed. In traditional software you debug code at a code location — stack trace, breakpoint, log line. In an LLM agent, the bug doesn't sit at a single line; it lives in a *trajectory* the model produced at runtime. Walk through the two columns concretely (one example each), then land on the implication: if final-output scoring is all you have, you cannot debug or regress agents — you can only re-roll the dice.
:::

<style>
.reveal {
  --ink: #182028;
  --muted: #52606d;
  --line: #cfd7df;
  --panel: #f8fafb;
  --source-note: #f5f8f9;
  --accent: #245f63;
  --accent-2: #8a5a13;
  --danger: #8f1d1d;
  --good: #2f6846;
  color: var(--ink);
}
/* Academic-style layout: titles top-left with a thin rule, body left-aligned, content anchored near the top. */
.reveal .slides {
  text-align: left;
}
.reveal .slides > section:not(.stack),
.reveal .slides > section > section {
  text-align: left;
  display: flex;
  flex-direction: column;
  align-items: stretch;
  justify-content: flex-start;
  height: 100%;
  /* Consistent academic gutter: left/right safe area + small top breathing room. */
  padding: 0.55em 1.20em 0.50em 1.20em;
  box-sizing: border-box;
}
.reveal .slides > section > h1,
.reveal .slides > section > h2,
.reveal .slides > section > section > h1,
.reveal .slides > section > section > h2 {
  align-self: flex-start;
  /* Thin academic rule under section/slide titles for a clean header band. */
  border-bottom: 1px solid var(--line);
  padding-bottom: 0.18em;
  width: 100%;
}
.reveal h1,
.reveal h2,
.reveal h3 {
  color: var(--ink);
  letter-spacing: -0.012em;
  text-align: left;
  text-transform: none;
  margin: 0 0 0.40em 0;
}
.reveal h1 { font-size: 1.95em; margin-bottom: 0.40em; }
.reveal h2 { font-size: 1.30em; margin-bottom: 0.40em; }
.reveal h3 { font-size: 0.95em; margin-bottom: 0.25em; }
.reveal p,
.reveal li,
.reveal ul,
.reveal ol {
  text-align: left;
}
.reveal p { margin: 0 0 0.45em 0; }
.reveal p,
.reveal li { font-size: 0.78em; line-height: 1.32; }
.reveal ul, .reveal ol {
  display: block;
  margin: 0 0 0.45em 0;
  padding-left: 1.20em;
}
.reveal li { margin: 0.10em 0; }
.reveal li > ul, .reveal li > ol { margin: 0.10em 0 0.10em 0; }
.reveal strong { color: var(--accent); }
.reveal .slide-claim {
  font-size: 0.92em;
  line-height: 1.24;
  margin: 0.05em 0 0.55em;
  color: var(--ink);
  text-align: left;
}
.reveal .slide-subtitle {
  font-size: 0.78em;
  line-height: 1.28;
  color: var(--muted);
  margin: -0.10em 0 0.55em;
  letter-spacing: 0;
  text-align: left;
}
.reveal .source-note {
  background: var(--source-note);
  border: 1px solid var(--line);
  border-left: 0.12em solid var(--muted);
  padding: 0.34em 0.52em;
  margin: 0.26em 0 0.42em;
}
.reveal .source-note p {
  font-size: 0.66em;
  line-height: 1.28;
  color: var(--muted);
}
.reveal .source-note strong {
  color: var(--ink);
}
.reveal .formal-anchor {
  border: 1px solid var(--line);
  background: #ffffff;
  padding: 0.42em 0.56em;
  margin: 0.26em 0;
}
.reveal .formal-anchor p,
.reveal .formal-anchor li { font-size: 0.70em; }
.reveal .developer-intuition {
  background: #fbf7ef;
  border-left: 0.22em solid var(--accent-2);
  padding: 0.40em 0.58em;
  margin: 0.28em 0;
}
.reveal .developer-intuition p,
.reveal .builder-implication p { font-size: 0.70em; }
.reveal .builder-implication {
  background: #f2f8f4;
  border-left: 0.22em solid var(--good);
  padding: 0.36em 0.56em;
  margin-top: 0.42em;
}
.reveal .split {
  display: grid;
  grid-template-columns: 1.02fr 0.98fr;
  gap: 0.70em;
  align-items: stretch;
}
.reveal .split > .comparison-card,
.reveal .split > .case,
.reveal .split > .gap-callout {
  display: flex;
  flex-direction: column;
}
.reveal .mini-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 0.7em;
  align-items: stretch;
}
/* 2x2 layout when there are exactly 4 cards */
.reveal .mini-grid:has(> .case:nth-child(4)):not(:has(> .case:nth-child(5))) {
  grid-template-columns: repeat(2, 1fr);
}
/* 2-column layout when there are exactly 2 cards */
.reveal .mini-grid:has(> .case:nth-child(2)):not(:has(> .case:nth-child(3))) {
  grid-template-columns: repeat(2, 1fr);
}
.reveal .comparison-card,
.reveal .case,
.reveal .principle {
  background: #ffffff;
  border-top: 0.18em solid var(--accent);
  box-shadow: inset 0 0 0 1px var(--line);
  padding: 0.34em 0.46em;
}
.reveal .comparison-card { padding: 0.24em 0.34em; }
.reveal .comparison-card p,
.reveal .comparison-card li { font-size: 0.70em; line-height: 1.22; }
.reveal .comparison-card > p { margin: 0 0 0.22em; }
.reveal .comparison-card ul { margin: 0.12em 0 0; }
.reveal .comparison-card li { margin: 0.08em 0; }
.reveal .comparison-card li p { margin: 0; }
.reveal .case p,
.reveal .principle p,
.reveal .case li,
.reveal .principle li { font-size: 0.65em; line-height: 1.24; }
.reveal .compact li { margin: 0.10em 0; }
.reveal .small { font-size: 0.65em; color: var(--muted); }
.reveal .tag {
  display: inline-block;
  border: 1px solid var(--line);
  border-radius: 999px;
  padding: 0.18em 0.65em;
  margin: 0.10em 0.12em;
  font-size: 0.62em;
  font-weight: 500;
  letter-spacing: 0.01em;
  color: var(--text);
  background: var(--panel);
}
.reveal .card {
  background: var(--panel);
  border: 1px solid var(--line);
  border-left: 0.20em solid var(--accent);
  border-radius: 0.18em;
  padding: 0.34em 0.48em;
  margin: 0.22em 0;
}
.reveal .card p,
.reveal .card li { font-size: 0.66em; }
.reveal .warning {
  border-left-color: var(--danger);
  background: #fff5f5;
}
.reveal .gap-callout {
  border: 1px solid #e8d6b8;
  border-left: 0.24em solid var(--accent-2);
  background: #fffaf1;
  padding: 0.38em 0.54em;
}
.reveal .gap-callout p { font-size: 0.70em; }
.reveal .paper-dive { margin-top: 0.08em; }
.reveal .paper-dive .claim {
  border-left: 0.22em solid var(--accent);
  background: #ffffff;
  box-shadow: inset 0 0 0 1px var(--line);
  padding: 0.26em 0.40em;
  margin-bottom: 0.16em;
}
.reveal .paper-dive p,
.reveal .paper-dive li { font-size: 0.50em; line-height: 1.18; }
.reveal .paper-dive .tikz { margin: 0.04em auto 0; }
.reveal .paper-dive .tikz svg { width: 76%; max-width: 980px; }
.reveal .tikz {
  display: flex;
  justify-content: center;
  margin: 0.12em 0 0.28em;
  width: 100%;
}
.reveal .tikz svg {
  width: 100%;
  height: auto;
  max-width: 100%;
}
.reveal .diagram-full svg {
  width: 100%;
  max-width: 100%;
}
.reveal table { font-size: 0.66em; }
.reveal table th { color: var(--accent); }
</style>

## Why ordinary evaluation is not enough

::: {.split}
:::: {.comparison-card}
**Final-output evaluation** compresses a run to one bit and hides where it went wrong:

- did planning decompose the task?
- was the prompt + context constructed right?
- was the right tool called with the right args?
- was the observation interpreted correctly?
- did routing / verification / supervision fire?
::::

:::: {.comparison-card}
**Trajectory analysis** keeps that evidence and asks a sharper question:

- which step changed the future of the run?
- was the wrong answer caused there or downstream?
- what evidence supports that step as the cause?
- which control surface, if edited, would flip the outcome?
- and would the same edit hold across other runs?
::::
:::

::: {.builder-implication}
Engineering synthesis: instrument the execution path before tuning prompts. Otherwise every failure looks like "the model gave a bad answer."
:::

::: { .notes }
Build the argument step by step. A pass/fail eval compresses every intermediate decision — planning, prompt construction, tool choice, observation interpretation, routing — into one bit. A wrong final answer is almost never the original mistake; it is usually the *last visible symptom* of an upstream decision. So before you "tune the prompt," the cheapest leverage is to instrument the path so you can see *which* decision actually changed the future.
:::

## Formal execution model

Agent system $M = \{a_1, \ldots, a_N\}$ executes a task $\tau$ in discrete steps. At step $t$: select $a(t)=g(h_{t-1})$ → form input $x_t=\phi(h_{t-1})$ → run agent $y_t=\pi_{a(t)}(x_t)$ → update context $h_t=u(h_{t-1},a(t),x_t,y_t)$ → emit observable $o_t$.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  state/.style={draw, rounded corners, fill=blue!8, minimum width=2.4cm, minimum height=1.05cm, inner xsep=7pt, inner ysep=5pt, align=center, line width=0.5pt},
  data/.style={draw, rounded corners, fill=gray!10, minimum width=2.2cm, minimum height=0.95cm, inner xsep=6pt, inner ysep=4pt, align=center, line width=0.5pt},
  obs/.style={draw, rounded corners, dashed, fill=orange!10, minimum width=2.4cm, minimum height=0.95cm, inner xsep=6pt, inner ysep=4pt, align=center, line width=0.5pt},
  op/.style={draw, circle, fill=green!12, minimum size=0.75cm, inner sep=1pt, font=\sffamily\footnotesize\bfseries, line width=0.5pt},
  flow/.style={->, >=stealth, thick, line width=0.55pt}
}
% Source state on the left
\node[state] (hprev) at (0,0) {prior context\\$h_{t-1}$};
% Operator g (select) and phi (form input)
\node[op] (g) at (3.0,1.25) {$g$};
\node[op] (phi) at (3.0,-1.25) {$\phi$};
% Data products of step t
\node[data] (a) at (5.6,1.25) {selected agent\\$a(t)$};
\node[data] (x) at (5.6,-1.25) {formed input\\$x_t$};
% Operator pi (run agent policy on input)
\node[op] (pi) at (8.0,0) {$\pi$};
\node[data] (y) at (10.4,0) {agent output\\$y_t$};
% Operator u (update context)
\node[op] (u) at (12.8,0) {$u$};
\node[state] (hnext) at (15.2,0) {updated context\\$h_t$};
\node[obs] (o) at (15.2,-1.7) {observable record\\$o_t$};
% h_{t-1} fans out into g, phi, and u
\draw[flow] (hprev.north east) -- (g.west);
\draw[flow] (hprev.south east) -- (phi.west);
\draw[flow] (hprev.north) .. controls (2.1,3.05) and (12.8,3.05) .. (u.north);
% g produces a(t); phi produces x_t
\draw[flow] (g.east) -- (a.west);
\draw[flow] (phi.east) -- (x.west);
% a(t) and x_t both feed pi, which produces y_t
\draw[flow] (a.east) -- (pi.north west);
\draw[flow] (x.east) -- (pi.south west);
\draw[flow] (pi.east) -- (y.west);
% a(t), x_t, y_t all feed u (along with h_{t-1} from far above)
\draw[flow] (a.east) .. controls (9.5,1.25) and (12.0,0.85) .. (u.north west);
\draw[flow] (x.east) .. controls (9.5,-1.25) and (12.0,-0.85) .. (u.south west);
\draw[flow] (y.east) -- (u.west);
% u produces h_t; h_t emits observable o_t
\draw[flow] (u.east) -- (hnext.west);
\draw[flow, dashed, draw=black!55] (hnext.south) -- (o.north);
```

::: {.developer-intuition}
**selection** $g$, **input formation** $\phi$, **agent policy** $\pi_a$, **context update** $u$. Repair edits one of these.
:::

::: { .notes }
This is the only formal slide; spend ~30 seconds. Anchor the audience with one sentence: "the survey says an agent system runs as a discrete-step state machine." Then point at each of the four named functions as the four places a developer can edit later — selection picks the next agent, input formation builds the prompt/context, the agent policy is the LLM call, and the context update writes results back. Every repair we discuss later targets one of these four surfaces. This is the first-principles vocabulary for the rest of the talk.
:::

## Observed trace vs omitted state

::: {.slide-subtitle}
The survey contrasts partial-observability traces (outputs only) with full-observability traces that include inputs, prompts, and environment state.
:::

::: {.slide-claim}
Diagnosis can only use what was preserved at run time. Whatever the trace omits becomes a guess that taxonomy, attribution, and repair will inherit.
:::

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=2.25cm, minimum height=0.68cm, inner xsep=6pt, inner ysep=4pt, align=center, font=\sffamily\small}, smallbox/.style={draw, rounded corners, minimum width=1.88cm, minimum height=0.56cm, inner xsep=5pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=blue!8] (task) at (0,1.4) {task\\instruction};
\node[box, fill=blue!8] (prompt) at (2.7,1.4) {prompt +\\context};
\node[box, fill=green!10] (msg) at (5.4,1.4) {agent\\message};
\node[box, fill=green!10] (tool) at (8.1,1.4) {tool call\\+ args};
\node[box, fill=orange!12] (obs) at (10.8,1.4) {observation\\+ status};
\node[box, fill=red!9] (out) at (13.5,1.4) {final\\outcome};
\draw[->, very thick] (task) -- (prompt);
\draw[->, very thick] (prompt) -- (msg);
\draw[->, very thick] (msg) -- (tool);
\draw[->, very thick] (tool) -- (obs);
\draw[->, very thick] (obs) -- (out);
\node[smallbox, fill=gray!10] at (2.7,-0.15) {hidden prompt\\variants};
\node[smallbox, fill=gray!10] at (5.4,-0.15) {latent reasoning\\policy};
\node[smallbox, fill=gray!10] at (8.1,-0.15) {external API\\state};
\node[smallbox, fill=gray!10] at (10.8,-0.15) {environment\\state};
\draw[dashed, thick] (-1.55,0.50) rectangle (15.05,2.30);
\node[font=\sffamily\scriptsize, anchor=west, fill=white, inner sep=2pt] at (-1.45,2.30) {observable trace};
\node[font=\sffamily\scriptsize, anchor=west, text=gray!70!black] at (-1.20,-0.72) {omitted or latent context changes what attribution can prove};
```

::: {.builder-implication}
Trace quality is an engineering choice that bounds every later layer: define what evidence you need for taxonomy and attribution before you decide what the trace must capture.
:::

::: { .notes }
Walk the dashed box top-to-bottom: this is what is usually written to a log. Then point below the line: this is what is *not* written — the prompt variants the orchestrator tried, the latent reasoning policy of the LLM, external API state, environment state. Stress: every "I don't know why this failed" moment in production is one of these omitted boxes. So the rest of the deck is really a debate about *which* of these to bring above the dashed line, at what cost.
:::

## Engineering reading of the five dimensions

```{.tikz .diagram-full}
\tikzset{
  layer/.style={draw, rounded corners, minimum width=8.4cm, minimum height=0.66cm, inner xsep=8pt, inner ysep=4pt, align=center, font=\sffamily\small},
  side/.style={draw, rounded corners, minimum width=3.4cm, minimum height=2.6cm, text width=2.9cm, inner xsep=6pt, inner ysep=4pt, align=center, font=\sffamily\small},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[layer, fill=green!10] (tax) at (0,3.3) {failure taxonomy $\rightarrow$ define what counts as a failure};
\node[layer, fill=orange!12] (att) at (0,2.2) {failure attribution $\rightarrow$ specify what we must prove};
\node[layer, fill=blue!8] (mon) at (0,1.1) {monitoring tools $\rightarrow$ capture the required evidence};
\node[layer, fill=red!8] (enh) at (0,0) {system enhancement $\rightarrow$ edit the implicated control surface};
\node[side, fill=yellow!10] (bench) at (6.85,1.65) {datasets and benchmarks\\train, compare, and regress each layer};
\draw[->, very thick] (tax) -- (att);
\draw[->, very thick] (att) -- (mon);
\draw[->, very thick] (mon) -- (enh);
\draw[->, thick] (bench.north west) to[out=180, in=0] (tax.east);
\draw[->, thick] (bench.west) -- (att.east);
\draw[->, thick] (bench.south west) to[out=180, in=0] (enh.east);
```

::: { .notes }
Frame this as the survey's five dimensions reordered into the engineering thinking sequence:
taxonomy first (define what counts as a failure), then attribution (specify what we must prove),
then monitoring (capture the evidence the first two demand), then enhancement (edit the implicated
control surface). Datasets and benchmarks sit alongside, training and regressing every layer.

This whole stack assumes the trace substrate established in the previous slide --- raw trajectories
(messages, tools, observations, state, outcomes) underpin every layer; we don't redraw them here
because they are not one of the five survey dimensions.

So when speaking, emphasize: define -> specify -> capture -> repair, with benchmarks closing the loop.
:::

# Failure Taxonomy

- **What it is:** A shared map of *what kind of failure* a trajectory exhibits.
- **Why it matters:** The category chooses the diagnostic search space (plan, memory, handoff, verifier, environment, runtime).
- **What it enables next:** Targeted attribution and repair instead of one-size-fits-all prompt tweaks.

::: { .notes }
Open the area by asking: "before we can debug a trajectory, what do we even *call* the bug?" Without a shared vocabulary, every failed run is a one-off anecdote. Taxonomy is the schema layer — it turns "the agent failed" into "the agent had a *planning decomposition* failure" or "an *inter-agent alignment* failure". The category chosen here narrows where attribution will look later, which is why this is step 1 in the engineering chain.
:::

## What a taxonomy must make explicit

For a failed trajectory $T$, a taxonomy should assign $\tau(T, e) \rightarrow (\text{view}, \text{failure type}, \text{evidence span}, \text{repair hint})$, where $e$ is the trace evidence that supports the class.

- **View:** Which lens the failure is read through (phase, capability module, system/interaction, domain).
- **Failure type:** A short, repair-oriented label that distinguishes this failure from other categories.
- **Evidence span:** The trace segment that justifies the label (plan step, memory update, handoff, verifier event, environment effect).
- **Repair hint:** What a developer should look at next: prompt, context update, workflow graph, verifier, tool, or supervisor.

::: {.builder-implication}
The important choice is not the label name. It is what evidence the label asks a developer to inspect and what repair it makes plausible.
:::

::: { .notes }
Frame this as a function from a failed trajectory $T$ + supporting evidence $e$ to four things: which lens the failure is read through, the failure type, the trace span that justifies the label, and the repair direction. Stress that "labelling well" is not about the prettiest name — it is about whether the label tells the next person *where to look* in the trace and *what to change* in the system.
:::

## Four ways to choose the diagnostic search space

::: {.slide-subtitle}
Four complementary perspectives the survey identifies for organizing failure taxonomies.
:::

```{.tikz .diagram-full}
\tikzset{
  perspective/.style={draw, rounded corners, minimum width=4.10cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\small\bfseries},
  meaning/.style={draw, rounded corners, minimum width=4.85cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  example/.style={draw, rounded corners, minimum width=4.85cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
% Header row
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (0,3.55) {Name};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (5.20,3.55) {What it is};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (10.55,3.55) {Examples};

% Row 1: Task-execution phase
\node[perspective, fill=blue!8] (n1) at (0,2.7) {task-execution phase};
\node[meaning, fill=blue!4] (w1) at (5.20,2.7) {read the trace chronologically as a pipeline};
\node[example, fill=blue!4] (e1) at (10.55,2.7) {planning, execution, response (Lu)};
\draw[->, thick] (n1) -- (w1);
\draw[->, thick] (w1) -- (e1);

% Row 2: Agent capability module
\node[perspective, fill=green!10] (n2) at (0,1.8) {agent capability module};
\node[meaning, fill=green!5] (w2) at (5.20,1.8) {classify by the cognitive function that broke};
\node[example, fill=green!5] (e2) at (10.55,1.8) {memory, reflection, planning, action, system};
\draw[->, thick] (n2) -- (w2);
\draw[->, thick] (w2) -- (e2);

% Row 3: System and interaction
\node[perspective, fill=orange!12] (n3) at (0,0.9) {system and interaction};
\node[meaning, fill=orange!5] (w3) at (5.20,0.9) {classify by multi-agent organization};
\node[example, fill=orange!5] (e3) at (10.55,0.9) {specification, alignment, verification (MAST)};
\draw[->, thick] (n3) -- (w3);
\draw[->, thick] (w3) -- (e3);

% Row 4: Domain-specific interaction
\node[perspective, fill=red!8] (n4) at (0,0) {domain-specific interaction};
\node[meaning, fill=red!4] (w4) at (5.20,0) {classify by environment-shaped behavior};
\node[example, fill=red!4] (e4) at (10.55,0) {exploration, exploitation, resources (Aegis-Song)};
\draw[->, thick] (n4) -- (w4);
\draw[->, thick] (w4) -- (e4);
```

::: { .notes }
The survey calls these "four complementary perspectives" — they are *not* a hierarchy and not a decision tree. They are four lenses for reading the *same* failed trajectory. Phase asks "where in the pipeline?", capability module asks "which cognitive function?", system & interaction asks "where in the multi-agent organization?", domain-specific asks "which environment-shaped behavior?". The right perspective is whichever maps cleanly to a different repair surface.
:::

## Highlight papers

::: {.slide-subtitle}
Taxonomy becomes a repair decision.
:::

::: {.mini-grid}
:::: {.case}
**01 Lu: phase boundaries**

Cut the trace into planning / execution / response and label faults per phase.
::::

:::: {.case}
**06 MAST: MAS organization**

Sort failures by specification, inter-agent alignment, or verification gaps.
::::

:::: {.case}
**04 AgentRx: unrecoverable labels**

Tie labels to violated constraints at the earliest unrecoverable step.
::::

:::: {.case}
**07 Aegis-Song: environment-shaped failures**

Classify exploration, exploitation, and resource-exhaustion failures of agent–environment interaction.
::::
:::

::: { .notes }
Three exemplars, one for each useful taxonomy stance. Lu = chronological/phase view (when in the pipeline). MAST = multi-agent organizational view (where in the org chart). AgentRx = repair-oriented view (which constraint was violated and at what step). Each one is a defensible answer to the same question; pick whichever maps to your own repair surface.
:::

## 01 Lu — phase boundaries

- **Claim:** Align labels with the execution phase where evidence first appears: planning, execution, or response generation.
- **Trace lens:** The unit of analysis is a phase-delimited run log, not a single final answer.
- **Developer takeaway:** Add phase boundaries to traces before building dashboards or evaluators.

```{.tikz .diagram-full}
\tikzset{phase/.style={draw, rounded corners, minimum width=2.45cm, minimum height=0.58cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\small}, label/.style={font=\sffamily\scriptsize, align=center}, every node/.style={font=\sffamily\small}, >=stealth}
\node[phase, fill=blue!8] (p) at (0,0) {planning};
\node[phase, fill=green!10] (e) at (3.0,0) {execution};
\node[phase, fill=orange!12] (r) at (6.0,0) {response};
\node[phase, fill=red!8] (o) at (9.0,0) {outcome};
\draw[->, very thick] (p) -- (e);
\draw[->, very thick] (e) -- (r);
\draw[->, very thick] (r) -- (o);
\node[label] at (0,-0.88) {decomposition fault};
\node[label] at (3.0,-0.88) {tool-use fault};
\node[label] at (6.0,-0.88) {unsupported claim};
\draw[dashed, thick] (-1.45,0.52) -- (-1.45,-1.32);
\draw[dashed, thick] (1.5,0.52) -- (1.5,-1.32);
\draw[dashed, thick] (4.5,0.52) -- (4.5,-1.32);
\draw[dashed, thick] (7.5,0.52) -- (7.5,-1.32);
\node[label, text=gray!70!black] at (4.5,0.88) {taxonomy starts by choosing phase cuts in the trajectory};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Lu et al. inspect 104 failures from 204 runs across TaskWeaver, MetaGPT, and AutoGen on 34 programmable tasks (web crawl, data analysis, file ops); the three phases unfold into 19 lower-level causes (improper decomposition, failed self-refinement loops, code/API errors, env setup, formatting, context-window limits, max-round failures).
- **Q: How do they actually decide which phase failed?** They label failed runs from full execution logs — planner prompts/plans, generated code, tool calls, execution outputs, and per-iteration behavior — not from the final answer alone.
- **Q: Planning vs execution failure?** Planning = decomposition, unrealistic plan, failed self-refinement; execution = bad code, API/schema mistakes, tool-use failure, env setup.
- **Q: What must I log if I want this taxonomy in my agent?** Planner prompts/plans, generated code or tool calls, execution outputs/errors, repair attempts, and final response formatting — without those phase boundaries, final failures cannot be mapped back to the broken component.
- **Q: Just classification, or does it suggest fixes?** Suggests feedback-aware replanning, meta-control routing, and early stopping when the same unresolved error repeats; reports diminishing returns from raising iteration limits.
:::

## 06 MAST — multi-agent organization

- **Claim:** Shift taxonomy from a single-agent timeline to the organizational structure of multi-agent systems.
- **Trace lens:** Evidence is not only what one agent did, but whether role specification, inter-agent alignment, or task verification failed.
- **Developer takeaway:** If the trace has handoffs and role contracts, classify coordination before changing prompts; the repair may be topology or verifier design.

```{.tikz .diagram-full}
\tikzset{axis/.style={draw, rounded corners, minimum width=2.9cm, minimum height=0.58cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\small}, trace/.style={draw, rounded corners, minimum width=1.9cm, minimum height=0.46cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize}, every node/.style={font=\sffamily\small}, >=stealth}
\node[trace, fill=gray!10] (a) at (0,1.1) {agent A};
\node[trace, fill=gray!10] (b) at (3.25,1.1) {agent B};
\node[trace, fill=gray!10] (v) at (6.5,1.1) {verifier};
\draw[->, thick] (a) -- (b);
\draw[->, thick] (b) -- (v);
\node[axis, fill=blue!8] (ax1) at (0,-0.35) {system design};
\node[axis, fill=orange!12] (ax2) at (3.25,-0.35) {inter-agent alignment};
\node[axis, fill=green!10] (ax3) at (6.5,-0.35) {task verification};
\draw[->, thick] (a) -- (ax1);
\draw[->, thick] (b) -- (ax2);
\draw[->, thick] (v) -- (ax3);
\node[font=\sffamily\scriptsize, align=center, text=gray!70!black] at (3.25,-1.15) {MAST asks whether failure is caused by specification, coordination, or verification};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** MAST is empirically derived via Grounded Theory over 150+ traces, validated with expert annotators (strong inter-annotator agreement). The taxonomy contains **14 failure modes** under three buckets — repetition, context loss, unclear termination, conversation reset, missed clarification, reasoning–action mismatch, premature termination, incomplete or incorrect verification, etc. They release MAST-Data (1,642 annotated traces from 7 MAS frameworks) and a few-shot LLM annotator (`agentdash`) for scalable labeling.
- **Q: What's actually in the three buckets?** System design issues (specification, role/topology), inter-agent misalignment (coordination, hand-off, context-loss), task verification (incomplete or incorrect verification, premature termination).
- **Q: How was the taxonomy built rather than invented?** Grounded Theory: open-coding traces, refined with expert rounds, then scaled with an LLM judge that uses MAST definitions and few-shot exemplars.
- **Q: How would a developer use MAST day-to-day?** Annotate failed traces with MAST labels → inspect the failure-mode distribution → change the system → check whether the targeted modes drop. Debugging by failure profile, not by anecdote.
- **Q: Does MAST identify the responsible agent and decisive step?** No — that's the Who&When framing. MAST explains organizational failure patterns; Who&When pinpoints culprit-agent and step.
:::

## 04 AgentRx — unrecoverable labels

- **Claim:** Make taxonomy repair-oriented by tying labels to evidence-backed constraints and the first unrecoverable critical failure.
- **Trace lens:** The trace is checked step by step against policies, tool schemas, prefix constraints, and task-specific requirements.
- **Developer takeaway:** Convert informal "agent rules" into checkable constraints; labels become useful when they explain why no later step could recover.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  stage/.style={draw, rounded corners, minimum width=3.6cm, minimum height=1.15cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt},
  io/.style={draw, rounded corners, dashed, minimum width=2.3cm, minimum height=0.85cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt, text=black!75},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60}
}
\node[io] (traj) at (0,1.65) {Failed\\Trajectory};
\node[stage, fill=blue!8] (s1) at (0,0)
  {1. Constraint Synthesis\\[1pt]\scriptsize global from schema + policy,\\\scriptsize dynamic from prefix};
\node[stage, fill=orange!10] (s2) at (5.0,0)
  {2. Validation Log\\[1pt]\scriptsize step-indexed violations\\\scriptsize with evidence};
\node[stage, fill=red!8] (s3) at (10.0,0)
  {3. Root-Cause Judge\\[1pt]\scriptsize first unrecoverable step,\\\scriptsize taxonomy label};
\node[io] (out) at (10.0,-1.65) {Critical Step\\+ Failure Category};
\draw[flow] (traj) -- (s1);
\draw[flow] (s1) -- node[edgelbl, above] {Constraints} (s2);
\draw[flow] (s2) -- node[edgelbl, above] {Validation Log} (s3);
\draw[flow] (s3) -- (out);
```

::: { .notes }
*Pitch:* AgentRx is our worked deep-dive for taxonomy — turn vague "agent rules" into checkable constraints, log violations, then judge the first unrecoverable step.

- Three stages: synthesise constraints, log violations, judge root cause.
- 9 taxonomy labels span three axes: plan, grounding, execution.
- Mirrors the paper's Fig. 2 — failed trajectory in, critical step + category out.
:::

## AgentRx — Stage 1: constraint synthesis

::: {.slide-subtitle}
Turn vague "agent rules" into checkable constraints anchored at each step.
:::

- **Global constraints** built once from the **tool schema** and **domain policy** — schema-valid invocation, declared policy compliance.
- **Dynamic constraints** built per step from the **task instruction** and **observed prefix** — consistency with the latest tool output, prefix-implied obligations.
- **Guarded evaluation** — each constraint runs only when its precondition fires; checks are programmatic when possible, semantic (LLM-judged) otherwise.
- **Output:** a step-keyed map from constraint to satisfied / violated / not-applicable.

::: { .notes }
*Pitch:* A raw trajectory is too messy to diagnose. AgentRx first translates rules and context into things you can mechanically check.

- Global = schema + policy, set once. Dynamic = re-synthesised at each step k from prefix τ_{≤k}.
- "Guarded" = constraints carry a precondition; only fire when relevant — avoids irrelevant violations.
:::

## AgentRx — Stage 2: validation log

::: {.slide-subtitle}
Compress violations so the judge reads evidence, not the whole trace.
:::

- **Record only violations**, not the whole trace — keeps the log compact.
- **Attach supporting evidence** to every violation — the tool output, prefix span, or policy clause that triggered it.
- **Step-keyed and auditable** — the judge can trace a final failure backward through dependent violations.
- **Output:** a validation log of step, broken constraint, and supporting evidence per violation.

::: { .notes }
*Pitch:* The judge doesn't read the raw trace; it reads a "diagnostic receipt" of what broke, where, and with what evidence.

- Formally `V`: list of `(step, constraint, evidence)` tuples.
- Compact log shrinks LLM context vs raw transcript — main reason judge stays accurate on long traces.
:::

## AgentRx — Stage 3: root-cause judge

::: {.slide-subtitle}
Pick the first step from which the agent does not recover, and label its cause.
:::

- **Find the first unrecoverable step** — not the first error; the first violation that explains terminal failure.
- **Plan-axis labels** *(plan: did the agent pursue the right intent?)* — instruction adherence, intent–plan mismatch, under-specified intent, unsupported intent.
- **Grounding-axis labels** *(grounding: did evidence stay faithful?)* — invented facts, tool-output misread, handoff failure.
- **Execution-axis labels** *(execution: did action complete?)* — invalid invocation, guardrails triggered, system failure.
- **Output:** critical failure step, failure category, and a short rationale.

::: { .notes }
*Pitch:* "Unrecoverable" matters more than "first error" — earlier mistakes often get fixed by retries; the decisive failure is the one the agent can't escape.

- 9 labels along 3 axes: plan / grounding / execution. Each axis names *where* the run went off the rails.
- Concrete example: WebSurfer error → orchestrator switched tools → recovered. True root cause was a later file-path hallucination.
- Headline: **+23.6%** failure localisation, **+22.9%** root-causing vs prior baselines on 115 trajectories across τ-bench, Flash, Magentic-One.
- Trace fields needed: stable step IDs, agent + tool names, tool I/O, observed env state, tool schema, optional policy.
:::

## 07 Aegis-Song — environment-shaped failures

- **Claim:** Classify failures by **agent–environment interaction**, not by chronology or capability module.
- **Trace lens:** Group failures into exploration (incomplete information gathering), exploitation (mis-processing of gathered information), and resource exhaustion (turn or token budget).
- **Developer takeaway:** When the symptom is "the agent ran out of budget" or "explored too narrowly," the right repair surface is the **environment / tool interface**, not the prompt.

```{.tikz .diagram-full}
\tikzset{
  bucket/.style={draw, rounded corners, minimum width=4.10cm, minimum height=0.52cm, inner xsep=4pt, inner ysep=2pt, align=center, font=\sffamily\small\bfseries},
  body/.style={draw, rounded corners, minimum width=5.50cm, minimum height=0.52cm, inner xsep=4pt, inner ysep=2pt, align=center, font=\sffamily\scriptsize},
  example/.style={draw, rounded corners, minimum width=3.80cm, minimum height=0.52cm, inner xsep=4pt, inner ysep=2pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[font=\sffamily\scriptsize\bfseries, text=gray!70!black] at (0,2.18) {Bucket};
\node[font=\sffamily\scriptsize\bfseries, text=gray!70!black] at (5.35,2.18) {What it is};
\node[font=\sffamily\scriptsize\bfseries, text=gray!70!black] at (10.55,2.18) {Sub-failures};

\node[bucket, fill=blue!8] (b1) at (0,1.45) {exploration};
\node[body, fill=blue!4] (w1) at (5.35,1.45) {agent fails to gather all required info};
\node[example, fill=blue!4] (e1) at (10.55,1.45) {state-space, tool-use};
\draw[->, thick] (b1) -- (w1);
\draw[->, thick] (w1) -- (e1);

\node[bucket, fill=green!10] (b2) at (0,0.72) {exploitation};
\node[body, fill=green!5] (w2) at (5.35,0.72) {agent mis-processes information it has};
\node[example, fill=green!5] (e2) at (10.55,0.72) {tool-output, domain-rule};
\draw[->, thick] (b2) -- (w2);
\draw[->, thick] (w2) -- (e2);

\node[bucket, fill=orange!12] (b3) at (0,0) {resource exhaustion};
\node[body, fill=orange!5] (w3) at (5.35,0) {budget runs out before task completes};
\node[example, fill=orange!5] (e3) at (10.55,0) {turns, tokens};
\draw[->, thick] (b3) -- (w3);
\draw[->, thick] (w3) -- (e3);
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Aegis-Song proposes a **taxonomy specific to agent–environment interaction**, distinct from phase-based or module-based taxonomies. Three top-level buckets — exploration, exploitation, resource exhaustion — each unfold into concrete sub-failures (e.g., state-space navigation, tool-output processing, domain-rule violation, turn/token budget).
- **Q: Why isn't this just "tool-use failure"?** A pure tool-use category collapses three different failure shapes: not gathering enough state (exploration), corrupting gathered state (exploitation), and running out of budget (resource exhaustion). Each shape implies a different repair.
- **Q: Why does this taxonomy belong in the *taxonomy* area at all?** Because it is the survey's only published taxonomy for environment-side failures — phase and capability-module taxonomies do not capture interaction or budget exhaustion as first-class categories.
- **Q: When should I reach for it instead of MAST or Lu?** When your agent operates in a closed environment with a finite budget (web/embodied/tool-rich), the repair surface is the **environment + interface**, and Aegis-Song labels point you straight at it.
- **Q: Same paper as 21 Aegis-Kong?** No — these are two distinct papers that happen to share the name "Aegis." Aegis-Song is the taxonomy paper (cited here); Aegis-Kong is the synthetic-error-injection attribution paper that appears in the fine-tuning row of the attribution table.
:::

# Failure Attribution

- **What it is:** Identify *who* caused the failure, *when* it became unrecoverable, and *why* the symptom appeared there.
- **Why it matters:** Repair without attribution is guesswork; attribution narrows the component to change.
- **What it enables next:** A concrete repair target: policy, prompt, context update, handoff, verifier, tool, or runtime controller.

::: { .notes }
Now that we have a label, we ask the harder question: *who, when, and why*. Frame attribution as the sharpest dependency in the chain — without it, repair is guesswork because we don't know which control surface to edit. This is also where the survey reports the field is least mature: agent-level accuracy is OK, but *exact step* is still hard.
:::

## When failure becomes inevitable

- Let $\Omega(T) \in \{0,1\}$ be the task outcome of trajectory $T$.
- A step $t$ is the failure boundary if every feasible continuation from $T_{\leq t}$ fails (the survey's "step at which failure becomes inevitable").
- **Target:** Attribution looks for $t^\ast$, the earliest such step — typically far before the visible wrong answer. **In practice,** methods approximate $t^\ast$ with labels.

```{.tikz .diagram-full}
\tikzset{traceStep/.style={draw, rounded corners, minimum width=1.65cm, minimum height=0.52cm, inner xsep=5pt, inner ysep=3pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[traceStep, fill=blue!8] (t1) at (0,0) {$T_{\leq 1}$};
\node[traceStep, fill=blue!8] (t2) at (2.45,0) {$T_{\leq 2}$};
\node[traceStep, fill=orange!18, very thick] (ts) at (4.9,0) {$T_{\leq t^\ast}$};
\node[traceStep, fill=red!9] (t4) at (7.35,0) {$T_{\leq 4}$};
\node[traceStep, fill=red!12] (tf) at (9.8,0) {wrong answer};
\draw[->, very thick] (t1) -- (t2);
\draw[->, very thick] (t2) -- (ts);
\draw[->, very thick] (ts) -- (t4);
\draw[->, very thick] (t4) -- (tf);
\node[traceStep, fill=green!10] (sp) at (7.35,1.00) {success path};
\node[traceStep, fill=red!9]  (fb) at (10.4,-0.85) {failure basin};
\draw[->, thick, green!50!black] (t2) to[out=60, in=180] (sp.west);
\draw[->, thick, red!80!black]  (ts) to[out=-60, in=180] (fb.west);
\draw[dashed, very thick, orange!90!black] (4.9,-1.20) -- (4.9,1.55);
\node[font=\sffamily\scriptsize, align=center] at (4.9,1.82) {earliest inevitable step $t^\ast$};
\node[font=\sffamily\scriptsize, align=center] at (9.8,0.78) {observed symptom};
```

::: { .notes }
The bullets and the diagram say the same thing. Walk through the diagram from $T_{\leq 1}$ left to right: the green arrow shows that *up to step 2* a successful continuation still existed, and from $T_{\leq t^\ast}$ onward every continuation falls into the failure basin. That step $t^\ast$ — not the wrong-answer cell at the right — is what attribution should target. Then close with the practical caveat: real systems can't enumerate "all feasible continuations", so the methods we'll see next *approximate* $t^\ast$ in different ways.
:::

## Four ways to justify blame

::: {.slide-subtitle}
Four attribution paradigms the survey identifies, ordered by analytical depth.
:::

```{.tikz .diagram-full}
\tikzset{
  perspective/.style={draw, rounded corners, minimum width=4.10cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\small\bfseries},
  meaning/.style={draw, rounded corners, minimum width=4.85cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  example/.style={draw, rounded corners, minimum width=4.85cm, minimum height=0.62cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (0,3.55) {Name};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (5.20,3.55) {What it is};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (10.55,3.55) {Examples};

\node[perspective, fill=blue!8] (n1) at (0,2.7) {pattern analysis};
\node[meaning, fill=blue!4] (w1) at (5.20,2.7) {mine recurring signals across traces};
\node[example, fill=blue!4] (e1) at (10.55,2.7) {FAMAS spectrum-based analysis};
\draw[->, thick] (n1) -- (w1);
\draw[->, thick] (w1) -- (e1);

\node[perspective, fill=green!10] (n2) at (0,1.8) {LLM reasoning};
\node[meaning, fill=green!5] (w2) at (5.20,1.8) {LLM-structured reasoning over trace/context};
\node[example, fill=green!5] (e2) at (10.55,1.8) {ECHO, RAFFLES, A2P, CHIEF};
\draw[->, thick] (n2) -- (w2);
\draw[->, thick] (w2) -- (e2);

\node[perspective, fill=orange!12] (n3) at (0,0.9) {model fine-tuning};
\node[meaning, fill=orange!5] (w3) at (5.20,0.9) {train a specialized tracer model};
\node[example, fill=orange!5] (e3) at (10.55,0.9) {GraphTracer, AgenTracer, Aegis-Kong};
\draw[->, thick] (n3) -- (w3);
\draw[->, thick] (w3) -- (e3);

\node[perspective, fill=red!8] (n4) at (0,0) {dynamic runtime};
\node[meaning, fill=red!4] (w4) at (5.20,0) {edit and replay to test causal hypotheses};
\node[example, fill=red!4] (e4) at (10.55,0) {DoVer interventions};
\draw[->, thick] (n4) -- (w4);
\draw[->, thick] (w4) -- (e4);
```

::: { .notes }
Read down the table as a progression of analytical depth. Pattern analysis is the cheapest — mine many traces for a recurring signal, no runtime intervention. LLM reasoning adds a model in the loop with structured prompting, hierarchical context, consensus voting, etc. Model fine-tuning trains a *specialized* tracer model — more accurate but more data-hungry. Dynamic runtime is the most ambitious — actually re-run the agent with a counterfactual edit and see if the failure flips. The deeper down the table, the stronger the causal evidence — and the more infrastructure you need.
:::

## Highlight papers

::: {.slide-subtitle}
Attribution targets and evidence.
:::

::: {.mini-grid}
:::: {.case}
**13 Who&When: label target**

Defines culprit-agent and decisive-step labels for benchmarkable attribution.
::::

:::: {.case}
**18 CHIEF: causal structure**

Converts flat logs into hierarchical causal graphs and back-traces dependencies.
::::

:::: {.case}
**22 DoVer: intervention evidence**

Edits the orchestrator message or plan and replays to validate the hypothesis.
::::
:::

::: { .notes }
Three attribution exemplars, picked to span analytical depth. Who&When: defines the target — culprit agent + decisive step. CHIEF: deepens the *structure* by replacing the flat log with a hierarchical causal graph and backtracking through dependencies. DoVer: adds *intervention* — actually edit and replay to test the hypothesis. Together they cover labelling, structural reasoning, and active causal testing.
:::

## 13 Who&When — attribution view

- **Claim:** Turn failure attribution into a labeled target: identify the responsible agent and the earliest decisive error step.
- **Trace lens:** The required object is an indexed multi-agent log with agent names, step numbers, task context, and outcome evidence.
- **Developer takeaway:** Store stable agent and step identifiers; without them, "who failed" and "when it became unrecoverable" cannot be benchmarked or audited.

```{.tikz .diagram-full}
\tikzset{tracebox/.style={draw, rounded corners, minimum width=1.35cm, minimum height=0.50cm, inner xsep=5pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize}, label/.style={draw, rounded corners, minimum width=2.45cm, minimum height=0.55cm, inner xsep=6pt, inner ysep=4pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[tracebox, fill=blue!8] (a1) at (0,0.7) {A:t1};
\node[tracebox, fill=blue!8] (b2) at (1.8,0.7) {B:t2};
\node[tracebox, fill=red!10, very thick] (a3) at (3.6,0.7) {A:t3};
\node[tracebox, fill=gray!10] (b4) at (5.4,0.7) {B:t4};
\node[tracebox, fill=red!8] (out) at (7.2,0.7) {fail};
\foreach \x/\y in {a1/b2,b2/a3,a3/b4,b4/out} {\draw[->, thick] (\x) -- (\y);}
\node[label, fill=orange!12] (who) at (2.55,-0.65) {who: agent A};
\node[label, fill=orange!12] (when) at (5.15,-0.65) {when: step 3};
\draw[->, thick] (a3) -- (who);
\draw[->, thick] (a3) -- (when);
\node[font=\sffamily\scriptsize, text=gray!70!black] at (3.65,1.38) {label target = agent-step pair, not just failed output};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Decisive error is defined **counterfactually**: replace the action at step $t$ with a correct one and the run flips from failure to success. If multiple decisive errors exist, pick the **earliest**. Dataset: 184 failure annotation tasks from 127 MAS (CaptainAgent-generated systems + Magnetic-One on GAIA / AssistantBench). Each item ships with query, full failure log, system info, responsible agent, decisive step, NL reason. Baselines: all-at-once, step-by-step, binary search, hybrid.
- **Q: What exactly is the "when" label?** Earliest step where correcting that agent's action would change the outcome from failure to success.
- **Q: How are labels produced?** Expert annotators inspect failed multi-agent logs and assign agent + decisive step + reason; uncertainty resolved by discussion. The paper notes manual attribution itself is hard.
- **Q: What auto-attribution methods do they test?** All-at-once full-log judgment, step-by-step incremental checking, binary-search localization, and a hybrid (all-at-once for agent + step-by-step for step).
- **Q: What must my trace contain to support this benchmark style?** Stable agent names, step numbers, task context, full failure log, final outcome evidence; ground-truth answer when available. Without explicit IDs, "who" and "when" cannot be reproduced.
:::

## 18 CHIEF — causal structure

- **Claim:** Deepen attribution by converting flat logs into hierarchical causal graphs, then backtracking through dependencies and counterfactual screens.
- **Trace lens:** The useful record contains subtasks, agents, structured step records, handoffs, data references, loops, and tool inputs/outputs.
- **Developer takeaway:** Preserve edges, not just events. Propagation paths explain why the visible bad step may be a downstream symptom.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  stage/.style={draw, rounded corners, minimum width=3.6cm, minimum height=1.15cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt},
  io/.style={draw, rounded corners, dashed, minimum width=2.3cm, minimum height=0.85cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt, text=black!75},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60}
}
\node[io] (traj) at (-4.0,0) {Agent\\Trajectory};
\node[stage, fill=blue!8] (s1) at (0,0)
  {1. Causal Graph Construction\\[1pt]\scriptsize OTAR parsing,\\\scriptsize subtask / agent / step edges};
\node[stage, fill=orange!10] (s2) at (5.8,1.10)
  {2. Oracle-Guided Backtracking\\[1pt]\scriptsize oracle synthesis,\\\scriptsize subtask, agent, step};
\node[stage, fill=red!8] (s3) at (5.8,-1.10)
  {3. Counterfactual Attribution\\[1pt]\scriptsize local, planning-control,\\\scriptsize data-flow, deviation};
\node[io] (out) at (9.8,-1.10) {Failure Agent\\+ Failure Step};
\draw[flow] (traj) -- (s1);
\draw[flow] (s1.north east) -- node[edgelbl, sloped, above, inner sep=1pt] {Causal Graph} (s2.west);
\draw[flow] (s2.south) -- node[edgelbl, right=2pt] {Failure Candidates} (s3.north);
\draw[flow] (s3) -- (out);
```

::: { .notes }
*Pitch:* CHIEF is our worked deep-dive for attribution — *structural evidence*, paired later with DoVer's *experimental evidence*. Three stages, simplified from the paper's Fig. 2.

- L-shape mirrors the paper.
- Two outputs (Failure Agent + Failure Step) = the root-cause pair $(a^\ast, x^\ast)$, presented as one diagnosis.
:::

## CHIEF — Stage 1: causal graph construction

::: {.slide-subtitle}
Make the flat trace structurally readable.
:::

- **Per step → OTAR.** Extend the prior `Thought / Action / Result` with **Observation** — a slot for what each step received.
- **Trace → subtasks.** RAG decomposition + trajectory-aligned reflection.
- **Three typed edges:** subtask order, agent collaboration, and step-level data flow (upstream `Result` → downstream `Observation`).
- **Output:** a hierarchical causal graph with subtask and agent nodes.

::: { .notes }
*Pitch:* TAR had no slot for what an agent *received* — so cross-agent data flow could only be guessed. Observation fixes that, and the step-edge has somewhere to land.

- TAR baseline: Bouzenia & Pradel, 2025. Formally `G = (V, E)` with edges `E_sub`, `E_agt`, `E_step`.
- 2 node types (subtask, agent), 3 edge types — don't conflate.
- Subtasks come from RAG decomposition + reflection; OTAR from a strict-template LLM parser.
:::

## CHIEF — Stage 2: oracle-guided backtracking

::: {.slide-subtitle}
Build a per-subtask checklist; walk the graph through it coarse to fine.
:::

- **Per-subtask oracle.** A 4-line LLM-written checklist: *Goal* (what this phase should achieve), *Pre* (what must hold before it starts), *Evidence* (facts/tool returns to verify), *Pass/Fail* (falsifiable post-hoc).
- **Top-down walk** (reverse topological): subtask fails Pass/Fail → agent OTAR violates Pre/Evidence → step breaks the checklist.
- **Prune** subgraphs that pass their oracle; drill into the rest.
- **Output:** Failure Candidates — narrowed, not yet proven causes.

::: { .notes }
*Pitch:* Build a checklist per subtask, then walk failures into it from outside in.

- Oracle is a labelled text block, not JSON. One prompt drafts all $O_k$ sequentially + self-checks for non-contradiction. Inputs: question, conversation log, RAG exemplars, subtask plan, prior oracles.
- Sees the failed trace — but preconditions must reference *upstream-only* state, so it can't peek ahead. That's how `w/o G` works.
- Reverse topological = failures show at the end; passed subgraphs pruned. Categorical-before-atomic is why three levels, not just step.
:::

## CHIEF — Stage 3: counterfactual attribution

::: {.slide-subtitle}
Filter candidates along three axes: scope, propagation, persistence.
:::

- **Local** *(scope: did it start here?)* — no upstream cause explains the bad output → origin is the step itself.
- **Planning-control** *(propagation: control loop?)* — planner repeats the same plan after error signals, or executor keeps violating valid replans.
- **Data-flow** *(propagation: corrupted value?)* — walk step-edges back to the earliest step where valid inputs first became wrong.
- **Deviation-aware** *(persistence: did it stick?)* — drop the candidate if a later step re-satisfies the oracle.
- **Output:** one tuple `(Agent, Step, Reason)`.

::: { .notes }
*Pitch:* Three axes, not four arbitrary rules. Where it started → how it propagated → whether it stuck.

- Sequential pipeline ("progressive causal screening"), not parallel vote: Local first; if non-local, split control-vs-data; reversibility filters last.
- Final pick: true origin (local OR earliest corruption) ∧ passes reversibility ∧ strongest downstream impact.
- Who&When: **77.6% / 29.3%** hand-crafted, **76.8% / 52.0%** algorithm-generated (agent / step). Beats 8 baselines.
:::

## 22 DoVer — intervention evidence

- **Claim:** Treat attribution as an experimental question: hypothesize a failure point, edit the orchestrator message or plan, and replay from that checkpoint.
- **Trace lens:** The trace must be segmentable into trials with preserved context, checkpoints, and milestone evaluations.
- **Developer takeaway:** Build replay hooks early; causal confidence improves when suspected failures can be validated, refuted, or marked inconclusive.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  stage/.style={draw, rounded corners, minimum width=3.6cm, minimum height=1.15cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt},
  io/.style={draw, rounded corners, dashed, minimum width=2.3cm, minimum height=0.85cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt, text=black!75},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60, align=center}
}
\node[io] (sess) at (0,1.85) {Failed\\Session Log};
\node[stage, fill=blue!8] (s1) at (0,0)
  {1. Trial Segmentation\\[1pt]\scriptsize cut at re-plan steps,\\\scriptsize one hypothesis per span};
\node[stage, fill=orange!10] (s2) at (5.6,0)
  {2. Hypothesise + Intervene\\[1pt]\scriptsize suspected step + rationale,\\\scriptsize edit orchestrator plan};
\node[stage, fill=red!8] (s3) at (11.2,0)
  {3. Replay + Verify\\[1pt]\scriptsize re-run from checkpoint,\\\scriptsize outcome + milestone check};
\node[io] (out) at (11.2,-1.85) {Validated /\\Partial / Refuted /\\Inconclusive};
\draw[flow] (sess) -- (s1);
\draw[flow] (s1) -- node[edgelbl, above] {Trials} (s2);
\draw[flow] (s2) -- node[edgelbl, above] {Intervention} (s3);
\draw[flow] (s3) -- (out);
```

::: { .notes }
*Pitch:* DoVer is our second deep-dive in attribution. CHIEF builds *structural* evidence; DoVer builds *experimental* evidence — same goal, orthogonal mechanism.

- **Post-hoc, not live.** Original run already failed; DoVer replays edited trials from saved checkpoints.
- Three stages: segment into trials, generate hypothesis + edit, replay and verify.
- Edits are orchestrator-level only — messages or plans, never arbitrary tool code.
- Validation rule: success in **≥2 of 3 replays** = Validated.
:::

## DoVer — Stage 1: trial segmentation

::: {.slide-subtitle}
Cut the session so each re-plan gets its own attribution unit.
:::

- **Cut at re-plan steps** — a planning/re-planning event marks a new trial boundary.
- **One trial = one plan** — the contiguous span from a planning step through everything executed under that plan, until the next re-plan.
- **Prompt-based, not framework-specific** — generalises to systems where re-plan markers aren't explicit.
- **Output:** trial segments, each treated as its own attribution candidate.

::: { .notes }
*Pitch:* A failed session may contain several independent re-planning attempts. Treating each as one global failure step is ill-posed.

- Formally `{τ_i}`. Without segmentation, a bad first plan contaminates attribution for later good attempts.
- Generalises across orchestrators because cuts are detected from the log, not framework hooks.
:::

## DoVer — Stage 2: hypothesise + intervene

::: {.slide-subtitle}
Turn each trial into a testable edit, not a final verdict.
:::

- **Per-trial hypothesis** — log-based attribution names a *suspected* faulty agent, step, and rationale.
- **Treat hypothesis as testable, not authoritative** — correctness is deferred to the replay.
- **Concrete edit at the orchestrator level** — *Modified Instructions to Sub-Agents* or *Plan Updates* (never tool internals).
- **Output:** a targeted intervention at the suspected fault point.

::: { .notes }
*Pitch:* Most attribution methods stop at "I think it was step 7." DoVer asks: *change step 7 — does the failure go away?*

- Orchestrator-level only: clarify sub-agent instructions, fix arguments, supply missing context, change strategy.
- Not editable: tool internals, sub-agent code, low-level execution.
:::

## DoVer — Stage 3: replay + verify

::: {.slide-subtitle}
Replay the edit; label outcomes along success and faithfulness.
:::

- **Replay from the checkpoint** *(setup: same past, edited next step)* — preserve all earlier state, then re-run the edited trial.
- **Validated** *(success: changed; faithfulness: followed edit)* — at least 2 of 3 replays now succeed.
- **Partial / refuted** *(success: partial or unchanged; faithfulness: followed edit)* — milestone progress improves, or the failure persists.
- **Inconclusive** *(faithfulness: edit not carried out)* — replay cannot test the hypothesis.
- **Output:** one validation label per trial.

::: { .notes }
*Pitch:* Two axes hide behind the four labels — did the outcome change, and did the agent actually do what we asked?

- **Truth = task success in replay.** No labelled "correct trajectory" needed; benchmark's own success criterion is the oracle. (GAIA adds optional milestone progress via human-annotated steps.)
- Validation rule: ≥2 of 3 replays succeed for "Validated".
- Headline: **18%** / **28%** failures flipped on AssistantBench / GAIA-derived M1; **49%** flip rate on GSMPlus / AG2.
- Required infra: checkpointing per step, state restore, message editing, replay-from-step.
:::

# Trajectory Monitoring & Analysis Tools

- **What it is:** The evidence and control layer that decides what a developer can see, replay, compare, and intervene on.
- **Why it matters:** Attribution and repair can only use what the trace preserves; output-only logs force diagnosis to guess.
- **What it enables next:** Capture exactly the trace fields that the taxonomy and attribution objectives demand.

::: { .notes }
This is where the survey order would have started. We are starting *here*, after taxonomy + attribution, on purpose: monitoring tooling should follow the diagnostic objective. If you don't know what failure you are hunting, you can't decide what to log. Telemetry is a *consequence* of taxonomy + attribution, not a precondition.
:::

## What observability must preserve

```{.tikz .diagram-full}
\tikzset{cell/.style={draw, rounded corners, minimum width=4.25cm, minimum height=1.20cm, inner xsep=7pt, inner ysep=5pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[cell, fill=blue!8] at (0,1.45) {trace logging\\model, prompt, tool records};
\node[cell, fill=green!10] at (5.0,1.45) {interactive debugging\\reset, edit, fork, compare};
\node[cell, fill=orange!12] at (0,0) {system monitoring\\metrics, anomaly, side effects};
\node[cell, fill=red!8] at (5.0,0) {AgentOps governance\\RCA, resolution, policy};
\draw[->, very thick] (-2.7,-1.15) -- (7.7,-1.15) node[right, font=\sffamily\scriptsize] {more active control};
\draw[->, very thick] (-3.0,-0.70) -- (-3.0,2.35) node[above, font=\sffamily\scriptsize] {more local trace detail};
\node[font=\sffamily\scriptsize] at (0,2.35) {passive capture};
\node[font=\sffamily\scriptsize] at (5.0,2.35) {active intervention};
\node[font=\sffamily\scriptsize, rotate=90] at (-3.45,1.45) {trace};
\node[font=\sffamily\scriptsize, rotate=90] at (-3.45,0) {operation};
```

::: { .notes }
The 2x2 carves observability into two axes: trace vs operation, passive vs active. Trace logging = capture model/prompt/tool records. Interactive debugging = reset/edit/fork/compare. System monitoring = metrics/anomaly/side effects. AgentOps governance = RCA, resolution, policy. The diagonal "passive capture → active intervention" is the maturity gradient: most teams sit in the top-left and need to climb toward the bottom-right.
:::

## From passive monitoring to active debugging

::: {.split}
:::: {.comparison-card}
**Passive system-level monitoring**

- captures logs, events, metrics, side effects
- summarizes long traces and surfaces patterns
- flags anomalies and suspicious trajectories
- does **not** change the run while observing
::::

:::: {.comparison-card}
**Active interactive debugging**

- inspects and annotates trajectory steps
- resets, edits, and replays from a checkpoint
- forks runs to test counterfactual edits
- steers behavior with operator interventions
::::
:::

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=2.45cm, minimum height=0.58cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=blue!8] (capture) at (0,0.7) {capture};
\node[box, fill=blue!8] (summ) at (2.85,0.7) {summarize};
\node[box, fill=orange!12] (diagnose) at (5.7,0.7) {diagnose};
\node[box, fill=green!10] (intervene) at (8.55,0.7) {intervene};
\foreach \x/\y in {capture/summ,summ/diagnose,diagnose/intervene} {\draw[->, very thick] (\x) -- (\y);}
\node[font=\sffamily\scriptsize, text=gray!70!black] at (4.28,1.45) {monitoring becomes more valuable as it approaches controlled replay};
```

::: { .notes }
Walk the arrow: capture → summarize → diagnose → intervene. The further right you can go, the more value the tool offers. Logging only captures; structured telemetry summarizes; interactive debuggers diagnose; checkpoint/replay frameworks intervene. The end state is a *controlled experiment* on a fixed trace.
:::

## Highlight papers

::: {.slide-subtitle}
Evidence capture and control.
:::

::: {.split}
:::: {.case}
**32 AgentSight: system effects**

Correlates LLM intent with kernel-level subprocess, file, and network events.
::::

:::: {.case}
**36 AGDebugger: control primitives**

Exposes pause, checkpoint, reset, edit, fork, and compare across the trajectory.
::::
:::

::: { .notes }
Two exemplars from opposite ends of the maturity gradient. AgentSight = passive capture, but goes *below* the model log into kernel/process/file/network events. AGDebugger = active intervention with checkpoint/edit/fork/replay primitives. Together they show "see more" and "do more" as the two distinct extensions to baseline logging.
:::

## 32 AgentSight — system effects

- **Claim:** Expand observability below the model log by correlating LLM intent signals with subprocesses, files, network, and kernel-level effects.
- **Trace lens:** The trace has two streams — high-level intent and low-level system actions — joined by lineage, timing, and argument matching.
- **Developer takeaway:** Monitor what the agent actually did to the system, not only what it said it intended to do.

```{.tikz .diagram-full}
\tikzset{stream/.style={draw, rounded corners, minimum width=2.75cm, minimum height=0.58cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\small}, eff/.style={draw, rounded corners, minimum width=1.75cm, minimum height=0.48cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize}, every node/.style={font=\sffamily\small}, >=stealth}
\node[stream, fill=blue!8] (intent) at (0,1.2) {LLM intent};
\node[stream, fill=orange!12] (sys) at (0,0) {system actions};
\node[stream, fill=green!10] (corr) at (3.25,0.6) {lineage matching};
\node[eff, fill=gray!10] (proc) at (6.25,1.25) {process};
\node[eff, fill=gray!10] (file) at (6.25,0.6) {file};
\node[eff, fill=gray!10] (net) at (6.25,-0.05) {network};
\node[stream, fill=red!8] (risk) at (8.8,0.6) {semantic analysis};
\draw[->, thick] (intent) -- (corr);
\draw[->, thick] (sys) -- (corr);
\foreach \x in {proc,file,net} {\draw[->, thick] (corr) -- (\x); \draw[->, thick] (\x) -- (risk);}
\node[font=\sffamily\scriptsize, text=gray!70!black] at (4.4,1.95) {observability links model intent to real side effects};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** AgentSight does **boundary tracing** at stable interfaces, not agent-framework internals. The **Intent Stream** uses uprobes on userspace SSL functions (`SSL_read` / `SSL_write`) to grab decrypted LLM traffic on the host. The **Action Stream** uses kernel/process monitoring (process creation, syscalls, file/network/process events, descendant processes). Correlation = process lineage + temporal proximity + argument matching. An **observer LLM** does semantic security/operations analysis over correlated traces. Case studies: prompt-injection exfiltration, reasoning/tool loop, six-subagent coordination bottleneck.
- **Q: How does it see LLM intent without instrumenting the framework?** Uprobes on `SSL_read`/`SSL_write` capture decrypted LLM traffic on the host, regardless of which agent framework is running.
- **Q: How does it connect a model response to system actions?** Process lineage, time windows, and argument matching link intent and action streams across child processes, files, network events, and commands.
- **Q: What does the observer LLM do?** Acts as an analyst over the correlated trace, judging whether observed behavior matches the stated task or indicates risk/waste/coordination problems. Raw + correlated events stay auditable because the observer can be wrong.
- **Q: Main implementation caveat?** TLS interception depends on local crypto stacks and host visibility; remote execution layers, sandboxing, alternate TLS libraries, or provider-side encryption can reduce coverage.
:::

## 36 AGDebugger — control primitives

- **Claim:** Make trajectory analysis interactive by exposing pause, checkpoint, reset, edit, fork, and comparison operations over multi-agent sessions.
- **Trace lens:** The trace is a rewindable state machine: message history plus checkpoints that can be modified and replayed.
- **Developer takeaway:** Debugging tools should make counterfactual inspection cheap; a developer should test whether changing one message changes downstream behavior.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  stage/.style={draw, rounded corners, minimum width=3.6cm, minimum height=1.15cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt},
  io/.style={draw, rounded corners, dashed, minimum width=2.3cm, minimum height=0.85cm, inner xsep=4pt, inner ysep=3pt, align=center, line width=0.5pt, text=black!75},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60, align=center}
}
\node[io] (run) at (0,1.85) {Running\\Multi-Agent System};
\node[stage, fill=blue!8] (s1) at (0,0)
  {1. Inspect + Steer\\[1pt]\scriptsize live history,\\\scriptsize pause / play / step / send};
\node[stage, fill=orange!10] (s2) at (5.6,0)
  {2. Reset + Edit\\[1pt]\scriptsize per-step checkpoints,\\\scriptsize edit message, fork session};
\node[stage, fill=red!8] (s3) at (11.2,0)
  {3. Overview Compare\\[1pt]\scriptsize branched timeline,\\\scriptsize original vs counterfactual};
\node[io] (out) at (11.2,-1.85) {Branched Sessions\\for Comparison};
\draw[flow] (run) -- (s1);
\draw[flow] (s1) -- node[edgelbl, above] {Inspectable\\history} (s2);
\draw[flow] (s2) -- node[edgelbl, above] {Forked\\session} (s3);
\draw[flow] (s3) -- (out);
```

::: { .notes }
*Pitch:* AGDebugger is our worked deep-dive for monitoring — the human-in-the-loop counterpart to CHIEF / DoVer. Three primitives: see, edit, compare.

- Three stages: inspect & steer, reset & edit, overview compare.
- Built on AutoGen; agents implement `save_state` / `load_state`.
- Distinct from DoVer: AGDebugger is *interactive*; DoVer is *automated*. Both ride the same checkpoint primitives.
:::

## AGDebugger — Stage 1: inspect + steer

::: {.slide-subtitle}
Expose the live message stream so the operator can steer before failure hardens.
:::

- **Live message viewer** — agent-to-agent traffic visible as it happens.
- **Pause / play / step** — drive the message queue at any granularity.
- **Send new messages** mid-run — broadcast to all agents, or targeted to one.
- **Output:** an inspectable, controllable message stream.

::: { .notes }
*Pitch:* The first move of debugging is *seeing*. AGDebugger surfaces the inter-agent traffic that frameworks normally swallow.

- Targets: broadcast or single-agent; injected from the debugger UI.
- Granularity: pause-anywhere is the audience's first "ah, I have a debugger" moment.
:::

## AGDebugger — Stage 2: reset + edit

::: {.slide-subtitle}
Restore state before testing a counterfactual edit.
:::

- **Checkpoint per message** — agent state saved *before* each new message via `save_state`.
- **Edit historical messages inline**, then reset to that timestamp — restores the corresponding checkpoint via `load_state`.
- **Fork the session** — the original branch is preserved; the edited path runs as a new session.
- **Output:** a forked session with an edit candidate, replayable from the fork point.

::: { .notes }
*Pitch:* The reset isn't just "trim the transcript" — it restores agent-internal state. WebSurfer goes back to the page it was on.

- Caveat: external side effects (real API calls, file writes) may not roll back cleanly — "good enough" checkpoint policy.
- Common edits observed: add specificity, simplify instructions, modify plan.
:::

## AGDebugger — Stage 3: overview compare

::: {.slide-subtitle}
Compare branches so edits become visible regression tests.
:::

- **Vertical timeline** — every message a rectangle, every fork a new column.
- **Forks marked** with a horizontal dash; pre-fork shared history is shown at lower opacity.
- **Color toggle** — encode message type, sender, or recipient depending on what you're hunting.
- **Output:** a branched comparison view — original run vs each counterfactual run, aligned by step.

::: { .notes }
*Pitch:* Without a comparison view, every fix is "trust me." The overview turns each edit into a visible regression test.

- User study: 14 participants total (6 + 8 across two parts); message resetting rated **4.9/5**, top feature.
- 24 message edits in 30-min sessions; 5/6 preferred AGDebugger over baseline.
- Required infra: AutoGen runtime, typed messages, per-agent `save_state` / `load_state`.
:::

# System Enhancement & Optimization

- **What it is:** Trace-guided editing of the agent system so future runs become more capable, reliable, efficient, or robust.
- **Why it matters:** Diagnosis only earns its keep when it leads to a tested system change rather than another postmortem label.
- **What it enables next:** Choose the right control surface to edit: policy, selection, context update, input formation, workflow, or supervisor.

::: { .notes }
This is the only step in the chain that actually changes the *future* behavior of the system. Everything before it — taxonomy, attribution, monitoring — is diagnostic. Enhancement is therapeutic. Tie it back to the formal model: every repair edits one of the four named control surfaces ($g$, $\phi$, $\pi_a$, $u$) or adds a new one (verifier, supervisor, workflow node).
:::

## What can be optimized

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=3.15cm, minimum height=0.64cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=red!8] (e1) at (0,2.4) {repeated wrong tool choice};
\node[box, fill=red!8] (e2) at (0,1.2) {context drift or stale memory};
\node[box, fill=red!8] (e3) at (0,0) {bad task framing or prompt assembly};
\node[box, fill=red!8] (e4) at (0,-1.2) {unsafe path or missing verification};
\node[box, fill=blue!8] (s1) at (4.5,2.4) {selection $g(\cdot)$};
\node[box, fill=blue!8] (s2) at (4.5,1.2) {context update $u(\cdot)$};
\node[box, fill=blue!8] (s3) at (4.5,0) {input formation $\phi(\cdot)$};
\node[box, fill=blue!8] (s4) at (4.5,-1.2) {policy or supervisor $\pi_a$};
\node[box, fill=green!10, minimum width=3.55cm] (r1) at (9.0,2.4) {route to better agent or tool};
\node[box, fill=green!10, minimum width=3.55cm] (r2) at (9.0,1.2) {retain or refresh state};
\node[box, fill=green!10, minimum width=3.55cm] (r3) at (9.0,0) {change prompt construction};
\node[box, fill=green!10, minimum width=3.55cm] (r4) at (9.0,-1.2) {insert validator or intervention};
\foreach \a/\b in {e1/s1,e2/s2,e3/s3,e4/s4} {\draw[->, thick] (\a) -- (\b);}
\foreach \a/\b in {s1/r1,s2/r2,s3/r3,s4/r4} {\draw[->, thick] (\a) -- (\b);}
\node[font=\sffamily\scriptsize, align=center] at (0,3.15) {trace evidence};
\node[font=\sffamily\scriptsize, align=center] at (4.5,3.15) {editable component};
\node[font=\sffamily\scriptsize, align=center] at (9.0,3.15) {system change};
```

::: {.small}
The formal target is not "make the prompt better." It is choosing the system component whose change should improve the measured objective.
:::

::: { .notes }
Read this as three columns: trace evidence (left) → editable component (middle, mapped to the formal control surfaces from the motivation slide) → system change (right). The arrows give the audience a concrete recipe: when you see *this* failure pattern in your trace, edit *this* component, by making *this* change. The point of the slide is that "tune the prompt" is only one of many possible edits, and rarely the right one.
:::

## Three places a trace can change the system

::: {.slide-subtitle}
Three enhancement families the survey identifies, by where the edit lands.
:::

```{.tikz .diagram-full}
\tikzset{
  perspective/.style={draw, rounded corners, minimum width=3.50cm, minimum height=0.82cm, text width=3.15cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\small\bfseries},
  meaning/.style={draw, rounded corners, minimum width=4.20cm, minimum height=0.90cm, text width=3.85cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  example/.style={draw, rounded corners, minimum width=4.00cm, minimum height=0.90cm, text width=3.65cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (0,2.95) {Name};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (4.85,2.95) {What it is};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (10.00,2.95) {Examples};

\node[perspective, fill=blue!8] (n1) at (0,1.85) {structural and\\workflow};
\node[meaning, fill=blue!4] (w1) at (4.85,1.85) {edit environment, graph,\\topology, system rules};
\node[example, fill=blue!4] (e1) at (10.00,1.85) {Aegis-Song,\\Maestro};
\draw[->, thick] (n1) -- (w1);
\draw[->, thick] (w1) -- (e1);

\node[perspective, fill=green!10] (n2) at (0,0.65) {agent\\internal};
\node[meaning, fill=green!5] (w2) at (4.85,0.65) {edit prompts, policies,\\context, scaffolds};
\node[example, fill=green!5] (e2) at (10.00,0.65) {ReCreate,\\AgentDevel};
\draw[->, thick] (n2) -- (w2);
\draw[->, thick] (w2) -- (e2);

\node[perspective, fill=orange!12] (n3) at (0,-0.55) {runtime and\\supervisory};
\node[meaning, fill=orange!5] (w3) at (4.85,-0.55) {add live supervision\\or reduce runtime\\interaction state};
\node[example, fill=orange!5] (e3) at (10.00,-0.55) {SupervisorAgent\\(supervision);\\AgentDiet\\(compression)};
\draw[->, thick] (n3) -- (w3);
\draw[->, thick] (w3) -- (e3);
```

::: { .notes }
The survey buckets enhancement into three families by *where the edit lands*. Structural/workflow = outside the agent (graph, env, system rules). Agent internal = inside the agent (prompt, policy, scaffold). Runtime/supervisory = on top of execution (live supervisor, observation purification). Help the audience map this back to the formal model: structural ≈ $g$ + workflow, internal ≈ $\pi_a$ + $\phi$, runtime ≈ added supervisor on top.
:::

## Highlight papers

::: {.slide-subtitle}
Where the repair happens.
:::

::: {.mini-grid}
:::: {.case}
**24 Maestro: graph-plus-config repair**

Jointly searches workflow-graph and configuration edits from evaluator feedback.
::::

:::: {.case}
**31 SupervisorAgent: runtime control**

Approves, guides, or corrects at risky interaction boundaries during execution.
::::

:::: {.case}
**07 Aegis-Song: environment optimization**

Fixes failures by editing the environment and tool interface, not the agent prompt.
::::
:::

::: { .notes }
Two exemplars chosen to span the family. Maestro = structural/workflow repair via joint graph + config search. SupervisorAgent = runtime/supervisory repair via a meta-agent. (We don't pick an "agent internal" exemplar here because most prompt-tuning is well known; structural and runtime are the underexplored surfaces.)
:::

## 24 Maestro — graph-plus-config repair

- **Claim:** Frame enhancement as joint optimization over workflow graph structure and configuration, guided by trace feedback.
- **Trace lens:** Failures are evidence about missing computation, routing, validation, state, or tool operations in a typed agent graph.
- **Developer takeaway:** Do not tune prompts when the graph lacks the operation needed to recover; add the missing node, edge, validator, or state variable.

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=2.45cm, minimum height=0.55cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\scriptsize}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=blue!8] (trace) at (0,1.0) {trace feedback};
\node[box, fill=orange!12] (crit) at (2.8,1.0) {evaluator rationale};
\node[box, fill=green!10] (config) at (5.6,1.55) {config search};
\node[box, fill=green!10] (graph) at (5.6,0.45) {graph edit};
\node[box, fill=red!8] (next) at (8.6,1.0) {improved graph};
\foreach \x/\y in {trace/crit,crit/config,crit/graph,config/next,graph/next} {\draw[->, thick] (\x) -- (\y);}
\node[font=\sffamily\scriptsize, align=center] at (5.6,-0.42) {add extractor, validator, router, state, or numeric tool};
\node[font=\sffamily\scriptsize, text=gray!70!black] at (4.3,2.12) {enhancement searches the system design space, not only prompt text};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Maestro models the agent as a **typed computation graph $G = (V, E)$** with stochastic nodes (LLM calls, retrieval, tools, memory, validators, controllers, merge functions). It runs **block-coordinate search** — a **C-step** for configuration (prompts, model, tool sets, decoding params, adapters, retrieval, merge params) and a **G-step** for graph edits (add/remove/rewire nodes/edges, tools, validators, memory/state, routing, module types). Reflective textual feedback from traces and evaluator rationales drives candidate edits. Warm-started configs + guarded acceptance criterion. Concrete reported edits: `extract_entities`, `validate_constraints`, `branches_done`, `numeric_compute`. **Caveat:** the paper's Section 4 notes some technical details are proprietary.
- **Q: What does Maestro mean by "graph"?** A typed computation graph whose nodes can be LLM calls, retrieval, tools, memory, validators, controllers, or merge functions; edges define artifact / context / control flow.
- **Q: What gets optimized in the C-step?** Existing node/edge configurations — prompts, model choice, tool sets, decoding params, adapters, retrieval settings, merge parameters — graph fixed.
- **Q: What graph edits did the paper actually find useful?** Simple structural additions: `extract_entities` for HotpotQA, `validate_constraints` + a rewrite pass for IFBench, an external `branches_done` state for an interviewer agent, a `numeric_compute` tool for financial RAG.
- **Q: How does trace feedback guide graph repair?** Evaluator rationales identify missing operations (skipped branches, violated constraints, missing second-hop retrieval, arithmetic errors); Maestro prioritizes targeted local edits instead of blind topology search.
:::

## 31 SupervisorAgent — runtime control

- **Claim:** Add a lightweight meta-agent that watches high-risk interactions and intervenes while the trajectory is still recoverable.
- **Trace lens:** The supervisor observes agent-agent, agent-tool, and agent-memory events with local and global context summaries.
- **Developer takeaway:** Put supervision at interaction boundaries: approve, guide, correct observations, or run verification before errors become irreversible.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  box/.style={draw, rounded corners, minimum width=2.85cm, minimum height=0.85cm, inner xsep=5pt, inner ysep=4pt, align=center, line width=0.5pt},
  action/.style={draw, rounded corners, minimum width=2.4cm, minimum height=0.65cm, inner xsep=5pt, inner ysep=3pt, align=center, line width=0.5pt},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  loopflow/.style={->, >=stealth, dashed, line width=0.55pt, draw=black!55},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60}
}
\node[box, fill=blue!8] (base) at (0,0) {base interaction};
\node[box, fill=orange!10] (trigger) at (3.6,0) {risk trigger};
\node[box, fill=green!10] (sup) at (7.2,0) {supervisor decision};
\node[action, fill=gray!10] (approve) at (11.4,1.45) {approve};
\node[action, fill=yellow!15] (guide) at (11.4,0) {guidance};
\node[action, fill=red!8] (corr) at (11.4,-1.45) {correct / verify};
\draw[flow] (base) -- (trigger);
\draw[flow] (trigger) -- (sup);
\draw[flow] (sup.east) -- (approve.west);
\draw[flow] (sup.east) -- (guide.west);
\draw[flow] (sup.east) -- (corr.west);
\draw[loopflow] (corr.south) -- ++(0,-0.65) -| node[edgelbl, pos=0.25, above] {resume with corrected context} (base.south);
\draw[loopflow] (guide.south) -- ++(0,-0.40) -| (base.south);
\node[font=\sffamily\scriptsize, text=black!70, align=center, text width=11cm] at (5.7,2.30) {runtime enhancement inserts control while recovery paths still exist};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Supervision targets agent–agent, agent–tool, and agent–memory interactions. A **cheap LLM-free adaptive filter** triggers only on risky events: explicit errors, inefficient/repetitive behavior, excessive observation length, sub-agent completion reports. The supervisor sees global task, local task, recent local trace, latest interaction summary, sometimes the global trace. Action set: `approve`, `provide_guidance`, `correct_observation`, `run_verification`. Strategies: proactive error correction, inefficiency guidance, adaptive observation purification. Reported tradeoff: substantial token reductions while preserving/improving accuracy on several benchmarks; latency increases because supervision adds work; observation purification is lossy.
- **Q: When does the supervisor actually run?** A cheap LLM-free filter triggers on explicit errors, inefficient/repetitive behavior, very long observations, or sub-agent completion reports. Avoids paying for supervision every step.
- **Q: What can the supervisor do?** Approve, provide guidance, correct/purify an observation, or run a verification sub-agent — assist or constrain the base MAS without changing its architecture.
- **Q: What context does the supervisor see?** Global task, local task, recent local trace, latest interaction summary, sometimes global trace. Focused, but enough to intervene.
- **Q: What's the tradeoff?** Substantial token reductions with preserved/improved accuracy on several benchmarks, but added latency and lossy observation purification.
:::

## 07 Aegis-Song — environment optimization

- **Claim:** Repair agent failures by **editing the environment**, not the prompt: enhance observability, offload deterministic computation, and speculate bundled actions.
- **Trace lens:** When traces show exploration/exploitation/resource-exhaustion failures, the implicated control surface is the environment + tool interface, not $\pi_a$.
- **Developer takeaway:** Before tuning prompts, ask: would the agent succeed if the environment exposed more state, did the deterministic work itself, or accepted batched actions?

```{.tikz .diagram-full}
\tikzset{
  origin/.style={draw, rounded corners, minimum width=3.10cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\small\bfseries},
  fix/.style={draw, rounded corners, minimum width=4.20cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  outcome/.style={draw, rounded corners, minimum width=3.40cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (0,2.85) {Trace symptom};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (4.85,2.85) {Environment-side fix};
\node[font=\sffamily\small\bfseries, text=gray!70!black] at (10.20,2.85) {Effect};

\node[origin, fill=blue!8] (s1) at (0,1.9) {missing state};
\node[fix, fill=blue!4] (f1) at (4.85,1.9) {expose richer observations};
\node[outcome, fill=blue!4] (o1) at (10.20,1.9) {fewer exploration faults};
\draw[->, thick] (s1) -- (f1);
\draw[->, thick] (f1) -- (o1);

\node[origin, fill=green!10] (s2) at (0,0.95) {wasted reasoning};
\node[fix, fill=green!5] (f2) at (4.85,0.95) {offload deterministic work};
\node[outcome, fill=green!5] (o2) at (10.20,0.95) {fewer exploitation faults};
\draw[->, thick] (s2) -- (f2);
\draw[->, thick] (f2) -- (o2);

\node[origin, fill=orange!12] (s3) at (0,0) {budget exhausted};
\node[fix, fill=orange!5] (f3) at (4.85,0) {speculate bundled actions};
\node[outcome, fill=orange!5] (o3) at (10.20,0) {fewer resource faults};
\draw[->, thick] (s3) -- (f3);
\draw[->, thick] (f3) -- (o3);
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Aegis-Song proposes a small set of **environment-side optimizations** validated against its own taxonomy: (1) **observability enhancement** (expose missing state in tool returns), (2) **common computation offloading** (move deterministic work out of the LLM), and (3) **speculative agentic actions** (let the environment accept batched / pre-validated actions).
- **Q: Why isn't this just "better tool design"?** It is — but the paper's contribution is the **mapping** from each environment-side fix to a specific failure category in its own taxonomy, so you can decide *which* environment-side change to invest in based on the trace.
- **Q: How does this compare to Maestro?** Maestro searches workflow-graph + configuration edits inside the agent system. Aegis-Song edits the environment / tool interface around the agent. They target different control surfaces and can be combined.
- **Q: When should I reach for this?** When repeated trace failures fall into exploration / exploitation / resource buckets, especially in environments with finite budgets (web, embodied, tool-rich). Tuning $\pi_a$ rarely fixes those; environment edits often do.
- **Q: Same paper as 21 Aegis-Kong?** No, two different papers; this one (07 Aegis-Song) is the taxonomy + environment-optimization paper. Aegis-Kong is the synthetic-error-injection attribution paper.
:::

# Datasets & Benchmarks

- **What they are:** The field's measurement and training substrate: what counts as progress, and what diagnostic models learn from.
- **Why they matter:** Benchmark labels become incentives; if they reward only agent/step accuracy, repair utility is invisible.
- **What they enable next:** Compare attribution and enhancement methods, and run regression suites on real or injected failures.

::: { .notes }
Last layer in the chain. The point is not to celebrate leaderboards but to ask: *what is being measured, and does that measurement push the field toward useful repairs?* Goodhart's law applies — whatever the benchmark scores becomes the optimization target. So if benchmarks only reward agent/step accuracy, methods will be optimized for diagnosis, not for repair utility.
:::

## What benchmark design optimizes for

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  box/.style={draw, rounded corners, minimum width=3.0cm, minimum height=1.05cm, inner xsep=5pt, inner ysep=4pt, align=center, line width=0.5pt},
  gap/.style={draw, rounded corners, dashed, minimum width=2.7cm, minimum height=1.05cm, inner xsep=5pt, inner ysep=4pt, align=center, line width=0.6pt},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  edgelbl/.style={font=\sffamily\scriptsize, fill=white, inner sep=2pt, text=black!60}
}
\node[box, fill=blue!8] (measure) at (0,0) {common labels\\agent + step accuracy};
\node[gap, fill=orange!10] (gap) at (5.5,0) {repair-utility gap};
\node[box, fill=green!10] (need) at (11.0,0) {developer need\\safe, durable repair};
\draw[flow] (measure) -- node[edgelbl, above, inner sep=1pt] {diagnosis may stop here} (gap);
\draw[flow] (gap) -- node[edgelbl, above, inner sep=1pt] {must prove utility} (need);
\node[font=\sffamily\scriptsize, align=center, text width=10cm, text=black!70] at (5.5,-1.30) {A useful benchmark tests whether attribution improves system changes, not only whether labels match.};
```

::: {.source-note}
**Open critique:** Agent-level and step-level accuracy are useful, but they do not fully measure whether attribution helps produce a safe, durable repair. The survey reports limited step-level attribution on established comparisons and points to benchmark diversity and observability as bottlenecks.
:::

::: { .notes }
Diagram is one sentence: today's benchmarks measure label match, but what developers need is *safe, durable repair*. The dashed box in the middle is the gap. The survey itself flags this gap. Use it as the bridge to the dataset-construction question on the next slide.
:::

## Two ways to build trajectory evaluation data

::: {.split}
:::: {.comparison-card}
**Real-world failure collections**

- preserve naturally occurring failures from real or realistic systems
- reflect messy production conditions and repair needs
- expensive to collect and annotate at scale
- biased toward what was actually deployed
::::

:::: {.comparison-card}
**Synthetic error-injection datasets**

- start from successful trajectories and inject controlled faults
- scale to thousands of labelled examples cheaply
- support training data-hungry attribution models
- must be checked for realism vs production failures
::::
:::

::: { .notes }
Two paths: real-world failure collection (top row) is realistic but expensive and biased toward what was actually deployed. Synthetic error injection (bottom row) is cheap and scalable but may not match production failure distributions. The choice is not "which is better" but "which mismatch can my use case tolerate". Mention that fine-tuned tracer methods especially depend on the synthetic path.
:::

## Highlight papers

::: {.slide-subtitle}
Evaluation roles.
:::

::: {.mini-grid}
:::: {.case}
**13 Who&When**

Canonical agent + decisive-step labels for benchmarkable attribution.
::::

:::: {.case}
**23 TraceElephant**

Full traces and reproducible environments for replay-ready evaluation.
::::

:::: {.case}
**04 AgentRx**

Repair-utility labels: critical step, category, rationale, evidence.
::::
:::

::: { .notes }
These three benchmarks span the maturity gradient. Who&When = the canonical attribution target (who failed, when). TraceElephant = full-observability trace contract (everything in the run). MAESTRO = framework-agnostic operational telemetry (cost, retries, silent semantic failures). Each one widens what gets measured.
:::

## 13 Who&When — benchmark view

- **Claim:** As a benchmark, operationalize attribution as two measurable labels: culprit agent and decisive error step.
- **Trace lens:** The dataset turns trace reading into comparable evaluation across alternative LLM-judge strategies.
- **Developer takeaway:** Use this benchmark lens to test whether your traces expose enough indexing and context for reproducible blame assignment.

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=2.45cm, minimum height=0.55cm, inner xsep=7pt, inner ysep=4pt, align=center, font=\sffamily\scriptsize}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=blue!8] (logs) at (0,0.9) {indexed logs};
\node[box, fill=orange!12] (labels) at (2.85,0.9) {agent-step labels};
\node[box, fill=green!10] (methods) at (5.7,0.9) {judge methods};
\node[box, fill=red!8] (score) at (8.55,0.9) {accuracy comparison};
\foreach \x/\y in {logs/labels,labels/methods,methods/score} {\draw[->, very thick] (\x) -- (\y);}
\node[box, fill=gray!10] at (2.85,-0.35) {who};
\node[box, fill=gray!10] at (5.7,-0.35) {when};
\draw[->, thick] (labels.south) -- (2.85,-0.08);
\draw[->, thick] (labels.south) -- (5.7,-0.08);
\node[font=\sffamily\scriptsize, text=gray!70!black] at (4.3,1.58) {benchmark contribution = make attribution labels comparable};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** A benchmark item bundles **query, full failure log, system info (when available), responsible agent, decisive step, NL failure explanation**. Coverage: failed logs from CaptainAgent algorithm-generated systems and hand-crafted Magnetic-One on GAIA + AssistantBench. Hand-crafted logs are much longer, making **exact step localization much harder**. Metrics: agent accuracy, exact step accuracy, tolerance-window step accuracy.
- **Q: What's inside a Who&When item?** Query, full failure log, system info if available, responsible agent, decisive step, NL failure explanation.
- **Q: What systems does the benchmark cover?** CaptainAgent algorithm-generated systems + hand-crafted Magnetic-One on GAIA / AssistantBench. Hand-crafted = longer logs = harder exact-step attribution.
- **Q: How is performance measured?** Responsible-agent accuracy, exact decisive-step accuracy, tolerance-window step accuracy (near-miss within a window). The window matters because exact-step is often very low even when the method is in the right region.
- **Q: Main lesson?** Agent-level attribution is feasible but unsolved; exact step attribution remains much harder, especially in long traces. Stronger reasoning models alone do not make it reliable.
:::

## 23 TraceElephant — benchmark view

- **Claim:** Test attribution under full observability, including complete traces and reproducible environments.
- **Trace lens:** The benchmark contrasts output-only attribution with full-trace static and dynamic replay/counterfactual probing.
- **Developer takeaway:** Evaluate attribution under the observability conditions developers actually have; partial traces can understate achievable diagnosis.

```{.tikz .diagram-full}
\renewcommand{\familydefault}{\sfdefault}
\normalfont
\tikzset{
  every node/.style={font=\sffamily\footnotesize, inner sep=0pt},
  box/.style={draw, rounded corners, minimum width=2.85cm, minimum height=0.85cm, inner xsep=5pt, inner ysep=4pt, align=center, line width=0.5pt},
  collabel/.style={font=\sffamily\scriptsize, text=black!55},
  flow/.style={->, >=stealth, thick, line width=0.55pt},
  weakflow/.style={->, >=stealth, dashed, line width=0.55pt, draw=black!55}
}
\node[collabel] at (0,2.05) {trace inputs};
\node[collabel] at (4.0,2.05) {attribution method};
\node[collabel] at (8.0,2.05) {evaluation};
\node[box, fill=gray!10] (out) at (0,1.05) {output-only\\view};
\node[box, fill=blue!8] (full) at (0,-1.05) {full-trace\\view};
\node[box, fill=orange!10] (static) at (4.0,1.05) {static\\attribution};
\node[box, fill=green!10] (dyn) at (4.0,-1.05) {dynamic\\replay};
\node[box, fill=red!8] (cmp) at (8.0,0) {benchmark\\comparison};
\draw[weakflow] (out) -- (static);
\draw[flow] (full) -- (static);
\draw[flow] (full) -- (dyn);
\draw[flow] (static) -- (cmp);
\draw[flow] (dyn) -- (cmp);
\node[font=\sffamily\scriptsize, text=black!70, align=center, text width=10cm] at (4.0,-2.30) {observability level is a benchmark variable, not a footnote — full traces unlock dynamic replay};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Formalizes the **earliest inevitable failure step** + responsible agent at that step. Trace collection via **LLM API middleware** without modifying agent implementations. Schema fields: task metadata, agent configuration, architecture, step ID, agent ID/name, input context, output content, tool logs. Dataset: **380 traces (220 failed)** across CaptainAgent, Magentic-One, SWE-Agent. Compares full-trace, output-only, metadata/input ablations, static methods, and dynamic replay. Finding: removing inputs/metadata sharply degrades step attribution; dynamic replay improves but exact step accuracy stays around one-third.
- **Q: What does "full trace" mean here?** Task metadata, agent config, architecture, acting agent, step ID, full input context, output content, tool logs (name, args, output, status) — paired with runnable code/configuration for replay.
- **Q: How were traces captured?** LLM API middleware + lightweight preprocessing intercepts requests/responses/tool interactions while preserving original implementations — closer to real developer debugging.
- **Q: What does dynamic attribution add?** Proposes candidate (agent, step, reason) triples, replays from the candidate point through middleware, intervenes, and checks whether a local expected oracle holds — filters spurious static diagnoses.
- **Q: Practical lesson?** Output-only logs understate what's knowable. Removing inputs/metadata hurts attribution sharply (especially step localization); replay helps but doesn't solve it.
:::

## 04 AgentRx — benchmark view

- **Claim:** As a benchmark, AgentRx packages **115 failed trajectories** with three repair-relevant labels: critical step, root-cause category, and supporting evidence.
- **Trace lens:** Each failed run is annotated by category, decisive step, and a step-indexed validation log of violated constraints.
- **Developer takeaway:** Use this benchmark when the question is "does my attribution method enable a *repair*?" — not just "does it match the agent/step label?"

```{.tikz .diagram-full}
\tikzset{
  src/.style={draw, rounded corners, minimum width=2.95cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  pipeline/.style={draw, rounded corners, minimum width=2.95cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  label/.style={draw, rounded corners, minimum width=2.95cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=3pt, align=center, font=\sffamily\scriptsize},
  every node/.style={font=\sffamily\small},
  >=stealth
}
\node[src, fill=blue!8]   (s1) at (0,1.6) {tau-bench failures};
\node[src, fill=blue!8]   (s2) at (0,0.8) {Flash failures};
\node[src, fill=blue!8]   (s3) at (0,0)   {Magentic-One failures};
\node[pipeline, fill=orange!12] (pipe) at (4.20,0.8) {AgentRx pipeline};
\node[label, fill=green!10] (l1) at (8.20,1.6) {critical step};
\node[label, fill=green!10] (l2) at (8.20,0.8) {root-cause category};
\node[label, fill=green!10] (l3) at (8.20,0)   {supporting evidence};
\foreach \s in {s1,s2,s3} {\draw[->, thick] (\s) -- (pipe);}
\foreach \l in {l1,l2,l3} {\draw[->, thick] (pipe) -- (\l);}
\node[font=\sffamily\scriptsize, text=gray!70!black] at (4.20,2.20) {115 failed trajectories with three repair-relevant labels};
```

::: { .notes }
**Audience supplement (not on slide):**

- **Behind the slide:** Same paper as the taxonomy view earlier in the deck, but reframed for the **benchmark area**. AgentRx releases 115 failed trajectories drawn from τ-bench, Flash, and Magentic-One, each annotated with: critical (first unrecoverable) step, one of 9 root-cause categories, and a step-indexed validation log of violated constraints + supporting evidence.
- **Q: Why is this a *benchmark* contribution and not just a method?** Because it is the cleanest published artifact that pairs decisive-step labels with **repair-relevant evidence** (constraint violations + categories). Most attribution benchmarks stop at agent/step accuracy; this one supplies what a developer would need to act on.
- **Q: How is this different from Who&When and TraceElephant?** Who&When is the canonical agent/step-label benchmark. TraceElephant adds full-observability traces and replay environments. AgentRx adds **constraint-grounded labels and rationales** — a third axis: not just *who failed when*, but *what rule was violated and what is the evidence*.
- **Q: What does it not give you?** End-to-end repair success measurements. The benchmark labels are inputs to repair, not measurements of repair utility — that's still an open community gap (see the limitations slide).
- **Q: Why drop MAESTRO from the highlights?** MAESTRO is a great operational-telemetry suite, but the benchmark *thesis of this deck* is "labels should support repair." MAESTRO measures runtime behavior across architectures rather than diagnostic-to-repair utility, so it stays in the appendix.
:::

# Takeaways

- **One closed loop:** collect → classify → attribute → repair → evaluate → operate.
- **Still research-stage:** SOTA exact step-level attribution ≈ **30%** (CHIEF on Who&When); full-observability ceiling ≈ **33%** (TraceElephant).
- **For builders:** no turnkey tool yet — pick the right axis, expect partial coverage, build the loop incrementally.

::: { .notes }
*Pitch:* The survey is one closed loop, and we're early. ~30% exact step accuracy on Who&When hand-crafted; ~33% even with full traces. Honest baseline.

- Sources: CHIEF Table 1 (29.31% hand-crafted, 52.00% algorithm-generated); TraceElephant (33.3% with full observability).
- Frame as motivation, not pessimism — the field is open because the easy gains haven't been collected.
:::

## Closed-loop agent improvement

```{.tikz .diagram-full}
\tikzset{box/.style={draw, rounded corners, minimum width=2.30cm, minimum height=0.66cm, inner xsep=6pt, inner ysep=4pt, align=center, font=\sffamily\small}, every node/.style={font=\sffamily\small}, >=stealth}
\node[box, fill=blue!8] (collect) at (0,1.8) {collect\\traces};
\node[box, fill=green!10] (classify) at (3.0,1.8) {classify\\failure};
\node[box, fill=orange!12] (attrib) at (6.0,1.8) {attribute\\$t^\ast$};
\node[box, fill=red!8] (repair) at (9.0,1.8) {repair\\$M \to M'$};
\node[box, fill=yellow!15] (eval) at (6.0,0) {benchmark\\repair utility};
\node[box, fill=purple!10] (ops) at (3.0,0) {operate with\\AgentOps};
\draw[->, very thick] (collect) -- (classify);
\draw[->, very thick] (classify) -- (attrib);
\draw[->, very thick] (attrib) -- (repair);
\draw[->, very thick] (repair) -- (eval);
\draw[->, very thick] (eval) -- (ops);
\draw[->, very thick] (ops) -- (collect);
\node[font=\sffamily\scriptsize, align=center, text width=3.0cm] at (0,-0.10) {gap: observability\\richness};
\node[font=\sffamily\scriptsize, align=center, text width=3.0cm] at (9.0,-0.10) {gap: repair-centered\\evaluation};
\node[font=\sffamily\scriptsize, align=center, text width=3.0cm] at (4.5,-1.0) {gap: fragmented\\AgentOps};
```

::: {.developer-intuition}
Engineering synthesis over the survey: collect enough evidence, constrain diagnosis, approximate the causal boundary, edit the right component, and evaluate whether the edit improves future systems.
:::

::: { .notes }
Synthesis time. Trace a full lap with the audience: collect → classify → attribute → repair → benchmark → operate → loop back. Each box is a survey area we covered. Then point at the three "gap" labels — observability richness on the left (we don't capture enough), repair-centered evaluation on the right (we don't measure utility), fragmented AgentOps below (the loop isn't closed in production tooling). These are the three open problems for the field, not solved problems.
:::

## Current limitations and open problems

- **Attribution accuracy is not enough** — agent-level and step-level metrics do not necessarily measure repair utility.
- **Benchmark observability can be mismatched** — partial traces understate what developers actually see in internal debugging.
- **Tools remain fragmented** — logging, visualization, replay, anomaly detection, RCA, and repair live in separate systems.
- **Domain structure is underused** — coding, GUI, web, and embodied agents expose different trace shapes and failure modes.

::: { .notes }
*Pitch:* Four research-stage gaps. Each is an opportunity, not a verdict.

- The community has first-cut answers everywhere, but none of these four are solved.
- Use these gaps as filters for picking which papers to follow next.
:::

## Closing thesis

::: {.slide-claim}
LLM agent trajectory analysis is becoming the engineering layer that connects evaluation, debugging, system optimization, observability, and operations.
:::

::: {.formal-anchor}
The trajectory is the evidence. Taxonomy gives the diagnostic lens. Attribution identifies the causal boundary. Enhancement edits the system. Monitoring and benchmarks make the loop repeatable.
:::

::: {.builder-implication}
For agent developers, the practical question is no longer "did this run fail?" It is: **what evidence proves why it failed, what component should change, and how do we know the repair generalizes?**
:::

::: { .notes }
End on first principles. Frame this as a closing reframe for the audience: pass/fail evaluation belongs to a previous era of ML. The next decade of agent engineering is about *trajectory-level* questions — evidence, component identity, and repair generalization. The survey's five dimensions are how the community is starting to answer those questions, and the rest of the journey is yours to build.
:::

# Appendix

::: {.compact}
- Each appendix slide summarizes one referenced paper.
- The template is: **background problem**, **fundamental idea**, **developer/evaluation takeaway**, and **survey areas**.
- **Numbering note:** The two-digit prefixes used in the deck (e.g. `01 Lu`, `13 Who&When`) follow our local analysis-note IDs, not the survey's Table 1 IDs.
:::

## 01. Exploring Autonomous Agents

<span class="tag">Failure Taxonomy</span>

- **Background problem:** Success rate hides which agent role failed when planner, code generator, executor, or final answer hand off work.
- **Fundamental idea:** Inspect full run logs from 204 executions, label failures into 19 causes under planning, execution, and response-generation phases.
- **Takeaway:** Store per-iteration prompts, code, outputs, and errors, then route repeated failures to replanning, local repair, or early stop.
- **Paper:** [https://arxiv.org/abs/2508.13143](https://arxiv.org/abs/2508.13143)
- **Code:** [https://github.com/lurf21/AgentEvaluationFramework](https://github.com/lurf21/AgentEvaluationFramework)

## 02. Where LLM Agents Fail

<span class="tag">Failure Taxonomy</span> <span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Early agent mistakes cascade, but prior failure studies label errors without tracing the root cause or enabling fixes.
- **Fundamental idea:** AgentDebug labels each step/module, uses counterfactual tests to find the earliest failure-causing step, then re-rolls out with targeted feedback.
- **Takeaway:** Debug failed agents from the first causal step, not every visible mistake; use AgentErrorBench-style annotations to test localization and recovery.
- **Paper:** [https://arxiv.org/abs/2509.25370](https://arxiv.org/abs/2509.25370)
- **Code:** [https://github.com/ulab-uiuc/AgentDebug](https://github.com/ulab-uiuc/AgentDebug)

## 03. TRAIL

<span class="tag">Failure Taxonomy</span> <span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Agent evals can score final answers, but developers need span-level root causes inside huge structured traces.
- **Fundamental idea:** Annotate OpenTelemetry traces from GAIA/SWE-Bench with error category, span location, evidence, impact, and quality scores.
- **Takeaway:** Use TRAIL to test whether an evaluator debugs real agent runs, not just final answers or synthetic planning cases.
- **Paper:** [https://arxiv.org/abs/2505.08638](https://arxiv.org/abs/2505.08638)
- **Code:** [https://github.com/patronus-ai/trail-benchmark](https://github.com/patronus-ai/trail-benchmark)

## 04. AgentRx

<span class="tag">Failure Taxonomy</span> <span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Terminal success hides the first unrecovered mistake; debugging needs step-level evidence, not just outcome labels.
- **Fundamental idea:** Turn tool schemas, policies, and trajectory prefixes into guarded checks; log violations with evidence for each step.
- **Takeaway:** Instrument agents so judges can trace "what constraint broke when" before deciding the unrecoverable root cause.
- **Paper:** [https://arxiv.org/abs/2602.02475](https://arxiv.org/abs/2602.02475)
- **Code:** [https://github.com/microsoft/AgentRx](https://github.com/microsoft/AgentRx)

## 05. Lifecycle of Failures

<span class="tag">Failure Taxonomy</span> <span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** In platform agent workflows, visible errors often surface far from the causal node after prompts, tools, and control logic interact.
- **Fundamental idea:** AgentFail labels 307 Dify/Coze failures by root location, cause level/category, propagation distance, and repair strategy.
- **Takeaway:** Debug by proving the earliest decisive node, then apply cause-matched fixes; taxonomy plus location made repairs safer.
- **Paper:** [https://arxiv.org/abs/2509.23735](https://arxiv.org/abs/2509.23735)
- **Code:** [https://github.com/Jenna-Ma/JaWs-AgentFail](https://github.com/Jenna-Ma/JaWs-AgentFail)

## 06. MAST

<span class="tag">Failure Taxonomy</span>

- **Background problem:** MAS benchmark failures hide whether the root cause is system design, inter-agent misalignment, or task verification.
- **Fundamental idea:** Derive MAST bottom-up from 150+ failed traces, yielding 14 failure modes across those three axes.
- **Takeaway:** Label failed traces first; then fix workflow design, agent information flow, or verification instead of blindly swapping models.
- **Paper:** [https://arxiv.org/abs/2503.13657](https://arxiv.org/abs/2503.13657)
- **Code:** [https://github.com/multi-agent-systems-failure-taxonomy/MAST](https://github.com/multi-agent-systems-failure-taxonomy/MAST)

## 07. Aegis

<span class="tag">Failure Taxonomy</span> <span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Agents fail differently across DB, filesystem, CRM, and medical environments: missing state, losing state, miscomputing outputs, violating rules, exhausting turns.
- **Fundamental idea:** Treat tools as reliability infrastructure: expose lookahead/state, offload sorting/calculation/rule checks, and speculate common follow-up calls.
- **Takeaway:** Don't only tune the agent; redesign tool responses so correct behavior becomes retrieval, validation, or bundled execution.
- **Paper:** [https://arxiv.org/abs/2508.19504](https://arxiv.org/abs/2508.19504)
- **Code:** Not released

## 08. LLMs in Agentic Scenarios

<span class="tag">Failure Taxonomy</span>

- **Background problem:** Aggregate agent scores hide why tool-using LLMs fail in enterprise-like workflows.
- **Fundamental idea:** Manually code 900 KAMI traces across filesystem, text, CSV, and SQL tasks.
- **Takeaway:** Require grounding, value verification, distractor control, and missing-entity discipline before trusting autonomous tool outputs.
- **Paper:** [https://arxiv.org/abs/2512.07497](https://arxiv.org/abs/2512.07497)
- **Code:** Not found

## 09. FAMAS

<span class="tag">Failure Attribution</span>

- **Background problem:** A failed MAS log rarely reveals whether an agent action caused failure or only appears downstream.
- **Fundamental idea:** Replay the task, cluster logs into agent-action-state triples, then rank triples with λ-decayed Kulczynski2 plus α/β/γ factors.
- **Takeaway:** Use pass/fail replay spectra when failures recur; FAMAS is statistical attribution, not single-log LLM judging.
- **Paper:** [https://arxiv.org/abs/2509.13782](https://arxiv.org/abs/2509.13782)
- **Code:** Not found

## 10. Traceability and Accountability

<span class="tag">Failure Attribution</span> <span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Sequential agent pipelines hide where failures begin; final-output scoring cannot separate planner mistakes from executor or critic harm.
- **Fundamental idea:** Record P/E/C answers, final answer, repair flags, harm flags, and earliest unrepaired error origin.
- **Takeaway:** Design handoffs around computable accountability: each stage should expose whether it repaired, preserved, or damaged the prior state.
- **Paper:** [https://arxiv.org/abs/2510.07614](https://arxiv.org/abs/2510.07614)
- **Code:** Not found

## 11. CORRECT

<span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Multi-agent failures cascade through long logs; developers need the first bad agent-step, not just a failed-run label.
- **Fundamental idea:** Distill past annotated failures into cached schemas: signatures, triggering context, propagation patterns, and detection heuristics.
- **Takeaway:** Use embedding retrieval over schemas, not raw traces or fine-tuning, to guide an LLM's step-level failure attribution.
- **Paper:** [https://arxiv.org/abs/2509.24088](https://arxiv.org/abs/2509.24088)
- **Code:** Not found

## 12. SDBL

<span class="tag">Failure Attribution</span>

- **Background problem:** Exact failure-step attribution in long multi-agent logs overwhelms general LLMs, especially when failures appear late or require MAS-specific expertise.
- **Fundamental idea:** First shrink the log to a small suspect scope, then localize; scopes come from stepwise expansion or Overstep/Loop expert heuristics.
- **Takeaway:** On Who&When, SDBL raises step accuracy by up to 24.27 percentage points, showing scoped diagnosis beats one-shot attribution.
- **Paper:** [https://ojs.aaai.org/index.php/AAAI/article/view/40594](https://ojs.aaai.org/index.php/AAAI/article/view/40594)
- **Code:** [https://github.com/Wen-qiangLi/SDBL](https://github.com/Wen-qiangLi/SDBL)

## 13. Who&When

<span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Multi-agent failure attribution lacks ground truth: who is the responsible agent and exactly which step is the decisive error?
- **Fundamental idea:** Curate 184 annotated tasks from 127 MAS runs over GAIA/AssistantBench; label decisive agent-step pairs with rationale.
- **Takeaway:** Test full-context vs incremental judging — agent attribution differs from step attribution; one-pass prompting is not enough.
- **Paper:** [https://arxiv.org/abs/2505.00212](https://arxiv.org/abs/2505.00212)
- **Code:** [https://github.com/ag2ai/Agents_Failure_Attribution](https://github.com/ag2ai/Agents_Failure_Attribution)

## 14. ECHO

<span class="tag">Failure Attribution</span>

- **Background problem:** Flat logs hide long-range error propagation; local windows miss causally relevant earlier turns.
- **Fundamental idea:** Build four positional context layers, run specialist analyst personas, and aggregate agent/step votes with confidence weighting and disagreement checks.
- **Takeaway:** Use compressed context plus voting when trace length matters; unlike CHIEF graphs or RAFFLES loops, ECHO is positional, not causal.
- **Paper:** [https://arxiv.org/abs/2510.04886](https://arxiv.org/abs/2510.04886)
- **Code:** Not found

## 15. RAFFLES

<span class="tag">Failure Attribution</span>

- **Background problem:** Single-pass judges miss decisive faults in long agent traces where early mistakes, symptoms, and recoverable errors blur together.
- **Fundamental idea:** A Judge proposes a fault step; specialized Evaluators score fault, primacy, decisiveness, and log consistency, feeding memory into retries.
- **Takeaway:** For trace debugging, use confidence-gated verifier loops, not one-shot attribution; stop when evaluators agree or max iterations expire.
- **Paper:** [https://arxiv.org/abs/2509.06822](https://arxiv.org/abs/2509.06822)
- **Code:** Not found

## 16. CDC-MAS

<span class="tag">Failure Attribution</span>

- **Background problem:** MAS failures often look downstream; log/data-flow analysis blames the final symptom, not the upstream bad decision.
- **Fundamental idea:** Reverse data-flow into a performance-causality graph, then use Shapley, ACE, and counterfactual repairs to rank agents and steps.
- **Takeaway:** Read for causal MAS debugging: strongest idea is performance causal inversion; evidence improves attribution but remains imperfect on hard traces.
- **Paper:** [https://arxiv.org/abs/2509.08682](https://arxiv.org/abs/2509.08682)
- **Code:** Not found

## 17. A2P

<span class="tag">Failure Attribution</span>

- **Background problem:** Long agent logs hide the decisive error; "spot the bad step" judging confuses correlation with cause.
- **Fundamental idea:** Prompt one judge to abduct a cause, propose a minimal fix, then simulate 3-5 counterfactual turns.
- **Takeaway:** For trace debugging, test whether a step's corrected action would change failure into success; step numbers matter.
- **Paper:** [https://arxiv.org/abs/2509.10401](https://arxiv.org/abs/2509.10401)
- **Code:** [https://github.com/ResearAI/A2P](https://github.com/ResearAI/A2P)

## 18. CHIEF

<span class="tag">Failure Attribution</span>

- **Background problem:** Flat agent logs hide whether failure began in planning, execution, data flow, or later symptom propagation.
- **Fundamental idea:** Build subtask/agent/step graphs, synthesize checkable oracles, then backtrack to the first irreversible corrupted state.
- **Takeaway:** Debuggable MAS need intermediate invariants, not just final-answer judges or after-the-fact trace summaries.
- **Paper:** [https://arxiv.org/abs/2602.23701](https://arxiv.org/abs/2602.23701)
- **Code:** [https://anonymous.4open.science/r/CHIEF-86B8](https://anonymous.4open.science/r/CHIEF-86B8)

## 19. AgenTracer

<span class="tag">Failure Attribution</span>

- **Background problem:** Prompting strong LLMs to blame failed agent traces gives poor step-level attribution, often below 10% on prior benchmarks.
- **Fundamental idea:** Build TracerTraj with counterfactual replay and fault injection, then RL-train Qwen3-8B to output `agentID | stepID`.
- **Takeaway:** Reliable agent debugging needs replay-derived root-cause labels; a small trained tracer can beat generic LLM judges.
- **Paper:** [https://arxiv.org/abs/2509.03312](https://arxiv.org/abs/2509.03312)
- **Code:** [https://github.com/bingreeky/AgenTracer](https://github.com/bingreeky/AgenTracer)

## 20. GraphTracer

<span class="tag">Failure Attribution</span>

- **Background problem:** Temporal trace attribution confuses late failure symptoms with earlier corrupted information sources in multi-agent deep search.
- **Fundamental idea:** Build an Information Dependency Graph: nodes are produced information, edges are explicit citation/reliance links across agent turns.
- **Takeaway:** Debug agents by tracing provenance paths, not timelines; target high-impact source nodes and dependency conflicts for realistic failure tests.
- **Paper:** [https://arxiv.org/abs/2510.10581](https://arxiv.org/abs/2510.10581)
- **Code:** Not found

## 21. AegisKong

<span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Manual MAS failure labels are tiny, so attribution models lack enough agent/error-mode pairs to learn root-cause patterns.
- **Fundamental idea:** Start from successful trajectories, inject MAST-style faults via prompt injection or response corruption, then keep failed runs as labeled counterfactuals.
- **Takeaway:** Aegis turns debugging data into controlled perturbation: 9,533 failures train Qwen/DCL/GRPO models and benchmark agent-error attribution.
- **Paper:** [https://arxiv.org/abs/2509.14295](https://arxiv.org/abs/2509.14295)
- **Code:** [https://kfq20.github.io/AEGIS-Website/](https://kfq20.github.io/AEGIS-Website/)

## 22. DoVer

<span class="tag">Failure Attribution</span> <span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Single blame steps break down when failed agent runs contain multiple re-plans, branches, and independently repairable mistakes.
- **Fundamental idea:** Segment by re-plan trials, edit the suspected message or plan, then replay from that point with prior context preserved.
- **Takeaway:** Treat attribution as executable evidence: success validates it, faithful failure refutes it, blocked execution exposes missing agent capabilities.
- **Paper:** [https://arxiv.org/abs/2512.06749](https://arxiv.org/abs/2512.06749)
- **Code:** [https://aka.ms/DoVer](https://aka.ms/DoVer)

## 23. TraceElephant

<span class="tag">Failure Attribution</span> <span class="tag">Datasets & Benchmarks</span>

- **Background problem:** Output-only attribution hides prompts, task context, tool logs, and environment state, making the decisive failure step ambiguous.
- **Fundamental idea:** TraceElephant packages 220 annotated failures as JSON step records plus runnable MAS environments for replay and counterfactual checks.
- **Takeaway:** Log agent inputs and tool interactions, not just outputs; full traces lift step attribution to 30.3%, dynamic replay to 33.3%.
- **Paper:** [https://openreview.net/forum?id=kLLYJ6Bm7n](https://openreview.net/forum?id=kLLYJ6Bm7n)
- **Code:** [https://github.com/TraceElephant/TraceElephant](https://github.com/TraceElephant/TraceElephant)

## 24. Maestro

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Prompt tuning cannot fix agents missing tools, state, validators, or routing.
- **Fundamental idea:** Alternate config tuning with local graph edits, guided by scores and reflective trace feedback.
- **Takeaway:** Treat agent failures as architecture signals: add the missing node, then retune its settings.
- **Paper:** [https://arxiv.org/abs/2509.04642](https://arxiv.org/abs/2509.04642)
- **Code:** [https://github.com/relai-ai/relai-sdk](https://github.com/relai-ai/relai-sdk)

## 25. CE-Graph

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Scalar workflow scores collapse rich failure traces, so global search like MaAS/AFlow misses recurring structural error modes.
- **Fundamental idea:** CE-Graph clusters counterexamples by failing node and error semantics, then verifies `RevisePrompt`, `InsertNode`, or `DeleteNode` graph edits.
- **Takeaway:** Optimize agent workflows by reducing dense failure modes, not by blindly searching for higher aggregate benchmark scores.
- **Paper:** [https://arxiv.org/abs/2510.10035](https://arxiv.org/abs/2510.10035)
- **Code:** Not found

## 26. ILWS

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** RAG is transient and fine-tuning is heavy; agents need durable domain learning without changing model weights.
- **Fundamental idea:** After sessions, reflect on traces and ratings to edit instructions, preferences, and tools under gated rollback.
- **Takeaway:** Treat system prompts as versioned pseudo-weights: persist only feedback-proven rules, not every retrieved fact.
- **Paper:** [https://arxiv.org/abs/2509.00251](https://arxiv.org/abs/2509.00251)
- **Code:** Not found

## 27. SCOPE

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Agents often see the right context, but static prompts do not teach them how to react to it.
- **Fundamental idea:** Synthesize trace-based guidelines, then route them into tactical or persistent system-prompt memory.
- **Takeaway:** For agents, evolve per-role prompts online; do not just append failures to chat history.
- **Paper:** [https://arxiv.org/abs/2512.15374](https://arxiv.org/abs/2512.15374)
- **Code:** [https://github.com/JarvisPei/SCOPE](https://github.com/JarvisPei/SCOPE)

## 28. AgentDevel

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Self-improving agents can raise averages while hiding regressions across versions.
- **Fundamental idea:** Run traces, label symptoms blindly, script diagnoses, then promote one RC using pass→fail / fail→pass gates.
- **Takeaway:** Build agent improvement as CI: auditable diffs, single version line, regression-first release decisions.
- **Paper:** [https://arxiv.org/abs/2601.04620](https://arxiv.org/abs/2601.04620)
- **Code:** Not found

## 29. ReCreate

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Creating domain agents needs evidence richer than final pass/fail scores.
- **Fundamental idea:** Inspect trajectories, verifier logs, artifacts, and environments to edit scaffold components.
- **Takeaway:** ReCreate outputs generalized prompts, workflows, tools, and memory—not tuned model weights.
- **Paper:** [https://arxiv.org/abs/2601.11100](https://arxiv.org/abs/2601.11100)
- **Code:** [https://github.com/zz-haooo/ReCreate](https://github.com/zz-haooo/ReCreate)

## 30. AgentDiet

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Agent tool traces accumulate useless, repeated, and expired tokens that get resent every later step.
- **Fundamental idea:** Use a cheap reflection LLM to rewrite one delayed long step within a small sliding context window.
- **Takeaway:** Reduce cost empirically, not by guarantee: protect success with delay, thresholds, structure preservation, and pass-rate/step-count checks.
- **Paper:** [https://arxiv.org/abs/2509.23586](https://arxiv.org/abs/2509.23586)
- **Code:** Not found

## 31. SupervisorAgent

<span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Multi-agent runs fail or waste tokens through errors, loops, and bloated tool observations during execution.
- **Fundamental idea:** Intercept each ActionStep with cheap heuristics, then replace observations, append guidance, or run verification.
- **Takeaway:** Runtime supervision works best as gated process control, not always-on monitoring or post-hoc debugging.
- **Paper:** [https://arxiv.org/abs/2510.26585](https://arxiv.org/abs/2510.26585)
- **Code:** [https://github.com/LINs-lab/SupervisorAgent](https://github.com/LINs-lab/SupervisorAgent)

## 32. AgentSight

<span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Framework logs miss shell escapes; syscall monitors miss the LLM intent behind file, process, and network effects.
- **Fundamental idea:** Attach eBPF uprobes to TLS reads/writes and kernel probes to syscalls, then correlate by lineage, timing, and argument matches.
- **Takeaway:** For coding agents, observability must follow descendant processes, not just tool calls, to catch prompt injection and exfiltration.
- **Paper:** [https://arxiv.org/abs/2508.02736](https://arxiv.org/abs/2508.02736)
- **Code:** [https://github.com/eunomia-bpf/agentsight](https://github.com/eunomia-bpf/agentsight)

## 33. AgentOps Automation

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Deployed agents are nondeterministic, stateful systems; traditional logs and dashboards miss planning, memory, tool, and coordination failures.
- **Fundamental idea:** Define AgentOps as observe behavior, collect metrics, detect issues, identify root causes, recommend fixes, and automate runtime operations.
- **Takeaway:** Treat agent reliability as production operations: instrument trajectories, compare healthy/failing traces, and close safe feedback loops with automated remediation.
- **Paper:** [https://arxiv.org/abs/2507.11277](https://arxiv.org/abs/2507.11277)
- **Code:** Not found

## 34. AgentDiagnose

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">Failure Attribution</span>

- **Background problem:** Trajectory replays show what happened, but rarely score how agents explore, plan, read observations, or verify progress.
- **Fundamental idea:** Score trajectories on five agentic competencies, then expose patterns through CLI output, dashboard plots, word clouds, and navigation timelines.
- **Takeaway:** Debugging tools should turn raw trajectories into selectable behavioral signals that support diagnosis and training-data filtering.
- **Paper:** [https://aclanthology.org/2025.emnlp-demos.15/](https://aclanthology.org/2025.emnlp-demos.15/)
- **Code:** [https://github.com/oootttyyy/AgentDiagnose](https://github.com/oootttyyy/AgentDiagnose)

## 35. Agent Trajectory Explorer

<span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Raw agent trajectories mix prompts, reasoning, tool calls, and observations, making human oversight difficult.
- **Fundamental idea:** Convert JSON traces via formatter plugins into linear TAO turns, with optional raw-context view.
- **Takeaway:** Start debugging UIs with TAO step inspection and per-thought/action positive-negative feedback.
- **Paper:** [https://ojs.aaai.org/index.php/AAAI/article/view/35350](https://ojs.aaai.org/index.php/AAAI/article/view/35350)
- **Code:** Not found

## 36. AGDebugger

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Multi-agent failures live inside stateful message queues; log viewers cannot test "what if this message changed?"
- **Fundamental idea:** Checkpoint each agent before messages, then restore, edit one message, and fork a replay branch.
- **Takeaway:** Best for counterfactual steering; study users mostly added specificity, simplified tasks, or changed plans.
- **Paper:** [https://arxiv.org/abs/2503.02068](https://arxiv.org/abs/2503.02068)
- **Code:** [https://github.com/microsoft/agdebugger](https://github.com/microsoft/agdebugger)

## 37. XAgen

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">Failure Attribution</span> <span class="tag">System Enhancement & Optimization</span>

- **Background problem:** Multi-agent failures are hard for mixed-expertise users to locate, attribute, and correct from raw logs.
- **Fundamental idea:** Convert CrewAI logs into a live flowchart; attach human feedback; use an LLM judge to score task outputs.
- **Takeaway:** Explanations matter when they identify a failing node and feed directly into prompt/config edits and reruns.
- **Paper:** [https://arxiv.org/abs/2512.17896](https://arxiv.org/abs/2512.17896)
- **Code:** Not found

## 38. DiLLS

<span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Chronological multi-agent logs hide plan failures, skipped actions, and stalled progress across verbose agent/tool exchanges.
- **Fundamental idea:** Use natural-language probes to organize traces into activity, action, and operation layers for drill-down diagnosis.
- **Takeaway:** Debugging agents needs task-structured trace views: plan updates first, action outcomes next, raw logs last.
- **Paper:** [https://arxiv.org/abs/2602.05446](https://arxiv.org/abs/2602.05446)
- **Code:** Not found

## 39. TAR Study of SE Agents

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">Failure Taxonomy</span>

- **Background problem:** SE agents leave TAR logs, but failures hide in repeated actions, untested fixes, ignored results, and thought-action mismatches.
- **Fundamental idea:** Normalize 120 RepairAgent, AutoCodeRover, and OpenHands trajectories; categorize actions, mine 4-grams, and open-code semantic TAR relations.
- **Takeaway:** Successful agents balance explore/fix/test; robust agents should flag repetition, fix-without-test, premature termination, and result-insensitive next actions.
- **Paper:** [https://arxiv.org/abs/2506.18824](https://arxiv.org/abs/2506.18824)
- **Code:** [https://github.com/sola-st/llm-agents-study](https://github.com/sola-st/llm-agents-study)

## 40. MAESTRO Evaluation Suite

<span class="tag">Datasets & Benchmarks</span> <span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Final-answer scores miss MAS runtime variance, silent failures, and architecture-driven cost.
- **Fundamental idea:** Run 12 heterogeneous MAS examples under one config, telemetry, and post-processing interface.
- **Takeaway:** Benchmark agent architectures as systems: trace calls, retries, tokens, latency, failures, and stability.
- **Paper:** [https://arxiv.org/abs/2601.00481](https://arxiv.org/abs/2601.00481)
- **Code:** [https://github.com/sands-lab/maestro](https://github.com/sands-lab/maestro)

## 41. TrajectoryGuard

<span class="tag">Trajectory Monitoring & Analysis Tools</span> <span class="tag">Failure Attribution</span>

- **Background problem:** Agent plans can be wrong for the task or structurally incoherent, and LLM judges are too slow for runtime screening.
- **Fundamental idea:** Train a Siamese GRU autoencoder on task/trajectory pairs, combining contrastive alignment with sequence reconstruction anomaly signals.
- **Takeaway:** Use learned trajectory guards for fast pre-execution checks; escalate uncertain or long-horizon cases to heavier judges.
- **Paper:** [https://arxiv.org/abs/2601.00516](https://arxiv.org/abs/2601.00516)
- **Code:** Not found

## 42. Features to Actions

<span class="tag">Trajectory Monitoring & Analysis Tools</span>

- **Background problem:** Feature attribution explains one prediction, but tool agents fail across state, action, observation trajectories.
- **Fundamental idea:** Package each run as a Minimal Explanation Packet: trace evidence plus rubric flags for intent, tools, state, recovery.
- **Takeaway:** Use SHAP for aggregate rubric importance; use trace rubrics to locate the failed step or violated constraint.
- **Paper:** [https://arxiv.org/abs/2602.06841](https://arxiv.org/abs/2602.06841)
- **Code:** [https://github.com/VectorInstitute/unified-xai-evaluation-framework](https://github.com/VectorInstitute/unified-xai-evaluation-framework)
