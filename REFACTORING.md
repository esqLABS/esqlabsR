# Refactoring brief: JSON-based project model

## Current goal

Ship the 6.0.0 JSON-driven project model. A project is a `definitions/` tree of small one-per-definition JSON files, and that tree is the single source of truth. It loads into an in-memory `Project`; edits stay in memory until `saveProject()` reconciles the tree (explicit-save, not write-through).

Excel is demoted from a source of truth to an interchange format: import a legacy Excel project, export to Excel, or round-trip. It stays a supported *secondary* entrypoint; JSON is primary.

Design record/remaining issues: issue #1088. Live status/project branch: PR #1017.

## Rules

- **Read callers before changing.** Before touching any exported function or R6 method, grep and read every caller in `R/` and `tests/`. Never refactor a public interface in isolation.
- **Excel stays as secondary entrypoint.** Do not remove the Excel-based configuration path. It stays functional; JSON is primary, Excel is the fallback. Any change to shared code must keep both entrypoints working.
- **Tests green after every step.** Run `testthat` after each step, not each session. Where coverage is thin on code being refactored, write characterization tests first.
- **One thing per step, one step per commit.** One file extracted, one caller updated, one test run, one commit. Never combine steps.
- **Plan before code.** For any non-trivial extraction, propose the plan (new files, moved functions, prop/argument changes) and get approval before writing code.
- **GitHub is the only status source.** The migration is built but still stabilizing: features shipped, some have open bugs/gaps. Live status lives only in GitHub (issue #1088 design record, PR #1017 review threads) and in-code `TODO`s where the gap is. Never mirror bug status into this file. Before treating any area below as finished, check for open issues touching it.
- **Account for other pending PRs and issues.** Before changing or reviewing code, check both open PRs against this branch (`gh pr list --base json-based-project`) and the open issues tied to the #1088 umbrella. Take in-flight PRs into account so work doesn't collide or duplicate. Read the related open issues so you don't implement something a planned issue will contradict, and so you can spot when one change can close several issues at once.
- **Comments explain, and describe now.** A comment earns its place only by explaining what the code cannot say itself: the why, a non-obvious constraint, a subtle edge case. Don't restate self-explanatory code. And describe how the code works currently, never its history (no "previously", "used to", "was write-through, now explicit-save"); that belongs in commits and PRs, not the source.
- **Log every commit.** Each time a commit is made, append one bullet to `REFACTORING-LOG.md`: `- YYYY-MM-DD HH:MM: <plain-language one-liner of what changed>. (#<issue number>)`. End the description with the associated issue number in that format when there is one; omit the `. (#N)` when the change has no issue. Newest at the bottom. Keep it a short human phrase, not the raw commit subject.
- **Use GitHub stacked Pull Request Framework**: use the gh-stack skill to manage a stacked PR that targets `json-based-project`. **Root the stack at `json-based-project`, never at `v6` or `main`**: `gh stack init --base json-based-project <branch>`. A stack rooted lower makes `v6` (and the whole v5 -> v6 migration) a member of the stack, so every PR on top inherits that chain's divergence: GitHub shows the top PR a conflict banner and an unknown mergeable state that describe the layers below it, not the PR's own changes. Do not "fix" that by rebasing the stack, which rewrites and force-pushes `v6` and `json-based-project` that every other open PR is based on. If a stack rooted too low already exists on GitHub, take the PR out of it and re-init with the right base (`gh stack unstack --local` drops only the local tracking, leaving other people's PRs alone). Always pass `--json` to `gh stack view` and a branch name to `init` / `add` / `checkout`; without them those commands open a prompt or TUI. Note that a stack number and a PR number look alike but are different: the number in GitHub's "Resolve conflicts via the command line" dialog is the stack's, so `gh stack checkout <n>` resolves it while `gh pr view <n>` does not.

## Conventions (established in the codebase)

Match these when adding or reviewing code, so new work stays consistent with what's already there. Each is a real, repeated pattern in the migration-era `R/` files, not an aspiration.

- **Authoring verbs are `add*` / `set*` / `remove*`** (plus `rename*` / `duplicate*` / `create*` where they apply). Name new mutators the same way.
- **Three-layer authoring chain.** An authoring operation is a free function -> public `Project` method -> private `.<verb><Section>_impl` free function. The free function guards the type and forwards; the method forwards through `private$.impl()`; the `_impl` holds the logic. Keep this shape; don't put logic in the free function or the method.
- **`_impl` signature and error attribution.** A `_impl` is dot-prefixed, named `.<verb><Section>_impl`, takes `(self, private, ..., .call)` with `.call` last, and its first statement is `rlang::local_error_call(.call)` so aborts read as `Error in \`addScenario()\`:`. Frame-depth arithmetic lives only in `private$.impl()` (`R/project.R`). A `_impl` calling a sibling `_impl` threads its own `.call` and skips `.impl()`.
- **Guard, then forward.** A free authoring function's first line is `validateIsOfType(project, "Project")`, then it forwards. Authoring functions return the project invisibly (`invisible(self)` / `invisible(project)`) and document `@returns The \`project\` object, invisibly.`
- **All state writes go through the private seam.** Read/write sections via `private$.getSection()` / `.setSection()` (they set the dirty bit and invalidate the validation cache). Don't poke section fields directly.
- **Ids are canonicalized; shared vectorization helpers.** Run ids through `.canonicalizeId()`, and use the shared `.assertIdVector()` / `.assertNoDuplicateIds()` / `.recycleField()` / `.alignAuthoringArgs()` helpers rather than re-implementing per domain.
- **User-facing text goes through the `messages` catalog.** Add an entry in `R/messages.R` and raise it via `cli::cli_abort` / `cli::cli_warn` / `cli::cli_inform`. Don't `stop()` / `warning()` on inline literals. Build the text with `cliFormat()` by default, and with `cli::format_message()` only when `cliFormat()` can't express the message: `cliFormat()` formats one inline string, so it drops the names of a `cli` bullet vector (and does not process the `\` end-of-line continuation), which means any message carrying `"i"` / `"x"` / `"!"` / `"*"` bullets needs `cli::format_message()`. (Known exception: project validators build `validationResult` messages inline with `paste0()`.)
- **Validation is a convention-based dispatcher.** Each section file defines `.<section>ValidatorAdapter(project)` (slices the project, calls a section-local `.validate<Section>()` returning a `validationResult`), registered in `.validationAdapters` in `R/validation.R`. `crossReferences` is deliberately a fixed final phase, not an adapter. Add a new section's validation the same way.
- **File layout: plural domain file owns its section, 1:1 with its test.** `R/<domain>.R` (plural, e.g. `scenarios.R`) owns that section's parse + validate + mutate and pairs with exactly one `tests/testthat/test-<domain>.R`.
- **roxygen: `@returns` (with the s)** is the house tag; pair `@keywords internal` with `@noRd` on internal helpers.


## Already done

_What shipped structurally. Shipped is not bug-free: for open gaps see the "GitHub is the only status source" rule above._

- v2.0 `Project.json` schema + in-memory `Project` R6 shape (`R/project.R`)
- Persistence engine: per-definition file tree + id canonicalization (`R/definition-files.R`, `R/definition-id.R`, `R/project-lifecycle.R`)
- `Project` -> JSON serialization (`R/project-to-json.R`)
- Explicit-save lifecycle (dropped auto-save)
- Typed definition classes parsed from `Project`: scenarios, individuals, populations, observed data, output paths, parameters; read-only `DefinitionList` wrapper per section
- runScenarios, data combination, plotting, and parameter identification all driven from a parsed `Project`
- Programmatic `add*`/`remove*` mutation API on `Project`
- Convention-based validation dispatcher (`R/validation.R`)
- Excel bridge: import/export kept as secondary entrypoint (`R/project-excel.R`)
- Restructures: plots (three per-definition kinds), PI reshape, sensitivity split into calc + plot files
- Utilities collapsed: `utilities-*.R` -> focused per-domain files (`R/utils.R`, `R/file-utils.R`, etc.)
- Docs: vignettes, README, NEWS, pkgdown, slide decks; regenerated `man/*.Rd` + NAMESPACE
- Fixtures: bundled example + test-data projects; test suite reorganized per domain

## Do not touch

- **Sensitivity analysis** (`R/sensitivity-calculation*.R`, `R/sensitivity-*-plot.R`, `R/sensitivity-time-profiles.R`). Leave its logic alone. Exception: if something internal it depends on changes, update the wiring so it keeps working.
