# Emacs Mortgage

I am loathe to declare emacs bankruptcy.

So, instead, I'm taking out a mortgage - dedicating small hunks of my time to
an incremental rewrite, repairing or avoiding the various hunks of cruft and
broken edges that have caused pain in my original config.

If it reaches sufficient stability for day-to-day use, I will move it out to
the project directory and it will become my daily driver.

Goals:

- Package management with elpaca.el. My hacks for auto-updating via package.el
  hit occasional explosions, and elpaca seems to enable a better workflow for
  managing packages that live in their own repos.

- E2E tests for core functionality, with assertions enforcing minimum execution
  speeds on day-to-day operations.
  [emacs-director](https://bard.github.io/emacs-director/end-to-end-testing)
  looks like a promising tool for building that out. Specific features to test
  / time:

  * startup (< 1 second)

- Tree-sitter major-modes by default.

- Find file by name in project - E2E test < 200ms

- Jump-to-def / find-references via LSP (TS / JS, Python, Rust) - E2E test < 200ms

- magit + delta + difftastic - status / diff < 1 s with >1000-line change active

- search all files in 40k files repo (rg in nixpkgs) - E2E test < 200 ms

- Set breakpoint, start debugger via dap-mode - E2E test < 1s (from debugger
  start to breakpoint inspection)

- Fully-automated, scheduled package updates, driven by elpaca and
  E2E tests (auto-bisect on failures).

- Counsel-dash docs exploration auto-installed / auto-enabled as needed, with
  hotkeys bound to look up thing-at-point or just open docs for current buffer.

- Bring over core features of my main config. smart-dash mode, my custom
  keybindings, yasnippet config, etc...
