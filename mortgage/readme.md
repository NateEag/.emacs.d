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
  * find file by name < 200 ms in 40k files repo (fd in nixpkgs)
  * search all files < 200 ms in 40k files repo (rg in nixpkgs)
  * magit status / diff with >1000-line change active
  * Jump-to-def / find-references via LSP (TS / JS, Python, Rust)

- Fully-automated, scheduled package updates, driven by elpaca and
  aforementioned E2E tests.

- Tree-sitter major-modes by default.

- Counsel-dash docs exploration auto-installed / auto-enabled as needed.

- Bring over core features of my main config. smart-dash mode, my various
  custom keybindings, yasnippet config, etc...
