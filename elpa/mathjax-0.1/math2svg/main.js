/*!
 * mathjax.el --- Render formulas using MathJax
 *
 * Copyright (C) 2024  Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

// Cf. https://github.com/mathjax/MathJax-demos-node/blob/master/preload

const config = JSON.parse(process.argv[2] || "{}")
MathJax = {
  options: {
    compileError: (_, __, err) => { throw err },
    typesetError: (_, __, err) => { throw err },
    ...config.options
  },
  tex: {
    formatError: (_, err) => { throw err },
    packages: ["base", "autoload", "require", "ams", "newcommand"],
    ...config.tex
  },
  svg: config.svg,
  startup: { typeset: false }
}

//  Load the needed components
require("mathjax-full/components/src/mml-svg/mml-svg.js")
require("mathjax-full/components/src/adaptors/liteDOM/liteDOM.js")
require("mathjax-full/components/src/startup/lib/startup.js")
require("mathjax-full/components/src/core/core.js")
require("mathjax-full/components/src/adaptors/liteDOM/liteDOM.js")
require("mathjax-full/components/src/input/tex-base/tex-base.js")
require("mathjax-full/components/src/input/tex/extensions/all-packages/all-packages.js")
require("mathjax-full/components/src/output/svg/svg.js")
require("mathjax-full/components/src/output/svg/fonts/tex/tex.js")
require("mathjax-full/components/src/a11y/assistive-mml/assistive-mml.js")
require("mathjax-full/components/src/startup/startup.js")
require("mathjax-full/components/src/input/asciimath/asciimath.js")

// Let MathJax know these are loaded
MathJax.loader.preLoad(
  "mml-svg",
  "core",
  "adaptors/liteDOM",
  "input/asciimath",
  "input/tex-base",
  "[tex]/all-packages",
  "output/svg",
  "output/svg/fonts/tex",
  "a11y/assistive-mml"
)

MathJax.config.startup.ready()
require("readline").createInterface(process.stdin).on("line", line => {
  let result
  try {
    const { math, format, ...options } = JSON.parse(line)
    const [svg, mml] = MathJax[`${format}2svg`](math, options).children
    result = {
      svg: MathJax.startup.adaptor.outerHTML(svg),
      mml: mml && MathJax.startup.adaptor.outerHTML(mml)
    }
  } catch (err) {
    result = { error: err.message }
  }
  process.stdout.write(JSON.stringify(result))
})

// Local Variables:
// js-indent-level: 2
// End:
