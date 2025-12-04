"""
pyimpsort.py --- Python import sorter

This module sorts Python import statements.

It is inspired by the Vim plugin 'vim-sort-python-imports'
(https://github.com/public/vim-sort-python-imports/blob/master/plugin/sort_imports.py)

References:
- PEP 8 — Style Guide for Python Code: https://www.python.org/dev/peps/pep-0008/#imports
- Reddit Python Import Guidelines: https://github.com/reddit/reddit/wiki/PythonImportGuidelines
- Google Python Style Guide: https://google.github.io/styleguide/pyguide.html#Imports
"""

# SPDX-License-Identifier: MIT
# Copyright (c) 2014-2025 The pyimpsort contributors
# See LICENSE-MIT for license details

import argparse
import ast
import functools
import importlib.util
import itertools
import pathlib
import re
import shutil
import sys
import sysconfig
import textwrap
from glob import glob1
from os import path, scandir


def _clean_ext_suffix(libname):
    # See: https://www.python.org/dev/peps/pep-3149/
    ext_suffix = sysconfig.get_config_var('EXT_SUFFIX') or '.so'
    return libname.replace(ext_suffix, '')


class ImpSorter:
    """
    Sorts Python import statements according to PEP8 and Google style guidelines.

    Organizes imports into groups:
        - FUTURE
        - STDLIB
        - SITE (optional)
        - THIRD_PARTY
        - LOCAL
        - RELATIVE

    Also ensures a single import per line and supports merging multiple imports
    from the same module, if configured.
    """

    FUTURE = 0
    STDLIB = 1
    SITE = 2
    THIRD_PARTY = 3
    LOCAL = 4
    RELATIVE = 5

    def __init__(self, group=False, site=False, user_locals=None):
        """
        Initialize the import sorter configuration.

        Args:
            group (bool): If True, group multiple imports from the same module.
            site (bool): If True, separate platform site-packages into their own group.
            user_locals (list[str] or None): List of module names to consider
                as local imports.
        """
        self.group = group
        self.site = site
        self.user_locals = user_locals or []
        self.imports = {}
        self.from_imports = {}
        self.stdlibs = set(self.iter_stdmodules()) | set(self.get_dynlibs()) | set(sys.builtin_module_names)
        self.python_paths = [p for p in sys.path if p]

    def _merge_comment(self, alias1, alias2):
        comment = " ".join(
            x
            for x in (getattr(alias, "comment", None) for alias in (alias1, alias2))
            if x
        )
        if comment:
            alias1.comment = comment

    def append(self, node):
        """
        Add an import or import-from AST node to the sorter, merging duplicates,
        preserving comments, and ensuring that imports are split into
        individual (unitary) statements.

        Args:
            node (ast.Import or ast.ImportFrom): The AST node to process.
        """
        if isinstance(node, ast.Import):
            for idx, alias in enumerate(node.names):
                simple_node = self.imports.get((alias.name, alias.asname))
                if simple_node:
                    self._merge_comment(simple_node.names[0], alias)
                else:
                    simple_node = ast.Import(names=[alias], comments=[])
                    self.imports[(alias.name, alias.asname)] = simple_node
                if idx == 0:
                    simple_node.comments = node.comments + simple_node.comments
        else:
            for idx, alias in enumerate(node.names):
                simple_node = self.from_imports.get(
                    (node.level, node.module, alias.name, alias.asname)
                )
                if simple_node:
                    self._merge_comment(simple_node.names[0], alias)
                else:
                    simple_node = ast.ImportFrom(
                        level=node.level, module=node.module, names=[alias], comments=[]
                    )
                    self.from_imports[
                        (node.level, node.module, alias.name, alias.asname)
                    ] = simple_node
                if idx == 0:
                    simple_node.comments = node.comments + simple_node.comments

    @staticmethod
    def get_dynlibs():
        """Return dynamic standard libraries (compiled extensions)."""
        dirname = path.join(sysconfig.get_path("stdlib"), "lib-dynload")
        dynlibs = glob1(dirname, '*.so')
        return [_clean_ext_suffix(x) for x in dynlibs]

    @staticmethod
    def iter_stdmodules():
        # note this misses some edge cases that the original ImpImporter covered, i.e., namespace packages
        # but works for the majority of common packages
        stdlib_path = sysconfig.get_path('stdlib')
        for entry in scandir(stdlib_path):
            if entry.is_file() and entry.name.endswith('.py'):
                module_name = path.splitext(entry.name)[0]
                if module_name != '__init__':
                    yield module_name
            elif entry.is_dir():
                init_file = path.join(entry.path, '__init__.py')
                if path.isfile(init_file):
                    yield entry.name

        # Check for frozen modules
        for module_name in sys.builtin_module_names:
            if importlib.util.find_spec(module_name) is not None:
                yield module_name

    def _node_sort_key(self, node):
        """
        Given an AST node return a tuple which is used for sorting.
        """
        if isinstance(node, ast.Import):
            sortname = tuple(
                x for x in (node.names[0].name, node.names[0].asname) if x is not None
            )
            fromimport = 0
        elif isinstance(node, ast.ImportFrom):
            sortname = tuple(
                x for x in (node.module or "", node.names[0].name, node.names[0].asname) if x is not None
            )
            fromimport = 1
        else:
            raise TypeError(node)

        modname = sortname[0].split('.')[0]
        if getattr(node, "level", 0) != 0:
            group = self.RELATIVE
        elif modname in self.user_locals:
            group = self.LOCAL
        elif modname == '__future__':
            group = self.FUTURE
        elif modname in self.stdlibs:
            group = self.STDLIB
        else:
            try:
                spec = next(
                    x
                    for x in (
                        importlib.util.find_spec(modname, p) for p in self.python_paths
                    )
                    if x
                )
                origin = pathlib.Path(spec.origin or "/")
                if self.site and any(
                    origin.is_relative_to(pathlib.Path(sysconfig.get_path(x)))
                    for x in ("purelib", "platlib")
                ):
                    group = self.SITE
                else:
                    group = self.THIRD_PARTY
            except StopIteration:
                group = self.LOCAL
        return (group, fromimport, sortname)

    def _format_as(self, name, asname, comment):
        res = name
        if asname:
            res += f" as {asname}"
        if comment:
            res += f"  #{comment}"
        return res

    def format_import(self, node):
        """Format a single 'import ...' AST node."""
        lines = [f"#{comment}" for comment in node.comments]
        alias = node.names[0]
        lines.append(
            "import "
            + self._format_as(alias.name, alias.asname, getattr(alias, "comment", None))
        )
        return lines

    def format_import_from(self, node):
        """
        Format an `ast.ImportFrom` node into a properly formatted list of lines.

        Tries to fit one-liners under 80 characters, otherwise uses multiline syntax.
        Preserves inline comments.
        """
        node.names.sort(key=lambda x: (x.name, x.asname or ""))
        lines = [f"#{comment}" for comment in node.comments]
        frompart = f"from {'.' * node.level}{node.module or ''} import "
        names = node.names
        if len(names) == 1:
            alias = node.names[0]
            lines.append(
                frompart
                + self._format_as(
                    alias.name, alias.asname, getattr(alias, "comment", None)
                )
            )
            return lines

        if any(getattr(alias, "comment", None) for alias in node.names):
            # At least one alias has an inline comment
            lines.append(f"{frompart} (")
            for alias in node.names:
                line = f"    {alias.name}"
                if alias.asname:
                    line += f" as {alias.asname}"
                line += ","
                comment = getattr(alias, "comment", None)
                if comment:
                    line += f"  #{comment}"
                lines.append(line)
            lines.append(")")
            return lines

        fullnames = [
            " as ".join(nm for nm in (name.name, name.asname) if nm)
            for name in node.names
        ]
        if len(frompart) + sum(len(x) for x in fullnames) + (len(fullnames) - 1) * 2 <= 80:
            # One-liner is short enough to fit within 80 characters
            lines.append(frompart + ", ".join(fullnames))
            return lines

        # Multiline formatting fallback
        lines.append(frompart + "(")
        toks = [x + "," for x in fullnames[:-1]]
        toks.append(fullnames[-1])
        line = "   "
        for tok in toks:
            if len(line) + len(tok) + 1 <= 80:
                line += f" {tok}"
            else:
                lines.append(line)
                line = f"    {tok}"
        lines.append(line)
        lines.append(")")
        return lines

    def _merge_from_import(self, a, b):
        a.comments.extend(b.comments)
        a.names.extend(b.names)
        return a

    def write_sorted(self, file=sys.stdout):
        """
        Write the sorted and formatted import statements to a file-like object.

        Args:
            file: Output stream to write sorted import block.
        """
        nodes = list(self.imports.values())
        if self.group:
            for module, group in itertools.groupby(
                sorted(self.from_imports.items()), key=lambda x: x[0][:2]
            ):
                nodes.append(
                    functools.reduce(self._merge_from_import, (n[1] for n in group))
                )
        else:
            nodes.extend(self.from_imports.values())

        pkey = None
        for key, node in sorted((self._node_sort_key(node), node) for node in nodes):
            # insert new lines between groups
            if pkey and key[0] != pkey[0]:
                file.write("\n")
            pkey = key
            if isinstance(node, ast.Import):
                lines = self.format_import(node)
            else:
                lines = self.format_import_from(node)
            for line in lines:
                file.write(line + "\n")


comment_regex = re.compile(r"^(?P<before>[^#]*)(#(?P<comment>.*))?$")
alias_regex = re.compile(r"(?P<name>\w+)(\s+as\s+(?P<asname>\w+))?,?\s*$")


def set_comment(node, lines):
    """
    Extracts inline and leading comments from the given source lines and attaches
    them to the appropriate parts of the AST import node.

    Args:
        node (ast.Import or ast.ImportFrom): The AST node to update.
        lines (list[str]): Corresponding lines from the source code.
    """
    node.comments = []
    for line in lines:
        m = comment_regex.match(line)
        if not m.group("comment"):
            continue
        comment = m.group("comment")
        if not comment:
            continue
        m = alias_regex.search(m.group("before"))
        if m:
            name = m.group("name")
            asname = m.group("asname")
            alias = next(x for x in node.names if x.name == name and x.asname == asname)
            alias.comment = comment
        else:
            node.comments.append(comment)


def end_of_statement(src, lineidx):
    """
    Replacement for ast.AST.end_lineno, which is missing in Python 3.7.

    FIXME: remove when dropping support for Python 3.7.
    """
    idx = lineidx
    parent = False
    while True:
        line = src[idx]
        idx += 1
        while line.endswith("\\"):
            line = line[:-1] + src[idx]
            idx += 1
        comment_idx = line.find("#")
        if comment_idx >= 0:
            line = line[:comment_idx]
        if "(" in line:
            parent = True
        if not parent or parent and ")" in line:
            return idx


def iter_blocks(nodes, src, sorter_factory):
    """
    Iterate over top-level blocks of import statements.

    Groups together adjacent import statements and applies the sorter.

    Args:
        nodes: List of top-level AST nodes.
        src: List of lines from the source file.
        sorter_factory: Function that returns a new ImpSorter instance.

    Yields:
        ImpSorter: A sorter instance for each block of contiguous imports.
    """
    node_it = iter(nodes)
    while True:
        try:
            node = next(
                n for n in node_it if isinstance(n, (ast.Import, ast.ImportFrom))
            )
        except StopIteration:
            return
        sorter = sorter_factory()
        idx = node.lineno - 1
        while True:
            if sys.version_info >= (3, 8):
                end_lineno = node.end_lineno
            else:
                end_lineno = end_of_statement(src, node.lineno - 1)
            set_comment(node, src[idx : end_lineno])
            sorter.append(node)
            idx = end_lineno
            try:
                node = next(node_it)
            except StopIteration:
                yield sorter
                return
            if not isinstance(node, (ast.Import, ast.ImportFrom)):
                break
        yield sorter


def parse_args(argv):
    parser = argparse.ArgumentParser(
        description=(
            "Sort import statements in a Python file.\n\n"
            + textwrap.fill(
                "Only the top contiguous block of imports is considered and "
                "written to the output; the rest of the code is ignored.",
                width=shutil.get_terminal_size(fallback=(80, 20)).columns,
            )
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "infile",
        nargs="?",
        type=argparse.FileType("r"),
        default=sys.stdin,
        help="Input Python file to sort imports from (defaults to stdin if not provided)",
    )
    parser.add_argument(
        "outfile",
        nargs="?",
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="Output file to write the sorted result to (defaults to stdout if not provided)",
    )
    parser.add_argument(
        "--group",
        action="store_true",
        default=False,
        help="Group multiple imports from the same module into a single "
        "'from ... import ...' statement",
    )
    parser.add_argument(
        "--site",
        action="store_true",
        default=False,
        help="Create a separate group for modules installed in the platform's "
        "site-packages directory.",
    )
    parser.add_argument(
        "--local",
        action="append",
        default=[],
        metavar="MODULE",
        help="Mark MODULE as a local import. Can be specified multiple times.",
    )
    return parser.parse_args(argv)


def pyimpsort(args):
    """
    Main logic for reading input, sorting imports, and writing output.

    Only processes the first contiguous block of import statements.
    """
    data = args.infile.read()
    for block in iter_blocks(
        ast.parse(data).body,
        data.splitlines(),
        lambda: ImpSorter(group=args.group, site=args.site, user_locals=args.local),
    ):
        block.write_sorted(args.outfile)
        # For compatibility, only the first block is processed for now.
        break


def main():
    """Entry point for CLI usage."""
    pyimpsort(parse_args(sys.argv[1:]))


if __name__ == '__main__':
    main()
