#!/usr/bin/env python

import re

FUNC_SKEL = """#contributor : Andrew Gwozdziewycz <web@apgwoz.com>
#name : %(func_name)s(%(func_sig)s)
# --
%(func_name)s(%(func_args)s)$0"""

func_args_re = re.compile('([a-zA-Z0-9_]+) \((.*?)\)')

def split_func_args(file):
    fi = open(file)
    lines = [x for x in fi.xreadlines() if not x.startswith('  ')]
    fargs = []
    for n in lines:
        it = func_args_re.search(n)
        if it:
            print it.groups()
            fargs.append(it.groups())
    return fargs

def parse_args(args):
    args = args.split('[')
    argsl = args[0].split(',')
    argsout = []
    for arg in argsl:
        arg = arg.strip()
        pieces = arg.split()
        if len(pieces) == 2:
            argsout.append(pieces[1])
    if len(args) > 1 and len(args[1]):
        newa = '[' + args[1].rstrip()
        if not newa.endswith(']'):
            newa += ']'
        if len(argsout):
            argsout[-1] += newa
        else:
            argsout.append(newa)
    return argsout

def parse_definitions(file):
    functions = split_func_args(file)
    out = []
    for f in functions:
        args = parse_args(f[1])
        out.append((f[0], args))
    return out
    
def generate_snippets(outpath, defs):
    for d in defs:
        ellipsis = ', '.join(['...'] * len(d[1]))
        args = ', '.join(['${%s}' % x for x in d[1]])
        filename = d[0].replace('_', '.', 1).replace('_', '-')
        f = open(outpath + '/' + filename, 'w')
        f.write(FUNC_SKEL % {'func_sig': ellipsis,
                           'func_name': d[0],
                           'func_args': args})

if __name__ == "__main__":
    defs = parse_definitions("php-functions-with-args.txt")
    generate_snippets('php-mode', defs)
