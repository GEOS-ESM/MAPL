#!/usr/bin/env python3
"""Verify that working-tree edits to Fortran files are comment-only.

Usage: check-comment-only.py [--base REV] [PATH ...]

For every modified *.F90 / *.pf file under PATH (default: whole repo),
strips '!' comments (outside string literals; '!$' directives are kept)
and blank lines from the REV version (default HEAD) and the working-tree
version, and diffs the results. Exit 0 only if every file is identical.
Also reports remaining 'openspec/changes' references in each PATH
(for a directory: its own *.F90 files, not subdirectories).
"""
import argparse
import difflib
import pathlib
import subprocess
import sys


def strip_line(line):
    s = line.rstrip('\n')
    if s.lstrip().startswith('!$'):
        return s.rstrip()
    quote = None
    for i, ch in enumerate(s):
        if quote:
            if ch == quote:
                quote = None
        elif ch in ('"', "'"):
            quote = ch
        elif ch == '!':
            s = s[:i]
            break
    return s.rstrip()


def strip(text):
    return [l for l in (strip_line(x) for x in text.splitlines()) if l.strip()]


def git(*args):
    return subprocess.run(['git', *args], capture_output=True, text=True, check=False)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--base', default='HEAD')
    ap.add_argument('paths', nargs='*', default=['.'])
    a = ap.parse_args()

    changed = git('diff', '--name-only', a.base, '--', *a.paths).stdout.split()
    files = [f for f in changed if f.endswith(('.F90', '.pf'))]
    bad = 0
    for f in files:
        old = git('show', f'{a.base}:{f}')
        if old.returncode != 0:
            print(f'NEW/UNKNOWN {f}: not in {a.base}; not comment-only')
            bad += 1
            continue
        if not pathlib.Path(f).exists():
            print(f'DELETED {f}: not comment-only')
            bad += 1
            continue
        d = list(difflib.unified_diff(strip(old.stdout), strip(open(f).read()),
                                      f'{a.base}:{f}', f, lineterm='', n=1))
        if d:
            bad += 1
            print(f'CODE CHANGED {f}:')
            print('\n'.join(d[:40]))
        else:
            print(f'ok {f}')
    print(f'{len(files)} Fortran file(s) checked, {bad} with non-comment changes')

    for p in a.paths:
        spec = f':(glob){p.rstrip("/")}/*.F90' if pathlib.Path(p).is_dir() else p
        r = git('grep', '-c', 'openspec/changes', '--', spec)
        n = sum(int(x.rsplit(':', 1)[1]) for x in r.stdout.split() if ':' in x)
        print(f'openspec/changes references remaining under {p}: {n}')
    return 1 if bad else 0


if __name__ == '__main__':
    sys.exit(main())
