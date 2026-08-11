#!/usr/bin/env python3
from argparse import ArgumentParser
import sys
import re
from enum import StrEnum

FILETYPES = 'add declare get'
FileType = StrEnum('FileType', FILETYPES.upper().split()) 
LineType = Flag("LineType", "NOTYPE FIRST LAST CONTINUED CONTINUATION".split)
QUOTE_REGEX = r''' ('.*')|(".*") '''.strip()
COMMENT_REGEX = r'!.*$'
CONTINUATION_REGEX = r'^\s*&'
CONTINUE_REGEX = r'^.*&.*'
continue_pattern = re.compile(CONTINUE_REGEX, re.I)
PROCEDURE_NAMES = {
        FileType.ADD: 'MAPL_GridCompAddSpec',
        FileType.GET: 'MAPL_StateGetPointer',
        FileType.DECLARE: ''
        }

def classify_line(line, previous=None):
    linetype = LineType.NOTYPE
    if previous is None:
        linetype += LineType.FIRST
    elif previous == LineType.LAST:
        linetype += LineType.FIRST
    if not continue_pattern.search(line) 


def trim_lines(lines):
    return [a for a in (line.strip() for line in lines) if a]

def strip_comment(line):
    r'^.*'
def stitch(lines):

def parse_file(filename, filetype, basedir):
    p = Path(basedir + filename)
    lines = []
    with p.open(r'r') as f:
        lines = f.read()
    lines = trim_lines(lines)
    procedure_name = PROCEDURE_NAMES.get(filetype)
    return read_procedures(lines, procedure_name) if procedure_name else read_declarations(lines) 

def get_args(argv=None):
    parser = ArgumentParser()
    parser.add_argument('left')
    parser.add_argument('right')
    parser.add_argument(r'-t', r'--type', choices=FILETYPES.split())
    parser.add_argument(r'-b', r'--base-dir', default=r'.')
    args = parse.parse_args(argv) if argv else parse.parse_args()
    argd = vars(args)
    if args.type:
       argd['type'] = FileType[args.type.upper()] 
    return argd

def main(argv=None):
    argd = get_args(argv)
    filetype = argd['type']
    basedir = argd['base_dir']
    if not basedir.endswith(r'/'):
        basedir = basedir + r'/'
    left = parse_file(argd['left'], filetype, basedir)
    right = parse_file(args['right'], filetype, basedir)
    compare(left, right)

if __name__ == '__main__':
    rc = main(sys.argv)
