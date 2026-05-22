#!/usr/bin/env python3
"""
Rename MAPL internal modules from mapl_<Name>[Mod] to mapl_<Name>_mod convention.

Rules:
- Match module names (case-insensitively) starting with mapl_ or MAPL_
- Strip trailing 'Mod' (but NOT '_mod') suffix from the name part
- Already-correct names ending in _mod are skipped
- Module *definitions* are only renamed in non-test files
- `use` statements are updated everywhere
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.abspath(__file__))

# Patterns for test paths to exclude from definition renaming
TEST_PATH_PATTERNS = [
    '/tests/',
    '/test/',
]
TEST_FILE_PATTERNS = [
    'Test.F90', 'Test.f90',
    '_test.F90', '_test.f90',
    'pfunit', 'pFUnit',
]

def is_test_file(path):
    for p in TEST_PATH_PATTERNS:
        if p in path:
            return True
    basename = os.path.basename(path)
    for p in TEST_FILE_PATTERNS:
        if p.lower() in basename.lower():
            return True
    return False

def compute_new_name(old_name):
    """
    Given an old module name like MAPL_FooMod or mapl_Foo,
    return the new name mapl_Foo_mod.
    Returns None if already correct or not a mapl_ module.
    """
    # Must start with mapl_ (case-insensitive)
    if not old_name.lower().startswith('mapl_'):
        return None
    
    # Extract the part after mapl_
    name_part = old_name[5:]  # strip mapl_ prefix
    
    # If already ends with _mod (case insensitive), it's already correct
    if name_part.lower().endswith('_mod'):
        return None
    
    # Strip trailing 'Mod' suffix (but only if it's exactly 'Mod' at the end,
    # not something like 'Module')
    if name_part.endswith('Mod'):
        name_part = name_part[:-3]
    elif name_part.lower().endswith('mod') and not name_part.endswith('_mod'):
        # e.g. 'FOOMOD' -> strip 'MOD' -> 'FOO'
        # Only strip if it's a pure 'Mod' suffix (case variations)
        name_part = name_part[:-3]
    
    new_name = 'mapl_' + name_part + '_mod'
    
    # Don't rename to itself
    if new_name.lower() == old_name.lower():
        return None
    
    return new_name


def collect_modules(root):
    """
    Walk the source tree and collect all mapl_* module definitions.
    Returns dict: old_name_lower -> new_name
    """
    module_map = {}  # old_name_lower -> new_name
    old_names = {}   # old_name_lower -> old_name (for reporting)
    
    module_re = re.compile(r'^\s*module\s+(mapl_\S+)', re.IGNORECASE)
    
    for dirpath, dirnames, filenames in os.walk(root):
        # Skip build directories
        dirnames[:] = [d for d in dirnames if d not in (
            'nag-ninja', 'gfortran-ninja', 'intel-ninja', '.git', 'build'
        )]
        for fname in filenames:
            if not fname.endswith(('.F90', '.f90')):
                continue
            fpath = os.path.join(dirpath, fname)
            if is_test_file(fpath):
                continue
            try:
                with open(fpath, 'r', errors='replace') as f:
                    for line in f:
                        m = module_re.match(line)
                        if m:
                            old = m.group(1).rstrip()
                            new = compute_new_name(old)
                            if new:
                                key = old.lower()
                                module_map[key] = new
                                old_names[key] = old
            except Exception as e:
                print(f"WARNING: could not read {fpath}: {e}", file=sys.stderr)
    
    return module_map, old_names


def apply_renames(root, module_map):
    """
    Apply renames to all .F90 files.
    - Module definitions (module X / end module X) only in non-test files
    - use statements in all files
    """
    # Build regex: match any of the old names (case-insensitive)
    # We need to match whole module names (word boundaries)
    
    # Sort by length descending to avoid partial replacements
    old_names_sorted = sorted(module_map.keys(), key=len, reverse=True)
    
    changed_files = []
    
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in (
            'nag-ninja', 'gfortran-ninja', 'intel-ninja', '.git', 'build'
        )]
        for fname in filenames:
            if not fname.endswith(('.F90', '.f90')):
                continue
            fpath = os.path.join(dirpath, fname)
            test_file = is_test_file(fpath)
            
            try:
                with open(fpath, 'r', errors='replace') as f:
                    content = f.read()
            except Exception as e:
                print(f"WARNING: could not read {fpath}: {e}", file=sys.stderr)
                continue
            
            original = content
            
            # Process line by line for precision
            lines = content.split('\n')
            new_lines = []
            changed = False
            
            for line in lines:
                stripped = line.lstrip()
                lower_stripped = stripped.lower()
                
                # Detect line type
                is_module_def = lower_stripped.startswith('module ') and not lower_stripped.startswith('module procedure')
                is_end_module = lower_stripped.startswith('end module')
                is_use = lower_stripped.startswith('use ')
                
                if (is_module_def or is_end_module) and not test_file:
                    # Replace module name in definition/end
                    new_line = replace_module_name_in_line(line, module_map)
                    if new_line != line:
                        changed = True
                    new_lines.append(new_line)
                elif is_use:
                    # Replace module name in use statement (all files)
                    new_line = replace_use_name_in_line(line, module_map)
                    if new_line != line:
                        changed = True
                    new_lines.append(new_line)
                else:
                    new_lines.append(line)
            
            if changed:
                new_content = '\n'.join(new_lines)
                with open(fpath, 'w') as f:
                    f.write(new_content)
                changed_files.append(fpath)
    
    return changed_files


def replace_module_name_in_line(line, module_map):
    """Replace module name in a module/end module definition line."""
    # Match: module <name> or end module <name>
    pattern = re.compile(
        r'((?:end\s+)?module\s+)(mapl_\S+)',
        re.IGNORECASE
    )
    def replacer(m):
        prefix = m.group(1)
        name = m.group(2).rstrip()
        # Check for trailing comment/stuff
        rest = m.group(2)[len(name):]
        key = name.lower()
        if key in module_map:
            return prefix + module_map[key] + rest
        return m.group(0)
    
    return pattern.sub(replacer, line)


def replace_use_name_in_line(line, module_map):
    """Replace module name in a use statement."""
    # Match: use <name> or use <name>, only: ...
    # Handle: use :: <name> as well
    pattern = re.compile(
        r'(use\s+(?:::\s*)?)(mapl_[a-z0-9_]+)(\b)',
        re.IGNORECASE
    )
    def replacer(m):
        prefix = m.group(1)
        name = m.group(2)
        suffix = m.group(3)
        key = name.lower()
        if key in module_map:
            return prefix + module_map[key] + suffix
        return m.group(0)
    
    return pattern.sub(replacer, line)


def main():
    print(f"Scanning {ROOT} for mapl_* module definitions...")
    module_map, old_names = collect_modules(ROOT)
    
    print(f"Found {len(module_map)} modules to rename:")
    for key in sorted(module_map.keys()):
        print(f"  {old_names[key]} -> {module_map[key]}")
    
    print(f"\nApplying renames...")
    changed = apply_renames(ROOT, module_map)
    
    print(f"\nModified {len(changed)} files:")
    for f in sorted(changed):
        print(f"  {f}")
    
    print("\nDone.")


if __name__ == '__main__':
    main()
