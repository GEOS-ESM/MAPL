"""This file act as a bypass importer for modules that triggers IEEE FP traps
during import.
WARNING: This should never be used for normal imports and each new entry should be
        flagged with their IEEE issue"""

try:
    import numpy  # invalid overflow
except ModuleNotFoundError:
    pass

try:
    import yaml  # invalid overflow
except ModuleNotFoundError:
    pass

try:
    import mpmath  # invalid overflow
except ModuleNotFoundError:
    pass
