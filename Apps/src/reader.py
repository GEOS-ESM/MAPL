import csv
import pandas as pd

def read_specs(specs_filename):
    
    def csv_record_reader(csv_reader):
        """ Read a csv reader iterator until a blank line is found. """
        prev_row_blank = True
        for row in csv_reader:
            if not (len(row) == 0):
                if row[0].startswith('#'):
                    continue
                yield [cell.strip() for cell in row]
                prev_row_blank = False
            elif not prev_row_blank:
                return

    column_aliases = {
        'NAME'      : 'short_name',
        'LONG NAME' : 'long_name',
        'VLOC'      : 'vlocation',
        'UNITS'     : 'units',
        'DIMS'      : 'dims',
        'UNGRIDDED' : 'ungridded_dims',
        'PREC'      : 'precision',
        'COND'      : 'condition'
    }

    specs = {}
    with open(specs_filename, 'r') as specs_file:
        specs_reader = csv.reader(specs_file, skipinitialspace=True,delimiter='|')
        gen = csv_record_reader(specs_reader)
        schema_version = next(gen)[0]
        print("version: ",schema_version)
        component = next(gen)[0]
        print("component: ",component)
        while True:
            try:
                gen = csv_record_reader(specs_reader)
                category = next(gen)[0].split()[1]
                bare_columns = next(gen)
                bare_columns = [c.strip() for c in bare_columns]
                columns = []
                for c in bare_columns:
                    if c in column_aliases:
                        columns.append(column_aliases[c])
                    else:
                        columns.append(c)
                specs[category] = pd.DataFrame(gen, columns=columns)
            except StopIteration:
                break

    entry_aliases = {'z'    : 'MAPL_DimsVertOnly',
                     'z*'   : 'MAPL_DimsVertOnly',
                     'xy'   : 'MAPL_DimsHorzOnly',
                     'xy*'  : 'MAPL_DimsHorzOnly',
                     'xyz'  : 'MAPL_DimsHorzVert',
                     'xyz*' : 'MAPL_DimsHorzVert',
                     'C'    : 'MAPL_VlocationCenter',
                     'E'    : 'MAPL_VlocationEdge',
                     'N'    : 'MAPL_VlocationNone'
    }

    specs['IMPORT'].replace(entry_aliases,inplace=True)
    specs['EXPORT'].replace(entry_aliases,inplace=True)
    specs['INTERNAL'].replace(entry_aliases,inplace=True)

    return specs


