import csv
import pandas as pd

def read(specs_filename):
    
    def csv_record_reader(csv_reader):
        """ Read a csv reader iterator until a blank line is found. """
        prev_row_blank = True
        for row in csv_reader:
            if not (len(row) == 0):
                if row[0].startswith('#'):
                    continue
                yield row
                prev_row_blank = False
            elif not prev_row_blank:
                return

    specs = {}
    with open(specs_filename, 'r') as specs_file:
        specs_reader = csv.reader(specs_file, skipinitialspace=True)
        while True:
            try:
                gen = csv_record_reader(specs_reader)
                category = next(gen)[0]
                columns = next(gen)
                specs[category] = pd.DataFrame(gen, columns=columns)
            except StopIteration:
                break

    if '*ALIASES*' in specs:
        for alias in specs['*ALIASES*'].to_dict('records'):
            specs['IMPORT'].replace({alias['OPTION']:{alias['ALIAS']:alias['VALUE']}},inplace=True)
            specs['EXPORT'].replace({alias['OPTION']:{alias['ALIAS']:alias['VALUE']}},inplace=True)
            specs['INTERNAL'].replace({alias['OPTION']:{alias['ALIAS']:alias['VALUE']}},inplace=True)

    return specs


