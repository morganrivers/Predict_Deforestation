import os
import csv

output_file = 'csv_previews.txt'

with open(output_file, 'w', encoding='utf-8') as out_f:
    for filename in os.listdir('.'):
        if filename.endswith('.csv'):
            out_f.write(f'--- {filename} ---\n')
            try:
                with open(filename, 'r', encoding='utf-8') as csv_file:
                    reader = csv.reader(csv_file)
                    for i, row in enumerate(reader):
                        if i >= 10:
                            break
                        out_f.write(','.join(row) + '\n')
            except Exception as e:
                out_f.write(f'Error reading {filename}: {e}\n')
            out_f.write('\n')
