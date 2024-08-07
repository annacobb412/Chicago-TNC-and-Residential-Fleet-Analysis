import os
import pandas as pd
import re

# Path to the directory containing the text files
txt_files_dir = 'Illinois Hybrid Registrations TXTs'
# Path to the CSV file containing Chicago zip codes
chicago_zip_codes_csv = 'Chicago_Zip_Codes.csv'
# Output CSV file name
out_name = 'Chicago_Hybrid_Count.csv'

# Read Chicago zip codes from the CSV file
chicago_zip_codes = pd.read_csv(chicago_zip_codes_csv)['ZIP'].astype(str).tolist()

# Initialize an empty dataframe to hold the electric vehicle counts
ev_counts_df = pd.DataFrame()

# Function to extract date from file name
def extract_date_from_filename(filename):
    date_str = filename[-10:-4]  # Assuming the date is the last 6 characters before the extension
    return pd.to_datetime(date_str, format='%m%d%y')

# Function to parse the electric vehicle counts from a text file
def parse_ev_counts_from_txt_file(txt_file_path):
    with open(txt_file_path, 'r') as file:
        lines = file.readlines()

    date = extract_date_from_filename(os.path.basename(txt_file_path))
    ev_counts = {}

    # Parse the lines to extract zip code counts
    is_zipcode_section = False
    for line in lines:
        if 'ZIPCODE TOTALS' in line:
            is_zipcode_section = True
            continue

        if is_zipcode_section:
            match = re.match(r'^\s*\D+\s+(\d+)\s+(\d+)\s*$', line)
            if match:
                zipcode = match.group(1)
                count = int(match.group(2))
                if zipcode in chicago_zip_codes:
                    ev_counts[zipcode] = count

    return date, ev_counts

# Process each text file and collect the data
for txt_file in os.listdir(txt_files_dir):
    if txt_file.endswith('.txt'):
        txt_file_path = os.path.join(txt_files_dir, txt_file)
        date, ev_counts = parse_ev_counts_from_txt_file(txt_file_path)
        
        if ev_counts:
            date_column = pd.Series(ev_counts, name=date)
            ev_counts_df = pd.concat([ev_counts_df, date_column], axis=1)

# Ensure the dataframe has the Chicago zip codes as rows
ev_counts_df = ev_counts_df.T
ev_counts_df.index.name = 'Date'
ev_counts_df = ev_counts_df.reindex(columns=chicago_zip_codes)

# Save the resulting dataframe to a CSV file
ev_counts_df.to_csv(out_name)

