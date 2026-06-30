import pytidycensus as tc
import pandas as pd
import os
from dotenv import load_dotenv

load_dotenv()
api_key = os.getenv("CENSUS_API_KEY")
if api_key:
    tc.set_census_api_key(api_key)

def search_vars(query, year=2019):
    print(f"Searching for '{query}' in {year} ACS5...")
    v = tc.load_variables(year, "acs5")
    matches = v[v['label'].str.contains(query, case=False) | v['concept'].str.contains(query, case=False)]
    return matches[['name', 'label', 'concept']]

if __name__ == "__main__":
    # Check occupied status for quality variables
    print(search_vars("B25047")) # Plumbing
    print(search_vars("B25051")) # Kitchen
