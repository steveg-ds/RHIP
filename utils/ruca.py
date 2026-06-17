import pandas as pd

def collect_ruca_data() -> pd.DataFrame:
    """Downloads and cleans the 2020 Rural-Urban Commuting Area (RUCA) codes.
    
    Loads Census API keys, reads the RUCA dataset directly from ERS USDA,
    standardizes column names, and filters to continental US census tracts.
    
    Returns:
        pd.DataFrame: Cleaned RUCA dataframe with columns:
            - TractFIPS: 2020 Census Tract FIPS code (str)
            - TractName: Tract description (str)
            - CountyFIPS: County FIPS code (str)
            - CountyName: County name (category)
            - StateFIPS: State FIPS code (str)
            - StateName: State name (category)
            - RUCA: Primary RUCA code (int8)
    """


    RUCA_URL: str = "https://www.ers.usda.gov/media/5443/2020-rural-urban-commuting-area-codes-census-tracts.csv?v=48133"

    cols_to_load = [
        'TractFIPS20', 'TractName20', 'CountyFIPS20', 'CountyName20', 
        'StateFIPS20', 'StateName20', 'PrimaryRUCA'
    ]

    dtypes = {
        'TractFIPS20': str,        # Preserve leading zeros 
        'CountyFIPS20': str,
        'StateFIPS20': str,
        'StateName20': 'category',
        'CountyName20': 'category',
        'PrimaryRUCA': 'int8'
    }

    ruca: pd.DataFrame = pd.read_csv(
        RUCA_URL, 
        encoding='latin1', 
        usecols=cols_to_load, 
        dtype=dtypes
    )

    print(f"Total # of census tracts: {len(ruca)}")

    ruca.columns = ruca.columns.str.replace('20$', '', regex=True)
    ruca.rename(columns={"PrimaryRUCA": "RUCA"}, inplace=True)
        
    # Remove non-continental US CTs
    non_continental = {
        'Alaska', 'Hawaii', 'American Samoa', 'Guam', 
        'Commonwealth of the Northern Mariana Islands', 'Puerto Rico', 
        'United States Virgin Islands'
    }

    ruca = ruca[~ruca['StateName'].isin(non_continental)].reset_index(drop=True)
    print(f"# of continental census tracts: {len(ruca)}")

    return ruca

if __name__ == "__main__":
    ruca_df = collect_ruca_data()
    print(ruca_df.head())
