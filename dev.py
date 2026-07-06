from utils import CensusDataLoader

if __name__ == "__main__":
    from os import getenv

    from dotenv import load_dotenv

    load_dotenv()

    x = CensusDataLoader(api_key=getenv("CENSUS_API_KEY"))

    poverty_vars = [
        "B17001_001E",  # Total
        "B17001_002E",  # Income in the past 12 months
        # below poverty level
    ]
    # print(x.fetch(poverty_vars).head())

    try:
        print(x.fetch(variables={"Population": "B01001_001E"}))
    except Exception as e:
        print(f"Oopsie: {e}")
