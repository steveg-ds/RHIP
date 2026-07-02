import pandas as pd
import pytest
import numpy as np
from utils.acs_loader import CensusDataLoader

def test_clean_data_low_units():
    # Test filtering logic for low unit counts
    data = {'unit_count': [5, 15, 20, 2], 'value': [100, 200, 300, 400]}
    df = pd.DataFrame(data)
    loader = CensusDataLoader()
    
    cleaned_df = loader.clean_data(df, unit_threshold=10)
    
    assert len(cleaned_df) == 2
    assert all(cleaned_df['unit_count'] >= 10)

def test_clean_data_outliers():
    # Test outlier removal (Z-score > 3)
    # Create data with an extreme outlier
    data = {'value': [10, 12, 11, 13, 10, 1000]} # 1000 is > 3 std devs
    df = pd.DataFrame(data)
    loader = CensusDataLoader()
    
    cleaned_df = loader.clean_data(df, unit_threshold=0) # Skip unit filter
    
    # 1000 should be removed
    assert len(cleaned_df) == 5
    assert 1000 not in cleaned_df['value'].values

def test_clean_data_housing_quality():
    # Test binary housing quality flag logic
    # Assume housing quality is based on some logic, e.g., if 'feature' > 50
    data = {'feature': [10, 60, 40, 80]}
    df = pd.DataFrame(data)
    loader = CensusDataLoader()
    
    cleaned_df = loader.clean_data(df, unit_threshold=0)
    
    assert 'housing_quality' in cleaned_df.columns
    # Check that quality flag is binary (0 or 1)
    assert set(cleaned_df['housing_quality'].unique()) <= {0, 1}
