from setuptools import setup, find_packages

setup(
    name="pyresearch",  # Change this to match the folder name
    version="0.1",
    packages=find_packages(),
    install_requires=[
        "pandas",
        "geopandas",
        "matplotlib",
        "python-dotenv",
        "pydantic",
        "pytidycensus",
    ],
)
