---
aliases: []
tags: []
---

# RHIP



## IDK

I just learned that the 2020 decennial census census tracts set for ACS geographic areas starting in 2022 (see Feb 7 of this news release for more info https://www.census.gov/programs-surveys/acs/news/data-releases/2020/release.html). This only has real implications for using census tracts.  There are relationship files to go from 2010 census tracts to 2020 census tracts (https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html).

## Rural Detention Project
- [-] write class `ViraDataLoader` in `utils/vira_loader.py` (needs to be added to local git repo) and needs to process the Vira data similar to how its done in `notebooks/DataProcessing.ipynb`
- Use `utils/ers_loader` to get RUCC data 
- Use `utils/acs_loader` to get 5 year ACS data for 2013, 2018, and 2023.
- Use `utils/vira_loader.py` to get Vira ICE incarceration data for 2013, 2018, and 2023.
- 
