from pydantic.dataclasses import dataclass
from typing import ClassVar, List


@dataclass
class RentBurden:
    TOTAL: ClassVar[str] = "B25070_001E"
    RENT_30_35: ClassVar[str] = "B25070_007E"
    RENT_35_40: ClassVar[str] = "B25070_008E"
    RENT_40_50: ClassVar[str] = "B25070_009E"
    RENT_50_PLUS: ClassVar[str] = "B25070_010E"

    @classmethod
    def all_vars(cls) -> List[str]:
        return [cls.TOTAL, cls.RENT_30_35, cls.RENT_35_40, cls.RENT_40_50, cls.RENT_50_PLUS]

    @classmethod
    def burdened_vars(cls) -> List[str]:
        return [cls.RENT_30_35, cls.RENT_35_40, cls.RENT_40_50, cls.RENT_50_PLUS]


@dataclass
class OwnerBurden:
    TOTAL: ClassVar[str] = "B25091_001E"
    MORT_30_35: ClassVar[str] = "B25091_008E"
    MORT_35_40: ClassVar[str] = "B25091_009E"
    MORT_40_50: ClassVar[str] = "B25091_010E"
    MORT_50_PLUS: ClassVar[str] = "B25091_011E"
    NO_MORT_30_35: ClassVar[str] = "B25091_019E"
    NO_MORT_35_40: ClassVar[str] = "B25091_020E"
    NO_MORT_40_50: ClassVar[str] = "B25091_021E"
    NO_MORT_50_PLUS: ClassVar[str] = "B25091_022E"

    @classmethod
    def all_vars(cls) -> List[str]:
        return [
            cls.TOTAL,
            cls.MORT_30_35, cls.MORT_35_40, cls.MORT_40_50, cls.MORT_50_PLUS,
            cls.NO_MORT_30_35, cls.NO_MORT_35_40, cls.NO_MORT_40_50, cls.NO_MORT_50_PLUS,
        ]

    @classmethod
    def burdened_vars(cls) -> List[str]:
        return [
            cls.MORT_30_35, cls.MORT_35_40, cls.MORT_40_50, cls.MORT_50_PLUS,
            cls.NO_MORT_30_35, cls.NO_MORT_35_40, cls.NO_MORT_40_50, cls.NO_MORT_50_PLUS,
        ]


@dataclass
class Poverty:
    TOTAL: ClassVar[str] = "B17001_001E"
    POVERTY: ClassVar[str] = "B17001_002E"


@dataclass
class HousingQuality:
    TOTAL_PLUMBING: ClassVar[str] = "B25047_001E"
    LACK_PLUMBING: ClassVar[str] = "B25047_003E"
    TOTAL_KITCHEN: ClassVar[str] = "B25051_001E"
    LACK_KITCHEN: ClassVar[str] = "B25051_003E"


@dataclass
class Education:
    TOTAL: ClassVar[str] = "B15003_001E"
    GRADES: ClassVar[List[str]] = [f"B15003_{num:03d}E" for num in range(2, 17)]


@dataclass
class Income:
    GINI: ClassVar[str] = "B19083_001E"


@dataclass
class GeographicMobility:
    TOTAL: ClassVar[str] = "B07012_002E"
    SAME_HOUSE: ClassVar[str] = "B07012_006E"
