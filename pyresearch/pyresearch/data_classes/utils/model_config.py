from pydantic import ConfigDict

MODEL_CONFIG = ConfigDict(
    str_strip_whitespace=True,
    populate_by_name=True,
    validate_assignment=True,
    arbitrary_types_allowed=True,
    validate_return=True,
)
