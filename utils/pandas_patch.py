# Apply monkeypatch to fix pandas 3.0.3 Copy-on-Write list pop bug in get_acs
import pandas.core.internals.blocks as blocks
from typing import Any, Iterable, Sequence
from pandas.core.internals.blocks import Block

def patched_replace_list(
    self,
    src_list: Iterable[Any],
    dest_list: Sequence[Any],
    inplace: bool = False,
    regex: bool = False,
) -> list[Block]:
    values = self.values
    pairs = [
        (x, y)
        for x, y in zip(src_list, dest_list, strict=True)
        if (self._can_hold_element(x) or (self.dtype == "string" and blocks.is_re(x)))
    ]
    if not pairs:
        return [self.copy(deep=False)]

    src_len = len(pairs) - 1

    if blocks.is_string_dtype(values.dtype):
        na_mask = ~blocks.isna(values)
        masks = (
            blocks.extract_bool_array(
                blocks.compare_or_regex_search(values, s[0], regex=regex, mask=na_mask),
            )
            for s in pairs
        )
    else:
        masks = (blocks.missing.mask_missing(values, s[0]) for s in pairs)
    if inplace:
        masks = list(masks)

    rb = [self]

    for i, ((src, dest), mask) in enumerate(zip(pairs, masks, strict=True)):
        new_rb: list[Block] = []
        for blk_num, blk in enumerate(rb):
            if len(rb) == 1:
                m = mask
            else:
                mib = mask
                assert not isinstance(mib, bool)
                m = mib[blk_num : blk_num + 1]

            result = blk._replace_coerce(
                to_replace=src,
                value=dest,
                mask=m,
                inplace=inplace,
                regex=regex,
            )

            if i != src_len:
                for b in result:
                    if b.refs is self.refs:
                        if b is not self:
                            to_remove = []
                            for ref in self.refs.referenced_blocks:
                                if ref() is b:
                                    to_remove.append(ref)
                            for ref in to_remove:
                                try:
                                    self.refs.referenced_blocks.remove(ref)
                                except ValueError:
                                    pass
                    else:
                        b.refs.referenced_blocks.clear()
            new_rb.extend(result)
        rb = new_rb
    return rb

# Apply monkeypatch on import
blocks.Block.replace_list = patched_replace_list
