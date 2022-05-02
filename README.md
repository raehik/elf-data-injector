# ELF data injector
Inject data into ELFs and patch over references to use them.

Intended primarily for the awkward task of replacing a string literal with an
arbitrarily longer one. Replacing a string with another string of the same
shorter length is easy; longer is challenging. This tool appends a new segment
to the ELF, amends headers and offsets, and patches offsets to replace
references to the old data with references to the new data.

## Rough notes
  * Lay out data and create blablabla

  * PH: Increment file offsets. Everything shifts by `e_phentsize * 1 = 0x20`.
  * PH: Insert new segment *before* the weird empty one. If it comes after,
    raehik's device immediately reboots.
    * `p_flags` is a bitmask. 4 is R, 5 is RW. Both work. Make it read-only,
      until we need to add our own global data for some reason.
    * Need to know total size, vaddr
  * FH: Increment `e_phnum`.

## To-dos
  * I talk about paddr where ELF talks about offset. I don't mean paddr in
    memory, I mean in ELF file. That's bad, should fix
