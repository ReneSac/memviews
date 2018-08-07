The *memviews* module implements a very thin wrapper around array like
types in Nim or any contiguous memory region you have a pointer to.
It can be used to make shallow slices of your data, have a uniform interface
to both seq and string (among other types) and acess C arrays more naturally
and with optional bounds checking.

Be carefull with the lifetime of the allocations you are viewing into, as
the view possess only an unmanaged pointer to the data. If the GC collects
your data, or you explicily deallocate it, this pointer may become invalid
and undefined behaviour ensues. You may have to use GC_ref() and GC_unref()
to manually stick the memory around.

The same may happen if the data is reallocated to another point in the
memory. For example, when a sequence has to grow as a result of ".add()".
One should abstain to change the original seq in those ways.

MemViews don't suport automatic growth as it don't owns the data it is
pointing to. However, you can mutate any point of this memory.

It is similar to Adrian Veith *seqslices*, but much simpler, less safe and
more general.

