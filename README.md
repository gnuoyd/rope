# Rope
Ropes for Swift based on the article by Boehm, Atkinson, and Plass.

# Terminology

extent: a rope may contain zero or more nested spans called extents.
        Sometimes, in my notes and code comments, I use parentheses to
        denote extents.

	*Note*: Someday I will rename extents "zones".

        extent controller: every extent has an associated controller
            that either modifies and performs or suppresses rope
            insertions, deletions, et cetera, that overlap the extent.

step: from the exterior of an extent to its interior, from the interior
        of an extent to its exterior, and across one UTF-16 code unit is
        one step

index: a labeled entity embedded in a rope, or an `Rope.Index struct`
        identifying to such an entity by its label.

	adjacent: two indices are adjacent if no *steps* separate them
	alias: two indices *alias* one another if they are adjacent
	label: an object that uniquely identifies an index
        expired: each embedded index holds a weak reference to
            its label; an index whose reference turns to `nil` is
            *expired*.  Expired indices are cleaned up by occasional
            garbage-collecting sweeps.
