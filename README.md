# Rope

Ropes for Swift based on the article by Boehm, Atkinson, and Plass.

# Terminology

zone: a rope may contain zero or more nested spans called zones.
  Sometimes, in my notes and code comments, I use parentheses to denote
  zones.

  *Note*: zones used to be "extents".

  zone controller: every zone has an associated controller that either
    modifies and performs or suppresses rope insertions, deletions, et
    cetera, that overlap the zone.

step: from the exterior of an zone to its interior, from the interior of
  an zone to its exterior, and across one UTF-16 code unit is one step

index: a labeled entity embedded in a rope, or an `Rope.Index struct`
  identifying to such an entity by its label.

  adjacent: two indices are adjacent if no *steps* separate them
  alias: two indices *alias* one another if they are adjacent
  label: an object that uniquely identifies an index
  expired: each embedded index holds a weak reference to its label; an
    index whose reference turns to `nil` is *expired*.  Expired indices
    are cleaned up by occasional garbage-collecting sweeps.
