//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
extension Rope.Index {
	init(abutting offset: Rope.Node.Offset, on side: Rope.Node.Side,
	     in rope: Rope) {
		let h = Label()
		rope.node = rope.node.inserting(index: h, abutting: side,
		    of: offset)
		self = .interior(of: rope, label: h)
	}
	init(unitOffset offset: Rope.Node.Offset, in rope: Rope) {
		let h = Label()
		rope.node = rope.node.inserting(index: h, at: offset)
		self = .interior(of: rope, label: h)
	}
}

enum RopeIndexComparisonError : Error {
case MismatchedOwners
case IndexNotFound
}

extension Rope.Index {
        public var owner: Rope {
		switch self {
		case .start(let r), .end(let r), .interior(let r, _):
			return r
		}
	}

	public static func == (_ l: Self, _ r: Self) -> Bool {
		do {
			return try l.aliases(r)
		} catch {
			fatalError("Not comparable")
		}
	}
	public static func < (_ l: Self, _ r: Self) -> Bool {
		do {
			return try l.isLessThan(r)
		} catch {
			fatalError("Not comparable")
		}
	}
}

extension Rope.Index {
        public func aliases(_ other: Self) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		switch (self, other) {
		case (.start(_), .start(_)), (.end(_), .end(_)):
			return true
		case (.start(_), .interior(_, let h)),
		     (.interior(_, let h), .start(_)):
			guard let precedingExist =
			    self.owner.steps(precede: h) else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return !precedingExist
		case (.end(_), .interior(_, let h)),
		     (.interior(_, let h), .end(_)):
			guard let followingExist =
			    self.owner.steps(follow: h) else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return !followingExist
		case (.interior(_, let h), .interior(_, let j))
		    where h == j:
			return true
		case (.interior(_, let h1), .interior(_, let h2)):
			guard let precedes = self.owner.step(h1, precedes: h2),
			      let follows = self.owner.step(h2, precedes: h1)
			      else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return !precedes && !follows
		default:
			return false
		}
	}
	public func isLessThan(_ other: Self) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		return try self.owner.step(self, precedes: other)
	}
}

extension Rope {
	public func step(_ l: Rope.Index, precedes r: Rope.Index)
	    throws -> Bool {
		return try node.step(l, precedes: r)
	}
}

extension Rope.Node {
	public func step(_ l: Rope.Index, precedes r: Rope.Index)
	    throws -> Bool {
		switch (l, r) {
		case (.start(_), .start(_)):
			return false
		case (.start(_), .interior(_, let h)):
			guard let precede = steps(precede: h) else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return precede
		case (.start(_), .end(_)):
			return !hasSingleIndex
		case (.end(_), .end(_)):
			return false
		case (.interior(_, let h), .end(_)):
			guard let follow = steps(follow: h) else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return follow
		case (.interior(_, let h1),
		      .interior(_, let h2)):
			guard let precedes = step(h1, precedes: h2) else {
				throw RopeIndexComparisonError.IndexNotFound
			}
			return precedes
		default:
			return false
		}
	}
}
