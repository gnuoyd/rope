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
case mismatchedOwners
case indexNotFound
}

extension Rope.Index {
        public var owner: Rope {
		switch self {
		case .end(let r), .interior(let r, _):
			return r
		}
	}
	public var label: Label {
		switch self {
		case .end(let r):
			return r._endLabel
		case .interior(_, let label):
			return label
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
			throw RopeIndexComparisonError.mismatchedOwners
		}
		switch (self, other) {
		case (.end(_), .end(_)):
			return true
		case (.end(_), .interior(_, let h)),
		     (.interior(_, let h), .end(_)):
			let follows = try self.owner.steps(follow: h)
			return !follows
		case (.interior(_, let h), .interior(_, let j)):
			return try self.owner.label(h, aliases: j)
		}
	}
	public func isLessThan(_ other: Self) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.mismatchedOwners
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
		case (.end(_), .end(_)):
			return false
		case (.interior(_, let h), .end(_)):
			return try steps(follow: h)
		case (.interior(_, let h1),
		      .interior(_, let h2)):
			return try h1 != h2 && step(h1, precedes: h2)
		default:
			return false
		}
	}
}
