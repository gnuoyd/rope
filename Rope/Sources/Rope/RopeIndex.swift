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
		case .interior(let r, _):
			return r
		}
	}
	public var label: Label {
		switch self {
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
		return try self.owner.label(label, aliases: other.label)
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
		return try l.label != r.label &&
	                   step(l.label, precedes: r.label)
	}
}
