//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
public enum RopeIndex<C : Content> : Comparable {
case start(of: Rope<C>)
case end(of: Rope<C>)
case interior(of: Rope<C>, at: UInt64, index: UInt64, handle: Handle)
}

extension RopeIndex {
	init(utf16Offset i: Int, in rope: Rope<C>) {
		let h = Handle()
		rope.node = rope.node.insertingIndex(h, at: i)
		self = .interior(of: rope, at: 0, index: 0, handle: h)
	}
}

enum RopeIndexComparisonError : Error {
case MismatchedOwners
}

extension RopeIndex {
        public var owner: Rope<C> {
		switch self {
		case .start(let r), .end(let r), .interior(let r, _, _, _):
			return r
		}
	}
}

public func == <C>(_ l: RopeIndex<C>, _ r: RopeIndex<C>) -> Bool {
	do {
		return try l.equals(r)
	} catch {
		fatalError("Not comparable")
	}
}

public func < <C>(_ l: RopeIndex<C>, _ r: RopeIndex<C>) -> Bool {
	do {
		return try l.isLessThan(r)
	} catch {
		fatalError("Not comparable")
	}
}

extension RopeIndex {
        public func equals<C>(_ other: RopeIndex<C>) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		switch (self, other) {
		case (.start(_), .start(_)), (.end(_), .end(_)):
			return true
		case (.interior(_, _, _, let h), .interior(_, _, _, let j))
		    where h == j:
			return true
		default:
			return false
		}
	}
	public func isLessThan<C>( _ other: RopeIndex<C>) throws -> Bool {
		guard self.owner === other.owner else {
			throw RopeIndexComparisonError.MismatchedOwners
		}
		switch (self, other) {
		case (.start(_), .start(_)):
			return false
		case (.start(_), _):
			return true
		case (.end(_), .end(_)):
			return false
		case (_, .end(_)):
			return true
		case (.interior(_, _, _, let h1),
		      .interior(_, _, _, let h2)):
			return self.owner.containsIndex(h1, before: h2)
		default:
			return false
		}
	}
}
