//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
public struct LabelSet : SetAlgebra, ExpressibleByArrayLiteral {
	public typealias Element = Label.Id
	var content: Set<Element>
	var _cursorCount: Int = 0
	var _extentCount: Int = 0
	var _indexCount: Int = 0
	public var cursorCount: Int { return _cursorCount }
	public var extentCount: Int { return _extentCount }
	public var indexCount: Int { return _indexCount }
	mutating func decreaseCounts(for e: Element) {
		switch e {
		case .cursor(_):
			_cursorCount -= 1
		case .extent(_):
			_extentCount -= 1
		case .index(_):
			_indexCount -= 1
		}
	}
	mutating func increaseCounts(for e: Element) {
		switch e {
		case .cursor(_):
			_cursorCount += 1
		case .extent(_):
			_extentCount += 1
		case .index(_):
			_indexCount += 1
		}
	}
	mutating func resetCounts() {
		_cursorCount = 0
		_extentCount = 0
		_indexCount = 0
	}
	mutating func recomputeCounts() {
		resetCounts()
		for e in content {
			increaseCounts(for: e)
		}
	}
	public init() {
		content = Set<Element>()
	}
	init(with content: Set<Element>) {
		self.content = content
		for e in self.content {
			increaseCounts(for: e)
		}
	}
	public init(arrayLiteral elts: Element...) {
		content = Set<Element>(elts)
		for e in elts {
			increaseCounts(for: e)
		}
	}
	public init<S>(_ sequence: S)
	    where S : Sequence, Self.Element == S.Element {
		content = Set<Element>(sequence)
		for e in sequence {
			increaseCounts(for: e)
		}
	}
	public func contains(_ e: Self.Element) -> Bool {
		return content.contains(e)
	}
	public mutating func insert(_ e: Self.Element)
	    -> (inserted: Bool, memberAfterInsert: Self.Element) {
		let result = content.insert(e)
		if case (true, _) = result {
			increaseCounts(for: e)
		}
		return result
	}
	public mutating func update(with e: Self.Element) -> Self.Element? {
		if let old = content.update(with: e) {
			return old
		}
		increaseCounts(for: e)
		return nil
	}
	public mutating func remove(_ e: Self.Element) -> Self.Element? {
		guard let old = content.remove(e) else {
			return nil
		}
		decreaseCounts(for: old)
		return old
	}
	public func union(_ other: Self) -> Self {
		return Self(with: content.union(other.content))
	}
	public mutating func formUnion(_ other: Self) {
		content.formUnion(other.content)
		recomputeCounts()
	}
	public func intersection(_ other: Self) -> Self {
		return Self(with: content.intersection(other.content))
	}
	public mutating func formIntersection(_ other: Self) {
		content.formIntersection(other.content)
		recomputeCounts()
	}
	public func symmetricDifference(_ other: Self) -> Self {
		return Self(with: content.symmetricDifference(other.content))
	}
	public mutating func formSymmetricDifference(_ other: Self) {
		content.formSymmetricDifference(other.content)
		recomputeCounts()
	}
	public func isStrictSubset(of other: Self) -> Bool {
		return content.isStrictSubset(of: other.content)
	}
	public func isStrictSuperset(of other: Self) -> Bool {
		return content.isStrictSuperset(of: other.content)
	}
	public var isEmpty: Bool {
		return content.isEmpty
	}
	public func isDisjoint(with other: Self) -> Bool {
		return content.isDisjoint(with: other.content)
	}
	public func isSubset(of other: Self) -> Bool {
		return content.isSubset(of: other.content)
	}
	public func isSuperset(of other: Self) -> Bool {
		return content.isSuperset(of: other.content)
	}
	public mutating func subtract(_ other: Self) {
		content.subtract(other.content)
		recomputeCounts()
	}
	public func subtracting(_ other: Self) -> Self {
		return Self(with: content.subtracting(other.content))
	}
}
