//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
public protocol UnitOffsetableIndex {
	init<S>(unitOffset offset: Int, in s: S)
	    where S : UnitViewableContent, S.Index == Self
	func unitOffset<S>(in s: S) -> Int
	    where S : UnitViewableContent, S.Index == Self
}

public extension UnitOffsetableIndex {
	init<S>(unitOffset offset: Int, in s: S)
	    where S : UnitViewableContent, S.Index == Self {
		self = s.index(unitOffset: offset)
	}
	func unitOffset<S>(in s: S) -> Int
	    where S : UnitViewableContent, S.Index == Self {
		return s.unitOffset(of: self)
	}
}

public protocol UnitDefault {
	static var `default`: Self { get }
}

public protocol UnitViewableContent {
	associatedtype Unit : UnitDefault
	associatedtype Index : UnitOffsetableIndex
	associatedtype SubSequence : UnitViewableContent
	associatedtype UnitView : BidirectionalCollection
	    where Self.UnitView.Element == Self.Unit,
	          Self.UnitView.Index == Self.Index
	var units: Self.UnitView { get }
	var startIndex: Self.Index { get }
	var endIndex: Self.Index { get }
	func unitOffset(of: Self.Index) -> Int
	func index(unitOffset: Int) -> Self.Index
}

public extension UnitViewableContent {
	func unitRange(for view: Self) -> Range<Int> {
		return unitOffset(of: view.startIndex)..<unitOffset(of: view.endIndex)
	}
}

public protocol Content : RangeReplaceableCollection, UnitViewableContent,
    BidirectionalCollection, Equatable {
	associatedtype SubSequence
	subscript(r: Range<Self.Index>) -> Self.SubSequence { get }
	static var empty: Self { get }
	var length: Int { get }
	var units: Self.UnitView { get }
}

extension Content {
	var restAndLast: (Self, Self)? {
		if isEmpty {
			return nil
		}
		return (Self.init(dropLast(1)),
		        Self.init(self[self.index(before: endIndex)...]))
	}
	var firstAndRest: (Self, Self)? {
		if isEmpty {
			return nil
		}
		return (Self.init(self[..<self.index(after: startIndex)]),
		        Self.init(dropFirst(1)))
	}
}

public extension Content {
	func extract(from start: Int, upTo end: Int,
	    filling buffer: inout UnsafeMutablePointer<Self.Unit>) {
		guard case true? =
		    (units.withContiguousStorageIfAvailable {
			guard let base = $0.baseAddress else {
				return false
			}
			let length = end - start
			buffer.initialize(from: base + start,
			    count: length)
			buffer += length
			return true
		} as Bool?) else {
			guard let sidx = index(units.startIndex,
				offsetBy: start,
				limitedBy: units.endIndex),
			    let eidx = index(units.startIndex,
				offsetBy: end,
				limitedBy: units.endIndex)
			    else {
				fatalError("In \(#function), " +
				    "no unit range \(start)..<\(end)")
			}
			for u in units[sidx..<eidx] {
				buffer.initialize(to: u)
				buffer += 1
			}
			return
		}
	}
}
