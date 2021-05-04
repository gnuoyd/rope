//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
public protocol UnitOffsetable {
	init<S>(unitOffset offset: Int, in s: S)
	    where S : UnitViewable, S.Index == Self
	func unitOffset<S>(in s: S) -> Int
	    where S : UnitViewable, S.Index == Self
}

public extension UnitOffsetable {
	init<S>(unitOffset offset: Int, in s: S)
	    where S : UnitViewable, S.Index == Self {
		self = s.index(unitOffset: offset)
	}
	func unitOffset<S>(in s: S) -> Int
	    where S : UnitViewable, S.Index == Self {
		return s.unitOffset(of: self)
	}
}

public protocol UnitViewable {
	associatedtype Unit
	associatedtype Index : UnitOffsetable
	associatedtype SubSequence : UnitViewable
	associatedtype UnitView : BidirectionalCollection where Self.UnitView.Element == Self.Unit, Self.UnitView.Index == Self.Index
	var units: Self.UnitView { get }
	func unitOffset(of: Self.Index) -> Int
	func index(unitOffset: Int) -> Self.Index
}

public protocol Content : RangeReplaceableCollection, UnitViewable,
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
		return (Self.init(self[..<self.index(after: startIndex)]), Self.init(dropFirst(1)))
	}
}


