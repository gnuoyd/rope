//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

extension Int : UnitOffsetable {
}

public extension Content where Self : RangeReplaceableCollection {
	var units: Self { return self }
	static var empty: Self { return Self.init() }
	var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
	func unitOffset(of index: Self.Index) -> Int {
		return distance(from: startIndex, to: index)
	}
	func index(unitOffset offset: Int) -> Self.Index {
		return index(startIndex, offsetBy: offset)
	}
}
