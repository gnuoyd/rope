//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

extension Substring : Content {
	public typealias Element = Character
	public typealias Unit = UTF16.CodeUnit
	public typealias UnitView = Substring.UTF16View
	public var units: UnitView { return self.utf16 }
	public static var empty: Substring { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
	public func unitOffset(of index: Self.Index) -> Int {
		return index.utf16Offset(in: self)
	}
	public func index(unitOffset offset: Int) -> Self.Index {
		return Self.Index(utf16Offset: offset, in: self)
	}
}
