//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
extension Substring : Content {
	public typealias Element = Character
	public static var empty: Substring { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
}
