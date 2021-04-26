//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
extension String : Content {
	public typealias Element = Character
	public static var empty: String { return "" }
	public var length: Int {
		return distance(from: startIndex, to: endIndex)
	}
}
