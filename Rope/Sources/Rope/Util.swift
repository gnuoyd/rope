//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import Foundation   // for NSRange

extension Range where Bound == Int {
	public var nsRange: NSRange {
		let lower = lowerBound
		let upper = upperBound
		return NSMakeRange(lower, upper - lower)
	}
}

extension NSRange {
	public var range: Range<Int> {
		let lower: Int = location
		let upper: Int = NSMaxRange(self)
		return lower..<upper
	}
}
