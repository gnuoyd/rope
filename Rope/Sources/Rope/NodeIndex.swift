//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
import Foundation   // for NSRange

extension Rope.Node.Offset {
	public static func utf16Range(_ range: NSRange) -> Range<Self> {
		let lower = Self(of: range.location)
		let upper = Self(of: NSMaxRange(range))
		return lower..<upper
	}
	public static func utf16Range(_ range: Range<Int>) -> Range<Self> {
		let lower = Self(of: range.lowerBound)
		let upper = Self(of: range.upperBound)
		return lower..<upper
	}
	public static func utf16RangeTo(_ upperBound: Int) -> Range<Self> {
		let upper = Self(of: upperBound)
		return 0..<upper
	}
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(of: l.utf16Offset + r.utf16Offset)
	}
	static func -(_ l: Self, _ r: Self) -> Self {
		return Self(of: l.utf16Offset - r.utf16Offset)
	}
}

extension Rope.Node.Offset : Comparable {
	public static func ==(_ l: Self, _ r: Self) -> Bool {
		return l.utf16Offset == r.utf16Offset
	}
	public static func <(_ l: Self, _ r: Self) -> Bool {
		return l.utf16Offset < r.utf16Offset
	}
	static func max(_ l: Self, _ r: Self) -> Self {
		return (l <= r) ? r : l
	}
	static func min(_ l: Self, _ r: Self) -> Self {
		return (l <= r) ? l : r
	}
}

public protocol UTF16Offset {
	var utf16Offset: Int { get }
}

extension Rope.Node.Offset : UTF16Offset {
}

extension Range where Bound : UTF16Offset {
	public var nsRange: NSRange {
		let lower = lowerBound.utf16Offset
		let upper = upperBound.utf16Offset
		return NSMakeRange(lower, upper - lower)
	}
}

