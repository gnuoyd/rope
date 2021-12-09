//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import Foundation   // for NSRange

extension Rope.Node.Offset {
	public static func unitRange(_ range: NSRange) -> Range<Self> {
		let lower = Self(of: range.location)
		let upper = Self(of: NSMaxRange(range))
		return lower..<upper
	}
	public static func unitRange(_ range: Range<Int>) -> Range<Self> {
		let lower = Self(of: range.lowerBound)
		let upper = Self(of: range.upperBound)
		return lower..<upper
	}
	public static func +(_ l: Self, _ r: Self) -> Self {
		return Self(of: l.unitOffset + r.unitOffset)
	}
	public static func -(_ l: Self, _ r: Self) -> Self {
		return Self(of: l.unitOffset - r.unitOffset)
	}
}

extension Rope.Node.Offset : Comparable {
	public static func ==(_ l: Self, _ r: Self) -> Bool {
		return l.unitOffset == r.unitOffset
	}
	public static func <(_ l: Self, _ r: Self) -> Bool {
		return l.unitOffset < r.unitOffset
	}
	public static func max(_ l: Self, _ r: Self) -> Self {
		return (l <= r) ? r : l
	}
	public static func min(_ l: Self, _ r: Self) -> Self {
		return (l <= r) ? l : r
	}
}

public protocol UnitOffset {
	var unitOffset: Int { get }
}

extension Rope.Node.Offset : UnitOffset {
}

extension Range where Bound : UnitOffset {
	public var nsRange: NSRange {
		let lower = lowerBound.unitOffset
		let upper = upperBound.unitOffset
		return NSMakeRange(lower, upper - lower)
	}
}

