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

extension Collection {
	var only: Self.Iterator.Element? {
		return (self.count > 1) ? nil : self.first
	}
}

public func commonPrefix<S>(_ s1: S, _ s2: S)
    -> [S.Element] where S : Sequence, S.Element : Equatable {
	return zip(s1, s2).prefix { (e1, e2) in e1 == e2 }.map { (e, _) in e }
}
