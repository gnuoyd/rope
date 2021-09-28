//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
import Foundation

public struct Fibonacci : Sequence, IteratorProtocol {
	var last, prev_to_last: UInt
	var index, limit: UInt
	public init(from start: UInt, through j: UInt = UInt.max) {
		self.init(through: j)
		for _ in 0..<start {
			let _ = next()
		}
	}
	public init(through j: UInt = UInt.max) {
		index = 0
		limit = j
		last = 0
		prev_to_last = 1
	}
	public mutating func next() -> UInt? {
		if index > limit {
			return nil
		}
		defer {
			index = index + 1
			let tmp: UInt = last + prev_to_last
			prev_to_last = last
			last = tmp
		}
		return last
	}
	public func makeIterator() -> Fibonacci {
		return self
	}
}

public func fibonacci(index i: UInt) -> UInt {
	return Fibonacci(from: i).last
}
