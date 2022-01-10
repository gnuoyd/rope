//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
	public func extractContent(_ range: Range<Offset>) -> C.SubSequence {
		return node.extractContent(from: range.lowerBound,
		    upTo: range.upperBound)
	}
	public func extractContent(_ range: Range<Offset>,
	    filling buffer: inout C) {
		node.extractContent(from: range.lowerBound,
		    upTo: range.upperBound, filling: &buffer)
	}
}
