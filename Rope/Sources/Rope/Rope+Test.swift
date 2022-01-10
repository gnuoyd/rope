//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

/* Test points */
extension Rope {
	subscript(r: Range<Index>) -> Element {
		get {
			if r.isEmpty {
				return .empty
			}
			guard let e = node.subrope(after: r.lowerBound,
			    upTo: r.upperBound) else {
				fatalError("No such range")
			}
			return e
		}
	}
/*
	subscript(i: Offset) -> Content.Element {
		return node.element(at: i)
	}
*/
}
