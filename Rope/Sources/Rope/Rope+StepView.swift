//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
	public var steps: RopeAxisView {
                return RopeAxisView(of: self, axis: \.steps,
		    properties: boundaryProperties)
	}
}
