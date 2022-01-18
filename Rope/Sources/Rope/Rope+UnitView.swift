//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
extension Rope {
	public var units: RopeAxisView {
                return RopeAxisView(of: self, axis: \.units,
		    properties: boundaryProperties)
	}
}
