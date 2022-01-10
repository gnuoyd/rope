//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
extension RangeReplaceableCollection {
    func firstIndex<C : Collection>(of target: C) -> Index?
        where C.Element == Element, Element : Equatable {
		guard let first = target.first else {
			// every Collection starts with the empty Collection
			return startIndex
		}
		var rest = self
		while let start = rest.firstIndex(of: first) {
			rest.removeFirst(1)
			if rest.starts(with: target.dropFirst(1)) {
				return start
			}
		}
		return nil
	}
}
