//
// Copyright (c) 2019, 2020 David Young.  All rights reserved.
//
public protocol Content : Initializable, StringProtocol {
	associatedtype SubSequence
	associatedtype Element
	subscript(r: Range<String.Index>) -> Self.SubSequence { get }
	var isEmpty: Bool { get }
	static var empty: Self { get }
	var length: Int { get }
	var utf16: Self.UTF16View { get }
	static func +<Other>(_ l: Self, _ r: Other) -> Self where Other : Sequence, Character == Other.Element
	init(_: SubSequence)
	init(repeating: Element, count: Int)
}

extension Content {
	var restAndLast: (Self, Self)? {
		if startIndex == endIndex {
			return nil
		}
		return (Self.init(dropLast(1)),
		        Self.init(self[self.index(before: endIndex)...]))
	}
	var firstAndRest: (Self, Self)? {
		if startIndex == endIndex {
			return nil
		}
		return (Self.init(self[..<self.index(after: startIndex)]), Self.init(dropFirst(1)))
	}
}


