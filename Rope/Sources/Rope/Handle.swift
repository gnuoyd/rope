//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//

public class Label : Hashable {
	public enum Id : Hashable {
	public typealias Number = UInt64
	case cursor(Number)
	case extent(Number)
	case index(Number)
	}
	static var nextNumber: Id.Number = 0
	var _id: Id.Number
	public var id: Id { return .index(_id) }
	public init() {
		_id = Label.nextNumber
		Label.nextNumber = Label.nextNumber + 1
	}
	public static func ==(_ l: Label, _ r: Label) -> Bool {
		return l._id == r._id
	}
	public func hash(into hasher: inout Hasher) {
		hasher.combine(_id)
	}
}

extension Label.Id : CustomDebugStringConvertible {
	public var debugDescription: String {
		switch self {
		case .cursor(let n):
			return ".cursor(\(n))"
		case .extent(let n):
			return ".zone(\(n))"
		case .index(let n):
			return ".index(\(n))"
		}
	}
}

extension Label : CustomDebugStringConvertible {
	public var debugDescription: String {
		return "Label(id: \(id.debugDescription))"
	}
}
