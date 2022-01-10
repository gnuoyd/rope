//
// Copyright (c) 2019, 2020, 2021 David Young.  All rights reserved.
//
public protocol UnitRopeView {
	associatedtype Cx : Content
	var length: Int { get }
	subscript(_ r: Range<Int>) -> Cx { get }
	func extract(_ range: Range<Int>,
	    filling buffer: inout UnsafeMutablePointer<Cx.Unit>)
	func attributes(at i: Int) -> (Attributes, Range<Int>)
}

extension UnitRopeView {
	public typealias Unit = Cx.Unit
	public subscript(_ i: Int) -> Unit {
		get {
			var result: Unit = Unit.default
			withUnsafeMutablePointer(to: &result) { _buf in
				var buf = _buf
				extract(i..<(i+1), filling: &buf)
			}
			return result
		}
	}
}
