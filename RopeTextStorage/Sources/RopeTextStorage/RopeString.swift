//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
import AppKit
import Rope

public protocol BackingViewPath {
	typealias Backing = Rope<ContiguousArray<UTF16.CodeUnit>>
	static var path: KeyPath<Backing, Backing.RopeAxisView> { get }
}

class GenericRopeString<P : BackingViewPath> : NSString {
	typealias Backing = Rope<ContiguousArray<UTF16.CodeUnit>>
	var backing: Backing
	var view: Backing.RopeAxisView
	var viewPath: KeyPath<Backing, Backing.RopeAxisView>
	init(with backing: Backing) {
		self.backing = backing
		self.viewPath = P.path
		self.view = backing[keyPath: viewPath]
		super.init()
	}
	override convenience init() {
		self.init(with: Backing())
	}
	required init?(coder aDecoder: NSCoder) {
		fatalError()
	}
	required init?(pasteboardPropertyList propertyList: Any,
	    ofType type: NSPasteboard.PasteboardType) {
		fatalError()
	}
	public override var length: Int {
		return view.length
	}
	override func character(at i: Int) -> unichar {
		let c = view[i]
		// Swift.print("character(at: \(i)) -> \(c)")
		return c
	}
	override func getCharacters(_ buffer_in: UnsafeMutablePointer<unichar>,
	    range: NSRange) {
		var buffer = buffer_in
		view.extract(range.range, filling: &buffer)
	}
	override func copy(with zone: NSZone? = nil) -> Any {
		return self
	}
}

public enum StepsPath : BackingViewPath {
	public static var path: KeyPath<Backing, Backing.RopeAxisView> = \.steps
}

public enum UnitsPath : BackingViewPath {
	public static var path: KeyPath<Backing, Backing.RopeAxisView> = \.units
}

typealias ExpandedRopeString = GenericRopeString<StepsPath>
typealias RopeString = GenericRopeString<UnitsPath>
