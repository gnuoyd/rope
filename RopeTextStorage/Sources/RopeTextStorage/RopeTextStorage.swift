//
// Copyright (c) 2020, 2021 David Young.  All rights reserved.
//
import AppKit
import Rope

extension Collection {
	var only: Self.Iterator.Element? {
		return (self.count > 1) ? nil : self.first
	}
}

public class GenericRopeTextStorage<P : BackingViewPath> : NSTextStorage {
	public typealias AxisPath = P
	public typealias Content = ContiguousArray<UTF16.CodeUnit>
	public typealias Backing = Rope<Content>
	public typealias Offset = Backing.Node.Offset
	let backing: Backing
	let _string: GenericRopeString<P>
	public init(with backing: Backing) {
		self.backing = backing
		_string = GenericRopeString<P>(with: backing)
		super.init()
		backing.axisDelegates[P.path] = AnyRopeDelegate<Content>(
		    didChange: self.ropeDidChange,
		    attributesDidChange: self.ropeAttributesDidChange)
	}
	public override init() {
		self.backing = Rope()
		self.backing.boundaryProperties = Backing.BoundaryProperties(attributes: Backing.BoundaryAttributes(open: [:], close: [:]), units: Backing.BoundaryUnits(open: "❮".utf16.only!, close: "❯".utf16.only!))
		_string = GenericRopeString<P>(with: backing)
		super.init()
		backing.axisDelegates[P.path] = AnyRopeDelegate<Content>(
		    didChange: self.ropeDidChange,
		    attributesDidChange: self.ropeAttributesDidChange)
	}
	required init?(coder: NSCoder) {
		fatalError("init(coder:) has not been implemented")
	}
	required init?(pasteboardPropertyList propertyList: Any,
	   ofType type: NSPasteboard.PasteboardType) {
		fatalError("init(pasteboardPropertyList:ofType:) " +
		    "has not been implemented")
	}
	override public var string: String {
		return _string as String
	}
	override public func attributes(at location: Int,
	    effectiveRange _range: NSRangePointer?)
	    -> [NSAttributedString.Key : Any] {
		let (attrs, r) =
		    backing[keyPath: AxisPath.path].attributes(at: location)
		if let range = _range {
			range.initialize(to: r.nsRange)
		}
/*
		Swift.print("\(attrs.count) attributes on " +
		    "[\(r.lowerBound), \(r.upperBound)], \(attrs)")
*/
		return attrs;
	}
	private func performEditing<T>(_ f: () -> T) -> T {
		beginEditing()
		defer {
			endEditing()
		}
		return f()
	}
	public func withRopeBacking<T>(_ f: (Backing) -> T) -> T {
		performEditing { f(backing) }
	}
	override public func replaceCharacters(in range: NSRange,
	    with str: String) {
		performEditing() {
			let undoList = ChangeList<Backing>()
			let ir = Range(range,
                           within: backing[keyPath: AxisPath.path])
			try! backing.replace(ir, with: Content(str.utf16[...]),
			    undoList: undoList)
		}
	}
	override public func setAttributes(
	    _ optAttrs: [NSAttributedString.Key : Any]?, range r: NSRange) {
		performEditing() {
			let range = Range(r, within: backing[keyPath: AxisPath.path])
			if let attrs = optAttrs {
				backing.setAttributes(attrs, range: range)
			} else {
				backing.clearAttributes(on: range)
			}
		}
	}
	public func ropeDidChange(on range: Range<Int>, changeInLength: Int) {
		Swift.print("\(#function)(on: \(range), " +
			    "changeInLength: \(changeInLength))")
		let actions = NSTextStorageEditActions.editedCharacters.union(
			.editedAttributes)
		edited(actions, range: range.nsRange,
		       changeInLength: changeInLength)
		return
	}
	public func ropeAttributesDidChange(on range: Range<Int>) {
		Swift.print("\(#function)(on: \(range))")
		edited(.editedAttributes, range: range.nsRange,
		       changeInLength: 0)
		return
	}
}

extension RopeTextStorage : RopeDelegate {
}

public typealias RopeTextStorage = GenericRopeTextStorage<UnitsPath>
public typealias ExpandedRopeTextStorage = GenericRopeTextStorage<StepsPath>
