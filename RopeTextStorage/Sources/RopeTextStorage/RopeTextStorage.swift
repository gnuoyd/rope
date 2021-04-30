//
// Copyright (c) 2020 David Young.  All rights reserved.
//
import AppKit
import Rope

public class RopeTextStorage: NSTextStorage {
	typealias Offset = Rope<Substring>.Node.Offset
	let rope: Rope<Substring>
	let _string: RopeString
	public override init() {
		rope = Rope()
		_string = RopeString(rope: rope)
		super.init()
	}
	public init(content t: Substring.Initializer) {
		rope = Rope<Substring>(content: t)
		_string = RopeString(rope: rope)
		super.init()
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
		let i = Offset(of: location)
		let (attrs, r) = rope.attributes(at: i)
		if let range = _range {
			range.initialize(to: r.nsRange)
		}
/*
		Swift.print("\(attrs.count) attributes on " +
		    "[\(r.lowerBound.unitOffset), " +
		    "\(r.upperBound.unitOffset)], \(attrs)")
*/
		return attrs;
	}
	override public func replaceCharacters(in range: NSRange,
	    with str: String) {
		beginEditing()
		// TBD make sure `str` is the most efficient representation
		// to add to Rope<Substring> ?
		rope[Offset.unitRange(range)] = Substring(str)
		let actions = NSTextStorageEditActions.editedCharacters.union(
		    .editedAttributes)
		edited(actions, range: range,
		    changeInLength: (str as NSString).length - range.length)
		endEditing()
	}
	override public func setAttributes(
	    _ optAttrs: [NSAttributedString.Key : Any]?, range r: NSRange) {
		beginEditing()
		let range = Offset.unitRange(r)
		if let attrs = optAttrs  {
			rope.setAttributes(attrs, range: range)
		} else {
			rope.clearAttributesOnRange(range)
		}
		edited(.editedAttributes, range: r, changeInLength: 0)
		endEditing()
	}
}
