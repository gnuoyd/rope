//: A Cocoa based Playground to present user interface

import AppKit
import PlaygroundSupport
/*
let nibFile = NSNib.Name("MyView")
var topLevelObjects : NSArray?

Bundle.main.loadNibNamed(nibFile, owner:nil, topLevelObjects: &topLevelObjects)
let views = (topLevelObjects as! Array<Any>).filter { $0 is NSTextView }


// Present the view in Playground
PlaygroundPage.current.liveView = views[0] as! NSView
 */

let view = NSTextView(frame: NSRect(x: 0, y: 0, width: 100, height: 100))
// view.text = "Boo!"
view.backgroundColor = .cyan
PlaygroundPage.current.liveView = view
view.needsDisplay = true

