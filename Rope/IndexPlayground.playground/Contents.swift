import Cocoa
import Rope

let r: Rope<Substring> = Rope(text: "abcdefghijkl")

r.index(after: r.startIndex)
