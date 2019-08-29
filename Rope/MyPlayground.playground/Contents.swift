import Cocoa
import Rope

typealias SSN = Node<Substring>

fibonacci(index: 0)
fibonacci(index: 1)
fibonacci(index: 2)
fibonacci(index: 3)
fibonacci(index: 4)
fibonacci(index: 5)

let h = Handle()
let j = Handle()
let k = Handle()
let n: SSN = Node(text: "lr")
n.afterStepInserting(index: h)
if case .step(let one) = n.afterStepInserting(index: h) {
    if case .step(let two) = one.inserting(index: j, oneStepAfter: h) {
        two.inserting(index: k, oneStepAfter: j)
    }
}
