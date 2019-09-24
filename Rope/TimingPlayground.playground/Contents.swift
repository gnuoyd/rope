import Cocoa

func ipow(_ base: Int, _ exponent: UInt) -> Int {
    if exponent == 0 {
        return 1
    }

    let e = ipow(base, exponent / 2)
    if exponent % 2 == 0 {
        return e * e
    }
    return base * e * e
}

func elapsedTime(_ f: () -> ()) -> TimeInterval {
    let start = Date()
    f()
    return -start.timeIntervalSinceNow
}

var y: Int = 0
let times: UInt = 5

for times in (0..<times).map({ ipow(2, $0) }) {
    y = y + 1
    var s = String(repeating: "xyz😀", count: times)
    let d = s.distance(from: s.startIndex, to: s.endIndex)
    let i = Int.random(in: 0..<d)
    let start = Date()
    let removeIndex = s.index(s.startIndex, offsetBy: i)
    -start.timeIntervalSinceNow / TimeInterval(times)
    elapsedTime() {
        let j = Int.random(in: 0..<d)
        s.index(s.startIndex, offsetBy: j)
    } / TimeInterval(times)
    let start3 = Date()
    s.remove(at: removeIndex)
    -start3.timeIntervalSinceNow
}

