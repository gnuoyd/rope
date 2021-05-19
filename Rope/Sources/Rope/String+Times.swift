public func *(_ s: String, _ times: Int) -> String {
	if times < 0 {
		return String(s.reversed()) * -times
	}
	if times == 0 {
		return ""
	}
	if times.isMultiple(of: 2) {
		let t = s * (times / 2)
		return t + t
	}
	return s + s * (times - 1)
}
