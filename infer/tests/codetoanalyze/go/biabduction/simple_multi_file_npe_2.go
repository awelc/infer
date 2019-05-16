package empty

func bar(a int, n *int) *int {
	if a == 7 {
		return nil
	} else {
		return n
	}
}

func baz() int {
	return foo(7)
}
