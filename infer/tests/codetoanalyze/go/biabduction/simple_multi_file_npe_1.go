package empty

func foo(a int) int {
	var n int = 42
	var p *int = bar(a, &n)
	return *p // error
}
