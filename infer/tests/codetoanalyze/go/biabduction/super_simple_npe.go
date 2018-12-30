package empty

func foo() int {
	var p *int = nil
	return *p
}
