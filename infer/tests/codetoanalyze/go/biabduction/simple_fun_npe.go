package empty

func foo() int {
	return bar(nil)
}

func bar(i *int) int {
	return *i // error
}
