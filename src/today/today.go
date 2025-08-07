package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Printf("%d-%02d-%02d", t.Year(), t.Month(), t.Day())
}
