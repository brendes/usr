// Print a timestamp in various formats. Prints a timestamp in the
// format of YYYYMMDDHHSS by default with no arguments.
package main

import (
	"flag"
	"fmt"
	"os"
	"time"
)

var (
	d = flag.Bool("d", false, "print current date in format YYYY-MM-DD")
	f = flag.Bool("f", false, "print current date and time in format YYYY-MM-DD weekday HH:MM")
	s = flag.Bool("s", false, "print timestamp in format YYYYMMDDHHSS")
	n = flag.Bool("n", false, "append a newline to the output")
	h = flag.Bool("h", false, "print usage")
)

func main() {
	delim := ""
	args := os.Args
	t := time.Now()
	flag.Parse()
	if *n {
		delim = "\n"
	}
	if *d {
		fmt.Printf("%d-%02d-%02d%s", t.Year(), t.Month(), t.Day(), delim)
	}
	if *f {
		fmt.Printf("%d-%02d-%02d %s %02d:%02d%s",
			t.Year(), t.Month(), t.Day(), t.Weekday(), t.Hour(), t.Minute(), delim)
	}
	if *s {
		fmt.Printf("%d%02d%02d%02d%02d%02d%s",
			t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second(), delim)
	}
	if *h {
		flag.Usage()
	}
}
