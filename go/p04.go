package lib

import "fmt"

func isPalindrome(n int) bool {
	s := fmt.Sprintf("%d", n)
	i, j := 0, len(s)-1
	for i < j {
		if s[i] != s[j] {
			return false
		}
		i++
		j--
	}
	return true
}

func LargestPalindrome(min, max int) int {
	best := 0
	for i := max; i >= min; i-- {
		for j := i; j >= min; j-- {
			p := i * j
			if p > best && isPalindrome(p) {
				best = p
			}
		}
	}
	return best
}
