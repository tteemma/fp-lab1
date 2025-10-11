package lib

import "testing"

func TestLab1(t *testing.T) {
	t.Run("TestLargestPalindrome", func(t *testing.T) {
		t.Parallel()
		const expected = 906609

		ans := LargestPalindrome(100, 999)

		if ans != expected {
			t.Fatalf("LargestPalindrome: got %d, expected %d", ans, expected)
		}
	})

	t.Run("TestLongestCycle", func(t *testing.T) {
		t.Parallel()
		const expected = 983

		ans := LongestCycle(1000)

		if ans != expected {
			t.Fatalf("LongestCycle: got %d, expected %d", ans, expected)
		}
	})
}
