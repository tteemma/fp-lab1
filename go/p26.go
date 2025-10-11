package lib

func RecurringCycleLen(d int) int {
	pos := make([]int, d)
	for i := 0; i < d; i++ { pos[i] = -1 }
	remainder := 1 % d
	index := 0
	for remainder != 0 {
		r := remainder % d
		if pos[r] != -1 {
			return index - pos[r]
		}
		pos[r] = index
		remainder = (r * 10) % d
		index++
	}
	return 0
}

func LongestCycle(limit int) int {
	bestD, bestL := 0, 0
	for d := 2; d < limit; d++ {
		l := RecurringCycleLen(d)
		if l > bestL {
			bestL, bestD = l, d
		}
	}
	return bestD
}
