
#include <ciltut.h>

int (instrument string_compare)(char *a, char *b)
{
	int i = 0;

	while (1) {
		if (a[i] > b[i]) return 1;
		if (a[i] < b[i]) return -1;
		if (a[i] == 0 && b[i] == 0) break;
		i++;
	}

	return 0;
}

uint64_t (autotest f)(int input a, int input b)
{
	if ((a * b) - (a + b) == 14862436) {
		return 1;
	}
	else return 0;
}

uint64_t (autotest g)(char *inputnt s)
{
	return string_compare(s, "autotest");
}

int main ()
{
	uint64_t res;
	char cheese[1024] = "cheese";

	res = f(0,0);
	res = g(cheese);

	return res;
}
