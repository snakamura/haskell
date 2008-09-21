#include <signal.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#include "alarm.h"

int g_count = 0;

void handler(int n) {
	++g_count;
	alarm(1);
}

typedef void (*callback)(int);

void test(callback f) {
	struct sigaction action;
	
	memset(&action, 0, sizeof(action));
	action.sa_handler = handler;
	sigemptyset(&action.sa_mask);
	sigaction(SIGALRM, &action, 0);
	
	alarm(1);
	
	while (getchar() != 'q') {
		f(g_count);
	}
}

void c(int n) {
	printf("C: %d\n", n);
}
/*
int main(int argc, char* argv[]) {
	test(&c);
	return 0;
}
*/
