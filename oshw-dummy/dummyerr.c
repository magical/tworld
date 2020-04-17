#include <stdio.h>
#include "../gen.h"
#include "../oshw.h"

void usermessage(
	int action,
	char const *prefix,
	char const *cfile,
	unsigned long lineno,
	char const *fmt,
	va_list args
) {
	if (action == NOTIFY_DIE) {
		fprintf(stderr, "FATAL: ");
	} else if (action == NOTIFY_ERR) {
		fprintf(stderr, "error: ");
	} else if (action == NOTIFY_LOG) {
		fprintf(stderr, "warning: ");
	}

	if (cfile != NULL) {
		fprintf(stderr, "[%s:%lu] ", cfile, lineno);
	}
	if (prefix != NULL) {
		fprintf(stderr, "%s: ", prefix);
	}
	if (fmt != NULL) {
		vfprintf(stderr, fmt, args);
	}
	fprintf(stderr, "\n");
	fflush(stderr);
}
