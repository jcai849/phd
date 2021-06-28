%{
#include <string.h>
#define BUFSIZ	100
#define SUFLEN	4

int printdep(char *type, char *file);
char *substr(char *x, int start, int stop);

int printdep(char *type, char *file) 
{
	char buf[BUFSIZ];
	char suffix[SUFLEN];
	suffix[0] = '\0';

	(void) strncpy(buf, type, sizeof(buf) - 1);
	buf[sizeof(buf) - 1] = '\0';
	(void) strncat(buf, "/", sizeof(buf) - 1 - strlen(buf));
	(void) strncat(buf, file, sizeof(buf) - 1 - strlen(buf));
	if (!strncmp(type, "img", SUFLEN - 1))
		strncpy(suffix, ".svg");
	return printf(" %s", buf);
}

char *substr(char *x, int start, int stop) 
{
	char buf[BUFSIZ];
	int  lastpos;

	lastpos = (stop < sizeof(buf) - 1) ? stop : sizeof(buf) - 1;
	strncpy(buf, x[start], lastpos);
	return buf;
}
%}

%%
\\img\{[^\}]*\} printdep("img", substr(yylex, 3, strlen(yylex) - 1));
.|\n			;

%%
int main(int argc, char *argv[])
{
	printf("%s:" argv[1])
	yylex();
	printf("\n\n")
}
