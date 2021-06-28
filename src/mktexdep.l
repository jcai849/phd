%{
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <regex.h>

typedef struct dep { char *dir; char *suf; } dep;
struct deps { dep img; dep tab; dep src; dep doc; dep bib; }
d = {
	{ "img/", ".pdf" },
	{ "db/",  ".tex" },
	{ "src/", ""     },
	{ "doc/", ".tex" },
	{ "db/",  ".bib" }
};

int printdep(dep deptype, char *filename) 
{
	char buf[BUFSIZ];

	(void) strncpy(buf, deptype.dir, sizeof(buf) - 1);
	(void) strncat(buf, filename, sizeof(buf) - 1 - strlen(buf));
	(void) strncat(buf, deptype.suf, sizeof(buf) - 1 - strlen(buf));
	return printf(" %s", buf);
}

%}

SP	("[".+"]")?"{"
IP	[^"}"]+
CP	"}"
%s IMG TAB SRC DOC BIB

%%
\\img{SP}		BEGIN(IMG);
\\tab{SP}		BEGIN(TAB);
\\src{SP}		BEGIN(SRC);
\\doc{SP}		BEGIN(DOC);
\\bib{SP}		BEGIN(BIB);
<IMG>{IP}		printdep(d.img, yytext);
<TAB>{IP}		printdep(d.tab, yytext);
<SRC>{IP}		printdep(d.src, yytext);
<DOC>{IP}		printdep(d.doc, yytext);
<BIB>{IP}		printdep(d.bib, yytext);
<*>{CP}			BEGIN(INITIAL);
.|\n			;

%%

int main(int argc, char *argv[])
{
	while (--argc > 0) {
		++argv;
		yyrestart(fopen(*argv, "r"));
		printf("%s:", *argv);
		yylex();
		printf("\n\n");
	}
	exit(0);
}
