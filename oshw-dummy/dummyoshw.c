#include <stddef.h>
#include "../gen.h"
#include "../oshw.h"
#include "../defs.h"

int main(int argc, char** argv)
{
	return tworld(argc, argv);
}

int oshwinitialize(int silence, int soundbufsize, int showhistogram, int fullscreen)
{
	(void)silence;
	(void)soundbufsize;
	(void)showhistogram;
	(void)fullscreen;
	return TRUE;
}

/* Timer */

static int utick;

void settimer(int action)
{
	if (action > 0) {
		utick = 0;
	}
}

void settimersecond(int ms)
{
	(void)ms;
}

int gettickcount(void)
{
	return utick;
}

int waitfortick(void)
{
	utick++;
	return TRUE;
}

int advancetick(void)
{
	utick++;
	return utick;
}

/* Input */

int setkeyboardrepeat(int enable)
{
	(void)enable;
	return TRUE;
}

int setkeyboardarrowsrepeat(int enable)
{
	(void)enable;
	return TRUE;
}

int setkeyboardinputmode(int enable)
{
	(void)enable;
	return TRUE;
}

int input(int wait)
{
	(void)wait;
	return CmdQuitLevel;
}

int anykey(void)
{
	return FALSE; // quit
}

tablespec const *keyboardhelp(int context)
{
	(void)context;
	return NULL;
}

/* Resources */

int loadfontfromfile(char const *filename, int complain)
{
	(void)filename;
	(void)complain;
	return TRUE;
}

void freefont(void)
{
}

int loadtileset(char const *filename, int complain)
{
	(void)filename;
	(void)complain;
	return TRUE;
}

void freetileset(void)
{
}

/* Video */

int creategamedisplay(void)
{
	return TRUE;
}

void setcolors(long bkgnd, long text, long bold, long dim)
{
	(void)bkgnd;
	(void)text;
	(void)bold;
	(void)dim;
}

void cleardisplay(void)
{
}

int displaygame(void const *state, int timeleft, int besttime)
{
	(void)state;
	(void)timeleft;
	(void)besttime;
	return TRUE;
}

int displayendmessage(int basescore, int timescore, long totalscore, int completed)
{
	(void)basescore;
	(void)timescore;
	(void)totalscore;
	(void)completed;
	return TRUE;
}

int setdisplaymsg(char const *msg, int msecs, int bold)
{
	(void)msg;
	(void)msecs;
	(void)bold;
	return TRUE;
}

int displaylist(char const *title, tablespec const *table, int *index, int (*inputcallback)(int*))
{
	int n = SCROLL_NOP;
	(void)title;
	(void)table;
	(void)index;
	inputcallback(&n);
	return n;
}

int displayinputprompt(char const *prompt, char *input, int maxlen, int (*inputcallback)(void))
{
	(void)prompt;
	(void)input;
	(void)maxlen;
	(void)inputcallback;
	return FALSE; // no input
}

/* Sound */

int setaudiosystem(int active)
{
	(void)active;
	return TRUE;
}

int loadsfxfromfile(int index, char const *filename)
{
	(void)index;
	(void)filename;
	return TRUE;
}

void playsoundeffects(unsigned long sfx)
{
	(void)sfx;
}

void setsoundeffects(int action)
{
	(void)action;
}

int setvolume(int volume, int display)
{
	(void)volume;
	(void)display;
	return TRUE;
}

int changevolume(int delta, int display)
{
	(void)delta;
	(void)display;
	return TRUE;
}

int getvolume(void)
{
	return 10;
}

void freesfx(int index)
{
	(void)index;
}

/* Miscellaneous */

void ding(void) { }

void setsubtitle(char const *subtitle)
{
	(void)subtitle;
}

int displaytextscroll(char const *title, char const **paragraphs, int ppcount, int completed, int (*inputcallback)(int*))
{
	(void)title;
	(void)paragraphs;
	(void)ppcount;
	(void)completed;
	(void)inputcallback;
	return TRUE;
}

int displaytiletable(char const *title, tiletablerow const *rows, int count, int completed)
{
	(void)title;
	(void)rows;
	(void)count;
	(void)completed;
	return TRUE;
}

int displaytable(char const *title, tablespec const *table, int completed)
{
	(void)title;
	(void)table;
	(void)completed;
	return TRUE;
}
