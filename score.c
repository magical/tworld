/* score.c: Calculating scores and formatting the display of same.
 *
 * Copyright (C) 2001 by Brian Raiter, under the GNU General Public
 * License. No warranty. See COPYING for details.
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"defs.h"
#include	"err.h"
#include	"play.h"
#include	"score.h"

/* Translate a number into an ASCII string, complete with commas.
 */
static char const *commify(int number)
{
    static char	buf[32];
    char       *dest = buf + sizeof buf;
    int		n = 0;

    *--dest = '\0';
    do {
	++n;
	if (n % 4 == 0) {
	    *--dest = ',';
	    ++n;
	}
	*--dest = '0' + number % 10;
	number /= 10;
    } while (number);
    return dest;
}

int getscoresforlevel(gameseries const *series, int level,
		      int *base, int *bonus, int *total)
{
    gamesetup	       *game;
    int			levelscore, timescore;
    unsigned int	totalscore;
    int			n;

    totalscore = 0;
    for (n = 0, game = series->games ; n < series->count ; ++n, ++game) {
	if (n >= series->allocated)
	    break;
	levelscore = 0;
	timescore = 0;
	if (hassolution(game)) {
	    levelscore = game->number * 500;
	    if (game->time)
		timescore = 10 * (game->time
					- game->besttime / TICKS_PER_SECOND);
	}
	if (n == level) {
	    *base = levelscore;
	    *bonus = timescore;
	}
	totalscore += levelscore + timescore;
    }
    *total = totalscore;
    return TRUE;
}

/* Produce a table that breaks down the player's current score.
 */
int createscorelist(gameseries const *series,
		    int **plevellist, int *pcount, tablespec *table)
{
    gamesetup	       *game;
    char	      **ptrs;
    char	       *textheap;
    int		       *levellist = NULL;
    unsigned int	levelscore, timescore;
    unsigned int	totalscore;
    int			count;
    int			used, j, n;

    if (plevellist) {
	levellist = malloc((series->count + 2) * sizeof *levellist);
	if (!levellist)
	    memerrexit();
    }
    ptrs = malloc((series->count + 2) * 5 * sizeof *ptrs);
    textheap = malloc((series->count + 2) * 96);
    if (!ptrs || !textheap)
	memerrexit();
    totalscore = 0;

    n = 0;
    used = 0;
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "1+Level");
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "1-Name");
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "1+Base");
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "1+Bonus");
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "1+Score");

    count = 0;
    for (j = 0, game = series->games ; j < series->count ; ++j, ++game) {
	if (j >= series->allocated)
	    break;
#if 0
	if (!(game->sgflags & SGF_HASPASSWD))
	    continue;
	if (plevellist)
	    levellist[count] = j;
	++count;
#endif
	ptrs[n++] = textheap + used;
	used += 1 + sprintf(textheap + used, "1+%d", game->number);
	if (hassolution(game)) {
	    ptrs[n++] = textheap + used;
	    used += 1 + sprintf(textheap + used, "1-%s", game->name);
	    ptrs[n++] = textheap + used;
	    levelscore = 500 * game->number;
	    used += 1 + sprintf(textheap + used, "1+%s", commify(levelscore));
	    ptrs[n++] = textheap + used;
	    if (game->time) {
		timescore = 10 * (game->time
					- game->besttime / TICKS_PER_SECOND);
		used += 1 + sprintf(textheap + used, "1+%s",
				    commify(timescore));
	    } else {
		timescore = 0;
		strcpy(textheap + used, "1+---");
		used += 6;
	    }
	    ptrs[n++] = textheap + used;
	    used += 1 + sprintf(textheap + used, "1+%s",
				commify(levelscore + timescore));
	    totalscore += levelscore + timescore;
#if 1
	    if (plevellist)
		levellist[count] = j;
	    ++count;
#endif
	} else {
	    ptrs[n++] = textheap + used;
#if 0
	    used += 1 + sprintf(textheap + used, "4-%s", game->name);
#else
	    if (game->sgflags & SGF_HASPASSWD) {
		used += 1 + sprintf(textheap + used, "4-%s", game->name);
		if (plevellist)
		    levellist[count] = j;
	    } else {
		used += 1 + sprintf(textheap + used, "4- ");
		if (plevellist)
		    levellist[count] = -1;
	    }
	    ++count;
#endif
	}
    }
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "2-Total Score");
    ptrs[n++] = textheap + used;
    used += 1 + sprintf(textheap + used, "3+%s", commify(totalscore));
    if (plevellist)
	levellist[count] = -1;
    ++count;

    if (plevellist)
	*plevellist = levellist;
    if (pcount)
	*pcount = count;

    table->rows = count + 1;
    table->cols = 5;
    table->sep = 2;
    table->collapse = 1;
    table->items = ptrs;

    return TRUE;
}

/* Free the memory allocated by createscorelist().
 */
void freescorelist(int *levellist, tablespec *table)
{
    free(levellist);
    free(table->items[0]);
    free(table->items);
}
