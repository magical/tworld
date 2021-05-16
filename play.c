/* play.c: Top-level game-playing functions.
 *
 * Copyright (C) 2001-2006 by Brian Raiter, under the GNU General Public
 * License. No warranty. See COPYING for details.
 */

#include	<stdlib.h>
#include	<string.h>
#include	"defs.h"
#include	"err.h"
#include	"state.h"
#include	"encoding.h"
#include	"oshw.h"
#include	"res.h"
#include	"logic.h"
#include	"random.h"
#include	"solution.h"
#include	"play.h"

/* The current state of the current game.
 */
static gamestate	state[2];

/* The current logic module.
 */
static gamelogic       *logic = NULL;

/* TRUE if the user has requested pedantic mode game play.
 */
static int		pedanticmode = FALSE;

/* How much mud to make the timer suck (i.e., the slowdown factor).
 */
static int		mudsucking = 1;

/* Turn on the pedantry.
 */
void setpedanticmode(void)
{
    pedanticmode = TRUE;
}

/* Set the slowdown factor.
 */
int setmudsuckingfactor(int mud)
{
    if (mud < 1)
	return FALSE;
    mudsucking = mud;
    return TRUE;
}

/* Configure the game logic, and some of the OS/hardware layer, as
 * required for the given ruleset. Do nothing if the requested ruleset
 * is already the current ruleset.
 */
static int setrulesetbehavior(int ruleset, int withgui)
{
    if (logic) {
	if (ruleset == logic->ruleset)
	    return TRUE;
	(*logic->shutdown)(logic, &state[0]);
	(*logic->shutdown)(logic, &state[1]);
	logic = NULL;
    }
    if (ruleset == Ruleset_None)
	return TRUE;

    switch (ruleset) {
      case Ruleset_Lynx:
	logic = lynxlogicstartup();
	if (!logic)
	    return FALSE;
	setkeyboardarrowsrepeat(TRUE);
	settimersecond(1000 * mudsucking);
	break;
      case Ruleset_MS:
	logic = mslogicstartup();
	if (!logic)
	    return FALSE;
	setkeyboardarrowsrepeat(FALSE);
	settimersecond(1100 * mudsucking);
	break;
      default:
	errmsg(NULL, "unknown ruleset requested (ruleset=%d)", ruleset);
	return FALSE;
    }

    if (withgui) {
	if (!loadgameresources(ruleset) || !creategamedisplay()) {
	    die("unable to proceed due to previous errors.");
	    return FALSE;
	}
    }

    return TRUE;
}

/* Initialize the current state to the starting position of the
 * given level.
 */
int initgamestate(gamesetup *game, int ruleset, int withgui, int side)
{
    int i = side;;

    if (!setrulesetbehavior(ruleset, withgui))
	die("unable to initialize the system for the requested ruleset");

    memset(state[i].map, 0, sizeof state[i].map);
    state[i].game = game;
    state[i].ruleset = ruleset;
    state[i].replay = -1;
    state[i].currenttime = -1;
    state[i].timeoffset = 0;
    state[i].currentinput = NIL;
    state[i].lastmove = NIL;
    state[i].initrndslidedir = NIL;
    state[i].stepping = -1;
    state[i].soundeffects = 0;
    state[i].timelimit = game->time * TICKS_PER_SECOND;
    state[i].statusflags = 0;
    if (pedanticmode)
	state[i].statusflags |= SF_PEDANTIC;
    initmovelist(&state[i].moves);
    resetprng(&state[i].mainprng);

    if (!expandleveldata(&state[i]))
	return FALSE;

    if (! (*logic->initgame)(logic, &state[i]))
	return FALSE;

    return TRUE;
}

/* Change the current state to run from the recorded solution.
 */
int prepareplayback(void)
{
    solutioninfo	solution;

    if (!state[0].game->solutionsize)
	return FALSE;
    solution.moves.list = NULL;
    solution.moves.allocated = 0;
    if (!expandsolution(&solution, state[0].game) || !solution.moves.count)
	return FALSE;

    destroymovelist(&state[0].moves);
    state[0].moves = solution.moves;
    for (int i = 0; i < 2; i++) {
	restartprng(&state[i].mainprng, solution.rndseed);
	state[i].initrndslidedir = solution.rndslidedir;
	state[i].stepping = solution.stepping;
	state[i].replay = 0;
    }
    return TRUE;
}

/* Return the amount of time passed in the current game, in seconds.
 */
int secondsplayed(void)
{
    return (state[0].currenttime + state[0].timeoffset) / TICKS_PER_SECOND;
}

/* Change the system behavior according to the given gameplay mode.
 */
void setgameplaymode(int mode)
{
    switch (mode) {
      case BeginInput:
	setkeyboardinputmode(TRUE);
	break;
      case EndInput:
	setkeyboardinputmode(FALSE);
	break;
      case BeginPlay:
	setkeyboardrepeat(FALSE);
	settimer(+1);
	break;
      case EndPlay:
	setkeyboardrepeat(TRUE);
	settimer(-1);
	break;
      case BeginVerify:
	settimer(+1);
	break;
      case EndVerify:
	settimer(-1);
	break;
      case SuspendPlayShuttered:
	if (state[0].ruleset == Ruleset_MS) {
	    state[0].statusflags |= SF_SHUTTERED;
	    state[1].statusflags |= SF_SHUTTERED;
	}
	/* fallthrough */
      case SuspendPlay:
	setkeyboardrepeat(TRUE);
	settimer(0);
	setsoundeffects(0);
	break;
      case ResumePlay:
	setkeyboardrepeat(FALSE);
	settimer(+1);
	setsoundeffects(+1);
	state[0].statusflags &= ~SF_SHUTTERED;
	state[1].statusflags &= ~SF_SHUTTERED;
	break;
    }
}

/* Alter the stepping. If display is true, the new stepping value is
 * reported to the user.
 */
int setstepping(int stepping, int display)
{
    char	msg[32], *p;

    state[0].stepping = stepping;
    state[1].stepping = stepping;
    if (display) {
	p = msg;
	p += sprintf(p, "%s-step", state[0].stepping & 4 ? "odd" : "even");
	if (state[0].stepping & 3)
	    p += sprintf(p, " +%d", state[0].stepping & 3);
	setdisplaymsg(msg, 500, 500);
    }
    return TRUE;
}

/* Alter the stepping by a delta. Force the stepping to be appropriate
 * to the current ruleset.
 */
int changestepping(int delta, int display)
{
    int	n;

    if (state[0].stepping < 0)
	state[0].stepping = 0;
    n = (state[0].stepping + delta) % 8;
    if (state[0].ruleset == Ruleset_MS)
	n &= ~3;
    if (state[0].stepping != n)
	return setstepping(n, display);
    return TRUE;
}

/* Rotate the initial random slide direction. Note that the stored
 * value for initrndslidedir is actually to the left of the first
 * direction that will actually be used, so the displayed message
 * needs to reflect that.
 */
int rotaterndslidedir(int display)
{
    char	msg[32];
    char const *dirname;

    if (state[0].ruleset == Ruleset_MS)
	return FALSE;
    state[0].initrndslidedir = right(state[0].initrndslidedir);
    state[1].initrndslidedir = state[0].initrndslidedir;
    if (display) {
	switch (right(state[0].initrndslidedir)) {
	  case NORTH:	dirname = "north";	break;
	  case WEST:	dirname = "west";	break;
	  case SOUTH:	dirname = "south";	break;
	  case EAST:	dirname = "east";	break;
	  default:	dirname = "(nil)";	break;
	}
	sprintf(msg, "random slide: %s", dirname);
	setdisplaymsg(msg, 500, 500);
    }
    return TRUE;
}

int doturnstate(struct gamestate *state, int cmd) {
    action	act;
    int		n;

    state->soundeffects &= ~((1 << SND_ONESHOT_COUNT) - 1);
    state->currenttime = gettickcount();
    if (state->currenttime >= MAXIMUM_TICK_COUNT) {
	errmsg(NULL, "timer reached its maximum of %d.%d hours; quitting now",
		     MAXIMUM_TICK_COUNT / (TICKS_PER_SECOND * 3600),
		     (MAXIMUM_TICK_COUNT / (TICKS_PER_SECOND * 360)) % 10);
	return -1;
    }
    // FIXME: replay is broken, one replay needs to feed both games
    if (state->replay < 0) {
	if (cmd != CmdPreserve)
	    state->currentinput = cmd;
    } else {
	if (state->replay < state->moves.count) {
	    if (state->currenttime > state->moves.list[state->replay].when)
		warn("Replay: Got ahead of saved solution: %d > %d!",
		     state->currenttime, state->moves.list[state->replay].when);
	    if (state->currenttime == state->moves.list[state->replay].when) {
		state->currentinput = state->moves.list[state->replay].dir;
		++state->replay;
	    }
	} else {
	    n = state->currenttime + state->timeoffset - 1;
	    if (n > state->game->besttime)
		return -1;
	}
    }

    n = (*logic->advancegame)(logic, state);

    if (state->replay < 0 && state->lastmove) {
	act.when = state->currenttime;
	act.dir = state->lastmove;
	addtomovelist(&state->moves, act);
	state->lastmove = NIL;
    }

    return n;
}

/* Advance the game one tick and update the game state. cmd is the
 * current keyboard command supplied by the user. The return value is
 * positive if the game was completed successfully, negative if the
 * game ended unsuccessfully, and zero otherwise.
 */
int doturn(int cmd)
{
    int status0 = doturnstate(&state[0], cmd);
    int status1 = doturnstate(&state[1], cmd);
    if (status0 < 0)
	return status0;
    if (status1 < 0)
	return status1;
    if (status0 > 0 && status1 > 0)
	return status0;
    return 0;
}

/* Update the display to show the current game state (including sound
 * effects, if any). If showframe is FALSE, then nothing is actually
 * displayed.
 */
int drawscreen(int showframe)
{
    int	besttime;

    playsoundeffects(state[0].soundeffects | state[1].soundeffects);

    if (!showframe)
	return TRUE;

    besttime = TIME_NIL;
    //if (hassolution(state[0].game))
    //    besttime = (state.game->time ? state.game->time : 999)
    //    			- state.game->besttime / TICKS_PER_SECOND;


    return displaygame(&state[0], &state[1], besttime);
}

/* Stop game play and clean up.
 */
int quitgamestate(void)
{
    state[0].soundeffects = 0;
    state[1].soundeffects = 0;
    setsoundeffects(-1);
    return TRUE;
}

/* Clean up after game play is over.
 */
int endgamestate(void)
{
    setsoundeffects(-1);
    int ok0 = (*logic->endgame)(logic, &state[0]);
    int ok1 = (*logic->endgame)(logic, &state[1]);
    return ok0 && ok1;
}

/* Close up shop.
 */
void shutdowngamestate(void)
{
    setrulesetbehavior(Ruleset_None, FALSE);
    destroymovelist(&state[0].moves);
    //destroymovelist(&state[1].moves);
}

/* Initialize the current game state to a small level used for display
 * at the completion of a series.
 */
void setenddisplay(void)
{
    for (int i = 0; i < 2; i++) {
	state[i].replay = -1;
	state[i].timelimit = 0;
	state[i].currenttime = -1;
	state[i].timeoffset = 0;
	state[i].chipsneeded = 0;
	state[i].currentinput = NIL;
	state[i].statusflags = 0;
	state[i].soundeffects = 0;
	getenddisplaysetup(&state[i]);
	(*logic->initgame)(logic, &state[i]);
    }
}

/*
 * Solution handling functions.
 */

/* Return TRUE if a solution exists for the given level.
 */
int hassolution(gamesetup const *game)
{
    return game->besttime != TIME_NIL;
}

/* Compare the most recent solution for the current game with the
 * user's best solution (if any). If this solution beats what's there,
 * or if the current solution has been marked as replaceable, then
 * replace it. TRUE is returned if the solution was replaced.
 */
int replacesolution(void)
{
    return FALSE; // XXX
    #if 0
    solutioninfo	solution;
    int			currenttime;

    if (state.statusflags & SF_NOSAVING)
	return FALSE;
    currenttime = state.currenttime + state.timeoffset;
    if (hassolution(state.game) && !(state.game->sgflags & SGF_REPLACEABLE)
				&& currenttime >= state.game->besttime)
	return FALSE;

    state.game->besttime = currenttime;
    state.game->sgflags &= ~SGF_REPLACEABLE;
    solution.moves = state.moves;
    solution.rndseed = getinitialseed(&state.mainprng);
    solution.flags = 0;
    solution.rndslidedir = state.initrndslidedir;
    solution.stepping = state.stepping;
    if (!contractsolution(&solution, state.game))
	return FALSE;

    return TRUE;
    #endif
}

/* Delete the user's best solution for the current game. FALSE is
 * returned if no solution was present.
 */
int deletesolution(void)
{
    return FALSE;
    #if 0
    if (!hassolution(state.game))
	return FALSE;
    state.game->besttime = TIME_NIL;
    state.game->sgflags &= ~SGF_REPLACEABLE;
    free(state.game->solutiondata);
    state.game->solutionsize = 0;
    state.game->solutiondata = NULL;
    return TRUE;
    #endif
}

/* Double-checks the timing for a solution that has just been played
 * back. If the timing is off, and the cause of the discrepancy can be
 * reasonably ascertained to be benign, the timing will be corrected
 * and TRUE is returned.
 */
int checksolution(void)
{
    int	currenttime;

    if (!hassolution(state[0].game))
	return FALSE;
    currenttime = state[0].currenttime + state[0].timeoffset;
    if (currenttime == state[0].game->besttime)
	return FALSE;
    warn("saved game has solution time of %d ticks, but replay took %d ticks",
	 state[0].game->besttime, currenttime);
    if (state[0].game->besttime == state[0].currenttime) {
	warn("difference matches clock offset; fixing.");
	state[0].game->besttime = currenttime;
	return TRUE;
    } else if (currenttime - state[0].game->besttime == 1) {
	warn("difference matches pre-0.10.1 error; fixing.");
	state[0].game->besttime = currenttime;
	return TRUE;
    }
    warn("reason for difference unknown.");
    state[0].game->besttime = currenttime;
    return FALSE;
}
