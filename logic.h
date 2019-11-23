/* logic.h: Declarations for the game logic modules.
 *
 * Copyright (C) 2001-2006 by Brian Raiter, under the GNU General Public
 * License. No warranty. See COPYING for details.
 */

#ifndef	_logic_h_
#define	_logic_h_

#include	"state.h"

/* One game logic engine.
 */
typedef	struct gamelogic gamelogic;
struct gamelogic {
    int		ruleset;		  /* the ruleset */
    int		localstateinfosize;	  /* how big to make localstateinfo */
    int	      (*initgame)(gamelogic*, gamestate*);	  /* prepare to play a game */
    int	      (*advancegame)(gamelogic*, gamestate*);	  /* advance the game one tick */
    int	      (*endgame)(gamelogic*, gamestate*);	  /* clean up after the game is done */
    void      (*shutdown)(gamelogic*, gamestate*);	  /* turn off the logic engine */
};

/* The available game logic engines.
 */
extern gamelogic *lynxlogicstartup(void);
extern gamelogic *mslogicstartup(void);

#endif
