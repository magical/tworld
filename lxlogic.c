/* lxlogic.c: The game logic for the Lynx ruleset.
 *
 * Copyright (C) 2001-2006 by Brian Raiter, under the GNU General Public
 * License. No warranty. See COPYING for details.
 */

#include	<stdlib.h>
#include	<string.h>
#include	"defs.h"
#include	"err.h"
#include	"state.h"
#include	"random.h"
#include	"logic.h"

/* The maximum number of creatures on the original Atari Lynx version.
 */
enum { PMAX_CREATURES = 128 };

/* Temporary "holding" values used in place of a direction.
 */
enum { WALKER_TURN = NORTH | SOUTH | EAST };
enum { BLOB_TURN   = NORTH | SOUTH | WEST };

/* TRUE if dir is a diagonal move.
 */
#define	isdiagonal(dir)	(((dir) & (NORTH | SOUTH)) && ((dir) & (EAST | WEST)))

/* My internal assertion macro.
 */
#ifdef NDEBUG
#define	_assert(test)	((void)0)
#else
#define	_assert(test)	((test) || (die("internal error: failed sanity check" \
				        " (%s)\nPlease report this error to"  \
				        " breadbox@muppetlabs.com", #test), 0))
#endif

/* Declarations of (indirectly recursive) functions.
 */
static int canmakemove(gamestate *state, creature const *cr, int dir, int flags);
static int advancecreature(gamestate *state, creature *cr, int releasing);

/* Used to calculate movement offsets.
 */
static int const	delta[] = { 0, -CXGRID, -1, 0, +CXGRID, 0, 0, 0, +1 };

/* The direction used the last time something stepped onto a random
 * slide floor.
 */
static int		lastrndslidedir = NORTH;

/*
 * Accessor macros for various fields in the game state. Many of the
 * macros can be used as an lvalue.
 */

#define	creaturelist()		(state->creatures)

#define	getchip(state)		(creaturelist())
#define	chippos(state)		(getchip(state)->pos)
#define	chipisalive(state)		(getchip(state)->id == Chip)

#define	mainprng()		(&state->mainprng)

#define	rndslidedir()		(state->initrndslidedir)

#define	setnosaving(state)	(state->statusflags |= SF_NOSAVING)
#define	showhint(state)		(state->statusflags |= SF_SHOWHINT)
#define	hidehint(state)		(state->statusflags &= ~SF_SHOWHINT)
#define	markinvalid(state)	(state->statusflags |= SF_INVALID)
#define	ismarkedinvalid(state)	(state->statusflags & SF_INVALID)
#define	ispedanticmode(state)	(state->statusflags & SF_PEDANTIC)

#define	chipsneeded()		(state->chipsneeded)

#define	clonerlist()		(state->cloners)
#define	clonerlistsize()	(state->clonercount)
#define	traplist()		(state->traps)
#define	traplistsize()		(state->trapcount)

#define	getlxstate()		(&state->lx)

#define	creaturearray()		(getlxstate()->creaturearray)
#define	completed()		(getlxstate()->completed)
#define	togglestate()		(getlxstate()->togglestate)
#define	couldntmove()		(getlxstate()->couldntmove)
#define	chippushing()		(getlxstate()->pushing)
#define	chipstuck()		(getlxstate()->stuck)
#define	mapbreached()		(getlxstate()->mapbreached)
#define	chiptopos()		(getlxstate()->chiptopos)
#define	chiptocr()		(getlxstate()->chiptocr)
#define	prngvalue1()		(getlxstate()->prng1)
#define	prngvalue2()		(getlxstate()->prng2)
#define	xviewoffset()		(getlxstate()->xviewoffset)
#define	yviewoffset()		(getlxstate()->yviewoffset)
#define	creaturelistend()	(getlxstate()->crend)

#define	inendgame(state)		(getlxstate()->endgametimer)
#define	startendgametimer(state)	(getlxstate()->endgametimer = 12 + 1)
#define	decrendgametimer(state)		(--getlxstate()->endgametimer)
#define	resetendgametimer(state)	(getlxstate()->endgametimer = 0)

#define	addsoundeffect(state, sfx)	(state->soundeffects |= 1 << (sfx))
#define	stopsoundeffect(state, sfx)	(state->soundeffects &= ~(1 << (sfx)))

#define	floorat(state, pos)		(state->map[pos].top.id)

#define	possession(obj)	(*_possession(state, obj))
static short *_possession(gamestate *state, int obj)
{
    switch (obj) {
      case Key_Red:		return &state->keys[0];
      case Key_Blue:		return &state->keys[1];
      case Key_Yellow:		return &state->keys[2];
      case Key_Green:		return &state->keys[3];
      case Boots_Ice:		return &state->boots[0];
      case Boots_Slide:		return &state->boots[1];
      case Boots_Fire:		return &state->boots[2];
      case Boots_Water:		return &state->boots[3];
      case Door_Red:		return &state->keys[0];
      case Door_Blue:		return &state->keys[1];
      case Door_Yellow:		return &state->keys[2];
      case Door_Green:		return &state->keys[3];
      case Ice:			return &state->boots[0];
      case IceWall_Northwest:	return &state->boots[0];
      case IceWall_Northeast:	return &state->boots[0];
      case IceWall_Southwest:	return &state->boots[0];
      case IceWall_Southeast:	return &state->boots[0];
      case Slide_North:		return &state->boots[1];
      case Slide_West:		return &state->boots[1];
      case Slide_South:		return &state->boots[1];
      case Slide_East:		return &state->boots[1];
      case Slide_Random:	return &state->boots[1];
      case Fire:		return &state->boots[2];
      case Water:		return &state->boots[3];
    }
    warn("Invalid object %d handed to possession()\n", obj);
    _assert(!"possession() called with an invalid object");
    return NULL;
}

/* The pseudorandom number generator, used by walkers and blobs. This
 * exactly matches the PRNG used in the original Lynx game.
 */
static unsigned char lynx_prng(gamestate *state)
{
    unsigned char n;

    n = (prngvalue1() >> 2) - prngvalue1();
    if (!(prngvalue1() & 0x02))
        --n;
    prngvalue1() = (prngvalue1() >> 1) | (prngvalue2() & 0x80);
    prngvalue2() = (prngvalue2() << 1) | (n & 0x01);
    return (prngvalue1() ^ prngvalue2()) & 0xFF;
}

/*
 * Simple floor functions.
 */

/* Floor state flags.
 */
enum {
    FS_CLAIMED		= 0x40,	/* spot is claimed by a creature */
    FS_ANIMATED		= 0x20	/* spot is playing an animation */
};

/* Accessor macros for the floor states.
 */
#define	claimlocation(state, pos)	(state->map[pos].top.state |= FS_CLAIMED)
#define	removeclaim(state, pos)	(state->map[pos].top.state &= ~FS_CLAIMED)
#define	islocationclaimed(state, pos)	(state->map[pos].top.state & FS_CLAIMED)
#define	markanimated(state, pos)	(state->map[pos].top.state |= FS_ANIMATED)
#define	clearanimated(state, pos)	(state->map[pos].top.state &= ~FS_ANIMATED)
#define	ismarkedanimated(state, pos)	(state->map[pos].top.state & FS_ANIMATED)

/* Translate a slide floor into the direction it points in. In the
 * case of a random slide floor, if advance is TRUE a new direction
 * shall be selected; otherwise the current direction is used.
 */
static int getslidedir(gamestate *state, int floor, int advance)
{
    (void)state;
    switch (floor) {
      case Slide_North:		return NORTH;
      case Slide_West:		return WEST;
      case Slide_South:		return SOUTH;
      case Slide_East:		return EAST;
      case Slide_Random:
	if (advance)
	    lastrndslidedir = right(lastrndslidedir);
	return lastrndslidedir;
    }
    warn("Invalid floor %d handed to getslidedir()\n", floor);
    _assert(!"getslidedir() called with an invalid object");
    return NIL;
}

/* Alter a creature's direction if they are at an ice wall.
 */
static void applyicewallturn(gamestate *state, creature *cr)
{
    int	floor, dir;

    floor = floorat(state, cr->pos);
    dir = cr->dir;
    switch (floor) {
      case IceWall_Northeast:
	dir = dir == SOUTH ? EAST : dir == WEST ? NORTH : dir;
	break;
      case IceWall_Southwest:
	dir = dir == NORTH ? WEST : dir == EAST ? SOUTH : dir;
	break;
      case IceWall_Northwest:
	dir = dir == SOUTH ? WEST : dir == EAST ? NORTH : dir;
	break;
      case IceWall_Southeast:
	dir = dir == NORTH ? EAST : dir == WEST ? SOUTH : dir;
	break;
    }
    cr->dir = dir;
}

/* Find the location of a beartrap from one of its buttons.
 */
static int trapfrombutton(gamestate *state, int pos)
{
    xyconn     *xy;
    int		i;

    if (ispedanticmode(state)) {
	i = pos;
	for (;;) {
	    ++i;
	    if (i == CXGRID * CYGRID)
		i = 0;
	    if (i == pos)
		break;
	    if (floorat(state, i) == Beartrap)
		return i;
	}
    } else {
	for (xy = traplist(), i = traplistsize() ; i ; ++xy, --i)
	    if (xy->from == pos)
		return xy->to;
    }
    return -1;
}

/* Find the location of a clone machine from one of its buttons.
 */
static int clonerfrombutton(gamestate *state, int pos)
{
    xyconn     *xy;
    int		i;

    if (ispedanticmode(state)) {
	i = pos;
	for (;;) {
	    ++i;
	    if (i == CXGRID * CYGRID)
		i = 0;
	    if (i == pos)
		break;
	    if (floorat(state, i) == CloneMachine)
		return i;
	}
    } else {
	for (xy = clonerlist(), i = clonerlistsize() ; i ; ++xy, --i)
	    if (xy->from == pos)
		return xy->to;
    }
    return -1;
}

/* Quell any continuous sound effects coming from what Chip is
 * standing on. If includepushing is TRUE, also quell the sound of any
 * blocks being pushed.
 */
static void resetfloorsounds(gamestate *state, int includepushing)
{
    stopsoundeffect(state, SND_SKATING_FORWARD);
    stopsoundeffect(state, SND_SKATING_TURN);
    stopsoundeffect(state, SND_FIREWALKING);
    stopsoundeffect(state, SND_WATERWALKING);
    stopsoundeffect(state, SND_ICEWALKING);
    stopsoundeffect(state, SND_SLIDEWALKING);
    stopsoundeffect(state, SND_SLIDING);
    if (includepushing)
	stopsoundeffect(state, SND_BLOCK_MOVING);
}

/*
 * Functions that manage the list of entities.
 */

/* Creature state flags.
 */
enum {
    CS_FDIRMASK		= 0x0F,	/* temp storage for forced moves */
    CS_SLIDETOKEN	= 0x10,	/* can move off of a slide floor */
    CS_REVERSE		= 0x20,	/* needs to turn around */
    CS_PUSHED		= 0x40,	/* block was pushed by Chip */
    CS_TELEPORTED	= 0x80	/* creature was just teleported */
};

#define	getfdir(cr)	((cr)->state & CS_FDIRMASK)
#define	setfdir(cr, d)	((cr)->state = ((cr)->state & ~CS_FDIRMASK) \
				     | ((d) & CS_FDIRMASK))

/* Return the creature located at pos. Ignores Chip unless includechip
 * is TRUE. (This is important in the case when Chip and a second
 * creature are currently occupying a single location.)
 */
static creature *lookupcreature(gamestate *state, int pos, int includechip)
{
    creature   *cr;

    cr = creaturelist();
    if (!includechip)
	++cr;
    for ( ; cr->id ; ++cr)
	if (cr->pos == pos && !cr->hidden && !isanimation(cr->id))
	    return cr;
    return NULL;
}

/* Return a fresh creature.
 */
static creature *newcreature(gamestate *state)
{
    creature   *cr;

    for (cr = creaturelist() + 1 ; cr->id ; ++cr) {
	if (cr->hidden)
	    return cr;
    }
    if (cr - creaturelist() >= LYNX_MAX_CREATURES) {
	warn("Ran out of room in the creatures array!");
	return NULL;
    }
    if (ispedanticmode(state) && cr - creaturelist() >= PMAX_CREATURES)
	return NULL;

    cr->hidden = TRUE;
    cr[1].id = Nothing;
    creaturelistend() = cr;
    return cr;
}

/* Flag all tanks to turn around.
 */
static void turntanks(gamestate *state)
{
    creature   *cr;

    for (cr = creaturelist() ; cr->id ; ++cr) {
	if (cr->hidden)
	    continue;
	if (cr->id != Tank)
	    continue;
	if (floorat(state, cr->pos) == CloneMachine || isice(floorat(state, cr->pos)))
	    continue;
	cr->state ^= CS_REVERSE;
    }
}

/* Start an animation sequence at the spot (formerly) occupied by the
 * given creature. The creature's slot in the creature list is reused
 * by the animation sequence.
 */
static void removecreature(gamestate *state, creature *cr, int animationid)
{
    if (cr->id != Chip)
	removeclaim(state, cr->pos);
    if (cr->state & CS_PUSHED)
	stopsoundeffect(state, SND_BLOCK_MOVING);
    cr->id = animationid;
    cr->frame = ((state->currenttime + state->stepping) & 1) ? 12 : 11;
    --cr->frame;
    cr->hidden = FALSE;
    cr->state = 0;
    cr->tdir = NIL;
    if (cr->moving == 8) {
	cr->pos -= delta[cr->dir];
	cr->moving = 0;
    }
    markanimated(state, cr->pos);
}

/* End the given animation sequence (thus removing the final vestige
 * of an ex-creature).
 */
static void removeanimation(gamestate *state, creature *cr)
{
    cr->hidden = TRUE;
    clearanimated(state, cr->pos);
    if (cr == creaturelistend()) {
	cr->id = Nothing;
	--creaturelistend();
    }
}

/* Abort the animation sequence occuring at the given location.
 */
static int stopanimationat(gamestate *state, int pos)
{
    creature   *anim;

    for (anim = creaturelist() ; anim->id ; ++anim) {
	if (!anim->hidden && anim->pos == pos && isanimation(anim->id)) {
	    removeanimation(state, anim);
	    return TRUE;
	}
    }
    return FALSE;
}

/* What happens when Chip dies. reason indicates the cause of death.
 * also is either NULL or points to a creature that dies with Chip.
 */
static void removechip(gamestate *state, int reason, creature *also)
{
    creature  *chip = getchip(state);

    switch (reason) {
      case CHIP_DROWNED:
	addsoundeffect(state, SND_WATER_SPLASH);
	removecreature(state, chip, Water_Splash);
	break;
      case CHIP_BOMBED:
	addsoundeffect(state, SND_BOMB_EXPLODES);
	removecreature(state, chip, Bomb_Explosion);
	break;
      case CHIP_OUTOFTIME:
	removecreature(state, chip, Entity_Explosion);
	break;
      case CHIP_BURNED:
	addsoundeffect(state, SND_CHIP_LOSES);
	removecreature(state, chip, Entity_Explosion);
	break;
      case CHIP_COLLIDED:
	addsoundeffect(state, SND_CHIP_LOSES);
	removecreature(state, chip, Entity_Explosion);
	if (also && also != chip)
	    removecreature(state, also, Entity_Explosion);
	break;
    }

    resetfloorsounds(state, FALSE);
    startendgametimer();
    state->timeoffset = 1;
}

/*
 * The laws of movement across the various floors.
 *
 * Chip, blocks, and other creatures all have slightly different rules
 * about what sort of tiles they are permitted to cross. The following
 * lookup table encapsulates these rules. These rules are only a first
 * check; a creature may be generally permitted a particular type of
 * move but still be prevented in a specific situation.
 */

#define NWSE	(NORTH | WEST | SOUTH | EAST)

static struct { unsigned char chip, block, creature; } const movelaws[] = {
    /* Nothing */		{ 0, 0, 0 },
    /* Empty */			{ NWSE, NWSE, NWSE },
    /* Slide_North */		{ NWSE, NWSE, NWSE },
    /* Slide_West */		{ NWSE, NWSE, NWSE },
    /* Slide_South */		{ NWSE, NWSE, NWSE },
    /* Slide_East */		{ NWSE, NWSE, NWSE },
    /* Slide_Random */		{ NWSE, NWSE, NWSE },
    /* Ice */			{ NWSE, NWSE, NWSE },
    /* IceWall_Northwest */	{ SOUTH | EAST, SOUTH | EAST, SOUTH | EAST },
    /* IceWall_Northeast */	{ SOUTH | WEST, SOUTH | WEST, SOUTH | WEST },
    /* IceWall_Southwest */	{ NORTH | EAST, NORTH | EAST, NORTH | EAST },
    /* IceWall_Southeast */	{ NORTH | WEST, NORTH | WEST, NORTH | WEST },
    /* Gravel */		{ NWSE, NWSE, 0 },
    /* Dirt */			{ NWSE, 0, 0 },
    /* Water */			{ NWSE, NWSE, NWSE },
    /* Fire */			{ NWSE, NWSE, NWSE },
    /* Bomb */			{ NWSE, NWSE, NWSE },
    /* Beartrap */		{ NWSE, NWSE, NWSE },
    /* Burglar */		{ NWSE, 0, 0 },
    /* HintButton */		{ NWSE, 0, 0 },
    /* Button_Blue */		{ NWSE, NWSE, NWSE },
    /* Button_Green */		{ NWSE, NWSE, NWSE },
    /* Button_Red */		{ NWSE, NWSE, NWSE },
    /* Button_Brown */		{ NWSE, NWSE, NWSE },
    /* Teleport */		{ NWSE, NWSE, NWSE },
    /* Wall */			{ 0, 0, 0 },
    /* Wall_North */		{ NORTH | WEST | EAST,
				  NORTH | WEST | EAST,
				  NORTH | WEST | EAST },
    /* Wall_West */		{ NORTH | WEST | SOUTH,
				  NORTH | WEST | SOUTH,
				  NORTH | WEST | SOUTH },
    /* Wall_South */		{ WEST | SOUTH | EAST,
				  WEST | SOUTH | EAST,
				  WEST | SOUTH | EAST },
    /* Wall_East */		{ NORTH | SOUTH | EAST,
				  NORTH | SOUTH | EAST,
				  NORTH | SOUTH | EAST },
    /* Wall_Southeast */	{ SOUTH | EAST, SOUTH | EAST, SOUTH | EAST },
    /* HiddenWall_Perm */	{ 0, 0, 0 },
    /* HiddenWall_Temp */	{ NWSE, 0, 0 },
    /* BlueWall_Real */		{ NWSE, 0, 0 },
    /* BlueWall_Fake */		{ NWSE, 0, 0 },
    /* SwitchWall_Open */	{ NWSE, NWSE, NWSE },
    /* SwitchWall_Closed */	{ 0, 0, 0 },
    /* PopupWall */		{ NWSE, 0, 0 },
    /* CloneMachine */		{ 0, 0, 0 },
    /* Door_Red */		{ NWSE, 0, 0 },
    /* Door_Blue */		{ NWSE, 0, 0 },
    /* Door_Yellow */		{ NWSE, 0, 0 },
    /* Door_Green */		{ NWSE, 0, 0 },
    /* Socket */		{ NWSE, 0, 0 },
    /* Exit */			{ NWSE, 0, 0 },
    /* ICChip */		{ NWSE, 0, 0 },
    /* Key_Red */		{ NWSE, NWSE, NWSE },
    /* Key_Blue */		{ NWSE, NWSE, NWSE },
    /* Key_Yellow */		{ NWSE, 0, 0 },
    /* Key_Green */		{ NWSE, 0, 0 },
    /* Boots_Slide */		{ NWSE, 0, 0 },
    /* Boots_Ice */		{ NWSE, 0, 0 },
    /* Boots_Water */		{ NWSE, 0, 0 },
    /* Boots_Fire */		{ NWSE, 0, 0 },
    /* Block_Static */		{ 0, 0, 0 },
    /* Drowned_Chip */		{ 0, 0, 0 },
    /* Burned_Chip */		{ 0, 0, 0 },
    /* Bombed_Chip */		{ 0, 0, 0 },
    /* Exited_Chip */		{ 0, 0, 0 },
    /* Exit_Extra_1 */		{ 0, 0, 0 },
    /* Exit_Extra_2 */		{ 0, 0, 0 },
    /* Overlay_Buffer */	{ 0, 0, 0 },
    /* Floor_Reserved2 */	{ 0, 0, 0 },
    /* Floor_Reserved1 */	{ 0, 0, 0 }
};

/* Including the flag CMM_RELEASING in a call to canmakemove()
 * indicates that the creature in question is being moved out of a
 * beartrap or clone machine, moves that would normally be forbidden.
 * CMM_CLEARANIMATIONS causes animations in the destination square to
 * be immediately quelled. CMM_STARTMOVEMENT indicates that this is
 * the final check before movement begins, thus triggering side
 * effects such as exposing hidden walls. CMM_PUSHBLOCKS causes blocks
 * to be pushed when in the way of Chip. CMM_PUSHBLOCKSNOW causes
 * blocks to be pushed immediately, instead of waiting for the block's
 * turn to move.
 */
enum {
    CMM_RELEASING		= 0x0001,
    CMM_CLEARANIMATIONS		= 0x0002,
    CMM_STARTMOVEMENT		= 0x0004,
    CMM_PUSHBLOCKS		= 0x0008,
    CMM_PUSHBLOCKSNOW		= 0x0010
};

/* Return TRUE if the given block is allowed to be moved in the given
 * direction. If flags includes CMM_PUSHBLOCKSNOW, then the indicated
 * movement of the block will be initiated.
 */
static int canpushblock(gamestate *state, creature *block, int dir, int flags)
{
    _assert(block && block->id == Block);
    _assert(floorat(state, block->pos) != CloneMachine);
    _assert(dir != NIL);

    if (!canmakemove(state, block, dir, flags)) {
	if (!block->moving && (flags & (CMM_PUSHBLOCKS | CMM_PUSHBLOCKSNOW)))
	    block->dir = dir;
	return FALSE;
    }
    if (flags & (CMM_PUSHBLOCKS | CMM_PUSHBLOCKSNOW)) {
	block->dir = dir;
	block->tdir = dir;
	block->state |= CS_PUSHED;
	if (flags & CMM_PUSHBLOCKSNOW)
	    advancecreature(state, block, FALSE);
    }

    return TRUE;
}

/* Return TRUE if the given creature is allowed to attempt to move in
 * the given direction. Side effects can and will occur from calling
 * this function, as indicated by flags.
 */
static int canmakemove(gamestate *state, creature const *cr, int dir, int flags)
{
    creature   *other;
    int		floor;
    int		to, y, x;

    _assert(cr);
    _assert(dir != NIL);

    floor = floorat(state, cr->pos);
    switch (floor) {
      case Wall_North:         if (dir & NORTH) return FALSE;            break;
      case Wall_West:          if (dir & WEST) return FALSE;             break;
      case Wall_South:         if (dir & SOUTH) return FALSE;            break;
      case Wall_East:          if (dir & EAST) return FALSE;             break;
      case Wall_Southeast:     if (dir & (SOUTH | EAST)) return FALSE;   break;
      case IceWall_Northwest:  if (dir & (SOUTH | EAST)) return FALSE;   break;
      case IceWall_Northeast:  if (dir & (SOUTH | WEST)) return FALSE;   break;
      case IceWall_Southwest:  if (dir & (NORTH | EAST)) return FALSE;   break;
      case IceWall_Southeast:  if (dir & (NORTH | WEST)) return FALSE;   break;
      case Beartrap:
      case CloneMachine:
	if (!(flags & CMM_RELEASING))
	    return FALSE;
	break;
    }
    if (isslide(floor) && (cr->id != Chip || !possession(Boots_Slide))
		       && getslidedir(state, floor, FALSE) == back(dir))
	return FALSE;

    y = cr->pos / CXGRID;
    x = cr->pos % CXGRID;
    y += dir == NORTH ? -1 : dir == SOUTH ? +1 : 0;
    x += dir == WEST ? -1 : dir == EAST ? +1 : 0;
    to = y * CXGRID + x;

    if (x < 0 || x >= CXGRID)
	return FALSE;
    if (y < 0 || y >= CYGRID) {
	if (ispedanticmode(state)) {
	    if (flags & CMM_STARTMOVEMENT) {
		mapbreached() = TRUE;
		warn("map breach in pedantic mode at (%d %d)", x, y);
	    }
	}
	return FALSE;
    }

    floor = floorat(state, to);
    if (floor == SwitchWall_Open || floor == SwitchWall_Closed)
	floor ^= togglestate();

    if (cr->id == Chip) {
	if (!(movelaws[floor].chip & dir))
	    return FALSE;
	if (floor == Socket && chipsneeded() > 0)
	    return FALSE;
	if (isdoor(floor) && !possession(floor))
	    return FALSE;
	if (ismarkedanimated(state, to))
	    return FALSE;
	other = lookupcreature(state, to, FALSE);
	if (other && other->id == Block) {
	    if (!canpushblock(state, other, dir, flags & ~CMM_RELEASING))
		return FALSE;
	}
	if (floor == HiddenWall_Temp || floor == BlueWall_Real) {
	    if (flags & CMM_STARTMOVEMENT)
		floorat(state, to) = Wall;
	    return FALSE;
	}
    } else if (cr->id == Block) {
	if (cr->moving > 0)
	    return FALSE;
	if (!(movelaws[floor].block & dir))
	    return FALSE;
	if (islocationclaimed(state, to))
	    return FALSE;
	if (flags & CMM_CLEARANIMATIONS)
	    if (ismarkedanimated(state, to))
		stopanimationat(state, to);
    } else {
	if (!(movelaws[floor].creature & dir))
	    return FALSE;
	if (islocationclaimed(state, to))
	    return FALSE;
	if (floor == Fire && cr->id != Fireball)
	    return FALSE;
	if (flags & CMM_CLEARANIMATIONS)
	    if (ismarkedanimated(state, to))
		stopanimationat(state, to);
    }

    return TRUE;
}

/*
 * How everyone selects their move.
 */

/* This function embodies the movement behavior of all the creatures.
 * Given a creature, this function enumerates its desired direction
 * of movement and selects the first one that is permitted.
 */
static void choosecreaturemove(gamestate *state, creature *cr)
{
    int		choices[4] = { NIL, NIL, NIL, NIL };
    int		dir, pdir;
    int		floor;
    int		y, x, m, n;

    if (isanimation(cr->id))
	return;

    cr->tdir = NIL;
    if (cr->id == Block)
	return;
    if (getfdir(cr) != NIL)
	return;
    floor = floorat(state, cr->pos);
    if (floor == CloneMachine || floor == Beartrap) {
	cr->tdir = cr->dir;
	return;
    }

    dir = cr->dir;
    pdir = NIL;

    _assert(dir != NIL);

    switch (cr->id) {
      case Tank:
	choices[0] = dir;
	break;
      case Ball:
	choices[0] = dir;
	choices[1] = back(dir);
	break;
      case Glider:
	choices[0] = dir;
	choices[1] = left(dir);
	choices[2] = right(dir);
	choices[3] = back(dir);
	break;
      case Fireball:
	choices[0] = dir;
	choices[1] = right(dir);
	choices[2] = left(dir);
	choices[3] = back(dir);
	break;
      case Bug:
	choices[0] = left(dir);
	choices[1] = dir;
	choices[2] = right(dir);
	choices[3] = back(dir);
	break;
      case Paramecium:
	choices[0] = right(dir);
	choices[1] = dir;
	choices[2] = left(dir);
	choices[3] = back(dir);
	break;
      case Walker:
	choices[0] = dir;
	choices[1] = WALKER_TURN;
	break;
      case Blob:
	choices[0] = BLOB_TURN;
	break;
      case Teeth:
	if ((state->currenttime + state->stepping) & 4)
	    return;
	if (getchip(state)->hidden)
	    return;
	y = chippos() / CXGRID - cr->pos / CXGRID;
	x = chippos() % CXGRID - cr->pos % CXGRID;
	n = y < 0 ? NORTH : y > 0 ? SOUTH : NIL;
	if (y < 0)
	    y = -y;
	m = x < 0 ? WEST : x > 0 ? EAST : NIL;
	if (x < 0)
	    x = -x;
	if (x > y) {
	    choices[0] = m;
	    choices[1] = n;
	} else {
	    choices[0] = n;
	    choices[1] = m;
	}
	pdir = choices[0];
	break;
    }

    for (n = 0 ; n < 4 && choices[n] != NIL ; ++n) {
	if (choices[n] == WALKER_TURN) {
	    m = lynx_prng(state) & 3;
	    choices[n] = cr->dir;
	    while (m--)
		choices[n] = right(choices[n]);
	} else if (choices[n] == BLOB_TURN) {
	    int cw[4] = { NORTH, EAST, SOUTH, WEST };
	    choices[n] = cw[random4(mainprng())];
	}
	cr->tdir = choices[n];
	if (canmakemove(state, cr, choices[n], CMM_CLEARANIMATIONS))
	    return;
    }

    if (pdir != NIL)
	cr->tdir = pdir;
}

/* Determine the direction of Chip's next move. If discard is TRUE,
 * then Chip is not currently permitted to select a direction of
 * movement, and the player's input should not be retained.
 */
static void choosechipmove(gamestate *state, creature *cr, int discard)
{
    int	dir;
    int	f1, f2;

    chippushing() = FALSE;

    dir = state->currentinput;
    state->currentinput = NIL;
    if (!directionalcmd(dir))
	dir = NIL;

    if (dir == NIL || discard) {
	cr->tdir = NIL;
	return;
    }

    state->lastmove = dir;
    cr->tdir = dir;

    if (cr->tdir != NIL)
	dir = cr->tdir;
    else if (getfdir(cr) != NIL)
	dir = getfdir(cr);
    else
	return;

    if (isdiagonal(dir)) {
	if (cr->dir & dir) {
	    f1 = canmakemove(state, cr, cr->dir, CMM_PUSHBLOCKS);
	    f2 = canmakemove(state, cr, cr->dir ^ dir, CMM_PUSHBLOCKS);
	    dir = !f1 && f2 ? dir ^ cr->dir : cr->dir;
	} else {
	    if (canmakemove(state, cr, dir & (EAST | WEST), CMM_PUSHBLOCKS))
		dir &= EAST | WEST;
	    else
		dir &= NORTH | SOUTH;
	}
	cr->tdir = dir;
    } else {
	(void)canmakemove(state, cr, dir, CMM_PUSHBLOCKS);
    }
}

/* This function determines if the given creature is currently being
 * forced to move. (Ice, slide floors, and teleports are the three
 * possible causes of this. Bear traps and clone machines also cause
 * forced movement, but these are handled outside of the normal
 * movement sequence.) If so, the direction is stored in the
 * creature's fdir field, and TRUE is returned unless the creature can
 * override the forced move.
 */
static int getforcedmove(gamestate *state, creature *cr)
{
    int	floor;

    setfdir(cr, NIL);

    floor = floorat(state, cr->pos);

    if (state->currenttime == 0)
	return FALSE;

    if (isice(floor)) {
	if (cr->id == Chip && possession(Boots_Ice))
	    return FALSE;
	if (cr->dir == NIL)
	    return FALSE;
	setfdir(cr, cr->dir);
	return TRUE;
    } else if (isslide(floor)) {
	if (cr->id == Chip && possession(Boots_Slide))
	    return FALSE;
	setfdir(cr, getslidedir(state, floor, TRUE));
	return !(cr->state & CS_SLIDETOKEN);
    } else if (cr->state & CS_TELEPORTED) {
	cr->state &= ~CS_TELEPORTED;
	setfdir(cr, cr->dir);
	return TRUE;
    }

    return FALSE;
}

/* Return the move a creature will make on the current tick.
 */
static int choosemove(gamestate *state, creature *cr)
{
    if (cr->id == Chip) {
	choosechipmove(state, cr, getforcedmove(state, cr));
	if (cr->tdir == NIL && getfdir(cr) == NIL)
	    resetfloorsounds(state, FALSE);
    } else {
	if (getforcedmove(state, cr))
	    cr->tdir = NIL;
	else
	    choosecreaturemove(state, cr);
    }

    return cr->tdir != NIL || getfdir(cr) != NIL;
}

/* Update the location that Chip is currently moving into (and reset
 * the pointer to the creature that Chip is colliding with).
 */
static void checkmovingto(gamestate *state)
{
    creature   *cr;
    int		dir;

    cr = getchip(state);
    dir = cr->tdir;
    if (dir == NIL || isdiagonal(dir)) {
	chiptopos() = -1;
	chiptocr() = NULL;
	return;
    }

    chiptopos() = cr->pos + delta[dir];
    chiptocr() = NULL;
}

/*
 * Special movements.
 */

/* Teleport the given creature instantaneously from one teleport tile
 * to another.
 */
static int teleportcreature(gamestate *state, creature *cr)
{
    int pos, origpos;

    _assert(floorat(state, cr->pos) == Teleport);

    origpos = pos = cr->pos;

    for (;;) {
	--pos;
	if (pos < 0)
	    pos += CXGRID * CYGRID;
	if (floorat(state, pos) == Teleport) {
	    if (cr->id != Chip)
		removeclaim(state, cr->pos);
	    cr->pos = pos;
	    if (!islocationclaimed(state, pos) && canmakemove(state, cr, cr->dir, 0))
		break;
	    if (pos == origpos) {
		if (cr->id == Chip)
		    chipstuck() = TRUE;
		else
		    claimlocation(state, cr->pos);
		return FALSE;
	    }
	}
    }

    if (cr->id == Chip)
	addsoundeffect(state, SND_TELEPORTING);
    else
	claimlocation(state, cr->pos);
    cr->state |= CS_TELEPORTED;
    return TRUE;
}

/* Release a creature currently inside a clone machine. If the
 * creature successfully exits, a new clone is created to replace it.
 */
static int activatecloner(gamestate *state, int pos)
{
    creature   *cr;
    creature   *clone;

    if (pos < 0)
	return FALSE;
    if (pos >= CXGRID * CYGRID) {
	warn("Off-map cloning attempted: (%d %d)",
	     pos % CXGRID, pos / CXGRID);
	return FALSE;
    }
    if (floorat(state, pos) != CloneMachine) {
	warn("Red button not connected to a clone machine at (%d %d)",
	     pos % CXGRID, pos / CXGRID);
	return FALSE;
    }
    cr = lookupcreature(state, pos, TRUE);
    if (!cr)
	return FALSE;
    clone = newcreature(state);
    if (!clone)
	return advancecreature(state, cr, TRUE) != 0;

    *clone = *cr;
    if (advancecreature(state, cr, TRUE) <= 0) {
	clone->hidden = TRUE;
	return FALSE;
    }
    return TRUE;
}

/* Release any creature on a beartrap at the given location.
 */
static void springtrap(gamestate *state, int pos)
{
    creature   *cr;

    if (pos < 0)
	return;
    if (pos >= CXGRID * CYGRID) {
	warn("Off-map trap opening attempted: (%d %d)",
	     pos % CXGRID, pos / CXGRID);
	return;
    }
    if (floorat(state, pos) != Beartrap) {
	warn("Brown button not connected to a beartrap at (%d %d)",
	     pos % CXGRID, pos / CXGRID);
	return;
    }
    cr = lookupcreature(state, pos, TRUE);
    if (cr && cr->dir != NIL)
	advancecreature(state, cr, TRUE);
}

/*
 * When something actually moves.
 */

/* Initiate a move by the given creature. The direction of movement is
 * given by the tdir field, or the fdir field if tdir is NIL.
 * releasing must be TRUE if the creature is moving out of a bear trap
 * or clone machine. +1 is returned if the creature succeeded in
 * moving, 0 is returned if the move could not be initiated, and -1 is
 * returned if the creature was killed in the attempt.
 */
static int startmovement(gamestate *state, creature *cr, int releasing)
{
    creature   *other;
    int		dir;
    int		floorfrom;

    _assert(cr->moving <= 0);

    if (cr->tdir != NIL)
	dir = cr->tdir;
    else if (getfdir(cr) != NIL)
	dir = getfdir(cr);
    else
	return 0;
    _assert(!isdiagonal(dir));

    cr->dir = dir;
    floorfrom = floorat(state, cr->pos);

    if (cr->id == Chip) {
	_assert(!cr->hidden);
	if (!possession(Boots_Slide)) {
	    if (isslide(floorfrom) && cr->tdir == NIL)
		cr->state |= CS_SLIDETOKEN;
	    else if (!isice(floorfrom) || possession(Boots_Ice))
		cr->state &= ~CS_SLIDETOKEN;
	}
    }

    if (!canmakemove(state, cr, dir, CMM_PUSHBLOCKSNOW
					| CMM_CLEARANIMATIONS
					| CMM_STARTMOVEMENT
					| (releasing ? CMM_RELEASING : 0))) {
	if (cr->id == Chip) {
	    if (!couldntmove()) {
		couldntmove() = TRUE;
		addsoundeffect(state, SND_CANT_MOVE);
	    }
	    chippushing() = TRUE;
	}
	if (isice(floorfrom) && (cr->id != Chip || !possession(Boots_Ice))) {
	    cr->dir = back(dir);
	    applyicewallturn(state, cr);
	}
	return 0;
    }
    if (mapbreached() && chipisalive()) {
	removechip(state, CHIP_COLLIDED, cr);
	return -1;
    }

    if (floorfrom == CloneMachine || floorfrom == Beartrap)
	_assert(releasing);

    if (cr->id != Chip) {
	removeclaim(state, cr->pos);
	if (cr->id != Block && cr->pos == chiptopos())
	    chiptocr() = cr;
    } else if (chiptocr() && !chiptocr()->hidden) {
	chiptocr()->moving = 8;
	removechip(state, CHIP_COLLIDED, chiptocr());
	return -1;
    }

    cr->pos += delta[dir];
    if (cr->id != Chip)
	claimlocation(state, cr->pos);

    cr->moving += 8;

    if (cr->id != Chip && cr->pos == chippos() && !getchip(state)->hidden) {
	removechip(state, CHIP_COLLIDED, cr);
	return -1;
    }
    if (cr->id == Chip) {
	couldntmove() = FALSE;
	other = lookupcreature(state, cr->pos, FALSE);
	if (other) {
	    removechip(state, CHIP_COLLIDED, other);
	    return -1;
	}
    }

    if (cr->state & CS_PUSHED) {
	chippushing() = TRUE;
	addsoundeffect(state, SND_BLOCK_MOVING);
    }

    return +1;
}

/* Continue the given creature's move.
 */
static int continuemovement(gamestate *state, creature *cr)
{
    int	floor, speed;

    if (isanimation(cr->id))
	return TRUE;

    _assert(cr->moving > 0);

    if (cr->id == Chip && chipstuck())
	return TRUE;

    speed = cr->id == Blob ? 1 : 2;
    floor = floorat(state, cr->pos);
    if (isslide(floor) && (cr->id != Chip || !possession(Boots_Slide)))
	speed *= 2;
    else if (isice(floor) && (cr->id != Chip || !possession(Boots_Ice)))
	speed *= 2;
    cr->moving -= speed;
    cr->frame = cr->moving / 2;
    return cr->moving > 0;
}

/* Complete the movement of the given creature. Most side effects
 * produced by moving onto a tile occur at this point. FALSE is
 * returned if the creature is removed by the time the function
 * returns.
 */
static int endmovement(gamestate *state, creature *cr)
{
    int	floor;
    int	survived = TRUE;

    if (isanimation(cr->id))
	return TRUE;

    _assert(cr->moving <= 0);

    floor = floorat(state, cr->pos);

    if (cr->id != Chip || !possession(Boots_Ice))
	applyicewallturn(state, cr);

    if (cr->id == Chip) {
	switch (floor) {
	  case Water:
	    if (!possession(Boots_Water)) {
		removechip(state, CHIP_DROWNED, NULL);
		survived = FALSE;
	    }
	    break;
	  case Fire:
	    if (!possession(Boots_Fire)) {
		removechip(state, CHIP_BURNED, NULL);
		survived = FALSE;
	    }
	    break;
	  case Dirt:
	  case BlueWall_Fake:
	    floorat(state, cr->pos) = Empty;
	    addsoundeffect(state, SND_TILE_EMPTIED);
	    break;
	  case PopupWall:
	    floorat(state, cr->pos) = Wall;
	    addsoundeffect(state, SND_WALL_CREATED);
	    break;
	  case Door_Red:
	  case Door_Blue:
	  case Door_Yellow:
	  case Door_Green:
	    _assert(possession(floor));
	    if (floor != Door_Green)
		--possession(floor);
	    floorat(state, cr->pos) = Empty;
	    addsoundeffect(state, SND_DOOR_OPENED);
	    break;
	  case Key_Red:
	  case Key_Blue:
	  case Key_Yellow:
	  case Key_Green:
	    if (possession(floor) == 255)
		possession(floor) = -1;
	    /*FALLTHROUGH*/
	  case Boots_Ice:
	  case Boots_Slide:
	  case Boots_Fire:
	  case Boots_Water:
	    ++possession(floor);
	    floorat(state, cr->pos) = Empty;
	    addsoundeffect(state, SND_ITEM_COLLECTED);
	    break;
	  case Burglar:
	    possession(Boots_Ice) = 0;
	    possession(Boots_Slide) = 0;
	    possession(Boots_Fire) = 0;
	    possession(Boots_Water) = 0;
	    addsoundeffect(state, SND_BOOTS_STOLEN);
	    break;
	  case ICChip:
	    if (chipsneeded())
		--chipsneeded();
	    floorat(state, cr->pos) = Empty;
	    addsoundeffect(state, SND_IC_COLLECTED);
	    break;
	  case Socket:
	    _assert(chipsneeded() == 0);
	    floorat(state, cr->pos) = Empty;
	    addsoundeffect(state, SND_SOCKET_OPENED);
	    break;
	  case Exit:
	    cr->hidden = TRUE;
	    completed() = TRUE;
	    addsoundeffect(state, SND_CHIP_WINS);
	    break;
	}
    } else if (cr->id == Block) {
	switch (floor) {
	  case Water:
	    floorat(state, cr->pos) = Dirt;
	    addsoundeffect(state, SND_WATER_SPLASH);
	    removecreature(state, cr, Water_Splash);
	    survived = FALSE;
	    break;
	  case Key_Blue:
	    floorat(state, cr->pos) = Empty;
	    break;
	}
    } else {
	switch (floor) {
	  case Water:
	    if (cr->id != Glider) {
		addsoundeffect(state, SND_WATER_SPLASH);
		removecreature(state, cr, Water_Splash);
		survived = FALSE;
	    }
	    break;
	  case Key_Blue:
	    floorat(state, cr->pos) = Empty;
	    break;
	}
    }

    if (!survived)
	return FALSE;

    switch (floor) {
      case Bomb:
	floorat(state, cr->pos) = Empty;
	if (cr->id == Chip) {
	    removechip(state, CHIP_BOMBED, NULL);
	} else {
	    addsoundeffect(state, SND_BOMB_EXPLODES);
	    removecreature(state, cr, Bomb_Explosion);
	}
	survived = FALSE;
	break;
      case Beartrap:
	addsoundeffect(state, SND_TRAP_ENTERED);
	break;
      case Button_Blue:
	turntanks(state);
	addsoundeffect(state, SND_BUTTON_PUSHED);
	break;
      case Button_Green:
	togglestate() ^= SwitchWall_Open ^ SwitchWall_Closed;
	addsoundeffect(state, SND_BUTTON_PUSHED);
	break;
      case Button_Red:
	if (activatecloner(state, clonerfrombutton(state, cr->pos)))
	    addsoundeffect(state, SND_BUTTON_PUSHED);
	break;
      case Button_Brown:
	addsoundeffect(state, SND_BUTTON_PUSHED);
	break;
    }

    return survived;
}

/* Advance the movement of the given creature. If the creature is not
 * currently moving but should be, movement is initiated. If the
 * creature completes their movement, any and all appropriate side
 * effects are applied. If releasing is TRUE, the movement is occuring
 * out-of-turn, as with movement across an open beatrap or an
 * activated clone machine. The return value is +1 if the creature
 * successfully moved (or successfully remained stationary), 0 if the
 * creature tried to move and failed, or -1 if the creature was killed
 * and exists no longer.
 */
static int advancecreature(gamestate *state, creature *cr, int releasing)
{
    char	tdir = NIL;
    int		f;

    if (cr->moving <= 0 && !isanimation(cr->id)) {
	if (releasing) {
	    _assert(cr->dir != NIL);
	    tdir = cr->tdir;
	    cr->tdir = cr->dir;
	} else if (cr->tdir == NIL && getfdir(cr) == NIL)
	    return +1;
	f = startmovement(state, cr, releasing);
	if (f < 0)
	    return f;
	if (f == 0) {
	    if (releasing)
		cr->tdir = tdir;
	    return 0;
	}
	cr->tdir = NIL;
    }

    if (!continuemovement(state, cr)) {
	if (!endmovement(state, cr))
	    return -1;
    }

    return +1;
}

#ifndef NDEBUG

/*
 * Debugging functions.
 */

/* Print out a rough image of the level and the list of creatures.
 */
static void dumpmap(gamestate *state)
{
    creature   *cr;
    int		y, x;

    for (y = 0 ; y < CXGRID * CYGRID ; y += CXGRID) {
	for (x = 0 ; x < CXGRID ; ++x)
	    fprintf(stderr, "%02X%c", state->map[y + x].top.id,
		    (state->map[y + x].top.state ?
		     state->map[y + x].top.state & 0x40 ? '*' : '.' : ' '));
	fputc('\n', stderr);
    }
    fputc('\n', stderr);
    for (cr = creaturelist() ; cr->id ; ++cr)
	fprintf(stderr, "%02X%c%1d (%d %d)%s%s%s\n",
			cr->id, "-^<?v?\?\?>"[(int)cr->dir],
			cr->moving, cr->pos % CXGRID, cr->pos / CXGRID,
			cr->hidden ? " dead" : "",
			cr->state & CS_SLIDETOKEN ? " slide-token" : "",
			cr->state & CS_REVERSE ? " reversing" : "");
    fflush(stderr);
}

/* Run various sanity checks on the current game state.
 */
static void verifymap(gamestate *state)
{
    creature   *cr;
    int		pos;

    for (pos = 0 ; pos < CXGRID * CYGRID ; ++pos) {
	if (state->map[pos].top.id >= 0x40)
	    warn("%d: Undefined floor %d at (%d %d)",
		 state->currenttime, state->map[pos].top.id,
		 pos % CXGRID, pos / CXGRID);
	if (state->map[pos].top.state & 0x80)
	    warn("%d: Undefined floor state %02X at (%d %d)",
		 state->currenttime, state->map[pos].top.id,
		 pos % CXGRID, pos / CXGRID);
    }

    for (cr = creaturelist() ; cr->id ; ++cr) {
	if (isanimation(state->map[cr->pos].top.id)) {
	    if (cr->moving > 12)
		warn("%d: Too-large animation frame %02X at (%d %d)",
		     state->currenttime, cr->moving,
		     cr->pos % CXGRID, cr->pos / CXGRID);
	    continue;
	}
	if (cr->id < 0x40 || cr->id >= 0x80)
	    warn("%d: Undefined creature %d:%d at (%d %d)",
		 state->currenttime, cr - creaturelist(), cr->id,
		 cr->pos % CXGRID, cr->pos / CXGRID);
	if (cr->pos < 0 || cr->pos >= CXGRID * CYGRID)
	    warn("%d: Creature %d:%d has left the map: %04X",
		 state->currenttime, cr - creaturelist(), cr->id, cr->pos);
	if (isanimation(cr->id))
	    continue;
	if (cr->dir > EAST && (cr->dir != NIL || cr->id != Block))
	    warn("%d: Creature %d:%d moving in illegal direction (%d)",
		 state->currenttime, cr - creaturelist(), cr->id, cr->dir);
	if (cr->dir == NIL)
	    warn("%d: Creature %d:%d lacks direction",
		 state->currenttime, cr - creaturelist(), cr->id);
	if (cr->moving > 8)
	    warn("%d: Creature %d:%d has a moving time of %d",
		 state->currenttime, cr - creaturelist(), cr->id, cr->moving);
	if (cr->moving < 0)
	    warn("%d: Creature %d:%d has a negative moving time: %d",
		 state->currenttime, cr - creaturelist(), cr->id, cr->moving);
    }
}

#endif

/*
 * Per-tick maintenance functions.
 */

/* Actions and checks that occur at the start of every tick.
 */
static void initialhousekeeping(gamestate *state)
{
    creature   *chip;
    creature   *cr;
    int		pos;

#ifndef NDEBUG
    verifymap(state);
#endif

    if (state->currenttime == 0) {
	lastrndslidedir = rndslidedir();
	state->laststepping = state->stepping;
    }

    chip = getchip(state);
    if (chip->id == Pushing_Chip)
	chip->id = Chip;

    if (!inendgame()) {
	if (completed()) {
	    startendgametimer();
	    state->timeoffset = 1;
	} else if (state->timelimit && state->currenttime >= state->timelimit) {
	    removechip(state, CHIP_OUTOFTIME, NULL);
	}
    }

    for (cr = creaturelist() ; cr->id ; ++cr) {
	if (cr->hidden)
	    continue;
	if (cr->state & CS_REVERSE) {
	    cr->state &= ~CS_REVERSE;
	    if (cr->moving <= 0)
		cr->dir = back(cr->dir);
	}
    }
    for (cr = creaturelist() ; cr->id ; ++cr) {
	if (cr->state & CS_PUSHED) {
	    if (cr->hidden || cr->moving <= 0) {
		stopsoundeffect(state, SND_BLOCK_MOVING);
		cr->state &= ~CS_PUSHED;
	    }
	}
    }

    if (togglestate()) {
	for (pos = 0 ; pos < CXGRID * CYGRID ; ++pos) {
	    if (floorat(state, pos) == SwitchWall_Open
				|| floorat(state, pos) == SwitchWall_Closed)
		floorat(state, pos) ^= togglestate();
	}
	togglestate() = 0;
    }

#ifndef NDEBUG
    if (state->currentinput == CmdDebugCmd2) {
	dumpmap(state);
	exit(0);
	state->currentinput = NIL;
    } else if (state->currentinput == CmdDebugCmd1) {
	static int mark = 0;
	warn("Mark %d (%d).", ++mark, state->currenttime);
	state->currentinput = NIL;
    }
    if (state->currentinput >= CmdCheatNorth && state->currentinput <= CmdCheatStuff) {
	switch (state->currentinput) {
	  case CmdCheatNorth:		--yviewoffset();		break;
	  case CmdCheatWest:		--xviewoffset();		break;
	  case CmdCheatSouth:		++yviewoffset();		break;
	  case CmdCheatEast:		++xviewoffset();		break;
	  case CmdCheatHome:		xviewoffset()=yviewoffset()=0;	break;
	  case CmdCheatStuff:
	    possession(Key_Red) = 127;
	    possession(Key_Blue) = 127;
	    possession(Key_Yellow) = 127;
	    possession(Key_Green) = 127;
	    possession(Boots_Ice) = 127;
	    possession(Boots_Slide) = 127;
	    possession(Boots_Fire) = 127;
	    possession(Boots_Water) = 127;
	    chipsneeded() = chipsneeded() ? 0 : 1;
	    break;
	}
	state->currentinput = NIL;
	setnosaving(state);
    }
#endif

    chiptopos() = -1;
    chiptocr() = NULL;
}

/* Actions and checks that occur at the end of every tick.
 */
static void finalhousekeeping(gamestate *state)
{
    (void)state;
    return;
}

/* Set the state fields specifically used to produce the output.
 */
static void preparedisplay(gamestate *state)
{
    creature   *chip;
    int		floor;

    chip = getchip(state);
    floor = floorat(state, chip->pos);

    state->xviewpos = (chip->pos % CXGRID) * 8 + xviewoffset() * 8;
    state->yviewpos = (chip->pos / CXGRID) * 8 + yviewoffset() * 8;
    if (chip->moving) {
	switch (chip->dir) {
	  case NORTH:	state->yviewpos += chip->moving;	break;
	  case WEST:	state->xviewpos += chip->moving;	break;
	  case SOUTH:	state->yviewpos -= chip->moving;	break;
	  case EAST:	state->xviewpos -= chip->moving;	break;
	}
    }

    if (!chip->hidden) {
	if (floor == HintButton && chip->moving <= 0)
	    showhint(state);
	else
	    hidehint(state);
	if (chip->id == Chip && chippushing())
	    chip->id = Pushing_Chip;
	if (chip->moving) {
	    resetfloorsounds(state, FALSE);
	    floor = floorat(state, chip->pos);
	    if (floor == Fire && possession(Boots_Fire))
		addsoundeffect(state, SND_FIREWALKING);
	    else if (floor == Water && possession(Boots_Water))
		addsoundeffect(state, SND_WATERWALKING);
	    else if (isice(floor)) {
		if (possession(Boots_Ice))
		    addsoundeffect(state, SND_ICEWALKING);
		else if (floor == Ice)
		    addsoundeffect(state, SND_SKATING_FORWARD);
		else
		    addsoundeffect(state, SND_SKATING_TURN);
	    } else if (isslide(floor)) {
		if (possession(Boots_Slide))
		    addsoundeffect(state, SND_SLIDEWALKING);
		else
		    addsoundeffect(state, SND_SLIDING);
	    }
	}
    }
}

/*
 * The functions provided by the gamelogic struct.
 */

/* Initialize the gamestate structure to the state at the beginning of
 * the level, using the data in the associated gamesetup structure.
 * The level map is decoded and assembled, the list of creatures is
 * drawn up, and other miscellaneous initializations are performed.
 */
static int initgame(gamelogic *logic)
{
    gamestate 	       *state;
    creature		crtemp;
    creature	       *cr;
    mapcell	       *cell;
    xyconn	       *xy;
    int			pos, num, n;

    state = logic->state;
    num = state->game->number;
    creaturelist() = creaturearray() + 1;
    cr = creaturelist();

    if (ispedanticmode(state))
	if (state->statusflags & SF_BADTILES)
	    markinvalid(state);

    n = -1;
    for (pos = 0, cell = state->map ; pos < CXGRID * CYGRID ; ++pos, ++cell) {
	if (cell->top.id == Block_Static)
	    cell->top.id = crtile(Block, NORTH);
	if (cell->bot.id == Block_Static)
	    cell->bot.id = crtile(Block, NORTH);
	if (ismsspecial(cell->top.id) && cell->top.id != Exited_Chip) {
	    cell->top.id = Wall;
	    if (ispedanticmode(state))
		markinvalid(state);
	}
	if (ismsspecial(cell->bot.id) && cell->bot.id != Exited_Chip) {
	    cell->bot.id = Wall;
	    if (ispedanticmode(state))
		markinvalid(state);
	}
	if (cell->bot.id != Empty) {
	    if (!isfloor(cell->bot.id) || isfloor(cell->top.id)) {
		warn("level %d: invalid \"buried\" tile at (%d %d)",
		     num, pos % CXGRID, pos / CXGRID);
		markinvalid(state);
	    }
	}

	if (iscreature(cell->top.id)) {
	    cr->pos = pos;
	    cr->id = creatureid(cell->top.id);
	    cr->dir = creaturedirid(cell->top.id);
	    cr->moving = 0;
	    cr->hidden = FALSE;
	    if (cr->id == Chip) {
		if (n >= 0) {
		    warn("level %d: multiple Chips on the map!", num);
		    markinvalid(state);
		}
		n = cr - creaturelist();
		cr->dir = SOUTH;
		cr->state = 0;
	    } else {
		cr->state = 0;
		claimlocation(state, pos);
	    }
	    setfdir(cr, NIL);
	    cr->tdir = NIL;
	    cr->frame = 0;
	    ++cr;
	    cell->top.id = cell->bot.id;
	    cell->bot.id = Empty;
	}
	if (ispedanticmode(state))
	    if (cell->top.id == Wall_North || cell->top.id == Wall_West)
		markinvalid(state);
    }

    if (n < 0) {
	warn("level %d: Chip isn't on the map!", num);
	markinvalid(state);
	n = cr - creaturelist();
	cr->pos = 0;
	cr->hidden = TRUE;
	++cr;
    }
    cr->pos = -1;
    cr->id = Nothing;
    cr->dir = NIL;
    creaturelistend() = cr - 1;
    if (n) {
	cr = creaturelist();
	crtemp = cr[0];
	cr[0] = cr[n];
	cr[n] = crtemp;
    }

    for (xy = traplist(), n = traplistsize() ; n ; --n, ++xy) {
	if (xy->from >= CXGRID * CYGRID || xy->to >= CXGRID * CYGRID) {
	    warn("Level %d: ignoring off-map beartrap wiring", num);
	    xy->from = -1;
	} else if (floorat(state, xy->from) != Button_Brown) {
	    warn("Level %d: invalid beartrap wiring: no button at (%d %d)",
		 num, xy->from % CXGRID, xy->to / CXGRID);
	} else if (floorat(state, xy->to) != Beartrap) {
	    warn("Level %d: disabling miswired beartrap button at (%d %d)",
		 num, xy->to % CXGRID, xy->to / CXGRID);
	    xy->from = -1;
	}
    }
    for (xy = clonerlist(), n = clonerlistsize() ; n ; --n, ++xy) {
	if (xy->from >= CXGRID * CYGRID || xy->to >= CXGRID * CYGRID) {
	    warn("Level %d: ignoring off-map cloner wiring", num);
	    xy->from = -1;
	} else if (floorat(state, xy->from) != Button_Red) {
	    warn("Level %d: invalid cloner wiring: no button at (%d %d)",
		 num, xy->from % CXGRID, xy->to / CXGRID);
	} else if (floorat(state, xy->to) != CloneMachine) {
	    warn("Level %d: disabling miswired cloner button at (%d %d)",
		 num, xy->to % CXGRID, xy->to / CXGRID);
	    xy->from = -1;
	}
    }

    possession(Key_Red) = possession(Key_Blue)
			= possession(Key_Yellow)
			= possession(Key_Green) = 0;
    possession(Boots_Ice) = possession(Boots_Slide)
			  = possession(Boots_Fire)
			  = possession(Boots_Water) = 0;

    resetendgametimer();
    togglestate() = 0;
    couldntmove() = FALSE;
    chippushing() = FALSE;
    chipstuck() = FALSE;
    mapbreached() = FALSE;
    completed() = FALSE;
    chiptopos() = -1;
    chiptocr() = NULL;
    prngvalue1() = 0;
    prngvalue2() = 0;
    rndslidedir() = lastrndslidedir;
    state->stepping = state->laststepping;
    xviewoffset() = 0;
    yviewoffset() = 0;

    preparedisplay(state);
    return !ismarkedinvalid(state);
}

/* Advance the game state by one tick.
 */
static int advancegame(gamelogic *logic)
{
    gamestate  *state;
    creature   *cr;

    state = logic->state;

    initialhousekeeping(state);

    for (cr = creaturelistend() ; cr >= creaturelist() ; --cr) {
	setfdir(cr, NIL);
	cr->tdir = NIL;
	if (cr->hidden)
	    continue;
	if (isanimation(cr->id)) {
	    --cr->frame;
	    if (cr->frame < 0)
		removeanimation(state, cr);
	    continue;
	}
	if (cr->moving <= 0)
	    choosemove(state, cr);
    }

    cr = getchip(state);
    if (getfdir(cr) == NIL && cr->tdir == NIL)
	couldntmove() = FALSE;
    else
	checkmovingto(state);

    for (cr = creaturelistend() ; cr >= creaturelist() ; --cr) {
	if (cr->hidden)
	    continue;
	if (advancecreature(state, cr, FALSE) < 0)
	    continue;
	cr->tdir = NIL;
	setfdir(cr, NIL);
	if (floorat(state, cr->pos) == Button_Brown && cr->moving <= 0)
	    springtrap(state, trapfrombutton(state, cr->pos));
    }

    for (cr = creaturelistend() ; cr >= creaturelist() ; --cr) {
	if (cr->hidden)
	    continue;
	if (cr->moving)
	    continue;
	if (floorat(state, cr->pos) == Teleport)
	    teleportcreature(state, cr);
    }

    finalhousekeeping(state);

    preparedisplay(state);

    if (inendgame()) {
	--state->timeoffset;
	if (!decrendgametimer()) {
	    resetfloorsounds(state, TRUE);
	    return completed() ? +1 : -1;
	}
    }

    return 0;
}

/* Free resources associated with the current game state.
 */
static int endgame(gamelogic *logic)
{
    (void)logic;
    return TRUE;
}

/* Free all allocated resources for this module.
 */
static void shutdown(gamelogic *logic)
{
    (void)logic;
}

/* The exported function: Initialize and return the module's gamelogic
 * structure.
 */
gamelogic *lynxlogicstartup(void)
{
    static gamelogic	logic;

    lastrndslidedir = NORTH;

    logic.ruleset = Ruleset_Lynx;
    logic.initgame = initgame;
    logic.advancegame = advancegame;
    logic.endgame = endgame;
    logic.shutdown = shutdown;

    return &logic;
}
