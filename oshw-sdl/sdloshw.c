/* sdloshw.c: Top-level SDL management functions.
 *
 * Copyright (C) 2001-2004 by Brian Raiter, under the GNU General Public
 * License. No warranty. See COPYING for details.
 */

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	"SDL.h"
#include	"sdlgen.h"
#include	"../err.h"

/* Values global to this library.
 */
oshwglobals	sdlg;

/* This is an automatically-generated file, which contains a
 * representation of the program's icon.
 */
#include	"ccicon.c"

/* Dispatch all events sitting in the SDL event queue. 
 */
static void _eventupdate(int wait)
{
    SDL_Event	event;

    if (wait)
	SDL_WaitEvent(NULL);
    SDL_PumpEvents();
    while (SDL_PeepEvents(&event, 1, SDL_GETEVENT, SDL_ALLEVENTS)) {
	switch (event.type) {
	  case SDL_KEYDOWN:
	    SDL_ShowCursor(SDL_DISABLE);
	    keyeventcallback(event.key.keysym.sym, TRUE);
	    if (event.key.keysym.unicode
			&& event.key.keysym.unicode != event.key.keysym.sym)
		keyeventcallback(event.key.keysym.unicode, TRUE);
	    break;
	  case SDL_KEYUP:
	    SDL_ShowCursor(SDL_DISABLE);
	    keyeventcallback(event.key.keysym.sym, FALSE);
	    if (event.key.keysym.unicode
			&& event.key.keysym.unicode != event.key.keysym.sym)
		keyeventcallback(event.key.keysym.unicode, FALSE);
	    break;
	  case SDL_MOUSEMOTION:
	  case SDL_MOUSEBUTTONUP:
	  case SDL_MOUSEBUTTONDOWN:
	    SDL_ShowCursor(SDL_ENABLE);
	    break;
	  case SDL_QUIT:
	    exit(EXIT_SUCCESS);
	}
    }
}

/* Alter the window decoration.
 */
void setsubtitle(char const *subtitle)
{
    char	buf[270];

    if (subtitle && *subtitle) {
	sprintf(buf, "Tile World - %.255s", subtitle);
	SDL_WM_SetCaption(buf, "Tile World");
    } else {
	SDL_WM_SetCaption("Tile World", "Tile World");
    }
}

/* Shut down SDL.
 */
static void shutdown(void)
{
    SDL_Quit();
}

/* Initialize SDL, create the program's icon, and then initialize
 * the other modules of the library.
 */
int oshwinitialize(int silence, int soundbufsize,
		   int showhistogram, int fullscreen)
{
    SDL_Surface	       *icon;

    sdlg.eventupdatefunc = _eventupdate;

    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
	errmsg(NULL, "Cannot initialize SDL system: %s\n", SDL_GetError());
	return FALSE;
    }
    atexit(shutdown);

    setsubtitle(NULL);

    icon = SDL_CreateRGBSurfaceFrom(cciconimage, CXCCICON, CYCCICON,
				    32, 4 * CXCCICON,
				    0x0000FF, 0x00FF00, 0xFF0000, 0);
    if (icon) {
	SDL_WM_SetIcon(icon, cciconmask);
	SDL_FreeSurface(icon);
    } else
	warn("couldn't create icon surface: %s", SDL_GetError());

    return _sdltimerinitialize(showhistogram)
	&& _sdltextinitialize()
	&& _sdltileinitialize()
	&& _sdlinputinitialize()
	&& _sdloutputinitialize(fullscreen)
	&& _sdlsfxinitialize(silence, soundbufsize);
}
