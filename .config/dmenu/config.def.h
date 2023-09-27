/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 0;                      /* -b  option; if 0, dmenu appears at bottom */
static int center = 1;                      /* -c  option; if 0, dmenu won't be centered on the screen */
static int min_width = 400;                 /* minimum width when centered */
static const int vertpad = 8;              /* vertical padding of bar */
static const int sidepad = 8;              /* horizontal padding of bar */
/* -fn option overrides fonts[0]; default X11 font or font set */
static char *fonts[] =
{
	"Iosevka Nerd Font:size=10"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */


static
char *colors[][2] = {
	/*               fg         bg       */
	[SchemeNorm] = { "#bbbbbb", "#222222" },
	[SchemeSel]  = { "#eeeeee", "#ac4f4f" },
	[SchemeOut]  = { "#000000", "#222222" },
	[SchemeBorder] = { "#000000", "#111111" },
	[SchemeSelHighlight]  = { "#ffeeee", "#222222" },
	[SchemeNormHighlight] = { "#ffeeee", "#222222" },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 9;
/* -g option; if nonzero, dmenu uses a grid comprised of columns and lines */
static unsigned int columns    = 1;
static unsigned int lineheight = 22;         /* -h option; minimum height of a menu line     */
static unsigned int min_lineheight = 8;

/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";

/* Size of the window border */
static unsigned int border_width = 3;

