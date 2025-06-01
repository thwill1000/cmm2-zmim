# Z-MIM
A Z-machine interpreter for playing the classic Infocom works of interactive fiction.

Written in MMBasic v6.00 for the [Colour Maximite 2](http://geoffg.net/maximite.html), [PicoMite VGA/HDMI](https://geoffg.net/picomitevga.html), [PicoCalc](https://www.clockworkpi.com/picocalc), [MMBasic for Linux](https://github.com/thwill1000/mmb4l) and MMBasic for Windows.

Copyright (c) 2019-2025 Thomas Hugo Williams
* Z-machine technical advice was provided by Fredrik Ramsberg, co-author of [Ozmoo](https://github.com/johanberntsson/ozmoo).
* PicoCalc testing by Chris Stoddard.

You can do what you like with this code subject to the [LICENSE](LICENSE),<br/> but if you use it then perhaps you would like to buy me a coffee?

<a href="https://www.buymeacoffee.com/thwill"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" style="width:217px;"></a>

## How do I run it?

 - Download the latest release source code: https://github.com/thwill1000/mmbasic-zmim/releases/latest
 - Extract and rename folder to `zmim/`.
 - `RUN "zmim/zmim.bas"`

## Story files

The games/adventures for the Z-machine are called "story files" and come in several versions z1, z2, z3, ... z8

Z-MIM currently only supports .z3 format as used by the classic 1980's Infocom games.

Z-MIM is distributed with four free story files that were distributed by Infocom:
 - tutorial.z3 - Introduction to interactive fiction and a little bit of Zork I
 - minizork.z3 - A nice big chunk of Zork I that was given away with the British Commodore users’ magazine “Zzap! 64″ no. 67. in 1990.
 - sampler1.z3 - Samples of Planetfall, Infidel, and The Witness.
 - sampler2.z3 - Samples of Zork I, Leather Goddesses of Phobos, and Trinity

Additional .z3 files can be copied into the `stories/` sub-directory of the Z-MIM installation.

The Infocom classic stories are currently available for download from https://eblong.com/infocom:
 - Ballyhoo
 - Cuthroats
 - Enchanter
 - Hollywood Hijinx
 - Infidel
 - Leather Goddesses of Phobos
 - Planetfall
 - Plundered Hearts
 - Seastalker
 - Sorcerer
 - Spellbreaker
 - The Lurking Horror
 - The Hitchhiker's Guide to the Galaxy
 - Wishbringer
 - Zork: The Great Underground Empire - Part I
 - Zork II: The Wizard of Frobozz
 - Zork III: The Dungeon Master

_Note that only the .z3 file versions are compatible with Z-MIM._

__Disclaimer__ the copyright of the files posted on the above site rests with Activision and I neither condone nor condem their usage. That said, to their credit, Activision does not appear to be acting to close down the above site, perhaps they accept the argument that "the historical value of these documents to the IF community outweighs the rights of the legal owner".

In addition many third-party story files are available, see the [Interactive Fiction Database](https://ifdb.org/search?searchfor=tag%3Apunyinform&sortby=&pg=all), and also:
 - curses-r10.z3 - Curses, An Interactive Diversion by Graham Nelson.
    - https://ifarchive.org/if-archive/games/zcode/old/curses-r10.z3
    - this is highly regarded.
 - advent.z3 - Adventure aka Colossal Cave, the original 350 points version ported to ZIL by Jesse McGrew, based on earlier work by Graham Nelson and Dave Baggett, with some elements of the original Fortran version restored.
    - https://ifarchive.org/if-archive/games/zcode/advent.z3
 - catseye.z3 - Cat's Eye, Miniventure #2 by Dave Bernazzani, release 3. "When your uncle Xevion asks you for a favor, you simply can't refuse. But what mysteries lie in his strange house?"
    - https://ifarchive.org/if-archive/games/zcode/catseye.z3
 - dejavu.z3 - Deja Vu, An Interactive Demonstration by Graham Nelson.
    - https://ifarchive.org/if-archive/games/zcode/dejavu.z3
 - gussdeath.z3 - Guss's Death, an Exercise in Interactive Fiction, by Kyle Frownfelter.
    - https://ifarchive.org/if-archive/games/zcode/gussdeath.z3
 - moonglow.z3 - Moonglow, Miniventure #1 by Dave Bernazzani, release 3. "What do you do when a mysterious orb crash-lands in your field? It's not your typical day at the farm!"
    - https://ifarchive.org/if-archive/games/zcode/moonglow.z3

## Meta-Commands

The interpreter provides a number of internal meta-commands all prefixed with a `*`:

 - `*break`
     - Breaks currently running story into Z-MIM story debugger.
     - Once in the debugger type `h` for a list of debugger commands.
 - `*credits`
     - displays Z-MIM credits.
 - `*font {small | medium}`
     - Changes the font size.
     - Defaults to `medium`.
     - __KNOWN BUG__: using *FONT SMALL on the Colour Maximite 2 causes the prompt to "glitch" in a way I have been unable to reproduce in a small example.
 - `*more {on | off}`
     - `on`: enables output paging so that a `[MORE]` prompt is shown whenever more output than can be shown on a single screen is generated; this is only likely to happen when replaying a script, see `*replay` below.
     - `off`: disables output paging.
     - Defaults to `on`.
 - `*record`
     - `on`: prompts the user to select and name one of 10 script file slots and then starts to echo every subsequent command into the selected file.
     - `off`: halts recording.
 - `*replay`
     - `on`: prompts the user to select a script file slot and then replays the contents of that file as if the user was typing it at the prompt.
    - `off`: halts replaying.
        - Only makes sense if inserted manually into a script file to prevent it from replaying to its end.
 - `*restore`
     - __TAKE NOTE__: this is provided to support any stories that __DO NOT__ provide their own `restore` command (without the `*` prefix) which should be used in preference.
     - Restores story progress previously saved with the `*save` command.
 - `*save`
     - __TAKE NOTE__: this is provided to support any stories that __DO NOT__ provide their own `save` command (without the `*` prefix) which should be used in preference.
     - Saves story progress.
     - The save format is a naive dump of the story's stack and dynamic memory area, see [src/zsave.inc](src/zsave.inc).
     - __WARNING!__ mixing the use of `*save` and `*restore` with any `save` and `restore` commands provided by the story may produce "odd" behaviour even though the same format is used by both.
 - `*screenshot`
     - Generates a .BMP screenshot.
     - Not available on all platforms.
 - `*spin`
     - Enables progress spinner when the Z-machine is "thinking".
     - This initially defaults to `on` for PicoMite devices.
 - `*spin off`
     - Disables progress spinner.
 - `*status`
     - Prints the current story status.

## Limitations

Z-MIM currently has the following limitations:

1. Only works with .z3 format stories.
2. Non standard support for scripts:
    - type `*record` to start recording a script.
    - type `*replay` to play-back a script.
3. No split screen, as used (optionally?) by Seastalker.
4. No sound support, as used (optionally?) by The Lurking Horror.

## FAQ

**1. Will you be supporting .z4, .z5, etc. story formats ?**

Perhaps. It depends on whether anyone finds this useful, or how bored I get.

**2. What possessed you to write this in BASIC ?**

This is currently the only option on the Colour Maximite 2 unless you want to rewrite/replace the firmware to include an ARM Z-machine implementation.

**3. How do I play these games ?**

Try playing "tutorial.z3" or read https://www.z-machine-matter.com/playing.html 

**4. How do I find out more about Interactive Fiction ?**

Visit https://intfiction.org/

**5. How do I find out more about the Z-machine ?**

The Z-machine standard documents can be found at https://www.inform-fiction.org/zmachine/standards/

**6. How do I contact the author ?**

The author can be contacted via:
 - https://github.com as user "thwill1000"
 - https://www.thebackshed.com/forum/index.php as user "thwill"
