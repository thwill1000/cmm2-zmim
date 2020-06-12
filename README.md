# Z-MIM
A Z-machine interpreter to allow the classic Infocom works of interactive fiction to be played on the Colour Maximite 2.

Written in MMBasic 5.05 by Thomas Hugo Williams in 2019-2020

## How do I run it?

 - Copy all the files to ```A:/ZMIM/```
    - to run from a different directory you need to edit the ```INSTALL_DIR``` string in ```src/main.bas```.
 - `run "zmim.bas"`

## Story Files

The games/adventures for Z-machines are called "story files" and come in several versions z1, z2, z3, .. z8

Z-MIM currently only supports z3 format as used by the classic 1980's Infocom games.

Z-MIM is distributed with four free story files that Infocom created:
 - tutorial.z3 - Introduction to interactive fiction and a little bit of Zork I
 - minizork.z3 - A nice big chunk of Zork I that was given away with the British Commodore users’ magazine “Zzap! 64″ no. 67. in 1990.
 - sampler1.z3 - Samples of Planetfall, Infidel, and The Witness.
 - sampler2.z3 - Samples of Zork I, Leather Goddesses of Phobos, and Trinity

Other story files can be found legitimately on the internet:
 - hhgg.z3 - The Hitchikers Guide To The Galaxy
    - http://www.douglasadams.com/creations/hhgg.z3
    - link is to a file on the official Douglas Adams website. 
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

The z3 compatible Infocom classic stories are:
 - ballyhoo.z3 - Ballyhoo
 - cutthroat.z3 - Cuthroats
 - enchanter.z3 - Enchanter
 - hollywood.z3 - Hollywood Hijinx
 - lgop.z3 - Leather Goddesses of Phobos
 - lurking.z3 - The Lurking Horror
 - planetfall.z3 - Planetfall
 - plundered.z3 - Plundered Hearts
 - seastalker.z3 - Seastalker
 - sorcerer.z3 - Sorcerer
 - spellbrkr.z3 - Spellbreaker
 - wishbringer.z3 - Wishbringer
 - infidel.z3 - Infidel
 - zork1.z3 - Zork: The Great Underground Empire - Part I
 - zork2.z3 - Zork II: The Wizard of Frobozz
 - zork3.z3 - Zork III: The Dungeon Master
 
*These files can be extracted from the several anthologies that have been published over the years, e.g. "The Lost Treasures of Infocom"*

## Limitations

Z-MIM currently has the following limitations:

1. Only works with .z3 format stories.
2. No status bar:
    - type `*status` to see current location, score and moves made.
3. No word wrapping or paging, i.e. "More..."
4. Non standard support for scripts:
    - user is prompted to write a script when the story starts.
    - type `*replay` to play-back a script.
5. No split screen, as used (optionally?) by Seastalker.
6. No sound support, as used (optionally?) by The Lurking Horror.

## FAQ

**1. What is a Colour Maximite 2 ?**

The Colour Maximite 2 is an as yet not widely publicised homage to 1980's and early 1990's micro-computers - see https://www.thebackshed.com/forum/ViewTopic.php?FID=16&TID=12105

**2. Will you be supporting the original Colour Maximite / Mono Maximite ?**

Perhaps.

I started developing this on the original Colour Maximite and when I switched to the CMM2 I tried to ensure I kept the memory footprint small and used only backward compatible features (or those that could be easily translated).

However a good Z-machine implementation needs to run at more than 1000 Z-machine instructions per second. Currently Z-MIM runs at about 550 instructions per second and early tests on the original Colour Maximite suggest it can manage about 20 instructions per second, so it may be possible to port back, but most likely will be unplayable ... or at least comparable to playing it over a very slow dial-up modem.

**3. Will you be supporting .z4, .z5, etc. story formats ?**

Perhaps. It depends on whether anyone finds this useful, or how bored I get.

**4. What possessed you to write this in BASIC ?**

This is currently the only option on the Colour Maximite 2 unless you want to rewrite/replace the firmware to include an ARM Z-machine implementation.

**5. How do I play these games ?**

Run "tutorial.z3" or read something like https://www.z-machine-matter.com/playing.html 

**6. How do I find out more about Interactive Fiction ?**

Visit https://intfiction.org/

**7. How do I find out more about the Z-machine ?**

The Z-machine standard documents can be found at https://www.inform-fiction.org/zmachine/standards/

**8. How do I contact the author ?**

The author can be contacted via:
 - https://github.com as user "thwill1000"
 - https://www.thebackshed.com/forum/index.php as user "thwill"
