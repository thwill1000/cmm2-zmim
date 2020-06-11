# Z-MIM
Z-MIM is a Z-Machine Interpreter that allows the classic 1980's Infocom interactive fiction titles to be played on the Colour Maximite 2.

It is written in MMBasic 5.05

## How do I run it?

 - Copy all the files to ```A:/ZMIM/```
    - to run from a different directory you need to edit the ```INSTALL_DIR``` string in ```src/main.bas```.
 - `run "zmim.bas"`

## Story Files

The games/adventures for Z-Machines are called "story files" and come in several versions z1, z2 .. z8

Z-MIM currently only supports z3 format as used by the classic 1980's Infocom games.

Z-MIM is distributed with four free story files that Infocom created:
 - tutorial.z3 - Introduction to interactive fiction and a little bit of Zork I
 - minizork.z3 - A nice big chunk of Zork I that was given away with the British Commodore users’ magazine “Zzap! 64″ no. 67. in 1990.
 - sampler1.z3 - Samples of Planetfall, Infidel, and The Witness.
 - sampler2.z3 - Samples of Zork I, Leather Goddesses of Phobos, and Trinity

Other story files can be found legitimately on the internet:
 - [hhgg.z3](http://www.douglasadams.com/creations/hhgg.z3) - The Hitchikers Guide To The Galaxy
    - link is to a file on the official Douglas Adams website. 
 - [advent.z3](https://ifarchive.org/if-archive/games/zcode/advent.z3) - Adventure aka Colossal Cave, the original 350 points version ported to ZIL by Jesse McGrew, based on earlier work by Graham Nelson and Dave Baggett, with some elements of the original Fortran version restored.
 - [catseye.z3](https://ifarchive.org/if-archive/games/zcode/catseye.z3) - Cat's Eye, Miniventure #2 by Dave Bernazzani, release 3. "When your uncle Xevion asks you for a favor, you simply can't refuse. But what mysteries lie in his strange house?"
 - [curses-r10.z3](https://ifarchive.org/if-archive/games/zcode/old/curses-r10.z3) - Curses, An Interactive Diversion by Graham Nelson.
 - [dejavu.z3](https://ifarchive.org/if-archive/games/zcode/dejavu.z3) - Deja Vu, An Interactive Demonstration by Graham Nelson.
 - [gussdeath.z3](https://ifarchive.org/if-archive/games/zcode/gussdeath.z3) - Guss's Death, an Exercise in Interactive Fiction, by Kyle Frownfelter.
 - [moonglow.z3](https://ifarchive.org/if-archive/games/zcode/moonglow.z3) - Moonglow, Miniventure #1 by Dave Bernazzani, release 3. "What do you do when a mysterious orb crash-lands in your field? It's not your typical day at the farm!"
 - and others ?

The names of the story files for the 1980's Infocom classics are:
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
 
*The files for these can be extracted from the several anthologies that have been published over the years, e.g. "The Lost Treasures of Infocom"*

## Limitations

Release 1 has the following limitations:

1. Only works with .z3 format stories.
2. No status bar:
    - type `*status` to see current location, score and moves made.
3. No word wrapping or paging, i.e. "More..."
4. Non standard support for scripts:
    - user is prompted to write a script when the story starts.
    - type `*replay` to read-back a script.
5. No split screen, as used (optionally?) by Seastalker.
6. No sound support, as used (optionally?) by The Lurking Horror.

## FAQ

**1. What is a Colour Maximite 2 ?**

The Colour Maximite 2 is an as yet not publicly announced homage to 1980's and early 1990's micro-computers.

**2. Will you be supporting the original Colour Maximite / Mono Maximite ?**

**3. Will you be supporting .z4, .z5, etc. story formats ?**

**4. What possessed you to write this in BASIC ?**

**5. How do I play these games ?**

**6. How do I find out more about Interactive Fiction ?**

**7. How do I find out more about the Z-machine ?**

**8. How do I contact the author ?**
