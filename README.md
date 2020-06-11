# Z-MIM
A Z-Machine Interpreter for the Colour Maximite

ZMIM is an attempt to write a Z-Machine Interpreter<sup>1</sup> that runs on the
[Colour Maximite](https://geoffg.net/maximite.html) computer.

**It is in its very early stages and completely unusable at the moment.**

The Colour Maximite is an unusual computer in that it is a 32-bit PIC
microcontroller (+ support components) that has been persuaded to
behave like an 8-bit Retro computer.

Without changing its firmware the only<sup>2</sup> development language available
for the Colour Maximite is an implementation of BASIC; MMBasic with ~80K of RAM.

## Story Files

The games/adventures for Z-Machines are called "Story files" and come in several versions z1, z2 .. z8

Z-MIM currently only supports z3 format as used by the classic 1980's Infocom games.

Z-MIM is distributed with four free story files that Infocom created:
 - tutorial.z3 - introduction to interactive fiction and a little bit of Zork I
 - minizork.z3 - a nice big chunk of Zork I that was given away with the British Commodore users’ magazine “Zzap! 64″ no. 67. in 1990.
 - sampler1.z3 - samples of Planetfall, Infidel, and The Witness.
 - sampler2.z3 - samples of Zork I, Leather Goddesses of Phobos, and Trinity

Other story files can be found legitimately on the internet:
 - hhgg.z3
 - [curses-r10.z3](https://ifarchive.org/if-archive/games/zcode/old/curses-r10.z3) - Curses, An Interactive Diversion by Graham Nelson
 
The names of the story files for the 1980's Infocom classics are:


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

**2. Will you be supporting the original Colour Maximite / Mono Maximite ?**

**3. Will you be supporting .z4, .z5, etc. story formats ?**

**4. What possessed you to write this in BASIC ?**

**5. How do I play these games ?**

**6. How do I find out more about Interactive Fiction ?**

**7. How do I find out more about the Z-machine ?**

**8. How do I contact the author ?**

##### Footnotes

<sup>1</sup> Currently only for version 3 files.

<sup>2</sup> The Colour Maximite is limited to Maximite BASIC v4.5, but its close cousin the Micromite micro-controller runs MMBasic v5 which does allow embedded C and MIPS assembler routines.
