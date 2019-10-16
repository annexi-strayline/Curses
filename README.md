# Introduction

The Curses library provides a very high-level **thick** binding for the common "ncurses" terminal control library common on most *nixes.

The library has the following key features:
* **Fully task-safe**, and allows for concurrent interface driving
* Full color support for xterm and xterm-256color, including palette changing ability (though this rarely supported by terminal emulators in the wild)
* Non signal-based, synchronous dynamic terminal resizing support with hooks
* Designed to drive multiple terminals simultaneously
  * Includes a unified color space shared amongst all terminals
  * Supports cross-terminal Surface (window, pad) transcription
* Abstracted "Surface" type represents traditional curses Windows, with fully automatic re-rendering on update, including visibility computation (automatic layering).
* The Curses package does not use allocators anywhere (however the (n)curses library itself likely does use allocations from the heap). All objects are stack allocated, unless explicitly created with a user-defined allocator.

# State of Release
This package is currently in **pre-release alpha** and is still very much under development. 

Core functionality (Surface primitives) is complete and working. Work on higher-level UI abstractions is underway.

**The interface is subject to breaking changes until further notice**

## Open issues
* Gnome terminal can't handle Set_Background with a Colored_Cursor. Artifacts appear during refresh. Almost all other tested terminals work fine (xterm, uxrvt, konsole).

## Work queue
- [ ] Higher-level UI abstractions
- [ ] "Dialog boxes", Menus, Forms, etc.
- [ ] Expanded documentation
- [ ] Examples

# Building and using
This library is presented as a gpr "library project" - libnadacurses.gpr.

The library can be built directly with grpbuild, or included within another project file.

The library project has two primary configuration properties \(set with -XProperty=..\), as follows:

1. HOST_OS

   * FreeBSD
   * Solaris
   * Linux
     Linux should have a distribution set if relevent, which shall be one of the following:
     * Ubuntu \(Default\)

2. WIDE_SUPPORT

   * NO \(Default\)
     No wide character (unicode) support. This links with the regular ncurses library.

   * YES
     Includes full wide character support. This requires the ncursesw library. When using this option, ensure the terminal supports wide character or UTF-8 encoding, and that "locale" for the terminal is set appropriately (eg. en_US.UTF-8).

# Alpha test-drive

* Refer to the extensive comments in the Curses package to understand basic Surface operations
* Refer to the Curses.Terminals for the Terminal type and comments for setting up a Terminal
* Refer to Curses.Terminals.Surfaces.Standard (Curses.Standard) for the basic Screen and Window primitive Surfaces
* Refer to Curses.Terminals.Color for enabling Color options (especially via the Colored_Cursor type)

Here is a basic example program which places a filled window in the centre of the screen, and prints "Hello World!" with centered justification.

```
with Curses;           use Curses;
with Curses.Terminals; use Curses.Terminals;
with Curses.Standard;  use Curses.Standard;

with Curses.Terminals.Surfaces;
with Curses.Device.Environment;

procedure Example is
   TTY: aliased Terminal (Curses.Device.Environment.Environment_Terminal);
   subtype Control_Character is Curses.Terminals.Surfaces.Control_Character;
begin
   TTY.Attach;

   declare
      Main_Screen: Screen       := New_Screen (TTY);
      My_Window  : Window'Class := Main_Screen.New_Window 
        (Proposed_Extents => (Row => 4, Column => 40));
      
      Input_Char: Control_Character;
      -- This will be centered on the screen, of size 4x40
      
      Fill_Cursor: Cursor := (Style    => (Inverted => True, others => <>),
                              others   => <>);
   begin
      
      My_Window.Set_Background (Fill_Cursor => Fill_Cursor);
      
      My_Window.Position_Cursor ( (Row    => 2,
                                   Column => (My_Window.Extents.Column / 2)) );
      
      My_Window.Put (Content        => "Type 'x' to exit.",
                     Justify        => Center,
                     Advance_Cursor => True);
      
      
      My_Window.Position_Cursor ( (Row => 3, Column => 2) );
      My_Window.Put (Content => ">",
                     Advance_Cursor => True);
      
      My_Window.Show;
      -- New windows are hidden by default
      
      loop
         Input_Char := My_Window.Input_Key;
         exit when Input_Char.Class = Graphic
           and then Input_Char.Key = 'x';
         
         if Input_Char.Class = Graphic 
           and then My_Window.Current_Cursor.Position < My_Window.Extents 
         then
            My_Window.Put (Content        => String'(1..1 => Input_Char.Key),
                           Advance_Cursor => True);
         end if;
      end loop;
      
      -- That's it, the Curses package will automatically shut everything down
      -- for you!
   end; 
end Example;
```

This example code can be found in the root directory under Tests/example.adb. A gprbuild project file can be found in the root directory as example.gpr

Compile as follows (gnat must be installed):
```
$ gprbuild -p -P example.adb -XHOST_OS=[Your host OS]
```

HOST_OS Must be set to one of the three currently supported OS types:
1. "Linux"
2. "FreeBSD"
3. "Solaris"


This binding has been tested to work as is on
- FreeBSD
- Linux
- Solaris (illumos)

