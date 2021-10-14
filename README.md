# G-Code Postprocessor

G-Code postprocessor created for add extra features not present on RepetierHost
with CuraEngine.

This can be called as a filter after the slicing inside RepetierHost, and can be
called itself for an existing G-Code.

Some of the work here depends on the comments generated by CuraEngine. If you're
using it outside RepetierHost, make sure that they are present.

## Features

### Show layer info
Write in the printer display the current layer. This is activated with the
command `layerinfo`.

### Power off bed
Sometimes the piece is glued enough to the print bed, and the bed doesn't need
to be hot after some layers. It can be used with the command `poweroffbed`,
providing the layer where the bed should be powered off.

### Change filament
When we're using 3D printers with only one extruder, we can change the filament
during the printing in order to print multicolor pieces. It can be done manually
(pausing, lifiting the extruder, changing the filament, lowering again the
extruder and resuming), or inserting `M600` in the resulting G-Code.

This feature inserts `M600` in the layers where we want to make a filament
change. It can be used with the command `changefilament`, providing the layers
where the filament should be changed.

### Speed change
Some objects has some detailed layers that need to be printed slower, sometimes
some layers can be printed faster as they don't have too much details. You can
manually change the speed through the printer controls, or inserting `M220` in
the G-Code. This feature makes automates that insertion.

You can use it using `speedChange`. The arguments should be a layer and the
desired percentual speed on it and the next ones. You can pass multiple pairs of
layer and percentage.

## Usage

### Dependencies

- Glasgow Haskell Compiler (`ghc`)
- `make`
- `Spĺit` (`haskell-split` in Arch Linux/Manjaro)

### Compiling

Inside the project root, run `make`.

### Running

#### CLI
`./postprocessor INPUT OUTPUT [args]`

#### RepetierHost
Add `<project_path>/postprocessor #in #out [args]` in Config > Printer Settings >
Advanced > Filter Path and Parameter. Check "Run Filter after every Slice".

### Arguments

You can pass the arguments directly when using CLI. You can use more than one
command, like the following:

`./postprocessor in.gcode out.gcode layerinfo changefilament 10 30 poweroffbed
20`

You can also insert the arguments as comments in the G-Code, like the
following:

~~~gcode
;POSTPROCESS layerinfo
;POSTPROCESS poweroffbed 4
~~~

In the case of passing the arguments inside the comments, you should use only
`auto` as the comment (i.e. `./postprocessor in.gcode out.gcode auto`).
