
<#

.SYNOPSIS
Use SML/NJ to run a Standard ML program defined in a .mlb file

.EXAMPLE
smlrun example.mlb datafile.txt
Run the Standard ML program defined in example.mlb using SML/NJ, passing datafile.txt as an argument to the program

#>

if ($args.Count -lt 1) {
   "Usage: smlrun file.mlb [args...]"
   exit 1
}

$mlb = $args[0]

if ($mlb -notmatch "[.]mlb$") {
   "Error: argument must be a .mlb file"
   exit 1
}

$libdir = "c:/Users/Chris/Documents/mlton-20150109-all-mingw-492/lib/sml"

$lines = (Get-Content $mlb)

# remove incompatible Basis lib and unneeded call to main
$lines = $lines -notmatch "basis[.]mlb" -notmatch "main[.]sml"

# remove ML-style comments
$lines = $lines -replace "\(\*[^\*]*\*\)",""

# expand library path
$lines = $lines -replace "\$\(SML_LIB\)",$libdir

# remove leading whitespace
$lines = $lines -replace "^ *",""

# remove trailing whitespace
$lines = $lines -replace " *$",""

# remove empty lines
$lines = $lines -notmatch "^$"

# add use declarations
$lines = $lines -replace "^(.*)$",'use "$1";'

$intro = @"
val smlrun__cp = 
    let val x = !Control.Print.out in
        Control.Print.out := { say = fn _ => (), flush = fn () => () };
        x
    end;
val smlrun__prev = ref "";
Control.Print.out := { 
    say = fn s => 
        (if String.isSubstring "Error" s 
         then (Control.Print.out := smlrun__cp;
               (#say smlrun__cp) (!smlrun__prev);
               (#say smlrun__cp) s)
         else (smlrun__prev := s; ())),
    flush = fn s => ()
};
"@ -split "[\r\n]+"

$outro = @"
val _ = main ();
val _ = OS.Process.exit (OS.Process.success);
"@ -split "[\r\n]+"

$script = @()
$script += $intro
$script += $lines
$script += $outro

$tmpfile = ([System.IO.Path]::GetTempFileName()) -replace "[.]tmp",".sml"

$tmpfile

$script | Out-File -Encoding "ASCII" $tmpfile

$env:CM_VERBOSE="false"

sml $tmpfile $args[1,$args.Length]

del $tmpfile

