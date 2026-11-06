# Command line arguments

While IdrAgra's executable can be launched manually by double-clicking it, it usually recommended to launch it from a terminal (e.g. PowerShell) whose working directory is the project folder:

```powershell
C:\path_to_idragra\idragra.exe
```

This prevents the console from closing automatically at the end of the simulation, and, additionally, allows the user to add command line arguments to modify the program's behaviour.

## Supported options

| Short form | Long form | Current behavior |
|---|---|---|
| `-h` | `-help` | Print the option list and stop. |
| `-d` | `-default` | Print all compiled default parameter values and stop. |
| `-p` | `-preview` | Print the effective settings after the parameter file has been read, then continue the run. |
| `-v` | `-verbose` | Print additional diagnostic and progress messages. It does not enable result files. |
| `-s` | `-summary` | Write only irrigation maps from the normal periodic and annual map groups. Independently enabled yield, debug, and cell outputs are still written. |
| `-r` | `-reckless` | Doesn't ask for confirmation before deleting a pre-existing output folder. Useful for automated runs. |
| `-f my_file.txt` | `-filename my_file.txt` | Read settings from `my_file.txt` instead of `idragra_parameters.txt`. |

Options are case-insensitive. An unrecognized option that begins with `-` prints the help text and stops.