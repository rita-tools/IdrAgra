# Readme #
## Project description ##
### IdrAgra - Idrologia Agraria (in Italian) ###
IdrAgra is a distributed-parameter agro-hydrological model that allows the simulation of the irrigation water distribution in agricultural areas and the estimation of the hydrological balance on a daily basis.
## Contact ##
prof. Claudio Gandolfi

email: claudio.gandolfi@unimi.it

## Contributors ##
* Anna Borghi %AB%
* Enrico Antonio Chiaradia %EAC% enrico.chiaradia@unimi.it
* Rachele Riva %RR%
* Paolo Soffietti %PS% paolo.soffietti@unimi.it

## Documentation

Visit [the official website](https://idragra.unimi.it/index.php?lang=en#resources) to download the IdrAgra manuals in pdf format.
Alternatively, you can browse the html-based documentation on [ReadTheDocs](https://idragra-v2.readthedocs.io/) (**Under development!**)

## Conventions ##
Please refer to the following note for reading the code and apply edits.
### Variable names ####
* t_* = soil water content, theta, dimensionless  [m^3/m^3]
* h_* = water height/depth specific volume always in millimeter [mm]
* v_* = water volume in cubic meter [m^3]
* q_* = discharge in cubic meter per second [m^3/s]

## Compile instruction ##
### Windows ###
#### Step 1: install MSYS2 (once) ####
MSYS2 is a collection of tools and libraries providing you with an easy-to-use build system.
1. download the installer from https://www.msys2.org
2. run the installer, enter the desired installation folder, leave other settings as default
3. when installation finished, run MSYS2
4. type the following commands in the MSYS2 console:
```Shell
pacman -Syu
pacman -Su
pacman -S --needed base-devel mingw-w64-x86_64-toolchain
pacman -S mingw-w64-x86_64-python3-pip mingw-w64-x86_64-python3-setuptools
/mingw64/bin/pip3 install fortran-language-server
pacman -S mingw-w64-x86_64-lapack
pacman -S git unzip zsh 
```
> [!NOTE]
> After step 5, the terminal will be closed and you have to reopen it. Step 7 will install a complete building toolchain, other steps install useful packages
#### Step 2: install Visual Studio Code (once) ####
Install Visual Studio Code, VSC, (not Visual Studio!) from https://code.visualstudio.com/
#### Step 3: install Visual Studio Code Extensions (once) ####
Launch VSC. In the side bar (left), click the “Extensions” icon (in the middle). The following extensions of VSC should be (already) installed:
- C/C++ extension
- Makefile extension 
- Code Runner
- Modern Fortran
- Data Preview (to view and analyze csv files, optional)
#### Step 4: edit settings (once) ####
In the side bar (left), click the "Manage" icon (bottom) , and select "Settings".
In the settings, search for "code runner" and set "on" the following three items:
- Code runner: run in terminal -> ON
- Code runner: save all file before run -> ON
- Code runner: save file before run -> ON
- Code runner: ignore selection -> true

#### Step 5: get the code from github ####
Open a new command terminal (Start menu -> cmd)

Go to the desired root folder (where the project folder will be saved):
```Shell
cd YOUR-ROOT-PATH
```

Clone your repo with the following git command:

```Shell
git clone https://github.com/rita-tools/IdrAgra_v2
```

Change your terminal into that new subdirectory:

```Shell
cd IdrAgra_v2
```
Open Visual Studio Code  and select File -> Open Folder

#### Step 6: compile the executable ####

In the terminal, type the following:

```Shell
make cleanall
make
```
A brand-new idragra executable should be created!

#### Step 6bis: alternative, run in debug mode ####

Go to the launch.json file under .vscode folder and replace the path with that to your idragra dataset (where idragra_parameters.txt exists):
```json
"cwd": "YOUR-PATH-TO-IDRAGRA-DATASET",
```

On the side bar (left), select Run and Debug. Then, click on the green triangle on the top. If necessary, select the debugger (gdb) Launch

### Linux (Ubuntu) ###
#### Step 1: install gfortran (once) ####
Commonly, gfortran (the compiler) is not installed by default. To install it, open the terminal and type:
```Shell
sudo apt install gfortran
```
#### Step 2: install Visual Studio Code (once) ####
The Ubuntu app center is the easy way to installVisual Studio Code, VSC.
#### Step 3: install Visual Studio Code Extensions (once) ####
Launch VSC. In the side bar (left), click the “Extensions” icon (in the middle). The following extensions of VSC should be (already) installed:
- C/C++ extension
- Makefile extension 
- Code Runner
- Modern Fortran
- Data Preview (to view and analyze csv files, optional)
#### Step 4: edit settings (once) ####
In the side bar (left), click the "Manage" icon (bottom) , and select "Settings".
In the settings, search for "code runner" and set "on" the following three items:
- Code runner: run in terminal -> ON
- Code runner: save all file before run -> ON
- Code runner: save file before run -> ON
- Code runner: ignore selection -> true

#### Step 5: set up GIT (once) ####
From the terminal, install git (if not already available):

```Shell
sudo apt-get install git
```

Set up github user credentials:

```Shell
git config --global user.name "your-user-name"
git config --global user.email "your@email.dummy"
```

#### Step 6: install the fortran language parser (once) ####

From the terminal (supposing python3 is not installed):

```Shell
sudo apt install python3-pip
pip install fortls --upgrade
```

#### Step 7: install make (once) ####

From the terminal (supposing make is not installed):

```Shell
sudo apt install make
```


#### Step 8: clone this repository ####

Clone your repo with the following git command:

```Shell
git clone --branch linux_comp https://github.com/rita-tools/IdrAgra_v2
```

Change your terminal into that new subdirectory:

```Shell
cd IdrAgra_v2
```
Open Visual Studio Code  and select File -> Open Folder

#### Step 9: compile the executable ####

In the terminal, type the following:

```Shell
make cleanall
make
```
A brand-new idragra executable should be created!

#### Step 9bis: alternative, run in debug mode ####

Go to the launch.json file under .vscode folder and replace the path with that to your idragra dataset (where idragra_parameters.txt exists):
```json
"cwd": "YOUR-PATH-TO-IDRAGRA-DATASET",
```

On the side bar (left), select Run and Debug. Then, click on the green triangle on the top. If necessary, select the debugger (gdb) Launch

#### IMPORTANT NOTES FOR LINUX ####

Capital letters are meaningful in Linux OS system (i.e. test.txt is different from Test.txt and test.TXT) so pay attention that the following files in your dataset are saved as follow:

<b>from the phenological folder</b>

* CropParam.dat
* CanopyRes.dat
* Kcb.dat
* H.dat
* Sr.dat
* LAI.dat
* CNvalue.dat
* fc.dat (lowercase)
* r_stress.dat (lowercase)
* WPadj.dat

<b>from the spatial data folder</b>

* domain.asc (lowercase)
* ThetaI_FC.asc
* ThetaII_FC.asc
* ThetaI_WP.asc
* ThetaII_WP.asc
* ThetaI_r.asc
* ThetaII_r.asc
* ThetaI_sat.asc
* ThetaII_sat.asc
* slope.asc
* hydr_cond.asc
* hydr_group.asc
* Ksat_I.asc
* Ksat_II.asc
* N_I.asc
* N_II.asc
* IC_thetaI.asc
* IC_thetaII.asc
* FC_thetaI.asc
* FC_thetaII.asc
* rice_soilparam.txt
* appl_eff.asc
* conv_eff.asc
* irr_units.asc
* irr_meth.asc
* waterdepth.asc
* CapRisePar_a3.asc
* CapRisePar_a4.asc
* CapRisePar_b1.asc
* CapRisePar_b2.asc
* CapRisePar_b3.asc
* CapRisePar_b4.asc
* soiluse.asc
* shapearea.asc
* meteo_123.asc