# DCS

DCS is a Driven-Cavity Simulator code.

## Authors

Stefano Zaghi and Emilio Campana

[CNR-INSEAN](http://www.insean.cnr.it/) _Marine Technology Research Institute_, via di Vallerano 139, 00128 Rome, Italy

## Goals

DCS is a didactic code aimed at presenting a practical implementation of some simple numerical methods for solving 2D Navier-Stokes equations. It constitutes supplementary educational material of other scientific disclosure efforts.

## Copyrights

DCS is an open source project, it is distributed under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html). Anyone is interest to use, to develop or to contribute to DCS is welcome.

## Documentation

Detailed documentation of the Application Program Interface (API) can be found on the [GitHub pages](http://szaghi.github.com/DCS/index.html) of the project. In what follows a brief guide is reported.

### Mathematical and Numerical Models

The 2D incompressible Navier-Stokes equations are written into the non dimensional streamfunction (s) and vorticty (v) formulation:

![equation](http://www.texify.com/img/%5CLARGE%5C%21%5Cbegin%7Bmatrix%7D%5Cfrac%7B%5Cpartial%5E2s%7D%7B%5Cpartial%20x%5E2%7D%2B%20%5Cfrac%7B%5Cpartial%5E2s%7D%7B%5Cpartial%20y%5E2%7D%3D-v%5C%5C%20%5Cfrac%7B1%7D%7BRe%7D%5Cfrac%7B%5Cpartial%5E2v%7D%7B%5Cpartial%20x%5E2%7D%2B%5Cfrac%7B1%7D%7BRe%7D%20%5Cfrac%7B%5Cpartial%5E2v%7D%7B%5Cpartial%20y%5E2%7D%3D%20%5Cfrac%7B%5Cpartial%20s%7D%7B%5Cpartial%20y%7D%5Cfrac%7B%5Cpartial%20v%7D%7B%5Cpartial%20x%7D-%5Cfrac%7B%5Cpartial%20s%7D%7B%5Cpartial%20x%7D%5Cfrac%7B%5Cpartial%20v%7D%7B%5Cpartial%20y%7D%5Cend%7Bmatrix%7D.gif)

### Implementation Details

DCS is written in pure Fortran 2003 standard, thus the only requirement is a compiler supporting modern Fortran standards. Presently, the GNU gfortran compiler and the Intel Fortran compiler are officially supported by DCS. DCS project contains a makefile in standard GNU Make able to compile DCS with both GNU gfortran and Intel Fortran compilers.

### Simple Usage

Firstly, DCS must be locally downloaded either by means of Zip archive provided from Github repository or by cloning DCS repository:
```shell
git clone https://github.com/szaghi/DCS
```
Secondly, DCS must be compiled. To compile DCS a suitable makefile is provided. This makefile has a rule for printing its own usage:
```shell
make help
```
This will print into the console the following message:
```shell
 Make options of DCS code

 Compiler choice: COMPILER=intel => default
  COMPILER=gnu   => GNU gfortran
  COMPILER=intel => Intel Fortran

 Compiling options
  DEBUG=yes(no)     => on(off) debug                  (default no)
  F03STD=yes(no)    => on(off) check standard fortran (default yes)
  OPTIMIZE=yes(no)  => on(off) optimization           (default no)
  OPENMP=yes(no)    => on(off) OpenMP directives      (default no)

 Provided Rules: default=DCS => compile the code
  cleanobj     => cleaning compiled object
  cleanmod     => cleaning .mod files
  cleanmsg     => cleaning make-log massage files
  clean        => running cleanobj, cleanmod and cleanmsg
  cleanall     => running clean and cleanexe
  tar          => creating a tar archive of the project
  doc          => building the documentation
```
The options should be self-explanatory. For a standard, non-debug compilation just type `make`. A successful compilation should print the following message:
```shell
 Compiler used  intel => ifort
 Source dir     ./src/
  Debug          no
  F-standard     yes
  Optimize       no

 Compiling options
 [-cpp -c -module ./mod/ -static -assume protect_parens -assume norealloc_lhs -fp-model source -std03 ]

 Linking options
 [ -std03 ]

 Compiling IR_Precision.f90
 Compiling Data_Type_Conservative.f90
 Compiling Data_Type_Vector.f90
 Compiling Data_Type_Node.f90
 Compiling Data_Type_Face.f90
 Compiling Data_Type_Cell_Quad.f90
 Compiling Data_Type_Mesh.f90
 Compiling Data_Type_Cavity.f90
 Compiling DCS.f90

 Assembling DCS
```
After DCS has been compiled, just run it without arguments and a help message will be prompted:
```shell
 Error: no argument has been passed to command line
 DCS
 Driven-Cavity Simulator code
 Usage:
 ./DCS -N number_of_cells [optionals parameters]

 Optional parameters:
 -Re Reynolds_number => default 500
 -beta over_relaxation_parameter => default 0.6
 -rtol residual_tollerance => default 10^-4
 -nout standard_output_update_frequency => default 1000 iterations
 -oform output_file_format => "tec" for Tecplot output or "gnu" for gnuplot one, default "tec"

 Example:
 ./DCS -Re 9.d2 -beta 0.5d0 -N 256 -nout 10000 -rtol 1.d-6
```
The options should be self-explanatory. An output file named `DCS_out.xxx` is created after the simulation is finished. Accordingly to the command line arguments used a Gnuplot (`DCS_out.gnu`) or Tecplot (`DCS_out.tec`) file is created. Two layout are provided within DCS, namely `stream.gnu` and `stream.lay` for visualizing the _stream_ function in the Gnuplot or Tecplot format respectively.
