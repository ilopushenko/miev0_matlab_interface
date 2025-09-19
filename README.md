[![Zenodo](https://zenodo.org/badge/DOI/10.5281/zenodo.17069742.svg)](https://doi.org/10.5281/zenodo.17069741) [![MATLAB](https://img.shields.io/badge/MATLAB-File_Exchange-color=68CA66)](https://se.mathworks.com/matlabcentral/fileexchange/181979-matlab-interface-to-w-wiscombe-s-mie-scattering-program)
 
# MIEV0 MATLAB interface (MEX API)
MATLAB interface to Dr. Warren Wiscombe's MIEV0 Fortran program.

Original MIEV0 program is widely known to be one of the most well-tested codes for calculating electromagnetic scattering from a homogeneous sphere. Current repo aims at providing modern interface to this highly efficient and a de-facto reference Mie theory implementation, so that one could easily call it from MATLAB environment - either for research, or for education purposes.

## Features
All capabilities of the original MIEV0 program are retained. The following quantities are computed:
- scattering $Q_{sca}$ and extinction $Q_{ext}$ efficiencies;
- asymmetry factor $G_{qsc}$;
- forward- and backscatter amplitudes (SFORW and SBACK);
- scattering amplitudes vs. scattering angle for incident polarization perpendicular $S_1$ and parallel $S_2$ to the plane of scattering;
- coefficients in the Legendre polynomial expansions of either the unpolarized phase function or the polarized phase matrix (PMOM);
- and some other quantities related to polarized radiative transfer and resonance hitting.

More details on input parameters and output quantities are available in the original manual file authored by W. Wiscombe (```/wiscombe/MIEV.doc``` or converted version ```/wiscombe/MIEV.pdf```), as well as in comments within source code ```MIEV0.f```.
Very thorough description can be found in NCAR reports (```/wiscombe/NCARMieReport_Jun79.pdf``` and ```/wiscombe/NCARMieReport_Aug96.pdf```). One is also urged to refer to the original papers enumerated below.

I have made minor modifications to ```MIEV0.f``` and ```ErrPack.f``` source code files, mainly to enable support of double precision arithmetic and to ensure that MIEV0 error messages are properly displayed in MATLAB command window instead of crashing the whole thing. These changes are mainly reflected in changing data types from REAL and COMPLEX to REAL\*8 and COMPLEX\*16, correspondingly. Perhaps there was a more elegant way to do it, e.g. by using certain ifort compiler flags, but somehow attempting to do so in the MATLAB Command window has not been successful. As of now, even line numbering in MIEV0.f is completely the same as in the original file. Original files are kept for reference in the ```/wiscombe/``` folder. In addition, interface binaries based on the unmodified MIEV0.f are available in the ```/legacy/``` folder, built with the default single precision arithmetic. 

This interface has been intentionally implemented with a bit outdated non-interleaved MATLAB MEX API in order to ensure compatibility with older MATLAB releases.
Consequently, calls to the ```mlMIEV0.mexw64``` library are supported in all Windows MATLAB releases beginning with R2011a without imposing requirements for any additional software. 

Potentially, MEX library can also be compiled from the sources on other platforms (see general instructions below). Currently, I consider building binary files for Linux and Apple Silicon platrorms. Octave port might also be considered at a later time.


## Availability
At the moment, library is available as binary MEX file for several platforms. 

1. **Windows**. Built with non-interleaved MATLAB MEX API. Supports **MATLAB R2011a** and all subsequent releases. Binary files: 
   * Modern Windows PC (x64 platform): [`mlMIEV0.mexw64`](https://github.com/ilopushenko/miev0_matlab_interface/releases/download/v1.2/mlMIEV0.mexw64);
   * Legacy Windows PC (x86 platform): [`mlMIEV0.mexw32`](https://github.com/ilopushenko/miev0_matlab_interface/releases/download/v1.2/mlMIEV0.mexw32);
   * Also, see example MATLAB script: [`example_scattered_intensities.m`](https://github.com/ilopushenko/miev0_matlab_interface/releases/download/v1.2/example_scattered_intensities.m).

2. **Linux**. Built with interleaved MATLAB MEX API. Supports **MATLAB R2023b** and all subsequent releases. Release files:
   * Binary file (x64 platform): [`mlMIEV0.mexa64`](https://github.com/ilopushenko/miev0_matlab_interface/releases/download/v1.2/mlMIEV0.mexa64);
   * Corresponding [example script](https://github.com/ilopushenko/miev0_matlab_interface/releases/download/v1.2/example_scattered_intensities.m) (the same as for Windows).

3. **MATLAB Online** and, correspondingly, **MATLAB Mobile**. By downloading Linux binary `mlMIEV0.mexa64` and uploading it to MATLAB Drive, one can call it with MATLAB Online services. Another option is to directly open this library in MATLAB Online through Mathworks File Exchange.

4. One can try to build for another platform, where MATLAB is available. 

## Usage

In order to use the library, please download the binary file appropriate for your system. In the example below, we assume that version for **Windows x64** is used.

1. Download ```mlMIEV0.mexw64``` from the [Releases](https://github.com/ilopushenko/miev0_matlab_interface/releases) section and put it into your project folder.
2. Create a MATLAB script, in which input parameters will be defined. For example:
```
XX       = 20;                             % size parameter;
Crefin   = complex(1.33+0.0i);             % refractive index (imaginary part can be + or -, but internally a negative imaginary index is assumed)
Perfct   = false;                          % perfectly conducting sphere or not
Mimcut   = 1e-12;                          % (positive) value below which imaginary refractive index is regarded as zero
Anyang   = false;                          % true <=> any angles whatsoever may be input through Xmu. false <=> the angles are monotone increasing and mirror symmetric about 90 degrees
angles   = linspace(-180,180,361) + 90;    % array of angles for diagram computation
Xmu      = cosd(angles);                   % cosines of angles at which S1, S2 are to be evaluated
Numang   = int32(numel(Xmu));              % number of angles
Nmom     = int32(0);                       % highest Legendre moment PMOM to calculate, 0 if PMOM are not to be computed
Ipolzn   = int32(0);                       % how to compute PMOM (see details in /wiscombe/MIEV.pdf)
Momdim   = int32(5);                       % determines first dimension of PMOM. Must be given a value, even if NMOM = 0. Minimum: 1.
Prnt     = false;                          % print flags (useless in MATLAB implementation at this moment)
Verbose  = true;                           % print flags for debugging mlMIEV0 MEX interface
```
3. Call mlMIEV0 as you would call any MATLAB function with appropriate input and output argument list:
```
[Qext, Qsca, Gqsc, S1, S2, Sforw, Sback, Tforw, Tback, Spike, PMOM] = ...
mlMIEV0(XX, Crefin, Perfct, Mimcut, Anyang, Numang, Xmu, Nmom, Ipolzn, Momdim, Prnt, Verbose);
```
Argument list fully corresponds to the original MIEV0 parameter list, so you are welcome to refer to the original documentation available e.g. in ```/wiscombe/MIEV.pdf```. One exception is the last "Verbose" argument which enables some additional output from the MATLAB-Fortran MEX interface.

An example file for computing and plotting scattering intensities $S_1$ and $S_2$ is available in the repo root folder (see ```example_scattered_intensities.m```). This script should result in the following polar plot (made with MATLAB R2022a):

![result of the example script](./example_scattered_intensities.svg)

> [!WARNING]
> Please, pay attention to the following facts: 
> 1. Expression like `complex(1.33+0.0i)` always has to be used to define refractive index, even when imaginary part is zero! This is because Fortran expects parameter of the specific type (COMPLEX*16) as an input.
> 2. Similarly, all integers in MATLAB have to be explicitly specified as `int32`, e.g. `Momdim = int32(5)`. 
> 
> Inappropriate input definition will either crash your MATLAB instance, or lead to incorrect produced results!
> Example file `example_scattered_intensities.m`, bundled with each release, appropriately depicts all these differences.

## Validation
The implemented MEX library has mostly successfully undergone testing procedures outlined in the original ```/wiscombe/MVTstNew.f``` file. This subroutine features an exhaustive set of 19 test cases including extremely large size parameters, and covers most parts of the original MIEV0 program. To perform testing, pre-computed data contained within ```/wiscombe/MVTstNew.f``` was serialized into ```MVTstNew.mat``` file, and test script ```run_MVTstNew.m``` was written to compare the output from mlMIEV0.mexw64 with the reference quantities. One can confirm the good quality of the produced results by launching this script. Some small issues might remain: so far I have observed only one issue which is listed in the appropriate section below.

Additionally, the same test script was used to compare the results yielded by single precision version 1.0 of the library (see ```/legacy/single_precision/run_MVTstNew.m```), which apparently fails to pass the extreme testing scenarios. 

Overall, the main version of library with double precision performs very well and is suitable for scientific applications.  

## Known issues
- Not all messages of the original code, including those governed by the PRNT input flag, are currently displayed in the MATLAB Command window.
- For two extreme test cases Tback, Tforw and Sback parameters differ from pre-computed values more than all other parameters in average. Yet, the difference is not very large and appears to be within the reasonably acceptable range.

You are very welcome to report any issues if you encounter any. 

## Build
### Current build directions (interleaved MEX API)
Ideally, one should be able to build current version of the library with any MATLAB edition. However, there are some platform-specific advices.

**Windows build instructions**: Prerequisites (verified): Visual Studio 2019 Community Edition + Intel oneAPI 2021 HPC with Fortran Compiler + MATLAB R2022a. Should work with any combination of Visual Studio, Intel oneAPI and MATLAB product releases, as soon as your specific releases are compatible with each other. List of [MATLAB supported compilers](https://www.mathworks.com/support/requirements/previous-releases.html) can be a starting guide point.

1. Install Visual Studio first.
2. Install Intel oneAPI, and its HPC additional installer containing Fortran compiler. 
3. Install MATLAB (if you already had MATLAB prior to installing other tools, do not reinstall it, it will see the other distros).
4. Configure MATLAB to use Intel oneAPI Fortran Compiler by executing `mex -setup Fortran` in MATLAB Command Window.
5. Build the library by calling `mex -R2018a mlMIEV0.F90 MIEV0.f ErrPack.f`. Flag `-R2018a` ensures that interleaved MEX API is used. Produced file is usually compatible with all versions of MATLAB starting with the one in which it is built.

**Linux build directions**:
Prerequisites (verified): Ubuntu 22.04 LTS + GNU gfortran-10 + MATLAB R2023b. Again, build should work with any supported combination of MATLAB and gfortran releases. 

1. Install MATLAB and gfortran-10 in any order. 
2. Make sure that gfortran-10 (or your system version) can be called from terminal as just `gfortran`.
3. Configure MATLAB to use gfortran by executing `mex -setup Fortran` in MATLAB Command Window.
4. Build the library by calling `mex -R2018a FFLAGS='$FFLAGS -ffixed-form' mlMIEV0.F90 MIEV0.f ErrPack.f`. 

**Apple Silicon build directions**:
Prerequisites: nAG Fortran Compiler 7.2 + MATLAB R2023b. 

1. Install MATLAB and nagfor in any order. 
2. Make sure that nagfor can be called from MATLAB Command Window. For this you will most likely have to redefine the NAGFOR_ROOT environment variable by executing the following in the MATLAB Command Window: `setenv("NAGFOR_ROOT","/usr/local")`, where `/usr/local` should be replaced with the root folder of your nAG distro. This root folder should, in turn, contain the `bin` folder with `nagfor` binary.
3. Configure MATLAB to use nagfor by executing `mex -setup Fortran` in MATLAB Command Window.
4. Build the library by calling `mex -R2018a FFLAGS='$FFLAGS -fixed -mismatch -kind=byte' mlMIEV0.F90 MIEV0.f ErrPack.f`. 

### Legacy build directions (non-interleaved MEX API)
This version, located in `/legacy/double_precision`, faces some build-related difficulties in recent MATLAB editions. For this reason, MEX file has to be built with older frameworks. As already mentioned, advantage of this approach is compatibility of the produced MEX file with any subsequent MATLAB release (at least, Mathworks still maintain proper binary support). Below the build procedure is discussed on the example of MATLAB R2011a, as this appeared to be the earliest version for which I already had compatible compilers. 

**Prerequisites (verified)**: Visual Studio 2008 Professional + Intel Fortran Compiler 11.1 + MATLAB R2011a. 

This list suggests that you have to be in posession of all of these classic compilers and frameworks. While MATLAB provides most of its editions legally through their website, it might be quite a challenge to find legal Intel and Microsoft compilers, as these were paid-only back in a day. In case you have these products, it's most likely that you already know what you are doing, so just ensure that you install VS2008 first, then IFC 11.1 with VS2008 integration, and execute 
```mex -setup```
 command in MATLAB to follow on-screen instructions and finish configuring your environment. After that, executing 
 
```mex mlMIEV0.F90 MIEV0.f ErrPack.f```

in MATLAB Command window results in a binary file ```mlMIEV0.mexw64``` (in case of the x64 platform).

Potentially, similar approach should work with any MATLAB up to R2017b and corresponding supported compilers (also on the x86 platform), as interleaved MEX API became a standard in R2018a, and Intel introduced its new Fortran compiler ifx (as a replacement to ifort) only in recent oneAPI releases. 

## Remarks
You are very welcome to use this library for education purposes, as well as for producing validation results for research papers.

As I do not possess any rights to the original MIEV0 code and just make use of the fact that it had been freely available for years from Dr. Wiscombe's anonymous NASA FTP at climate.gsfc.nasa.gov in the public folder (as well as a code listing in the '79 NCAR report referred below), in this repo MIT license only applies to the MEX interface and MATLAB files which were completely written by myself.

If you use this code, please make a reference to the original **W. Wiscombe Appl. Opt. 1980 paper**. Also I would be very happy if you recognize my efforts on implementing this MEX interface by mentioning it somewhere in the acknowledgements, or by referring to the [Zenodo](https://zenodo.org/records/17069742) repository. 

## References
1. Wiscombe, W. ["Mie Scattering Calculations - Advances in Technique And Fast, Vector-Speed Computer Codes,"](https://www.researchgate.net/publication/253485579_Mie_Scattering_Calculations_Advances_in_Technique_and_Fast_Vector-speed_Computer_Codes) Ncar Tech Note TN-140+STR, National Center For Atmospheric Research, Boulder, Colorado (1979).
2. Wiscombe, W. ["Improved Mie scattering algorithms,"](https://doi.org/10.1364/AO.19.001505) Appl. Opt. 19, 1505-1509 (1980). 
3. Van De Hulst. "Light Scattering by Small Particles," Dover Press, New York (1957, 1982).
4. Bohren, C. and Huffman, D. "Absorption and Scattering of Light by Small Particles," Wiley, New York (1983).

