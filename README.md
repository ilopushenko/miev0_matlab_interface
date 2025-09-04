# MIEV0 MATLAB interface (MEX API)
MATLAB interface to Warren Wiscombe's MIEV0 Fortran program

pics

## About

## Usage
**Setup**:

Available as binary .mex files on:
1. Windows (x64 Intel and AMD), MATLAB R2011a and subsequent releases. Download mlMIEV0.mexw64 from the Releases section.
2. ~~Linux (x64 Intel and AMD), MATLAB R2017a and subsequent releases. Download mlMIEV0.mexa64 from the Releases section.~~
3. ~~MATLAB Online. Download mlMIEV0.mexa64 from the Releases section and upload into MATLAB Drive to use.~~
4. ~~MacOS (Apple Silicon), MATLAB R2023a and subsequent releases. Download mlMIEV0.mexmaca64 from the Releases section.~~

MEX library can also be compiled from the sources on other platforms (see details below). Octave port is also planned.

**Input variables**:

**Output variables**:

**Examples**:

## Validation

## Known issues

## Manual

## Build
Current version faces some build-related difficulties in recent MATLAB editions as it is implemented with non-interleaved MATLAB MEX API. As a matter of fact, this causes some problems with array memory allocation when compiling with modern Visual Studio, Intel OneAPI and MATLAB R2022a+ editions. For this reason, compilation has to be performed with older frameworks. A clear advantage of this approach is compatibility of the produced MEX file with any subsequent MATLAB release (at least, for now). The interface will be later updated for interleaved MATLAB MEX API for compilation with up-to-date tools. However, current version will also be preserved for compatibility purposes.

**Windows build instructions**:

Prerequisites (verified):

Visual Studio 2008 Professional + Intel Fortran Compiler 11.1 + MATLAB R2011a. 

This list suggests that you have to be in posession of all of these classic compilers and frameworks. While MATLAB provides most of its editions legally through their website, it might be quite a challenge to find legal Intel and Microsoft compilers, as these were paid-only back in a day. In case you have these products, it's most likely that you already know what you are doing, so just ensure that you install VS2008 first, then IFC 11.1 with VS2008 integration, and execute "mex -setup" command in MATLAB to finish configuring your environment. After that, "mex mlMIEV0.F90 MIEV0.f ErrPack.f" command does the job.

Potentially, similar approach should work with any MATLAB up to R2017b and corresponding [supported compilers](https://www.mathworks.com/support/requirements/previous-releases.html), as interleaved MEX API became a standard in R2018a. 

## Remarks

## References

