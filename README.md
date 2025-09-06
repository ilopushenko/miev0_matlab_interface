# MIEV0 MATLAB interface (MEX API)
MATLAB interface to Dr. Warren Wiscombe's MIEV0 Fortran program

pics

## About
and why

also for research and education purposes!

## Usage
**Setup**:

Available as binary MEX files on:
1. Windows (x64 Intel and AMD), MATLAB R2011a and subsequent releases. Download mlMIEV0.mexw64 from the Releases section.
2. ~~Linux (x64 Intel and AMD), MATLAB R2017a and subsequent releases. Download mlMIEV0.mexa64 from the Releases section.~~
3. ~~MATLAB Online. Download mlMIEV0.mexa64 from the Releases section and upload into MATLAB Drive to use.~~
4. ~~MacOS (Apple Silicon), MATLAB R2023a and subsequent releases. Download mlMIEV0.mexmaca64 from the Releases section.~~

Potentially, MEX library can also be compiled from the sources on other platforms (see details below). Octave port might be considered at a later time.

**Input variables**:

**Output variables**:

**Examples**:

**Manual**:

## Validation

## Known issues
- Not all messages of the original code are currently displayed in the MATLAB Command window.
- For two extreme test cases Tback and Tforw parameters differ from pre-computed values more than all other parameters in average. Yet, the difference is not that large.

## Build
Ideally, one should be able to build the library with any MATLAB edition, as soon as you also have [supported compiler(s)](https://www.mathworks.com/support/requirements/previous-releases.html) installed in your system. The command "mex mlMIEV0.F90 MIEV0.f ErrPack.f" launched from MATLAB Command window should then do the job.

In practice, current version faces some build-related difficulties in recent MATLAB editions as it is implemented with non-interleaved MATLAB MEX API. As a matter of fact, this causes some problems with copying arrays into Fortran-allocated memory after compiling with modern Visual Studio, Intel OneAPI / Parallel Studio XE and MATLAB R2018a+ editions. For this reason, currently MEX file has to be built with older frameworks. A clear advantage of this approach is compatibility of the produced MEX file with any subsequent MATLAB release (at least, for now). So, below the build procedure is discussed on the example of MATLAB R2011a, as this appeared to be the earliest version for which author had compatible compilers. 

_The interface will be later updated for the interleaved MATLAB MEX API in order to perform build with up-to-date tools. However, current non-interleaved version will also be preserved for compatibility purposes._

**Windows build instructions (legacy)**:

Prerequisites (verified): Visual Studio 2008 Professional + Intel Fortran Compiler 11.1 + MATLAB R2011a. 

This list suggests that you have to be in posession of all of these classic compilers and frameworks. While MATLAB provides most of its editions legally through their website, it might be quite a challenge to find legal Intel and Microsoft compilers, as these were paid-only back in a day. In case you have these products, it's most likely that you already know what you are doing, so just ensure that you install VS2008 first, then IFC 11.1 with VS2008 integration, and execute "mex -setup" command in MATLAB to finish configuring your environment. After that, "mex mlMIEV0.F90 MIEV0.f ErrPack.f" command does the job.

Potentially, similar approach should work with any MATLAB up to R2017b  and corresponding [supported compilers](https://www.mathworks.com/support/requirements/previous-releases.html) (also on x86 platform), as interleaved MEX API became a standard in R2018a, and Intel introduced its new Fortran compiler ifx (as a replacement to ifort) only in recent oneAPI releases. 

**Linux build instructions (legacy)**:

Here, we can go into more details, as everything except MATLAB is usually available from Linux repos.

## Remarks
You are very welcome to use this library for education purposes, as well as for producing validation results for research papers.

If you use this code, please reference the original W. Wiscombe Appl. Opt. 1980 paper.

If applicable, I would also be very grateful if you recognize my efforts on making this MEX interface also by mentioning it somewhere in the acknowledgements. 

## References

