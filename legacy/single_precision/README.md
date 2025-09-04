# MEX interface to completely unmodified MIEV0.f
I created this very first version of the interface as a prototype, just to ensure that everything compiles and launches. I tend to keep it for legacy and possible compatibility purposes. 
ErrPack.f was slightly modified to allow display of MIEV0 error messages in MATLAB command window instead of crashing the whole thing.

**Remark**: This version is a good demo showcase for single precision failure at large size parameters.
For this reason proper test script (run_MVTstNew.m) is also included.

MVTstNew.mat contains input data for 19 extensive tests proposed by W. Wiscombe.
It also contains corresponding output values obtained by Warren on Cray supercomputer.
All data had been serialized from the original MVTstNew.f.

Another drawback of this demo is that PMOM computation in the compiled MEX library does not work properly.

Compiled with VS2008 + IFC 11.1 + MATLAB R2011a.
Therefore, should be compatible with any subsequent MATLAB release.

MIT License does not apply here to the files originally authored by W. Wiscombe (MIEV0.f, ErrPack.f)