!=================================================================
!  MATLAB interface to W. Wiscombe's MIEV0, by I. Lopushenko
!  Version 1.0, 3.9.2025. Calls !original, single-precision! MIEV0
!  Compatibility: !non-interleaved! MATLAB MEX API
!  Compile: mex mlMIEV0.f MIEV0.f ErrPack.f
!  Verified build config (Win32): VS2008, IFC 11.1, MATLAB R2011a 
!=================================================================
!  MIT License
!  Copyright (c) 2025 Ivan Lopushenko
!  Permission is hereby granted, free of charge, to any person obtaining a copy
!  of this software and associated documentation files (the "Software"), to deal
!  in the Software without restriction, including without limitation the rights
!  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!  copies of the Software, and to permit persons to whom the Software is
!  furnished to do so, subject to the following conditions:
!  The above copyright notice and this permission notice shall be included in all
!  copies or substantial portions of the Software.
!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!  SOFTWARE.
!=================================================================

#include "fintrf.h"

      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
      implicit none

c=== MEX gateway arguments
      INTEGER nlhs, nrhs
      mwPointer plhs(*), prhs(*)

c=== MATLAB Fortran classic MEX API functions
      REAL*8 mxGetScalar
      mwPointer mxGetPr, mxGetPi, mxCreateDoubleMatrix, 
     + mxCreateDoubleScalar, mxCreateNumericMatrix, 
     + mxClassIDFromClassName
      mwSize mxGetM, mxGetN, mxGetNumberOfElements

      EXTERNAL mxCopyPtrToReal4, mxCopyReal4ToPtr, 
     + mxCopyPtrToInteger4

c=== Local MATLAB data pointers
      mwPointer pr, pi, prXMU

c=== Inputs (!original MIEV0 compatibility!)
      REAL XX, MIMCUT
      COMPLEX CREFIN
      LOGICAL PERFCT, ANYANG
      LOGICAL PRNT(2), ISVERBOSE
      INTEGER NUMANG, NMOM, IPOLZN, MOMDIM
      REAL, allocatable :: XMU(:)

c=== Outputs
      REAL QEXT, QSCA, GQSC, SPIKE
      COMPLEX SFORW, SBACK, TFORW(2), TBACK(2)
      COMPLEX, allocatable :: S1(:), S2(:)
      REAL, allocatable :: PMOM(:,:)   

c=== Temps
      INTEGER i, j, NQ
      mwSize mrows, ncols, numel, ncopy, size 
      CHARACTER (len=*), PARAMETER :: txt = "\nCalling MIEV0...\n"
      CHARACTER tempoutputmex*2000

c=================================================================
c  Check number of inputs/outputs & report
c=================================================================
      if (nrhs .ne. 12) then
         call mexErrMsgTxt('MIEV0 requires 12 input arguments.')
      endif
      if (nlhs .ne. 11) then
         call mexErrMsgTxt('MIEV0 returns 11 output arguments.')
      endif

c=================================================================
c  Transfer inputs from MATLAB to Fortran arrays
c=================================================================

      call mxCopyPtrToReal4(mxGetPr(prhs(1)),XX,1)

      size = mxGetM(prhs(2))*mxGetN(prhs(2))
      call mxCopyPtrToComplex8(mxGetPr(prhs(2)),
     +                         mxGetPi(prhs(2)),CREFIN,size)

      PERFCT = (mxGetScalar(prhs(3)) .ne. 0.d0)
      
      call mxCopyPtrToReal4(mxGetPr(prhs(4)), MIMCUT, 1)

      ANYANG = (mxGetScalar(prhs(5)) .ne. 0.d0)

      call mxCopyPtrToInteger4(mxGetPr(prhs(6)),NUMANG,1)

      if (NUMANG .lt. 0) then
         call mexErrMsgTxt('NUMANG must be non-negative.')
      endif
      
      allocate(XMU(NUMANG+1))
      if (NUMANG .gt. 0) then
         prXMU = mxGetPr(prhs(7))
         if (prXMU .eq. 0) call mexErrMsgTxt('XMU must be real array.')
         mrows = mxGetM(prhs(7))
         ncols = mxGetN(prhs(7))
         numel = mrows*ncols
         if (numel .lt. NUMANG) then
            call mexErrMsgTxt('Length of XMU is less than NUMANG.')
         endif
         call mxCopyPtrToReal4(prXMU, XMU, numel)
      endif

      call mxCopyPtrToInteger4(mxGetPr(prhs(8)),NMOM,1)
      call mxCopyPtrToInteger4(mxGetPr(prhs(9)),IPOLZN,1)
      call mxCopyPtrToInteger4(mxGetPr(prhs(10)),MOMDIM,1)

      if (NMOM .lt. 0) then
         call mexErrMsgTxt('NMOM must be >= 0.')
      endif
      if (MOMDIM .lt. 1) then
         call mexErrMsgTxt('MOMDIM must be >= 1.')
      endif
      if (NMOM .gt. MOMDIM) then
         call mexWarnMsgTxt('NMOM > MOMDIM: values beyond MOMDIM are 
     +                       ignored by interface.')
      endif

      PRNT(1) = (mxGetScalar(prhs(11)) .ne. 0.d0)
      PRNT(2) = (mxGetScalar(prhs(11)) .ne. 0.d0)
      
      ISVERBOSE = (mxGetScalar(prhs(12)) .ne. 0.d0)  
      if (ISVERBOSE) then
          write(tempoutputmex,*) "\n\n--- Input variables ---\n", 
     +     "\nXX:", XX, "\nCREFIN:", CREFIN, "\nPERCFT:", PERFCT,
     +     "\nMIMCUT:", MIMCUT, "\nANYANG:", ANYANG, "\nNUMANG:", 
     +     NUMANG, "\nXMU:"
          call mexPrintf(tempoutputmex)
          DO 99 I = 1, NUMANG
            write(tempoutputmex,*) XMU(I)
            call mexPrintf(tempoutputmex)
   99     CONTINUE      
          write(tempoutputmex,*) "\nNMOM:", NMOM, "\nIPOLZN:", 
     +     IPOLZN, "\nMOMDIM:", MOMDIM, "\nPRNT:", PRNT
          call mexPrintf(tempoutputmex) 
      endif   

c=================================================================
c  Allocate memory for outputs
c=================================================================
      allocate(S1(NUMANG), S2(NUMANG))
      NQ = max(1, abs(IPOLZN))
      allocate(PMOM(0:MOMDIM, NQ))

c=================================================================
c  Call MIEV0
c================================================================= 
      if (ISVERBOSE) then
          call mexPrintf(txt)
          call mexEvalStringWithTrap('disp(" ")')
      endif   
       
      call MIEV0( XX, CREFIN, PERFCT, MIMCUT, ANYANG, NUMANG,  
     +            XMU, NMOM, IPOLZN, MOMDIM, PRNT,                   
     +            QEXT, QSCA, GQSC, PMOM,                          
     +            SFORW, SBACK, S1, S2, TFORW, TBACK, SPIKE )

      if (ISVERBOSE) then
          call mexPrintf("...Done.\n")
          call mexEvalStringWithTrap('disp(" ")')
      endif
      
c=================================================================
c  Package outputs into MATLAB plhs
c=================================================================
c (1) QEXT
      plhs(1) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),0)
      call mxCopyReal4ToPtr( QEXT, mxGetPr(plhs(1)), 1 )

c (2) QSCA
      plhs(2) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),0)
      call mxCopyReal4ToPtr( QSCA, mxGetPr(plhs(2)), 1 )

c (3) GQSC
      plhs(3) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),0)
      call mxCopyReal4ToPtr( GQSC, mxGetPr(plhs(3)), 1 )

c (4) S1  complex vector (NUMANG x 1)
      plhs(4) = mxCreateNumericMatrix(NUMANG, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(S1,mxGetPr(plhs(4)),
     +                            mxGetPi(plhs(4)), NUMANG)
      
c (5) S2  complex vector (NUMANG x 1)
      plhs(5) = mxCreateNumericMatrix(NUMANG, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(S2,mxGetPr(plhs(5)),
     +                            mxGetPi(plhs(5)), NUMANG)

c (6) SFORW  complex scalar
      plhs(6) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(SFORW,mxGetPr(plhs(6)),
     +                               mxGetPi(plhs(6)), 1)

c (7) SBACK  complex scalar
      plhs(7) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(SBACK,mxGetPr(plhs(7)),
     +                               mxGetPi(plhs(7)), 1)

c (8) TFORW  complex vector (2 x 1)
      plhs(8) = mxCreateNumericMatrix(2, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(TFORW,mxGetPr(plhs(8)),
     +                               mxGetPi(plhs(8)), 2)

c (9) TBACK complex vector (2 x 1)
      plhs(9) = mxCreateNumericMatrix(2, 1,
     +          mxClassIDFromClassName('single'),1)
      call mxCopyComplex8ToPtr(TBACK,mxGetPr(plhs(9)),
     +                               mxGetPi(plhs(9)), 2)

c (10) SPIKE
      plhs(10) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('single'),0)
      call mxCopyReal4ToPtr( SPIKE, mxGetPr(plhs(10)), 1 )

      
c (11) PMOM  (MOMDIM+1 x NQ), column-major
      plhs(11) = mxCreateNumericMatrix(MOMDIM+1, NQ,
     +           mxClassIDFromClassName('single'), 0)
      call mxCopyReal4ToPtr( PMOM, mxGetPr(plhs(11)), NQ*(MOMDIM+1) )
      
c=================================================================
c  Cleanup
c=================================================================
      if (allocated(XMU))  deallocate(XMU)
      if (allocated(S1))   deallocate(S1)
      if (allocated(S2))   deallocate(S2)
      if (allocated(PMOM)) deallocate(PMOM)

      return
      end