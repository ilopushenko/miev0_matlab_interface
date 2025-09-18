!=================================================================================
!  MATLAB interface to W. Wiscombe's MIEV0, by I. Lopushenko.
!  Version 1.2, 18 September 2025. Calls modified, double-precision MIEV0.
!  Compatibility: interleaved MATLAB MEX API.
!  Compile: mex mlMIEV0.f MIEV0.f ErrPack.f
!  Verified build config (Win32): VS2019, Intel oneAPI 2021 Fortran, MATLAB R2022a
!=================================================================================
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
!=================================================================================

#include "fintrf.h"

      subroutine mexFunction(nlhs, plhs, nrhs, prhs)
      implicit none

!=== MEX gateway arguments
      INTEGER nlhs, nrhs
      mwPointer plhs(*), prhs(*)

!=== MATLAB Fortran MEX API functions
      REAL*8 mxGetScalar
      mwPointer mxCreateDoubleMatrix, mxGetInt64s,
     + mxCreateNumericMatrix, mxGetPr, 
     + mxClassIDFromClassName
      mwSize mxGetM, mxGetN, mxGetNumberOfElements

      EXTERNAL mxCopyPtrToReal8, mxCopyReal8ToPtr, 
     + mxCopyPtrToInteger8, mxCopyComplex16ToPtr,
     + mxCopyPtrToComplex16

!=== Local MATLAB data pointers
      mwPointer pr, prXMU, mxGetDoubles, mxGetComplexDoubles

!=== Inputs 
      REAL*8 XX, MIMCUT
      COMPLEX*16 CREFIN
      LOGICAL PERFCT, ANYANG
      LOGICAL PRNT(2), ISVERBOSE
      INTEGER NUMANG, NMOM, IPOLZN, MOMDIM
      REAL*8, allocatable :: XMU(:)

!=== Outputs (Fortran side)
      REAL*8 QEXT, QSCA, GQSC, SPIKE
      COMPLEX*16 SFORW, SBACK, TFORW(2), TBACK(2)
      COMPLEX*16, allocatable :: S1(:), S2(:)
      REAL*8, allocatable :: PMOM(:,:) 

!=== Temps
      INTEGER i, j, N, TMP
      mwSize mrows, ncols, numel, ncopy, size 
      CHARACTER (len=*), PARAMETER :: txt = "\nCalling MIEV0...\n"
      CHARACTER tempoutputmex*2000

!=================================================================
!  Check number of inputs/outputs & report
!=================================================================
      if (nrhs .ne. 12) then
         call mexErrMsgTxt('MIEV0 requires 12 input arguments.')
      endif
      if (nlhs .ne. 11) then
         call mexErrMsgTxt('MIEV0 returns 11 output arguments.')
      endif

!=================================================================
!  Transfer inputs from MATLAB to Fortran arrays
!=================================================================

      call mxCopyPtrToReal8(mxGetDoubles(prhs(1)),XX,1)

      size = mxGetM(prhs(2))*mxGetN(prhs(2))
	  pr = mxGetComplexDoubles(prhs(2))
      call mxCopyPtrToComplex16(pr,
     +                          CREFIN,size)

      PERFCT = (mxGetScalar(prhs(3)) .ne. 0.d0)
      
      call mxCopyPtrToReal8(mxGetDoubles(prhs(4)), MIMCUT, 1)

      ANYANG = (mxGetScalar(prhs(5)) .ne. 0.d0)

      call mxCopyPtrToInteger8(mxGetInt64s(prhs(6)),NUMANG,1)

      if (NUMANG .lt. 0) then
         call mexErrMsgTxt('NUMANG must be non-negative.')
      endif
      
      allocate(XMU(NUMANG+1))
      if (NUMANG .gt. 0) then
         prXMU = mxGetDoubles(prhs(7))
         if (prXMU .eq. 0) call mexErrMsgTxt('XMU must be real 
     +                                                    array.')
         mrows = mxGetM(prhs(7))
         ncols = mxGetN(prhs(7))
         numel = mrows*ncols
         if (numel .lt. NUMANG) then
            call mexErrMsgTxt('Length of XMU is less than NUMANG.')
         endif
         call mxCopyPtrToReal8(prXMU, XMU, numel)
      endif

      call mxCopyPtrToInteger8(mxGetInt64s(prhs(8)),NMOM,1)
      call mxCopyPtrToInteger8(mxGetInt64s(prhs(9)),IPOLZN,1)
      call mxCopyPtrToInteger8(mxGetInt64s(prhs(10)),MOMDIM,1)

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

      PRNT(1) = (mxGetScalar(prhs(11)) .ne. 0.d0) !TODO:
      PRNT(2) = (mxGetScalar(prhs(11)) .ne. 0.d0) !print to MLAB CMD
      
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

!=================================================================
!  Allocate memory for outputs
!=================================================================
      N = 0
      TMP = IPOLZN
      
      if (TMP .EQ. 0) then
         N = 1
      else
         if (TMP .LT. 0) TMP = -TMP
         do while (TMP .GT. 0)
            TMP = TMP / 10
            N = N + 1
         end do
      end if
      
      if (ISVERBOSE) then
      write(tempoutputmex,*) "\nDIGITS:", N
      call mexPrintf(tempoutputmex)
      endif 

      allocate(S1(NUMANG), S2(NUMANG))
      allocate(PMOM(0:MOMDIM, N))
      
      DO 97 i = 0, MOMDIM
         DO 98 j = 1, N
             PMOM(i,j) = 0
   98    CONTINUE
   97 CONTINUE 

!=================================================================
!  Call MIEV0
!================================================================= 
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

!=================================================================
!  Package outputs into MATLAB plhs
!=================================================================
! (1) QEXT
      plhs(1) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),0)
      call mxCopyReal8ToPtr( QEXT, mxGetDoubles(plhs(1)), 1 )

! (2) QSCA
      plhs(2) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),0)
      call mxCopyReal8ToPtr( QSCA, mxGetDoubles(plhs(2)), 1 )

! (3) GQSC
      plhs(3) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),0)
      call mxCopyReal8ToPtr( GQSC, mxGetDoubles(plhs(3)), 1 )

! (4) S1  COMPLEX*16 vector (NUMANG x 1)
      plhs(4) = mxCreateNumericMatrix(NUMANG, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(S1,mxGetComplexDoubles(plhs(4)),
     +                             NUMANG)
      

! (5) S2  COMPLEX*16 vector (NUMANG x 1)
      plhs(5) = mxCreateNumericMatrix(NUMANG, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(S2,mxGetComplexDoubles(plhs(5)),
     +                             NUMANG)

! (6) SFORW  COMPLEX*16 scalar
      plhs(6) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(SFORW,
     +                          mxGetComplexDoubles(plhs(6)), 1)

! (7) SBACK  COMPLEX*16 scalar
      plhs(7) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(SBACK,
     +                          mxGetComplexDoubles(plhs(7)), 1)

! (8) TFORW  COMPLEX*16 vector (2 x 1)
      plhs(8) = mxCreateNumericMatrix(2, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(TFORW,
     +                          mxGetComplexDoubles(plhs(8)), 2)

! (9) TBACK COMPLEX*16 vector (2 x 1)
      plhs(9) = mxCreateNumericMatrix(2, 1,
     +          mxClassIDFromClassName('double'),1)
      call mxCopyComplex16ToPtr(TBACK,
     +                          mxGetComplexDoubles(plhs(9)), 2)

! (10) SPIKE
      plhs(10) = mxCreateNumericMatrix(1, 1,
     +          mxClassIDFromClassName('double'),0)
      call mxCopyReal8ToPtr( SPIKE, mxGetDoubles(plhs(10)), 1 )

      
! (11) PMOM  (MOMDIM+1 x N)
      plhs(11) = mxCreateNumericMatrix(MOMDIM+1, N,
     +           mxClassIDFromClassName('double'), 0)
      call mxCopyReal8ToPtr( PMOM, mxGetDoubles(plhs(11)),
     +                       N*(MOMDIM+1) )
      

!=================================================================
!  Cleanup
!=================================================================
      if (allocated(XMU))  deallocate(XMU)
      if (allocated(S1))   deallocate(S1)
      if (allocated(S2))   deallocate(S2)
      if (allocated(PMOM)) deallocate(PMOM)

      return
      end
