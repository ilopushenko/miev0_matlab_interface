%=================================================================
%  Tests for MATLAB interface to W. Wiscombe's MIEV0
%  Based on the data from the original MVTstNew.f
%=================================================================
%  MIT License
%  Copyright (c) 2025 Ivan Lopushenko
%  Permission is hereby granted, free of charge, to any person obtaining a 
%  copy of this software and associated documentation files (the "Software"),
%  to deal in the Software withRES restriction, including withRES limitation 
%  the rights  to use, copy, modify, merge, publish, distribute, sublicense, 
%  and/or sell copies of the Software, and to permit persons to whom the 
%  Software is furnished to do so, subject to the following conditions:
%  The above copyright notice and this permission notice shall be included 
%  in all copies or substantial portions of the Software.
%  THE SOFTWARE IS PROVIDED "AS IS", WITHRES WARRANTY OF ANY KIND, EXPRESS OR
%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
%  FROM, RES OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
%  DEALINGS IN THE SOFTWARE.
%=================================================================

% --- Load tests ---
% Both input data and test results precomputed by W. Wiscombe are loaded
load('MVTstNew.mat');

% --- Produce results with MIEV0 ---
RES = struct;
for i=1:19
    RES(i).about = strcat('XX: ', num2str(INPUTS.TestXX(i)), ...
                          ' | CREFIN: ', num2str(INPUTS.TestCR(i)));
    [RES(i).Qext, RES(i).Qsca, RES(i).Gqsc, RES(i).S1, ...
     RES(i).S2, RES(i).Sforw, RES(i).Sback, RES(i).Tforw, ...
     RES(i).Tback, RES(i).Spike, RES(i).PMOM] = ...
        mlMIEV0(double(INPUTS.TestXX(i)),double(INPUTS.TestCR(i)), ...
                INPUTS.PERFCT(i), double(1e-6), INPUTS.ANYANG(i), ...
                int32(INPUTS.NUMANG), double(INPUTS.XMU), ...
                int32(INPUTS.NMOM(i)), INPUTS.TestIP(i), int32(200), ...
                false, false);
    
    % Properly normalize PMOM output to match the precomputed values        
    RES(i).PMOM = RES(i).PMOM * 4/(INPUTS.TestXX(i)^2*RES(i).Qsca);
end

% --- Organize test results into structure for convenient comparison ---
REFERENCE = struct;
for i=1:19
    REFERENCE(i).about = strcat('XX: ', num2str(INPUTS.TestXX(i)), ...
                                ' | CREFIN: ', num2str(INPUTS.TestCR(i)));
    REFERENCE(i).Qext  = TESTVALS.TestQE(i,1);
    REFERENCE(i).Qsca  = TESTVALS.TestQS(i,1);
    REFERENCE(i).Gqsc  = TESTVALS.TestGQ(i,1);
    REFERENCE(i).S1    = TESTVALS.TestS1(:,i);
    REFERENCE(i).S2    = TESTVALS.TestS2(:,i);
    REFERENCE(i).Sforw = TESTVALS.TestSF(i,1);
    REFERENCE(i).Sback = TESTVALS.TestSB(i,1);
    REFERENCE(i).Tforw = TESTVALS.TestTF(i,:);
    REFERENCE(i).Tback = TESTVALS.TestTB(i,:);
    REFERENCE(i).PMOM  = TESTVALS.TestPM(:,:,i);
end


% ---  Compute differences between MATLAB MIEV0 and W. Wiscombe values ---
% (except PMOM)
% TODO: Ratio function like in the original MVTstsNew.f
DIFF = struct;
for i=1:19
    DIFF(i).about = strcat('XX: ', num2str(INPUTS.TestXX(i)), ...
                           ' | CREFIN: ', num2str(INPUTS.TestCR(i)));
    DIFF(i).dQext = (REFERENCE(i).Qext - RES(i).Qext)/REFERENCE(i).Qext;
    DIFF(i).dQsca = (REFERENCE(i).Qsca - RES(i).Qsca)/REFERENCE(i).Qsca;
    DIFF(i).dGqsc = (REFERENCE(i).Gqsc - RES(i).Gqsc)/REFERENCE(i).Gqsc;
    DIFF(i).dS1 = max(abs(REFERENCE(i).S1) - abs(RES(i).S1))...
                                                /max(abs(REFERENCE(i).S1));
    DIFF(i).dS2 = max(abs(REFERENCE(i).S2) - abs(RES(i).S2))...
                                                /max(abs(REFERENCE(i).S2));
    DIFF(i).dSforw = max(abs(REFERENCE(i).Sforw) - abs(RES(i).Sforw.'))...
                                             /max(abs(REFERENCE(i).Sforw));
    DIFF(i).dSback = max(abs(REFERENCE(i).Sback) - abs(RES(i).Sback.'))...
                                             /max(abs(REFERENCE(i).Sback));
    DIFF(i).dTforw = max(abs(REFERENCE(i).Tforw) - abs(RES(i).Tforw.'))...
                                             /max(abs(REFERENCE(i).Tforw));
    DIFF(i).dTback = max(abs(REFERENCE(i).Tback) - abs(RES(i).Tback.'))...
                                             /max(abs(REFERENCE(i).Tback));
end

clear i
disp('Test cases computed, see workspace variables for data.')