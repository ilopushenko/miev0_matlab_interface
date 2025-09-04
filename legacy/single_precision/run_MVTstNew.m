%=================================================================
%  Tests for MATLAB interface to W. Wiscombe's MIEV0
%  Based on the data from the original MVTstNew.f
%=================================================================
%  MIT License
%  Copyright (c) 2025 Ivan Lopushenko
%  Permission is hereby granted, free of charge, to any person obtaining a copy
%  of this software and associated documentation files (the "Software"), to deal
%  in the Software without restriction, including without limitation the rights
%  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%  copies of the Software, and to permit persons to whom the Software is
%  furnished to do so, subject to the following conditions:
%  The above copyright notice and this permission notice shall be included in all
%  copies or substantial portions of the Software.
%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%  SOFTWARE.
%=================================================================

load('MVTstNew.mat');

OUT = struct;
for i=1:19
    [OUT(i).Qext, OUT(i).Qsca, OUT(i).Gqsc, OUT(i).S1, ...
     OUT(i).S2, OUT(i).Sforw, OUT(i).Sback, OUT(i).Tforw, ...
     OUT(i).Tback, OUT(i).Spike, OUT(i).PMOM] = ...
        mlMIEV0(INPUTS.TestXX(i), INPUTS.TestCR(i), INPUTS.PERFCT(i), ...
                single(1e-6), INPUTS.ANYANG(i), int32(INPUTS.NUMANG), ...
                single(INPUTS.XMU), int32(INPUTS.NMOM(i)), ...
                INPUTS.TestIP(i), int32(200), false, true);
    %i
end


COMPARE = struct;
for i=1:19
    COMPARE(i).about = strcat('XX: ', num2str(INPUTS.TestXX(i)), ' | CREFIN: ', num2str(INPUTS.TestCR(i)));
    COMPARE(i).Qext = TESTVALS.TestQE(i,1);
    COMPARE(i).Qsca = TESTVALS.TestQS(i,1);
    COMPARE(i).Gqsc = TESTVALS.TestGQ(i,1);
    COMPARE(i).S1 = TESTVALS.TestS1(:,i);
    COMPARE(i).S2 = TESTVALS.TestS2(:,i);
    COMPARE(i).Sforw = TESTVALS.TestSF(i,1);
    COMPARE(i).Sback = TESTVALS.TestSB(i,1);
    COMPARE(i).Tforw = TESTVALS.TestTF(i,:);
    COMPARE(i).Tback = TESTVALS.TestTB(i,:);
    COMPARE(i).PMOM = TESTVALS.TestPM(:,:,i);
end

DIFFALL = struct;
for i=1:19
    DIFFALL(i).about = strcat('XX: ', num2str(INPUTS.TestXX(i)), ' | CREFIN: ', num2str(INPUTS.TestCR(i)));
    DIFFALL(i).dQext = COMPARE(i).Qext - OUT(i).Qext;
    DIFFALL(i).dQsca = COMPARE(i).Qsca - OUT(i).Qsca;
    DIFFALL(i).dGqsc = COMPARE(i).Gqsc - OUT(i).Gqsc;
    DIFFALL(i).dS1 = max(abs(COMPARE(i).S1 - OUT(i).S1));
    DIFFALL(i).dS2 = max(abs(COMPARE(i).S2 - OUT(i).S2));
    DIFFALL(i).dSforw = max(abs(COMPARE(i).Sforw - OUT(i).Sforw));
    DIFFALL(i).dSback = max(abs(COMPARE(i).Sback - OUT(i).Sback));
    DIFFALL(i).dTforw = max(abs(COMPARE(i).Tforw - OUT(i).Tforw.'));
    DIFFALL(i).dTback = max(abs(COMPARE(i).Tback - OUT(i).Tback.'));
end

clear i;