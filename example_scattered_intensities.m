%=========================================================================
%  Example file for MATLAB interface to W. Wiscombe's MIEV0
%  Computes and plots scattering intensities for size parameter kr=20
%=========================================================================
%  MIT License
%  Copyright (c) 2025 Ivan Lopushenko
%  Permission is hereby granted, free of charge, to any person obtaining a 
%  copy of this software and associated documentation files (the "Software"),
%  to deal in the Software without restriction, including without limitation 
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
%=========================================================================

clear;

%% --- Specify angular grid to compute scattering amplitudes ---
% Here, ANYANG=false is satisfied (see W. Wiscombe's MIEV.doc for details):
angulargrid = linspace(-180,180,361*2)+90; 

%% --- Example: size parameter 20 and refractive index 1.33 ---
% This example corresponds to:
% C. F. Bohren, D. R. Huffman, "Absorption and scattering of light by 
% small particles", New York: Wiley (1983), p. 387, Fig. 13.3.

% Configure input for MIEV0 subroutine
IN(1).XX      = 20;                      % size parameter
IN(1).Crefin  = complex(1.33 + 0.0i);    % refractive index (imaginary part can be + or -, but internally a negative imaginary index is assumed)
IN(1).Perfct  = false;                   % perfectly conducting sphere or not
IN(1).Mimcut  = 1e-12;                   % (positive) value below which imaginary refractive index is regarded as zero
IN(1).Anyang  = false;                   % true <=> any angles whatsoever may be input through Xmu. false <=> the angles are monotone increasing and mirror symmetric about 90 degrees
IN(1).Xmu     = cosd(angulargrid);       % cosines of angles at which S1, S2 are to be evaluated
IN(1).Numang  = int32(numel(IN(1).Xmu)); % number of angles
IN(1).Nmom    = int32(0);                % highest Legendre moment PMOM to calculate, 0 if PMOM are not to be computed
IN(1).Ipolzn  = int32(0);                % how to compute PMOM (see details in /wiscombe/MIEV.pdf)        
IN(1).Momdim  = int32(5);                % determines first dimension of PMOM. Must be given a value, even if NMOM = 0. Minimum: 1.   
IN(1).Prnt    = false;                   % print flags (useless in MATLAB implementation at this moment)
IN(1).Verbose = true;                    % print flags for verbose output from mlMIEV0 MEX interface (sometimes useful for debugging without VS)

% Call mlMIEV0 interface and write results into RES structure
RES = struct;
tic
[RES(1).Qext, RES(1).Qsca, RES(1).Gqsc, RES(1).S1, RES(1).S2, ...
 RES(1).Sforw, RES(1).Sback, RES(1).Tforw, RES(1).Tback, RES(1).Spike, ...
 RES(1).PMOM] = mlMIEV0(IN(1).XX, IN(1).Crefin, IN(1).Perfct, ...
                        IN(1).Mimcut, IN(1).Anyang, IN(1).Numang, ...
                        IN(1).Xmu, IN(1).Nmom, IN(1).Ipolzn, ...
                        IN(1).Momdim, IN(1).Prnt, IN(1).Verbose);
toc

% Plot intensity results
I1 = RES(1).S1.*conj(RES(1).S1);
I2 = RES(1).S2.*conj(RES(1).S2);

figure;
hold on;
p1 = polar(angulargrid.'*pi/180,I1);
p2 = polar(angulargrid.'*pi/180,I2);
p1.LineWidth = 2;
p2.LineWidth = 2;
legend('$I_\perp=|S_1|^2$','$I_{||}=|S_2|^2$','Interpreter','LaTeX')
hold off;
axis equal;
set(gca,'fontsize', 14);
title('Scattering intensity for size parameter 20, refractive index 1.33','FontSize',11);
set(gca,'TitleHorizontalAlignment','left');
