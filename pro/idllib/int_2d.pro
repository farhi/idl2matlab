;$Id: int_2d.pro,v 1.4 1994/11/29 18:40:36 beth Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       INT_2D
;
; PURPOSE:
;       This function computes the double integral of a bivariate
;       function F(x,y) with limits of integration from A to B
;       for x and from P(x) to Q(x) for y.
;
; CATEGORY:
;       Numerical Analysis.
;
; CALLING SEQUENCE:
;       Result = INT_2D(Fxy, AB_Limits, PQ_Limits, Pts)
;
; INPUTS:
;       Fxy:  A scalar string specifying the name of a user-supplied
;             IDL function that defines the bivariate function to be
;             integrated. The function must accept x & y and return
;             a scalar result.
;       AB_Limits:  A two-element vector containing the lower, A, and
;                   upper, B,  limits of integration for x.
;       PQ_Limits:  A scalar string specifying the name of a user-
;                   supplied IDL function that defines the lower, P(x),
;                   and upper, Q(x), limits of integration for y. The
;                   function must accept x and return a two-element
;                   vector result.
;       Pts:  The number of transformation points used in the
;             computation. Possible values are: 6, 10, 20, 48 or 96.
;
; KEYWORD PARAMETERS:
;       DOUBLE: If set to a non-zero value, computations are done in
;               double precision arithmetic.
;
; EXAMPLE:
;       Compute the double integral of the bivariate function
;       F(x, y) = exp(-x^2 -y^2) over the circular region:
;       A = -4, B = 4, Px = -sqrt(16 - x^2), Qx = sqrt(16 - x^2).
; 
;       ;Define the bivariate function.
;         function Fxy, x, y
;           return, exp(-x^2. -y^2.)
;         end
;
;       ;Define the limits of integration for y.
;         function PQ_Limits, x
;           return, [-sqrt(16. - x^2), sqrt(16. - x^2)]
;         end
;
;       ;Define the limits of integration for x.
;         AB_Limits = [-4.0, 4.0]
;
;       ;Integrate with 10, 20, 48, and 96 point formulas using double-
;       ;precision arithmetic. 
;         print, INT_2D('Fxy', AB_Limits, 'PQ_Limits', 10, /double)
;         print, INT_2D('Fxy', AB_Limits, 'PQ_Limits', 20, /double)
;         print, INT_2D('Fxy', AB_Limits, 'PQ_Limits', 48, /double) 
;         print, INT_2D('Fxy', AB_Limits, 'PQ_Limits', 96, /double)
;
;       INT_2D with 10 transformation points yields:    3.1208432
;       INT_2D with 20 transformation points yields:    3.1415923
;       INT_2D with 48 transformation points yields:    3.1415923
;       INT_2D with 96 transformation points yields:    3.1415923
;       The exact solution (7 decimal accuracy) yields: 3.1415923
;
; PROCEDURE:
;       INT_2D.PRO computes the double integral of a bivariate function
;       using iterated Gaussian Quadrature. The algorithm's transformation
;       data is provided in tabulated form with 15 decimal accuracy.
;
; REFERENCE:
;       Handbook of Mathematical Functions
;       U.S. Department of Commerce
;       Applied Mathematics Series 55
;
; MODIFICATION HISTORY:
;       Written by:  GGS, RSI, January 1994
;       Modified:    GGS, RSI, September 1994
;                    Added 96 point transformation data.               
;                    Added DOUBLE keyword. Replaced nested FOR loop with
;                    vector operations resulting in faster execution.
;-          

function int_2d, Fxy, AB_Limits, PQ_Limits, pts, double = double

  on_error, 2

; Tabulated transformation data with 15 decimal accuracy.
if pts eq 6 then begin
  ri    = dblarr(pts)          &   wi    = dblarr(pts)
  ri(0) = 0.932469514203152d   &   wi(0) = 0.171324492379170d
  ri(1) = 0.661209386466265d   &   wi(1) = 0.360761573048139d
  ri(2) = 0.238619186083197d   &   wi(2) = 0.467913934572691d
  ri(indgen(pts/2) + (pts/2)) = - ri((pts/2) - indgen(pts/2) -1)
  wi(indgen(pts/2) + (pts/2)) =   wi((pts/2) - indgen(pts/2) -1)  
endif else if pts eq 10 then begin
  ri    = dblarr(pts)          &   wi    = dblarr(pts)
  ri(0) = 0.973906528517172d   &   wi(0) = 0.066671344308688d
  ri(1) = 0.865063366688985d   &   wi(1) = 0.149451349150581d
  ri(2) = 0.679409568299024d   &   wi(2) = 0.219086362515982d
  ri(3) = 0.433395394129247d   &   wi(3) = 0.269266719309996d
  ri(4) = 0.148874338981631d   &   wi(4) = 0.295524224714753d
  ri(indgen(pts/2) + (pts/2)) = - ri((pts/2) - indgen(pts/2) -1)
  wi(indgen(pts/2) + (pts/2)) =   wi((pts/2) - indgen(pts/2) -1)
endif else if pts eq 20 then begin
  ri     = dblarr(pts)         &   wi     = dblarr(pts)
  ri(0)  = 0.993128599185094d  &   wi(0)  = 0.017614007139152d
  ri(1)  = 0.963971927277913d  &   wi(1)  = 0.040601429800386d
  ri(2)  = 0.912234428251325d  &   wi(2)  = 0.062672048334109d
  ri(3)  = 0.839116971822218d  &   wi(3)  = 0.083276741576704d
  ri(4)  = 0.746331906460150d  &   wi(4)  = 0.101930119817240d 
  ri(5)  = 0.636053680726515d  &   wi(5)  = 0.118194531961518d
  ri(6)  = 0.510867001950827d  &   wi(6)  = 0.131688638449176d
  ri(7)  = 0.373706088715419d  &   wi(7)  = 0.142096109318382d
  ri(8)  = 0.227785851141645d  &   wi(8)  = 0.149172986472603d
  ri(9)  = 0.076526521133497d  &   wi(9)  = 0.152753387130725d
  ri(indgen(pts/2) + (pts/2)) = - ri((pts/2) - indgen(pts/2) -1)
  wi(indgen(pts/2) + (pts/2)) =   wi((pts/2) - indgen(pts/2) -1)
endif else if pts eq 48 then begin
  ri     = dblarr(pts)         &   wi     = dblarr(pts)
  ri(0)  = 0.998771007252426d  &   wi(0)  = 0.003153346052305d
  ri(1)  = 0.993530172266350d  &   wi(1)  = 0.007327553901276d
  ri(2)  = 0.984124583722826d  &   wi(2)  = 0.011477234579234d
  ri(3)  = 0.970591592546247d  &   wi(3)  = 0.015579315722943d
  ri(4)  = 0.952987703160430d  &   wi(4)  = 0.019616160457355d
  ri(5)  = 0.931386690706554d  &   wi(5)  = 0.023570760839324d
  ri(6)  = 0.905879136715569d  &   wi(6)  = 0.027426509708356d
  ri(7)  = 0.876572020274247d  &   wi(7)  = 0.031167227832798d
  ri(8)  = 0.843588261624393d  &   wi(8)  = 0.034777222564770d
  ri(9)  = 0.807066204029442d  &   wi(9)  = 0.038241351065830d
  ri(10) = 0.767159032515740d  &   wi(10) = 0.041545082943464d
  ri(11) = 0.724034130923814d  &   wi(11) = 0.044674560856694d
  ri(12) = 0.677872379632663d  &   wi(12) = 0.047616658492490d
  ri(13) = 0.628867396776513d  &   wi(13) = 0.050359035553854d
  ri(14) = 0.577224726083972d  &   wi(14) = 0.052890189485193d
  ri(15) = 0.523160974722233d  &   wi(15) = 0.055199503699984d
  ri(16) = 0.466902904750958d  &   wi(16) = 0.057277292100403d
  ri(17) = 0.408686481990716d  &   wi(17) = 0.059114839698395d
  ri(18) = 0.348755886292160d  &   wi(18) = 0.060704439165893d
  ri(19) = 0.287362487355455d  &   wi(19) = 0.062039423159892d
  ri(20) = 0.224763790394689d  &   wi(20) = 0.063114192286254d
  ri(21) = 0.161222356068891d  &   wi(21) = 0.063924238584648d
  ri(22) = 0.097004699209462d  &   wi(22) = 0.064466164435950d
  ri(23) = 0.032380170962869d  &   wi(23) = 0.064737696812683d
  ri(indgen(pts/2) + (pts/2)) = - ri((pts/2) - indgen(pts/2) -1)
  wi(indgen(pts/2) + (pts/2)) =   wi((pts/2) - indgen(pts/2) -1)
endif else if pts eq 96 then begin
  ri     = dblarr(pts)         &   wi     = dblarr(pts)
  ri(0)  = 0.999689503883230d  &   wi(0)  = 0.000796792065552d
  ri(1)  = 0.998364375863181d  &   wi(1)  = 0.001853960788946d
  ri(2)  = 0.995981842987209d  &   wi(2)  = 0.002910731817934d
  ri(3)  = 0.992543900323762d  &   wi(3)  = 0.003964554338444d
  ri(4)  = 0.988054126329623d  &   wi(4)  = 0.005014202742927d
  ri(5)  = 0.982517263563014d  &   wi(5)  = 0.006058545504235d
  ri(6)  = 0.975939174585136d  &   wi(6)  = 0.007096470791153d
  ri(7)  = 0.968326828463264d  &   wi(7)  = 0.008126876925698d
  ri(8)  = 0.959688291448742d  &   wi(8)  = 0.009148671230783d
  ri(9)  = 0.950032717784437d  &   wi(9)  = 0.010160770535008d
  ri(10) = 0.939370339752755d  &   wi(10) = 0.011162102099838d
  ri(11) = 0.927712456722308d  &   wi(11) = 0.012151604671088d
  ri(12) = 0.915071423120898d  &   wi(12) = 0.013128229566961d
  ri(13) = 0.901460635315852d  &   wi(13) = 0.014090941772314d
  ri(14) = 0.886894517402420d  &   wi(14) = 0.015038721026994d
  ri(15) = 0.871388505909296d  &   wi(15) = 0.015970562902562d
  ri(16) = 0.854959033434601d  &   wi(16) = 0.016885479864245d
  ri(17) = 0.837623511228187d  &   wi(17) = 0.017782502316045d
  ri(18) = 0.819400310737931d  &   wi(18) = 0.018660679627411d
  ri(19) = 0.800308744139140d  &   wi(19) = 0.019519081140145d
  ri(20) = 0.780369043867433d  &   wi(20) = 0.020356797154333d
  ri(21) = 0.759602341176647d  &   wi(21) = 0.021172939892191d
  ri(22) = 0.738030643744400d  &   wi(22) = 0.021966644438744d
  ri(23) = 0.715676812348967d  &   wi(23) = 0.022737069658329d
  ri(24) = 0.692564536642171d  &   wi(24) = 0.023483399085926d
  ri(25) = 0.668718310043916d  &   wi(25) = 0.024204841792364d
  ri(26) = 0.644163403784967d  &   wi(26) = 0.024900633222483d
  ri(27) = 0.618925840125468d  &   wi(27) = 0.025570036005349d
  ri(28) = 0.593032364777572d  &   wi(28) = 0.026212340735672d
  ri(29) = 0.566510418561397d  &   wi(29) = 0.026826866725591d
  ri(30) = 0.539388108324357d  &   wi(30) = 0.027412962726029d
  ri(31) = 0.511694177154667d  &   wi(31) = 0.027970007616848d
  ri(32) = 0.483457973920596d  &   wi(32) = 0.028497411065085d
  ri(33) = 0.454709422167743d  &   wi(33) = 0.028994614150555d
  ri(34) = 0.425478988407300d  &   wi(34) = 0.029461089958167d
  ri(35) = 0.395797649828908d  &   wi(35) = 0.029896344136328d
  ri(36) = 0.365696861472313d  &   wi(36) = 0.030299915420827d
  ri(37) = 0.335208522892625d  &   wi(37) = 0.030671376123669d
  ri(38) = 0.304364944354496d  &   wi(38) = 0.031010332586313d
  ri(39) = 0.273198812591049d  &   wi(39) = 0.031316425596861d
  ri(40) = 0.241743156163840d  &   wi(40) = 0.031589330770727d
  ri(41) = 0.210031310460567d  &   wi(41) = 0.031828758894411d
  ri(42) = 0.178096882367618d  &   wi(42) = 0.032034456231992d
  ri(43) = 0.145973714654896d  &   wi(43) = 0.032206204794030d
  ri(44) = 0.113695850110665d  &   wi(44) = 0.032343822568575d
  ri(45) = 0.081297495464425d  &   wi(45) = 0.032447163714064d
  ri(46) = 0.048812985136049d  &   wi(46) = 0.032516118713868d
  ri(47) = 0.016276744849602d  &   wi(47) = 0.032550614492363d
  ri(indgen(pts/2) + (pts/2)) = - ri((pts/2) - indgen(pts/2) -1)
  wi(indgen(pts/2) + (pts/2)) =   wi((pts/2) - indgen(pts/2) -1)
endif else message,'Pts parameter must be 6, 10, 20, 48 or 96.'

  if keyword_set(double) eq 0 then begin
    ri = float(ri) & wi = float(wi)
  endif

  h1 = (AB_Limits(1) - AB_Limits(0))/2.0
  h2 = (AB_Limits(1) + AB_Limits(0))/2.0
  aj = 0.0
  for i = 0, pts-1 do begin
    x  = h1 * ri(i) + h2
    cf = call_function(PQ_limits, x)
    k1 = (cf(1) - cf(0))/2.0
    k2 = (cf(1) + cf(0))/2.0
    jx = total(wi * call_function(Fxy, x, k1*ri+k2))
    aj = aj + (wi(i) * k1 * jx)
  endfor
  return, (aj * h1)
  ri = 0 & wi = 0
end
