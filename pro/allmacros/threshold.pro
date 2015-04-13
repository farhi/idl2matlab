; Program/procedure for D20 PSD threshold calculation
; Programm "regseu3", 24.3.93 by Anton OED and Pierre CONVERT (BASIC)
; Modified 6.5.96 by Anton OED
; Procedure re-written for IDL/LAMP in February 1997 by Thomas HANSEN

PRO threshold, a, thresh, boucle            ; a = counting rates, thresh = threshold values

a      = a(*,0)
ncan   = n_elements(a)                       ; number of detector cells
IF N_ELEMENTS(boucle) eq 0 THEN boucle = 50                                              ; number of iterations0
PRINT, "Procedure for threshold adjustment of D20's PSD"
PRINT, "DO NOT effectuate 'pretreat' before!"
PRINT, "Each zero-counting cell is considered as 'parasited' and will be set"
PRINT, "to the mean value for the counting rate which is calculated by all"
PRINT, "non-zero counting cells. You may mark any parasite-affected cell as"
PRINT, "zero counting (e.g. W6(234)=0 for cell 234 parasite-affected)."
PRINT, "After this 'correction' the counting rates of the border cells are set"
PRINT, "to the mean value, and two additional dummy cells are added on the border"
PRINT, "with the same counting rate."
PRINT, "Parameters: counting rates, old thresholds (optional) : will be changed!"
PRINT, "            number of loops (optional) : not to be changed"

;------ corrections (detector borders etc.) -----------------------------------
a(0)            = 0
a(ncan-1)       = 0
sum1            = total(a(where(a,counts)))
moyen           = sum1 / counts
a(where(a eq 0)) = moyen
sum1   = total(a)
abw    = SQRT (sum1 / ncan)   ; or square ?
mplus  = moyen + abw
mminus = moyen - abw

;-------------- max and min ---------------------------
min = min(a)
max = max(a)

;-------------- calculation ---------------------------
var1 = .0011                  ; as explained below  (changed 29. 8.96)
var2 = var1 / 5               ; empirical values!!
a     = [moyen, a, moyen]     ; counting rates plus border dummy countings
mV    = a*0.0                 ; threshold variation in each loop
B     = a*0.0                 ; new neutron counting rate
MV2   = a*0.0                 ; total threshold variation
FOR k = 1, boucle DO BEGIN
   FOR i = 2, ncan, 2 DO BEGIN                          ;caneaux impair
        mV(i) = -(moyen - a(i)) / (var1 * a(i))         ;apres [ 1 ]
        B(i) = moyen                                    ;stockage du can. i
        B(i - 1) = a(i - 1) + (var2) * a(i - 1) * mV(i) ;apres [ 2 ] can i-1
        a(i + 1) = a(i + 1) + (var2) * a(i + 1) * mV(i) ;apres [ 2 ] can i+1
   ENDFOR
   a(1:i-2) = B(1:i-2)                                  ;nouvelles val.
   FOR i = 1, ncan, 2 DO BEGIN                          ;caneaux pair
        mV(i) = -(moyen - a(i)) / (var1 * a(i))
        B(i) = moyen
        B(i - 1) = a(i - 1) + (var2) * a(i - 1) * mV(i) ;apres [ 2 ] can i-1
        a(i + 1) = a(i + 1) + (var2) * a(i + 1) * mV(i) ;apres [ 2 ] can i+1
   ENDFOR
   a(0:i-2) = B(0:i-2)                                  ;nouvelles val.
   MV2 = MV2 + mV                                       ;sum des variations
   plot,MV2(1:ncan)
ENDFOR
sum2 = total(a(1:ncan))
mmax = max (ABS(MV2(1:ncan)))
PRINT,"New total counting and maximal threshold shift: ", sum2,mmax

;---- presentation du spectre effectuer par la changement des seuils
a = a(1:ncan)            ; new counting rates minus dummy cells

;------- presentation de la variation des seuils ------------------
MV2 = MV2(1:ncan)         ; total threshold variation in mV minus dummy values
plot,MV2
thresh = thresh + MV2     ; new threshold values (optional output)

END

;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;Soubrotines
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; calcul: pour regler les seuils d'un detecteur pour recevoir une
;         reponse egale pour chaque canal.
;         Voraussetzung:
;         Le detecteur soit illuminer uniform
;         les valeurs des seuils et le comptage de chaque canal sont connue.
;         Cettes valeurs doivent disponible dans deux fichier
; Soit:
; Va = contenue actuell mesure d'un canal et
; Vs = continue du meme canal apres une changemente du seuil de dx mV
; Apres Taylor le nouveau comptage est une premier ordre:
;
;                   Vs= Va- var1* Va * dx     [ 1 ]
;
; avec la constante var1  [1/mV]
; Cette constante var1 est a determiner par une calibrage:
; en changant le seuil d'un  canal et enregister les different comptage.
; apres [ 1 ] la constante de variation est  donne par:
;
;                     var1 = -(Vs - Va) / (dx * Va) [ 2 ]
;
;------------------
; A cause de cette de variation de comptage, des caneaux voisins devoient
; aussi changes. Leurs nouveaux comptages Vs soient proportionelle au
; comptage Va avant cette changement et partiellement proportionelle au
; constante var1. Avec la methode des elements finis le calcul prend d'abord
; en compte les caneaux pairs et apres dans un deuxieme pas les caneaux unpairs
; Le constante soit alors : var2 = var1 /4.
; Le comptage des caneaux voisines est:
;
;            ===>  Vs=Va+ var2*Va * dx    [ 3 ]
;
; Apres une certaine nombre des boucles de cet calcul les valeurs de comptage ce
; stabilisees au valeur moyen de comptage, qui est connue.
;
; Les resultats, pour mesuree dans chaque canal le comptage moyen,
; les nouveaux valeurs des seuils seront imprimer,
; et en meme temps stocker dans en fichier, pour repeter cette procedure
; plus tard.
;--------------------------------------------------
; Pour le proto type de D20 avec 3,1 b He(3) + 0,9 bar CF4
; avec les tensions: Uac= 940 V; Uc = 400 V et Ud = 620 V
; la variation var1 etait mesure avec le canal Nr. 48 = pos1 ampli 15
;         seuils en mV      comptage/min
;                600            2086
;                800            1823
;               1000            1707
;               1200            1616
;               1400            1445
;
;apres [ 2 ] la constante de variation est  donne par
;                     var1 = -(Vs - Va) / (dx * Va)
;et pour les valeur mesures pour le proto type cette constante a
;le valeur moyen de
;                     var1= 3,85 E-4
;-----------------------------------------------------------------

