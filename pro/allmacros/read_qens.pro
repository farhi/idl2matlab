; ***************************************************
pro HeaderSetup, IPNSHeader1,Debug=Debug
; Check to see if Debug option is used if not assign a false value
if (not N_Elements(Debug)) then Debug = 0

IPNSTable1  = {IPNSTable , Location:0L, Size:0L}

IPNSHeader1 = {IPNSHeader, $
	  CNT: IPNSTable1, $
	  MAP: IPNSTable1, $
	  TTP: IPNSTable1, $
	  TSC: IPNSTable1, $
	  TSH: IPNSTable1, $
	  CST: IPNSTable1, $
	  DDL: IPNSTable1, $
	  DataD: 0L, $
	  Block: 0L, $
	  Free: 0L, $
	  VERSN: 0L, $
	  DEG: IPNSTable1, $
	  LFI: IPNSTable1, $
	  ZDE: IPNSTable1, $
	  IDE: IPNSTable1, $
	  CTL: IPNSTable1, $
	  HST: IPNSTable1, $
	  NDE: 0, $
	  NAM: String(' ', FORMAT='(a20)'), $
	  TTL: String(' ', FORMAT='(a80)'), $
	  RUN: 0L, $
	  RCY: 0L, $
	  SDT: String(' ', FORMAT='(a9)'), $
	  STM: String(' ', FORMAT='(a8)'), $
	  EDT: String(' ', FORMAT='(a9)'), $
	  ETM: String(' ', FORMAT='(a8)'), $
	  STS: 0B,$
	  VMN: ' ', $
	  PRC: 0L, $
	  ELM: 0L, $
	  NCY: 0, $
	  ICY: 0, $
	  NEN: 0L, $
	  ELN: 0L, $
	  DCL: '    ', $
          DNT: ' ', $
	  PTN: ' ', $
	  LI: 0.0, $
	  LCH: 0.0, $
	  MCL: '    ', $
	  FMN: 0, $
	  CMN: 0, $
	  NHT: 0, $
	  NTY: 0, $
	  NCT: 0, $
	  ICT: 0, $
	  NSH: 0, $
	  NCH: 0L, $
	  PLS: 0L, $
	  SIZ: 0L, $
	  TMN: 0L, $
	  TMX: 0L, $
	  HTD: 0L, $
	  XNM: 0, $
	  YNM: 0, $
	  WNM: 0, $
	  WMA: 0L, $
	  WMI: 0L, $
	  DTA: 0.0, $
	  DTD: 0.0, $
	  OMG: 0.0, $
	  CHI: 0.0, $
	  PHI: 0.0, $
	  XLT: 0.0, $
	  XRT: 0.0, $
	  YLO: 0.0, $
	  YUP: 0.0, $
	  XDS: 0.0, $
	  YDS: 0.0, $
	  XLG: 0.0, $
	  CHW: 0, $
	  CWL: 0, $
	  L1D: 0L, $
	  L2D: 0L, $
	  LOF: 0L, $
	  NCI: 0L, $
	  NOV: 0, $
	  CLK: 0.0, $
	  MSZ: 0L, $
	  KSZ: 0L, $
	  DSZ: 0L, $
	  LSZ: 0L, $
	  ZSZ: 0L, $
	  ISZ: 0L, $
	  TSZ: 0L, $
	  SSZ: 0L, $
	  EIN: 0.0, $
	  EOU: 0.0, $
	  NHS: 0, $
	  PROT: 0.0, $
	  BIN: 0, $
	  MIC: 0, $
	  MOF: 0, $
	  FOF: 0L, $
	  ENM: 0L, $
	  RFI: 0L, $
	  RLA: 0L, $
	  PSN: 0, $
	  DRN: 0L, $
	  NHBLK: 0, $
	  IFSORT: 0, $
	  MSG: IPNSTable1, $
	  DSC: IPNSTable1, $
	  DID: IPNSTable1, $
	  SP4: IPNSTable1, $
	  SP5: IPNSTable1, $
	  SP6: IPNSTable1, $
	  SP7: IPNSTable1, $
	  SP8: IPNSTable1, $
	  SP9: IPNSTable1, $
	  SP10: IPNSTable1, $
	  SP11: IPNSTable1, $
	  SP12: IPNSTable1, $
	  SP13: IPNSTable1, $
	  SP14: IPNSTable1, $
	  SP15: IPNSTable1, $
	  CKS: 0.0, $
	  CKL: 0.0, $	  
	  CK2: 0.0 	}	  

end
; ***************************************************
Pro GetHeader, filename, header,Debug=Debug,lvopen=lvopen
; Check to see if Debug option is used if not assign a false value
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

HeaderSetup, header

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, LUN, filename, /Get_Lun, /swap_if_big_endian
endelse

point_lun,lun,0
ReadU, Lun, header

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,Lun
   free_lun,Lun
endelse

end


; ***************************************************
PRO GetZDE, FileName, IErr, ZDE, ZDESize,debug=debug,lvopen=lvopen
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, Lun, Filename, /GET_LUN,/block, /swap_if_big_endian
endelse

GetHeader, filename, header,Debug=Debug,lvopen=lun
ZDESize = Header.ZDE.Size/4
ZDELoc = Header.ZDE.Location
if(debug) then Begin
    help,ZDEloc
Endif

ZDEtemp= Assoc(Lun, FltArr(ZDESize), ZDELoc)
ZDE = ZDETemp(0)


IErr = 0L
if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,lun
   free_lun, lun
endelse
END


; ***************************************************
PRO GetLFI, FileName, IErr, LFI, LFISize,debug=debug,lvopen=lvopen
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, Lun, Filename, /GET_LUN,/block, /swap_if_big_endian
endelse

GetHeader, filename, header,Debug=Debug,lvopen=lun
LFISize = Header.LFI.Size/4
LFILoc = Header.LFI.Location
if (debug) then begin
    help,lfiloc
Endif


Lfitemp= Assoc(Lun, FltArr(LfiSize), LfiLoc)
LFI = LfiTemp(0)


IErr = 0L
if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,lun
   free_lun, lun
endelse
END


; ***************************************************
PRO GetDEG, FileName, IErr, DEG, DEGSize,Debug=Debug,lvopen=lvopen
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, Lun, Filename, /GET_LUN,/block, /swap_if_big_endian
endelse

GetHeader, filename, header,Debug=Debug,lvopen=lun
DEGSize = Header.DEG.Size/4
DEGLoc = Header.DEG.Location

If(Debug) then Begin
   help,degloc
endif

Degtemp= Assoc(Lun, FltArr(DegSize), DegLoc)
DEG = DegTemp(0)


IErr = 0L
if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,lun
   free_lun, lun
endelse
END


; ***************************************************
PRO GetTTP, FileName, IErr, TTP, TTPSize,debug=debug,lvopen=lvopen
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0


if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, Lun, Filename, /GET_LUN,/block, /swap_if_big_endian
endelse

GetHeader, filename, header,debug=debug,lvopen=lun

TTPSize = Header.TTP.Size/16
TTPLoc = Header.TTP.Location
TFT_binary = Replicate({nchann:0, Flags:0, ClMin:0L, ClRange:0L, chw:0, $
                         vcwl:0},header.nty)

TFT_b= Assoc(Lun, TFT_Binary, TTPLoc)
TFT_b2 = TFT_b(0)

TFT = Replicate({T_Min: 0.0, T_Max:0.0, T_Step:0.0, T_DoubleLen: 0L, $
       NumOfChannels:0, TimeFocusBit:0, EmissionDelayBit:0, $
       ConstantDelayBit:0, dBit:0, eBit:0, fBit:0, gBit:0, hBit:0}, header.nty)
;print,tft_b2
index=where(TFT_b2.chw ne 0)
   TFT(index).NumOfChannels = $
(TFT_b2(index).ClRange-TFT_b2(index).ClMin)/TFT_b2(index).chw

TFT.T_Min = TFT_b2.ClMin / 8.0
TFT.T_Max = TFT_b2.ClRange / 8.0
TFT.T_Step = TFT_b2.chw / 8.0
TFT.T_DoubleLen = TFT_b2.vcwl
TFT.TimeFocusBit = ishft(TFT_b2.Flags, -15) AND 1
TFT.EmissionDelayBit = ishft(TFT_b2.Flags, -14) AND 1
TFT.ConstantDelayBit = ishft(TFT_b2.Flags, -13) AND 1
TFT.dBit = ishft(TFT_b2.Flags, -12) AND 1
TFT.eBit = ishft(TFT_b2.Flags, -11) AND 1
TFT.fBit = ishft(TFT_b2.Flags, -10) AND 1
TFT.gBit = ishft(TFT_b2.Flags, -9) AND 1
TFT.hBit = ishft(TFT_b2.Flags, -8) AND 1

TTP = TFT
IErr = 0L

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,lun
   free_lun, lun
endelse

END


; ***************************************************
PRO GetMAP, FileName, IErr, MAP, MAPSize,Debug=Debug,lvopen=lvopen
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   OpenR, Lun, Filename, /GET_LUN,/block, /swap_if_big_endian
endelse

GetHeader, filename, header,debug=debug,lvopen=lun
MAPSize = Header.MAP.Size/4
;BYTEORDER,MAPSize ,/lswap
MAPLoc = Header.MAP.Location
;BYTEORDER,MAPLoc ,/lswap
Maptemp= Assoc(Lun, LonArr(MapSize), MapLoc)
MAP_b2 = MapTemp(0)

MAP=Replicate({Address:0L, TFType:0L, MoreHistBit:0L},Mapsize)

MAP.Address = MAP_b2 AND "037777777L
MAP.TFType = ISHFT(MAP_b2, -24) And "0377L
MAP.MoreHistBit = ISHFT(MAP_b2,-23) AND "01L

IErr = 0L

if (lvopen ne 0) then begin
   LUN = lvopen
endif else begin
   close,lun
   free_lun, lun
endelse
END

;***************************************************************************

PRO GetSpectrumByID, Filename, Ierr, Spectrum, ID,Debug=Debug,lvopen=lvopen
; Check to see if Debug option is used if not assign a false value
if (not N_Elements(Debug)) then Debug = 0
if (not N_Elements(lvopen)) then lvopen = 0

;  Read runfile header structure

if (lvopen NE 0) then begin
   LUN = lvopen
endif else begin
   OpenR, LUN, filename, /get_lun, /block, /swap_if_big_endian
endelse

getHeader, filename, header, lvopen=lun

; Read Mapping Tables and Detector location info

getMAP, filename, Ierr, DetectorMap, DMAPSize,lvopen=lun
GetTTP, filename, IErr, TimeFieldTable, TFTSize,lvopen=lun
GetDEG, filename, IErr, Angle, AngleSize,lvopen=lun
GetLFI, filename, IErr, Distance, DistanceSize,lvopen=lun
GetZDE, filename, IErr, Height, HeightSize,lvopen=lun

;Check to see if referenced detector ID holds data
If(DetectorMap(ID-1).TFType eq 0) then begin
   If (debug) then Begin
	print, 'Detector has no data'
   Endif
	ierr=-1
	if (lvopen ne 0) then begin
	   LUN=lvopen
	endif else begin
	   close,lun
	   free_lun,lun
	endelse
	return
endif else begin

;Open File for reading spectrum data

;OpenR, LUN, filename, /get_lun, /block, /swap_if_big_endian

if(header.PTN eq 'I') then begin
   If (debug) then Begin
	   print, 'Run was Chopper Focused'
   endif
   IF (not TimeFieldTable(DetectorMap(ID-1).TFType-1).TimeFocusBit) then Begin
   ; *********Detector element not focused ***************
     TMin= Float(TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Min) - $
           float(header.htd)/8.0
     TMax= float(TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Max) - $
              float(header.htd)/8.0
     TMin = TMin + header.LI/sqrt(header.ein/5.22707E6)
     TMax = TMax + header.LI/sqrt(header.ein/5.22707E6)
   endif else begin
   ; *********Detector element is focused ****************
     TMin= Float(TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Min) + $
           header.LI/sqrt(header.ein/5.22707E6)
     TMax= float(TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Max) + $
              header.LI/sqrt(header.ein/5.22707E6)
   endelse
      TStep= TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Step
endif else begin
   If (debug) then Begin
   	print, '************Run was not focused*************'
   EndIf
   TMin= TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Min - header.htd/8.0
   TMax= TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Max - header.htd/8.0
   TStep= TimeFieldTable(DetectorMap(ID-1).TFType-1).T_Step
endelse

nchannels= FIX((TMax-TMin)/TStep)

; Read the data for this spectrum
if (header.wnm gt 0) then begin
	OFFST = header.DataD + header.nch*2 + DetectorMap(ID-1).address+4
endif else begin
	OFFST = header.DataD + DetectorMap(ID-1).address+4
endelse
spec=Assoc(LUN, IntArr(nchannels),OFFSt)
data=long(spec(0))

If (Debug) then Begin
	Print, 'FOF, LOF, NOV, NCH, NCI'
	Print,header.fof,header.lof,header.nov,header.nch,header.nci
Endif



; Parse the overflows to produce an array with the number of overflows for
;  this ID
totchan = header.nch + header.nci
nover = header.lof-totchan

If (header.lof Gt totchan) then begin
	OFs = Assoc(LUN,LonArr(nover), header.DataD+totchan*2)
	OverFlows = OFs(0)
	If (debug) then print,overflows
	ofindex=where(overflows gt header.l1d)
	overflows(ofindex)=(overflows(ofindex)-header.l1d)
	AddOverFlows = Histogram(Overflows, MIN=DetectorMap(ID-1).address+4, $
                         MAX=DetectorMap(ID-1).address+4 + nchannels*2 -2,bin=2)
	if (debug) then begin
		print,addoverflows
		help,addoverflows
	endif
endif else Begin
	addoverflows=0
endelse

;Create a time array
Times = TMin + IndGen(nchannels)*TStep

;  IDL uses signed integers. IPNS DAS uses unsigned integers need to correct
;   for this
indices=where(Data LT 0)
if (indices(0) ne -1)then data(indices)=data(indices)+65536

;  Add in Overflows
data=data+addoverflows*65536

; Package the spectrum data
Spectrum = {data:data, Times:Times, $
             Angle:Angle(ID-1), Distance:Distance(ID-1), Height:Height(ID-1)}

if (lvopen ne 0) then begin
   LUN=lvopen
endif else begin
   close,lun
   free_lun,lun
endelse
endelse
End

;*****************************************************************************

function read_qens, INST , PATH , FILENAME , STATUS , DATP
;*****************************************************************************
;** Reading Data files from raw QENS data files
;** input file #1 : QENS????.RUN file = contains raw DATA (S(n_channel,2theta))
;** and parameters of the run like angles, tof, and flight distance
;** Stephane Rols 11/2001 srols@anl.gov
;** Modified by S.R 03/18/02
;*****************************************************************************

; ** Initializing of some parameters and arrays
; ** *******************************************
;print,'c est parti mon kiki'
iprint=0
DATA  =0
PAR_TXT=STRARR(20) & P=PAR_TXT
n_channels=1800 & str_n_channels=STRCOMPRESS(STRING(n_channels),/REMOVE_ALL)
n_channels_mon=3880 & str_n_channels_mon=STRCOMPRESS(STRING(n_channels_mon),/REMOVE_ALL)
xv=FINDGEN(n_channels)+1.							; The channel array
nsiz_max=258 & y_read=FLTARR(nsiz_max)
STATUS=7
;CATCH,stat & if stat ne 0 then begin print,!err_string & RETURN, DATA & endif
FILENAME=STRMID(FILENAME,2,4)

; ** First Step : Read the angles of the detectors in the angles array
; ** and the detector distances in the distances array
; ** ****************************************************************

STATUS=13

;********** Distances in meters **********
y_read=[    2.18400,      2.18400,      2.18400,      2.18400,      2.18400,      2.18400,$
            2.18400,      2.18400,      2.18400,      2.18400,      2.18400,      1.94000,$
            1.94000,      1.94000,      1.94000,      1.94000,      1.94000,      1.94000,$
            1.94000,      1.94000,      1.94000,      1.72500,      1.72500,      1.72500,$
            1.72500,      1.72500,      1.72500,      1.72500,      1.72500,      1.72500,$
            1.53000,      1.53000,      1.53000,      1.53000,      1.53000,      1.53000,$
            1.53000,      1.53000,      1.35300,      1.35300,      1.35300,      1.35300,$
            1.35300,      1.35300,      1.35300,      1.19400,      1.19400,      1.19400,$
            1.19400,      1.19400,      1.19400,      1.05100,      1.05100,      1.05100]
y_read=[y_read,      1.05100,      1.05100,     1.05100,    0.922000,    0.922000,    0.922000]
y_read=[y_read,     0.922000,     0.922000,    0.806000,    0.806000,    0.806000,    0.806000]
y_read=[y_read,     0.703000,     0.703000,    0.703000,    0.703000,    0.610000,    0.610000]
y_read=[y_read,     0.610000,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.610000,    0.610000]
y_read=[y_read,     0.610000,    0.000000,    0.703000,    0.703000,    0.703000,    0.703000]
y_read=[y_read,     0.806000,    0.806000,    0.806000,    0.806000,    0.922000,    0.922000]
y_read=[y_read,     0.922000,    0.922000,    0.922000,     1.05100,     1.05100,     1.05100]
y_read=[y_read,      1.05100,     1.05100,     1.05100,     1.19400,     1.19400,     1.19400]
y_read=[y_read,      1.19400,     1.19400,     1.19400,     1.35300,     1.35300,     1.35300]
y_read=[y_read,      1.35300,     1.35300,     1.35300,     1.35300,     1.53000,     1.53000]
y_read=[y_read,      1.53000,     1.53000,     1.53000,     1.53000,     1.53000,     1.53000]
y_read=[y_read,      1.72500,     1.72500,     1.72500,     1.72500,     1.72500,     1.72500]
y_read=[y_read,      1.72500,     1.72500,     1.72500,     1.94000,     1.94000,     1.94000]
y_read=[y_read,      1.94000,     1.94000,     1.94000,     1.94000,     1.94000,     1.94000]
y_read=[y_read,      1.94000,     2.18400,     2.18400,     2.18400,     2.18400,     2.18400]
y_read=[y_read,      2.18400,     2.18400,     2.18400,     2.18400,     2.18400,     2.18400]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    -1.38250,    0.000000]

index_d=WHERE(y_read NE 0) & distances=y_read(index_d)
d_mod_mon=ABS(distances(n_elements(distances)-1))
distances=distances(0:n_elements(distances)-2)

;********** Detectors Angles in Degree **********
y_read=[      16.5000,     16.5000,     16.5000,     16.5000,     16.5000,     16.5000]
y_read=[y_read,      16.5000,     16.5000,     16.5000,     16.5000,     16.5000,     23.5000]
y_read=[y_read,      23.5000,     23.5000,     23.5000,     23.5000,     23.5000,     23.5000]
y_read=[y_read,      23.5000,     23.5000,     23.5000,     30.5000,     30.5000,     30.5000]
y_read=[y_read,      30.5000,     30.5000,     30.5000,     30.5000,     30.5000,     30.5000]
y_read=[y_read,      37.5000,     37.5000,     37.5000,     37.5000,     37.5000,     37.5000]
y_read=[y_read,      37.5000,     37.5000,     44.5000,     44.5000,     44.5000,     44.5000]
y_read=[y_read,      44.5000,     44.5000,     44.5000,     51.5000,     51.5000,     51.5000]
y_read=[y_read,      51.5000,     51.5000,     51.5000,     58.5000,     58.5000,     58.5000]
y_read=[y_read,      58.5000,     58.5000,     58.5000,     65.5000,     65.5000,     65.5000]
y_read=[y_read,      65.5000,     65.5000,     72.5000,     72.5000,     72.5000,     72.5000]
y_read=[y_read,      79.5000,     79.5000,     79.5000,     79.5000,     86.5000,     86.5000]
y_read=[y_read,      86.5000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,     93.5000,     93.5000]
y_read=[y_read,      93.5000,    0.000000,     100.500,     100.500,     100.500,     100.500]
y_read=[y_read,      107.500,     107.500,     107.500,     107.500,     114.500,     114.500]
y_read=[y_read,      114.500,     114.500,     114.500,     121.500,     121.500,     121.500]
y_read=[y_read,      121.500,     121.500,     121.500,     128.500,     128.500,     128.500]
y_read=[y_read,      128.500,     128.500,     128.500,     135.500,     135.500,     135.500]
y_read=[y_read,      135.500,     135.500,     135.500,     135.500,     142.500,     142.500]
y_read=[y_read,      142.500,     142.500,     142.500,     142.500,     142.500,     142.500]
y_read=[y_read,      149.500,     149.500,     149.500,     149.500,     149.500,     149.500]
y_read=[y_read,      149.500,     149.500,     149.500,     156.500,     156.500,     156.500]
y_read=[y_read,      156.500,     156.500,     156.500,     156.500,     156.500,     156.500]
y_read=[y_read,     156.500,     163.500,     163.500,     163.500,     163.500,     163.500]
y_read=[y_read,      163.500,     163.500,     163.500,     163.500,     163.500,     163.500]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]
y_read=[y_read,     0.000000,    0.000000,    0.000000,    0.000000,    0.000000,    0.000000]

index_a=WHERE(y_read NE 0) & angles=y_read(index_d)
IF N_ELEMENTS(index_a)+1 NE N_ELEMENTS(index_d) THEN message,'Problem angles and distances arrays have not the right dimensions'

t_min=2000.00 & ch_width=10.0

t_min_mon=600.00 & ch_width_mon=5.0


IF iprint NE 0 THEN BEGIN
PRINT,'Angles in degree'
PRINT,angles
PRINT,'Distances in meters'
PRINT,distances
ENDIF
print,'second step'
; ** Second Step : Read the DATA in neutron counts in the DATA_LECT array
; ** This uses the GetSpectrumById.pro pocedure written by J. Hammonds
; ** ********************************************************************
str_filen=PATH+'qens'+FILENAME+'.run'
n_spectra=N_ELEMENTS(index_a) & str_n_spectra=STRCOMPRESS(STRING(n_spectra),/REMOVE_ALL)
DATA_LECT=FLTARR(n_channels,n_spectra)	;=S(t,2theta)
FOR i=0,N_ELEMENTS(index_a)-1 DO BEGIN
ID=FIX(index_a(i)) & ID=ID(0)
GetSpectrumByID,str_filen,ierr,Spectrum,ID+1
DATA_LECT(*,i)=Spectrum.data(*)
ENDFOR

GetSpectrumByID,str_filen,ierr,Spectrum,index_d(N_ELEMENTS(index_d)-1)+1 ;Lecture monitor
MON=Spectrum.data

; ** Now deal with some input parameters found in the header
; ** This uses the GetHeader.pro procedure written by J. Hammonds
; ** ***************************************************************

GetHeader,str_filen,header
str_uname=strtrim(header.nam,2)
str_date=strtrim(header.sdt,2)
str_rtitle=strtrim(header.ttl,2)

; ** Data are read ... Now extract the monitor and the error arrays
; ** and pass into the DATP structure
; ** ***************************************************************

DATA=DATA_LECT
DATPN=MON
DATAE=SQRT(DATA)
DATPPV=distances
DATPX=xv & DATPY=angles(0:n_spectra-1)
DATPW_TIT=str_rtitle
DATPX_TIT='channel'
DATPY_TIT='spectrum'
DATPOTHER_TIT=str_uname
DATPTIME=str_date

; ** Also find the elastic channel and pass it into the DATP str
; ** ****************************************************************

;x_in=xv & w_in=DATA &
;i=INDGEN(n_spectra)
;xtot=x_in & ytot=TOTAL(w_in(*,i),2) & etot=SQRT(ytot)
;y0=MAX(ytot,i0)   & xmin=xtot((i0-20)>0) & xmax=xtot((i0+20)<(n_channels-1))
;fitgauss, xtot, ytot, etot, xmin, xmax, gauss, dgauss
;chel=gauss(2) & str_chel=STRCOMPRESS(STRING(chel),/REMOVE_ALL)   ; elastic channel

; ** Pass into the DATP structure
; ** *****************************

; ** Now write the read parameters in the DATP.P corresponding array
; ** ***************************************************************

P(0)=FILENAME		  &		PAR_TXT(0)='Run Number'
P(1)=header.sdt		  &		PAR_TXT(1)='Start Date'
P(2)=header.edt		  &		PAR_TXT(2)='Stop Date'
P(3)=header.stm		  &		PAR_TXT(3)='Start Time'
P(4)=header.etm	 	  &		PAR_TXT(4)='Stop Time'
P(5)=header.rfi	  	  &		PAR_TXT(5)='First Run#'
P(6)=header.rla		  &		PAR_TXT(6)='Last Run#'
P(7)='10'		 	  &		PAR_TXT(7)='Channel Width (mmsec)'
P(8)=str_rtitle	  	  &		PAR_TXT(8)='Experimental Title'
P(9)=str_uname		  &		PAR_TXT(9)='User Name'
P(10)=str_n_spectra   &		PAR_TXT(10)='Number of Detectors'
P(11)=str_n_channels  &		PAR_TXT(11)='Number of TOF Channels'
P(12)=header.pls	  &		PAR_TXT(12)='Total Number of Pulses in this Run'
P(13)=t_min			  &		PAR_TXT(13)='Minimum TOF (channel 1) detected'
P(14)=ch_width		  &		PAR_TXT(14)='Channel Width spectra'
P(15)=t_min_mon		  &		PAR_TXT(15)='Minimum TOF (channel 1) detected for mon'
P(16)=ch_width_mon	  &		PAR_TXT(16)='Channel Width monitor'
P(17)=d_mod_mon		  &		PAR_TXT(17)='Moderator to Monitor Distance'

DATP={X:			DATPX,			$
	  Y:			DATPY,			$
	  W_TIT:		DATPW_TIT,		$
	  X_TIT:		DATPX_TIT,		$
	  Y_TIT:		DATPY_TIT,		$
	  N:			DATPN,			$
	  E:			DATAE,			$
	  OTHER_TIT:	DATPOTHER_TIT,	$
	  TIME:			DATPTIME,		$
	  PV:			DATPPV,			$
	  P:			P,				$
	  PAR_TXT:		PAR_TXT }
STATUS=0

no_file:

RETURN, DATA                                         ;Return the data values
;************
END
