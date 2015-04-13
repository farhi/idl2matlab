; $Id: d_contour.pro,v 1.35 1998/04/16 20:40:58 paulcs Exp $
;
;  Copyright (c) 1997-1998, Research Systems, Inc. All rights reserved.
;       Unauthorized reproduction prohibited.
;
;+
;  FILE:
;       d_contour.pro
;
;  CALLING SEQUENCE: d_contour
;
;  PURPOSE:
;       Shows several feature of contour plots and contour objects.
;
;  MAJOR TOPICS: Plots, widgets and objects
;
;  CATEGORY:
;       IDL Demo System
;
;  REFERENCE: IDL Reference Guide, IDL User's Guide
;-
;--------------------------------------------------------------------
;
;   PURPOSE  Assign a color to the polyline
;
function d_contourGetColorArray, $
    high_color, $        ; IN: highest color index.
    nLevels              ; IN: number of contour levels.

if (nLevels eq 15) then begin
    ColorArray = [0, 4, 11, 9, 3, 10, 5, 8, 2, 6, 13, 12, 14, 15, 1 ]
endif else if (nLevels eq 11) then begin
    ColorArray = [0, 11,  9, 10, 5, 8, 2, 6, 12,  15, 1 ]
endif else begin
    ColorArray = [0, 11,   5, 8, 2,  15, 1 ]
endelse
ColorArray = colorArray + high_color

RETURN, colorArray

end      ;   of  d_contourGetColorArray

;--------------------------------------------------------------------
;
;   PURPOSE  Assign a color to the polyline
;
function d_contourGetContourColor, $
    level, $      ; IN: level index
    nLevels       ; IN: number of levels

high_color = !D.TABLE_SIZE-18 + 1
colorArray = d_contourGetColorArray( high_color, nLevels)
index = ColorArray[level]
TVLCT, Red, Green, Blue,  /GET

RETURN, [Red[index], Green[index], Blue[index]]

end

;--------------------------------------------------------------------
;
;   PURPOSE  Create a contour plot with white solid lines
;            (direct graphics)
;
pro d_contourMakeBasic, $
    data, $             ; IN:  data (n by m)
    WindowID, $         ; IN:  window identifier (pixmap)
    draw_x_size, $      ; IN:  x dimension of drawing area
    draw_y_size, $      ; IN:  y dimension of drawing area
    high_color, $       ; IN   highest color index of color table
    AX=ax, $            ; IN:  x rotation (in degree)
    AZ=az, $            ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    NLevels=nLevels     ; IN:  number of levels

LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

sf_x = (FLOAT(draw_y_size) / FLOAT(draw_x_size)) < 1.0
sf_y = (FLOAT(draw_x_size) / FLOAT(draw_y_size)) < 1.0

WSET, windowID

;    YTITLE='Latitude', XTITLE='Longitude', $	;!did!
;    TITLE='World Elevation', $			;!did!

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5
Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    NLEVELS=nLevels, /NODATA, $
    BACKGROUND=high_color, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

end
;--------------------------------------------------------------------
;
;   PURPOSE  Create a contour plot colored lines
;            (direct graphics)
;
pro d_contourMakeColor, $
    data, $           ; IN:  data (n by m)
    WindowID, $       ; IN:  window identifier (pixmap)
    draw_x_size, $    ; IN:  x dimension of drawing area
    draw_y_size, $    ; IN:  y dimension of drawing area
    high_color, $     ; IN   highest color index of color table
    AX=ax, $          ; IN:  x rotation (in degree)
    AZ=az, $          ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    NLevels=nLevels   ; IN:  number of levels


LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

sf_x = (FLOAT(draw_y_size) / FLOAT(draw_x_size)) < 1.0
sf_y = (FLOAT(draw_x_size) / FLOAT(draw_y_size)) < 1.0

WSET, windowID

colorArray = d_contourGetColorArray(high_color, nLevels)

;CONTOUR, data, COLOR=high_color+17, $		;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+17, $		;!did!
    BACKGROUND=high_color, $
    C_COLORS=colorArray, NLEVELS=nLevels, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5
Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, /NODATA, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

end

;--------------------------------------------------------------------
;
;   PURPOSE  Create a filled contour plot
;            (direct graphics)
;
pro d_contourMakeFill, $
    data, $           ; IN:  data (n by m)
    WindowID, $       ; IN:  window identifier (pixmap)
    draw_x_size, $    ; IN:  x dimension of drawing area
    draw_y_size, $    ; IN:  y dimension of drawing area
    high_color, $     ; IN   highest color index of color table
    AX=ax, $          ; IN:  x rotation (in degree)
    AZ=az, $          ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    NLevels=nLevels   ; IN:  number of levels


LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

sf_x = (FLOAT(draw_y_size) / FLOAT(draw_x_size)) < 1.0
sf_y = (FLOAT(draw_x_size) / FLOAT(draw_y_size)) < 1.0

WSET, windowID

colorArray = d_contourGetColorArray(high_color, nLevels)

;CONTOUR, data, COLOR=high_color+17, $		;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+17, $		;!did!
    BACKGROUND=high_color, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
    C_COLORS=colorArray, NLEVELS=nLevels, /FILL, $
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5
Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, /NODATA, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

end

;--------------------------------------------------------------------
;
;   PURPOSE  Create a contour plot with varios linestyle
;            (direct graphics)
;
pro d_contourMakeLineStyle, $
    data, $           ; IN:  data (n by m)
    WindowID, $       ; IN:  window identifier (pixmap)
    draw_x_size, $    ; IN:  x dimension of drawing area
    draw_y_size, $    ; IN:  y dimension of drawing area
    high_color, $     ; IN   highest color index of color table
    AX=ax, $          ; IN:  x rotation (in degree)
    AZ=az, $          ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    NLevels=nLevels   ; IN:  number of levels


LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

sf_x = (FLOAT(draw_y_size) / FLOAT(draw_x_size)) < 1.0
sf_y = (FLOAT(draw_x_size) / FLOAT(draw_y_size)) < 1.0

WSET, windowID
lineStyle = [0,1,2,3,4,5]

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
    C_LINESTYLE=lineStyle, $
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5
Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, /NODATA, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

end

;--------------------------------------------------------------------
;
;   PURPOSE  Create a contour plot with annotations
;            (direct graphics)
;
pro d_contourMakeAnnotation, $
 ;  data, $           ; IN:  data (n by m)			;!did!
    datz, $           ; IN:  data (n by m)			;!did!
    WindowID, $       ; IN:  window identifier (pixmap)
    draw_x_size, $    ; IN:  x dimension of drawing area
    draw_y_size, $    ; IN:  y dimension of drawing area
    high_color, $     ; IN   highest color index of color table
    AX=ax, $          ; IN:  x rotation (in degree)
    AZ=az, $          ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    WE_fac=we_fac, $						;!did!
    NLevels=nLevels   ; IN:  number of levels

;  Use the Hersey font.
;
!P.FONT = -1

;  Create 7 contour levels here.
;
LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

WSET, windowID


;  Define the levels and their corresponding
;  annotations which are the elevation in meters.
;
levels = [42B, 84B, 105b, 125B, 145b, 169B, 213B]
annotation = STRTRIM(levels * 40 - 5000, 2)
nLevels = N_ELEMENTS(levels)

colorArray = d_contourGetColorArray(high_color, nLevels)

data=datz*we_fac(0) + we_fac(1)			;!did!

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels,  $
    C_COLORS=colorArray, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;   C_ANNOTATION=Annotation, /FOLLOW, C_CHARSIZE=1.25, $	;!did!
    /FOLLOW, C_CHARSIZE=1., $					;!did!
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5
Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, /NODATA, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

;  Restore the hardware drawn font.
;
!P.FONT = 0

end

;--------------------------------------------------------------------
;
;   PURPOSE  Create a filled contour plot with lines
;            (direct graphics)
;
pro d_contourMakeFillAndLine, $
    data, $           ; IN:  data (n by m)
    WindowID, $       ; IN:  window identifier (pixmap)
    draw_x_size, $    ; IN:  x dimension of drawing area
    draw_y_size, $    ; IN:  y dimension of drawing area
    high_color, $     ; IN   highest color index of color table
    AX=ax, $          ; IN:  x rotation (in degree)
    AZ=az, $          ; IN:  z rotation (in degree)
    XX=xx,YY=yy,TIT=tit,XTIT=Xtit,Ytit=Ytit, $		;!did!
    NLevels=nLevels   ; IN:  number of levels


LOADCT, 5, /SILENT
TEK_COLOR, !D.TABLE_SIZE-18, 16

sf_x = (FLOAT(draw_y_size) / FLOAT(draw_x_size)) < 1.0
sf_y = (FLOAT(draw_x_size) / FLOAT(draw_y_size)) < 1.0

WSET, windowID

colorArray = d_contourGetColorArray(high_color, nLevels)

;CONTOUR, data, COLOR=high_color+17, $		;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+17, $		;!did!
    BACKGROUND=high_color, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
    C_COLORS=colorArray, NLEVELS=nLevels, /FILL, $
    XSTYLE=5, YSTYLE=5, TICKLEN=(-0.02), ZSTYLE=5

;CONTOUR, data, COLOR=high_color+17, $		;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+17, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels,  /OVERPLOT, FONT=-1

Empty

;CONTOUR, data, COLOR=high_color+1, $			;!did!
 CONTOUR, data,XX,YY, COLOR=high_color+1, $		;!did!
    BACKGROUND=high_color, $
    NLEVELS=nLevels, /NODATA, $
    XSTYLE=1, YSTYLE=1, TICKLEN=(-0.02), ZSTYLE=5, $
    XTICKS=4, YTICKS=4, $
    YTITLE=Ytit, XTITLE=Xtit, $
    TITLE=TIT, $
;    XTICKNAME=['0', '90', '180', '270', '0'], $	;!did!
;    YTICKNAME=['-90', '-45', '0', '+45', '+90'], $	;!did!
    /NOERASE
Empty

end
;
;--------------------------------------------------------------------
function d_contourWorld_event, event

WIDGET_CONTROL, event.top, GET_UVALUE=pState
WIDGET_CONTROL, (*pState).wWorldLevelsSlider, GET_VALUE=levelValue

;  Here, level value is 1, 2, or 3. We want to
;  have the number of levels to be 7, 11, or 15.
;
(*pState).world_n_levels = levelValue*4 + 3
WIDGET_CONTROL, (*pState).wWorldStyleRadio, GET_VALUE=selection
WSET, (*pState).world_winID

WIDGET_CONTROL, (*pState).wWorldLevelsLabel, $
SET_VALUE=STRING((*pState).world_n_levels, FORMAT='(i2)')

case selection of

    ;  Create the basic contour
    ;
    0 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=1
        d_contourMakeBasic , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            NLEVELS=(*pState).world_n_levels
       end

    ;  Create the contour with colored lines.
    ;
    1 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=1
        d_contourMakeColor , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            NLEVELS=(*pState).world_n_levels
       end

    ;  Create filled contour plot.
    ;
    2 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=1
        d_contourMakeFill , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            NLEVELS=(*pState).world_n_levels
       end

    ;  Create contour lines with vaious line styles.
    ;
    3 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=1
        d_contourMakeLineStyle , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            NLEVELS=(*pState).world_n_levels
       end

    ;  Create contour with annotations.
    ;
    4 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=0
        d_contourMakeAnnotation , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            WE_fac=(*pState).WE_fac, $				;!did!
            NLEVELS=7
       end

    ;  Create filled contour with overlaying lines.
    ;
    5 : begin
        WIDGET_CONTROL, (*pState).wWorldLevelsSlider, SENSITIVE=1
        d_contourMakeFillAndLine , (*pState).world_elev, $
            (*pState).world_winID, $
            (*pState).draw_x_size, (*pState).draw_y_size, $
            (*pState).high_color, $
            XX=(*pState).XX,YY=(*pState).YY,TIT=(*pState).TIT,$	;!did!
            XTIT=(*pState).XTIT,YTIT=(*pState).Ytit, $		;!did!
            NLEVELS=(*pState).world_n_levels
       end

    endcase

RETURN, event
end
;--------------------------------------------------------------------
function d_contourTerrain_event, event

WIDGET_CONTROL, event.top, GET_UVALUE=pState
WIDGET_CONTROL, (*pState).wDraw[1], GET_VALUE=rWindow

if (*pState).rTerrainView eq OBJ_NEW() then begin
    WIDGET_CONTROL, /HOURGLASS
    rWindow->GetProperty, DIMENSIONS=wDims

    ; Compute viewplane rect based on aspect ratio.
    aspect = FLOAT(wDims[0]) / FLOAT(wDims[1])
    myview = [ -0.5, -0.5, 1, 1 ]
    if (aspect > 1) then begin
        myview[0] = myview[0] - ((aspect-1.0)*myview[2])/2.0
        myview[2] = myview[2] * aspect
        end $
    else begin
        myview[1] = myview[1] - (((1.0/aspect)-1.0)*myview[3])/2.0
        myview[3] = myview[3] / aspect
        end

    ; Create view.
    rTerrainView = OBJ_NEW('IDLgrView', PROJECTION=2, EYE=3, $
        ZCLIP=[1.4,-1.4],$
        VIEWPLANE_RECT=myview, COLOR=[40,40,40])

    ; Create model.
    rTerrainTop = OBJ_NEW('IDLgrModel')
    rTerrainGroup = OBJ_NEW('IDLgrModel')
    rTerrainTop->Add, rTerrainGroup

;    z = BYTARR(64,64, /NOZERO)			;!did!
;    OPENR, lun, $					;!did!
;         filepath( $				;!did!
;            'elevbin.dat', $				;!did!
;            SUBDIR=['examples','demo','demodata'] $	;!did!
;            ), $					;!did!
;       /GET_LUN					;!did!
;    READU, lun, z					;!did!
;    FREE_LUN, lun					;!did!
    z=(*pState).world_elev & ii=(size(z))(1) & jj=(size(z))(2)	;!did!
;I
;    idata=bytarr(ii,jj,3,/nozer)					;!did! true texture
;    for i=0,ii-1 do $							;!did!
;    for j=0,jj-1 do idata(i,j,*)=(*pState).TEXrgb(z(i,j),*)	;!did! +rImage
;II
;    rImage= OBJ_NEW('idlgrImage',smooth(z,3,/edge),PALETTE=(*pState).Opal) ;!did! texture palette
;III
     vertexColors = BYTARR(3, ii*jj, /NOZERO)			;!did! vertice texture
     vertexColors[*, *]= transpose((*pState).TEXrgb(z,*))		;!did!
     rImage= OBJ_NEW()							;!did!

    ;
    ;  Reduce high frequencies in z data, so as to better expose
    ;  contour lines that will be lying on surface of z data.
;    z = smooth(TEMPORARY(z), 3, /edge)			;!did!

    ;  Create texture map.
    ;
;   READ_JPEG, demo_filepath('elev_t.jpg', $			;!did!
;      SUBDIR=['examples','demo','demodata']), $		;!did!
;      idata, TRUE=3						;!did!
;   rImage = OBJ_NEW('IDLgrImage', idata, INTERLEAVE=2)	;!did! here is rImage

    ;  Create the surface object.
    ;
    rTerrainSurface = OBJ_NEW('IDLgrSurface', z, $
        (*pState).XX, (*pState).YY, $				;!did!
        STYLE=2, $
        SHADING=1, $
        COLOR=[255,255,255], $
        VERT_COLORS=vertexColors)				;!did!
;       TEXTURE_MAP=rImage)					;!did!

    zz= smooth(z, 3, /edge)					;!did!



    rTerrainPalette = OBJ_NEW('IDLgrPalette')
    if sys_dep('VERSION') ge 5.1 then rTerrainPalette->LoadCT, 34
    rTerrainPalette->GetProperty, RED=r, GREEN=g, BLUE=b
    r = r * .75
    g = g * .75
    b = b * .75
    rTerrainPalette->SetProperty, RED=r, GREEN=g, BLUE=b
    n_levels = 5 ; looks nice.
    WIDGET_CONTROL, (*pState).wTerrainLevelsSlider, $
        SET_VALUE=n_levels-1
    rTerrainContours = OBJ_NEW('IDLgrContour', zz, $
        GEOMX=(*pState).XX, GEOMY=(*pState).YY, $		;!did!
        COLOR=[255,255,0], $
        GEOMZ=z + 2., $
        TICKLEN=7, $
        N_LEVELS=n_levels $
        )
    rTerrainNewContour = OBJ_NEW('IDLgrContour', zz, $
        GEOMX=(*pState).XX, GEOMY=(*pState).YY, $		;!did!
        COLOR=[0,255,0], $
        GEOMZ=z + 2., $
        C_VALUE=[-1], $
        HIDE=1 $ ; we will show it on right click.
        )
    rTerrainCustomContours = OBJ_NEW('IDLgrContour', zz, $
        GEOMX=(*pState).XX, GEOMY=(*pState).YY, $		;!did!
        COLOR=[255,255,0], $
        GEOMZ=z + 2., $
        C_VALUE=[0], $
        HIDE=1 $ ; we will show it on right click.
        )
;   closed_z = bytarr(64, 64)					;!did!
;   closed_z[1:62, 1:62] = z[1:62, 1:62]			;!did!
    xi=(size(z))(1) & yi=(size(z))(2)				;!did!
    closed_z = bytarr(xi, yi)					;!did!
    closed_z[1:xi-2, 1:yi-2] =zz[1:xi-2, 1:yi-2]		;!did!
    rTerrainClosedContours = OBJ_NEW('IDLgrContour', closed_z, $
        GEOMX=(*pState).XX, GEOMY=(*pState).YY, $		;!did!
        C_COLOR=congrid((*pState).ramp, n_levels / 2, /MINUS_ONE), $
        GEOMZ=z, $
        N_LEVELS=n_levels, $
        /FILL, $
        PALETTE=rTerrainPalette, $
        /HIDE $
        )

    rTerrainGroup->Add, rTerrainContours
    rTerrainGroup->Add, rTerrainNewContour
    rTerrainGroup->Add, rTerrainCustomContours
    rTerrainGroup->Add, rTerrainSurface
    rTerrainGroup->Add, rTerrainClosedContours

    ; Compute coordinate conversion.
    rTerrainSurface->GetProperty, XRANGE=xrange, YRANGE=yrange, $
        ZRANGE=zrange
    xLen = xrange[1] - xrange[0]
    yLen = yrange[1] - yrange[0]
    zLen = zrange[1] - zrange[0]

    ; Compute coordinate conversion to normalize.
    xs = [-0.5,1.0/xLen]
    ys = [-0.5,1.0/yLen]
    zs = [-0.5,1.0/zLen]/4

    xs = [-0.5-(1.*xrange[0]/xLen),1.0/xLen]		;!did!
    ys = [-0.5-(1.*yrange[0]/yLen),1.0/yLen]		;!did!
    zs = [-0.5-(1.*zrange[0]/zLen),1.0/zLen]/4	;!did!

    rTerrainContours->SetProperty, XCOORD_CONV=xs, YCOORD_CONV=ys, $
        ZCOORD_CONV=zs
    rTerrainSurface->SetProperty, XCOORD_CONV=xs, YCOORD_CONV=ys, $
        ZCOORD_CONV=zs
    rTerrainClosedContours->SetProperty, XCOORD_CONV=xs, $
        YCOORD_CONV=ys, $
        ZCOORD_CONV=zs
    rTerrainNewContour->SetProperty, XCOORD_CONV=xs, $
        YCOORD_CONV=ys, $
        ZCOORD_CONV=zs
    rTerrainCustomContours->SetProperty, XCOORD_CONV=xs, $
        YCOORD_CONV=ys, $
        ZCOORD_CONV=zs

    ; Create some lights.
    rLight = OBJ_NEW('IDLgrLight', LOCATION=[2,1,2], TYPE=1)
    rTerrainTop->Add, rLight
    rLight = OBJ_NEW('IDLgrLight', TYPE=0, INTENSITY=0.5)
    rTerrainTop->Add, rLight

    ; Place the model in the view.
    rTerrainView->Add, rTerrainTop

    ; Rotate to a nice perspective for first draw.
    rTerrainGroup->Rotate, [1,0,0], -90
    rTerrainGroup->Rotate, [0,1,0], 30
    rTerrainGroup->Rotate, [1,0,0], 30

    (*pState).rImage = rImage
    (*pState).rTerrainView = rTerrainView
    (*pState).rTerrainGroup = rTerrainGroup
    (*pState).rTerrainContours = rTerrainContours
    (*pState).rTerrainNewContour = rTerrainNewContour
    (*pState).rTerrainCustomContours = rTerrainCustomContours
    (*pState).rTerrainSurface = rTerrainSurface
    (*pState).rTerrainClosedContours = rTerrainClosedContours
    (*pState).rTerrainPalette = rTerrainPalette
    end

; Handle events.
;
CASE event.id OF
    (*pState).wDraw[1]: begin
        ; Expose.
        if (event.type EQ 4) then begin
            rWindow->Draw, (*pState).rTerrainView
            return, event
            endif


        ; Handle trackball updates.
        if (*pState).rTerrainTrackball->Update(event, TRANSFORM=qmat) $
        NE 0 $
        then begin
            (*pState).rTerrainGroup->GetProperty, TRANSFORM=t
            (*pState).rTerrainGroup->SetProperty, TRANSFORM=t#qmat
            rWindow->Draw, (*pState).rTerrainView
            return, event
            endif

        if event.type EQ 0 then begin ; Button press.
            (*pState).btndown = event.press
            (*pState).rTerrainClosedContours->GetProperty, $
                 HIDE=fill_hidden
            if fill_hidden and event.press EQ 4 then begin
                if rWindow->Pickdata( $
                    (*pState).rTerrainView, $
                    (*pState).rTerrainSurface, $
                    [event.x, event.y], $
                    xyzlocation $
                    ) $
                then begin
                    (*pState).rTerrainContours->SetProperty, /HIDE
                    (*pState).rTerrainCustomContours->SetProperty, $
                        HIDE=0
                    (*pState).rTerrainNewContour->SetProperty, HIDE=0, $
                        C_VALUE=[xyzlocation[2]]
                    rWindow->Draw, (*pState).rTerrainView
                    end
                end
            WIDGET_CONTROL, (*pState).wDraw[1], /DRAW_MOTION
            endif

        if event.type EQ 2 then begin ; Motion
            (*pState).rTerrainClosedContours->GetProperty, $
                HIDE=fill_hidden
            if fill_hidden and (*pState).btndown EQ 4b then begin
                if rWindow->Pickdata( $
                    (*pState).rTerrainView, $
                    (*pState).rTerrainSurface, $
                    [event.x, event.y], $
                    xyzlocation $
                    ) $
                then begin
                    (*pState).rTerrainNewContour->SetProperty, $
                        C_VALUE=[xyzlocation[2]], $
                        HIDE=0
                    (*pState).rTerrainContours->SetProperty, /HIDE
                    (*pState).rTerrainCustomContours->SetProperty, HIDE=0
                    rWindow->Draw, (*pState).rTerrainView
                    end
                end
            end

        if (event.type EQ 1) then begin ; Button release.
            (*pState).rTerrainClosedContours->GetProperty, HIDE=fill_hidden
            if fill_hidden and ((*pState).btndown EQ 4b) then begin
                (*pState).rTerrainNewContour->GetProperty, $
                    C_VALUE=new_c_value
                if new_c_value[0] ne -1 then begin
                    (*pState).rTerrainCustomContours->GetProperty, $
                        C_VALUE=custom_c_values
                    c_values = [new_c_value, custom_c_values]
                    c_values = $
                        c_values[ $
                            0 $
                            :(n_elements(c_values) - 1) $
                                 < ((*pState).max_terrain_levels - 1) $
                            ]
                    c_values = c_values[UNIQ(ROUND(c_values), SORT(c_values))]
                    if n_elements(c_values) GT 1 then begin
                        WIDGET_CONTROL, (*pState).wTerrainLevelsSlider, $
                            SET_VALUE=n_elements(c_values)
                        (*pState).rTerrainCustomContours->SetProperty, $
                            C_VALUE=c_values
                        (*pState).in_sync = 0b
                        end $
                    else begin
                        (*pState).rTerrainContours->SetProperty, HIDE=0
                        end
                    end
                (*pState).rTerrainNewContour->SetProperty, $
                    C_VALUE=[-1], $
                    /HIDE
                rWindow->Draw, (*pState).rTerrainView
                end
            (*pState).btndown = 0b
            WIDGET_CONTROL, (*pState).wDraw[1], DRAW_MOTION=0
            endif
        end
    (*pState).wTerrainLevelsSlider: begin
        WIDGET_CONTROL, /HOURGLASS
        (*pState).rTerrainCustomContours->SetProperty, C_VALUE=[0]
        (*pState).rTerrainContours->SetProperty, $
            N_LEVELS=event.value + 1
        (*pState).rTerrainClosedContours->GetProperty, HIDE=fill_hidden
        if fill_hidden then begin
            (*pState).rTerrainContours->SetProperty, HIDE=0
            (*pState).in_sync = 0b
            end $
           else begin
            (*pState).rTerrainClosedContours->SetProperty, $
                C_VALUE=0, $
                N_LEVELS=event.value + 1, $
                C_COLOR=congrid( $
                    (*pState).ramp, $
                    ((event.value + 1) / 2) > 2, $
                    /MINUS_ONE $
                    )
            (*pState).in_sync = 1b
            end

        rWindow->Draw, (*pState).rTerrainView
        end
    (*pState).wTerrainStyleRadio: begin
        (*pState).rTerrainNewContour->SetProperty, /HIDE
        case event.value of
            0 : begin
                (*pState).rTerrainClosedContours->SetProperty, /HIDE
                (*pState).rTerrainSurface->SetProperty, HIDE=0
                (*pState).rTerrainCustomContours->GetProperty, $
                    C_VALUE=custom_c_value
                   if n_elements(custom_c_value) GT 1 then $
                       (*pState).rTerrainCustomContours->SetProperty, $
                       HIDE=0 $
                   else $
                       (*pState).rTerrainContours->SetProperty, HIDE=0
                WIDGET_CONTROL, /HOURGLASS
                rWindow->Draw, (*pState).rTerrainView
                end
            1 : begin
                WIDGET_CONTROL, /HOURGLASS
                (*pState).rTerrainSurface->SetProperty, /HIDE
                (*pState).rTerrainCustomContours->SetProperty, /HIDE
                (*pState).rTerrainContours->SetProperty, /HIDE

                (*pState).rTerrainClosedContours->SetProperty, HIDE=0
                if not (*pState).in_sync then begin
                    (*pState).rTerrainSurface->GetProperty, $
                        ZRANGE=zrange
                    (*pState).rTerrainCustomContours->GetProperty, $
                        C_VALUE=c_value
                    if n_elements(c_value) LE 1 then $
                        (*pState).rTerrainContours->GetProperty, $
                        C_VALUE=c_value
                    (*pState).rTerrainClosedContours->SetProperty, $
                        C_VALUE=[c_value, zrange[1] + 1], $
                        C_COLOR=congrid( $
                            (*pState).ramp, $
                            (n_elements(c_value) / 2) > 2, $
                            /MINUS_ONE $
                            )
                    (*pState).in_sync = 1b
                    end
                rWindow->Draw, (*pState).rTerrainView
                end
            else:
            endcase
        end
    endcase
end

;--------------------------------------------------------------------
;
;   PURPOSE  Handle the event
;
pro d_contourEvent, $
    event        ; IN: event structure

if (TAG_NAMES(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST') $
then begin
    WIDGET_CONTROL, event.top, /DESTROY
    RETURN
    end

WIDGET_CONTROL, event.top, GET_UVALUE=pState

case event.id of

    (*pState).wDatasetRadio : begin
        WIDGET_CONTROL, /HOURGLASS
        WIDGET_CONTROL, $
            (*pState).wCurrentControlsBase, $
            MAP=0
        WIDGET_CONTROL, $
            (*pState).wCurrentDraw, $
            MAP=0
        WIDGET_CONTROL, $
            (*pState).wControlsBase[event.value], $
            MAP=1
        (*pState).wCurrentControlsBase = $
            (*pState).wControlsBase[event.value]
        (*pState).wCurrentDraw = $
            (*pState).wDraw[event.value]
            
        if event.value EQ 1 then begin tvlct,(*pState).TEXrgb		;!did!
                         WIDGET_CONTROL,(*pState).wPrWRButton, SENSITIVE=1	;!did!
                         WIDGET_CONTROL,(*pState).wPrLPButton, SENSITIVE=1	;!did!
        endif else begin WIDGET_CONTROL,(*pState).wPrWRButton, SENSITIVE=0	;!did!
                         WIDGET_CONTROL,(*pState).wPrLPButton, SENSITIVE=0	;!did!
        endelse								;!did!

        ;  Re-draw direct graphics to ensure correct colors.
        ;
        if event.value EQ 0 then begin
            WIDGET_CONTROL, $
                (*pState).wWorldStyleRadio, $
                GET_VALUE=indx
            void = d_contourWorld_event({ $
                id: (*pState).wWorldStyleRadio, $
                top: event.top, $
                handler: 0L, $
                value: indx $
                })
            end

        WIDGET_CONTROL, $
            (*pState).wDraw[event.value], $
            MAP=1
        end
        
    ;  Quit his application.
    ;
    (*pState).wQuitButton : begin
        WIDGET_CONTROL, event.top, /DESTROY					;!did! cleanup problem
        end
        
    ;  Print Screen PS or GIF.							;!did!
    ;											;!did!
    (*pState).wPrPSButton : begin							;!did!
        WIDGET_CONTROL, /HOURGLASS							;!did!
        WIDGET_CONTROL, (*pState).wCurrentDraw, GET_VALUE=WinId			;!did!
        IF OBJ_VALID(WinId) then begin						;!did!
		WIDGET_CONTROL, (*pState).wDraw[1], GET_VALUE=rWindow		;!did!
		rWindow->GetProperty, dimensions=Sxy					;!did!
		Obuf= obj_new('IDLgrBuffer',dimensions=Sxy)				;!did!
		Obuf->draw, (*pState).rTerrainView					;!did!
		Oimg=Obuf->read() & Oimg->GetProperty,data=w				;!did!
		obj_destroy,Obuf  & obj_destroy,Oimg					;!did!
		tz  =size(w)      & tbas=widget_base(map=0)      & r=0		;!did!
		if tz(0) eq 3 then begin w=Color_Quan(w,1,r,g,b,/dither)		;!did!
		                         tz=size(w) & tvlct,kr,kg,kb,/get & endif	;!did!
		tdrw=widget_draw(tbas,retain=2,xsize=tz(1),ysize=tz(2))		;!did!
		widget_control,tbas,/realize & widget_control,tdrw,get_value=WinId	;!did!
		wset,WinId & if n_elements(r) gt 1 then tvlct,r,g,b   & tv,w	;!did!
        endif	else tbas=0								;!did!
        P_DID_EVENT, 0, [-88,350,WinId,0,(*pState).WI,0,1,0]				;!did!
		if tbas gt 0 then begin widget_control,tbas,/destroy			;!did!
		             if n_elements(r) gt 1 then tvlct,kr,kg,kb  &  endif	;!did!
        end										;!did!
    (*pState).wPrGFButton : begin							;!did!
        WIDGET_CONTROL, /HOURGLASS							;!did!
        WIDGET_CONTROL, (*pState).wCurrentDraw, GET_VALUE=WinId			;!did!
        IF OBJ_VALID(WinId) then begin						;!did!
		WIDGET_CONTROL, (*pState).wDraw[1], GET_VALUE=rWindow		;!did!
		rWindow->GetProperty, dimensions=Sxy					;!did!
		Obuf= obj_new('IDLgrBuffer',dimensions=Sxy)				;!did!
		Obuf->draw, (*pState).rTerrainView					;!did!
		Oimg=Obuf->read() & Oimg->GetProperty,data=w				;!did!
		obj_destroy,Obuf  & obj_destroy,Oimg					;!did!
		tz  =size(w)      & tbas=widget_base(map=0)      & r=0		;!did!
		if tz(0) eq 3 then begin w=Color_Quan(w,1,r,g,b,/dither)		;!did!
		                         tz=size(w) & tvlct,kr,kg,kb,/get & endif	;!did!
		tdrw=widget_draw(tbas,retain=2,xsize=tz(1),ysize=tz(2))		;!did!
		widget_control,tbas,/realize & widget_control,tdrw,get_value=WinId	;!did!
		wset,WinId & if n_elements(r) gt 1 then tvlct,r,g,b   & tv,w	;!did!
        endif	else tbas=0								;!did!
        myfile='lamp_'+strtrim(string((*pState).WI),2)+'_cp.gif' & wset,WinId	;!did!
        if n_elements(r) gt 1 then WRITE_KIF,myfile,w,r,g,b $			;!did!
                              else WRITE_KIF,myfile,tvrd()				;!did!
       ;P_DID_EVENT, 0, [-88,350,WinId,0,(*pState).WI,0,2,0]				;!did!
 		if tbas gt 0 then begin   widget_control,tbas,/destroy		;!did!
		             if n_elements(r) gt 1 then tvlct,kr,kg,kb  &  endif	;!did!
       end										;!did!
    (*pState).wPrLPButton : begin							;!did!
        WIDGET_CONTROL, (*pState).wCurrentDraw, GET_VALUE=WinId			;!did!
        IF OBJ_VALID(WinId) then begin						;!did!
		WIDGET_CONTROL, (*pState).wDraw[1], GET_VALUE=rWindow		;!did!
		rWindow->GetProperty, dimensions=Sxy					;!did!
		Olpr= obj_new('IDLgrPrinter',/landscape,print_quality=1)		;!did!
		Result = DIALOG_PRINTERSETUP(Olpr,resource_name='lamptouch')	;!did!
        	WIDGET_CONTROL,/HOURGLASS						;!did!
		if Result ne 0 then begin Olpr->draw, (*pState).rTerrainView	;!did!
		                          Olpr->NewDocument   & endif		;!did!
		obj_destroy,Olpr							;!did!
        endif										;!did!
       end										;!did!
    (*pState).wPrWRButton : begin							;!did!
        WIDGET_CONTROL, (*pState).wCurrentDraw, GET_VALUE=WinId			;!did!
        IF OBJ_VALID(WinId) then begin						;!did!
        	WIDGET_CONTROL, /HOURGLASS						;!did!
		WIDGET_CONTROL, (*pState).wDraw[1], GET_VALUE=rWindow		;!did!
		rWindow->GetProperty, dimensions=Sxy					;!did!
		FNam='lamp_w'+strtrim(string((*pState).WI),2)+'.wrl'			;!did!
		Owrl= obj_new('IDLgrVrml',dimensions=Sxy,filename=FNam, $		;!did!
		                          WORLDTITLE='LAMP: '+(*pState).TIT)	;!did!
		Owrl->draw, (*pState).rTerrainView					;!did!
		obj_destroy,Owrl							;!did!
		ii=execute("i=sys_dep('VIEWER',FNam)")				;!did!
        endif										;!did!
       end										;!did!

    ;  Display the information file.
    ;
    (*pState).wAboutButton : begin
        if( Xregistered('XDisplayFile') ne 0) then RETURN
;        XDisplayFile, demo_filepath('contr.txt', $		;!did!
;            SUBDIR=['examples','demo','demotext']), $	;!did!
;            DONE_BUTTON='Done', $				;!did!
;            TITLE="About contour plots" , $			;!did!
;            GROUP=event.top, WIDTH=55, HEIGHT=14		;!did!
        end           ;  of ABOUT

    else : ;  do nothing
    endcase

end     ;   of d_contourEvent

;--------------------------------------------------------------------
;
pro d_contourCleanup, wTopBase

;  Get the color table saved in the window's user value.
;
WIDGET_CONTROL, wTopBase, GET_UVALUE=pState

;  Restore the previous color table.
;
TVLCT, (*pState).color_table

;  Restore the previous plot font.
;
!P.FONT = (*pState).plot_font

;  Delete the pixmap.
;
WDELETE, (*pState).pixmap_winID

;  Restore default margins.
;
!X.MARGIN = [10, 3]
!Y.MARGIN = [4, 2]

if WIDGET_INFO((*pState).group_base, /VALID_ID) then $
    WIDGET_CONTROL, (*pState).group_base, /MAP
;
;Silently flush any accumulated math error.
;
void = check_math()
;
;Restore math exception behavior
;
!except = (*pState).orig_except

ptr_free, (*pState).pTerrainData
obj_destroy, (*pState).rImage
obj_destroy, (*pState).rTerrainView
obj_destroy, (*pState).rTerrainGroup
obj_destroy, (*pState).rTerrainContours
obj_destroy, (*pState).rTerrainClosedContours
obj_destroy, (*pState).rTerrainPalette
obj_destroy, (*pState).rTerrainSurface
obj_destroy, (*pState).rTerrainTrackball
ptr_free, pState
end   ; of Cleanup

;--------------------------------------------------------------------
;
;pro d_contour, $							;!did!
 pro livc_lamp, wksp, $						;!did!
    GROUP=group, $      ; IN: (opt) group identifier
    APPTLB=appTLB,$     ; OUT: (opt) TLB of this application
    TIT=tit, XTIT=xtit, YTIT=ytit, XX=xx, YY=yy ,WI=idn		;!did!

if n_elements(idn) ne 1 then lmp=0 else lmp=1			;!did!
if n_elements(idn) ne 1 then idn=1 & stwi=strtrim(string(idn),2)	;!did!
if n_elements(tit) ne 1 then tit='W'+stwi				;!did!

;  Check the validity of the group identifier.
;
ngroup = N_ELEMENTS(group)
if (ngroup NE 0) then begin
    check = WIDGET_INFO(group, /VALID_ID)
    if (check NE 1) then begin
        print,'Error, the group identifier is not valid'
        print, 'Return to the main application'
        RETURN
       endif
    group_base = group
   endif else group_base = 0L

;
;Silently flush any accumulated math error.
;
void = check_math()
;
;Silently accumulate any subsequent math errors.
;
orig_except = !except
!except = 0
;
DEVICE, DECOMPOSED=0, BYPASS_TRANSLATION = 0

;  Get the current color table. It will be restored when exiting.
;
TVLCT, savedR, savedG, savedB, /GET
color_table = [[savedR],[savedG],[savedB]]

;  Get the screen size.
;
DEVICE, GET_SCREEN_SIZE = screenSize

;  Create the starting up message.
;
;drawbase = demo_startmes(GROUP=group_base)	;!did!

;  Also save the font.
;
plot_font = !P.FONT

; Get the character scaling factor.
;
char_scale = 8.0/!d.X_CH_SIZE

;  Load a new color table.
;
LOADCT, 4, /SILENT						;!did!
TVLCT ,r, g, b ,/get & TEXrgb=[[r],[g],[b]]			;!did!
n=n_elements(r)-1 & TEXrgb(n-10:n,*)=255			;!did!
oPal = OBJ_NEW('idlgrPalette', TEXrgb(*,0),$			;!did!  
                               TEXrgb(*,1), TEXrgb(*,2))	;!did!

LOADCT, 5, /SILENT
high_color = !D.TABLE_SIZE-18
TEK_COLOR, high_color+1, 16

;  Use hardware-drawn font.
;
!P.FONT=0

;  Set up the drawing area size.
;
draw_x_size = 0.6 * screenSize[0]
draw_y_size = 0.8 * draw_x_size

;  Set the initial number of contour levels.
;
world_n_levels = 15

;  Create the widgets.
;
wTopBase = WIDGET_BASE(TITLE=tit,resource_name='lamptouch', $	;!did!
    /TLB_KILL_REQUEST_EVENTS,   $
    MAP=0,                      $
    /COLUMN,                    $
    GROUP_LEADER=group_base,    $
    TLB_FRAME_ATTR=1,           $
    MBAR=barBase)

    ;  Create the menu bar items.
    ;
    wFileButton = WIDGET_BUTTON(barBase, VALUE='File')

        wQuitButton = WIDGET_BUTTON(wFileButton, VALUE='Quit') & WIDGET_CONTROL,wQuitButton,sensitive=0
        wPrLPButton = WIDGET_BUTTON(wFileButton, VALUE='Print')
        wPrWRButton = WIDGET_BUTTON(wFileButton, VALUE='Vrml->lamp_w'+stwi+'.wrl')
        wPrPSButton = WIDGET_BUTTON(wFileButton, VALUE='Screen->lamp_w'+stwi+'_cp.ps')
        wPrGFButton = WIDGET_BUTTON(wFileButton, VALUE='Screen->lamp_w'+stwi+'_cp.gif(png)')

;   wHelpButton = WIDGET_BUTTON(barBase, VALUE='About', /HELP)	;!did!
        wAboutButton = 0L						;!did!
;       wAboutButton = WIDGET_BUTTON(wHelpButton, $			;!did!
;           VALUE='About contours')					;!did!

    ;  Create left and right bases.
    ;
    dataset_titles = [      $
        'Flat Contours',  $
        '3 dim Contours' $
        ]

    wRowBase = WIDGET_BASE(wTopBase, /ROW)
    wLeftBase = WIDGET_BASE(wRowBase, /COLUMN)
    wStackerBase = WIDGET_BASE(wRowBase)
        wRightBase = LONARR(N_ELEMENTS(dataset_titles))
        for i=0,N_ELEMENTS(dataset_titles)-1 do $
            wRightBase[i] = WIDGET_BASE(wStackerBase)

    wDatasetRadio = CW_BGROUP(  $
        wLeftBase,              $
        dataset_titles,         $
        UVALUE='DATASET',       $
        /EXCLUSIVE,             $
        /NO_RELEASE             $
        )
    if lmp then PUT_LOGO, wLeftBase					;!did!

    ;  Create Control Panel bases.  One for each dataset.
    ;        ;
    wStackerBase = WIDGET_BASE(wLeftBase)
    for i=0, N_ELEMENTS(dataset_titles)-1 do  begin
        temp = WIDGET_BASE(     $
            wStackerBase,       $
            UVALUE=STRUPCASE(dataset_titles[i]), $
            /COLUMN,            $
            MAP=0               $
            )
        if N_ELEMENTS(wControlsBase) eq 0 then $
            wControlsBase = temp $
        else $
            wControlsBase = [wControlsBase, temp]
        endfor
    ;
    wWorldLevelsLabel = WIDGET_LABEL( $
        wControlsBase[0],       $
        VALUE='15',             $
        /ALIGN_CENTER           $
        )

    wWorldLevelsSlider = WIDGET_SLIDER( $
        wControlsBase[0],           $
        MINIMUM=1,                  $
        MAXIMUM=3,                  $
        VALUE=3,                    $
        /SUPPRESS_VALUE,            $
        TITLE='Number of Levels'    $
        )

    wWorldStyleRadio = CW_BGROUP(   $
        wControlsBase[0],           $
        ['Basic', 'Colored Lines', 'Fill', 'Linestyle', $
         'Annotation', 'Fill and Lines'], $
        SET_VALUE=1,                $
        /EXCLUSIVE,                 $
        /NO_RELEASE                 $
        )

    max_terrain_levels = 20 ; looks nice.
    wTerrainLevelsSlider = WIDGET_SLIDER( $
        wControlsBase[1],           $
        MINIMUM=2,                  $
        MAXIMUM=max_terrain_levels, $
        TITLE='Number of Levels'    $
        )

    wTerrainStyleRadio = CW_BGROUP( $
        wControlsBase[1], $
        ['Lines', 'Fill'], $
        SET_VALUE=0,                $
        /EXCLUSIVE,                 $
        /NO_RELEASE                 $
        )

    wDraw = LONARR(N_ELEMENTS(dataset_titles))
    wDraw[0] = WIDGET_DRAW(wRightBase[0], XSIZE=draw_x_size, $
        YSIZE=draw_y_size, RETAIN=2)
    wDraw[1] = WIDGET_DRAW(wRightBase[1], XSIZE=draw_x_size, $
        YSIZE=draw_y_size, RETAIN=0, $
        /EXPOSE_EVENTS, /BUTTON_EVENTS, $
        GRAPHICS_LEVEL=2)

    ;  Create tips texts.
    ;
    wStatusBase = WIDGET_BASE(wTopBase, MAP=1, /ROW)

;  Realize the widget hierarchy.
;
WIDGET_CONTROL, wTopBase, MAP=1
WIDGET_CONTROL, wTopBase, /REALIZE
WIDGET_CONTROL, wTopBase, MAP=1
WIDGET_CONTROL, wDraw[0], MAP=0
WIDGET_CONTROL, wDraw[1], MAP=1
WIDGET_CONTROL, wTopBase, SENSITIVE=0

WIDGET_CONTROL, /HOURGLASS
;appTLB = wTopBase ; Return parameter.				;!did!
;sText = demo_getTips(demo_filepath('contr.tip', $			;!did!
;                    SUBDIR=['examples','demo', 'demotext']), $	;!did!
;                   wTopBase, $					;!did!
;                    wStatusBase)					;!did!
 sText = ''								;!did!
;  Determine the window value of plot window.
;
WIDGET_CONTROL, wDraw[0], GET_VALUE=world_winID

Window, /FREE, XSIZE=draw_x_size, YSIZE=draw_y_size, /PIXMAP
pixmap_winID = !D.Window

;  Open and read the world elevation data file.
;
WIDGET_CONTROL, /HOURGLASS
;world_elev = BYTARR(360, 360, /Nozero)		;!did!
;GET_LUN, data_lun					;!did!
;OPENR, data_lun, demo_filepath('worldelv.dat', $	;!did!
;    SUBDIR=['examples', 'data'])			;!did!
;READU, data_lun, world_elev				;!did!
;CLOSE, data_lun					;!did!
;FREE_LUN, data_lun					;!did!
;world_elev = REBIN(TEMPORARY(world_elev), 90, 45)	;!did!

wksz=size(wksp)					;!did!
if wksz(1) ge wksz(2) then xi=128 else xi=96		;!did!
if wksz(1) le wksz(2) then yi=128 else yi=96		;!did!
xi=wksz(1)<xi>(xi/2) & yi=wksz(2)<yi>(yi/2)		;!did!
world_elev=congrid(wksp,xi,yi)			;!did!
we_max=max(world_elev,min=we_min)			;!did!
we_fac=[float(we_max-we_min)/255 , we_min]		;!did!
world_elev=bytscl (temporary(world_elev))		;!did!

wksz=(size(xx))(0)					;!did!
if wksz eq 0 then we_xx=indgen(xi)+1		 	;!did!
if wksz eq 1 then we_xx=congrid(xx,xi) 		;!did!
if wksz eq 2 then we_xx=congrid(xx,xi,yi)		;!did!
wksz=(size(yy))(0)					;!did!
if wksz eq 0 then we_yy=indgen(yi)+1			;!did!
if wksz eq 1 then we_yy=congrid(yy,yi)		;!did!
if wksz eq 2 then we_yy=congrid(yy,xi,yi)		;!did!

xtiq=string([we_xx(0),we_xx(xi/4),we_xx((xi-1)/2),we_xx(xi*3/4),we_xx(xi-1)]) ;!did!
ytiq=string([we_yy(0),we_yy(yi/4),we_yy((yi-1)/2),we_yy(yi*3/4),we_yy(yi-1)]) ;!did!

if n_elements(xtit) ne 1 then xtit='X'		;!did!
if n_elements(ytit) ne 1 then ytit='Y'		;!did!

;  Make the fill contour with line the default.
;  of the direct graphics.
;
d_contourMakeFillAndLine , world_elev, world_winID, $
    draw_x_size, draw_y_size, high_color, $
    XX=we_XX,YY=we_YY,TIT=TIT,XTIT=XTIT,YTIT=YTIT, $ ;!did!
    NLEVELS=world_n_levels

;  Destroy the starting up window.
;
;WIDGET_CONTROL, drawbase, /DESTROY	;!did!

;  Map the top level base.
;
WIDGET_CONTROL, wTopBase, MAP=1

WIDGET_CONTROL, wControlsBase[0], MAP=1
WIDGET_CONTROL, wWorldStyleRadio, SET_VALUE=5

;  Create the info structure.
;
pState = PTR_NEW({ $
    sText: sText, $             ; Text structure for tips
    world_elev: TEMPORARY(world_elev), $ ; Elevation data
    world_n_levels: world_n_levels, $ ; Number of contour levels
    terrain_n_levels: 0, $
    high_color: high_color, $   ; Color index of highest color.
    wCurrentControlsBase: wControlsBase[0], $
    wCurrentDraw: wDraw[0], $
    draw_x_size: draw_x_size, $ ; X size of drawing area
    draw_y_size: draw_y_size, $ ; Y size of drawing area
    color_table: color_table, $ ; Color table to restore at exit
    char_scale: char_scale, $   ; Character scaling factor
    pixmap_winID: pixmap_winID, $
    world_winID: world_winID, $ ; Direct graphics window ID
    wTopBase: wTopBase, $       ; Top level base
    wQuitButton: wQuitButton, $
    wAboutButton: wAboutButton, $
    wWorldLevelsLabel: wWorldLevelsLabel, $
    wWorldLevelsSlider: wWorldLevelsSlider, $
    wWorldStyleRadio: wWorldStyleRadio, $
    wTerrainLevelsSlider: wTerrainLevelsSlider, $
    wDatasetRadio: wDatasetRadio, $
    wControlsBase: wControlsBase, $
    wDraw: wDraw, $
    plot_font: plot_font, $     ; Font ID
    pTerrainData: PTR_NEW(), $
    rImage: OBJ_NEW(), $
    rTerrainView: OBJ_NEW(), $
    rTerrainGroup: OBJ_NEW(), $
    rTerrainContours: OBJ_NEW(), $
    rTerrainNewContour: OBJ_NEW(), $
    rTerrainCustomContours: OBJ_NEW(), $
    rTerrainClosedContours: OBJ_NEW(), $
    rTerrainPalette: OBJ_NEW(), $
    rTerrainSurface: OBJ_NEW(), $
    wTerrainStyleRadio: wTerrainStyleRadio, $
    rTerrainTrackball: OBJ_NEW('Trackball', $
        [draw_x_size/2, draw_y_size/2.], $
        draw_x_size/2.), $
    btndown: 0b, $
    max_terrain_levels: max_terrain_levels, $
    orig_except: orig_except, $
    ramp: bindgen(256), $
    in_sync: 0b, $ ; boolean. Filled contours levels same as non-filled.
    TIT:tit, XTIT:xtit, YTIT:ytit, XX:we_xx, YY:we_yy, XTIQ:xtiq, YTIQ:ytiq, $	;!did!
    TEXrgb:texrgb, Opal:Opal, WE_fac:we_fac, $					;!did!
    wPrWRButton:wPrWRButton, wPrLPButton:wPrLPButton, $				;!did!
    wPrPSButton:wPrPSButton, wPrGFButton:wPrGFButton, WI:idn, $			;!did!
    group_base: group_base $    ; Base of Group Leader
    })

WIDGET_CONTROL, wTopBase, SET_UVALUE=pState

WIDGET_CONTROL, wControlsBase[0], $
    EVENT_FUNC='d_contourWorld_event'
WIDGET_CONTROL, wControlsBase[1], $
    EVENT_FUNC='d_contourTerrain_event'

WIDGET_CONTROL, wDraw[1], $
    EVENT_FUNC='d_contourTerrain_event'

WIDGET_CONTROL, wDatasetRadio, SET_VALUE=1
d_contourEvent, { $
    id:wDatasetRadio, top:wTopBase, handler:0L, select:1, value:1 $
    }

; Register with the XMANAGER.
;
XMANAGER, "d_contour", wTopBase, $
;    /NO_BLOCK, $							;!did!
     /JUST_REG, $							;!did!
    EVENT_HANDLER = "d_contourEvent",CLEANUP="d_contourCleanup"

 WIDGET_CONTROL, wTopBase, SENSITIVE=1 & if lmp then PUT_LOGO	;!did!
;WIDGET_CONTROL, wPrWRButton, SENSITIVE=0				;!did!
 WIDGET_CONTROL, wPrLPButton, SENSITIVE=abs(lmp-1)			;!did!
;WIDGET_CONTROL, wPrGFButton, SENSITIVE=0				;!did!
 WIDGET_CONTROL, wPrPSButton, SENSITIVE=lmp				;!did!
end                      ; of d_contour
