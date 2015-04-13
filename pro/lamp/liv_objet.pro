;  liv_objet.pro  v 1.0
;  Source:  d_obj.pro,v 1.10 1999/01/16 01:12:20 scottm Exp
;  Modified 2001/02  D.Richard


;  ORB Written by: RF, September 1996.
;	To initially create:		oOrb = OBJ_NEW('orb') 
;	To retrieve a property value:	oOrb->GetProperty
;	To set a property value:	oOrb->SetProperty
;	To print to the standard output stream the current properties of 
;	the orb:			oOrb->Print
;	To destroy:			OBJ_DESTROY, oOrb
;
; KEYWORD PARAMETERS:
;   ORB::INIT:
;	<Note that keywords accepted by IDLgrModel::Init and/or
;	 IDLgrPolygon::Init are also accepted here.>
;	POS:	A three-element vector, [x,y,z], specifying the position
;               of the center of the orb, measured in data units . 
;		Defaults to [0,0,0].
;	RADIUS: A floating point number representing the radius of the
;               orb (measured in data units).  The default is 1.0.
;	DENSITY:A floating point number representing the density at which
;               the vertices should be generated along the surface of the
;               orb.  The default is 1.0.
;	TEX_COORDS: Set this keyword to a nonzero value if texture map
;               coordinates are to be generated for the orb.
;
;   ORB::GETPROPERTY: (POS= RADIUS= DENSITY=)
;   ORB::SETPROPERTY: (POS= RADIUS= DENSITY=)
;	<Note that keywords accepted by IDLgrModel::SetProperty and/or
;	 IDLgrPolygon::SetProperty are also accepted here.>
; EXAMPLE:
;	Create an orb centered at the origin with a radius of 0.5:
;		oOrb = OBJ_NEW('Orb', POS=[0,0,0], RADIUS=0.5) 
;----------------------------------------------------------------------------
;This function returns a 1 if initialization is successful, or 0 otherwise.
;******* *********
FUNCTION Orb::Init, POS=pos, RADIUS=radius, DENSITY=density, $
                    TEX_COORDS=tex_coords, _EXTRA=e

;    IF (self->IDLgrModel::Init(_EXTRA=e) NE 1) THEN RETURN, 0

    self.pos = [0.0,0.0,0.0]
    self.radius = 1.0
    self.density = 1.0

    IF (N_ELEMENTS(pos) EQ 3) THEN $
        self.pos = pos

    IF (N_ELEMENTS(radius) EQ 1) THEN $
        self.radius = radius

    IF (N_ELEMENTS(density) EQ 1) THEN $
        self.density = density

    IF (N_ELEMENTS(tex_coords) EQ 1) THEN $
        self.texture = tex_coords

    ; Initialize the polygon object that will be used to represent
    ; the orb.
    self.oPoly = OBJ_NEW('IDLgrPolygon', SHADING=1, /REJECT, _EXTRA=e)

    self->Add,self.oPoly

    ; Build the polygon vertices and connectivity based on property settings.
    self->BuildPoly

    RETURN, 1
END

;----------------------------------------------------------------------------
;Cleans up all memory associated with the orb.
;** ************
PRO orb::Cleanup

    ; Cleanup the polygon object used to represent the orb.
    OBJ_DESTROY, self.oPoly

    ; Cleanup the superclass.
    self->IDLgrModel::Cleanup
END

;----------------------------------------------------------------------------
;Sets the value of properties associated with the orb object.
;** ****************
PRO orb::SetProperty, POS=pos, RADIUS=radius, DENSITY=density, _EXTRA=e

    self->IDLgrModel::SetProperty, _EXTRA=e
    self.oPoly->SetProperty, _EXTRA=e

    IF (N_ELEMENTS(pos) EQ 3) THEN $
        self.pos = pos

    IF (N_ELEMENTS(radius) EQ 1) THEN $
        self.radius = radius

    IF (N_ELEMENTS(density) EQ 1) THEN $
        self.density = density

    ; Rebuild the polygon according to keyword settings.
    self->BuildPoly
END

;----------------------------------------------------------------------------
;Retrieves the value of properties associated with the orb object.
;** ****************
PRO orb::GetProperty, POS=pos, RADIUS=radius, DENSITY=density,$
                         POBJ=pobj, _REF_EXTRA=re

    self->IDLgrModel::GetProperty, _EXTRA=re
    self.oPoly->GetProperty, _EXTRA=re

    pos    = self.pos
    radius = self.radius 
    density= self.density 
    pobj   = self.oPoly
END
;----------------------------------------------------------------------------
PRO orb::Print
    PRINT, self.pos
    PRINT, self.radius
    PRINT, self.density
END

;----------------------------------------------------------------------------
;Sets the vertex and connectivity arrays for the polygon used to
;represent the orb.
;** **************
PRO orb::BuildPoly

    ; Number of rows and columns of vertices is based upon the density
    ; property.
    nrows = LONG(20.0*self.density)
    ncols = LONG(20.0*self.density)
    IF (nrows LT 2) THEN nrows = 2
    IF (ncols LT 2) THEN ncols = 2

    ; Create the vertex list and the connectivity array.
    nverts = nrows*ncols + 2
    nconn = (ncols*(nrows-1)*5) + (2*ncols*4)
    conn = LONARR(ncols*(nrows-1)*5 + 2*ncols*4)
    verts = FLTARR(3, nverts)
    IF (self.texture NE 0) THEN $
        tex = FLTARR(2,nverts)

    ; Fill in the vertices.
    i = 0L
    j = 0L
    k = 0L
    tzinc = (!PI)/FLOAT(nrows+1)
    tz = (!PI/2.0) - tzinc 
    FOR k=0,(nrows-1) DO BEGIN
        z = SIN(tz)*self.radius
        r = COS(tz)*self.radius
        t = 0
        IF (self.texture NE 0) THEN BEGIN
            tinc = (2.*!PI)/FLOAT(ncols-1)
        ENDIF ELSE BEGIN
            tinc = (2.*!PI)/FLOAT(ncols)
        ENDELSE
        FOR j=0,(ncols-1) DO BEGIN
            verts[0,i] = r*COS(t) + self.pos[0]
            verts[1,i] = r*SIN(t) + self.pos[1]
            verts[2,i] = z + self.pos[2]

            IF (self.texture NE 0) THEN BEGIN
                tex[0,i] = t/(2.*!PI)
                tex[1,i] = (tz+(!PI/2.0))/(!PI)
            ENDIF

            t = t + tinc
            i = i + 1L
        ENDFOR
        tz = tz - tzinc
    ENDFOR
    top = i
    verts[0,i] = self.pos[0]
    verts[1,i] = self.pos[1]
    verts[2,i] = self.radius*1.0 + self.pos[2]
    i = i + 1L
    bot = i
    verts[0,i] = self.pos[0]
    verts[1,i] = self.pos[1]
    verts[2,i] = self.radius*(-1.0) + self.pos[2]

    IF (self.texture NE 0) THEN BEGIN
        tex[0,i] = 0.5
        tex[1,i] = 0.0
        tex[0,i-1] = 0.5
        tex[1,i-1] = 1.0
    ENDIF

    ; Fill in the connectivity array.
    i = 0
    FOR k=0,(nrows-2) DO BEGIN
        FOR j=0,(ncols-1) DO BEGIN
            conn[i] = 4

            conn[i+4] = k*ncols + j

            w = k*ncols + j + 1L
            IF (j EQ (ncols-1)) THEN w = k*ncols
            conn[i+3] = w

            w = k*ncols + j + 1L + ncols
            IF (j EQ (ncols-1)) THEN w = k*ncols + ncols
            conn[i+2] = w

            conn[i+1] = k*ncols + j + ncols

            i = i + 5L
            IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
                i = i - 5L
        ENDFOR
    ENDFOR
    FOR j=0,(ncols-1) DO BEGIN
        conn[i] = 3
        conn[i+3] = top
        conn[i+2] = j+1L
        IF (j EQ (ncols-1)) THEN conn[i+2] = 0
        conn[i+1] = j
        i = i + 4L
        IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
            i = i - 4L
    ENDFOR
    FOR j=0,(ncols-1) DO BEGIN
        conn[i] = 3
        conn[i+3] = bot
        conn[i+2] = j+(nrows-1L)*ncols
        conn[i+1] = j+(nrows-1L)*ncols+1L
        IF (j EQ (ncols-1)) THEN conn[i+1] = (nrows-1L)*ncols
        i = i + 4L
        IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
            i = i - 4L
    ENDFOR

    self.oPoly->SetProperty, DATA=verts, POLYGONS=conn

    IF (self.texture NE 0) THEN $
        self.oPoly->SetProperty, TEXTURE_COORD=tex
END
;----------------------------------------------------------------------------
;Defines the object structure for an orb object.
;** ***********
PRO orb__define
    struct = { orb, $
               INHERITS IDLgrModel, $
               pos: [0.0,0.0,0.0], $
               radius: 1.0, $
               density: 1.0, $
               texture: 0, $
               oPoly: OBJ_NEW() }
END

;----------------------------------------------------------------------------
;ARROW
;*****
;This function returns a 1 if initialization is successful, or 0 otherwise.
;******* *********
FUNCTION arrow::Init, POS=pos, RADIUS=radius, DENSITY=density, $
                      LENGTH=length, ANGLES=angles, _EXTRA=e

    IF (self->IDLgrModel::Init(_EXTRA=e) NE 1) THEN RETURN, 0

    self.pos     =[0.0,0.0,0.0]
    self.radius  = 0.5
    self.density = 1.0
    self.length  = 1.5
    self.angles  =[!pi/4,!pi/4,!pi/4]

    IF (N_ELEMENTS(pos)     EQ 3) THEN $
        self.pos     = pos

    IF (N_ELEMENTS(radius)  EQ 1) THEN $
        self.radius  = radius
        
    self.length  = self.radius*3.

    IF (N_ELEMENTS(density) EQ 1) THEN $
        self.density = density

    IF (N_ELEMENTS(length)  EQ 1) THEN $
        self.length  = length

    IF (N_ELEMENTS(angles)  EQ 3) THEN $
        self.angles  = angles

    ; Initialize the polygon object that will be used to represent
    ; the arrow.
    self.oPoly = OBJ_NEW('IDLgrPolygon', SHADING=1, /REJECT, _EXTRA=e)

    self->Add,self.oPoly

    ; Build the polygon vertices and connectivity based on property settings.
    self->BuildArrow

    RETURN, 1
END
;Cleans up all memory associated with the arrow.
;** ************
PRO arrow::Cleanup

    ; Cleanup the polygon object used to represent the arrow.
    OBJ_DESTROY, self.oPoly

    ; Cleanup the superclass.
    self->IDLgrModel::Cleanup
END
;Sets the value of properties associated with the arrow object.
;** ****************
PRO arrow::SetProperty, POS=pos, RADIUS=radius, DENSITY=density,$
                        LENGTH=length, ANGLES=angles, _EXTRA=e

    self->IDLgrModel::SetProperty, _EXTRA=e
    self.oPoly->SetProperty, _EXTRA=e

    IF (N_ELEMENTS(pos)     EQ 3) THEN $
        self.pos     = pos

    IF (N_ELEMENTS(radius)  EQ 1) THEN $
        self.radius  = radius

    IF (N_ELEMENTS(density) EQ 1) THEN $
        self.density = density

    IF (N_ELEMENTS(length)  EQ 1) THEN $
        self.length  = length

    IF (N_ELEMENTS(angles)  EQ 3) THEN $
        self.angles  = angles

    ; Rebuild the polygon according to keyword settings.
    self->BuildArrow
END
;Retrieves the value of properties associated with the arrow object.
;** ****************
PRO arrow::GetProperty, POS=pos, RADIUS=radius, DENSITY=density,$
                        POBJ=pobj, LENGTH=length, ANGLES=angles, _REF_EXTRA=re

    self->IDLgrModel::GetProperty, _EXTRA=re
    self.oPoly->GetProperty, _EXTRA=re

    pos    = self.pos
    radius = self.radius 
    density= self.density
    length = self.length
    angles = self.angles
    pobj   = self.oPoly
END
;represent the arrow.
;** ****************
PRO arrow::BuildArrow

    p3=self.pos(2) - self.length/2.2
    p4=self.pos(2) + self.length/1.8
    n =round(self.density* 20)
    m =n*3/2
    array1=fltarr(n,m)+self.radius
    array1(*,0)=self.radius/2
    
    l =(m/5)>2  &  q =self.radius*1.3  &  d =q/(l)
    
    array1(*,m-l)=q
    for i=m-l+1,m-1 do array1(*,i)=q-d*(i-m+l)
    
    MESH_OBJ,3,V,P,array1,P3=p3,p4=p4
    
    self.oPoly->SetProperty, DATA=V, POLYGONS=P
END

;Defines the object structure for an arrow object.
;** ***********
PRO arrow__define
    struct = { arrow, $
               INHERITS IDLgrModel, $
               pos: [0.0,0.0,0.0], $
               radius:  0.5, $
               density: 1.0, $
               length : 1.5, $
               angles :[!pi/4,!pi/4,!pi/4], $
               oPoly: OBJ_NEW() }
END
;--------------------------------------------------------------------------------

;toggle off and on state.
;**
;******* ********************
Function d_livMenuToggleState, wid   ;  IN: widget identifier

WIDGET_CONTROL, wid, GET_VALUE=name

s = STRPOS(name, '(Off)')
ret = s ne -1                   ;TRUE if new state is on
if ret then strput, name, '(On )', s $
else strput, name, '(Off)', strpos(name, '(On )')
WIDGET_CONTROL, wid, SET_VALUE=name
RETURN, ret
end

;**
;** *****************
pro d_livMenuSetState, Wid, NAMES=names, Indices = indices, State

; Set the value of a menu toggle item to State.  Either supply the
; Widget ID of the button (wid), or the button's name (wid) along with
; the arrays of button names and indices.

if n_elements(names) then begin ;Name supplied?
    index = where(strpos(names, Wid) ge 0, count)
    if count le 0 then print,Wid+' not found in menu'
    w = Indices(index(0))
endif else w = wid

WIDGET_CONTROL, w, GET_VALUE=value
On = STRPOS(value, '(On )')
if (on ge 0) eq State then return ;Already at proper value?

if On ge 0 then strput, value, '(Off)', on $ ;Change value of string
else strput, value, '(On )', strpos(value, '(Off)')

WIDGET_CONTROL, w, SET_VALUE=value ;update the widget
end



;----------------------------------------------------------------------------
;
;    PURPOSE   Given a uservalue from a menu button created
;              by d_livMenuCreate, return the index of the choice
;              within the category.  Set the selected menu button
;              to insensitive to signify selection, and set all
;              other choices for the category to sensitive.
;
;**
;******* ******************
function d_livMapMenuChoice, $
            Eventval, $         ; IN: uservalue from seleted menu button
            MenuItems, $        ; IN: menu item array, as returned by d_livMenuCreate
            MenuButtons         ; IN: button array as returned by d_livMenuCreate


i = STRPOS(eventval, '|', 0)    ;Get the name less the last qualifier
while (i GE 0) do begin
    j = i
    i = STRPOS(eventval, '|', i+1)
endwhile

base = STRMID(eventval, 0, j+1) ;  Get the common buttons, includes last | .
buttons = WHERE(STRPOS(MenuItems, base) EQ 0) ;  buttons that share base name.
this = (WHERE(eventval EQ MenuItems))(0) ;  Get the Index of the selected item.
for i=0, N_ELEMENTS(buttons)-1 do begin ;Each button in this category
    index = buttons(i)
    WIDGET_CONTROL, MenuButtons(buttons(i)), $
      SENSITIVE=index NE this
endfor

RETURN, this - buttons(0)       ;  Return the selected button's index.
end

;----------------------------------------------------------------------------
;
;    PURPOSE  Create a menu from a string descriptor (MenuItems).
;             Return the parsed menu items in MenuItems (overwritten),
;             and the array of corresponding menu buttons in MenuButtons.
;
;    MenuItems = (input/output), on input the menu structure
;                in the form of a string array.  Each button
;                is an element, encoded as follows:
;
;    Character 1 = integer bit flag.  Bit 0 = 1 to denote a
;                  button with children.  Bit 1 = 2 to denote
;                  this is the last child of its parent.
;                  Bit 2 = 4 to show that this button should
;                  initially be insensitive, to denote selection.
;                  Any combination of bits may be set.
;              On RETURN, MenuItems contains the fully
;                  qualified button names.
;
;    Characters 2-end = Menu button text.  Text should NOT
;                       contain the character |, which is used
;                       to delimit menu names.
;
;    Example:
;
;        MenuItems = ['1File', '0Save', '2Quit', $
;       '1Edit', '3Cut', $
;       '3Help']
;
;         Creates a menu with three top level buttons
;         (file, edit and help). File has 2 choices
;         (save and exit), Edit has one choice, and help has none.
;         On RETURN, MenuItems contains the fully qualified
;         menu button names in a string array of the
;         form: ['<Prefix>|File', '<Prefix>|File|Save',
;           '<Prefix>|File|Quit', '<Prefix>|Edit',..., etc. ]
;
;**
;** ***************
pro d_livMenuCreate, $
                MenuItems, $    ; IN/OUT: See below
                MenuButtons, $  ; OUT: Button widget id's of the created menu
                Bar_base, $     ; IN: menu base ID
                Prefix=prefix   ; IN: (opt) Prefix for this menu's button names.
                                ;     If omitted, no prefix

level = 0
parent = [ bar_base, 0, 0, 0, 0, 0]
names = STRARR(5)
lflags = INTARR(5)

MenuButtons = LONARR(N_ELEMENTS(MenuItems))

if (N_ELEMENTS(prefix)) then begin
    names(0) = prefix + '|'
endif else begin
    names(0) = '|'
endelse

widget_control,bad_id=i,Bar_base,UPDATE=0

for i=0, N_ELEMENTS(MenuItems)-1 do begin
    flag = FIX(STRMID(MenuItems(i), 0, 1))
    txt = STRMID(MenuItems(i), 1, 100)
    uv = ''

    for j = 0, level do uv = uv + names(j)
    MenuItems(i) = uv + txt     ;  Create the button for fully qualified names.
    isHelp = txt eq 'Help' or txt eq 'About'
    MenuButtons(i) = WIDGET_BUTTON(parent(level), $
                                   VALUE= txt, UVALUE=uv+txt, $
                                   MENU=flag and 1, HELP=isHelp)

    if ((flag AND 4) NE 0) then begin
        WIDGET_CONTROL, MenuButtons(i), SENSITIVE = 0
    endif

    if (flag AND 1) then begin
        level = level + 1
        parent(level) = MenuButtons(i)
        names(level) = txt + '|'
        lflags(level) = (flag and 2) NE 0
    endif else if ((flag AND 2) NE 0) then begin
        while lflags(level) do level = level-1 ;  Pops the previous levels.
        level = level - 1
    endif
endfor
widget_control,bad_id=i,Bar_base,UPDATE=1
end


;**
;******* *************
function d_livReadNoff, $
             file, $            ; IN: filename
             xr, $              ; OUT: x radius
             yr, $              ; OUT: y radius
             zr, $              ; OUT: z radius
	     mesh		; IN: if filename=''
s = ' '
npsize = 1

if file gt ' ' then RESTORE, file $
else begin
	x=xr & y=yr & z=zr & endelse

xr = [min(x, max = xx), xx]     ;Get ranges
yr = [min(y, max = xx), xx]
zr = [min(z, max = xx), xx]

sc = [xr(1)-xr(0), yr(1)-yr(0), zr(1)-zr(0)] ;Ranges
xr(0) = (xr(1) + xr(0))/2.0     ;Midpoint
yr(0) = (yr(1) + yr(0))/2.0
zr(0) = (zr(1) + zr(0))/2.0
s = max(sc)                     ;Largest range...

x = (x - xr(0)) / s
y = (y - yr(0)) / s
z = (z - zr(0)) / s

xr = [-0.7, 0.7]                ;Fudge the ranges
yr = xr
zr = xr
s = OBJ_NEW("IDLgrPolygon", TRANSPOSE([[x],[y],[z]]), $
            SHADING=1, $
            POLY=mesh, COLOR=[200,200,200])
RETURN, s
end


;Read the molecule data input file.
;**
;******* ************
function d_livMolRead, $
    filename, $          ; IN: filename
    atom_xyz, $
    atoms   , $
    amesh   , $
    img, Np

n_atoms = 0L
img     = 0

CASE filename of

'caffeine.mol':begin
   atom_xyz=[[-0.802,-0.906, 0.120 ],[-0.802, 0.494,-0.114 ],[ 0.411, 1.194, 0.120 ],$
	     [ 1.622, 0.494,-0.114 ],[ 1.622,-0.906, 0.120 ],[ 0.411,-1.606,-0.114 ],$
	     [-2.134, 0.928, 0.120 ],[-2.134,-1.337,-0.114 ],[-2.958,-0.203, 0.120 ],$
	     [-2.134, 2.328,-0.114 ],[ 0.411,-3.006, 0.120 ],[ 2.836, 1.194,-0.114 ],$
	     [ 2.836,-1.606, 0.120 ],[ 0.411, 2.594,-0.114 ],[-1.055,-1.102, 1.172 ],$
	     [-1.055, 0.691,-1.166 ],[-4.021,-0.098, 0.281 ],[-2.838, 2.565,-0.925 ],$
	     [-2.442, 2.853, 0.803 ],[-1.123, 2.652,-0.400 ],[ 0.664,-3.533,-0.812 ],$
	     [ 1.156,-3.249, 0.892 ],[-0.586,-3.321, 0.460 ],[ 3.493, 0.789, 0.670 ],$
	     [ 3.323, 1.084,-1.094 ],[ 2.644, 2.260, 0.082 ]]
   atoms   = [6,6,6,7,6,7,7,7,6,6,6,-6,8,8,1,1,1,1,1,1,1,1,1,1,1,1]
   n_atoms = n_elements(atoms)
   amesh   = [8,  1,2,3,4,5,6,1,$
	      6,  1,8,9,7,2,$
	      3,  7,10,	  3,6,11,   3,5,13,   3,3,14,   3,4,12,$
	      3,  10,18,  3,10,19,  3,10,20,  3,9,17,   3,2,16,$
	      3,  1,15,   3,11,21,  3,11,22,  3,11,23,  3,12,24,  3,12,25,  3,12,26] -1
   a=64 & img=fltarr(a,a)
   xmi=min(atom_xyz(0,*),max=xma)
   ymi=min(atom_xyz(1,*),max=yma)
   stx=(a-6.)/(xma-xmi)
   sty=(a-6.)/(yma-ymi)
   for  i=0,n_atoms-1 do begin
	px=round((atom_xyz(0,i)-xmi)*stx)+3
	py=round((atom_xyz(1,i)-ymi)*sty)+3
	if atoms(i) eq 1 then img(px,py)=-2 else img(px,py)=atoms(i)
   endfor
   img=smooth(smooth(img,3),3)
   Np =[min(atom_xyz(0,*),max=xma),xma,min(atom_xyz(1,*),max=yma),yma,min(atom_xyz(2,*),max=zma),zma]
   end
ELSE:begin
   atom_xyz = FLTARR(3,1000, /nozero)
   x = (y = (z = 0.))
   atoms = INTARR(1000, /nozero)

   OPENR, lun, filename, /GET_LUN, error = i
   if i lt 0 then print, filename + ' not found'
   s = " "

   while eof(lun) eq 0 do begin
    READF, lun, s
    i = strpos(s, ":")
    if i NE -1 then begin
        READS, strmid(s,i+1,strlen(s)-i+1),x,y,z
        case strmid(s,0,2) of
            "C:" : a_type = 6  ;z=6   m=12.0111
            "H:" : a_type = 1  ;z=1   m=1.00797
            "N:" : a_type = 7  ;z=7   m=14.0067
            "S:" : a_type = 16 ;z=16  m=32.064
            "X:" : a_type = 54 ;z=54  m=131.30
            "Ce" : a_type = 58 ;z=58  m=140.12
            "BR" : a_type = 35 ;z=35  m=79.909
            "O:" : a_type = 8  ;z=8   m=15.9994
            else : a_type = 0
        endcase

        if (a_type NE -1) then begin
            atom_xyz(0,n_atoms) = x
            atom_xyz(1,n_atoms) = y
            atom_xyz(2,n_atoms) = z
            atoms(n_atoms) = a_type
            n_atoms = n_atoms + 1
            if n_atoms ge n_elements(atoms) then $
              print, 'Too many atoms in molecule'
        endif
    endif
   endwhile
   endoffile:
   FREE_LUN,lun
   if (n_atoms GE 0) then begin
    atoms = atoms(0:n_atoms-1)
    atom_xyz = atom_xyz(*,0:n_atoms-1)
   endif
   end
ENDCASE
RETURN, n_atoms
end

;**
;** ***************
pro d_livMakeAtomes

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow
A_symb=['a' , $
	'H' ,'He','Li','Be','B' ,'C' ,'N' ,'O' ,'F' ,'Ne','Na','Mg','Al','Si','P' ,$
        'S' ,'Cl','Ar','K' ,'Ca','Sc','Ti','V' ,'Cr','Mn','Fe','Co','Ni','Cu','Zn',$
        'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y' ,'Zr','Nb','Mo','Tc','Ru','Rh',$
        'Pd','Ag','Cd','In','Sn','Sb','Te','I' ,'Xe','Cs','Ba','La','Ce','Pr','Nd']
	A_symb=[A_symb,$
	'Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W' ,'Re',$
        'Os','Ir','Pt','Au','Hg','Ti','Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th',$
        'Pa','U' ,'Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No','Lr'          ,$
        'Unq','Unp','Unh','Uns','Uno','Une','Unk']
A_name=['Atom', $
	'Hydrogen',	'Helium',	'Lithium',	'Beryllium',	'Bore',		$
	'Carbon',	'Azote',	'Oxygen',	'Fluor',	'Neon',		$
	'Sodium',	'Magnesium',	'Aluminium',	'Silicium',	'Phosphore',	$
	'Sulfur',	'Chlore',	'Argon',	'Potassium',	'Calcium',	$
	'Scandium',	'Titane',	'Vanadium',	'Chrome',	'Manganese',	$
	'Fer',		'Cobalt',	'Nickel',	'Cuivre',	'Zinc',		$
	'Gallium',	'Germanium',	'Arsenic',	'Selenium',	'Brome',	$
	'Krypton',	'Rubidium',	'Strontium',	'Yttrium',	'Zirconium',	$
	'Nobium',	'Molybdene',	'Technetium',	'Ruthenium',	'Rhodium',	$
	'Palladium',	'Argent',	'Cadmium',	'Indium',	'Etain',	$
	'Antimoine',	'Tellurium',	'Iode',		'Xenon',	'Cesium']
	A_name=[A_name, $
	'Baryum',	'Lanthane',	'Cerium',	'Praseodyme',	'Neodyme',	$
	'Promethium',	'Samarium',	'Europium',	'Gadolinium',	'Terbium',	$
	'Dysprosium',	'Holmium',	'Erbium',	'Thulium',	'Ytterbium',	$
	'Lutenium',	'Hafnium',	'Tantale',	'Tungstene',	'Rhenium',	$
	'Osmium',	'Iridium',	'Platine',	'Or',		'Mercure',	$
	'Tallium',	'Plomb',	'Bismuth',	'Polonium',	'Astate',	$
	'Radon',	'Francium',	'Radium',	'Actinium',	'Thorium',	$
	'Protactinium',	'Uranium',	'Neptunium',	'Plutonium',	'Americium',	$
	'Curium',	'Berkelium',	'Californium',	'Einsteinium',	'Fermium',	$
	'Mendelevium',	'Nobelium',	'Lawrencium',	                                $
	'Unnilquadium',	'Unnilpentium', 'Unnilhexium',  'Unnilseptium',	'Unniloctium', 'Unnilennium', 'Unknown']
A_dens=[0.0700, $
	0.0899, 0.1787, 0.5300, 1.8500, 2.3400, 2.6200, 1.2510, 1.4290, 1.6960, 0.9010, $
	0.9700, 1.7400, 2.7000, 2.3300, 1.8200, 2.0700, 3.1700, 1.7840, 0.8600, 1.5500, $
	3.0000, 4.5000, 5.8000, 7.1900, 7.4300, 7.8600, 8.9000, 8.9000, 8.9600, 7.1400, $
	5.9100, 5.3200, 5.7200, 4.8000, 3.1200, 3.7400, 1.5300, 2.6000, 4.5000, 6.4900, $
	8.5500, 10.200, 11.500, 12.200, 12.400, 12.000, 10.500, 8.6500, 7.3100, 7.3000, $
	6.6800, 6.2400, 4.9200, 5.8900, 1.8700, 3.5000, 6.7000, 6.7800, 6.7700, 7.0000]
	A_dens=[A_dens, $
	6.4750, 7.5400, 5.2600, 7.8900, 8.2700, 8.5400, 8.8000, 9.0500, 9.3300, 6.9800, $
	9.8400, 13.100, 16.600, 19.300, 21.000, 22.400, 22.500, 21.400, 19.300, 13.530, $
	11.850, 11.400, 9.8000, 9.4000, 9.5000, 9.9100, 9.5000, 5.0000, 10.070, 11.700, $ ;verify 85 & 87 (9.5)
	15.400, 18.900, 20.400, 19.800, 13.600, 13.511, 14.000, 14.000, 14.000, 14.000, $ ;verify (14.0)
	14.000, 14.000, 14.000, 14.000, 14.000, 14.000, 14.000, 14.000,                 $
	14.000, 14.000, 14.000, 14.000, 14.000, 14.000, 14.000]
d=111
a_col =bytarr(3,d)
idx_1 =[3,4,11,12,13,19,20,30,31,37,38,48,49,50,51,55,56,80,81,82,83,84,87,88] ;Metaux simples     (blue)
idx_2 =[indgen(9)+21 ,indgen(9)+39 ,indgen(8)+72 ]                             ;Elts de transition (orange)
idx_3 =[5,6,7,8,9,14,15,16,17,32,33,34,35,52,53,85]                            ;Non-metaux         (violet)
idx_4 =[indgen(15)+57,indgen(15)+89]                                           ;Terres rares       (green)
idx_5 =[2,10,18,36,54,86]                                                      ;Gaz nobles         (pink)
idx_6 =[0,1,104,105,106,107,108,109,110]                                       ;Others             (pale blue)

a_col(0,idx_1)= 20 & a_col(1,idx_1)= 20 & a_col(2,idx_1)=230
a_col(0,idx_2)=230 & a_col(1,idx_2)=100 & a_col(2,idx_2)= 20
a_col(0,idx_3)=100 & a_col(1,idx_3)= 20 & a_col(2,idx_3)=230
a_col(0,idx_4)= 20 & a_col(1,idx_4)=230 & a_col(2,idx_4)= 20
a_col(0,idx_5)=230 & a_col(1,idx_5)= 20 & a_col(2,idx_5)=190
a_col(0,idx_6)=150 & a_col(1,idx_6)=150 & a_col(2,idx_6)=230

a_col(0,0)  =[255,255,255]
a_col(0,1)  =[200,200,255]
a_col(0,6)  =[128,128,128]
a_col(0,7)  =[0  ,0  ,255]
a_col(0,8)  =[255,0  ,0  ]
a_col(0,16) =[230,230,0  ]
a_col(0,35) =[204,25 ,230]
a_col(0,54) =[25 ,25 ,0  ]
a_col(0,58) =[204,0  ,153]
 
a_sphere=objarr(d)
a_arrow =objarr(d)

end

;**
;******* ***************
function d_livMakeSphere, ty

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

dens = .8
r    = .6

if a_sphere(ty) eq OBJ_NEW() then begin
   a_sphere(ty) =  OBJ_NEW('orb', COLOR=a_col(*,ty), DENSITY=dens, RADIUS=(sqrt(a_dens(ty))>.6)*r)
end

RETURN, a_sphere(ty)
end
;**
;******* ***************
function d_livMakeArrow, ty

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

dens = 1.
r    = .6
rad  = (sqrt(a_dens(ty))>.6)*r/3.

if a_arrow(ty) eq OBJ_NEW() then begin
   a_arrow(ty) =  OBJ_NEW('arrow', COLOR=a_col(*,ty), DENSITY=dens, RADIUS=rad, LENGTH=rad*8.5, ANGLES=[0.,0.,0.])
end

RETURN, a_arrow(ty)
end


;**
;** *************
Pro d_livAddLight, Model             ;Set up the lighting
Model->Add, OBJ_NEW('IDLgrLight', $ ;Directional
                    LOCATION=[2,2,5], TYPE=2, COLOR=[255,255,255], $
                    INTENSITY=0.6 )

Model->Add, OBJ_NEW('IDLgrLight', $ ;Directional
                    LOCATION=[2,-2,-5], TYPE=2, COLOR=[255,255,0], $
                    INTENSITY=0.25 )

Model->add, OBJ_NEW('IDLgrLight', TYPE=0, $ ;Ambient light
                    INTENSITY=0.375, COLOR=[255,255,255])
end

;**
;******* *****************
Function d_livMakeMolecule, filename, state ,atom_xyz, atom_type, amesh, img ,Np, dup

;oModelTop <- oModelSurface <- oModelAtom*n <- Polygon sharing data from MakeAtomes
;oSurface used for styles   <- oModelAtom*n

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

if  n_elements(a_symb) eq 0 then d_livMakeAtomes

n =n_elements(atom_type)
if n eq 0 then n=d_livMolRead(filename, atom_xyz, atom_type, amesh, img, Np)  ;Get atoms
atom_typs=abs(atom_type) <n_elements(a_name)

sc = 0.35 * 2.0 / (max(atom_xyz(0,*), min = mn) - mn) ;X extent
xmi= min(atom_xyz(0,*),max=xma)
ymi= min(atom_xyz(1,*),max=yma)
zmi= min(atom_xyz(2,*),max=zma)

state.oModelTop = OBJ_NEW('IDLgrModel')       ;Top model
state.oModelSurface = OBJ_NEW('IDLgrModel')
state.oModelTop->add, state.oModelSurface

state.oModelOffset = OBJ_NEW('IDLgrModel')
state.oModelOffset->translate, 0, 0, 0.005   ;Offset Z to make visible
state.oModelEdges = OBJ_NEW('IDLgrModel')
state.oModelOffset->add, state.oModelEdges


; destroy the old objects prior to saving the new ones
obj_destroy, state.oSurface
state.oSurface = OBJ_NEW('IDL_CONTAINER')

QX=0. & QY=0. & QZ=0.
iz= SIZE(img)
if  iz(0) eq 2 then begin
   ;GRAVITY
    QX=.125/sc ;(Color bar)
    QX=total(atom_xyz(0,*)) /n + QX
    QY=total(atom_xyz(1,*)) /n + QY
    QZ=total(atom_xyz(2,*)) /n + QZ
    atom_xyz(0,*)=atom_xyz(0,*)- QX
    atom_xyz(1,*)=atom_xyz(1,*)- QY
    atom_xyz(2,*)=atom_xyz(2,*)- QZ
    
    xmi=Np(0)-QX & xma=Np(1)-QX
    ymi=Np(2)-QY & yma=Np(3)-QY
    
    TVLCT ,r, g, b ,/get
    dmi=min(img,max=dma) & sz=n_elements(r) & rg=float(dma-dmi)/(sz-1) & zh=round(-dmi/rg)
    if (zh gt 1) and (zh lt sz) then begin zh=zh+1
	gray=bytscl((findgen(zh))/zh*sz)
	r(0:zh-1)=gray & g(0:zh-1)=gray & b(0:zh-1)=gray
    endif
    imb= bytscl(img) & imb=byte(round(imb*((sz-1)/float(max(imb)))))
    oP = OBJ_NEW('IDLgrPalette',r,g,b)
    oI = OBJ_NEW('IDLgrImage',imb,palette=oP)

    state.oModelImage  = OBJ_NEW('IDLgrModel')
    state.oImage = OBJ_NEW('IDLgrPolygon', [xmi,xma,xma,xmi], [ymi,ymi,yma,yma], [zmi,zmi,zmi,zmi],$
                                 poly=[4,0,1,2,3], color=[255,255,255], /texture_interp, $
    				 texture_map=oI, texture_coord=[[0,0],[1,0],[1,1],[0,1]])

    IF !VERSION.release ge '5.2' then begin
    Bar = OBJ_NEW('IDLgrColorBar', R,G,B, COLOR=[255, 255, 255], SHOW_AXIS=2, /THREED)
    Bar-> GetProperty,MAJOR=major
    Tex = OBJ_NEW('IDLgrText',STRING(FINDGEN(major)/(major-1)*(dma+abs(dmi))-abs(dmi), $
                              FORMAT='(F5.2)'))
    Bar-> SetProperty, TICKTEXT=Tex
    Bar-> GetProperty, XRANGE=xrange, YRANGE=yrange, ZRANGE=zrange, TICKLEN=tl
    xLen = xrange[1]-xrange[0]
    yLen = yrange[1]-yrange[0]
    zLen = zrange[1]-zrange[0]
    maxLen =  yLen /(yma-ymi)
    xs = [xma+.2, 1.0 / maxLen]
    ys = [ymi   , 1.0 / maxLen]
    zs = [zmi   , 1.0 / maxLen]
    Bar->SetProperty, XCOORD_CONV=xs, YCOORD_CONV=ys, ZCOORD_CONV=zs, TICKLEN=tl/2.5
    state.oModelImage   ->Add, Bar
    endif

    state.oModelImage   ->Add, state.oImage
    state.oModelSurface ->Add, state.oModelImage
    state.oModelImage   ->add, OBJ_NEW('IDLgrLight', TYPE=0, INTENSITY=.9, COLOR=[255,255,255])
endif

if n_elements(amesh) gt n then begin
    nn=n_elements(atom_type)
    vcol=bytarr(3,nn) & for i=0,nn-1 do vcol(*,i)=a_col(*,atom_typs(i))
    
    state.oModelSticks = OBJ_NEW('IDLgrModel')
    state.oSticks = OBJ_NEW('IDLgrPolyline', DATA=atom_xyz, POLYLINE=amesh, shading=1, thick=3, vert_colors=vcol) ;COLOR=[50,50,50])
    state.oModelSticks ->Add, state.oSticks
    state.oModelSurface->Add, state.oModelSticks
endif

for kz=0,dup(2)-1 do begin
for ky=0,dup(1)-1 do begin
for kx=0,dup(0)-1 do begin
for i =0,n-1 do begin            ;For each atom in molecule
    s = d_livMakeSphere(atom_typs(i))   ;A sphere for the atom
    s ->GetProperty, POBJ=sh
    sh->GetProperty, COLOR=col, POLY=pmesh
    p = OBJ_NEW('IDLgrPolygon', SHARE_DATA=sh, POLY=pmesh, COLOR=col)
    p ->SetProperty, SHADING=1
    if  atom_type(i) lt 0 then p ->SetProperty, HIDE=1
    
    if  atom_type(i) lt 0 then begin
    s = d_livMakeArrow (atom_typs(i))   ;An arrow for the atom
    s ->GetProperty, POBJ=sh
    sh->GetProperty, COLOR=col, POLY=pmesh
    pa= OBJ_NEW('IDLgrPolygon', SHARE_DATA=sh, POLY=pmesh, COLOR=col)
    pa->SetProperty, SHADING=1
    endif
    
    oModelAtom = OBJ_NEW('IDLgrModel', /SELECT_TARGET)
    oModelAtom->SetProperty, UVALUE=string(a_name(atom_typs(i)), atom_xyz(0,i)+QX, atom_xyz(1,i)+QY, $
                                           atom_xyz(2,i)+QZ, format='(A, " at [", 3F6.2, "]")')

    if  atom_type(i) lt 0 then begin
	oModelAtom->Rotate,[1,1,0],60.
	oModelAtom->Scale, .6,.6,.6
    endif

    oModelAtom->Translate,  atom_xyz(0,i)-(xma-xmi)*kx, atom_xyz(1,i)-(yma-ymi)*ky, atom_xyz(2,i)-(zma-zmi)*kz
    
    oModelAtom    ->Add, p
    state.oSurface->add, p
    if  atom_type(i) lt 0 then oModelAtom->Add, pa

    state.oModelSurface->Add, oModelAtom
    
                                ; Make the mesh object
    oModelAtomEdge = OBJ_NEW('IDLgrModel')
    p1 = OBJ_NEW('IDLgrPolyline', SHARE_DATA=sh, POLYLINE=pmesh, COLOR=[0,0,0])
    state.oModelEdges->add, oModelAtomEdge
    oModelAtomEdge->add, p1
    ; Don't add offset here.  Do it below to parent model
    oModelAtomEdge->translate, atom_xyz(0,i)-(xma-xmi)*kx,atom_xyz(1,i)-(yma-ymi)*ky, atom_xyz(2,i)-(zma-zmi)*kz
endfor
endfor
endfor
endfor

state.oModelOffset->translate, 0, 0, 0.005
state.oModelSurface->Scale, sc, sc, sc
state.oModelEdges->Scale, sc, sc, sc

d_livAddLight, state.oModelSurface

if state.vert_coloring then begin
    state.vert_coloring = 0
    d_livMenuSetState, '|Vertex Coloring', 0, $
      Names=state.menuitems, Indices=state.menubuttons
endif

state.oModelTop->add, state.oModelOffset

return, state.oModelTop
end

;**
;******* **************
Function d_livMakeShape, filename, state ,xr,yr,zr,mesh

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

state.oModelTop = OBJ_NEW('IDLgrModel')       ;Top model

state.oSurface = d_livReadNoff(filename, xr, yr, zr ,mesh)
state.oSurface->SetProperty, VERT_COLORS= vcc

state.oModelSurface = OBJ_NEW('IDLgrModel')
state.oModelTop->add, state.oModelSurface
state.oModelSurface->add, state.oSurface

state.oModelOffset = OBJ_NEW('IDLgrModel')
state.oModelOffset->translate, 0, 0, 0.005   ;Offset Z to make visible
state.oModelEdges = OBJ_NEW('IDLgrModel')
state.oModelOffset->add, state.oModelEdges
state.oSurface->GetProperty, POLY=pmesh
p1 = OBJ_NEW('IDLgrPolyline', SHARE_DATA=state.oSurface, POLY=pmesh, $
   COLOR=[0,0,0])
state.oModelEdges->add, p1                  ;Add the edging data

if state.vert_coloring eq 1 then begin ;Default = vertex coloring OFF
    state.vert_coloring = 0
    d_livMenuSetState, '|Vertex Coloring', 0, $
      NAMES=state.menuitems, INDICES=state.menubuttons
endif

state.oModelTop->add, state.oModelOffset

d_livAddLight, state.oModelTop

return, state.oModelTop
end


;**
;******* ****************
Function d_livMakeLampWks, filename, state

wi=fix(filename)
TAKE_DATP,datp,W=wi
if n_elements(state.did_xx) eq 1 then xr  =datp.x     else xr  =state.did_xx
if n_elements(state.did_yy) eq 1 then yr  =datp.y     else yr  =state.did_yy
if n_elements(state.did_zz) eq 1 then zr  =datp.z     else zr  =state.did_zz
if n_elements(state.did_ee) eq 1 then ee  =datp.e     else ee  =state.did_ee
if n_elements(state.did_nn) eq 1 then N   =datp.n     else N   =state.did_nn
if n_elements(state.did_vv) eq 1 then mesh=datp.PV    else mesh=state.did_vv
if n_elements(state.did_ww) eq 1 then TAKE_W,W,W=wi   else W   =state.did_ww
P     =datp.P & P=float(P)
thresh=state.did_thresh
repr  =state.did_repr

DID_OBJET,wi,dobj
case dobj of
 1:	begin ;Molecule (mesh=links, p1->p6=dimension, p7->p9=angles, p10=#atoms, N=density)
        sz=SIZE(N)
 	atom_xyz=transpose([[xr],[yr],[zr]])
	n_atom  =0
	dup=[1,1,1]
	Np =0

        if sz(0) ge 2 then begin
	  if n_elements(P) lt 10 then Np =[min(atom_xyz(0,*),max=xma),xma,min(atom_xyz(1,*),max=yma),$
	                                   yma,min(atom_xyz(2,*),max=zma),zma] $
	  else begin
	     x1=min (xr,max=x2) & spx=abs((p(2)-p(1))/(sz(1)-1))
	                          sx1=abs(fix ((x1-p(1))/spx)) & sx2=abs(fix((p(2)-x2)/spx))
 	     y1=min (yr,max=y2) & spy=abs((p(4)-p(3))/(sz(2)-1))
	                          sy1=abs(fix ((y1-p(3))/spy)) & sy2=abs(fix((p(4)-y2)/spy))
	     z1=min (zr,max=z2) & spz=abs((p(6)-p(5))/(sz(3)-1))
	                          sz1=abs(fix ((z1-p(5))/spz)) & sz2=abs(fix((p(6)-z2)/spz))
	     if sz(0) eq 3 then begin
		N=N(sx1:sz(1)-1-sx2,sy1:sz(2)-1-sy2,sz1:sz(3)-1-sz2)
		N=TOTAL(N,3)
             endif else $
		N=N(sx1:sz(1)-1-sx2,sy1:sz(2)-1-sy2)

	     n_atom=p(10)
             Np =[p(1)+sx1*spx, p(2)-sx2*spx, p(3)+sy1*spy, p(4)-sy2*spy]
	  endelse
        endif
	sm=size(mesh)
	if (sm(0) eq 2) and (sm(1) eq 2) then begin
		links=0L & for i=0,sm(2)-1 do links=[links,2,mesh(*,i)] & mesh=links(1:sm(2)*3)
	endif else if n_elements(W) ne n_atom then mesh=0
	
	return, d_livMakeMolecule('', state ,atom_xyz, W, mesh, N, Np, dup)
	end
 2:	begin ;Shape
	return, d_livMakeShape   ('', state ,xr,yr,zr,mesh)
	end
 else:
endcase

return,OBJ_NEW()
end


;**
;** ************************
pro d_livSetObjectAttributes, state ;Set object's attrib to current settings

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

if state.oModelEdges ne obj_new() then $
  d_livSetProp, state.oModelEdges, HIDE=1-state.edging, EXCLUD=state.oSticks

if state.vert_coloring then vc = vcc else vc = 0
if state.bottomColor then bot = 0 else bot = [64, 192, 128]

d_livSetProp, state.oSurface, STYLE=([0,1,2,0,1])(state.style), $
  HIDDEN_LINE=([0,0,0,1,1])(state.style), BOTTOM=bot, REJECT = state.reject, $
  THICK=state.thick, VERT_COLORS=vc, SHADING=state.shading, EXCLUD=state.oSticks
end


;**
;** *************
pro d_livLoadItem, index, State      ;Load the new item...

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

filename = Items(2,index)

state.oModelTop = call_function(Items(1,index), filename, state) ;Load it

state.oModelSurface->GetProperty, TRANSFORM=tmg0

state.oView->add, state.oModelTop

state.tmg0 = tmg0
state.cur_sel = obj_new()       ;Nothing's selected
d_livSetObjectAttributes, state    ; Now set the attributes to current values

if filename eq 'caffeine.mol' then begin
  s=2.14
  state.scaling=s
  t=[[0.0693024,   -0.0396087,   0.00211983,     0.000000],$
    [ 0.0164601,    0.0326003,    0.0710102,     0.000000],$
    [-0.0360888,   -0.0611926,    0.0364585,     0.000000],$
    [ 0.000000,     0.000000,     0.000000,      1.00000]]
  state.oModelSurface->SetProperty, TRANSFORM = t
  state.oModelEdges  ->SetProperty, TRANSFORM = t
  state.oModelTop->SetProperty, TRANSFORM = [[s,0,0,0],[0,s,0,0], [0,0,s,0], [0,0,0,1]]
  state.oModelTop->GetProperty, TRANSFORM = t
  state.tmg0 = t
endif
end


;**
;** ************
PRO d_livSetProp, o, EXCLUD=Sticks, _extra = e ;Set a property on either a container that
; contains a number of objects, or a single object.

if obj_class(o) eq 'IDL_CONTAINER' then arr = o->get(/all) $
else arr = o
for i=0, n_elements(arr)-1 do if arr(i) ne Sticks then arr(i)->setproperty, _EXTRA=e
end


;**
;** ****************
pro d_livNewSelected, state, target

WIDGET_CONTROL, state.wDraw , GET_UVALUE=basets & baset=basets.baset
       targets=basets.targets & styles=basets.styles

if (N_ELEMENTS(target) EQ 1) then begin
    target->GetProperty, HIDE =hid
    ostyle = ([0,1,2,0,1])(state.style) ;Original style
    if ostyle eq 2 then nstyle = 1 else nstyle = 2 ;New style
    if hid then ostyle=0
    if ostyle eq 0 then nstyle = 0
    
    idx=n_elements(targets)
    if idx gt 0 then idx=where(targets eq target) $
    else begin  targets=[target] & styles=[ostyle] & endelse
    if  idx(0)  eq -1 then begin   idx=n_elements(targets)
                targets=[targets,target] & styles=[styles,ostyle] & endif
    idx=idx(0)
    if styles(idx) eq ostyle then styles(idx)=nstyle else styles(idx)=ostyle
    if (nstyle eq 0) and (hid eq 0) then hid=1 else hid=0
    target->SetProperty, STYLE=styles(idx), HIDE=hid
    WIDGET_CONTROL, state.wDraw , SET_UVALUE={baset:baset,targets:targets,styles:styles}
endif
end

;**
;** **************
pro d_livDrawPrint, wDraw , flag        ;Handle print event

WIDGET_CONTROL, wDraw , GET_UVALUE=basets & baset=basets.baset
WIDGET_CONTROL, baset , GET_UVALUE=state
file='lamp_W'+strtrim(string(state.did_wi),2)
case flag of
'PS':	begin
        oPrinter= obj_new('IDLgrPrinter')
;	if (dialog_printersetup(oPrinter)) then begin
;	    if (dialog_printjob(oPrinter)) then begin
                state.oWindow->GetProperty,resolution=res,dimensions=Sxy
                DPI = 2.54/float(res)
                ;Hack, swap from pixels to inches for views...
                ;If a scene is defined: oViews=scene->get(/all)
                state.oView  ->IDLgrView::getproperty,loc=loc ,dim=vdim ,color=col
                    locp  = loc /DPI
                    vdimp = Sxy /DPI
                state.oView  ->IDLgrView::setproperty,loc=locp,dim=vdimp,units=1,color=[245,245,245]

                ;PRINT...
                oPrinter->Draw,state.oView
                oPrinter->newdocument

                ;...and back to pixels
                state.oView  ->IDLgrView::setproperty,loc=loc ,dim=vdim ,units=0,color=col
;	    endif
;	endif
        obj_destroy,oPrinter
	end
'GIF':	begin	file=file+'.gif'
		state.oWindow->GetProperty, dimensions=Sxy
		Obuf= obj_new('IDLgrBuffer',dimensions=Sxy)
		Obuf->draw, state.oView
		Oimg=Obuf->read() & Oimg->GetProperty,data=w
		obj_destroy,Obuf  & obj_destroy,Oimg
		
		tz  =size(w)      & tbas=widget_base(map=0) & r=0
		if tz(0) eq 3 then begin w=Color_Quan(w,1,r,g,b,/dither)
		                         tz=size(w) & tvlct,kr,kg,kb,/get & endif
		tdrw=widget_draw(tbas,retain=2,xsize=tz(1),ysize=tz(2))
		widget_control,tbas,/realize & widget_control,tdrw,get_value=WinId
		wset,WinId & if n_elements(r) gt 1 then tvlct,r,g,b   & tv,w
 		
		if n_elements(r) gt 1 then WRITE_KIF,file,w,r,g,b else WRITE_KIF,file,tvrd()
 		widget_control,tbas,/destroy
		if n_elements(r) gt 1 then tvlct,kr,kg,kb
                WIDGET_CONTROL,state.l_mess,bad_id=ii,set_value=file+' is updated'
	end
'SCREEN':begin	file=file+'.ps'
		state.oWindow->GetProperty, dimensions=Sxy
		Obuf= obj_new('IDLgrBuffer',dimensions=Sxy)
                state.oView  ->IDLgrView::getproperty,color=col
                state.oView  ->IDLgrView::setproperty,color=[180,180,180]
		Obuf->draw, state.oView
                state.oView  ->IDLgrView::setproperty,color=col
		Oimg=Obuf->read() & Oimg->GetProperty,data=w
		obj_destroy,Obuf  & obj_destroy,Oimg
		
		tz  =size(w)      & tbas=widget_base(map=0) & r=0
		if tz(0) eq 3 then begin w=Color_Quan(w,1,r,g,b,/dither)
		                         tz=size(w) & tvlct,kr,kg,kb,/get & endif
		tdrw=widget_draw(tbas,retain=2,xsize=tz(1),ysize=tz(2))
		widget_control,tbas,/realize & widget_control,tdrw,get_value=WinId
		wset,WinId & if n_elements(r) gt 1 then tvlct,r,g,b   & tv,w

		P_DID_EVENT, 0, [-88,350,WinId,0,state.did_wi,0,1,0]
 		widget_control,tbas,/destroy
		if n_elements(r) gt 1 then tvlct,kr,kg,kb
                WIDGET_CONTROL,state.l_mess,bad_id=ii,set_value=file+' is updated'
	end
'VRML':	begin	file=file+'.wrl'
                state.oWindow->GetProperty   ,dimensions=wdims, $
                          resolution=res,color_model=cm,n_colors=icolors
                oVRML=obj_new('IDLgrVRML',dimensions=wdims, $
                          resolution=res,color_model=cm,n_colors=icolors)
                oVRML->setproperty, filename=file
                oVRML->Draw,state.oView
                obj_destroy,oVRML
                WIDGET_CONTROL,state.l_mess,bad_id=ii,set_value=file+' is updated'
	end
ELSE:
endcase
end

;**
;** **************
pro d_livDrawEvent, ev        ;Handle events for the draw window

WIDGET_CONTROL, ev.id , GET_UVALUE=basets & baset=basets.baset
WIDGET_CONTROL, baset , GET_UVALUE=state, /NO_COPY

if (ev.type EQ 4) then begin ;  Expose.
    WIDGET_CONTROL, baset, /HOURGLASS ;Redraw entire window
    state.oWindow->draw, state.oView

endif else begin                ;Not expose
    bHaveTransform=0
    bHaveScaling  =0
    if state.did_scaling eq 0 then begin
      bHaveTransform = state.oTrack->Update(ev, TRANSFORM=qmat) ;trackball update
      if (bHaveTransform NE 0) then begin
        state.oModelSurface->GetProperty, TRANSFORM=t
        state.oModelSurface->SetProperty,TRANSFORM= t # qmat
        state.oModelEdges->SetProperty,TRANSFORM= t # qmat
      endif
    endif else if (ev.Y ge 0) then begin
      delta=(ev.Y - state.did_oldny)/2
      kpscl= state.did_scalval
      state.did_scalval=(state.did_scalval+delta)<300>40
      state.did_oldny  = ev.Y
      if state.did_scalval ne kpscl then begin
		s=state.did_scalval/100.
		state.scaling =s
		bHaveScaling  =1
		state.oModelTop->SetProperty, $
		      TRANSFORM = [[s,0,0,0],[0,s,0,0], [0,0,s,0], [0,0,0,1]]
      endif
    endif
    
    if (ev.type EQ 0) then begin ;  Button press.
        if ev.press gt 1 then begin ;Press with right or middle button
            picked = state.oWindow->select(state.oView, [ev.x, ev.y])
            if (size(picked))(0) eq 0 then begin ;Hit anything?
            			state.did_scaling=1
            			state.did_oldny=ev.Y
    				goto, drag_it & endif
            if obj_class(picked(0)) ne "IDLGRMODEL" then goto, done
            picked(0)->GetProperty, UVALUE=uval, TRANSFORM=tm
           ;str = string(uval, tm(3,0), tm(3,1), tm(3,2), format='(A, " at [", 3F6.2, "]")')
	    if state.l_mess gt 0 then WIDGET_CONTROL, state.l_mess,bad_id=ii,set_value=uval $
	    			 else print,uval
            d_livNewSelected, state, picked(0)->get()
            goto, draw_it
        endif else begin        ;Not rt or middle button
	    drag_it:
            state.btndown = 1
            state.oWindow->SetProperty, QUALITY=state.dragq
            WIDGET_CONTROL, state.wDraw, /DRAW_MOTION
        endelse
    endif else if ((ev.type eq 2) and (state.btndown eq 1)) then begin ;MOTION.
        if (bHaveTransform) then state.oWindow->Draw, state.oView
        if (bHaveScaling)   then state.oWindow->Draw, state.oView
    endif else if (ev.type eq 1) and state.btndown then begin ;Release
        state.btndown = 0
	state.did_scaling=0
        state.oWindow->SetProperty, QUALITY=2
        draw_it:
        WIDGET_CONTROL, ev.top, /HOURGLASS
        state.oWindow->Draw, state.oView
        WIDGET_CONTROL, state.wDraw, DRAW_MOTION=0
    endif
endelse

done: WIDGET_CONTROL, baset, SET_UVALUE=state, /NO_COPY
end


;**
;** **********
pro d_livEvent, ev             ;Main event handler


WIDGET_CONTROL, ev.id, GET_UVALUE=uval
WIDGET_CONTROL, ev.handler, GET_UVALUE=state, /NO_COPY

if uval eq 'RESET' then begin ;
    if ev.value eq 'Defaults' then begin ;Reset the defaults?
        state.shading = 1
        state.thick = 1
        state.edging = 0
        state.reject = 0
        state.bottomColor = 1
        state.style = 2
        d_livMenuSetState, '|Edging', state.edging, $
          Names=state.menuitems, Indices=state.menubuttons
        d_livMenuSetState, '|Bottom Color', state.bottomColor, $
          Names=state.menuitems, Indices=state.menubuttons
        d_livMenuSetState, '|Backface', state.reject, $
          Names=state.menuitems, Indices=state.menubuttons
    endif

reset_transform:
    state.scaling = 1.0
    state.oModelSurface->SetProperty, TRANSFORM=state.tmg0 ;And fall thru
    state.oModelEdges->SetProperty, TRANSFORM=state.tmg0 ;And fall thru
new_scale:
    s = state.scaling
    ;state.oModelSurface->SetProperty, $
    ;  TRANSFORM = [[s,0,0,0],[0,s,0,0], [0,0,s,0], [0,0,0,1]] ;New transform
    ;state.oModelEdges->SetProperty, $
    ;  TRANSFORM = [[s,0,0,0],[0,s,0,0], [0,0,s,0], [0,0,0,1]] ;New transform
    state.oModelTop->SetProperty, $
      TRANSFORM = [[s,0,0,0],[0,s,0,0], [0,0,s,0], [0,0,0,1]] ;New transform

redraw:
    d_livSetObjectAttributes, state ;Update attributes
    widget_control, ev.top, /HOURGLASS
    state.oWindow->Draw, state.oView

endif else if strpos(uval, 'Vrml') ge 0 then begin drole=state.wDraw
    WIDGET_CONTROL, ev.handler, SET_UVALUE=state
    d_livDrawPrint, drole ,'VRML'

endif else if strpos(uval, '|Shading') ge 0 then begin
    state.shading = d_livMapMenuChoice(uval, state.MenuItems, $
                       state.MenuButtons)
    goto, redraw

endif else if strpos(uval, '|Drag ') ge 0 then begin
    state.dragq = d_livMapMenuChoice(uval, state.MenuItems, state.MenuButtons)
    
endif else if strpos(uval, '|Vertex') ge 0 then begin
    state.vert_coloring = d_livMenuToggleState(ev.id)
    goto, redraw

endif else if strpos(uval, '|Line ') ge 0 then begin
    state.thick = ([1,3,5,7])(d_livMapMenuChoice(uval, state.MenuItems, $
                                              state.MenuButtons))
    goto, redraw

endif else if strpos(uval, '|Edging') ge 0 then begin
    state.edging = d_livMenuToggleState(ev.id)
    goto, redraw

endif else if strpos(uval, '|Backface') ge 0 then begin
    state.reject = d_livMenuToggleState(ev.id) ;New backface culling
    goto, redraw

endif else if strpos(uval, '|Bottom Color') ge 0 then begin
    state.bottomColor = d_livMenuToggleState(ev.id)    ;New backface culling
    goto, redraw

endif else if strpos(uval, '|Style') ge 0 then begin
    state.style = d_livMapMenuChoice(uval, state.MenuItems, state.MenuButtons)
    goto, redraw

endif else if uval eq 'OBJ' then begin ;Load a new object
    prev = state.oModelTop
    state.oView->remove, prev
    obj_destroy, state.oModelTop
    d_livLoadItem, ev.value, state
    obj_destroy, prev
    goto, reset_transform
endif

done : WIDGET_CONTROL, ev.handler, SET_UVALUE=state, /NO_COPY
end


;**
;** ************
pro d_livCleanup, wBase ,state

if n_elements(state) eq 0 then WIDGET_CONTROL, wBase, GET_UVALUE=state, /NO_COPY

OBJ_DESTROY, state.oView        ;  Destroy the top objects
OBJ_DESTROY, state.oTrack
OBJ_DESTROY, state.oModelTop

OBJ_DESTROY, state.oSurface
OBJ_DESTROY, state.oModelEdges
;OBJ_DESTROY, state.oModelOffset
;OBJ_DESTROY, state.oModelSticks
;OBJ_DESTROY, state.oModelImage
;OBJ_DESTROY, state.oModelSurface

end


;**
;** *********
PRO liv_objet, WDRAW=wDraw, DIM=dim, MENUBAS=menuBas ,ALTER=alter, L_MESS=l_mess, EE=ee,NN=nn,VV=vv,$
               MYTEM=mytem, WI =wi , PS=ps ,XX=xx,YY=yy,ZZ=zz,WW=ww, THRESH=thresh, REPR=repr

common c_liv_obj, allstates,allbases, vcc, Items, a_sphere, a_dens, a_name, a_symb ,a_col, a_arrow

IF n_elements(ps) eq 1 then $
IF ps gt ' ' then begin d_livDrawPrint, wDraw ,ps  & RETURN & ENDIF

if n_elements(mytem)  eq 0 then mytem =3
if n_elements(l_mess) eq 0 then l_mess=0
if n_elements(dim)    eq 0 then dim   =[512,350]
if n_elements(wi)     ne 1 then wi    =0
if n_elements(xx)     eq 0 then xx    =0
if n_elements(yy)     eq 0 then yy    =0
if n_elements(zz)     eq 0 then zz    =0
if n_elements(ee)     eq 0 then ee    =0
if n_elements(nn)     eq 0 then nn    =0
if n_elements(vv)     eq 0 then vv    =0
if n_elements(ww)     eq 0 then ww    =0
if n_elements(thresh) eq 0 then thresh=-99
if n_elements(repr)   eq 0 then repr  =0
if wi gt 0 then mytem=6
xdim =dim(0)
ydim =dim(1)

Items = [ ['Seashell', 'd_livMakeShape', 'seashell.dat'], $
          ['Knot'    , 'd_livMakeShape', 'knot.dat'],     $
          ['Teapot'  , 'd_livMakeShape', 'teapot.dat'],   $
          ['Valium Molecule'   , 'd_livMakeMolecule', 'valium.mol'],   $
          ['Aspartame Molecule', 'd_livMakeMolecule', 'aspartam.mol'], $
          ['Caffeine Molecule' , 'd_livMakeMolecule', 'caffeine.mol'], $
          ['Lamp Object'       , 'd_livMakeLampWks' ,  string(wi) ]]

wBase=menuBas

MenuItems = ['1Shading', '0Flat', '6Gouraud', $
             '1Style', '0Points', '0Wire', '4Solid', '0Hidden Points', '2Hidden Wire', $
             '1Coloring', $
             '0Backface Culling (Off)', $
             '0Vertex Coloring (On )', $
             '0Edging (Off)', $
             '2Bottom Color (On )', $
             '1Line Thickness', '41', '03', '05','27', $
             '3Drag Quality', '0Low','4Medium','2High']

WIDGET_CONTROL, wDraw  , GET_VALUE =oWindow ; Grab the window id of the drawable.
WIDGET_CONTROL, wDraw  , SET_UVALUE={baset:menuBas,targets:obj_new(),styles:[0]}
WIDGET_CONTROL, wDraw  , /HOURGLASS
WIDGET_CONTROL, menuBas, SENSITIVE =1
WIDGET_CONTROL, menuBas, GET_UVALUE=state
newobj=0
IF   n_tags(state) gt 20 THEN BEGIN  d_livCleanup, menuBas     & newobj=1
     MenuButtons=state.MenuButtons     & MenuItems=state.MenuItems
     dragq=state.dragq                 & shading=state.shading
     vert_coloring=state.vert_coloring & thick=state.thick
     edging=state.edging               & reject=state.reject
     bottomColor=state.bottomColor     & style=state.style
     
ENDIF else begin d_livMenuCreate , MenuItems, MenuButtons, menuBas
     dragq=1  & shading=1 & vert_coloring=1 & thick=1
     edging=0 & reject=0  & bottomColor=1   & style=2
ENDELSE

aspect = float(xdim)/float(ydim) ; viewplane rect based on aspect ratio.
myview = [-0.5,-0.5,1,1]
if (aspect > 1) then begin
    myview(0) = myview(0) - ((aspect-1.0)*myview(2))/2.0
    myview(2) = myview(2) * aspect
endif else begin
    myview(1) = myview(1) - (((1.0/aspect)-1.0)*myview(3))/2.0
    myview(3) = myview(3) * aspect
endelse

oView = OBJ_NEW('idlgrview', PROJECTION=1, EYE=3, ZCLIP=[1.5,-1.5],$
                 VIEWPLANE_RECT=myview, COLOR=[110,110,110])

oTrack= OBJ_NEW('Trackball', [xdim/2.0, ydim/2.0], xdim/2.0)

tvlct,rr,gg,bb,/get & vcc = transpose([[rr], [gg],[bb]])

state = $
  { wBase: wBase, $             ; Main base
    wDraw: wDraw, $             ; Widget draw ID
    l_mess: l_mess, $		; base label for message
    currentItem : mytem, $      ; Index of current item
    oView : oView, $            ; the view
    oModelTop: obj_new(), $
    oModelSurface: obj_new(), $
    oModelEdges: obj_new(), $
    oModelOffset: obj_new(), $
    oModelSticks: obj_new(), $
    oSticks: obj_new(), $
    oModelImage: obj_new(), $
    oImage: obj_new(), $
    oTrack: oTrack, $
    oWindow : oWindow, $
    oSurface : obj_new(), $     ; Visible surface
    tmg0: fltarr(4,4), $        ; Initial transform
    dragq: dragq, $             ; Drag quality
    btndown: 0, $               ; Button down flag
    MenuItems: MenuItems, $
    MenuButtons: MenuButtons, $
    cur_sel: obj_new(), $
    scaling: 1.0, $
    shading: shading, $
    vert_coloring: vert_coloring, $
    thick: thick, $
    edging: edging, $
    reject: reject, $
    bottomColor: bottomColor, $
    style: style, $
    did_wi:wi     , $
    did_scaling: 0, $
    did_oldny:   0, $
    did_xx: xx    , $
    did_yy: yy    , $
    did_zz: zz    , $
    did_ee: ee    , $
    did_nn: nn    , $
    did_vv: vv    , $
    did_ww: ww    , $
    did_thresh: thresh , $
    did_repr:   repr   , $
    did_scalval: 110 }

WIDGET_CONTROL, wBase, SET_UVALUE=  state
WIDGET_CONTROL, wDraw, EVENT_PRO = 'd_livDrawEvent'
WIDGET_CONTROL, wDraw, map=1
if n_elements(alter) eq 1 then if alter gt 0 then WIDGET_CONTROL,alter, bad_id=ii, map=0

d_livLoadItem , mytem, state
state.oWindow->Draw, state.oView

idx=-1
IF n_elements(allbases) gt 0 then begin			;Clear old objects
   FOR i=0,n_elements(allbases)-1 do begin
	IF   allbases(i) eq 0 then idx=i $
	ELSE BEGIN
	     WIDGET_CONTROL, allbases(i),bad_id=ii, GET_UVALUE=curstate
	     IF (ii gt 0) or (n_tags(curstate) lt 20) or (allbases(i) eq wBase) then begin
		bb={oView:allstates(0,i),oTrack:allstates(1,i),oModelTop:allstates(2,i),$
		    oSurface:allstates(3,i),oModelEdges:allstates(4,i)}
		idx=i & allbases(i)=0L & d_livCleanup, 0,bb & ENDIF
	ENDELSE
   ENDFOR
ENDIF

BB=[state.oView,state.oTrack,state.oModelTop,state.oSurface,state.oModelEdges] ;Keep object in mind
IF n_elements(allstates) eq 0 then begin allstates=BB & allbases=[wBase]
ENDIF ELSE	  IF idx ge 0 then begin allstates(*,idx)=BB & allbases(idx)=wBase
		  ENDIF       else begin allstates=[[allstates],[BB]] & allbases=[allbases,wBase]
ENDELSE

WIDGET_CONTROL, wBase, SET_UVALUE=state, /NO_COPY
WIDGET_CONTROL, wBase, EVENT_PRO = 'd_livEvent'

end
