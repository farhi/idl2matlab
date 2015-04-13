; $Id: mesh_obj.pro,v 1.1 1994/04/05 17:22:40 dan Exp $
;
; Copyright (c) 1994, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       MESH_OBJ
;
; PURPOSE:
;       MESH_OBJ generates a polygon mesh (vertex list and polygon list) that
;       represent the desired primitive object. The available primitive
;       objects are: triangulated surface, rectangular surface, polar surface,
;       cylindrical surface, spherical surface, surface of extrusion, surface
;       of revolution, and ruled surface.
;
; CATEGORY:
;       Graphics.
;
; CALLING SEQUENCE:
;       MESH_OBJ, Type, Vertex_List, Polygon_List, Array1, Array2
;
; INPUTS:
;
;       Type:   An integer which specifies what type of object to create.
;
;                  TYPE CODE:      SURFACE TYPE:
;                  ----------      -------------
;
;                  0               TRIANGULATED
;                  1               RECTANGULAR
;                  2               POLAR
;                  3               CYLINDRICAL
;                  4               SPHERICAL
;                  5               EXTRUSION
;                  6               REVOLUTION
;                  7               RULED
;
;                  ELSE            none
;
;       Vertex_List:
;               On input, Vertex_List may be undefined. On output, it contains
;               the mesh vertices. Vertex_List and Polygon_List have the same
;               format as the lists returned by the SHADE_VOLUME procedure.
;
;       Polygon_List:
;               On input, Polygon_List may be undefined. On output, it contains
;               the mesh indexes.
;
;       Array1:
;               An array whose use depends on the type of object being created.
;
;                  SURFACE TYPE:   Array1 type
;                  -------------   --------------------------------------------
;
;                  TRIANGULATED    A (3, n) array containing random [x, y, z]
;                                  points to build a triangulated surface from.
;                                  The resulting polygon mesh will have n
;                                  vertices. When shading a triangulated mesh,
;                                  the shading array should have (n) elements.
;
;                  RECTANGULAR     A two dimensional (n, m) array containing
;                                  z values. The resulting polygon mesh will
;                                  have n*m vertices.
;                                  When shading a rectangular mesh, the shading
;                                  array should have (n, m) elements.
;
;                  POLAR           A two dimensional (n, m) array containing
;                                  z values. The resulting polygon mesh will
;                                  have n*m vertices. The n dimension of the
;                                  array is mapped to the polar angle, and the
;                                  m dimension is mapped to the polar radius.
;                                  When shading a polar mesh, the shading array
;                                  should have (n, m) elements.
;
;                  CYLINDRICAL     A two dimensional (n, m) array containing
;                                  radius values. The resulting polygon mesh
;                                  will have n*m vertices. The n dimension of
;                                  the array is mapped to the polar angle,
;                                  and the m dimension is mapped to the Z axis.
;                                  When shading a cylindrical mesh, the shading
;                                  array should have (n, m) elements.
;
;                  SPHERICAL       A two dimensional (n, m) array containing
;                                  radius values. The resulting polygon mesh
;                                  will have n*m vertices. The n dimension of
;                                  the array is mapped to the longitude (0.0 to
;                                  360.0 degrees), and the m dimension is
;                                  mapped to the latitude (-90.0 to +90.0
;                                  degrees).
;                                  When shading a spherical mesh, the shading
;                                  array should have (n, m) elements.
;
;                  EXTRUSION       A (3, n) array of connected 3-D points which
;                                  define the shape to extrude. The resulting
;                                  polygon mesh will have n*(steps+1) vertices
;                                  (where steps is the number of "segments" in
;                                  the extrusion). (See the P1 keyword).
;                                  If the order of the elements in Array1 is
;                                  reversed, then the polygon facing is
;                                  reversed.
;                                  When shading an extrusion mesh, the shading
;                                  array should have (n, steps+1) elements.
;
;                  REVOLUTION      A (3, n) array of connected 3-D points which
;                                  define the shape to revolve. The resulting
;                                  polygon mesh will have n*((steps>3)+1)
;                                  vertices (where steps is the number of
;                                  "steps" in the revolution). (See the P1
;                                  keyword). If the order of the elements in
;                                  Array1 is reversed, then the polygon facing
;                                  is reversed.
;                                  When shading a revolution mesh, the shading
;                                  array should have (n, (steps>3)+1) elements.
;
;                  RULED           A (3, n) array of connected 3-D points which
;                                  define the shape of the first ruled vector.
;                                  The optional (3, m) Array2 parameter defines
;                                  the shape of the second ruled vector. The
;                                  resulting polygon mesh will have
;                                  (n>m)*(steps+1) vertices (where steps is the
;                                  number of intermediate "steps"). (See the P1
;                                  keyword).
;                                  When shading a ruled mesh, the shading
;                                  array should have (n>m, steps+1) elements.
;
; OPTIONAL INPUTS:
;
;       Array2:
;               If the object type is 7 (Ruled Surface) then Array2 is a (3, m)
;               array containing the 3-D points which define the second ruled
;               vector. If Array2 has fewer elements than Array1 then Array2 is
;               processed with CONGRID to give it the same number of elements
;               as Array1. If Array1 has fewer elements than Array2 then Array1
;               is processed with CONGRID to give it the same number of
;               elements as Array2.
;               Array2 MUST be supplied if the object type is 7. Otherwise,
;               Array2 is ignored.
;
; KEYWORD PARAMETERS:
;
;       P1 - P5:
;               The meaning of the keywords P1 through P5 vary depending upon
;               the object type.
;
;                  SURFACE TYPE:   Keywords
;                  -------------   --------------------------------------------
;
;                  TRIANGULATED    P1, P2, P3, P4, and P5 are ignored.
;
;                  RECTANGULAR     If Array1 is an (n, m) array, and if P1 has
;                                  n elements, then the values contained in
;                                  P1 are the X coordinates for each column of
;                                  vertices. Otherwise, FINDGEN(n) is used for
;                                  the X coordinates. If P2 has m elements,
;                                  then the values contained in P2 are the Y
;                                  coordinates for each row of vertices.
;                                  Otherwise, FINDGEN(m) is used for the Y
;                                  coordinates. The polygon facing is reversed
;                                  if the order of either P1 or P2 (but not
;                                  both) is reversed.
;                                  P3, P4, and P5 are ignored.
;
;                  POLAR           P1 specifies the polar angle of the first
;                                  column of Array1 (the default is 0). P2
;                                  specifies the polar angle of the last column
;                                  of Array1 (the default is 2*PI). If P2 is
;                                  less than P1 then the polygon facing is
;                                  reversed. P3 specifies the radius of the
;                                  first row of Array1 (the default is 0). P4
;                                  specifies the radius of the last row of
;                                  Array1 (the default is m-1). If P4 is less
;                                  than P3 then the polygon facing is reversed.
;                                  P5 is ignored.
;
;                  CYLINDRICAL     P1 specifies the polar angle of the first
;                                  column of Array1 (the default is 0). P2
;                                  specifies the polar angle of the last column
;                                  of Array1 (the default is 2*PI). If P2 is
;                                  less than P1 then the polygon facing is
;                                  reversed. P3 specifies the Z coordinate of
;                                  the first row of Array1 (the default is 0).
;                                  P4 specifies the Z coordinate of the last
;                                  row of Array1 (the default is m-1). If P4 is
;                                  less than P3 then the polygon facing is
;                                  reversed. P5 is ignored.
;
;                  SPHERICAL       P1 specifies the longitude of the first
;                                  column of Array1 (the default is 0). P2
;                                  specifies the longitude of the last column
;                                  of Array1 (the default is 2*PI). IF P2 is
;                                  less than P1 then the polygon facing is
;                                  reversed. P3 specifies the latitude of the
;                                  first row of Array1 (the default is -PI/2).
;                                  P4 specifies the latitude of the last row of
;                                  Array1 (the default is +PI/2). If P4 is less
;                                  than P3 then the polygon facing is reversed.
;                                  P5 is ignored.
;
;                  EXTRUSION       P1 specifies the number of steps in the
;                                  extrusion (the default is 1). P2 is a three
;                                  element vector specifying the direction
;                                  (and length) of the extrusion (the default
;                                  is [0, 0, 1]). P3, P4, and P5 are ignored.
;
;                  REVOLUTION      P1 specifies the number of "facets" in the
;                                  revolution (the default is 3). If P1 is less
;                                  than 3 then 3 is used. P2 is a three element
;                                  vector specifying a point that the rotation
;                                  vector passes through (the default is
;                                  [0, 0, 0]). P3 is a three element vector
;                                  specifying the direction of the rotation
;                                  vector (the default is [0, 0, 1]). P4
;                                  specifies the starting angle for the
;                                  revolution (the default is 0). P5 specifies
;                                  the ending angle for the revolution (the
;                                  default is 2*PI). If P5 is less than P4 then
;                                  the polygon facing is reversed.
;
;                  RULED           P1 specifies the number of "steps" in the
;                                  ruling (the default is 1).
;                                  P2, P3, P4, and P5 are ignored.
;
;       DEGREES:   If set, then the input parameters are in degrees
;                  (where applicable). Otherwise, the angles are in
;                  radians.
;
; EXAMPLE:
;
;       ; Create a 48x64 cylinder with a constant radius of 0.25.
;       MESH_OBJ, 3, Vertex_List, Polygon_List, Replicate(0.25, 48, 64), $
;          P4=0.5
;
;       ; Transform the vertices.
;       T3d, /Reset
;       T3d, Rotate=[0.0, 30.0, 0.0]
;       T3d, Rotate=[0.0, 0.0, 40.0]
;       T3d, Translate=[0.25, 0.25, 0.25]
;       Vertex_List = Vert_T3d(Vertex_List)
;
;       ; Create the window and view.
;       Window, 0, Xsize=512, Ysize=512
;       Create_View, Winx=512, Winy=512
;
;       ; Render the mesh.
;       Set_Shading, Light=[-0.5, 0.5, 2.0], Reject=0
;       Tvscl, Polyshade(Vertex_List, Polygon_List, /Normal)
;
;
;       ; Create a cone (surface of revolution).
;       MESH_OBJ, 6, Vertex_List, Polygon_List, $
;          [[0.75, 0.0, 0.25], [0.5, 0.0, 0.75]], P1=16, P2=[0.5, 0.0, 0.0]
;
;       ; Create the window and view.
;       Window, 0, Xsize=512, Ysize=512
;       Create_View, Winx=512, Winy=512, Ax=30.0, Ay=(140.0), Zoom=0.5
;
;       ; Render the mesh.
;       Set_Shading, Light=[-0.5, 0.5, 2.0], Reject=0
;       Tvscl, Polyshade(Vertex_List, Polygon_List, /Data, /T3d)
;
; MODIFICATION HISTORY:
;       Written by:     Daniel Carr, Thu Mar 31 19:16:43 MST 1994
;-

PRO MESH_OBJ, obj_type, vertex_list, polygon_list, array1, array2, $
              P1=p1, P2=p2, P3=p3, P4=p4, P5=p5, $
              Degrees=degrees

CASE obj_type OF

   0: BEGIN ; Triangulated surface.
      sz_array = Size(array1)
      IF (sz_array(1) GE 3L) THEN vertex_list = array1 $
      ELSE vertex_list = [array1, Replicate(0.0, 1, sz_array(2))]
      
      Triangulate, vertex_list(0, *), vertex_list(1, *), tri
      polygon_list = [Replicate(3L, 1L, (N_Elements(tri) / 3L)), Temporary(tri)]
      polygon_list = polygon_list(*)

      RETURN
   END

   1: BEGIN ; Rectangular surface.
      sz_array = Size(array1)
      dim_x = sz_array(1)
      dim_y = sz_array(2)
      vert_num = dim_x * dim_y
      indx_num = (dim_x - 1L) * (dim_y - 1L)
      poly_num = 5L * indx_num

      vertex_list = Fltarr(3, vert_num, /Nozero)

      IF (N_Elements(p1) EQ dim_x) THEN $
         vertex_list(0, *) = p1 # Replicate(1.0, dim_y) $
      ELSE vertex_list(0, *) = Findgen(dim_x) # Replicate(1.0, dim_y)

      IF (N_Elements(p2) EQ dim_y) THEN $
         vertex_list(1, *) = Replicate(1.0, dim_x) # p2 $
      ELSE vertex_list(1, *) = Replicate(1.0, dim_x) # Findgen(dim_y)

      vertex_list(2, *) = Float(array1)

      polygon_list = Lonarr(poly_num, /Nozero)

      p_ind = 5L * Lindgen(indx_num)
      y_inc = Replicate(1L, (dim_x - 1L)) # Lindgen(dim_y - 1L)

      polygon_list(p_ind) = 4L
      p_ind = p_ind + 1L
      polygon_list(p_ind) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 1L) = polygon_list(p_ind) + 1L
      p_ind = p_ind + 1L
      polygon_list(p_ind + 1L) = polygon_list(p_ind) + dim_x
      p_ind = p_ind + 1L
      polygon_list(p_ind + 1L) = polygon_list(p_ind) - 1L
      p_ind = 0

      RETURN
   END

   2: BEGIN ; Polar surface.
      sz_array = Size(array1)
      dim_x = sz_array(1)
      dim_y = sz_array(2)
      vert_num = dim_x * dim_y

      vertex_list = Fltarr(3, vert_num, /Nozero)

      min_ang = 0.0
      max_ang = 2.0 * !PI
      min_rad = 0.0
      max_rad = Float(dim_y - 1L)
      IF (N_Elements(p1) GT 0L) THEN BEGIN
         min_ang = Float(p1)
         IF (Keyword_Set(degrees)) THEN min_ang = min_ang * !Dtor
      ENDIF
      IF (N_Elements(p2) GT 0L) THEN BEGIN
         max_ang = Float(p2)
         IF (Keyword_Set(degrees)) THEN max_ang = max_ang * !Dtor
      ENDIF
      IF (N_Elements(p3) GT 0L) THEN min_rad = Float(p3)
      IF (N_Elements(p4) GT 0L) THEN max_rad = Float(p4)

      min_rad = min_rad > 1.0E-4
      max_rad = max_rad > 1.0E-4
      IF (min_ang EQ max_ang) THEN max_ang = max_ang + (2.0 * !PI)

      vertex_list(2, *) = Float(array1)
      vertex_list(1, *) = Replicate(1.0, dim_x) # $
         (min_rad + ((max_rad - min_rad) * Findgen(dim_y) / Float(dim_y - 1L)))
      vertex_list(0, *) = $
         (min_ang + ((max_ang - min_ang) * Findgen(dim_x) / $
         Float(dim_x - 1L))) # Replicate(1.0, dim_y)

      indx_num = (dim_x - 1L) * (dim_y - 1L)
      poly_num = 5L * indx_num

      polygon_list = Lonarr(poly_num, /Nozero)
      y_inc = Replicate(1L, (dim_x - 1L)) # Lindgen(dim_y - 1L)

      p_ind = 5L * Lindgen(indx_num)

      polygon_list(p_ind) = 4L
      polygon_list(p_ind + 1L) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 2L) = polygon_list(p_ind + 1L) + dim_x
      polygon_list(p_ind + 3L) = polygon_list(p_ind + 2L) + 1L
      polygon_list(p_ind + 4L) = polygon_list(p_ind + 3L) - dim_x
      p_ind = 0

      vertex_list = CV_COORD(From_Cylin=vertex_list, /To_Rect)
      RETURN
   END

   3: BEGIN ; Cylindrical surface.
      sz_array = Size(array1)
      dim_x = sz_array(1)
      dim_y = sz_array(2)
      vert_num = dim_x * dim_y

      vertex_list = Fltarr(3, vert_num, /Nozero)

      min_ang = 0.0
      max_ang = 2.0 * !PI
      min_len = 0.0
      max_len = Float(dim_y - 1L)
      IF (N_Elements(p1) GT 0L) THEN BEGIN
         min_ang = Float(p1)
         IF (Keyword_Set(degrees)) THEN min_ang = min_ang * !Dtor
      ENDIF
      IF (N_Elements(p2) GT 0L) THEN BEGIN
         max_ang = Float(p2)
         IF (Keyword_Set(degrees)) THEN max_ang = max_ang * !Dtor
      ENDIF
      IF (N_Elements(p3) GT 0L) THEN min_len = Float(p3)
      IF (N_Elements(p4) GT 0L) THEN max_len = Float(p4)

      IF (min_ang EQ max_ang) THEN max_ang = max_ang + (2.0 * !PI)

      vertex_list(1, *) = Float(array1)
      vertex_list(2, *) = Replicate(1.0, dim_x) # $
         (min_len + ((max_len - min_len) * Findgen(dim_y) / Float(dim_y - 1L)))
      vertex_list(0, *) = $
         (min_ang + ((max_ang - min_ang) * Findgen(dim_x) / $
         Float(dim_x - 1L))) # Replicate(1.0, dim_y)

      indx_num = (dim_x - 1L) * (dim_y - 1L)
      poly_num = 5L * indx_num

      polygon_list = Lonarr(poly_num, /Nozero)
      y_inc = Replicate(1L, (dim_x - 1L)) # Lindgen(dim_y - 1L)

      p_ind = 5L * Lindgen(indx_num)

      polygon_list(p_ind) = 4L
      polygon_list(p_ind + 1L) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 2L) = polygon_list(p_ind + 1L) + 1L
      polygon_list(p_ind + 3L) = polygon_list(p_ind + 2L) + dim_x
      polygon_list(p_ind + 4L) = polygon_list(p_ind + 3L) - 1L
      p_ind = 0

      vertex_list = CV_COORD(From_Cylin=vertex_list, /To_Rect)
      RETURN
   END

   4: BEGIN ; Spherical surface.
      sz_array = Size(array1)
      dim_x = sz_array(1)
      dim_y = sz_array(2)
      vert_num = dim_x * dim_y

      vertex_list = Fltarr(3, vert_num, /Nozero)

      min_lon = 0.0
      max_lon = 2.0 * !PI
      min_lat = (-!PI) / 2.0
      max_lat = (+!PI) / 2.0
      IF (N_Elements(p1) GT 0L) THEN BEGIN
         min_lon = Float(p1)
         IF (Keyword_Set(degrees)) THEN min_lon = min_lon * !Dtor
      ENDIF
      IF (N_Elements(p2) GT 0L) THEN BEGIN
         max_lon = Float(p2)
         IF (Keyword_Set(degrees)) THEN max_lon = max_lon * !Dtor
      ENDIF
      IF (N_Elements(p3) GT 0L) THEN BEGIN
         min_lat = Float(p3)
         IF (Keyword_Set(degrees)) THEN min_lat = min_lat * !Dtor
      ENDIF
      IF (N_Elements(p4) GT 0L) THEN BEGIN
         max_lat = Float(p4)
         IF (Keyword_Set(degrees)) THEN max_lat = max_lat * !Dtor
      ENDIF

      min_lat = (min_lat > (((-!PI) / 2.0) + 1.0E-4)) < $
                           (((+!PI) / 2.0) - 1.0E-4)
      max_lat = (max_lat > (((-!PI) / 2.0) + 1.0E-4)) < $
                           (((+!PI) / 2.0) - 1.0E-4)
      IF (min_lon EQ max_lon) THEN max_lon = max_lon + (2.0 * !PI)

      vertex_list(2, *) = Float(array1)
      vertex_list(1, *) = Replicate(1.0, dim_x) # $
         (min_lat + ((max_lat - min_lat) * Findgen(dim_y) / Float(dim_y - 1L)))
      vertex_list(0, *) = $
         (min_lon + ((max_lon - min_lon) * Findgen(dim_x) / $
         Float(dim_x - 1L))) # Replicate(1.0, dim_y)

      indx_num = (dim_x - 1L) * (dim_y - 1L)
      poly_num = 5L * indx_num

      polygon_list = Lonarr(poly_num, /Nozero)
      y_inc = Replicate(1L, (dim_x - 1L)) # Lindgen(dim_y - 1L)

      p_ind = 5L * Lindgen(indx_num)

      polygon_list(p_ind) = 4L
      polygon_list(p_ind + 1L) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 2L) = polygon_list(p_ind + 1L) + 1L
      polygon_list(p_ind + 3L) = polygon_list(p_ind + 2L) + dim_x
      polygon_list(p_ind + 4L) = polygon_list(p_ind + 3L) - 1L
      p_ind = 0

      vertex_list = CV_COORD(From_Sphere=vertex_list, /To_Rect)
      RETURN
   END

   5: BEGIN ; Surface of extrusion.
      sz_array = Size(array1)
      max_ind = sz_array(2) - 1L
      steps = 1L
      IF (N_Elements(p1) GT 0L) THEN steps = Long(p1) > 1L

      vert_num = sz_array(2) * (steps + 1L)

      indx_num = max_ind * steps
      poly_num = 5L * indx_num

      polygon_list = Lonarr(poly_num, /Nozero)
      y_inc = Replicate(1L, max_ind) # Lindgen(steps)

      p_ind = 5L * Lindgen(indx_num)

      polygon_list(p_ind) = 4L
      polygon_list(p_ind + 1L) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 2L) = polygon_list(p_ind + 1L) + 1L
      polygon_list(p_ind + 3L) = polygon_list(p_ind + 2L) + sz_array(2)
      polygon_list(p_ind + 4L) = polygon_list(p_ind + 3L) - 1L
      p_ind = 0

      ex_vec = [0.0, 0.0, 1.0]
      IF (N_Elements(p2) GT 0L) THEN ex_vec = Float(p2)
      len = Sqrt(ex_vec(0)^2 + ex_vec(1)^2 + ex_vec(2)^2)
      IF (len EQ 0.0) THEN BEGIN
         ex_vec = [0.0, 0.0, 1.0]
         len = 1.0
      ENDIF
      len = Sqrt(ex_vec(0)^2 + ex_vec(1)^2 + ex_vec(2)^2)
      ex_vec = ex_vec / len

      z_ang = 0.0
      y_len = Sqrt(ex_vec(0)^2 + ex_vec(1)^2)
      IF (y_len GT 0.0) THEN $
         z_ang = Atan(ex_vec(1), ex_vec(0)) * !Radeg
      y_ang = 0.0
      IF ((ex_vec(2) NE 0.0) OR (y_len GT 0.0)) THEN $
         y_ang = Atan(ex_vec(2), y_len) * !Radeg

      save_pt = !P.T
      T3d, /Reset
      T3d, Rotate=[0.0, 0.0, (-z_ang)]
      T3d, Rotate=[0.0, (y_ang), 0.0]
      T3d, Rotate=[0.0, (-90.0), 0.0]

      IF (sz_array(1) EQ 2L) THEN $
         perim = Vert_T3d([array1(0, *), array1(1, *), $
                          Replicate(0.0, 1L, sz_array(2))]) $
      ELSE perim = Vert_T3d(array1)

      vertex_list = Fltarr(3, vert_num, /Nozero)
      vertex_list(0, *) = Reform(perim(0, *)) # Replicate(1.0, (steps + 1L))
      vertex_list(1, *) = Reform(perim(1, *)) # Replicate(1.0, (steps + 1L))
      vertex_list(2, *) = $
         (Reform(perim(2, *)) # Replicate(1.0, (steps + 1L))) + $
         Replicate(1.0, sz_array(2)) # $
         (len * Findgen(steps + 1L) / Float(steps))

      T3d, /Reset
      T3d, Rotate=[0.0, (90.0), 0.0]
      T3d, Rotate=[0.0, (-y_ang), 0.0]
      T3d, Rotate=[0.0, 0.0, (z_ang)]

      vertex_list = Vert_T3d(vertex_list, /No_Copy)
      !P.T = save_pt

      RETURN
   END

   6: BEGIN ; Surface of revolution.
      sz_array = Size(array1)
      max_ind = sz_array(2) - 1L
      steps = 3L
      IF (N_Elements(p1) GT 0L) THEN steps = p1 > 3L

      vert_num = sz_array(2) * (steps + 1L)

      indx_num = max_ind * steps
      poly_num = 5L * indx_num

      polygon_list = Lonarr(poly_num, /Nozero)
      y_inc = Replicate(1L, max_ind) # Lindgen(steps)

      p_ind = 5L * Lindgen(indx_num)

      polygon_list(p_ind) = 4L
      polygon_list(p_ind + 1L) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 2L) = polygon_list(p_ind + 1L) + sz_array(2)
      polygon_list(p_ind + 3L) = polygon_list(p_ind + 2L) + 1L
      polygon_list(p_ind + 4L) = polygon_list(p_ind + 3L) - sz_array(2)
      p_ind = 0

      min_ang = 0.0
      max_ang = 2.0 * !PI
      IF (N_Elements(p4) GT 0L) THEN BEGIN
         min_ang = Float(p4)
         IF (Keyword_Set(degrees)) THEN min_ang = min_ang * !Dtor
      ENDIF
      IF (N_Elements(p5) GT 0L) THEN BEGIN
         max_ang = Float(p5)
         IF (Keyword_Set(degrees)) THEN max_ang = max_ang * !Dtor
      ENDIF

      IF (min_ang EQ max_ang) THEN max_ang = max_ang + (2.0 * !PI)

      rot_point = [0.0, 0.0, 0.0]
      IF (N_Elements(p2) GT 0L) THEN rot_point = Float(p2)
      rot_vec = [0.0, 0.0, 1.0]
      IF (N_Elements(p3) GT 0L) THEN rot_vec = Float(p3)
      len = Sqrt(rot_vec(0)^2 + rot_vec(1)^2 + rot_vec(2)^2)
      IF (len EQ 0.0) THEN BEGIN
         rot_vec = [0.0, 0.0, 1.0]
         len = 1.0
      ENDIF
      len = Sqrt(rot_vec(0)^2 + rot_vec(1)^2 + rot_vec(2)^2)
      rot_vec = rot_vec / len

      z_ang = 0.0
      y_len = Sqrt(rot_vec(0)^2 + rot_vec(1)^2)
      IF (y_len GT 0.0) THEN $
         z_ang = Atan(rot_vec(1), rot_vec(0)) * !Radeg
      y_ang = 0.0
      IF ((rot_vec(2) NE 0.0) OR (y_len GT 0.0)) THEN $
         y_ang = Atan(rot_vec(2), y_len) * !Radeg

      save_pt = !P.T
      T3d, /Reset
      T3d, Translate=[(-rot_point(0)), (-rot_point(1)), (-rot_point(2))]
      T3d, Rotate=[0.0, 0.0, (-z_ang)]
      T3d, Rotate=[0.0, (y_ang - 90.0), 0.0]

      IF (sz_array(1) EQ 2L) THEN $
         sweep = Vert_T3d([array1(0, *), array1(1, *), $
                          Replicate(0.0, 1L, sz_array(2))]) $
      ELSE sweep = Vert_T3d(array1)

      neg_ind = Where(sweep(0, *) LT 0.0)
      sweep(0, *) = Abs(sweep(0, *)) > 1.0E-4
      IF (neg_ind(0) GE 0L) THEN sweep(0, neg_ind) = sweep(0, neg_ind) * (-1.0)
      neg_ind = 0

      vertex_list = Fltarr(3, vert_num, /Nozero)
      vertex_list(0, *) = Reform(sweep(0, *)) # Replicate(1.0, (steps + 1L))
      vertex_list(1, *) = Reform(sweep(1, *)) # Replicate(1.0, (steps + 1L))
      vertex_list(2, *) = Reform(sweep(2, *)) # Replicate(1.0, (steps + 1L))

      perim_ind = Lindgen(sz_array(2))

      FOR i=0L, steps DO BEGIN
         T3d, /Reset
         T3d, Rotate=[0.0, 0.0, (!Radeg * $
            ((Float(i) * (max_ang - min_ang) / Float(steps)) + min_ang))]
         vertex_list(*, perim_ind) = Vert_T3d(vertex_list(*, perim_ind))

         perim_ind = perim_ind + sz_array(2)
      ENDFOR
      perim_ind = 0

      T3d, /Reset
      T3d, Rotate=[0.0, (90.0 - y_ang), 0.0]
      T3d, Rotate=[0.0, 0.0, (z_ang)]
      T3d, Translate=[rot_point(0), rot_point(1), rot_point(2)]

      vertex_list = Vert_T3d(vertex_list, /No_Copy)
      !P.T = save_pt

      RETURN
   END

   7: BEGIN ; Ruled surface.
      sz_array = Size(array1)
      sz_array2 = Size(array2)
      IF (sz_array(1) EQ 2L) THEN $
         start_rule = [array1(0, *), array1(1, *), $
                       Replicate(0.0, 1L, sz_array(2))] $
      ELSE start_rule = array1
      IF (sz_array2(1) EQ 2L) THEN $
         end_rule = [array2(0, *), array2(1, *), $
                     Replicate(0.0, 1L, sz_array2(2))] $
      ELSE end_rule = array2

      IF (sz_array(2) GT sz_array2(2)) THEN $
         end_rule = Congrid(end_rule, 3, sz_array(2), /Interp, /Minus_One)

      IF (sz_array(2) LT sz_array2(2)) THEN BEGIN
         start_rule = Congrid(start_rule, 3, sz_array2(2), /Interp, /Minus_One)
         sz_array = Size(start_rule)
      ENDIF

      steps = 1L
      IF (N_Elements(p1) GT 0L) THEN steps = Long(p1) > 1L

      dim_x = sz_array(2)
      dim_y = steps + 1L

      vert_num = dim_x * dim_y
      indx_num = (dim_x - 1L) * (dim_y - 1L)
      poly_num = 5L * indx_num

      vertex_list = Fltarr(3, vert_num, /Nozero)

      strip_ind = Lindgen(dim_x)
      FOR i=0L, steps DO BEGIN
         fac = Float(i) / Float(steps)
         vertex_list(0, strip_ind) = $
            ((1.0 - fac) * start_rule(0, *)) + (fac * end_rule(0, *))
         vertex_list(1, strip_ind) = $
            ((1.0 - fac) * start_rule(1, *)) + (fac * end_rule(1, *))
         vertex_list(2, strip_ind) = $
            ((1.0 - fac) * start_rule(2, *)) + (fac * end_rule(2, *))

         strip_ind = strip_ind + dim_x
      ENDFOR

      polygon_list = Lonarr(poly_num, /Nozero)

      p_ind = 5L * Lindgen(indx_num)
      y_inc = Replicate(1L, (dim_x - 1L)) # Lindgen(dim_y - 1L)

      polygon_list(p_ind) = 4L
      p_ind = p_ind + 1L
      polygon_list(p_ind) = Lindgen(indx_num) + y_inc(*)
      y_inc = 0
      polygon_list(p_ind + 1L) = polygon_list(p_ind) + dim_x
      p_ind = p_ind + 1L
      polygon_list(p_ind + 1L) = polygon_list(p_ind) + 1L
      p_ind = p_ind + 1L
      polygon_list(p_ind + 1L) = polygon_list(p_ind) - dim_x
      p_ind = 0

      RETURN
   END

   ELSE:
ENDCASE

RETURN
END
