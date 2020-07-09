
open MyUtil
open LengthInterface


type color =
  | DeviceGray of float
  | DeviceRGB  of float * float * float
  | DeviceCMYK of float * float * float * float
[@@deriving show { with_path = false }]

type 'a path_element =
  | LineTo              of 'a
  | CubicBezierTo       of point * point * 'a

type path =
  | GeneralPath of point * (point path_element) list * (unit path_element) option
  | Rectangle   of point * point


let (+@%) (x, y) (vx, vy) =
  (x +% vx, y +% vy)

let (-@%) (x, y) (vx, vy) =
  (x -% vx, y -% vy)

let rotate (x, y) t =
  (x *% cos t -% y *% sin t,
   x *% sin t +% y *% cos t)

let rotate_around pt c t =
  (rotate (pt -@% c) t) +@% c


let shift_path_element v pe =
  match pe with
  | LineTo(pt)                  -> LineTo(pt +@% v)
  | CubicBezierTo(pt1, pt2, pt) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, pt +@% v)


let shift_path v path =
  match path with
  | Rectangle(pt1, pt2) ->
      Rectangle(pt1 +@% v, pt2 +@% v)

  | GeneralPath(pt0, pelst, cycleopt) ->
      let cycleopt_s =
        cycleopt |> option_map (function
          | LineTo(()) as l             -> l
          | CubicBezierTo(pt1, pt2, ()) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, ())
        )
      in
        GeneralPath(pt0 +@% v, pelst |> List.map (shift_path_element v), cycleopt_s)


let rotate_path_element c t pe =
  match pe with
  | LineTo(pt)                  ->
    let newpt = rotate_around pt c t in
    LineTo(newpt)
  | CubicBezierTo(pt1, pt2, pt) -> CubicBezierTo(rotate_around pt1 c t, rotate_around pt2 c t, rotate_around pt c t)


let rotate_path c t path =
  match path with
  | Rectangle(pt1, pt2) ->
      Rectangle(rotate_around pt1 c t, rotate_around pt2 c t)

  | GeneralPath(pt0, pelst, cycleopt) ->
      let cycleopt_s =
        cycleopt |> option_map (function
          | LineTo(()) as l             -> l
          | CubicBezierTo(pt1, pt2, ()) -> CubicBezierTo(rotate_around pt1 c t, rotate_around pt2 c t, ())
        )
      in
        GeneralPath(rotate_around pt0 c t, pelst |> List.map (rotate_path_element c t), cycleopt_s)


let bezier_bbox (x0, y0) (x1, y1) (x2, y2) (x3, y3) =

  let bezier_point t r0 r1 r2 r3 =
    if t < 0. then r0 else
      if 1. < t then r3 else
        let c1 = 3. *. (-. r0 +. r1) in
        let c2 = 3. *. (r0 -. 2. *. r1 +. r2) in
        let c3 = -. r0 +. 3. *. (r1 -. r2) +. r3 in
          r0 +. t *. (c1 +. t *. (c2 +. t *. c3))
  in

  let aux r0 r1 r2 r3 =
    let a = -. r0 +. 3. *. (r1 -. r2) +. r3 in
    let b = 2. *. (r0 -. 2. *. r1 +. r2) in
    let c = -. r0 +. r1 in
    if a = 0. then
      [r0; r3]
    else
      let det = b *. b -. 4. *. a *. c in
      if det < 0. then
        [r0; r3]
      else
        let delta = sqrt det in
        let t_plus  = (-. b +. delta) /. (2. *. a) in
        let t_minus = (-. b -. delta) /. (2. *. a) in
        [r0; r3; bezier_point t_plus r0 r1 r2 r3; bezier_point t_minus r0 r1 r2 r3]
  in

  let ( !=> ) = Length.to_pdf_point in
  let ( !<= ) = Length.of_pdf_point in

  let xoptlst = aux (!=> x0) (!=> x1) (!=> x2) (!=> x3) in
  let xmax = xoptlst |> List.fold_left max (!=> x0) in
  let xmin = xoptlst |> List.fold_left min (!=> x0) in
  let yoptlst = aux (!=> y0) (!=> y1) (!=> y2) (!=> y3) in
  let ymax = yoptlst |> List.fold_left max (!=> y0) in
  let ymin = yoptlst |> List.fold_left min (!=> y0) in
  ((!<= xmin, !<= ymin), (!<= xmax, !<= ymax))


let update_min (x0, y0) (x1, y1) =
  (Length.min x0 x1, Length.min y0 y1)


let update_max (x0, y0) (x1, y1) =
  (Length.max x0 x1, Length.max y0 y1)


let update_bbox_by_path_element (ptmin, ptmax) ptfrom pe =
    match pe with
    | LineTo(ptto) ->
        let bboxnew = (update_min ptmin ptto, update_max ptmax ptto) in
          (bboxnew, ptto)

    | CubicBezierTo(pt1, pt2, ptto) ->
        let (ptminbz, ptmaxbz) = bezier_bbox ptfrom pt1 pt2 ptto in
        let bboxnew = (update_min ptmin ptminbz, update_max ptmax ptmaxbz) in
          (bboxnew, ptto)


let get_path_bbox path =
  match path with
  | Rectangle(pt1, pt2) ->
      (pt1, pt2)

  | GeneralPath(pt0, pelst, cycleopt) ->
      let bboxinit = (pt0, pt0) in
      let (bbox, ptfrom) =
        pelst |> List.fold_left (fun (bbox, ptfrom) pe ->
          update_bbox_by_path_element bbox ptfrom pe
        ) (bboxinit, pt0)
      in
      begin
        match cycleopt with
        | None
        | Some(LineTo(())) ->
            bbox

        | Some(CubicBezierTo(pt1, pt2, ())) ->
            let (bbox, _) =
              update_bbox_by_path_element bbox ptfrom (CubicBezierTo(pt1, pt2, pt0))
            in
              bbox
      end


let get_path_list_bbox pathlst =
  let bboxinit =
    match pathlst with
    | []                          -> assert false  (* -- does not deal with the empty path list -- *)
    | Rectangle(pt1, pt2) :: _    -> (pt1, pt2)
    | GeneralPath(pt0, _, _) :: _ -> (pt0, pt0)
  in
  pathlst |> List.fold_left (fun (ptmin0, ptmax0) path ->
    let (ptmin1, ptmax1) = get_path_bbox path in
      (update_min ptmin0 ptmin1, update_max ptmax0 ptmax1)
  ) bboxinit
